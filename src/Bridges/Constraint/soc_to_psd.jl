"""
    _SOCtoPSDaff{T}(f::MOI.VectorAffineFunction{T}, g::MOI.ScalarAffineFunction{T})

Builds a VectorAffineFunction representing the upper (or lower) triangular part of the matrix
[ f[1]     f[2:end]' ]
[ f[2:end] g * I     ]
"""
function _SOCtoPSDaff(T::Type, F::Type{<:MOI.AbstractVectorFunction},
                      f::MOI.AbstractVectorFunction, g::MOI.AbstractScalarFunction)
    dim = MOI.output_dimension(f)
    n = div(dim * (dim+1), 2)
    h = MOIU.zero_with_output_dimension(F, n)
    f_scalars = MOIU.eachscalar(f)
    MOIU.operate_output_index!(+, T, trimap(1, 1), h, f_scalars[1])
    for i in 2:dim
        MOIU.operate_output_index!(+, T, trimap(1, i), h, f_scalars[i])
        MOIU.operate_output_index!(+, T, trimap(i, i), h, g)
    end
    return h
end

"""
The `SOCtoPSDBridge` transforms the second order cone constraint ``\\lVert x \\rVert \\le t`` into the semidefinite cone constraints
```math
\\begin{pmatrix}
  t & x^\\top\\\\
  x & tI
\\end{pmatrix} \\succeq 0
```
Indeed by the Schur Complement, it is positive definite iff
```math
\\begin{align*}
  tI & \\succ 0\\\\
  t - x^\\top (tI)^{-1} x & \\succ 0
\\end{align*}
```
which is equivalent to
```math
\\begin{align*}
  t & > 0\\\\
  t^2 & > x^\\top x
\\end{align*}
```
This bridge is not added by default by [`MOI.Bridges.full_bridge_optimizer`](@ref)
as bridging second order cone constraints to semidefinite constraints can be
achieved by the [`SOCRBridge`](@ref) followed by the [`RSOCtoPSDBridge`](@ref)
while creating a smaller semidefinite constraint.
"""
struct SOCtoPSDBridge{T, F, G} <: AbstractBridge
    dim::Int
    cr::MOI.ConstraintIndex{F, MOI.PositiveSemidefiniteConeTriangle}
end
function bridge_constraint(::Type{SOCtoPSDBridge{T, F, G}}, model::MOI.ModelLike, g::G,
                           s::MOI.SecondOrderCone) where {T, F, G}
    d = MOI.dimension(s)
    f = _SOCtoPSDaff(T, F, g, MOIU.eachscalar(g)[1])
    cr = MOI.add_constraint(model, f, MOI.PositiveSemidefiniteConeTriangle(d))
    return SOCtoPSDBridge{T, F, G}(d, cr)
end

function MOI.supports_constraint(
    ::Type{<:SOCtoPSDBridge}, ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.SecondOrderCone})
    return true
end
MOIB.added_constrained_variable_types(::Type{<:SOCtoPSDBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:SOCtoPSDBridge{T, F}}) where {T, F}
    return [(F, MOI.PositiveSemidefiniteConeTriangle)]
end
function concrete_bridge_type(::Type{<:SOCtoPSDBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.SecondOrderCone}) where T
    F = MOIU.promote_operation(vcat, T, MOIU.scalar_type(G), T)
    return SOCtoPSDBridge{T, F, G}
end


# Attributes, Bridge acting as a model
function MOI.get(
    ::SOCtoPSDBridge{T, F},
    ::MOI.NumberOfConstraints{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F}
    return 1
end
function MOI.get(
    bridge::SOCtoPSDBridge{T},
    ::MOI.ListOfConstraintIndices{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F}
    return [bridge.cr]
end

# References
function MOI.delete(model::MOI.ModelLike, c::SOCtoPSDBridge)
    MOI.delete(model, c.cr)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::SOCtoPSDBridge{T, F, G}) where {T, F, G}
    f = MOI.get(model, attr, bridge.cr)
    g = MOIU.eachscalar(f)[trimap.(1, 1:bridge.dim)]
    return MOIU.convert_approx(G, g)
end
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::SOCtoPSDBridge)
    return MOI.SecondOrderCone(bridge.dim)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintPrimal, c::SOCtoPSDBridge)
    return MOI.get(model, a, c.cr)[trimap.(1, 1:c.dim)]
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SOCtoPSDBridge)
    dual = MOI.get(model, a, c.cr)
    tdual = sum(i -> dual[trimap(i, i)], 1:c.dim)
    return [tdual; dual[trimap.(2:c.dim, 1)]*2]
end


"""
The `RSOCtoPSDBridge` transforms the second order cone constraint ``\\lVert x \\rVert \\le 2tu`` with ``u \\ge 0`` into the semidefinite cone constraints
```math
\\begin{pmatrix}
  t & x^\\top\\\\
  x & 2uI
\\end{pmatrix} \\succeq 0
```
Indeed by the Schur Complement, it is positive definite iff
```math
\\begin{align*}
  uI & \\succ 0\\\\
  t - x^\\top (2uI)^{-1} x & \\succ 0
\\end{align*}
```
which is equivalent to
```math
\\begin{align*}
  u & > 0\\\\
  2tu & > x^\\top x
\\end{align*}
```
"""
struct RSOCtoPSDBridge{T, F, G} <: AbstractBridge
    dim::Int
    cr::MOI.ConstraintIndex{F, MOI.PositiveSemidefiniteConeTriangle}
end
function bridge_constraint(::Type{RSOCtoPSDBridge{T, F, G}}, model::MOI.ModelLike, g::G,
                           set::MOI.RotatedSecondOrderCone) where {T, F, G}
    dim = MOI.dimension(set) - 1
    g_scalars = MOIU.eachscalar(g)
    h = MOIU.operate!(*, T, g_scalars[2], convert(T, 2))
    f = _SOCtoPSDaff(T, F, g_scalars[[1; 3:MOI.output_dimension(g)]], h)
    cr = MOI.add_constraint(model, f, MOI.PositiveSemidefiniteConeTriangle(dim))
    return RSOCtoPSDBridge{T, F, G}(dim, cr)
end


function MOI.supports_constraint(
    ::Type{<:RSOCtoPSDBridge}, ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RotatedSecondOrderCone})
    return true
end
MOIB.added_constrained_variable_types(::Type{<:RSOCtoPSDBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:RSOCtoPSDBridge{T, F}}) where {T, F}
    return [(F, MOI.PositiveSemidefiniteConeTriangle)]
end
function concrete_bridge_type(::Type{<:RSOCtoPSDBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.RotatedSecondOrderCone}) where T
    S = MOIU.scalar_type(G)
    H = MOIU.promote_operation(*, T, T, S)
    F = MOIU.promote_operation(vcat, T, S, H, T)
    return RSOCtoPSDBridge{T, F, G}
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::RSOCtoPSDBridge{T, F},
    ::MOI.NumberOfConstraints{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F}
    return 1
end
function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.ListOfConstraintIndices{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F}
    return [bridge.cr]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::RSOCtoPSDBridge)
    MOI.delete(model, bridge.cr)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::RSOCtoPSDBridge{T, F, G}) where {T, F, G}
    f_scalars = MOIU.eachscalar(MOI.get(model, attr, bridge.cr))
    t = f_scalars[1]
    u = MOIU.operate!(/, T, f_scalars[3], convert(T, 2))
    funcs = [t, u]
    for i in 2:bridge.dim
        push!(funcs, f_scalars[trimap(1, i)])
    end
    return MOIU.convert_approx(G, MOIU.vectorize(funcs))
end
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::RSOCtoPSDBridge)
    return MOI.RotatedSecondOrderCone(bridge.dim + 1)
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintPrimal, bridge::RSOCtoPSDBridge)
    x = MOI.get(model, MOI.ConstraintPrimal(), bridge.cr)[[trimap(1, 1); trimap(2, 2); trimap.(2:bridge.dim, 1)]]
    x[2] /= 2 # It is (2u*I)[1,1] so it needs to be divided by 2 to get u
    return x
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, bridge::RSOCtoPSDBridge)
    dual = MOI.get(model, MOI.ConstraintDual(), bridge.cr)
    udual = sum(i -> dual[trimap(i, i)], 2:bridge.dim)
    return [dual[1]; 2udual; dual[trimap.(2:bridge.dim, 1)]*2]
end
