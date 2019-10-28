"""
    _SOCtoPSDaff{T}(f::MOI.VectorAffineFunction{T}, g::MOI.ScalarAffineFunction{T})

Builds a VectorAffineFunction representing the upper (or lower) triangular part of the matrix
[ f[1]     f[2:end]' ]
[ f[2:end] g * I     ]
"""
function _SOCtoPSDaff(T::Type,
                      f::MOI.AbstractVectorFunction,
                      g::MOI.AbstractScalarFunction)
    F = MOIU.promote_operation(vcat, T, typeof(g), T)
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
struct SOCtoPSDBridge{T, F, G} <: SetMapBridge{T, MOI.SecondOrderCone, MOI.PositiveSemidefiniteConeTriangle, F, G}
    constraint::MOI.ConstraintIndex{F, MOI.PositiveSemidefiniteConeTriangle}
end
function concrete_bridge_type(::Type{<:SOCtoPSDBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.SecondOrderCone}) where T
    F = MOIU.promote_operation(vcat, T, MOIU.scalar_type(G), T)
    return SOCtoPSDBridge{T, F, G}
end

function map_set(::Type{<:SOCtoPSDBridge}, set::MOI.PositiveSemidefiniteConeTriangle)
    return MOI.SecondOrderCone(MOI.side_dimension(set))
end
function inverse_map_set(::Type{<:SOCtoPSDBridge}, set::MOI.SecondOrderCone)
    return MOI.PositiveSemidefiniteConeTriangle(MOI.dimension(set))
end

function map_function(::Type{<:SOCtoPSDBridge{T}}, func) where T
    return _SOCtoPSDaff(T, g, MOIU.eachscalar(g)[1])
end
function inverse_map_function(::Type{<:SOCtoPSDBridge}, func)
    scalars = MOIU.eachscalar(func)
    dim = side_dimension_for_vectorized_dimension(length(scalars))
    return scalars[trimap.(1, 1:dim)]
end
function adjoint_map_function(::Type{<:SOCtoPSDBridge{T}}, func) where T
    scalars = MOIU.eachscalar(func)
    dim = side_dimension_for_vectorized_dimension(length(scalars))
    tdual = sum(i -> func[trimap(i, i)], 1:dim)
    return MOIU.operate(vcat, T, tdual, func[trimap.(2:c.dim, 1)]*2)
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
struct RSOCtoPSDBridge{T, F, G} <: SetMapBridge{T, MOI.RotatedSecondOrderCone, MOI.PositiveSemidefiniteConeTriangle, F, G}
    constraint::MOI.ConstraintIndex{F, MOI.PositiveSemidefiniteConeTriangle}
end
function concrete_bridge_type(::Type{<:RSOCtoPSDBridge{T}},
                              G::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.RotatedSecondOrderCone}) where T
    S = MOIU.scalar_type(G)
    H = MOIU.promote_operation(*, T, T, S)
    F = MOIU.promote_operation(vcat, T, S, H, T)
    return RSOCtoPSDBridge{T, F, G}
end

function map_set(::Type{<:RSOCtoPSDBridge}, set::MOI.PositiveSemidefiniteConeTriangle)
    return MOI.RotatedSecondOrderCone(MOI.side_dimension(set) + 1)
end
function inverse_map_set(::Type{<:RSOCtoPSDBridge}, set::MOI.RotatedSecondOrderCone)
    return MOI.PositiveSemidefiniteConeTriangle(MOI.dimension(set) - 1)
end

function map_function(::Type{<:RSOCtoPSDBridge{T}}, func) where T
    scalars = MOIU.eachscalar(func)
    h = MOIU.operate!(*, T, scalars[2], convert(T, 2))
    return _SOCtoPSDaff(T, scalars[[1; 3:MOI.output_dimension(g)]], h)
end
function inverse_map_function(::Type{<:RSOCtoPSDBridge}, func)
    scalars = MOIU.eachscalar(func)
    dim = side_dimension_for_vectorized_dimension(length(scalars))
    t = scalars[1]
    # It is (2u*I)[1,1] so it needs to be divided by 2 to get u
    u = MOIU.operate!(/, T, scalars[3], convert(T, 2))
    funcs = [t, u]
    for i in 2:dim
        push!(funcs, scalars[trimap(1, i)])
    end
    return MOIU.vectorize(funcs)
end
function adjoint_map_function(::Type{<:RSOCtoPSDBridge{T}}, func) where T
    scalars = MOIU.eachscalar(func)
    dim = side_dimension_for_vectorized_dimension(length(scalars))
    udual = sum(i -> func[trimap(i, i)], 2:dim)
    return MOIU.operate(vcat, T, dual[1],  2udual, func[trimap.(2:dim, 1)]*2)
end
