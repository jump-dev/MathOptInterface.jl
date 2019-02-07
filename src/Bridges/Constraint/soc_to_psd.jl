"""
    _SOCtoPSDaff{T}(f::MOI.VectorAffineFunction{T}, g::MOI.ScalarAffineFunction{T})

Builds a VectorAffineFunction representing the upper (or lower) triangular part of the matrix
[ f[1]     f[2:end]' ]
[ f[2:end] g * I     ]
"""
function _SOCtoPSDaff(
    ::Type{T},
    f::Union{MOI.AbstractVectorFunction,AbstractVector{T}},
    g::Union{MOI.AbstractScalarFunction,T},
) where {T}
    F = MOIU.promote_operation(vcat, T, typeof(g), T)
    f_scalars = MOIU.eachscalar(f)
    dim = length(f_scalars)
    n = div(dim * (dim + 1), 2)
    h = MOIU.zero_with_output_dimension(F, n)
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

!!! warning
    This bridge is not added by default by [`MOI.Bridges.full_bridge_optimizer`](@ref)
    as bridging second order cone constraints to semidefinite constraints can be
    achieved by the [`SOCtoRSOCBridge`](@ref) followed by the [`RSOCtoPSDBridge`](@ref)
    while creating a smaller semidefinite constraint.
"""
struct SOCtoPSDBridge{T,F,G} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.SecondOrderCone,
    F,
    G,
}
    constraint::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
end

function concrete_bridge_type(
    ::Type{<:SOCtoPSDBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.SecondOrderCone},
) where {T}
    F = MOIU.promote_operation(vcat, T, MOIU.scalar_type(G), T)
    return SOCtoPSDBridge{T,F,G}
end

function MOIB.map_set(::Type{<:SOCtoPSDBridge}, set::MOI.SecondOrderCone)
    return MOI.PositiveSemidefiniteConeTriangle(MOI.dimension(set))
end

function MOIB.inverse_map_set(
    ::Type{<:SOCtoPSDBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.SecondOrderCone(MOI.side_dimension(set))
end

function MOIB.map_function(::Type{<:SOCtoPSDBridge{T}}, func) where {T}
    return _SOCtoPSDaff(T, func, MOIU.eachscalar(func)[1])
end

function MOIB.inverse_map_function(::Type{<:SOCtoPSDBridge}, func)
    scalars = MOIU.eachscalar(func)
    dim = MOIU.side_dimension_for_vectorized_dimension(length(scalars))
    return scalars[trimap.(1, 1:dim)]
end

function MOIB.adjoint_map_function(::Type{<:SOCtoPSDBridge{T}}, func) where {T}
    scalars = MOIU.eachscalar(func)
    dim = MOIU.side_dimension_for_vectorized_dimension(length(scalars))
    tdual = sum(i -> func[trimap(i, i)], 1:dim)
    return MOIU.operate(vcat, T, tdual, func[trimap.(2:dim, 1)] * 2)
end

function MOIB.inverse_adjoint_map_function(
    ::Type{<:SOCtoPSDBridge{T}},
    func,
) where {T}
    # func is (t, x) such that x'x ≤ t^2 and we need to find a PSD matrix
    # [a   x'/2]
    # [x/2 Q   ]
    # such that a + tr(Q) == t
    # By choosing a = t/2 and Q = t/(2x'x) * xx', we get
    # a + tr(Q) = t/2 + t/(2x'x) * tr(xx') = t/2 + t/(2x'x) * tr(x'x) = t
    # Moreover, by the Schur complement, the matrix is PSD iff
    # xx'/(2t) ⪯ t/(2x'x) * xx'
    # 1/(2t) ≤ t/(2x'x)
    # x'x ≤ t^2
    # which is the SOC inequality
    t = func[1]
    x = func[2:end]
    return MOIB.inverse_adjoint_map_function(RSOCtoPSDBridge{T}, [t / 2; t; x])
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
struct RSOCtoPSDBridge{T,F,G} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.RotatedSecondOrderCone,
    F,
    G,
}
    constraint::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
end

function concrete_bridge_type(
    ::Type{<:RSOCtoPSDBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RotatedSecondOrderCone},
) where {T}
    S = MOIU.scalar_type(G)
    H = MOIU.promote_operation(*, T, T, S)
    F = MOIU.promote_operation(vcat, T, S, H, T)
    return RSOCtoPSDBridge{T,F,G}
end

function MOIB.map_set(
    ::Type{<:RSOCtoPSDBridge},
    set::MOI.RotatedSecondOrderCone,
)
    return MOI.PositiveSemidefiniteConeTriangle(MOI.dimension(set) - 1)
end

function MOIB.inverse_map_set(
    ::Type{<:RSOCtoPSDBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.RotatedSecondOrderCone(MOI.side_dimension(set) + 1)
end

function MOIB.map_function(::Type{<:RSOCtoPSDBridge{T}}, func) where {T}
    scalars = MOIU.eachscalar(func)
    h = MOIU.operate!(*, T, scalars[2], convert(T, 2))
    return _SOCtoPSDaff(T, scalars[[1; 3:length(scalars)]], h)
end

function MOIB.inverse_map_function(::Type{<:RSOCtoPSDBridge{T}}, func) where {T}
    scalars = MOIU.eachscalar(func)
    dim = MOIU.side_dimension_for_vectorized_dimension(length(scalars))
    t = scalars[1]
    # It is (2u*I)[1,1] so it needs to be divided by 2 to get u
    u = MOIU.operate!(/, T, scalars[3], convert(T, 2))
    return MOIU.operate(vcat, T, t, u, scalars[[trimap(1, i) for i in 2:dim]])
end

function MOIB.adjoint_map_function(::Type{<:RSOCtoPSDBridge{T}}, func) where {T}
    scalars = MOIU.eachscalar(func)
    dim = MOIU.side_dimension_for_vectorized_dimension(length(scalars))
    udual = sum(i -> func[trimap(i, i)], 2:dim)
    return MOIU.operate(vcat, T, func[1], 2udual, func[trimap.(2:dim, 1)] * 2)
end

function MOIB.inverse_adjoint_map_function(
    ::Type{<:RSOCtoPSDBridge{T}},
    func,
) where {T}
    # func is (t, u, x) such that x'x ≤ 2tu and we need to find a PSD matrix
    # [t   x'/2]
    # [x/2 Q   ]
    # such that 2tr(Q) == u
    # By choosing Q = u/(2x'x) * xx', we get
    # 2tr(Q) = u/(x'x) * tr(xx') = u/(x'x) * tr(x'x) = u
    # Moreover, by the Schur complement, the matrix is PSD iff
    # xx'/(4t) ⪯ u/(2x'x) * xx'
    # 1/(4t) ≤ u/(2x'x)
    # x'x ≤ 2tu
    # which is the RSOC inequality
    t = func[1]
    u = func[2]
    x = func[3:end]
    Q = (x * x') * (u / (2 * (x' * x)))
    M = [
        t x'/2
        x/2 Q
    ]
    return [M[i, j] for j in 1:size(M, 2) for i in 1:j]
end
