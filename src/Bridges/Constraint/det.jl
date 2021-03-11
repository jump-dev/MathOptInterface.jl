# TODO(odow): this appears elsewhere
function trimap(i::Integer, j::Integer)
    if i < j
        trimap(j, i)
    else
        div((i - 1) * i, 2) + j
    end
end

"""
    extract_eigenvalues(
        model,
        f::MOI.VectorAffineFunction{T},
        d::Int,
        offset::Int
    ), where {T}

The vector `f` contains `t` (if `offset = 1`) or `(t, u)` (if `offset = 2`)
followed by the matrix `X` of dimension `d`.

This functions extracts the eigenvalues of `X` and returns a vector containing
`t` or `(t, u)`, a vector `MOI.VariableIndex` containing the eigenvalues of `X`,
the variables created and the index of the constraint created to extract the
eigenvalues.
"""
function extract_eigenvalues(
    model,
    f::MOI.VectorAffineFunction{T},
    d::Int,
    offset::Int,
) where {T}
    f_scalars = MOIU.eachscalar(f)
    tu = [f_scalars[i] for i in 1:offset]

    n = trimap(d, d)
    X = f_scalars[offset.+(1:n)]
    m = length(X.terms)
    M = m + n + d

    terms = Vector{MOI.VectorAffineTerm{T}}(undef, M)
    terms[1:m] = X.terms
    N = trimap(2d, 2d)
    constant = zeros(T, N)
    constant[1:n] = X.constants

    Δ = MOI.add_variables(model, n)

    cur = m
    for j in 1:d
        for i in j:d
            cur += 1
            terms[cur] = MOI.VectorAffineTerm(
                trimap(i, d + j),
                MOI.ScalarAffineTerm(one(T), Δ[trimap(i, j)]),
            )
        end
        cur += 1
        terms[cur] = MOI.VectorAffineTerm(
            trimap(d + j, d + j),
            MOI.ScalarAffineTerm(one(T), Δ[trimap(j, j)]),
        )
    end
    @assert cur == M
    Y = MOI.VectorAffineFunction(terms, constant)
    sdindex =
        MOI.add_constraint(model, Y, MOI.PositiveSemidefiniteConeTriangle(2d))

    D = Δ[trimap.(1:d, 1:d)]

    return tu, D, Δ, sdindex
end

"""
    LogDetBridge{T}

The `LogDetConeTriangle` is representable by a
`PositiveSemidefiniteConeTriangle` and `ExponentialCone` constraints.

Indeed, ``\\log\\det(X) = \\log(\\delta_1) + \\cdots + \\log(\\delta_n)`` where
``\\delta_1``, ..., ``\\delta_n`` are the eigenvalues of ``X``.

Adapting the method from [1, p. 149], we see that ``t \\le u \\log(\\det(X/u))``
for ``u > 0`` if and only if there exists a lower triangular matrix ``Δ`` such
that
```math
\\begin{align*}
  \\begin{pmatrix}
    X & Δ\\\\
    Δ^\\top & \\mathrm{Diag}(Δ)
  \\end{pmatrix} & \\succeq 0\\\\
  t & \\le u \\log(Δ_{11}/u) + u \\log(Δ_{22}/u) + \\cdots + u \\log(Δ_{nn}/u)
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex
    optimization: analysis, algorithms, and engineering applications*. Society
    for Industrial and Applied Mathematics, 2001.
```
"""
struct LogDetBridge{T} <: AbstractBridge
    Δ::Vector{MOI.VariableIndex}
    l::Vector{MOI.VariableIndex}
    sdindex::CI{
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    }
    lcindex::Vector{CI{MOI.VectorAffineFunction{T},MOI.ExponentialCone}}
    tlindex::CI{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}
end

function bridge_constraint(
    ::Type{LogDetBridge{T}},
    model,
    f::MOI.VectorOfVariables,
    s::MOI.LogDetConeTriangle,
) where {T}
    return bridge_constraint(
        LogDetBridge{T},
        model,
        MOI.VectorAffineFunction{T}(f),
        s,
    )
end

function bridge_constraint(
    ::Type{LogDetBridge{T}},
    model,
    f::MOI.VectorAffineFunction{T},
    s::MOI.LogDetConeTriangle,
) where {T}
    d = s.side_dimension
    tu, D, Δ, sdindex = extract_eigenvalues(model, f, d, 2)
    t, u = tu
    l = MOI.add_variables(model, d)
    lcindex = [sublog(model, l[i], u, D[i], T) for i in eachindex(l)]
    tlindex = subsum(model, t, l, T)
    return LogDetBridge(Δ, l, sdindex, lcindex, tlindex)
end

function MOI.supports_constraint(
    ::Type{LogDetBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.LogDetConeTriangle},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:LogDetBridge})
    return Tuple{DataType}[]
end

function MOIB.added_constraint_types(::Type{LogDetBridge{T}}) where {T}
    return [
        (MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeTriangle),
        (MOI.VectorAffineFunction{T}, MOI.ExponentialCone),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

"""
    sublog(
        model,
        x::MOI.VariableIndex,
        y::MOI.VariableIndex,
        z::MOI.VariableIndex,
        ::Type{T},
    ) where {T}

Constrains ``x \\le y \\log(z/y)`` and returns the constraint index.
"""
function sublog(
    model,
    x::MOI.VariableIndex,
    y::MOI.ScalarAffineFunction{T},
    z::MOI.VariableIndex,
    ::Type{T},
) where {T}
    return MOI.add_constraint(
        model,
        MOIU.operate(vcat, T, MOI.SingleVariable(x), y, MOI.SingleVariable(z)),
        MOI.ExponentialCone(),
    )
end

"""
    subsum(
        model,
        t::MOI.ScalarAffineFunction,
        l::Vector{MOI.VariableIndex},
        ::Type{T}
    ) where {T}

Constrains ``t \\le l_1 + \\cdots + l_n`` where `n` is the length of `l` and
returns the constraint index.
"""
function subsum(
    model,
    t::MOI.ScalarAffineFunction,
    l::Vector{MOI.VariableIndex},
    ::Type{T},
) where {T}
    n = length(l)
    f = MOIU.operate!(-, T, t, MOIU.operate(sum, T, l))
    return MOIU.normalize_and_add_constraint(
        model,
        f,
        MOI.LessThan(zero(T)),
        allow_modify_function = true,
    )
end

# Attributes, Bridge acting as a model

function MOI.get(b::LogDetBridge, ::MOI.NumberOfVariables)
    return Int64(length(b.Δ) + length(b.l))
end

MOI.get(b::LogDetBridge, ::MOI.ListOfVariableIndices) = [b.Δ; b.l]

function MOI.get(
    ::LogDetBridge{T},
    ::MOI.NumberOfConstraints{
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    },
) where {T}
    return Int64(1)
end

function MOI.get(
    b::LogDetBridge{T},
    ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},MOI.ExponentialCone},
) where {T}
    return Int64(length(b.lcindex))
end

function MOI.get(
    ::LogDetBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return Int64(1)
end

function MOI.get(
    b::LogDetBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    },
) where {T}
    return [b.sdindex]
end

function MOI.get(
    b::LogDetBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.ExponentialCone,
    },
) where {T}
    return copy(b.lcindex)
end

function MOI.get(
    b::LogDetBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return [b.tlindex]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::LogDetBridge)
    MOI.delete(model, bridge.tlindex)
    MOI.delete(model, bridge.lcindex)
    MOI.delete(model, bridge.sdindex)
    MOI.delete(model, bridge.l)
    MOI.delete(model, bridge.Δ)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::LogDetBridge,
)
    d = length(bridge.lcindex)
    Δ = MOI.get(model, MOI.VariablePrimal(), bridge.Δ)
    t =
        MOI.get(model, attr, bridge.tlindex) +
        sum(MOI.get(model, attr, ci)[1] for ci in bridge.lcindex)
    u = MOI.get(model, attr, first(bridge.lcindex))[2]
    x = MOI.get(model, attr, bridge.sdindex)[1:length(bridge.Δ)]
    return vcat(t, u, x)
end

# [X Δ; Δ' Diag(Δ)] in PSD
# t - sum(l) >= 0
# (l_i, u, Δ_ii) in Exp
# (t, u, x) in LogDet <=> exists Δ, l such that At + Bu + Cx + DΔ + El in (PSD, >=, Exp_i)
# so LogDet* = [A'; B'; C'] (PSD, >=, Exp_i)*
# and 0 = [D'; E'] (PSD, >=, Exp_i)*
# where
# A = [0, 0, 0, 1, 0, 0, 0]
# B = [0, 0, 0, 0, 0, 1, 0]
# C = [I, 0, 0, 0, 0, 0, 0]
# D = [0, I, I(i=j), 0, 0, 0, I(i=j)]
# E = [0, 0, 0, 1, I, 0, 0]
# so given dual q = (a, b, c, d, e, f, g), we get
# t = A' q = d => t = d
# u = B' q = f => u = sum(f)
# x = C' q = a => x = a
# offdiag(b) = 0
# 0 = D' q = diag(b) + diag(c) + g => g = -diag(b) - diag(c)
# let b = 0, so g_i = -c_i
# 0 = E' q = d + e => d = -e
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::LogDetBridge,
)
    t_dual = MOI.get(model, attr, bridge.tlindex)
    u_dual =
        sum(MOI.get(model, attr, lcindex_i)[2] for lcindex_i in bridge.lcindex)
    x_dual = MOI.get(model, attr, bridge.sdindex)[1:length(bridge.Δ)]
    return vcat(t_dual, u_dual, x_dual)
end

"""
    RootDetBridge{T}

The `RootDetConeTriangle` is representable by a
`PositiveSemidefiniteConeTriangle` and an `GeometricMeanCone` constraints; see
[1, p. 149].

Indeed, ``t \\le \\det(X)^{1/n}`` if and only if there exists a lower triangular
matrix ``Δ`` such that:
```math
\\begin{align*}
  \\begin{pmatrix}
    X & Δ\\\\
    Δ^\\top & \\mathrm{Diag}(Δ)
  \\end{pmatrix} & \\succeq 0\\\\
  t & \\le (Δ_{11} Δ_{22} \\cdots Δ_{nn})^{1/n}
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex
    optimization: analysis, algorithms, and engineering applications*. Society
    for Industrial and Applied Mathematics, 2001.
"""
struct RootDetBridge{T} <: AbstractBridge
    Δ::Vector{MOI.VariableIndex}
    sdindex::CI{
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    }
    gmindex::CI{MOI.VectorAffineFunction{T},MOI.GeometricMeanCone}
end

function bridge_constraint(
    ::Type{RootDetBridge{T}},
    model,
    f::MOI.VectorOfVariables,
    s::MOI.RootDetConeTriangle,
) where {T}
    return bridge_constraint(
        RootDetBridge{T},
        model,
        MOI.VectorAffineFunction{T}(f),
        s,
    )
end

function bridge_constraint(
    ::Type{RootDetBridge{T}},
    model,
    f::MOI.VectorAffineFunction{T},
    s::MOI.RootDetConeTriangle,
) where {T}
    d = s.side_dimension
    tu, D, Δ, sdindex = extract_eigenvalues(model, f, d, 1)
    t = tu[1]
    DF = MOI.VectorAffineFunction{T}(MOI.VectorOfVariables(D))
    gmindex = MOI.add_constraint(
        model,
        MOIU.operate(vcat, T, t, DF),
        MOI.GeometricMeanCone(d + 1),
    )
    return RootDetBridge(Δ, sdindex, gmindex)
end

function MOI.supports_constraint(
    ::Type{RootDetBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.RootDetConeTriangle},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:RootDetBridge})
    return Tuple{DataType}[]
end

function MOIB.added_constraint_types(::Type{RootDetBridge{T}}) where {T}
    return [
        (MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeTriangle),
        (MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone),
    ]
end

# Attributes, Bridge acting as a model

MOI.get(b::RootDetBridge, ::MOI.NumberOfVariables) = Int64(length(b.Δ))

MOI.get(b::RootDetBridge, ::MOI.ListOfVariableIndices) = copy(b.Δ)

function MOI.get(
    ::RootDetBridge{T},
    ::MOI.NumberOfConstraints{
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    },
) where {T}
    return Int64(1)
end

function MOI.get(
    ::RootDetBridge{T},
    ::MOI.NumberOfConstraints{
        MOI.VectorAffineFunction{T},
        MOI.GeometricMeanCone,
    },
) where {T}
    return Int64(1)
end

function MOI.get(
    b::RootDetBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    },
) where {T}
    return [b.sdindex]
end

function MOI.get(
    b::RootDetBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.GeometricMeanCone,
    },
) where {T}
    return [b.gmindex]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::RootDetBridge)
    MOI.delete(model, bridge.gmindex)
    MOI.delete(model, bridge.sdindex)
    MOI.delete(model, bridge.Δ)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::RootDetBridge,
)
    t = MOI.get(model, attr, bridge.gmindex)[1]
    x = MOI.get(model, attr, bridge.sdindex)[1:length(bridge.Δ)]
    return vcat(t, x)
end

# (t, x) in RootDet <=> exists Δ such that At + Bx + CΔ in (PSD, GeoMean)
# so RootDet* = [A'; B'] (PSD, GeoMean)*
# and 0 = [C'] (PSD, GeoMean)*
# where
# A = [0, 0, 0, 1, 0]
# B = [I, 0, 0, 0, 0]
# C = [0, I, I(i=j), 0, I(i=j)]
# so given dual q = (a, b, c, d, e), we get
# t = A' q = d => t = d
# x = B' q = a => x = a
# offdiag(b) = 0
# 0 = C' q = diag(b) + diag(c) + e => e = -diag(b) - diag(c)
# let b = 0, so e_i = -c_i
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::RootDetBridge,
)
    t_dual = MOI.get(model, attr, bridge.gmindex)[1]
    x_dual = MOI.get(model, attr, bridge.sdindex)[1:length(bridge.Δ)]
    return vcat(t_dual, x_dual)
end
