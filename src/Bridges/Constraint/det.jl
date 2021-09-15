"""
    extract_eigenvalues(
        model,
        f::MOI.AbstractVectorFunction,
        d::Int,
        offset::Int,
        ::Type{T},
    ) where {T}

The vector `f` contains `t` (if `offset = 1`) or `(t, u)` (if `offset = 2`)
followed by the matrix `X` of dimension `d`.

This functions extracts the eigenvalues of `X` and returns a vector containing
`t` or `(t, u)`, a vector `MOI.VariableIndex` containing the eigenvalues of `X`,
the variables created and the index of the constraint created to extract the
eigenvalues.
"""
function extract_eigenvalues(
    model,
    f::MOI.AbstractVectorFunction,
    d::Int,
    offset::Int,
    ::Type{T},
) where {T}
    f_scalars = MOI.Utilities.eachscalar(f)
    tu = [f_scalars[i] for i in 1:offset]
    n = MOI.Utilities.trimap(d, d)
    X = f_scalars[offset.+(1:n)]
    N = MOI.Utilities.trimap(2d, 2d)
    Δ = MOI.add_variables(model, n)
    Z = [zero(MOI.ScalarAffineFunction{T}) for i in 1:(N-n)]
    for j in 1:d
        for i in j:d
            Z[MOI.Utilities.trimap(i, d + j)-n] = Δ[MOI.Utilities.trimap(i, j)]
        end
        Z[MOI.Utilities.trimap(d + j, d + j)-n] = Δ[MOI.Utilities.trimap(j, j)]
    end
    Y = MOIU.operate(vcat, T, X, MOI.Utilities.vectorize(Z))
    set = MOI.PositiveSemidefiniteConeTriangle(2d)
    sdindex = MOI.add_constraint(model, Y, set)
    D = Δ[MOI.Utilities.trimap.(1:d, 1:d)]
    return tu, D, Δ, sdindex
end

"""
    LogDetBridge{T,F,G,H,I}

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
struct LogDetBridge{T,F,G,H,I} <: AbstractBridge
    Δ::Vector{MOI.VariableIndex}
    l::Vector{MOI.VariableIndex}
    sdindex::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
    lcindex::Vector{MOI.ConstraintIndex{G,MOI.ExponentialCone}}
    tlindex::MOI.ConstraintIndex{H,MOI.LessThan{T}}
end

function bridge_constraint(
    ::Type{LogDetBridge{T,F,G,H,I}},
    model,
    f::I,
    s::MOI.LogDetConeTriangle,
) where {T,F,G,H,I}
    d = s.side_dimension
    tu, D, Δ, sdindex = extract_eigenvalues(model, f, d, 2, T)
    t, u = tu
    l = MOI.add_variables(model, d)
    lcindex = [sublog(model, l[i], u, D[i], T) for i in eachindex(l)]
    tlindex = subsum(model, t, l, T)
    return LogDetBridge{T,F,G,H,I}(Δ, l, sdindex, lcindex, tlindex)
end

function MOI.supports_constraint(
    ::Type{LogDetBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.LogDetConeTriangle},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:LogDetBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:LogDetBridge{T,F,G,H}},
) where {T,F,G,H}
    return Tuple{Type,Type}[
        (F, MOI.PositiveSemidefiniteConeTriangle),
        (G, MOI.ExponentialCone),
        (H, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:LogDetBridge{T}},
    I::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.LogDetConeTriangle},
) where {T}
    S = MOIU.scalar_type(I)
    G = MOIU.promote_operation(vcat, T, S, MOI.VariableIndex)
    F = MOIU.promote_operation(vcat, T, G, T)
    H = MOIU.promote_operation(+, T, S, MOI.ScalarAffineFunction{T})
    return LogDetBridge{T,F,G,H,I}
end

"""
    sublog(
        model,
        x::MOI.VariableIndex,
        y::MOI.AbstractScalarFunction,
        z::MOI.VariableIndex,
        ::Type{T},
    ) where {T}

Constrains ``x \\le y \\log(z/y)`` and returns the constraint index.
"""
function sublog(
    model,
    x::MOI.VariableIndex,
    y::MOI.AbstractScalarFunction,
    z::MOI.VariableIndex,
    ::Type{T},
) where {T}
    return MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, x, y, z),
        MOI.ExponentialCone(),
    )
end

"""
    subsum(
        model,
        t::MOI.AbstractScalarFunction,
        l::Vector{MOI.VariableIndex},
        ::Type{T}
    ) where {T}

Constrains ``t \\le l_1 + \\cdots + l_n`` where `n` is the length of `l` and
returns the constraint index.
"""
function subsum(
    model,
    t::MOI.AbstractScalarFunction,
    l::Vector{MOI.VariableIndex},
    ::Type{T},
) where {T}
    rhs = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), l), zero(T))
    f = MOI.Utilities.operate!(-, T, t, rhs)
    return MOI.Utilities.normalize_and_add_constraint(
        model,
        f,
        MOI.LessThan(zero(T)),
        allow_modify_function = true,
    )
end

function MOI.get(b::LogDetBridge, ::MOI.NumberOfVariables)::Int64
    return length(b.Δ) + length(b.l)
end

MOI.get(b::LogDetBridge, ::MOI.ListOfVariableIndices) = [b.Δ; b.l]

function MOI.get(
    ::LogDetBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.PositiveSemidefiniteConeTriangle},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    b::LogDetBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.ExponentialCone},
)::Int64 where {T,F,G}
    return length(b.lcindex)
end

function MOI.get(
    ::LogDetBridge{T,F,G,H},
    ::MOI.NumberOfConstraints{H,MOI.LessThan{T}},
)::Int64 where {T,F,G,H}
    return 1
end

function MOI.get(
    b::LogDetBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.PositiveSemidefiniteConeTriangle},
) where {T,F}
    return [b.sdindex]
end

function MOI.get(
    b::LogDetBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.ExponentialCone},
) where {T,F,G}
    return copy(b.lcindex)
end

function MOI.get(
    b::LogDetBridge{T,F,G,H},
    ::MOI.ListOfConstraintIndices{H,MOI.LessThan{T}},
) where {T,F,G,H}
    return [b.tlindex]
end

function MOI.delete(model::MOI.ModelLike, bridge::LogDetBridge)
    MOI.delete(model, bridge.tlindex)
    MOI.delete(model, bridge.lcindex)
    MOI.delete(model, bridge.sdindex)
    MOI.delete(model, bridge.l)
    MOI.delete(model, bridge.Δ)
    return
end

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
# (t, u, x) in LogDet <=>
# exists Δ, l such that At + Bu + Cx + DΔ + El in (PSD, >=, Exp_i)
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

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::LogDetBridge)
    return MOI.LogDetConeTriangle(length(bridge.l))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::LogDetBridge{T,F,G,H,I},
) where {T,F,G,H,I}
    n = length(bridge.Δ)
    Y = MOI.get(model, attr, bridge.sdindex)::F
    X = MOI.Utilities.convert_approx(I, MOI.Utilities.eachscalar(Y)[1:n])
    slog = MOI.get(model, attr, first(bridge.lcindex))::G
    u = MOI.Utilities.eachscalar(slog)[2]
    ssu = MOI.get(model, attr, bridge.tlindex)::H
    taff = MOI.Utilities.remove_variable(ssu, bridge.l)
    SI = MOI.Utilities.scalar_type(I)
    t = MOI.Utilities.convert_approx(SI, taff)
    return MOI.Utilities.operate(vcat, T, t, u, X)::I
end

"""
    RootDetBridge{T,F,G,H}

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
struct RootDetBridge{T,F,G,H} <: AbstractBridge
    Δ::Vector{MOI.VariableIndex}
    sdindex::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
    gmindex::MOI.ConstraintIndex{G,MOI.GeometricMeanCone}
end

function bridge_constraint(
    ::Type{RootDetBridge{T,F,G,H}},
    model,
    f::G,
    s::MOI.RootDetConeTriangle,
) where {T,F,G,H}
    d = s.side_dimension
    tu, D, Δ, sdindex = extract_eigenvalues(model, f, d, 1, T)
    t = tu[1]
    DF = MOI.VectorOfVariables(D)
    gmindex = MOI.add_constraint(
        model,
        MOI.Utilities.operate(vcat, T, t, DF),
        MOI.GeometricMeanCone(d + 1),
    )
    return RootDetBridge{T,F,G,H}(Δ, sdindex, gmindex)
end

function MOI.supports_constraint(
    ::Type{<:RootDetBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RootDetConeTriangle},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:RootDetBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:RootDetBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[
        (F, MOI.PositiveSemidefiniteConeTriangle),
        (G, MOI.GeometricMeanCone),
    ]
end

function concrete_bridge_type(
    ::Type{<:RootDetBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RootDetConeTriangle},
) where {T}
    S = MOIU.scalar_type(H)
    G = MOIU.promote_operation(vcat, T, S, MOI.VariableIndex)
    F = MOIU.promote_operation(vcat, T, G, T)
    return RootDetBridge{T,F,G,H}
end

MOI.get(b::RootDetBridge, ::MOI.NumberOfVariables)::Int64 = length(b.Δ)

MOI.get(b::RootDetBridge, ::MOI.ListOfVariableIndices) = copy(b.Δ)

function MOI.get(
    ::RootDetBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.PositiveSemidefiniteConeTriangle},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    ::RootDetBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.GeometricMeanCone},
)::Int64 where {T,F,G}
    return 1
end

function MOI.get(
    b::RootDetBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.PositiveSemidefiniteConeTriangle},
) where {T,F}
    return [b.sdindex]
end

function MOI.get(
    b::RootDetBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.GeometricMeanCone},
) where {T,F,G}
    return [b.gmindex]
end

function MOI.delete(model::MOI.ModelLike, bridge::RootDetBridge)
    MOI.delete(model, bridge.gmindex)
    MOI.delete(model, bridge.sdindex)
    MOI.delete(model, bridge.Δ)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
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
    attr::Union{MOI.ConstraintDual},
    bridge::RootDetBridge,
)
    t_dual = MOI.get(model, attr, bridge.gmindex)[1]
    x_dual = MOI.get(model, attr, bridge.sdindex)[1:length(bridge.Δ)]
    return vcat(t_dual, x_dual)
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::RootDetBridge)
    n = length(bridge.Δ)
    d = MOI.Utilities.side_dimension_for_vectorized_dimension(n)
    return MOI.RootDetConeTriangle(d)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::RootDetBridge{T,F,G,H},
) where {T,F,G,H}
    n = length(bridge.Δ)
    Y = MOI.get(model, attr, bridge.sdindex)::F
    X = MOI.Utilities.convert_approx(H, MOI.Utilities.eachscalar(Y)[1:n])
    func = MOI.get(model, attr, bridge.gmindex)::G
    t = MOI.Utilities.eachscalar(func)[1]
    return MOI.Utilities.operate(vcat, T, t, X)::H
end
