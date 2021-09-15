"""
    GeoMeanBridge{T, F, G, H}

The `GeometricMeanCone` is `SecondOrderCone` representable; see [1, p. 105].

The reformulation is best described in an example.

Consider the cone of dimension 4:
```math
t \\le \\sqrt[3]{x_1 x_2 x_3}
```
This can be rewritten as ``\\exists x_{21} \\ge 0`` such that:
```math
\\begin{align*}
  t & \\le x_{21},\\\\
  x_{21}^4 & \\le x_1 x_2 x_3 x_{21}.
\\end{align*}
```

Note that we need to create ``x_{21}`` and not use ``t^4`` directly as ``t`` is
allowed to be negative. Now, this is equivalent to:
```math
\\begin{align*}
  t & \\le x_{21}/\\sqrt{4},\\\\
  x_{21}^2 & \\le 2x_{11} x_{12},\\\\
  x_{11}^2 & \\le 2x_1 x_2, & x_{12}^2 & \\le 2x_3(x_{21}/\\sqrt{4}).
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex
    optimization: analysis, algorithms, and engineering applications*. Society
    for Industrial and Applied Mathematics, 2001.
"""
struct GeoMeanBridge{T,F,G,H} <: AbstractBridge
    d::Int
    xij::Vector{MOI.VariableIndex}
    t_upper_bound_constraint::MOI.ConstraintIndex{F,MOI.LessThan{T}}
    rsoc_constraints::Vector{MOI.ConstraintIndex{G,MOI.RotatedSecondOrderCone}}
    x_nonnegative_constraint::Union{
        Nothing,
        MOI.ConstraintIndex{H,MOI.Nonnegatives},
    }
end

_ilog2(n, i = 0) = n <= (1 << i) ? i : _ilog2(n, i + 1)

function bridge_constraint(
    ::Type{GeoMeanBridge{T,F,G,H}},
    model,
    f::MOI.AbstractVectorFunction,
    s::MOI.GeometricMeanCone,
) where {T,F,G,H}
    f_scalars = MOI.Utilities.eachscalar(f)
    t = f_scalars[1]
    if MOI.dimension(s) == 2
        # The case {(t, x) ∈ R²: x₁ >= 0, t <= x₁}
        # The constraint t <= x₁
        t_upper_bound_constraint = MOI.Utilities.normalize_and_add_constraint(
            model,
            MOI.Utilities.operate!(-, T, t, f_scalars[2]),
            MOI.LessThan(zero(T)),
            allow_modify_function = true,
        )
        # The constraint x₁ >= 0
        x_nonnegative_constraint =
            MOI.add_constraint(model, f_scalars[2:2], MOI.Nonnegatives(1))
        return GeoMeanBridge{T,F,G,H}(
            MOI.dimension(s),
            MOI.VariableIndex[],
            t_upper_bound_constraint,
            MOI.ConstraintIndex{G,MOI.RotatedSecondOrderCone}[],
            x_nonnegative_constraint,
        )
    end
    SG = MOIU.scalar_type(G)
    d = MOI.dimension(s)
    n = d - 1
    l = _ilog2(n)
    N = 1 << l
    rsoc_constraints =
        Vector{MOI.ConstraintIndex{G,MOI.RotatedSecondOrderCone}}(undef, N - 1)
    xij = MOI.add_variables(model, N - 1)
    xl1 = xij[1]
    sN = one(T) / sqrt(N)
    _getx(i)::SG = i > n ? sN * xl1 : f_scalars[1+i]

    # With sqrt(2)^l*t - xl1, we should scale both the ConstraintPrimal and
    # ConstraintDual
    t_upper_bound_constraint = MOIU.normalize_and_add_constraint(
        model,
        MOIU.operate!(+, T, t, -sN * xl1),
        MOI.LessThan(zero(T)),
        allow_modify_function = true,
    )
    offset = offset_next = 0
    for i in 1:l
        num_lvars = 1 << (i - 1)
        offset_next = offset + num_lvars
        for j in 1:num_lvars
            if i == l
                a = _getx(2j - 1)
                b = _getx(2j)
            else
                a = convert(SG, xij[offset_next+2j-1])
                b = convert(SG, xij[offset_next+2j])
            end
            c = xij[offset+j]
            rsoc_constraints[offset+j] = MOI.add_constraint(
                model,
                MOIU.operate(vcat, T, a, b, c),
                MOI.RotatedSecondOrderCone(3),
            )
        end
        offset = offset_next
    end
    @assert offset == length(rsoc_constraints)
    return GeoMeanBridge{T,F,G,H}(
        d,
        xij,
        t_upper_bound_constraint,
        rsoc_constraints,
        nothing,
    )
end

function MOI.supports_constraint(
    ::Type{GeoMeanBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:GeoMeanBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{<:GeoMeanBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[
        (F, MOI.LessThan{T}),
        (G, MOI.RotatedSecondOrderCone),
        (G, MOI.Nonnegatives),
    ]
end

function concrete_bridge_type(
    ::Type{<:GeoMeanBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.GeometricMeanCone},
) where {T}
    S = MOIU.scalar_type(H)
    A = MOIU.promote_operation(*, T, T, MOI.VariableIndex)
    F = MOIU.promote_operation(+, T, S, A)
    G = MOIU.promote_operation(vcat, T, A, S, MOI.VariableIndex)
    return GeoMeanBridge{T,F,G,H}
end

# Attributes, Bridge acting as a model
MOI.get(b::GeoMeanBridge, ::MOI.NumberOfVariables)::Int64 = length(b.xij)

MOI.get(b::GeoMeanBridge, ::MOI.ListOfVariableIndices) = copy(b.xij)

function MOI.get(
    ::GeoMeanBridge{T,F},
    ::MOI.NumberOfConstraints{F,MOI.LessThan{T}},
)::Int64 where {T,F}
    return 1
end

function MOI.get(
    b::GeoMeanBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.RotatedSecondOrderCone},
)::Int64 where {T,F,G}
    return length(b.rsoc_constraints)
end

function MOI.get(
    b::GeoMeanBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.Nonnegatives},
)::Int64 where {T,F,G}
    return b.d > 2 ? 0 : 1
end

function MOI.get(
    b::GeoMeanBridge{T,F},
    ::MOI.ListOfConstraintIndices{F,MOI.LessThan{T}},
) where {T,F}
    return [b.t_upper_bound_constraint]
end

function MOI.get(
    b::GeoMeanBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.RotatedSecondOrderCone},
) where {T,F,G}
    return copy(b.rsoc_constraints)
end

function MOI.get(
    b::GeoMeanBridge{T,F,G,H},
    ::MOI.ListOfConstraintIndices{H,MOI.Nonnegatives},
) where {T,F,G,H}
    if b.d > 2
        return MOI.ConstraintIndex{H,MOI.Nonnegatives}[]
    end
    return [b.x_nonnegative_constraint]
end

function MOI.delete(model::MOI.ModelLike, bridge::GeoMeanBridge)
    MOI.delete(model, bridge.xij)
    MOI.delete(model, bridge.t_upper_bound_constraint)
    MOI.delete(model, bridge.rsoc_constraints)
    if bridge.d == 2
        MOI.delete(model, bridge.x_nonnegative_constraint)
    end
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::GeoMeanBridge)
    return MOI.GeometricMeanCone(bridge.d)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::GeoMeanBridge{T,F,G,H},
) where {T,F,G,H}
    d = bridge.d
    f_scalars = Vector{MOIU.scalar_type(H)}(undef, bridge.d)
    tub = MOI.get(model, attr, bridge.t_upper_bound_constraint)
    rhs = MOI.constant(
        MOI.get(model, MOI.ConstraintSet(), bridge.t_upper_bound_constraint),
    )
    tub = MOIU.operate(-, T, tub, rhs)
    if d == 2
        x = MOI.get(model, attr, bridge.x_nonnegative_constraint)
        f_scalars[2] = MOIU.eachscalar(x)[1]
        f_scalars[1] = MOIU.operate(+, T, tub, f_scalars[2])
    else
        t = MOIU.remove_variable(tub, bridge.xij[1])
        f_scalars[1] = t
        n = d - 1
        l = _ilog2(n)
        num_lvars = 1 << (l - 1)
        offset = num_lvars - 1
        for j in 1:num_lvars
            if 2j <= bridge.d
                func = MOI.get(model, attr, bridge.rsoc_constraints[offset+j])
                func_scalars = MOIU.eachscalar(func)
                # Numerical issues can arise when a VectorOfVariables function
                # is turned into a VectorAffineFunction, because the RSOC
                # constraints might return something like `0.999999999x` instead
                # of exactly `1.0x`. To counteract this, use `convert_approx`.
                f_scalars[2j] = MOI.Utilities.convert_approx(
                    MOI.Utilities.scalar_type(H),
                    func_scalars[1],
                )
                if 2j + 1 <= bridge.d
                    f_scalars[2j+1] = MOI.Utilities.convert_approx(
                        MOI.Utilities.scalar_type(H),
                        func_scalars[2],
                    )
                end
            end
        end
    end
    return MOIU.vectorize(f_scalars)
end

function _getconstrattr(model, attr, bridge::GeoMeanBridge{T}) where {T}
    output = Vector{T}(undef, bridge.d)
    output[1] = MOI.get(model, attr, bridge.t_upper_bound_constraint)
    if bridge.d == 2
        output[2] = MOI.get(model, attr, bridge.x_nonnegative_constraint)[1]
    else
        N = length(bridge.xij) + 1
        # div(N, 2) gets layer before original problem variables are involved
        offset = div(N, 2) - 1 # 1 + 2 + ... + n/4
        for i in 1:(bridge.d-1)
            j = ((i - 1) >> 1) + 1
            k = i - 2(j - 1)
            output[1+i] =
                MOI.get(model, attr, bridge.rsoc_constraints[offset+j])[k]
        end
    end
    return output
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::GeoMeanBridge,
)
    output = _getconstrattr(model, attr, bridge)
    N = length(bridge.xij) + 1
    # the constraint is t - x_l1/sqrt(2^l) ≤ 0, we need to add the value of x_l1
    if bridge.d == 2
        output[1] += MOI.get(model, attr, bridge.x_nonnegative_constraint)[1]
    else
        output[1] +=
            MOI.get(model, MOI.VariablePrimal(), bridge.xij[1]) / sqrt(N)
    end
    return output
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::GeoMeanBridge,
)
    output = _getconstrattr(model, attr, bridge)
    if bridge.d == 2
        # The transformation is:
        # [t]                [1 -1] [t]
        # [x] in GeoMean <=> [0  1] [x] in R- x R+
        # Hence the dual transformation is
        # [ 1 0] [u]                 [u]
        # [-1 1] [v] in GeoMean* <=> [v] in R- x R+
        output[2] -= MOI.get(model, attr, bridge.t_upper_bound_constraint)
    end
    # Otherwise, the transformation is:
    # [t]                                  [t]
    # [x] in GeoMean <=> exists xij s.t. A [x] + B xij in R- x RSOC^(N-1)
    # Hence the dual transformation is
    # A' y in GeoMean* <=>
    # B' y = 0         <=> y in R- x RSOC^(N-1)
    # Here A has at most one nonzero entry by line and column and this entry is 1.
    # Therefore we can A' = inv(A) hence we can use the same `getconstrattr` function
    # than for getting the `ConstraintPrimal`.
    # We don't need to use B' y = 0.
    return output
end
