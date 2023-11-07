# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    GeoMeanBridge{T,F,G,H} <: Bridges.Constraint.AbstractBridge

`GeoMeanBridge` implements a reformulation from [`MOI.GeometricMeanCone`](@ref)
into [`MOI.RotatedSecondOrderCone`](@ref).

The reformulation is best described in an example.

Consider the cone of dimension 4:
```math
t \\le \\sqrt[3]{x_1 x_2 x_3}
```
This can be rewritten as ``\\exists y \\ge 0`` such that:
```math
\\begin{align*}
  t & \\le y,\\\\
  y^4 & \\le x_1 x_2 x_3 y.
\\end{align*}
```
Note that we need to create ``y`` and not use ``t^4`` directly because ``t`` is
not allowed to be negative.

This is equivalent to:
```math
\\begin{align*}
    t & \\le \\frac{y_1}{\\sqrt{4}},\\\\
    y_1^2 & \\le 2y_2 y_3,\\\\
    y_2^2 & \\le 2x_1 x_2, \\\\
    y_3^2 & \\le 2x_3(y_1/\\sqrt{4}) \\\\
    y     & \\ge 0.
\\end{align*}
```

More generally, you can show how the geometric mean code is recursively expanded
into a set of new variables ``y`` in [`MOI.Nonnegatives`](@ref), a set of
[`MOI.RotatedSecondOrderCone`](@ref) constraints, and a [`MOI.LessThan`](@ref)
constraint between ``t`` and ``y_1``.

## Source node

`GeoMeanBridge` supports:

  * `H` in [`MOI.GeometricMeanCone`](@ref)

## Target nodes

`GeoMeanBridge` creates:

  * `F` in [`MOI.LessThan{T}`](@ref)
  * `G` in [`MOI.RotatedSecondOrderCone`](@ref)
  * `G` in [`MOI.Nonnegatives`](@ref)
"""
struct GeoMeanBridge{T,F,G,H} <: AbstractBridge
    d::Int
    xij::Vector{MOI.VariableIndex}
    t_upper_bound_constraint::MOI.ConstraintIndex{F,MOI.LessThan{T}}
    rsoc_constraints::Vector{MOI.ConstraintIndex{G,MOI.RotatedSecondOrderCone}}
    # Ihe case that `d > 2`, this is `Nothing` because the new variables are
    # non-negative due to the RotatedSecondOrderCone constraint.
    x_nonnegative_constraint::Union{
        Nothing,
        MOI.ConstraintIndex{H,MOI.Nonnegatives},
    }

    function GeoMeanBridge{T,F,G,H}(
        d,
        xij,
        t_upper_bound_constraint,
        rsoc,
        x_nonnegative_constraint,
    ) where {T,F,G,H}
        return new{T,F,G,H}(
            d,
            xij,
            t_upper_bound_constraint,
            rsoc,
            x_nonnegative_constraint,
        )
    end
end

const GeoMean{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{GeoMeanBridge{T},OT}

_ilog2(n, i = 0) = n <= (1 << i) ? i : _ilog2(n, i + 1)

function bridge_constraint(
    ::Type{GeoMeanBridge{T,F,G,H}},
    model,
    f::MOI.AbstractVectorFunction,
    s::MOI.GeometricMeanCone,
) where {T,F,G,H}
    d = MOI.dimension(s)
    f_scalars = MOI.Utilities.eachscalar(f)
    if d == 2
        # We have a special case for two-dimensional set, because this doesn't
        # require adding a rotated second order cone.
        return GeoMeanBridge{T,F,G,H}(
            d,
            MOI.VariableIndex[],
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate!(-, T, f_scalars[1], f_scalars[2]),
                MOI.LessThan(zero(T));
                allow_modify_function = true,
            ),
            MOI.ConstraintIndex{G,MOI.RotatedSecondOrderCone}[],
            MOI.add_constraint(model, f_scalars[2:2], MOI.Nonnegatives(1)),
        )
    end
    t = f_scalars[1]
    SG = MOI.Utilities.scalar_type(G)
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
    t_upper_bound_constraint = MOI.Utilities.normalize_and_add_constraint(
        model,
        MOI.Utilities.operate!(+, T, t, -sN * xl1),
        MOI.LessThan(zero(T)),
        allow_modify_function = true,
    )
    offset = 0
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
                MOI.Utilities.operate(vcat, T, a, b, c),
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

function MOI.Bridges.added_constrained_variable_types(::Type{<:GeoMeanBridge})
    return Tuple{Type}[(MOI.Reals,)]
end

function MOI.Bridges.added_constraint_types(
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
    S = MOI.Utilities.scalar_type(H)
    A = MOI.Utilities.promote_operation(*, T, T, MOI.VariableIndex)
    F = MOI.Utilities.promote_operation(+, T, S, A)
    G = MOI.Utilities.promote_operation(vcat, T, A, S, MOI.VariableIndex)
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
    b::GeoMeanBridge{T,F,G,H},
    ::MOI.NumberOfConstraints{H,MOI.Nonnegatives},
)::Int64 where {T,F,G,H}
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
    f_scalars = Vector{MOI.Utilities.scalar_type(H)}(undef, bridge.d)
    tub = MOI.get(model, attr, bridge.t_upper_bound_constraint)
    rhs = MOI.constant(
        MOI.get(model, MOI.ConstraintSet(), bridge.t_upper_bound_constraint),
    )
    tub = MOI.Utilities.operate(-, T, tub, rhs)
    if d == 2
        x = MOI.get(model, attr, bridge.x_nonnegative_constraint)
        f_scalars[2] = MOI.Utilities.eachscalar(x)[1]
        f_scalars[1] = MOI.Utilities.convert_approx(
            MOI.Utilities.scalar_type(H),
            MOI.Utilities.operate(+, T, tub, f_scalars[2]),
        )
    else
        t = MOI.Utilities.remove_variable(tub, bridge.xij[1])
        f_scalars[1] = t
        n = d - 1
        l = _ilog2(n)
        num_lvars = 1 << (l - 1)
        offset = num_lvars - 1
        for j in 1:num_lvars
            if 2j <= bridge.d
                func = MOI.get(model, attr, bridge.rsoc_constraints[offset+j])
                func_scalars = MOI.Utilities.eachscalar(func)
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
    return MOI.Utilities.vectorize(f_scalars)
end

function _get_attribute(model, attr, bridge::GeoMeanBridge{T}) where {T}
    output = Vector{T}(undef, bridge.d)
    ret = MOI.get(model, attr, bridge.t_upper_bound_constraint)
    if ret === nothing
        return ret
    end
    output[1] = ret
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

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    ::Type{GeoMeanBridge{T,F,G,H}},
) where {T,F,G,H}
    FS, GS = MOI.LessThan{T}, MOI.RotatedSecondOrderCone
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,FS}) &&
           MOI.supports(model, attr, MOI.ConstraintIndex{G,GS}) &&
           MOI.supports(model, attr, MOI.ConstraintIndex{H,MOI.Nonnegatives})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::GeoMeanBridge,
)
    output = _get_attribute(model, attr, bridge)
    if output === nothing
        return nothing
    end
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

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::GeoMeanBridge{T},
    value,
) where {T}
    if bridge.d == 2
        new_value = value[1] - value[2]
        MOI.set(model, attr, bridge.t_upper_bound_constraint, new_value)
        MOI.set(model, attr, bridge.x_nonnegative_constraint, [value[2]])
        return
    end
    n = bridge.d - 1
    l = _ilog2(n)
    N = 1 << l
    sN = one(T) / sqrt(N)
    xl1 = prod(value[2:end])^(1 / n) / xN
    xij = zeros(T, N - 1)
    xij[1] = xl1
    _get_x(i) = i > n ? sN * xl1 : value[1+i]
    # With sqrt(2)^l*t - xl1, we should scale both the ConstraintPrimal and
    # ConstraintDual
    MOI.set(model, attr, bridge.t_upper_bound_constraint, value[1] - sN * xl1)
    offset = length(bridge.rsoc_constraints)
    for i in l:-1:1
        num_lvars = 1 << (i - 1)
        offset_next = offset + num_lvars
        for j in 1:num_lvars
            a, b = if i == l
                _get_x(2j - 1), _get_x(2j)
            else
                xij[offset_next+2j-1], xij[offset_next+2j]
            end
            c = sqrt(2a * b)
            xij[offset+j] = c
            MOI.set(model, attr, bridge.rsoc_constraints[offset+j], [a, b, c])
        end
        offset -= 1 << (i - 2)
    end
    @assert offset == 0
    return
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::GeoMeanBridge,
    ::Nothing,
)
    MOI.set(model, attr, bridge.t_upper_bound_constraint, nothing)
    if bridge.x_nonnegative_constraint !== nothing
        MOI.set(model, attr, bridge.x_nonnegative_constraint, nothing)
    end
    for ci in bridge.rsoc_constraints
        MOI.set(model, attr, ci, nothing)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::GeoMeanBridge,
)
    output = _get_attribute(model, attr, bridge)
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
