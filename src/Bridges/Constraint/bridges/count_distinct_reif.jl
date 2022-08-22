# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    ReifiedCountDistinctToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`ReifiedCountDistinctToMILPBridge` implements the following reformulation:

  * ``r \\iff (n, x) \\in \\textsf{CountDistinct}(1+d)`` into a mixed-integer
    linear program.

## Reformulation

The reformulation is non-trivial, and it depends on the finite domain of each
variable ``x_i``, which we as define ``S_i = \\{l_i,\\ldots,u_i\\}``.

First, we introduce new binary variables ``z_{ij}``, which are ``1`` if variable
``x_i`` takes the value ``j`` in the optimal solution and ``0`` otherwise:
```math
\\begin{aligned}
z_{ij} \\in \\{0, 1\\}                              & \\;\\; \\forall i \\in 1\\ldots d, j \\in S_i  \\\\
x_i - \\sum\\limits_{j\\in S_i} j \\cdot z_{ij} = 0 & \\;\\; \\forall i \\in 1\\ldots d              \\\\
\\sum\\limits_{j\\in S_i} z_{ij} = 1                & \\;\\; \\forall i \\in 1\\ldots d              \\\\
\\end{aligned}
```

Then, we introduce new binary variables ``y_j``, which are ``1`` if a variable
takes the value ``j`` in the optimal solution and ``0`` otherwise.
```math
\\begin{aligned}
y_{j} \\in \\{0, 1\\}                        & \\;\\; \\forall j \\in \\bigcup_{i=1,\\ldots,d} S_i \\\\
y_j \\le \\sum\\limits_{i \\in 1\\ldots d: j \\in S_i} z_{ij} \\le M y_j & \\;\\; \\forall j \\in \\bigcup_{i=1,\\ldots,d} S_i\\\\
\\end{aligned}
```

Finally, ``n`` is constrained to be the number of ``y_j`` elements that are
non-zero, with some slack:
```math
n - \\sum\\limits_{j \\in \\bigcup_{i=1,\\ldots,d} S_i} y_{j} = \\delta^+ - \\delta^-
```
And then the slack is constrained to respect the reif variable ``r``:
```math
\\begin{aligned}
d_1 \\le \\delta^+ \\le M d_1 \\\\
d_2 \\le \\delta^- \\le M d_s \\\\
d_1 + d_2 + r = 1             \\\\
d_1, d_2 \\in \\{0, 1\\}
\\end{aligned}
```

## Source node

`ReifiedCountDistinctToMILPBridge` supports:

  * `F` in [`MOI.Reified{MOI.CountDistinct}`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`ReifiedCountDistinctToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
mutable struct ReifiedCountDistinctToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    # A mix of z and α, which are added as needed. We need to store the vector
    # so we can delete them later. The exact structure of which index maps to
    # which variable doesn't matter.
    variables::Vector{MOI.VariableIndex}
    # ∑_j a_j + -1.0 * n - δ == 0.0
    # x_i - ∑_j z_ij = 0 ∀i
    # ∑_j z_ij = 1 ∀i
    equal_to::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # ∑_i z_ij - |I| α_j <= 0 ∀j
    # α_j - ∑_i z_ij <= 0 ∀j
    #  δ + M * r <= M
    # -δ + M * r <= M
    less_than::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    function ReifiedCountDistinctToMILPBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
    ) where {T}
        return new{T,typeof(f)}(
            f,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
        )
    end
end

const ReifiedCountDistinctToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{ReifiedCountDistinctToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{ReifiedCountDistinctToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.Reified{MOI.CountDistinct},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    # !!! info
    #     Postpone creation until final_touch.
    return ReifiedCountDistinctToMILPBridge{T}(f)
end

function MOI.supports_constraint(
    ::Type{<:ReifiedCountDistinctToMILPBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.Reified{MOI.CountDistinct}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:ReifiedCountDistinctToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:ReifiedCountDistinctToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:ReifiedCountDistinctToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.Reified{MOI.CountDistinct}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return ReifiedCountDistinctToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::ReifiedCountDistinctToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::ReifiedCountDistinctToMILPBridge,
)
    return MOI.Reified(MOI.CountDistinct(MOI.output_dimension(bridge.f) - 1))
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::ReifiedCountDistinctToMILPBridge,
)
    for ci in bridge.equal_to
        MOI.delete(model, ci)
    end
    empty!(bridge.equal_to)
    for ci in bridge.less_than
        MOI.delete(model, ci)
    end
    empty!(bridge.less_than)
    for x in bridge.variables
        MOI.delete(model, x)
    end
    empty!(bridge.variables)
    return
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return length(bridge.variables)
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge,
    ::MOI.ListOfVariableIndices,
)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.variables) - 2
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}[
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value) for
        x in bridge.variables[1:end-2]
    ]
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.equal_to)
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.equal_to)
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.less_than)
end

function MOI.get(
    bridge::ReifiedCountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.less_than)
end

MOI.Bridges.needs_final_touch(::ReifiedCountDistinctToMILPBridge) = true

# We use the bridge as the first argument to avoid type piracy of other methods.
function _get_bounds(
    bridge::ReifiedCountDistinctToMILPBridge{T},
    model::MOI.ModelLike,
    bounds::Dict{MOI.VariableIndex,NTuple{2,T}},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    lb = ub = f.constant
    for term in f.terms
        ret = _get_bounds(bridge, model, bounds, term.variable)
        if ret === nothing
            return nothing
        end
        lb += term.coefficient * ret[1]
        ub += term.coefficient * ret[2]
    end
    return lb, ub
end

# We use the bridge as the first argument to avoid type piracy of other methods.
function _get_bounds(
    ::ReifiedCountDistinctToMILPBridge{T},
    model::MOI.ModelLike,
    bounds::Dict{MOI.VariableIndex,NTuple{2,T}},
    x::MOI.VariableIndex,
) where {T}
    if haskey(bounds, x)
        return bounds[x]
    end
    ret = MOI.Utilities.get_bounds(model, T, x)
    if ret == (typemin(T), typemax(T))
        return nothing
    end
    bounds[x] = ret
    return ret
end

function MOI.Bridges.final_touch(
    bridge::ReifiedCountDistinctToMILPBridge{T,F},
    model::MOI.ModelLike,
) where {T,F}
    # Clear any existing reformulations!
    MOI.delete(model, bridge)
    S = Dict{T,Vector{MOI.VariableIndex}}()
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    bounds = Dict{MOI.VariableIndex,NTuple{2,T}}()
    for i in 3:length(scalars)
        x = scalars[i]
        ret = _get_bounds(bridge, model, bounds, x)
        if ret === nothing
            error(
                "Unable to use ReifiedCountDistinctToMILPBridge because " *
                "element $i in the function has a non-finite domain: $x",
            )
        end
        unit_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        convex_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        for xi in ret[1]::T:ret[2]::T
            new_var, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
            push!(bridge.variables, new_var)
            if !haskey(S, xi)
                S[xi] = MOI.VariableIndex[]
            end
            push!(S[xi], new_var)
            push!(unit_f.terms, MOI.ScalarAffineTerm(T(-xi), new_var))
            push!(convex_f.terms, MOI.ScalarAffineTerm(one(T), new_var))
        end
        push!(
            bridge.equal_to,
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate(+, T, x, unit_f),
                MOI.EqualTo(zero(T));
                allow_modify_function = true,
            ),
        )
        push!(
            bridge.equal_to,
            MOI.add_constraint(model, convex_f, MOI.EqualTo(one(T))),
        )
    end
    count_terms = MOI.ScalarAffineTerm{T}[]
    # We use a sort so that the model order is deterministic.
    for s in sort!(collect(keys(S)))
        terms = S[s]
        new_var, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
        push!(bridge.variables, new_var)
        push!(count_terms, MOI.ScalarAffineTerm(one(T), new_var))
        big_M_terms = [MOI.ScalarAffineTerm(T(1), z) for z in terms]
        push!(big_M_terms, MOI.ScalarAffineTerm(T(-length(terms)), new_var))
        push!(
            bridge.less_than,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(big_M_terms, zero(T)),
                MOI.LessThan(zero(T)),
            ),
        )
        big_M_terms_upper = [MOI.ScalarAffineTerm(T(-1), z) for z in terms]
        push!(big_M_terms_upper, MOI.ScalarAffineTerm(T(1), new_var))
        push!(
            bridge.less_than,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(big_M_terms_upper, zero(T)),
                MOI.LessThan(zero(T)),
            ),
        )
    end
    z = MOI.add_variables(model, 4)
    append!(bridge.variables, z)
    MOI.add_constraint(model, z[1], MOI.ZeroOne())
    MOI.add_constraint(model, z[2], MOI.ZeroOne())
    # ∑y - n - δ⁺ + δ⁻ = 0
    f_0 = MOI.ScalarAffineFunction(count_terms, zero(T))
    MOI.Utilities.operate!(-, T, f_0, scalars[2])
    MOI.Utilities.operate!(-, T, f_0, z[3])
    MOI.Utilities.operate!(+, T, f_0, z[4])
    push!(
        bridge.equal_to,
        MOI.Utilities.normalize_and_add_constraint(
            model,
            f_0,
            MOI.EqualTo(zero(T));
            allow_modify_function = true,
        ),
    )
    M = T(length(scalars) - 2)
    # δ⁺ <= M * z₁
    f_1 = MOI.Utilities.operate(-, T, z[3], M * z[1])
    push!(bridge.less_than, MOI.add_constraint(model, f_1, MOI.LessThan(T(0))))
    # z₁ <= δ⁺
    f_2 = MOI.Utilities.operate(-, T, z[1], z[3])
    push!(bridge.less_than, MOI.add_constraint(model, f_2, MOI.LessThan(T(0))))
    # δ⁻ <= M * z₂
    f_3 = MOI.Utilities.operate(-, T, z[4], M * z[2])
    push!(bridge.less_than, MOI.add_constraint(model, f_3, MOI.LessThan(T(0))))
    # z₁ <= δ⁺
    f_4 = MOI.Utilities.operate(-, T, z[2], z[4])
    push!(bridge.less_than, MOI.add_constraint(model, f_4, MOI.LessThan(T(0))))
    # z₁ + z₂ + r == 1
    f_4 = MOI.Utilities.operate(+, T, z[1], z[2], scalars[1])
    push!(bridge.equal_to, MOI.add_constraint(model, f_4, MOI.EqualTo(T(1))))
    return
end
