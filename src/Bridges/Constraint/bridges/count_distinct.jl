# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    CountDistinctToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`CountDistinctToMILPBridge` implements the following reformulation:

  * ``(n, x) \\in \\textsf{CountDistinct}(1+d)`` into a mixed-integer linear program.

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
non-zero:
```math
n - \\sum\\limits_{j \\in \\bigcup_{i=1,\\ldots,d} S_i} y_{j} = 0
```

## Source node

`CountDistinctToMILPBridge` supports:

  * `F` in [`MOI.CountDistinct`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`CountDistinctToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
mutable struct CountDistinctToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    # A mix of z and α, which are added as needed. We need to store the vector
    # so we can delete them later. The exact structure of which index maps to
    # which variable doesn't matter.
    variables::Vector{MOI.VariableIndex}
    # ∑_j a_j + -1.0 * n == 0.0
    # x_i - ∑_j z_ij = 0 ∀i
    # ∑_j z_ij = 1 ∀i
    equal_to::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # ∑_i z_ij - |I| α_j <= 0 ∀j
    # α_j - ∑_i z_ij <= 0 ∀j
    less_than::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    bounds::Vector{NTuple{2,T}}
    function CountDistinctToMILPBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
    ) where {T}
        return new{T,typeof(f)}(
            f,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
            NTuple{2,T}[],
        )
    end
end

const CountDistinctToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{CountDistinctToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{CountDistinctToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.CountDistinct,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    # !!! info
    #     Postpone creation until final_touch.
    return CountDistinctToMILPBridge{T}(f)
end

function MOI.supports_constraint(
    ::Type{<:CountDistinctToMILPBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.CountDistinct},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:CountDistinctToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:CountDistinctToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:CountDistinctToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.CountDistinct},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return CountDistinctToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::CountDistinctToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::CountDistinctToMILPBridge,
)
    return MOI.CountDistinct(MOI.output_dimension(bridge.f))
end

function MOI.delete(model::MOI.ModelLike, bridge::CountDistinctToMILPBridge)
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
    empty!(bridge.bounds)
    return
end

function MOI.get(
    bridge::CountDistinctToMILPBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::CountDistinctToMILPBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.variables)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}[
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value) for
        x in bridge.variables
    ]
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.equal_to)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.equal_to)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.less_than)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.less_than)
end

MOI.Bridges.needs_final_touch(::CountDistinctToMILPBridge) = true

# We use the bridge as the first argument to avoid type piracy of other methods.
function _get_bounds(
    bridge::CountDistinctToMILPBridge{T},
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
    ::CountDistinctToMILPBridge{T},
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
    bridge::CountDistinctToMILPBridge{T,F},
    model::MOI.ModelLike,
) where {T,F}
    S = Dict{T,Vector{MOI.VariableIndex}}()
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    bounds = Dict{MOI.VariableIndex,NTuple{2,T}}()
    for i in 2:length(scalars)
        x = scalars[i]
        ret = _get_bounds(bridge, model, bounds, x)
        if ret === nothing
            error(
                "Unable to use CountDistinctToMILPBridge because element $i " *
                "in the function has a non-finite domain: $x",
            )
        end
        if length(bridge.bounds) < i
            # This is the first time calling final_touch
            push!(bridge.bounds, ret)
        elseif bridge.bounds[i] == ret
            # We've called final_touch before, and the bounds match. No need to
            # reformulate a second time.
            continue
        elseif bridge.bounds[i] != ret
            # There is a stored bound, and the current bounds do not match. This
            # means the model has been modified since the previous call to
            # final_touch. We need to delete the bridge and start again.
            MOI.delete(model, bridge)
            MOI.Bridges.final_touch(bridge, model)
            return
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
    if isempty(S)
        return  # Nothing to bridge. We must have already called final_touch.
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
    count_f = MOI.ScalarAffineFunction(count_terms, zero(T))
    MOI.Utilities.operate!(-, T, count_f, scalars[1])
    push!(
        bridge.equal_to,
        MOI.Utilities.normalize_and_add_constraint(
            model,
            count_f,
            MOI.EqualTo(zero(T));
            allow_modify_function = true,
        ),
    )
    return
end
