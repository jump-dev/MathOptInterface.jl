# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    CountDistinctToMILPBridge{T} <: Bridges.Constraint.AbstractBridge

`CountDistinctToMILPBridge` implements the following reformulation:

## Source node

`CountDistinctToMILPBridge` supports:

  * [`MOI.VectorOfVariables`] in [`MOI.AllDifferent`](@ref)

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
    count::Union{
        Nothing,
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # x_i - ∑_j z_ij = 0 ∀i
    unit_expansion::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    # ∑_i z_ij - |I| α_j <= 0 ∀j
    # α_j - ∑_i z_ij <= 0 ∀j
    big_M::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    function CountDistinctToMILPBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
    ) where {T}
        return new{T,typeof(f)}(
            f,
            MOI.VariableIndex[],
            nothing,
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
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
    if bridge.count !== nothing
        MOI.delete(model, bridge.count)
    end
    for ci in bridge.unit_expansion
        MOI.delete(model, ci)
    end
    for ci in bridge.big_M
        MOI.delete(model, ci)
    end
    for x in bridge.variables
        MOI.delete(model, x)
    end
    empty!(bridge.variables)
    bridge.count = nothing
    empty!(bridge.unit_expansion)
    empty!(bridge.big_M)
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
    return (count === nothing ? 0 : 1) + length(bridge.unit_expansion)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return vcat(bridge.count, bridge.unit_expansion)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.big_M)
end

function MOI.get(
    bridge::CountDistinctToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.big_M)
end

MOI.Bridges.needs_final_touch(::CountDistinctToMILPBridge) = true

function _get_bounds(
    ::Type{T},
    model,
    bounds,
    f::MOI.ScalarAffineFunction{T},
) where {T}
    lb = ub = f.constant
    for term in f.terms
        l, u = _get_bounds(T, model, bounds, term.variable)
        if l === nothing || u === nothing
            return nothing, nothing
        end
        lb += term.coefficient * l
        ub += term.coefficient * u
    end
    return lb, ub
end

function _get_bounds(::Type{T}, model, bounds, x::MOI.VariableIndex) where {T}
    if haskey(bounds, x)
        return bounds[x]
    end
    lb, ub = nothing, nothing
    F, f = MOI.VariableIndex, x.value
    ci = MOI.ConstraintIndex{F,MOI.GreaterThan{T}}(f)
    if MOI.is_valid(model, ci)
        lb = MOI.get(model, MOI.ConstraintSet(), ci).lower
    end
    ci = MOI.ConstraintIndex{F,MOI.LessThan{T}}(f)
    if MOI.is_valid(model, ci)
        ub = MOI.get(model, MOI.ConstraintSet(), ci).upper
    end
    ci = MOI.ConstraintIndex{F,MOI.EqualTo{T}}(f)
    if MOI.is_valid(model, ci)
        lb = ub = MOI.get(model, MOI.ConstraintSet(), ci).value
    end
    ci = MOI.ConstraintIndex{F,MOI.Interval{T}}(f)
    if MOI.is_valid(model, ci)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        lb, ub = set.lower, set.upper
    end
    bounds[x] = (lb, ub)
    return lb, ub
end

function MOI.Utilities.final_touch(
    bridge::CountDistinctToMILPBridge{T,F},
    model::MOI.ModelLike,
) where {T,F}
    if bridge.count !== nothing
        MOI.delete(model, bridge)
    end
    S = Dict{T,Vector{MOI.VariableIndex}}()
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    bounds = Dict{MOI.VariableIndex,NTuple{2,Union{T,Nothing}}}()
    for i in 2:length(scalars)
        x = scalars[i]
        lb, ub = _get_bounds(T, model, bounds, x)
        if lb === nothing || ub === nothing
            error(
                "Unable to use CountDistinctToMILPBridge because element $i " *
                "in the function has a non-finite domain: $x",
            )
        end
        unit_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        for xi in lb:ub
            new_var, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
            push!(bridge.variables, new_var)
            if !haskey(S, xi)
                S[xi] = MOI.VariableIndex[]
            end
            push!(S[xi], new_var)
            push!(unit_f.terms, MOI.ScalarAffineTerm(T(-xi), new_var))
        end
        push!(
            bridge.unit_expansion,
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate(+, T, x, unit_f),
                MOI.EqualTo(zero(T));
                allow_modify_function = true,
            ),
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
            bridge.big_M,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(big_M_terms, zero(T)),
                MOI.LessThan(zero(T)),
            ),
        )
        big_M_terms_upper = [MOI.ScalarAffineTerm(T(-1), z) for z in terms]
        push!(big_M_terms_upper, MOI.ScalarAffineTerm(T(1), new_var))
        push!(
            bridge.big_M,
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(big_M_terms_upper, zero(T)),
                MOI.LessThan(zero(T)),
            ),
        )
    end
    count_f = MOI.ScalarAffineFunction(count_terms, zero(T))
    MOI.Utilities.operate!(-, T, count_f, scalars[1])
    bridge.count = MOI.Utilities.normalize_and_add_constraint(
        model,
        count_f,
        MOI.EqualTo(zero(T));
        allow_modify_function = true,
    )
    return
end
