# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    FixParametricVariablesBridge{T,S} <: AbstractBridge
"""
struct FixParametricVariablesBridge{T,S} <: AbstractBridge
    affine_constraint::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},S}
    f::MOI.ScalarQuadraticFunction{T}
end

const FixParametricVariables{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{FixParametricVariablesBridge{T},OT}

function bridge_constraint(
    ::Type{FixParametricVariablesBridge{T,S}},
    model::MOI.ModelLike,
    f::MOI.ScalarQuadraticFunction{T},
    s::S,
) where {T,S<:MOI.AbstractScalarSet}
    affine = MOI.ScalarAffineFunction(f.affine_terms, f.constant)
    ci = MOI.add_constraint(model, affine, s)
    return FixParametricVariablesBridge{T,S}(ci, f)
end

function MOI.supports_constraint(
    ::Type{<:FixParametricVariablesBridge{T}},
	::Type{MOI.ScalarQuadraticFunction{T}},
	::Type{<:MOI.AbstractScalarSet},
) where {T}
	return true
end

function concrete_bridge_type(
    ::Type{<:FixParametricVariablesBridge},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{S},
) where {T,S<:MOI.AbstractScalarSet}
    return FixParametricVariablesBridge{T,S}
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:FixParametricVariablesBridge},
)
	return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{FixParametricVariablesBridge{T,S}},
) where {T,S}
	return Tuple{Type,Type}[(MOI.ScalarAffineFunction{T}, S)]
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::FixParametricVariablesBridge,
)
    return bridge.f
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::FixParametricVariablesBridge,
)
    return MOI.get(model, MOI.ConstraintSet(), bridge.affine_constraint)
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::FixParametricVariablesBridge,
)
    MOI.delete(model, bridge.affine_constraint)
    return
end

MOI.get(::FixParametricVariablesBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(
    ::FixParametricVariablesBridge,
    ::MOI.ListOfVariableIndices,
)
    return MOI.VariableIndex[]
end

function MOI.get(
    bridge::FixParametricVariablesBridge{T,S},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},S},
)::Int64 where {T,S}
    return 1
end

function MOI.get(
    bridge::FixParametricVariablesBridge{T,S},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},S},
) where {T,S}
    return [bridge.affine_constraint]
end

MOI.Bridges.needs_final_touch(::FixParametricVariablesBridge) = true

function _get_fix_value(model, values::Dict{MOI.VariableIndex,T}, x) where {T}
    v = get(values, x, nothing)
    if v !== nothing
        return v
    end
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}(x.value)
    if MOI.is_valid(model, ci)
        v = MOI.get(model, MOI.ConstraintSet(), ci).value
        values[x] = v
        return v
    end
    return nothing
end

function _get_affine_coefficient(f::MOI.ScalarQuadraticFunction{T}, x) where {T}
    c = zero(T)
    for t in f.affine_terms
        if t.variable == x
            c += t.coefficient
        end
    end
    return c
end

function MOI.Bridges.final_touch(
    bridge::FixParametricVariablesBridge{T,S},
    model::MOI.ModelLike,
) where {T,S}
    values = Dict{MOI.VariableIndex,T}()
    new_coefs = Dict{MOI.VariableIndex,T}()
    for term in bridge.f.quadratic_terms
        v1 = _get_fix_value(model, values, term.variable_1)
        v2 = _get_fix_value(model, values, term.variable_2)
        if v1 !== nothing
            new_coef = v1 * term.coefficient
            if haskey(new_coefs, term.variable_2)
                new_coefs[term.variable_2] += new_coef
            else
                coef = _get_affine_coefficient(bridge.f, term.variable_2)
                new_coefs[term.variable_2] = coef + new_coef
            end
        elseif v2 !== nothing
            new_coef = v2 * term.coefficient
            if haskey(new_coefs, term.variable_1)
                new_coefs[term.variable_1] += new_coef
            else
                coef = _get_affine_coefficient(bridge.f, term.variable_1)
                new_coefs[term.variable_1] = coef + new_coef
            end
        else
            error("At least one variable is not fixed")
        end
    end
    for (k, v) in new_coefs
        MOI.modify(
            model,
            bridge.affine_constraint,
            MOI.ScalarCoefficientChange(k, v),
        )
    end
    return
end