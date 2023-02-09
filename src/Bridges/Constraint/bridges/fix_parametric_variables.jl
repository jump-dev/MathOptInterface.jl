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
    values::Dict{MOI.VariableIndex,Union{Nothing,T}}
    new_coefs::Dict{MOI.VariableIndex,T}
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
    values = Dict{MOI.VariableIndex,Union{Nothing,T}}()
    new_coefs = Dict{MOI.VariableIndex,T}()
    for term in f.quadratic_terms
        values[term.variable_1] = nothing
        values[term.variable_2] = nothing
        new_coefs[term.variable_1] = zero(T)
        new_coefs[term.variable_2] = zero(T)
    end
    return FixParametricVariablesBridge{T,S}(ci, f, values, new_coefs)
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

function MOI.delete(model::MOI.ModelLike, bridge::FixParametricVariablesBridge)
    MOI.delete(model, bridge.affine_constraint)
    return
end

MOI.get(::FixParametricVariablesBridge, ::MOI.NumberOfVariables)::Int64 = 0

function MOI.get(::FixParametricVariablesBridge, ::MOI.ListOfVariableIndices)
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

function MOI.modify(
    model::MOI.ModelLike,
    bridge::FixParametricVariablesBridge{T,S},
    chg::MOI.ScalarCoefficientChange{T},
) where {T,S}
    MOI.modify(model, bridge.affine_constraint, chg)
    MOI.Utilities.modify_function!(bridge.f, chg)
    return
end

MOI.Bridges.needs_final_touch(::FixParametricVariablesBridge) = true

function MOI.Bridges.final_touch(
    bridge::FixParametricVariablesBridge{T,S},
    model::MOI.ModelLike,
) where {T,S}
    for x in keys(bridge.values)
        ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}(x.value)
        if MOI.is_valid(model, ci)
            bridge.values[x] = MOI.get(model, MOI.ConstraintSet(), ci).value
        else
            bridge.values[x] = nothing
        end
        new_coef = zero(T)
        for term in bridge.f.affine_terms
            if term.variable == x
                new_coef += term.coefficient
            end
        end
        bridge.new_coefs[x] = new_coef
    end
    for term in bridge.f.quadratic_terms
        v1, v2 = bridge.values[term.variable_1], bridge.values[term.variable_2]
        if v1 !== nothing
            if term.variable_1 == term.variable_2
                bridge.new_coefs[term.variable_2] += v1 * term.coefficient / 2
            else
                bridge.new_coefs[term.variable_2] += v1 * term.coefficient
            end
        elseif v2 !== nothing
            bridge.new_coefs[term.variable_1] += v2 * term.coefficient
        else
            error("At least one variable is not fixed")
        end
    end
    for (k, v) in bridge.new_coefs
        MOI.modify(
            model,
            bridge.affine_constraint,
            MOI.ScalarCoefficientChange(k, v),
        )
    end
    return
end
