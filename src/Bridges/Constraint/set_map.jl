# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type SetMapBridge{T,S2,S1,F,G} <: AbstractBridge end

Consider two type of sets, `S1` and `S2`, and a linear mapping `A` such that
the image of a set of type `S1` under `A` is a set of type `S2`.

A `SetMapBridge{T,S2,S1,F,G}` is a bridge that maps `G`-in-`S2` constraints
into `F`-in-`S1` by mapping the function through `A`.

The linear map `A` is described by;

 * [`MathOptInterface.Bridges.map_set`](@ref)
 * [`MathOptInterface.Bridges.map_function`](@ref).

Implementing a method for these two functions is sufficient to bridge
constraints. However, in order for the getters and setters of attributes such as
dual solutions and starting values to work as well, a method for the following
functions must be implemented:

 * [`MathOptInterface.Bridges.inverse_map_set`](@ref)
 * [`MathOptInterface.Bridges.inverse_map_function`](@ref)
 * [`MathOptInterface.Bridges.adjoint_map_function`](@ref)
 * [`MathOptInterface.Bridges.inverse_adjoint_map_function`](@ref)

See the docstrings of each function to see which feature would be missing if it
was not implemented for a given bridge.
"""
abstract type SetMapBridge{T,S2,S1,F,G} <: AbstractBridge end

function bridge_constraint(
    BT::Type{<:SetMapBridge{T,S2,S1,F,G}},
    model::MOI.ModelLike,
    func::G,
    set::S1,
) where {T,S2,S1,F,G}
    mapped_func = MOI.Bridges.map_function(BT, func)
    mapped_set = MOI.Bridges.map_set(BT, set)
    constraint = MOI.add_constraint(model, mapped_func, mapped_set)
    return BT(constraint)
end

function MOI.supports_constraint(
    ::Type{<:SetMapBridge{T,S2,S1}},
    ::Type{<:MOI.AbstractScalarFunction},
    ::Type{S1},
) where {T,S2,S1<:MOI.AbstractScalarSet}
    return true
end

function MOI.supports_constraint(
    ::Type{<:SetMapBridge{T,S2,S1}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{S1},
) where {T,S2,S1<:MOI.AbstractVectorSet}
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:SetMapBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return Tuple{Type,Type}[(F, S2)]
end

# Attributes, Bridge acting as a model

function MOI.get(
    ::SetMapBridge{T,S2,S1,F},
    ::MOI.NumberOfConstraints{F,S2},
)::Int64 where {T,S2,S1,F}
    return 1
end

function MOI.get(
    bridge::SetMapBridge{T,S2,S1,F},
    ::MOI.ListOfConstraintIndices{F,S2},
) where {T,S2,S1,F}
    return [bridge.constraint]
end

# References

function MOI.delete(model::MOI.ModelLike, bridge::SetMapBridge)
    MOI.delete(model, bridge.constraint)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SetMapBridge{T,S2,S1,F,G},
) where {T,S2,S1,F,G}
    mapped_func = MOI.get(model, attr, bridge.constraint)
    func = MOI.Bridges.inverse_map_function(typeof(bridge), mapped_func)
    return MOI.Utilities.convert_approx(G, func)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SetMapBridge{T,S2,S1,F,G},
    func::G,
) where {T,S2,S1,F,G}
    new_f = MOI.Bridges.map_function(typeof(bridge), func)
    MOI.set(model, attr, bridge.constraint, new_f)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge,
)
    set = MOI.get(model, attr, bridge.constraint)
    return MOI.Bridges.inverse_map_set(typeof(bridge), set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::SetMapBridge{T,S2,S1},
    set::S1,
) where {T,S2,S1}
    new_set = MOI.Bridges.map_set(typeof(bridge), set)
    MOI.set(model, attr, bridge.constraint, new_set)
    return
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S2})
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    if value === nothing
        return nothing
    end
    return MOI.Bridges.inverse_map_function(typeof(bridge), value)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::SetMapBridge,
    value,
)
    if value === nothing
        MOI.set(model, attr, bridge.constraint, nothing)
    else
        mapped_value = MOI.Bridges.map_function(typeof(bridge), value)
        MOI.set(model, attr, bridge.constraint, mapped_value)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::SetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    if value === nothing
        return nothing
    end
    return MOI.Bridges.adjoint_map_function(typeof(bridge), value)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::BT,
    value,
) where {BT<:SetMapBridge}
    if value === nothing
        MOI.set(model, attr, bridge.constraint, nothing)
    else
        mapped_value = MOI.Bridges.inverse_adjoint_map_function(BT, value)
        MOI.set(model, attr, bridge.constraint, mapped_value)
    end
    return
end

# By linearity of the map, we can just change the constant/coefficient

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.ScalarConstantChange,
) where {BT<:SetMapBridge}
    new_constant = MOI.Bridges.map_function(BT, change.new_constant)
    MOI.modify(model, bridge.constraint, MOI.ScalarConstantChange(new_constant))
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.VectorConstantChange,
) where {BT<:SetMapBridge}
    new_constant = MOI.Bridges.map_function(BT, change.new_constant)
    MOI.modify(model, bridge.constraint, MOI.VectorConstantChange(new_constant))
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.ScalarCoefficientChange,
) where {BT<:SetMapBridge}
    new_coefficient = MOI.Bridges.map_function(BT, change.new_coefficient)
    new_change = MOI.ScalarCoefficientChange(change.variable, new_coefficient)
    MOI.modify(model, bridge.constraint, new_change)
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.MultirowChange,
) where {BT<:SetMapBridge}
    # It is important here that `change.new_coefficients` contains the complete
    # new sparse column associated to the variable. Calling modify twice with
    # part of the column won't work since  the linear map might reset all the
    # column each time.
    coefficients = MOI.Bridges.map_function(BT, change.new_coefficients)
    new_change = MOI.MultirowChange(change.variable, coefficients)
    MOI.modify(model, bridge.constraint, new_change)
    return
end
