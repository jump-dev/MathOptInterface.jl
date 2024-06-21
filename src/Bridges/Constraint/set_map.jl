# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type MultiSetMapBridge{T,S1,G} <: AbstractBridge end

Same as `SetMapBridge` but the output constraint type does not only depend on
the input constraint type.

When subtyping `MultiSetMapBridge`, `added_constraint_types` and `supports`
should additionally be implemented by the bridge.

For example, if a bridge `BridgeType` may create either a constraint of type
`F2`-in-`S2` or `F3`-in-`S3`, these methods should be implemented as follows:
```julia
function MOI.Bridges.added_constraint_types(
    ::Type{<:BridgeType{T,F2,F3}},
) where {T,F2,F3}
    return Tuple{Type,Type}[(F2, S2), (F3, S3)]
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:BridgeType{T,F2,F3}},
) where {T,F2,F3}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F2,S2}) ||
           MOI.supports(model, attr, MOI.ConstraintIndex{F3,S3})
end
```
"""
abstract type MultiSetMapBridge{T,S1,G} <: AbstractBridge end

function bridge_constraint(
    BT::Type{<:MultiSetMapBridge{T,S1,G}},
    model::MOI.ModelLike,
    func::G,
    set::S1,
) where {T,S1,G}
    mapped_func = MOI.Bridges.map_function(BT, func)
    mapped_set = MOI.Bridges.map_set(BT, set)
    constraint = MOI.add_constraint(model, mapped_func, mapped_set)
    return BT(constraint)
end

function MOI.supports_constraint(
    ::Type{<:MultiSetMapBridge{T,S1}},
    ::Type{F},
    ::Type{S1},
) where {T,S1<:MOI.AbstractScalarSet,F<:MOI.AbstractScalarFunction}
    return !MOI.Utilities.is_complex(F)
end

function MOI.supports_constraint(
    ::Type{<:MultiSetMapBridge{T,S1}},
    ::Type{F},
    ::Type{S1},
) where {T,S1<:MOI.AbstractVectorSet,F<:MOI.AbstractVectorFunction}
    return !MOI.Utilities.is_complex(F)
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:MultiSetMapBridge},
)
    return Tuple{Type}[]
end

# Attributes, Bridge acting as a model

function MOI.get(
    bridge::MultiSetMapBridge,
    ::MOI.NumberOfConstraints{F,S},
)::Int64 where {F,S}
    return bridge.constraint isa MOI.ConstraintIndex{F,S} ? 1 : 0
end

function MOI.get(
    bridge::MultiSetMapBridge,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    if bridge.constraint isa MOI.ConstraintIndex{F,S}
        return MOI.ConstraintIndex{F,S}[bridge.constraint]
    end
    return MOI.ConstraintIndex{F,S}[]
end

# References

function MOI.delete(model::MOI.ModelLike, bridge::MultiSetMapBridge)
    MOI.delete(model, bridge.constraint)
    return
end

# Attributes, Bridge acting as a constraint

# MapNotInvertible is thrown if the bridge does not support inverting the
# function. The user doesn't need to know this, only that they cannot get the
# attribute. Throwing `GetAttributeNotAllowed` allows `CachingOptimizer` to fall
# back to using the cache.
function _not_invertible_error_message(attr, message)
    s = "Cannot get `$attr` as the constraint is reformulated through a linear transformation that is not invertible."
    if isempty(message)
        return s
    end
    return s * " " * message
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::MultiSetMapBridge{T,S1,G},
) where {T,S1,G}
    mapped_func = MOI.get(model, attr, bridge.constraint)
    func = try
        MOI.Bridges.inverse_map_function(bridge, mapped_func)
    catch err
        if err isa MOI.Bridges.MapNotInvertible
            msg = _not_invertible_error_message(attr, err.message)
            throw(MOI.GetAttributeNotAllowed(attr, msg))
        end
        rethrow(err)
    end
    return MOI.Utilities.convert_approx(G, func)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::MultiSetMapBridge{T,S1,G},
    func::G,
) where {T,S1,G}
    new_f = MOI.Bridges.map_function(typeof(bridge), func)
    MOI.set(model, attr, bridge.constraint, new_f)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::MultiSetMapBridge,
)
    set = MOI.get(model, attr, bridge.constraint)
    return MOI.Bridges.inverse_map_set(bridge, set)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    bridge::MultiSetMapBridge{T,S1},
    set::S1,
) where {T,S1}
    new_set = MOI.Bridges.map_set(bridge, set)
    MOI.set(model, attr, bridge.constraint, new_set)
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::MultiSetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    if value === nothing
        return nothing
    end
    try
        return MOI.Bridges.inverse_map_function(bridge, value)
    catch err
        # MapNotInvertible is thrown if the bridge does not support inverting
        # the function. The user doesn't need to know this, only that they
        # cannot get the attribute.
        if err isa MOI.Bridges.MapNotInvertible
            msg = _not_invertible_error_message(attr, err.message)
            throw(MOI.GetAttributeNotAllowed(attr, msg))
        end
        rethrow(err)
    end
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimalStart,
    bridge::MultiSetMapBridge,
    value,
)
    if value === nothing
        MOI.set(model, attr, bridge.constraint, nothing)
    else
        mapped_value = MOI.Bridges.map_function(bridge, value)
        MOI.set(model, attr, bridge.constraint, mapped_value)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::MultiSetMapBridge,
)
    value = MOI.get(model, attr, bridge.constraint)
    if value === nothing
        return nothing
    end
    return MOI.Bridges.adjoint_map_function(bridge, value)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDualStart,
    bridge::BT,
    value,
) where {BT<:MultiSetMapBridge}
    if value === nothing
        MOI.set(model, attr, bridge.constraint, nothing)
    else
        mapped_value = try
            MOI.Bridges.inverse_adjoint_map_function(bridge, value)
        catch err
            if err isa MOI.Bridges.MapNotInvertible
                msg = _not_invertible_error_message(attr, err.message)
                throw(MOI.SetAttributeNotAllowed(attr, msg))
            end
            rethrow(err)
        end
        MOI.set(model, attr, bridge.constraint, mapped_value)
    end
    return
end

# By linearity of the map, we can just change the constant/coefficient

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.ScalarConstantChange,
) where {BT<:MultiSetMapBridge}
    new_constant = MOI.Bridges.map_function(BT, change.new_constant)
    MOI.modify(model, bridge.constraint, MOI.ScalarConstantChange(new_constant))
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.VectorConstantChange,
) where {BT<:MultiSetMapBridge}
    new_constant = MOI.Bridges.map_function(BT, change.new_constant)
    MOI.modify(model, bridge.constraint, MOI.VectorConstantChange(new_constant))
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.ScalarCoefficientChange,
) where {BT<:MultiSetMapBridge}
    new_coefficient = MOI.Bridges.map_function(BT, change.new_coefficient)
    new_change = MOI.ScalarCoefficientChange(change.variable, new_coefficient)
    MOI.modify(model, bridge.constraint, new_change)
    return
end

function MOI.modify(
    model::MOI.ModelLike,
    bridge::BT,
    change::MOI.MultirowChange,
) where {BT<:MultiSetMapBridge}
    # It is important here that `change.new_coefficients` contains the complete
    # new sparse column associated to the variable. Calling modify twice with
    # part of the column won't work since  the linear map might reset all the
    # column each time.
    coefficients = MOI.Bridges.map_function(BT, change.new_coefficients)
    new_change = MOI.MultirowChange(change.variable, coefficients)
    MOI.modify(model, bridge.constraint, new_change)
    return
end

"""
    abstract type SetMapBridge{T,S2,S1,F,G} <: MultiSetMapBridge{T,S1,G} end

Consider two type of sets, `S1` and `S2`, and a linear mapping `A` such that
the image of a set of type `S1` under `A` is a set of type `S2`.

A `SetMapBridge{T,S2,S1,F,G}` is a bridge that maps `G`-in-`S1` constraints
into `F`-in-`S2` by mapping the function through `A`.

The linear map `A` is described by;

 * [`MOI.Bridges.map_set`](@ref)
 * [`MOI.Bridges.map_function`](@ref).

Implementing a method for these two functions is sufficient to bridge
constraints. However, in order for the getters and setters of attributes such as
dual solutions and starting values to work as well, a method for the following
functions must be implemented:

 * [`MOI.Bridges.inverse_map_set`](@ref)
 * [`MOI.Bridges.inverse_map_function`](@ref)
 * [`MOI.Bridges.adjoint_map_function`](@ref)
 * [`MOI.Bridges.inverse_adjoint_map_function`](@ref)

See the docstrings of each function to see which feature would be missing if it
was not implemented for a given bridge.
"""
abstract type SetMapBridge{T,S2,S1,F,G} <: MultiSetMapBridge{T,S1,G} end

function MOI.Bridges.added_constraint_types(
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return Tuple{Type,Type}[(F, S2)]
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:SetMapBridge{T,S2,S1,F}},
) where {T,S2,S1,F}
    return MOI.supports(model, attr, MOI.ConstraintIndex{F,S2})
end
