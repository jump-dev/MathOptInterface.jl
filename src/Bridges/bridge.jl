# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractBridge end

An abstract type representing a bridged constraint or variable in a
[`MathOptInterface.Bridges.AbstractBridgeOptimizer`](@ref).

All bridges must implement:

 * [`added_constrained_variable_types`](@ref)
 * [`added_constraint_types`](@ref)
 * [`MOI.get(::AbstractBridge, ::MOI.NumberOfVariables)`](@ref)
 * [`MOI.get(::AbstractBridge, ::MOI.ListOfVariableIndices)`](@ref)
 * [`MOI.get(::AbstractBridge, ::MOI.NumberOfConstraints)`](@ref)
 * [`MOI.get(::AbstractBridge, ::MOI.ListOfConstraintIndices)`](@ref)

Subtypes of `AbstractBridge` may have additional requirements. Consult their
docstrings for details.

In addition, all subtypes may optionally implement the following constraint
attributes with the bridge in place of the constraint index:

 * [`MathOptInterface.ConstraintDual`](@ref)
 * [`MathOptInterface.ConstraintPrimal`](@ref)
"""
abstract type AbstractBridge end

"""
    IndexInVector

Index of variable in vector of variables.
"""
struct IndexInVector
    value::Int
end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables)::Int64

Return the number of variables created by the bridge `b` in the model.

See also [`MOI.NumberOfConstraints`](@ref).

## Implementation notes

 * There is a default fallback, so you need only implement this if the bridge
   adds new variables.
"""
MOI.get(::AbstractBridge, ::MOI.NumberOfVariables)::Int64 = 0

"""
    MOI.get(b::AbstractBridge, ::MOI.ListOfVariableIndices)

Return the list of variables created by the bridge `b`.

See also [`MOI.ListOfVariableIndices`](@ref).

## Implementation notes

 * There is a default fallback, so you need only implement this if the bridge
   adds new variables.
"""
MOI.get(::AbstractBridge, ::MOI.ListOfVariableIndices) = MOI.VariableIndex[]

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F,S})::Int64 where {F,S}

Return the number of constraints of the type `F`-in-`S` created by the bridge
`b`.

See also [`MOI.NumberOfConstraints`](@ref).

## Implementation notes

 * There is a default fallback, so you need only implement this for the
   constraint types returned by [`added_constraint_types`](@ref).
"""
MOI.get(::AbstractBridge, ::MOI.NumberOfConstraints)::Int64 = 0

"""
    MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintIndices{F,S}) where {F,S}

Return a  `Vector{ConstraintIndex{F,S}}` with indices of all constraints of
type `F`-in-`S` created by the bride `b`.

See also [`MOI.ListOfConstraintIndices`](@ref).

## Implementation notes

 * There is a default fallback, so you need only implement this for the
   constraint types returned by [`added_constraint_types`](@ref).
"""
function MOI.get(
    ::AbstractBridge,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    return MOI.ConstraintIndex{F,S}[]
end

"""
    MOI.supports(
        model::MOI.ModelLike,
        attr::MOI.AbstractConstraintAttribute,
        BT::Type{<:AbstractBridge},
    )

Return a `Bool` indicating whether `BT` supports setting `attr` to `model`.
"""
function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.AbstractConstraintAttribute,
    ::Type{<:AbstractBridge},
)
    return false
end

"""
    function MOI.get(
        model::MOI.ModelLike,
        attr::MOI.AbstractConstraintAttribute,
        bridge::AbstractBridge,
    )

Return the value of the attribute `attr` of the model `model` for the
constraint bridged by `bridge`.
"""
function MOI.get(
    ::MOI.ModelLike,
    attr::MOI.AbstractConstraintAttribute,
    bridge::AbstractBridge,
)
    return throw(
        ArgumentError(
            "Bridge of type `$(typeof(bridge))` does not support accessing " *
            "the attribute `$attr`. If you encountered this error " *
            "unexpectedly, it probably means your model has been " *
            "reformulated using the bridge, and you are attempting to query " *
            "an attribute that we haven't implemented yet for this bridge. " *
            "Please open an issue at https://github.com/jump-dev/MathOptInterface.jl/issues/new " *
            "and provide a reproducible example explaining what you were " *
            "trying to do.",
        ),
    )
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.CanonicalConstraintFunction,
    bridge::AbstractBridge,
)
    return MOI.Utilities.canonical(
        MOI.get(model, MOI.ConstraintFunction(), bridge),
    )
end

"""
    function MOI.set(
        model::MOI.ModelLike,
        attr::MOI.AbstractConstraintAttribute,
        bridge::AbstractBridge,
        value,
    )

Set the value of the attribute `attr` of the model `model` for the
constraint bridged by `bridge`.
"""
function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.AbstractConstraintAttribute,
    bridge::AbstractBridge,
    value,
)
    if MOI.is_copyable(attr) && !MOI.supports(model, attr, typeof(bridge))
        throw(MOI.UnsupportedAttribute(attr))
    else
        throw(MOI.SetAttributeNotAllowed(attr))
    end
end

"""
    added_constrained_variable_types(
        BT::Type{<:AbstractBridge},
    )::Vector{Tuple{Type}}

Return a list of the types of constrained variables that bridges of concrete
type `BT` add.

## Implementation notes

 * This method depends only on the type of the bridge, not the runtime value.

## Example

```jldoctest; setup=(using MathOptInterface; const MOI = MathOptInterface)
julia> MOI.Bridges.added_constrained_variable_types(
           MOI.Bridges.Variable.NonposToNonnegBridge{Float64},
       )
1-element Vector{Tuple{Type}}:
 (MathOptInterface.Nonnegatives,)
```
"""
function added_constrained_variable_types end

"""
    added_constraint_types(
        BT::Type{<:AbstractBridge},
    )::Vector{Tuple{Type,Type}}

Return a list of the types of constraints that bridges of concrete type `BT`
add.

## Implementation notes

 * This method depends only on the type of the bridge, not the runtime value.

## Example

```jldoctest; setup=(using MathOptInterface; const MOI = MathOptInterface)
julia> MOI.Bridges.added_constraint_types(
           MOI.Bridges.Constraint.ZeroOneBridge{Float64},
       )
2-element Vector{Tuple{Type, Type}}:
 (MathOptInterface.VariableIndex, MathOptInterface.Interval{Float64})
 (MathOptInterface.VariableIndex, MathOptInterface.Integer)
```
"""
function added_constraint_types end

"""
    set_objective_function_type(
        BT::Type{<:Objective.AbstractBridge},
    )::Type{<:MOI.AbstractScalarFunction}

Return the type of objective function that bridges of concrete type `BT`
set.

## Implementation notes

 * This method depends only on the type of the bridge, not the runtime value.

## Example

```jldoctest; setup=(using MathOptInterface; const MOI = MathOptInterface)
julia> MOI.Bridges.set_objective_function_type(
           MOI.Bridges.Objective.FunctionizeBridge{Float64},
       )
MathOptInterface.ScalarAffineFunction{Float64}
```
"""
function set_objective_function_type end

"""
    needs_final_touch(bridge::AbstractBridge)::Bool

Return whether [`MOI.Utilities.final_touch`](@ref) is implemented by `bridge`.

For example, if the correctness of `bridge` depends on the bounds of a variable
or the fact that variables are integer, then the bridge can implement
[`MOI.Utilities.final_touch`](@ref) to check assumptions immediately before a
call to [`MOI.optimize!](@ref).
"""
needs_final_touch(::AbstractBridge) = false
