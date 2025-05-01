# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Variables

"""
    struct AddVariableNotAllowed <: NotAllowedError
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that variables cannot be added to the model.
"""
struct AddVariableNotAllowed <: NotAllowedError
    message::String # Human-friendly explanation why the attribute cannot be set
end

AddVariableNotAllowed() = AddVariableNotAllowed("")

operation_name(::AddVariableNotAllowed) = "Adding variables"

"""
    add_variables(model::ModelLike, n::Int)::Vector{VariableIndex}

Add `n` scalar variables to the model, returning a vector of variable indices.

An [`AddVariableNotAllowed`](@ref) error is thrown if adding variables cannot be
done in the current state of the model `model`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> MOI.add_variables(model, 2)
2-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
```
"""
function add_variables(model::ModelLike, n)
    return VariableIndex[add_variable(model) for _ in 1:n]
end

"""
    add_variable(model::ModelLike)::VariableIndex

Add a scalar variable to the model, returning a variable index.

A [`AddVariableNotAllowed`](@ref) error is thrown if adding variables cannot be
done in the current state of the model `model`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(model)
MOI.VariableIndex(1)
```
"""
add_variable(model::ModelLike) = throw(AddVariableNotAllowed())

"""
    supports_add_constrained_variable(
        model::ModelLike,
        S::Type{<:AbstractScalarSet}
    )::Bool

Return a `Bool` indicating whether `model` supports constraining a variable
to belong to a set of type `S` either on creation of the variable with
[`add_constrained_variable`](@ref) or after the variable is created with
[`add_constraint`](@ref).

By default, this function falls back to
`supports_add_constrained_variables(model, Reals) &&
supports_constraint(model, MOI.VariableIndex, S)` which is the correct
definition for most models.

## Example

Suppose that a solver supports only two kind of variables: binary variables
and continuous variables with a lower bound. If the solver decides not to
support `VariableIndex`-in-`Binary` and `VariableIndex`-in-`GreaterThan`
constraints, it only has to implement `add_constrained_variable` for these
two sets which prevents the user to add both a binary constraint and a
lower bound on the same variable. Moreover, if the user adds a
`VariableIndex`-in-`GreaterThan` constraint, implementing this interface (that is,
`supports_add_constrained_variables`) enables the constraint to be transparently
bridged into a supported constraint.
"""
function supports_add_constrained_variable(
    model::ModelLike,
    S::Type{<:AbstractScalarSet},
)
    return supports_add_constrained_variables(model, Reals) &&
           supports_constraint(model, VariableIndex, S)
end

"""
    add_constrained_variable(
        model::ModelLike,
        set::AbstractScalarSet
    )::Tuple{MOI.VariableIndex,
             MOI.ConstraintIndex{MOI.VariableIndex, typeof(set)}}

Add to `model` a scalar variable constrained to belong to `set`, returning the
index of the variable created and the index of the constraint constraining the
variable to belong to `set`.

By default, this function falls back to creating a free variable with
[`add_variable`](@ref) and then constraining it to belong to `set` with
[`add_constraint`](@ref).
"""
function add_constrained_variable(model::ModelLike, set::AbstractScalarSet)
    variable = add_variable(model)
    constraint = add_constraint(model, variable, set)
    return variable, constraint
end

"""
    add_constrained_variable(
        model::ModelLike,
        set::Tuple{<:GreaterThan,<:LessThan},
    )

A special-case method to add a scalar variable with a lower and upper bound.

This method should be implemented by optimizers which have native support for
adding a variable with bounds and which cannot performantly modify the variable
bounds after creation.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> set = (MOI.GreaterThan(1.0), MOI.LessThan(2.0));

julia> x, (c_l, c_u) = MOI.add_constrained_variable(model, set);

julia> c_l
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.GreaterThan{Float64}}(1)

julia> c_u
MathOptInterface.ConstraintIndex{MathOptInterface.VariableIndex, MathOptInterface.LessThan{Float64}}(1)

julia> print(model)
Feasibility

Subject to:

VariableIndex-in-GreaterThan{Float64}
 v[1] >= 1.0

VariableIndex-in-LessThan{Float64}
 v[1] <= 2.0
```
"""
function add_constrained_variable(
    model::ModelLike,
    set::Tuple{<:GreaterThan,<:LessThan},
)
    set_1, set_2 = set
    x, c_1 = add_constrained_variable(model, set_1)
    c_2 = add_constraint(model, x, set_2)
    return x, (c_1, c_2)
end

function supports_add_constrained_variable(
    model::ModelLike,
    ::Type{Tuple{L,U}},
) where {L<:GreaterThan,U<:LessThan}
    return supports_add_constrained_variable(model, L) &&
           supports_constraint(model, VariableIndex, U)
end

"""
    supports_add_constrained_variables(
        model::ModelLike,
        S::Type{<:AbstractVectorSet}
    )::Bool

Return a `Bool` indicating whether `model` supports constraining a vector of
variables to belong to a set of type `S` either on creation of the vector of
variables with [`add_constrained_variables`](@ref) or after the variable is
created with [`add_constraint`](@ref).

By default, if `S` is `Reals` then this function returns `true` and otherwise,
it falls back to `supports_add_constrained_variables(model, Reals) &&
supports_constraint(model, MOI.VectorOfVariables, S)` which is the correct
definition for most models.

## Example

In the standard conic form (see [Duality](@ref)), the variables are grouped into
several cones and the constraints are affine equality constraints.
If `Reals` is not one of the cones supported by the solvers then it needs
to implement `supports_add_constrained_variables(::Optimizer, ::Type{Reals}) = false`
as free variables are not supported.
The solvers should then implement
`supports_add_constrained_variables(::Optimizer, ::Type{<:SupportedCones}) = true`
where `SupportedCones` is the union of all cone types that are supported;
it does not have to implement the method
`supports_constraint(::Type{VectorOfVariables}, Type{<:SupportedCones})`
as it should return `false` and it's the default.
This prevents the user to constrain the same variable in two different cones.
When a `VectorOfVariables`-in-`S` is added, the variables of the vector
have already been created so they already belong to given cones.
If bridges are enabled, the constraint will therefore be bridged by adding slack
variables in `S` and equality constraints ensuring that the slack variables are
equal to the corresponding variables of the given constraint function.

Note that there may also be sets for which
`!supports_add_constrained_variables(model, S)` and
`supports_constraint(model, MOI.VectorOfVariables, S)`.
For instance, suppose a solver supports positive semidefinite variable
constraints and two types of variables: binary variables and nonnegative
variables. Then the solver should support adding
`VectorOfVariables`-in-`PositiveSemidefiniteConeTriangle` constraints, but it
should not support creating variables constrained to belong to the
`PositiveSemidefiniteConeTriangle` because the variables in
`PositiveSemidefiniteConeTriangle` should first be created as either binary or
non-negative.
"""
function supports_add_constrained_variables(
    model::ModelLike,
    S::Type{<:AbstractVectorSet},
)
    return supports_add_constrained_variables(model, Reals) &&
           supports_constraint(model, VectorOfVariables, S)
end
supports_add_constrained_variables(::ModelLike, ::Type{Reals}) = true

"""
    add_constrained_variables(
        model::ModelLike,
        sets::AbstractVector{<:AbstractScalarSet}
    )::Tuple{
        Vector{MOI.VariableIndex},
        Vector{MOI.ConstraintIndex{MOI.VariableIndex,eltype(sets)}},
    }

Add to `model` scalar variables constrained to belong to `sets`, returning the
indices of the variables created and the indices of the constraints constraining
the variables to belong to each set in `sets`. That is, if it returns `variables`
and `constraints`, `constraints[i]` is the index of the constraint constraining
`variable[i]` to belong to `sets[i]`.

By default, this function falls back to calling
[`add_constrained_variable`](@ref) on each set.
"""
function add_constrained_variables(
    model::ModelLike,
    sets::AbstractVector{<:AbstractScalarSet},
)
    variables = Vector{VariableIndex}(undef, length(sets))
    constraints =
        Vector{ConstraintIndex{VariableIndex,eltype(sets)}}(undef, length(sets))
    for (i, set) in enumerate(sets)
        variables[i], constraints[i] = add_constrained_variable(model, set)
    end
    return variables, constraints
end

"""
    add_constrained_variables(
        model::ModelLike,
        set::AbstractVectorSet,
    )::Tuple{
        Vector{MOI.VariableIndex},
        MOI.ConstraintIndex{MOI.VectorOfVariables,typeof(set)},
    }

Add to `model` a vector of variables constrained to belong to `set`, returning
the indices of the variables created and the index of the constraint
constraining the vector of variables to belong to `set`.

By default, this function falls back to creating free variables with
[`add_variables`](@ref) and then constraining it to belong to `set` with
[`add_constraint`](@ref).
"""
function add_constrained_variables(model::ModelLike, set::AbstractVectorSet)
    variables = add_variables(model, dimension(set))
    constraint = add_constraint(model, VectorOfVariables(variables), set)
    return variables, constraint
end
