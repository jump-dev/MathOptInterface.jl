module Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

using Test

# Be wary of adding new fields to this Config struct. Always think: can it be
# achieved a different way?
mutable struct Config{T<:Real}
    atol::T
    rtol::T
    optimal_status::MOI.TerminationStatusCode
    exclude::Vector{Any}
end

"""
    Config(
        ::Type{T} = Float64;
        atol::Real = Base.rtoldefault(T),
        rtol::Real = Base.rtoldefault(T),
        optimal_status::MOI.TerminationStatusCode = MOI.OPTIMAL,
        exclude::Vector{Any} = Any[],
    ) where {T}

Return an object that is used to configure various tests.

## Configuration arguments

  * `atol::Real = Base.rtoldefault(T)`: Control the absolute tolerance used
    when comparing solutions.
  * `rtol::Real = Base.rtoldefault(T)`: Control the relative tolerance used
    when comparing solutions.
  * `optimal_status = MOI.OPTIMAL`: Set to `MOI.LOCALLY_SOLVED` if the solver
    cannot prove global optimality.
  * `exclude = Vector{Any}`: Pass attributes or functions to `exclude` to skip
    parts of tests that require certain functionality. Common arguments include:
     - `MOI.delete` to skip deletion-related tests
     - `MOI.optimize!` to skip optimize-related tests
     - `MOI.ConstraintDual` to skip dual-related tests
     - `MOI.VariableName` to skip setting variable names
     - `MOI.ConstraintName` to skip setting constraint names

## Examples

For a nonlinear solver that finds local optima and does not support finding
dual variables or constraint names:
```julia
Config(
    Float64;
    optimal_status = MOI.LOCALLY_SOLVED,
    exclude = Any[
        MOI.ConstraintDual,
        MOI.VariableName,
        MOI.ConstraintName,
        MOI.delete,
    ],
)
```
"""
function Config(
    ::Type{T} = Float64;
    atol::Real = Base.rtoldefault(T),
    rtol::Real = Base.rtoldefault(T),
    optimal_status::MOI.TerminationStatusCode = MOI.OPTIMAL,
    exclude::Vector{Any} = Any[],
) where {T<:Real}
    return Config{T}(atol, rtol, optimal_status, exclude)
end

function Base.copy(config::Config{T}) where {T}
    return Config{T}(
        config.atol,
        config.rtol,
        config.optimal_status,
        copy(config.exclude),
    )
end

"""
    setup_test(::typeof(f), model::MOI.ModelLike, config::Config)

Overload this method to modify `model` before running the test function `f` on
`model` with `config`. You can also modify the fields in `config` (e.g., to
loosen the default tolerances).

This function should either return `nothing`, or return a function which, when
called with zero arguments, undoes the setup to return the model to its
previous state. You do not need to undo any modifications to `config`.

This function is most useful when writing new tests of the tests for MOI, but it
can also be used to set test-specific tolerances, etc.

See also: [`runtests`](@ref)

## Example

```julia
function MOI.Test.setup_test(
    ::typeof(MOI.Test.test_linear_VariablePrimalStart_partial),
    mock::MOIU.MockOptimizer,
    ::MOI.Test.Config,
)
    MOIU.set_mock_optimize!(
        mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 0.0]),
    )
    mock.eval_variable_constraint_dual = false

    function reset_function()
        mock.eval_variable_constraint_dual = true
        return
    end
    return reset_function
end
```
"""
setup_test(::Any, ::MOI.ModelLike, ::Config) = nothing

"""
    runtests(
        model::MOI.ModelLike,
        config::Config;
        include::Vector{String} = String[],
        exclude::Vector{String} = String[],
        warn_unsupported::Bool = false,
    )

Run all tests in `MathOptInterface.Test` on `model`.

## Configuration arguments

 * `config` is a [`Test.Config`](@ref) object that can be used to modify the
   behavior of tests.
 * If `include` is not empty, only run tests that contain an element from
   `include` in their name.
 * If `exclude` is not empty, skip tests that contain an element from `exclude`
   in their name.
 * `exclude` takes priority over `include`.
 * If `warn_unsupported` is `false`, `runtests` will silently skip tests that
   fail with `UnsupportedConstraint` or `UnsupportedAttribute`. When
   `warn_unsupported` is `true`, a warning will be printed. For most cases the
   default behavior (`false`) is what you want, since these tests likely test
   functionality that is not supported by `model`. However, it can be useful to
   run  `warn_unsupported = true` to check you are not skipping tests due to a
   missing `supports_constraint` method or equivalent.

See also: [`setup_test`](@ref).

## Example

```julia
config = MathOptInterface.Test.Config()
MathOptInterface.Test.runtests(
    model,
    config;
    include = ["test_linear_"],
    exclude = ["VariablePrimalStart"],
    warn_unsupported = true,
)
```
"""
function runtests(
    model::MOI.ModelLike,
    config::Config;
    include::Vector{String} = String[],
    exclude::Vector{String} = String[],
    warn_unsupported::Bool = false,
)
    for name_sym in names(@__MODULE__; all = true)
        name = string(name_sym)
        if !startswith(name, "test_")
            continue  # All test functions start with test_
        elseif !isempty(include) && !any(s -> occursin(s, name), include)
            continue
        elseif !isempty(exclude) && any(s -> occursin(s, name), exclude)
            continue
        end
        @testset "$(name)" begin
            test_function = getfield(@__MODULE__, name_sym)
            c = copy(config)
            tear_down = setup_test(test_function, model, c)
            # Make sure to empty the model before every test!
            MOI.empty!(model)
            try
                test_function(model, c)
            catch err
                _error_handler(err, name, warn_unsupported)
            end
            if tear_down !== nothing
                tear_down()
            end
        end
    end
    return
end

"""
    RequirementUnmet(msg::String) <: Exception

An error for throwing in tests to indicate that the model does not support some
requirement expected by the test function.
"""
struct RequirementUnmet <: Exception
    msg::String
end

function Base.show(io::IO, err::RequirementUnmet)
    print(io, "RequirementUnmet: $(err.msg)")
    return
end

"""
    @requires(x)

Check that the condition `x` is `true`. Otherwise, throw an [`RequirementUnmet`](@ref)
error to indicate that the model does not support something required by the test
function.

## Examples

```julia
@requires MOI.supports(model, MOI.Silent())
@test MOI.get(model, MOI.Silent())
```
"""
macro requires(x)
    msg = string(x)
    return quote
        if !$(esc(x))
            throw(RequirementUnmet($msg))
        end
    end
end

function _error_handler(
    err::Union{MOI.NotAllowedError,MOI.UnsupportedError,RequirementUnmet},
    name::String,
    warn_unsupported::Bool,
)
    if warn_unsupported
        @warn("Skipping $(name): $(err)")
    end
    return
end

_error_handler(err, ::String, ::Bool) = rethrow(err)

###
### The following are helpful utilities for writing tests in MOI.Test.
###

"""
    Base.isapprox(x, y, config::Config)

A three argument version of `isapprox` for use in MOI.Test.
"""
function Base.isapprox(x, y, config::Config{T}) where {T}
    return Base.isapprox(x, y; atol = config.atol, rtol = config.rtol)
end

"""
    _supports(config::Config, attribute::MOI.AnyAttribute)

Return `true` if the `attribute` is supported by the `config`.

This is helpful when writing tests.

## Example

```julia
if MOI.Test._supports(config, MOI.Silent)
    @test MOI.get(model, MOI.Silent()) == true
end
```
"""
_supports(config::Config, T::Any)::Bool = !(T in config.exclude)

"""
    _test_model_solution(
        model::MOI.ModelLike,
        config::Config;
        objective_value = nothing,
        variable_primal = nothing,
        constraint_primal = nothing,
        constraint_dual = nothing,
    )

Solve, and then test, various aspects of a model.

First, check that `TerminationStatus == config.optimal_status`.

If `objective_value` is not nothing, check that the attribute `ObjectiveValue()`
is approximately `objective_value`.

If `variable_primal` is not nothing, check that the attribute  `PrimalStatus` is
`MOI.FEASIBLE_POINT`. Then for each `(index, value)` in `variable_primal`, check
that the primal value of the variable `index` is approximately `value`.

If `constraint_primal` is not nothing, check that the attribute  `PrimalStatus`
is `MOI.FEASIBLE_POINT`. Then for each `(index, value)` in `constraint_primal`,
check that the primal value of the constraint `index` is approximately `value`.

Finally, if `config.duals = true`, and if `constraint_dual` is not nothing,
check that the attribute  `DualStatus` is `MOI.FEASIBLE_POINT`. Then for each
`(index, value)` in `constraint_dual`, check that the dual of the constraint
`index` is approximately `value`.

### Example

```julia
MOIU.loadfromstring!(model, \"\"\"
    variables: x
    minobjective: 2.0x + 1.0
    c: x >= 1.0
\"\"\")
x = MOI.get(model, MOI.VariableIndex, "x")
c = MOI.get(
    model,
    MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}},
    "c",
)
_test_model_solution(
    model,
    config;
    objective_value = 3.0,
    variable_primal = [(x, 1.0)],
    constraint_primal = [(c, 1.0)],
    constraint_dual = [(c, 2.0)],
)
```
"""
function _test_model_solution(
    model::MOI.ModelLike,
    config::Config{T};
    objective_value = nothing,
    variable_primal = nothing,
    constraint_primal = nothing,
    constraint_dual = nothing,
) where {T}
    if !_supports(config, MOI.optimize!)
        return
    end
    MOI.optimize!(model)
    # No need to check supports. Everyone _must_ implement ObjectiveValue.
    @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
    if objective_value !== nothing && _supports(config, MOI.ObjectiveValue())
        @test isapprox(
            MOI.get(model, MOI.ObjectiveValue()),
            objective_value,
            config,
        )
    end
    # No need to check supports. Everyone _must_ implement VariablePrimal.
    if variable_primal !== nothing
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in variable_primal
            @test isapprox(
                MOI.get(model, MOI.VariablePrimal(), index),
                solution_value,
                config,
            )
        end
    end
    if constraint_primal !== nothing && _supports(config, MOI.ConstraintPrimal)
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in constraint_primal
            @test isapprox(
                MOI.get(model, MOI.ConstraintPrimal(), index),
                solution_value,
                config,
            )
        end
    end
    if constraint_dual !== nothing && _supports(config, MOI.ConstraintDual)
        @test MOI.get(model, MOI.DualStatus()) == MOI.FEASIBLE_POINT
        for (index, solution_value) in constraint_dual
            @test isapprox(
                MOI.get(model, MOI.ConstraintDual(), index),
                solution_value,
                config,
            )
        end
    end
    return
end

# TODO(odow): The following are helper functions for testing the value types of
#     different attributes. The following attributes are not tested:
# BarrierIterations()
# CallbackNodeStatus()
# ConflictStatus()
# ConstraintBridgingCost()
# ConstraintConflictStatus()
# LazyConstraintCallback()
# NodeCount
# RelativeGap
# SimplexIterations
# VariableBridgingCost()

function _test_attribute_value_type(
    model::MOI.ModelLike,
    attribute::Union{MOI.AbstractModelAttribute,MOI.AbstractOptimizerAttribute},
)
    T = MOI.attribute_value_type(attribute)
    @static if VERSION < v"1.5"
        @test MOI.get(model, attribute) isa T
    else
        @test @inferred(T, MOI.get(model, attribute)) isa T
    end
    return
end

function _test_attribute_value_type(
    model::MOI.ModelLike,
    attribute::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
)
    T = MOI.attribute_value_type(attribute)
    @static if VERSION < v"1.5"
        @test MOI.get(model, attribute, ci) isa T
    else
        @test @inferred(T, MOI.get(model, attribute, ci)) isa T
    end
    return
end

function _test_attribute_value_type(
    model::MOI.ModelLike,
    attribute::MOI.AbstractVariableAttribute,
    x::MOI.VariableIndex,
)
    T = MOI.attribute_value_type(attribute)
    @static if VERSION < v"1.5"
        @test MOI.get(model, attribute, x) isa T
    else
        @test @inferred(T, MOI.get(model, attribute, x)) isa T
    end
    return
end

function _test_variablenames_equal(model, variable_names)
    seen_name = Dict(name => false for name in variable_names)
    for index in MOI.get(model, MOI.ListOfVariableIndices())
        name = MOI.get(model, MOI.VariableName(), index)
        @test haskey(seen_name, name)
        @test seen_name[name] == false
        seen_name[name] = true
    end
    @test all(values(seen_name))
    return
end

function _test_constraintnames_equal(model, constraint_names)
    seen_name = Dict(name => false for name in constraint_names)
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if F == MOI.SingleVariable
            continue
        end
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            name = MOI.get(model, MOI.ConstraintName(), index)
            @test haskey(seen_name, name)
            @test seen_name[name] == false
            seen_name[name] = true
        end
    end
    @test all(values(seen_name))
    return
end

"""
    test_models_equal(
        model1::ModelLike,
        model2::ModelLike,
        variable_names::Vector{String},
        constraint_names::Vector{String},
        single_variable_constraints::Vector{Tuple{String,<:MOI.AbstractScalarSet}}
    )

Test that `model1` and `model2` are identical using `variable_names` as keys for
the variable names and `constraint_names` as keys for the constraint names.

In addition, it checks that there is a SingleVariable-in-Set constraint for each
`(name, set)` tuple in `single_variable_constraints`, where `name` is the name
of the corresponding variable.

!!! warning
    This is not a generic function that works in all cases. It is mainly
    intended for writing tests in which all variables and constraints have
    unique names.
"""
function test_models_equal(
    model1::MOI.ModelLike,
    model2::MOI.ModelLike,
    variable_names::Vector{String},
    constraint_names::Vector{String},
    single_variable_constraints::Vector{<:Tuple} = Tuple[],
)
    for (name, set) in single_variable_constraints
        x1 = MOI.get(model1, MOI.VariableIndex, name)
        ci1 = MOI.ConstraintIndex{MOI.SingleVariable,typeof(set)}(x1.value)
        @test MOI.is_valid(model1, ci1)
        @test MOI.get(model1, MOI.ConstraintSet(), ci1) == set
        x2 = MOI.get(model2, MOI.VariableIndex, name)
        ci2 = MOI.ConstraintIndex{MOI.SingleVariable,typeof(set)}(x2.value)
        @test MOI.is_valid(model2, ci2)
        @test MOI.get(model2, MOI.ConstraintSet(), ci2) == set
    end
    _test_variablenames_equal(model1, variable_names)
    _test_variablenames_equal(model2, variable_names)
    _test_constraintnames_equal(model1, constraint_names)
    _test_constraintnames_equal(model2, constraint_names)
    map_2to1 = Dict{MOI.VariableIndex,MOI.VariableIndex}()
    for name in variable_names
        index2 = MOI.get(model2, MOI.VariableIndex, name)
        map_2to1[index2] = MOI.get(model1, MOI.VariableIndex, name)
    end
    for name in constraint_names
        c1 = MOI.get(model1, MOI.ConstraintIndex, name)
        c2 = MOI.get(model2, MOI.ConstraintIndex, name)
        f1 = MOI.get(model1, MOI.ConstraintFunction(), c1)
        f2 = MOI.get(model2, MOI.ConstraintFunction(), c2)
        @test isapprox(f1, MOI.Utilities.map_indices(map_2to1, f2))
        @test MOI.get(model1, MOI.ConstraintSet(), c1) ==
              MOI.get(model2, MOI.ConstraintSet(), c2)
    end
    attrs1 = MOI.get(model1, MOI.ListOfModelAttributesSet())
    attrs2 = MOI.get(model2, MOI.ListOfModelAttributesSet())
    for attr in union(attrs1, attrs2)
        value1 = MOI.get(model1, attr)
        value2 = MOI.get(model2, attr)
        if value1 isa MOI.AbstractFunction
            @test value2 isa MOI.AbstractFunction
            @test isapprox(value1, MOI.Utilities.map_indices(map_2to1, value2))
        else
            @test !(value2 isa MOI.AbstractFunction)
            @test value1 == value2
        end
    end
    return
end

###
### Include all the test files!
###

for file in readdir(@__DIR__)
    if startswith(file, "test_")
        include(file)
    end
end

end # module
