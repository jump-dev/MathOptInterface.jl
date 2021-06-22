module Test

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

using Test

include("config.jl")

include("modellike.jl")

include("contlinear.jl")
include("contconic.jl")
include("contquadratic.jl")

include("intlinear.jl")
include("intconic.jl")

include("nlp.jl")

include("UnitTests/unit_tests.jl")

"""
    setup_test(::typeof(f), model::MOI.ModelLike, config::Config)

Overload this method to modify `model` before running the test function `f` on
`model` with `config`.

This function should either return `nothing`, or return a function which, when
called with zero arguments, undoes the setup to return the model to its
previous state.

This is most useful when writing new tests of the tests for MOI, but can also be
used to set test-specific tolerances, etc.

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

See also: [`setup_test`](@ref).

## Example

```julia
config = MathOptInterface.Test.Config()
MathOptInterface.Test.runtests(
    model,
    config;
    include = ["test_linear_"],
    exclude = ["VariablePrimalStart"],
)
```
"""
function runtests(
    model::MOI.ModelLike,
    config::Config;
    include::Vector{String} = String[],
    exclude::Vector{String} = String[],
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
            tear_down = setup_test(test_function, model, config)
            test_function(model, config)
            if tear_down !== nothing
                tear_down()
            end
        end
    end
end

end # module
