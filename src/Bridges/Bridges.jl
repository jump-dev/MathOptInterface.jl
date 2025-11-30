# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# !!! info "COV_EXCL_LINE"
#
#     The Julia coverage check is not perfect, particularly when it comes to
#     macros that produce code that is not executed. To work around
#     false-negatives, some lines in this file are excluded from coverage with
#     `# COV_EXCL_LINE`. (In most of the excluded cases, the default is for the
#     tests to pass, so the failure case of the testset macro is not executed,
#     and so no code is executed that can be tied back to the excluded lines.

module Bridges

import MathOptInterface as MOI
import OrderedCollections: OrderedDict
import Printf
import Test

include("bridge.jl")
include("set_map.jl")
include("bridge_optimizer.jl")

include("Variable/Variable.jl")
include("Constraint/Constraint.jl")
include("Objective/Objective.jl")

include("lazy_bridge_optimizer.jl")
include("debug.jl")

"""
    full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where {T}

Returns a [`LazyBridgeOptimizer`](@ref) bridging `model` for every bridge
defined in this package (see below for the few exceptions) and for the
coefficient type `T`, as well as the bridges in the list returned by the
[`ListOfNonstandardBridges`](@ref) attribute.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Float64}();

julia> bridged_model = MOI.Bridges.full_bridge_optimizer(model, Float64);
```

## Exceptions

The following bridges are not added by `full_bridge_optimizer`, except if
they are in the list returned by the [`ListOfNonstandardBridges`](@ref) attribute:

 * [`Constraint.SOCtoNonConvexQuadBridge`](@ref)
 * `Constraint.RSOCtoNonConvexQuadBridge`](@ref)
 * [`Constraint.SOCtoPSDBridge`](@ref)
 * If `T` is not a subtype of `AbstractFloat`, subtypes of
   [`Constraint.AbstractToIntervalBridge`](@ref)
    * [`Constraint.GreaterToIntervalBridge`](@ref)
    * [`Constraint.LessToIntervalBridge`](@ref))

See the docstring of the each bridge for the reason they are not added.
"""
function full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where {T}
    bridged_model = LazyBridgeOptimizer(model)
    for BT in MOI.get(model, ListOfNonstandardBridges{T}())
        add_bridge(bridged_model, BT)
    end
    Variable.add_all_bridges(bridged_model, T)
    Constraint.add_all_bridges(bridged_model, T)
    Objective.add_all_bridges(bridged_model, T)
    return bridged_model
end

"""
    ListOfNonstandardBridges{T}() <: MOI.AbstractOptimizerAttribute

Any optimizer can be wrapped in a [`LazyBridgeOptimizer`](@ref) using
[`full_bridge_optimizer`](@ref). However, by default [`LazyBridgeOptimizer`](@ref)
uses a limited set of bridges that are:

  1. implemented in `MOI.Bridges`
  2. generally applicable for all optimizers.

For some optimizers however, it is useful to add additional bridges, such as
those that are implemented in external packages (for example, within the solver package
itself) or only apply in certain circumstances (for example,
[`Constraint.SOCtoNonConvexQuadBridge`](@ref)).

Such optimizers should implement the `ListOfNonstandardBridges` attribute to
return a vector of bridge types that are added by [`full_bridge_optimizer`](@ref)
in addition to the list of default bridges.

Note that optimizers implementing `ListOfNonstandardBridges` may require
package-specific functions or sets to be used if the non-standard bridges
are not added. Therefore, you are recommended to use
`model = MOI.instantiate(Package.Optimizer; with_bridge_type = T)` instead of
`model = MOI.instantiate(Package.Optimizer)`. See
[`MOI.instantiate`](@ref).

## Example

### An optimizer using a non-default bridge in `MOI.Bridges`

Solvers supporting [`MOI.ScalarQuadraticFunction`](@ref) can support
[`MOI.SecondOrderCone`](@ref) and [`MOI.RotatedSecondOrderCone`](@ref) by
defining:
```julia
function MOI.get(::MyQuadraticOptimizer, ::ListOfNonstandardBridges{Float64})
    return Type[
        MOI.Bridges.Constraint.SOCtoNonConvexQuadBridge{Float64},
        MOI.Bridges.Constraint.RSOCtoNonConvexQuadBridge{Float64},
    ]
end
```

### An optimizer defining an internal bridge

Suppose an optimizer can exploit specific structure of a constraint, for example, it
can exploit the structure of the matrix `A` in the linear system of equations
`A * x = b`.

The optimizer can define the function:
```julia
struct MatrixAffineFunction{T} <: MOI.AbstractVectorFunction
    A::SomeStructuredMatrixType{T}
    b::Vector{T}
end
```
and then a bridge
```julia
struct MatrixAffineFunctionBridge{T} <: MOI.Constraint.AbstractBridge
    # ...
end
# ...
```
from `VectorAffineFunction{T}` to the `MatrixAffineFunction`. Finally, it
defines:
```julia
function MOI.get(::Optimizer{T}, ::ListOfNonstandardBridges{T}) where {T}
    return Type[MatrixAffineFunctionBridge{T}]
end
```
"""
struct ListOfNonstandardBridges{T} <: MOI.AbstractOptimizerAttribute end

# This should be Vector{Type}, but MOI <=v1.37.0 had a bug that meant this was
# not implemented. To maintain backwards compatibility, we make this `Vector`.
MOI.attribute_value_type(::ListOfNonstandardBridges) = Vector

MOI.is_copyable(::ListOfNonstandardBridges) = false

MOI.get_fallback(::MOI.ModelLike, ::ListOfNonstandardBridges) = Type[]

function _test_structural_identical(
    a::MOI.ModelLike,
    b::MOI.ModelLike;
    cannot_unbridge::Bool = false,
)
    # Test that the variables are the same. We make the strong assumption that
    # the variables are added in the same order to both models.
    a_x = MOI.get(a, MOI.ListOfVariableIndices())
    b_x = MOI.get(b, MOI.ListOfVariableIndices())
    Test.@testset "Test NumberOfVariables" begin                                # COV_EXCL_LINE
        Test.@test MOI.get(a, MOI.NumberOfVariables()) ==
                   MOI.get(b, MOI.NumberOfVariables())
    end
    Test.@testset "Test length ListOfVariableIndices" begin                     # COV_EXCL_LINE
        Test.@test length(a_x) == length(b_x)
    end
    # A dictionary that maps things from `b`-space to `a`-space.
    x_map = Dict(bx => a_x[i] for (i, bx) in enumerate(b_x))
    # To check that the constraints, we need to first cache all of the
    # constraints in `a`.
    constraints = Dict{Any,Any}()
    a_constraint_types = MOI.get(a, MOI.ListOfConstraintTypesPresent())
    b_constraint_types = MOI.get(b, MOI.ListOfConstraintTypesPresent())
    _name(F, S) = MOI.Utilities._drop_moi("$F-in-$S")
    Test.@testset "get $(_name(F, S))" for (F, S) in a_constraint_types
        Test.@test MOI.supports_constraint(a, F, S)
        constraints[(F, S)] =
            map(MOI.get(a, MOI.ListOfConstraintIndices{F,S}())) do ci
                return (
                    MOI.get(a, MOI.ConstraintFunction(), ci),
                    MOI.get(a, MOI.ConstraintSet(), ci),
                )
            end
        # There may be constraint types reported in `a` that are not in `b`, but
        # have zero constraints in `a`.
        Test.@test (F, S) in b_constraint_types ||
                   MOI.get(a, MOI.NumberOfConstraints{F,S}()) == 0
    end                                                                         # COV_EXCL_LINE
    Test.@testset "$(_name(F, S))" for (F, S) in b_constraint_types
        Test.@test haskey(constraints, (F, S))
        # Check that the same number of constraints are present
        Test.@test MOI.get(a, MOI.NumberOfConstraints{F,S}()) ==
                   MOI.get(b, MOI.NumberOfConstraints{F,S}())
        # Check that supports_constraint is implemented
        Test.@test MOI.supports_constraint(b, F, S)
        # Check that each function in `b` matches a function in `a`
        for ci in MOI.get(b, MOI.ListOfConstraintIndices{F,S}())
            f_b = try
                MOI.get(b, MOI.ConstraintFunction(), ci)
            catch err
                _runtests_error_handler(err, cannot_unbridge)
                continue
            end
            f_b = MOI.Utilities.map_indices(x_map, f_b)
            s_b = MOI.get(b, MOI.ConstraintSet(), ci)
            # We don't care about the order that constraints are added, only
            # that one matches.
            Test.@test any(constraints[(F, S)]) do (f, s)
                return s_b == s && isapprox(f, f_b) && typeof(f) == typeof(f_b)
            end
        end
    end                                                                         # COV_EXCL_LINE
    # Test model attributes are set, like ObjectiveSense and ObjectiveFunction.
    a_attrs = MOI.get(a, MOI.ListOfModelAttributesSet())
    b_attrs = MOI.get(b, MOI.ListOfModelAttributesSet())
    Test.@testset "Test length ListOfModelAttributesSet" begin                  # COV_EXCL_LINE
        Test.@test length(a_attrs) == length(b_attrs)
    end
    Test.@testset "$attr" for attr in b_attrs
        Test.@test attr in a_attrs
        if attr == MOI.ObjectiveSense()
            # map_indices isn't defined for `OptimizationSense`
            Test.@test MOI.get(a, attr) == MOI.get(b, attr)
        else
            attr_b = MOI.Utilities.map_indices(x_map, MOI.get(b, attr))
            Test.@test isapprox(MOI.get(a, attr), attr_b)
        end
    end                                                                         # COV_EXCL_LINE
    return
end

_runtests_error_handler(err, ::Bool) = rethrow(err)

function _runtests_error_handler(
    err::Union{
        MOI.GetAttributeNotAllowed{MOI.ConstraintFunction},
        MOI.GetAttributeNotAllowed{MOI.ConstraintPrimalStart},
    },
    cannot_unbridge::Bool,
)
    if cannot_unbridge
        return  # This error is expected. Do nothing.
    end
    return rethrow(err)
end

"""
    runtests(
        Bridge::Type{<:AbstractBridge},
        input_fn::Function,
        output_fn::Function;
        variable_start = 1.2,
        constraint_start = 1.2,
        eltype = Float64,
        cannot_unbridge::Bool = false,
    )

Run a series of tests that check the correctness of `Bridge`.

`input_fn` and `output_fn` are functions such that `input_fn(model)`
and `output_fn(model)` load the corresponding model into `model`.

Set `cannot_unbridge` to `true` if the bridge transformation is not invertible.
If `Bridge` is a variable bridge this allows [`Variable.unbridged_map`](@ref)
to returns `nothing` so that the tests allow errors that can be raised due to this.
If `Bridge` is a constraint bridge this allows the getter of [`MOI.ConstraintFunction`](@ref)
and [`MOI.ConstraintPrimalStart`](@ref) to throw [`MOI.GetAttributeNotAllowed`](@ref).

## Example

```jldoctest; filter=[r"[0-9.]+s", r"\\s+Time"]
julia> MOI.Bridges.runtests(
           MOI.Bridges.Constraint.ZeroOneBridge,
           model -> MOI.add_constrained_variable(model, MOI.ZeroOne()),
           model -> begin
               x, _ = MOI.add_constrained_variable(model, MOI.Integer())
               MOI.add_constraint(model, 1.0 * x, MOI.Interval(0.0, 1.0))
           end,
       )
Test Summary:    | Pass  Total  Time
Bridges.runtests |   33     33  0.8s
```
"""
function runtests(args...; kwargs...)
    Test.@testset "Bridges.runtests" begin
        _runtests(args...; kwargs...)
    end
    return
end

# A good way to check that the linear mapping implemented in the setter of
# `ConstraintDual` is the inverse-adjoint of the mapping implemented in the
# constraint transformation is to check `get_fallback` for `DualObjectiveValue`.
# Indeed, it will check that the inner product between the constraint constants
# and the dual is the same before and after the bridge transformations.
# For this test to be enabled, the bridge should implement `supports`
# for `ConstraintDual` and implement `MOI.set` for `ConstraintDual`.
# Typically, this would be achieved using
# `Union{ConstraintDual,ConstraintDualStart}` for `MOI.get`, `MOI.set` and
# `MOI.supports`
function _test_dual(
    Bridge::Type{<:AbstractBridge},
    input_fn::Function;
    dual,
    eltype,
    model_eltype,
)
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{model_eltype}())
    mock = MOI.Utilities.MockOptimizer(inner)
    model = _bridged_model(Bridge{eltype}, mock)
    input_fn(model)
    final_touch(model)
    # Should be able to call final_touch multiple times.
    final_touch(model)
    # If the bridges does not support `ConstraintDualStart`, it probably won't
    # support `ConstraintDual` so we skip these tests
    list_of_constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
    attr = MOI.ConstraintDual()
    for (F, S) in list_of_constraints
        if !MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
            # We need all duals for `DualObjectiveValue` fallback
            # TODO except the ones with no constants, we could ignore them
            return
        end
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            set = MOI.get(model, MOI.ConstraintSet(), ci)
            MOI.set(model, MOI.ConstraintDual(), ci, _fake_start(dual, set))
        end
    end
    model_dual =
        MOI.Utilities.get_fallback(model, MOI.DualObjectiveValue(), eltype)
    mock_dual =
        MOI.Utilities.get_fallback(mock, MOI.DualObjectiveValue(), eltype)
    # Need `atol` in case one of them is zero and the other one almost zero
    Test.@test model_dual ≈ mock_dual atol = 1e-6
end

function _runtests(
    Bridge::Type{<:AbstractBridge},
    input_fn::Function,
    output_fn::Function;
    variable_start = 1.2,
    constraint_start = 1.2,
    dual = constraint_start,
    eltype = Float64,
    model_eltype = eltype,
    print_inner_model::Bool = false,
    cannot_unbridge::Bool = false,
)
    # Load model and bridge it
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{model_eltype}())
    model = _bridged_model(Bridge{eltype}, inner)
    input_fn(model)
    final_touch(model)
    # Should be able to call final_touch multiple times.
    final_touch(model)
    if print_inner_model
        print(inner)
    end
    Test.@testset "Test outer bridged model appears like the input" begin       # COV_EXCL_LINE
        test =
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{model_eltype}())
        input_fn(test)
        _test_structural_identical(
            test,
            model;
            cannot_unbridge = cannot_unbridge,
        )
    end
    Test.@testset "Test inner bridged model appears like the target" begin      # COV_EXCL_LINE
        target =
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{model_eltype}())
        output_fn(target)
        _test_structural_identical(target, inner)
    end
    Test.@testset "Test MOI.VariablePrimalStart" begin                          # COV_EXCL_LINE
        attr = MOI.VariablePrimalStart()
        bridge_supported = all(values(Variable.bridges(model))) do bridge
            return MOI.supports(model, attr, typeof(bridge))
        end
        if MOI.supports(model, attr, MOI.VariableIndex) && bridge_supported
            x = MOI.get(model, MOI.ListOfVariableIndices())
            MOI.set(model, attr, x, fill(nothing, length(x)))
            Test.@test all(isnothing, MOI.get(model, attr, x))
            primal_start = fill(variable_start, length(x))
            MOI.set(model, attr, x, primal_start)
            if !isempty(x)
                # ≈ does not work if x is empty because the return of get is Any[]
                Test.@test MOI.get(model, attr, x) ≈ primal_start
            end
        end
    end
    Test.@testset "Test ConstraintPrimalStart and ConstraintDualStart" begin    # COV_EXCL_LINE
        list_of_constraints = MOI.get(model, MOI.ListOfConstraintTypesPresent())
        Test.@testset "$F-in-$S" for (F, S) in list_of_constraints
            for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
                set = try
                    MOI.get(model, MOI.ConstraintSet(), ci)
                catch err
                    _runtests_error_handler(err, cannot_unbridge)
                    continue
                end
                attrs = (MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart())
                Test.@testset "$attr" for attr in attrs
                    if !MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
                        continue
                    end
                    MOI.set(model, attr, ci, nothing)
                    Test.@test MOI.get(model, attr, ci) === nothing
                    start = _fake_start(constraint_start, set)
                    MOI.set(model, attr, ci, start)
                    returned_start = try
                        MOI.get(model, attr, ci)
                    catch err
                        # For a Constraint bridge for which the map is not
                        # invertible, the constraint primal cannot be inverted
                        _runtests_error_handler(
                            err,
                            Bridge <: MOI.Bridges.Constraint.AbstractBridge &&
                                cannot_unbridge,
                        )
                        continue
                    end
                    Test.@test returned_start ≈ start
                end                                                             # COV_EXCL_LINE
            end
        end                                                                     # COV_EXCL_LINE
    end
    Test.@testset "Test general bridge tests" begin                             # COV_EXCL_LINE
        Test.@testset "Constraint" begin                                        # COV_EXCL_LINE
            for b in values(Constraint.bridges(model))
                _general_bridge_tests(something(b))
            end
        end
        Test.@testset "Objective" begin                                         # COV_EXCL_LINE
            for b in values(Objective.bridges(model))
                _general_bridge_tests(something(b))
            end
        end
        Test.@testset "Variable" begin                                          # COV_EXCL_LINE
            for b in values(Variable.bridges(model))
                _general_bridge_tests(something(b))
            end
        end
    end
    Test.@testset "Test delete" begin                                           # COV_EXCL_LINE
        _test_delete(Bridge, model, inner)
    end
    if !isnothing(dual)
        Test.@testset "Test ConstraintDual" begin
            _test_dual(Bridge, input_fn; dual, eltype, model_eltype)
        end
    end
    return
end

"""
    runtests(
        Bridge::Type{<:AbstractBridge},
        input::String,
        output::String;
        variable_start = 1.2,
        constraint_start = 1.2,
        eltype = Float64,
    )

Run a series of tests that check the correctness of `Bridge`.

`input` and `output` are models in the style of
[`MOI.Utilities.loadfromstring!`](@ref).

## Example

```jldoctest; filter=[r"[0-9.]+s", r"\\s+Time"]
julia> MOI.Bridges.runtests(
           MOI.Bridges.Constraint.ZeroOneBridge,
           \"\"\"
           variables: x
           x in ZeroOne()
           \"\"\",
           \"\"\"
           variables: x
           x in Integer()
           1.0 * x in Interval(0.0, 1.0)
           \"\"\",
       )
Test Summary:    | Pass  Total  Time
Bridges.runtests |   32     32  0.0s
```
"""
function runtests(
    Bridge::Type{<:AbstractBridge},
    input::String,
    output::String;
    kwargs...,
)
    runtests(
        Bridge,
        model -> MOI.Utilities.loadfromstring!(model, input),
        model -> MOI.Utilities.loadfromstring!(model, output);
        kwargs...,
    )
    return
end

_test_delete(::Type{<:Variable.AbstractBridge}, model, inner) = nothing

function _test_delete(Bridge, model, inner)
    # Test deletion of things in the bridge.
    #  * We reset the objective
    MOI.set(model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    #  * and delete all constraints
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        MOI.delete.(model, MOI.get(model, MOI.ListOfConstraintIndices{F,S}()))
    end
    #  * So now there should be no constraints in the problem
    Test.@test isempty(MOI.get(inner, MOI.ListOfConstraintTypesPresent()))
    #  * And there should be the same number of variables
    attr = MOI.NumberOfVariables()
    Test.@test MOI.get(inner, attr) == MOI.get(model, attr)
    return
end

_fake_start(value, ::MOI.AbstractScalarSet) = value

_fake_start(value, set::MOI.AbstractVectorSet) = fill(value, MOI.dimension(set))

_fake_start(value::AbstractVector, ::MOI.AbstractVectorSet) = value

function _bridged_model(Bridge::Type{<:Constraint.AbstractBridge}, inner)
    return Constraint.SingleBridgeOptimizer{Bridge}(inner)
end

function _bridged_model(Bridge::Type{<:Objective.AbstractBridge}, inner)
    return Objective.SingleBridgeOptimizer{Bridge}(inner)
end

function _bridged_model(Bridge::Type{<:Variable.AbstractBridge}, inner)
    return Variable.SingleBridgeOptimizer{Bridge}(inner)
end

function _general_bridge_tests(bridge::B) where {B<:AbstractBridge}
    Test.@test added_constrained_variable_types(B) isa Vector{Tuple{Type}}
    for (F, S) in added_constraint_types(B)
        Test.@test(
            length(MOI.get(bridge, MOI.ListOfConstraintIndices{F,S}())) ==
            MOI.get(bridge, MOI.NumberOfConstraints{F,S}())
        )
    end
    Test.@test(
        length(MOI.get(bridge, MOI.ListOfVariableIndices())) ==
        MOI.get(bridge, MOI.NumberOfVariables())
    )
    if MOI.get(bridge, MOI.NumberOfVariables()) > 0
        Test.@test !isempty(added_constrained_variable_types(B))
    end
    if B <: Objective.AbstractBridge
        Test.@test set_objective_function_type(B) <: MOI.AbstractFunction
    end
    return
end

"""
    BridgeRequiresFiniteDomainError{
        B<:AbstractBridge,
        F<:MOI.AbstractFunction,
    } <: Exception

An error thrown when the bridge requires the input function to have a finite
variable domain.
"""
struct BridgeRequiresFiniteDomainError{
    B<:AbstractBridge,
    F<:MOI.AbstractFunction,
} <: Exception
    bridge::B
    f::F
end

function Base.showerror(io::IO, err::BridgeRequiresFiniteDomainError)
    return print(
        io,
        """
        $(typeof(err)):

        There was an error reformulating your model into a form supported by the
        solver because one of the bridges requires that all variables have a
        finite domain.

        To fix this error, add a lower and upper bound to all variables in your
        model.

        If you have double checked that all variables have finite bounds and you
        are still encountering this issue, please open a GitHub issue at
        https://github.com/jump-dev/MathOptInterface.jl

        ## Common mistakes

        A common mistake is to add the variable bounds as affine constraints. For
        example, if you are using JuMP, do not use `@constraint` to add variable
        bounds:
        ```julia
        using JuMP
        model = Model()
        @variable(model, x)
        @constraint(model, x >= 0)
        @constraint(model, x <= 1)
        ```
        do instead:
        ```julia
        using JuMP
        model = Model()
        @variable(model, 0 <= x <= 1)
        ```

        ## Large bound values

        Do not add arbitrarily large variable bounds to fix this error. Doing so
        will likely result in a reformulation that takes a long time to build
        and solve. Use domain knowledge to find the tightest valid bounds.

        Alternatively, use a solver that has native support for the constraint
        types you are using so that you do not need to use the bridging system.
        """,
    )
end

end  # module Bridges
