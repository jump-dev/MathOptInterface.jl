# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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

```jldoctest; setup=:(import MathOptInterface as MOI)
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
those that are implemented in external packages (e.g., within the solver package
itself) or only apply in certain circumstances (e.g.,
[`Constraint.SOCtoNonConvexQuadBridge`](@ref)).

Such optimizers should implement the `ListOfNonstandardBridges` attribute to
return a vector of bridge types that are added by [`full_bridge_optimizer`](@ref)
in addition to the list of default bridges.

Note that optimizers implementing `ListOfNonstandardBridges` may require
package-specific functions or sets to be used if the non-standard bridges
are not added. Therefore, you are recommended to use
`model = MOI.instantiate(Package.Optimizer; with_bridge_type = T)` instead of
`model = MOI.instantiate(Package.Optimizer)`. See
[`MathOptInterface.instantiate`](@ref).

## Examples

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

Suppose an optimizer can exploit specific structure of a constraint, e.g., it
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

attribute_value_type(::ListOfNonstandardBridges) = Vector{Type}

MOI.is_copyable(::ListOfNonstandardBridges) = false

MOI.get_fallback(model::MOI.ModelLike, ::ListOfNonstandardBridges) = Type[]

include("precompile.jl")

function _test_structural_identical(a::MOI.ModelLike, b::MOI.ModelLike)
    # Test that the variables are the same. We make the strong assumption that
    # the variables are added in the same order to both models.
    a_x = MOI.get(a, MOI.ListOfVariableIndices())
    b_x = MOI.get(b, MOI.ListOfVariableIndices())
    attr = MOI.NumberOfVariables()
    Test.@test MOI.get(a, attr) == MOI.get(b, attr)
    Test.@test length(a_x) == length(b_x)
    # A dictionary that maps things from `b`-space to `a`-space.
    x_map = Dict(bx => a_x[i] for (i, bx) in enumerate(b_x))
    # To check that the constraints, we need to first cache all of the
    # constraints in `a`.
    constraints = Dict{Any,Any}()
    for (F, S) in MOI.get(a, MOI.ListOfConstraintTypesPresent())
        Test.@test MOI.supports_constraint(a, F, S)
        constraints[(F, S)] =
            map(MOI.get(a, MOI.ListOfConstraintIndices{F,S}())) do ci
                return (
                    MOI.get(a, MOI.ConstraintFunction(), ci),
                    MOI.get(a, MOI.ConstraintSet(), ci),
                )
            end
    end
    # Now compare the constraints in `b` with the cache in `constraints`.
    b_constraint_types = MOI.get(b, MOI.ListOfConstraintTypesPresent())
    # There may be constraint types reported in `a` that are not in `b`, but
    # have zero constraints in `a`.
    for (F, S) in keys(constraints)
        attr = MOI.NumberOfConstraints{F,S}()
        Test.@test (F, S) in b_constraint_types || MOI.get(a, attr) == 0
    end
    for (F, S) in b_constraint_types
        # Check that the same number of constraints are present
        attr = MOI.NumberOfConstraints{F,S}()
        if !haskey(constraints, (F, S))
            # Constraint is reported in `b`, but not in `a`. Check that there
            # are no actual constraints in `b`.
            Test.@test MOI.get(b, attr) == 0
            continue
        else
            Test.@test MOI.get(a, attr) == MOI.get(b, attr)
        end
        # Check that supports_constraint is implemented
        Test.@test MOI.supports_constraint(b, F, S)
        # Check that each function in `b` matches a function in `a`
        for ci in MOI.get(b, MOI.ListOfConstraintIndices{F,S}())
            f_b = MOI.get(b, MOI.ConstraintFunction(), ci)
            f_b = MOI.Utilities.map_indices(x_map, f_b)
            s_b = MOI.get(b, MOI.ConstraintSet(), ci)
            # We don't care about the order that constraints are added, only
            # that one matches.
            Test.@test any(constraints[(F, S)]) do (f, s)
                return s_b == s && isapprox(f, f_b) && typeof(f) == typeof(f_b)
            end
        end
    end
    # Test model attributes are set, like ObjectiveSense and ObjectiveFunction.
    a_attrs = MOI.get(a, MOI.ListOfModelAttributesSet())
    b_attrs = MOI.get(b, MOI.ListOfModelAttributesSet())
    Test.@test length(a_attrs) == length(b_attrs)
    for attr in b_attrs
        Test.@test attr in a_attrs
        if attr == MOI.ObjectiveSense()
            # map_indices isn't defined for `OptimizationSense`
            Test.@test MOI.get(a, attr) == MOI.get(b, attr)
        else
            attr_b = MOI.Utilities.map_indices(x_map, MOI.get(b, attr))
            Test.@test isapprox(MOI.get(a, attr), attr_b)
        end
    end
    return
end

"""
    runtests(Bridge::Type{<:AbstractBridge}, input::String, output::String)

Run a series of tests that check the correctness of `Bridge`.

`input` and `output` are models in the style of
[`MOI.Utilities.loadfromstring!`](@ref).

## Example

```jldoctest; setup=:(import MathOptInterface as MOI)
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
```
"""
function runtests(
    Bridge::Type{<:AbstractBridge},
    input::String,
    output::String;
    constraint_start = 1.2,
)
    # Load model and bridge it
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = _bridged_model(Bridge, inner)
    MOI.Utilities.loadfromstring!(model, input)
    final_touch(model)
    # Should be able to call final_touch multiple times.
    final_touch(model)
    # Load a non-bridged input model, and check that getters are the same.
    test = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    MOI.Utilities.loadfromstring!(test, input)
    _test_structural_identical(test, model)
    # Load a bridged target model, and check that getters are the same.
    target = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    MOI.Utilities.loadfromstring!(target, output)
    _test_structural_identical(target, inner)
    # Test ConstraintPrimalStart and ConstraintDualStart
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            set = MOI.get(model, MOI.ConstraintSet(), ci)
            for attr in (MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart())
                if MOI.supports(model, attr, MOI.ConstraintIndex{F,S})
                    MOI.set(model, attr, ci, nothing)
                    Test.@test MOI.get(model, attr, ci) === nothing
                    start = _fake_start(constraint_start, set)
                    MOI.set(model, attr, ci, start)
                    Test.@test MOI.get(model, attr, ci) ≈ start
                end
            end
        end
    end
    # Test other bridge functions
    for b in values(Constraint.bridges(model))
        _general_bridge_tests(b)
    end
    for b in values(Objective.bridges(model))
        _general_bridge_tests(b)
    end
    for b in values(Variable.bridges(model))
        _general_bridge_tests(b)
    end
    _test_delete(Bridge, model, inner)
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
    for (F, S) in MOI.get(inner, MOI.ListOfConstraintTypesPresent())
        Test.@test MOI.get(inner, MOI.NumberOfConstraints{F,S}()) == 0
    end
    #  * And there should be the same number of variables
    attr = MOI.NumberOfVariables()
    Test.@test MOI.get(inner, attr) == MOI.get(model, attr)
    return
end

_fake_start(value, ::MOI.AbstractScalarSet) = value

_fake_start(value, set::MOI.AbstractVectorSet) = fill(value, MOI.dimension(set))

function _bridged_model(Bridge::Type{<:Constraint.AbstractBridge}, inner)
    return Constraint.SingleBridgeOptimizer{Bridge{Float64}}(inner)
end

function _bridged_model(Bridge::Type{<:Objective.AbstractBridge}, inner)
    return Objective.SingleBridgeOptimizer{Bridge{Float64}}(inner)
end

function _bridged_model(Bridge::Type{<:Variable.AbstractBridge}, inner)
    return Variable.SingleBridgeOptimizer{Bridge{Float64}}(inner)
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
    if B <: Objective.AbstractBridge
        Test.@test set_objective_function_type(B) <: MOI.AbstractFunction
    end
    return
end

end
