module TestBridgesLazyBridgeOptimizer

using Test

import MathOptInterface

const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

include("utilities.jl")

function test_add_remove_has_bridges()
    T = Int
    model = MOI.Utilities.Model{T}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    for BT in [
        MOI.Bridges.Variable.VectorizeBridge{T},
        MOI.Bridges.Constraint.VectorizeBridge{T},
        MOI.Bridges.Objective.FunctionizeBridge{T},
        MOI.Bridges.Constraint.ScalarFunctionizeBridge{T},
        MOI.Bridges.Variable.FreeBridge{T},
    ]
        @test !MOI.Bridges.has_bridge(bridged, BT)
        MOI.Bridges.add_bridge(bridged, BT)
        @test MOI.Bridges.has_bridge(bridged, BT)
        if BT != MOI.Bridges.Variable.FreeBridge{T}
            @test length(MOI.Bridges._bridge_types(bridged, BT)) == 1
        end
        MOI.Bridges.add_bridge(bridged, BT)
        @test MOI.Bridges.has_bridge(bridged, BT)
        if BT != MOI.Bridges.Variable.FreeBridge{T}
            @test length(MOI.Bridges._bridge_types(bridged, BT)) == 1
        end
        MOI.Bridges.remove_bridge(bridged, BT)
        @test !MOI.Bridges.has_bridge(bridged, BT)
        @test isempty(MOI.Bridges._bridge_types(bridged, BT))
        err = ErrorException(
            "Cannot remove bridge `$BT` as it was never added or was already removed.",
        )
        @test_throws err MOI.Bridges.remove_bridge(bridged, BT)
    end
    return
end

function _functionize_error(b, bridge_type, func, name)
    return ErrorException(
        "Need to apply a `$bridge_type` to a `$func` $name because the" *
        " variable is bridged but $name bridges are not supported by" *
        " `$(typeof(b))`.",
    )
end

function _lazy_functionize_error(bridge_type, func, name)
    return ErrorException(
        "Need to apply a `$bridge_type` to a `$func` $name because the variable is" *
        " bridged but no such $name bridge type was added. Add one" *
        " with `add_bridge`.",
    )
end

MOI.Utilities.@model(
    StandardLPModel,
    (),
    (MOI.EqualTo, MOI.Test.UnknownScalarSet),
    (MOI.Nonnegatives,),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports_constraint(
    ::StandardLPModel{T},
    ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.GreaterThan{T},MOI.LessThan{T},MOI.EqualTo{T}}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::StandardLPModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
)
    return false
end

function MOI.supports(
    ::StandardLPModel{T},
    ::MOI.ObjectiveFunction{
        <:Union{MOI.SingleVariable,MOI.ScalarQuadraticFunction{T}},
    },
) where {T}
    return false
end

function _test_bridged_variable_in_SingleVariable_constraint(T)
    S = MOI.Test.UnknownScalarSet{T}
    set = S(one(T))
    model = StandardLPModel{T}()
    bridged = MOI.Bridges.Variable.Vectorize{T}(model)
    x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
    fx = MOI.SingleVariable(x)
    err = _functionize_error(
        bridged,
        MOI.Bridges.Constraint.ScalarFunctionizeBridge,
        "SingleVariable",
        "constraint",
    )
    @test_throws err MOI.add_constraint(bridged, fx, set)
    @test_throws err MOI.add_constraints(bridged, [fx], [set])
    function _bridged()
        model = StandardLPModel{T}()
        bridged = MOI.Bridges.LazyBridgeOptimizer(model)
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.SingleVariable(x)
        return model, bridged, fx
    end
    _, bridged, fx = _bridged()
    err = _lazy_functionize_error(
        MOI.Bridges.Constraint.ScalarFunctionizeBridge,
        "SingleVariable",
        "constraint",
    )
    @test_throws err MOI.add_constraint(bridged, fx, set)
    @test_throws err MOI.add_constraints(bridged, [fx], [set])
    for i in 1:2
        model, bridged, fx = _bridged()
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.ScalarFunctionizeBridge{T},
        )
        if i == 1
            cx = MOI.add_constraint(bridged, fx, set)
        else
            cx = MOI.add_constraints(bridged, [fx], [set])[1]
        end
        @test MOI.get(bridged, MOI.ConstraintFunction(), cx) == fx
        @test MOI.get(bridged, MOI.ConstraintSet(), cx) == set
        a = MOI.get(model, MOI.ListOfVariableIndices())[1]
        fa = MOI.SingleVariable(a)
        ca = MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},S}(),
        )[1]
        @test MOI.get(model, MOI.ConstraintFunction(), ca) ≈
              convert(MOI.ScalarAffineFunction{T}, fa)
        @test MOI.get(model, MOI.ConstraintSet(), ca) ==
              MOI.Utilities.shift_constant(set, -one(T))
    end
    return
end

function test_bridged_variable_in_SingleVariable_constraint()
    _test_bridged_variable_in_SingleVariable_constraint(Float64)
    _test_bridged_variable_in_SingleVariable_constraint(Int)
    return
end

function _test_bridged_variable_in_VectorOfVariables_constraint(T)
    set = MOI.Zeros(1)
    model = StandardLPModel{T}()
    bridged = MOI.Bridges.Variable.Vectorize{T}(model)
    x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
    fx = MOI.VectorOfVariables([x])
    err = _functionize_error(
        bridged,
        MOI.Bridges.Constraint.VectorFunctionizeBridge,
        "VectorOfVariables",
        "constraint",
    )
    @test_throws err MOI.add_constraint(bridged, fx, set)
    @test_throws err MOI.add_constraints(bridged, [fx], [set])
    function _bridged()
        model = StandardLPModel{T}()
        bridged = MOI.Bridges.LazyBridgeOptimizer(model)
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.ScalarizeBridge{T},
        )
        x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
        fx = MOI.VectorOfVariables([x])
        return model, bridged, fx
    end
    _, bridged, fx = _bridged()
    err = _lazy_functionize_error(
        MOI.Bridges.Constraint.VectorFunctionizeBridge,
        "VectorOfVariables",
        "constraint",
    )
    @test_throws err MOI.add_constraint(bridged, fx, set)
    @test_throws err MOI.add_constraints(bridged, [fx], [set])
    for i in 1:2
        model, bridged, fx = _bridged()
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.VectorFunctionizeBridge{T},
        )
        if i == 1
            cx = MOI.add_constraint(bridged, fx, set)
        else
            cx = MOI.add_constraints(bridged, [fx], [set])[1]
        end
        @test MOI.get(bridged, MOI.ConstraintFunction(), cx) == fx
        @test MOI.get(bridged, MOI.ConstraintSet(), cx) == set
        a = MOI.get(model, MOI.ListOfVariableIndices())[1]
        fa = MOI.SingleVariable(a)
        ca = MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        )[1]
        @test MOI.get(model, MOI.ConstraintFunction(), ca) ≈
              convert(MOI.ScalarAffineFunction{T}, fa)
        @test MOI.get(model, MOI.ConstraintSet(), ca) == MOI.EqualTo(-one(T))
    end
    return
end

function test_bridged_variable_in_VectorOfVariables_constraint()
    _test_bridged_variable_in_VectorOfVariables_constraint(Float64)
    _test_bridged_variable_in_VectorOfVariables_constraint(Int)
    return
end

function _test_bridged_variable_in_SingleVariable_obiective(T)
    model = StandardLPModel{T}()
    bridged = MOI.Bridges.Variable.Vectorize{T}(model)
    x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
    fx = MOI.SingleVariable(x)
    err = _functionize_error(
        bridged,
        MOI.Bridges.Objective.FunctionizeBridge,
        "SingleVariable",
        "objective",
    )
    @test_throws err MOI.set(bridged, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    model = StandardLPModel{T}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
    x, cx = MOI.add_constrained_variable(bridged, MOI.GreaterThan(one(T)))
    fx = MOI.SingleVariable(x)
    err = _lazy_functionize_error(
        MOI.Bridges.Objective.FunctionizeBridge,
        "SingleVariable",
        "objective",
    )
    @test_throws err MOI.set(bridged, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Objective.FunctionizeBridge{T})
    MOI.set(bridged, MOI.ObjectiveFunction{typeof(fx)}(), fx)
    @test MOI.get(bridged, MOI.ObjectiveFunctionType()) == MOI.SingleVariable
    @test MOI.get(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}()) ≈ fx
    a = MOI.get(model, MOI.ListOfVariableIndices())[1]
    fa = MOI.SingleVariable(a)
    @test MOI.get(model, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarAffineFunction{T}
    @test MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}()) ≈
          fa + one(T)
    return
end

function test_bridged_variable_in_SingleVariable_obiective()
    _test_bridged_variable_in_SingleVariable_obiective(Float64)
    _test_bridged_variable_in_SingleVariable_obiective(Int)
    return
end

MOI.Utilities.@model(
    LPModel,
    (),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)

function test_MOI_runtests_LPModel()
    model = LPModel{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    MOI.Test.runtests(
        bridged,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        include = ["test_model_", "test_constraint_"],
        exclude = [
            "test_constraint_ConstraintDualStart",
            "test_constraint_ConstraintPrimalStart",
            "test_model_default_DualStatus",
            "test_model_default_PrimalStatus",
            "test_model_default_TerminationStatus",
            "test_model_LowerBoundAlreadySet",
            "test_model_UpperBoundAlreadySet",
        ],
    )
    return
end

# Model similar to SDPA format, it gives a good example because it does not
# support a lot hence need a lot of bridges
MOI.Utilities.@model(
    SDPAModel,
    (),
    (MOI.EqualTo,),
    (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports_constraint(
    ::SDPAModel{T},
    ::Type{MOI.SingleVariable},
    ::Type{
        <:Union{
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.EqualTo{T},
            MOI.Interval{T},
            MOI.ZeroOne,
            MOI.Integer,
        },
    },
) where {T}
    return false
end

function MOI.supports_constraint(
    ::SDPAModel{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
) where {T}
    return false
end

function MOI.supports_add_constrained_variables(
    ::SDPAModel,
    ::Type{<:Union{MOI.Nonnegatives,MOI.PositiveSemidefiniteConeTriangle}},
)
    return true
end

MOI.supports_add_constrained_variables(::SDPAModel, ::Type{MOI.Reals}) = false

function MOI.supports(
    ::SDPAModel{T},
    ::MOI.ObjectiveFunction{
        <:Union{MOI.SingleVariable,MOI.ScalarQuadraticFunction{T}},
    },
) where {T}
    return false
end

function test_MOI_runtests_SDPAModel()
    model = SDPAModel{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    MOI.Test.runtests(
        bridged,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        include = ["ConstraintName", "VariableName"],
    )
    return
end

function test_show_SPDA()
    model = SDPAModel{Float64}()
    model_str = sprint(MOI.Utilities.print_with_acronym, string(typeof(model)))
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    # no bridges
    @test sprint(show, bridged) === """
    MOIB.LazyBridgeOptimizer{$model_str}
    with 0 variable bridges
    with 0 constraint bridges
    with 0 objective bridges
    with inner model $model_str"""

    MOI.add_constrained_variable(bridged, MOI.LessThan(1.0))
    # add variable bridges
    @test sprint(show, bridged) == """
    MOIB.LazyBridgeOptimizer{$model_str}
    with 2 variable bridges
    with 0 constraint bridges
    with 0 objective bridges
    with inner model $model_str"""
    return
end

function _test_SDPA_format(T)
    model = SDPAModel{T}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Nonpositives,
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.Nonpositives,
    )
    @test !MOI.supports_add_constrained_variables(bridged, MOI.Nonpositives)
    MOI.Bridges.add_bridge(
        bridged,
        MOI.Bridges.Variable.NonposToNonnegBridge{T},
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.Nonpositives,
    )
    @test MOI.supports_add_constrained_variables(bridged, MOI.Nonpositives)
    @test MOI.Bridges.bridge_type(bridged, MOI.Nonpositives) ==
          MOI.Bridges.Variable.NonposToNonnegBridge{T}
    @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    @test !MOI.supports_add_constrained_variables(bridged, MOI.Zeros)
    @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Zeros)
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.ZerosBridge{T})
    @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Zeros)
    @test MOI.supports_add_constrained_variables(bridged, MOI.Zeros)
    @test MOI.Bridges.bridge_type(bridged, MOI.Zeros) ==
          MOI.Bridges.Variable.ZerosBridge{T}
    @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Reals)
    @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Reals)
    @test !MOI.supports_add_constrained_variables(bridged, MOI.Reals)
    @test_throws MOI.UnsupportedConstraint{MOI.VectorOfVariables,MOI.Reals} MOI.add_variable(
        bridged,
    )
    @test_throws MOI.UnsupportedConstraint{MOI.VectorOfVariables,MOI.Reals} MOI.add_variables(
        bridged,
        2,
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.FreeBridge{T})
    @test !MOI.supports_constraint(bridged, MOI.VectorOfVariables, MOI.Reals)
    @test MOI.supports_add_constrained_variables(bridged, MOI.Reals)
    @test MOI.Bridges.bridge_type(bridged, MOI.Reals) ==
          MOI.Bridges.Variable.FreeBridge{T}
    @test !MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.GreaterThan{T},
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.SingleVariable,
        MOI.GreaterThan{T},
    )
    @test !MOI.supports_add_constrained_variable(bridged, MOI.GreaterThan{T})
    @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{T})
    @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.LessThan{T})
    @test !MOI.supports_add_constrained_variable(bridged, MOI.LessThan{T})
    @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{T})
    @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.EqualTo{T})
    @test !MOI.supports_add_constrained_variable(bridged, MOI.EqualTo{T})
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
    @test !MOI.supports_constraint(
        bridged,
        MOI.SingleVariable,
        MOI.GreaterThan{T},
    )
    @test MOI.supports_add_constrained_variable(bridged, MOI.GreaterThan{T})
    @test MOI.Bridges.bridge_type(bridged, MOI.GreaterThan{T}) ==
          MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonnegatives}
    @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.LessThan{T})
    @test MOI.supports_add_constrained_variable(bridged, MOI.LessThan{T})
    @test MOI.Bridges.bridge_type(bridged, MOI.LessThan{T}) ==
          MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonpositives}
    @test !MOI.supports_constraint(bridged, MOI.SingleVariable, MOI.EqualTo{T})
    @test MOI.supports_add_constrained_variable(bridged, MOI.EqualTo{T})
    @test MOI.Bridges.bridge_type(bridged, MOI.EqualTo{T}) ==
          MOI.Bridges.Variable.VectorizeBridge{T,MOI.Zeros}
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    )
    @test !MOI.supports_add_constrained_variables(
        bridged,
        MOI.RotatedSecondOrderCone,
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.RSOCtoPSDBridge{T})
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    )
    @test !MOI.supports_add_constrained_variables(
        bridged,
        MOI.RotatedSecondOrderCone,
    )
    MOI.Bridges.add_bridge(
        bridged,
        MOI.Bridges.Constraint.ScalarFunctionizeBridge{T},
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    )
    @test MOI.supports_add_constrained_variables(
        bridged,
        MOI.RotatedSecondOrderCone,
    )
    @test MOI.Bridges.bridge_type(bridged, MOI.RotatedSecondOrderCone) ==
          MOI.Bridges.Variable.RSOCtoPSDBridge{T}
    x, cx = MOI.add_constrained_variable(bridged, MOI.LessThan(one(T)))
    _test_delete_bridged_variable(
        bridged,
        x,
        MOI.LessThan{T},
        1,
        (
            (MOI.VectorOfVariables, MOI.Nonnegatives, 0),
            (MOI.VectorOfVariables, MOI.Nonpositives, 0),
        ),
        used_bridges = 2,
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Constraint.VectorSlackBridge{T})
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Constraint.ScalarizeBridge{T})
    @test MOI.supports_constraint(
        bridged,
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    )
    @test MOI.Bridges.bridge_type(
        bridged,
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    ) == MOI.Bridges.Constraint.VectorSlackBridge{
        T,
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    }
    @test MOI.supports_constraint(
        bridged,
        MOI.VectorAffineFunction{T},
        MOI.Zeros,
    )
    @test MOI.Bridges.bridge_type(
        bridged,
        MOI.VectorAffineFunction{T},
        MOI.Zeros,
    ) == MOI.Bridges.Constraint.ScalarizeBridge{
        T,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    }
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeSquare,
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeSquare,
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Constraint.SquareBridge{T})
    @test_throws MOI.UnsupportedConstraint{
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeSquare,
    } MOI.Bridges.bridge_type(bridged, MOI.PositiveSemidefiniteConeSquare)
    @test MOI.Bridges._dist(
        bridged.graph,
        MOI.Bridges.node(bridged, MOI.PositiveSemidefiniteConeSquare),
    ) == MOI.Bridges.INFINITY
    @test sprint(MOI.Bridges.print_graph, bridged) == """
Bridge graph with 1 variable nodes, 0 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.PositiveSemidefiniteConeSquare` are not supported
"""
    @test MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeSquare,
    )
    @test sprint(MOI.Bridges.print_graph, bridged) ==
          MOI.Utilities.replace_acronym(
        """
Bridge graph with 1 variable nodes, 1 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.PositiveSemidefiniteConeSquare` are not supported
 (1) `MOI.VectorOfVariables`-in-`MOI.PositiveSemidefiniteConeSquare` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.SquareBridge{T,MOI.VectorOfVariables,MOI.ScalarAffineFunction{T},MOI.PositiveSemidefiniteConeTriangle,MOI.PositiveSemidefiniteConeSquare}).
""",
    )
    MOI.Bridges.add_bridge(
        bridged,
        MOI.Bridges.Constraint.VectorFunctionizeBridge{T},
    )
    @test_throws MOI.UnsupportedConstraint{
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeSquare,
    } MOI.Bridges.bridge_type(bridged, MOI.PositiveSemidefiniteConeSquare)
    @test MOI.Bridges._dist(
        bridged.graph,
        MOI.Bridges.node(bridged, MOI.PositiveSemidefiniteConeSquare),
    ) == 6
    @test sprint(MOI.Bridges.print_graph, bridged) ==
          MOI.Utilities.replace_acronym(
        """
Bridge graph with 1 variable nodes, 3 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.PositiveSemidefiniteConeSquare` are supported (distance 6) by adding free variables and then constrain them, see (1).
 (1) `MOI.VectorAffineFunction{$T}`-in-`MOI.PositiveSemidefiniteConeSquare` constraints are bridged (distance 3) by $(MOI.Bridges.Constraint.SquareBridge{T,MOI.VectorAffineFunction{T},MOI.ScalarAffineFunction{T},MOI.PositiveSemidefiniteConeTriangle,MOI.PositiveSemidefiniteConeSquare}).
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.ScalarizeBridge{T,MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}).
 (3) `MOI.VectorAffineFunction{$T}`-in-`MOI.PositiveSemidefiniteConeTriangle` constraints are bridged (distance 2) by $(MOI.Bridges.Constraint.VectorSlackBridge{T,MOI.VectorAffineFunction{T},MOI.PositiveSemidefiniteConeTriangle}).
""",
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Constraint.VectorizeBridge{T})
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    )
    @test MOI.Bridges.bridge_type(
        bridged,
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    ) == MOI.Bridges.Constraint.VectorizeBridge{
        T,
        MOI.VectorAffineFunction{T},
        MOI.Nonnegatives,
        MOI.ScalarAffineFunction{T},
    }
    @test !MOI.supports_constraint(
        bridged,
        MOI.ScalarQuadraticFunction{T},
        MOI.GreaterThan{T},
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Constraint.QuadtoSOCBridge{T})
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarQuadraticFunction{T},
        MOI.GreaterThan{T},
    )
    @test MOI.Bridges.bridge_type(
        bridged,
        MOI.ScalarQuadraticFunction{T},
        MOI.GreaterThan{T},
    ) == MOI.Bridges.Constraint.QuadtoSOCBridge{T}
    @test MOI.supports_constraint(
        bridged,
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    )
    @test MOI.Bridges.bridge_type(
        bridged,
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    ) == MOI.Bridges.Constraint.QuadtoSOCBridge{T}
    F = MOI.ScalarQuadraticFunction{T}
    attr = MOI.ObjectiveFunction{F}()
    @test !MOI.supports(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @test !MOI.supports(bridged, attr)
    err = MOI.UnsupportedAttribute(attr)
    @test_throws err MOI.Bridges.bridge_type(bridged, F)
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Objective.SlackBridge{T})
    @test !MOI.supports(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @test !MOI.supports(bridged, attr)
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Objective.FunctionizeBridge{T})
    @test MOI.supports(bridged, MOI.ObjectiveFunction{MOI.SingleVariable}())
    @test MOI.Bridges.bridge_type(bridged, MOI.SingleVariable) ==
          MOI.Bridges.Objective.FunctionizeBridge{T}
    @test MOI.supports(bridged, attr)
    @test MOI.Bridges.bridge_type(bridged, F) ==
          MOI.Bridges.Objective.SlackBridge{T,F,F}
    return
end

function test_SDPA_format()
    _test_SDPA_format(Float64)
    _test_SDPA_format(Int)
    return
end

function test_SDPA_debug()
    _test_SDPA_debug(Float64)
    _test_SDPA_debug(Int)
    return
end

function _test_SDPA_debug(T)
    model = SDPAModel{T}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    function debug_string(f, args...)
        s = IOBuffer()
        f(bridged, args...; io = s)
        return String(resize!(s.data, s.size))
    end
    @testset "LessThan variables" begin
        S = MOI.LessThan{T}
        @test debug_string(
            MOI.Bridges.debug_supports_add_constrained_variable,
            S,
        ) == """
Constrained variables in `MOI.LessThan{$T}` are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.LessThan{$T}` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
"""
        @test sprint(MOI.Bridges.print_graph, bridged) == """
Bridge graph with 1 variable nodes, 0 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.LessThan{$T}` are not supported
"""
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
        @test debug_string(
            MOI.Bridges.debug_supports_add_constrained_variable,
            S,
        ) == MOI.Utilities.replace_acronym(
            """
Constrained variables in `MOI.LessThan{$T}` are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.LessThan{$T}` are not supported because:
   Cannot use `$(MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonpositives})` because:
   [2] constrained variables in `MOI.Nonpositives` are not supported
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 [2] constrained variables in `MOI.Nonpositives` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Variable.NonposToNonnegBridge{T},
        )
        @test debug_string(
            MOI.Bridges.debug_supports_add_constrained_variable,
            S,
        ) == "Constrained variables in `MOI.LessThan{$T}` are supported.\n"
        @test sprint(MOI.Bridges.print_graph, bridged) ==
              MOI.Utilities.replace_acronym(
            """
Bridge graph with 2 variable nodes, 0 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.LessThan{$T}` are bridged (distance 2) by $(MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonpositives}).
 [2] constrained variables in `MOI.Nonpositives` are bridged (distance 1) by $(MOI.Bridges.Variable.NonposToNonnegBridge{T}).
""",
        )
    end
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    @testset "LessThan variables with ScalarFunctionizeBridge" begin
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.ScalarFunctionizeBridge{T},
        )
        S = MOI.LessThan{T}
        @test debug_string(
            MOI.Bridges.debug_supports_add_constrained_variable,
            S,
        ) == """
Constrained variables in `MOI.LessThan{$T}` are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.LessThan{$T}` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because:
   (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because no added bridge supports bridging it.
"""
        @test sprint(MOI.Bridges.print_graph, bridged) == """
Bridge graph with 1 variable nodes, 1 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.LessThan{$T}` are not supported
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
"""
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
        @test debug_string(
            MOI.Bridges.debug_supports_add_constrained_variable,
            S,
        ) == MOI.Utilities.replace_acronym(
            """
Constrained variables in `MOI.LessThan{$T}` are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.LessThan{$T}` are not supported because:
   Cannot use `$(MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonpositives})` because:
   [2] constrained variables in `MOI.Nonpositives` are not supported
   Cannot add free variables and then constrain them because:
   (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
 [2] constrained variables in `MOI.Nonpositives` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because no added bridge supports bridging it.
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Variable.NonposToNonnegBridge{T},
        )
        @test debug_string(
            MOI.Bridges.debug_supports_add_constrained_variable,
            S,
        ) == "Constrained variables in `MOI.LessThan{$T}` are supported.\n"
        @test sprint(MOI.Bridges.print_graph, bridged) ==
              MOI.Utilities.replace_acronym(
            """
Bridge graph with 2 variable nodes, 1 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.LessThan{$T}` are bridged (distance 2) by $(MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonpositives}).
 [2] constrained variables in `MOI.Nonpositives` are bridged (distance 1) by $(MOI.Bridges.Variable.NonposToNonnegBridge{T}).
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
""",
        )
    end
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    @testset "Interval constraint" begin
        F = MOI.ScalarAffineFunction{T}
        S = MOI.Interval{T}
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) == """
`MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported because no added bridge supports bridging it.
"""
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.SplitIntervalBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) ==
              MOI.Utilities.replace_acronym(
            """
`MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.SplitIntervalBridge{T,MOI.ScalarAffineFunction{T},MOI.Interval{T},MOI.GreaterThan{T},MOI.LessThan{T}})` because:
   (2) `MOI.ScalarAffineFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported
   (3) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
 (2) `MOI.ScalarAffineFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported because no added bridge supports bridging it.
 (3) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because no added bridge supports bridging it.
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.ScalarSlackBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) ==
              MOI.Utilities.replace_acronym(
            """
`MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.GreaterThan{$T}` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 [2] constrained variables in `MOI.LessThan{$T}` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 [3] constrained variables in `MOI.Interval{$T}` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.SplitIntervalBridge{T,MOI.ScalarAffineFunction{T},MOI.Interval{T},MOI.GreaterThan{T},MOI.LessThan{T}})` because:
   (2) `MOI.ScalarAffineFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported
   (3) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
   Cannot use `$(MOI.Bridges.Constraint.ScalarSlackBridge{T,MOI.ScalarAffineFunction{T},MOI.Interval{T}})` because:
   [3] constrained variables in `MOI.Interval{$T}` are not supported
 (2) `MOI.ScalarAffineFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.ScalarSlackBridge{T,MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}})` because:
   [1] constrained variables in `MOI.GreaterThan{$T}` are not supported
 (3) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.ScalarSlackBridge{T,MOI.ScalarAffineFunction{T},MOI.LessThan{T}})` because:
   [2] constrained variables in `MOI.LessThan{$T}` are not supported
""",
        )
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) ==
              MOI.Utilities.replace_acronym(
            """
`MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [2] constrained variables in `MOI.LessThan{$T}` are not supported because:
   Cannot use `$(MOI.Bridges.Variable.VectorizeBridge{T,MOI.Nonpositives})` because:
   [3] constrained variables in `MOI.Nonpositives` are not supported
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 [3] constrained variables in `MOI.Nonpositives` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 [4] constrained variables in `MOI.Interval{$T}` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 (1) `MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.SplitIntervalBridge{T,MOI.ScalarAffineFunction{T},MOI.Interval{T},MOI.GreaterThan{T},MOI.LessThan{T}})` because:
   (3) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
   Cannot use `$(MOI.Bridges.Constraint.ScalarSlackBridge{T,MOI.ScalarAffineFunction{T},MOI.Interval{T}})` because:
   [4] constrained variables in `MOI.Interval{$T}` are not supported
 (3) `MOI.ScalarAffineFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.ScalarSlackBridge{T,MOI.ScalarAffineFunction{T},MOI.LessThan{T}})` because:
   [2] constrained variables in `MOI.LessThan{$T}` are not supported
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Variable.NonposToNonnegBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) ==
              "`MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are supported.\n"
    end
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    @testset "Quadratic objective" begin
        F = MOI.ScalarQuadraticFunction{T}
        attr = MOI.ObjectiveFunction{F}()
        @test debug_string(MOI.Bridges.debug_supports, attr) == """
Objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported and cannot be bridged into a supported objective function by adding only supported constrained variables and constraints. See details below:
 |1| objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported because no added bridge supports bridging it.
"""
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Objective.SlackBridge{T})
        @test debug_string(MOI.Bridges.debug_supports, attr) ==
              MOI.Utilities.replace_acronym(
            """
Objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported and cannot be bridged into a supported objective function by adding only supported constrained variables and constraints. See details below:
 (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported because no added bridge supports bridging it.
 (2) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because no added bridge supports bridging it.
 |1| objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported because:
   Cannot use `$(MOI.Bridges.Objective.SlackBridge{T,MOI.ScalarQuadraticFunction{T},MOI.ScalarQuadraticFunction{T}})` because:
   (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported
   (2) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
   |2| objective function of type `MOI.SingleVariable` is not supported
 |2| objective function of type `MOI.SingleVariable` is not supported because no added bridge supports bridging it.
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Objective.FunctionizeBridge{T},
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.QuadtoSOCBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports, attr) ==
              MOI.Utilities.replace_acronym(
            """
Objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported and cannot be bridged into a supported objective function by adding only supported constrained variables and constraints. See details below:
 (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.QuadtoSOCBridge{T})` because:
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported because no added bridge supports bridging it.
 (3) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.QuadtoSOCBridge{T})` because:
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported
 |1| objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported because:
   Cannot use `$(MOI.Bridges.Objective.SlackBridge{T,MOI.ScalarQuadraticFunction{T},MOI.ScalarQuadraticFunction{T}})` because:
   (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported
   (3) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.VectorSlackBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports, attr) ==
              MOI.Utilities.replace_acronym(
            """
Objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported and cannot be bridged into a supported objective function by adding only supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.RotatedSecondOrderCone` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.QuadtoSOCBridge{T})` because:
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.VectorSlackBridge{T,MOI.VectorAffineFunction{T},MOI.RotatedSecondOrderCone})` because:
   [1] constrained variables in `MOI.RotatedSecondOrderCone` are not supported
   (3) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported
 (3) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported because no added bridge supports bridging it.
 (4) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.QuadtoSOCBridge{T})` because:
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported
 |1| objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported because:
   Cannot use `$(MOI.Bridges.Objective.SlackBridge{T,MOI.ScalarQuadraticFunction{T},MOI.ScalarQuadraticFunction{T}})` because:
   (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported
   (4) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
""",
        )
        MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.RSOCtoPSDBridge{T})
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.ScalarFunctionizeBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports, attr) ==
              MOI.Utilities.replace_acronym(
            """
Objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported and cannot be bridged into a supported objective function by adding only supported constrained variables and constraints. See details below:
 (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.QuadtoSOCBridge{T})` because:
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.VectorSlackBridge{T,MOI.VectorAffineFunction{T},MOI.RotatedSecondOrderCone})` because:
   (4) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported
 (4) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported because no added bridge supports bridging it.
 (5) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.QuadtoSOCBridge{T})` because:
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are not supported
 |1| objective function of type `MOI.ScalarQuadraticFunction{$T}` is not supported because:
   Cannot use `$(MOI.Bridges.Objective.SlackBridge{T,MOI.ScalarQuadraticFunction{T},MOI.ScalarQuadraticFunction{T}})` because:
   (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are not supported
   (5) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are not supported
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.ScalarizeBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports, attr) ==
              "Objective function of type `MOI.ScalarQuadraticFunction{$T}` is supported.\n"
        @test sprint(MOI.Bridges.print_graph, bridged) ==
              MOI.Utilities.replace_acronym(
            """
Bridge graph with 1 variable nodes, 5 constraint nodes and 2 objective nodes.
 [1] constrained variables in `MOI.RotatedSecondOrderCone` are bridged (distance 2) by $(MOI.Bridges.Variable.RSOCtoPSDBridge{T}).
 (1) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.GreaterThan{$T}` constraints are bridged (distance 5) by $(MOI.Bridges.Constraint.QuadtoSOCBridge{T}).
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are bridged (distance 4) by $(MOI.Bridges.Constraint.VectorSlackBridge{T,MOI.VectorAffineFunction{T},MOI.RotatedSecondOrderCone}).
 (3) `MOI.SingleVariable`-in-`MOI.EqualTo{$T}` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.ScalarFunctionizeBridge{T,MOI.EqualTo{T}}).
 (4) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.ScalarizeBridge{T,MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}).
 (5) `MOI.ScalarQuadraticFunction{$T}`-in-`MOI.LessThan{$T}` constraints are bridged (distance 5) by $(MOI.Bridges.Constraint.QuadtoSOCBridge{T}).
 |1| objective function of type `MOI.ScalarQuadraticFunction{$T}` is bridged (distance 12) by $(MOI.Bridges.Objective.SlackBridge{T,MOI.ScalarQuadraticFunction{T},MOI.ScalarQuadraticFunction{T}}).
 |2| objective function of type `MOI.SingleVariable` is bridged (distance 1) by $(MOI.Bridges.Objective.FunctionizeBridge{T}).
""",
        )
    end
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    @testset "Exponential constraint" begin
        F = MOI.VectorAffineFunction{T}
        S = MOI.ExponentialCone
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) == """
                                                                    `MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
                                                                     (1) `MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported because no added bridge supports bridging it.
                                                                    """
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.VectorSlackBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) ==
              MOI.Utilities.replace_acronym(
            """
`MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.ExponentialCone` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because free variables are bridged but no functionize bridge was added.
 (1) `MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.VectorSlackBridge{T,MOI.VectorAffineFunction{T},MOI.ExponentialCone})` because:
   [1] constrained variables in `MOI.ExponentialCone` are not supported
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported because no added bridge supports bridging it.
""",
        )
        MOI.Bridges.add_bridge(
            bridged,
            MOI.Bridges.Constraint.VectorFunctionizeBridge{T},
        )
        @test debug_string(MOI.Bridges.debug_supports_constraint, F, S) ==
              MOI.Utilities.replace_acronym(
            """
`MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported and cannot be bridged into supported constrained variables and constraints. See details below:
 [1] constrained variables in `MOI.ExponentialCone` are not supported because no added bridge supports bridging it.
   Cannot add free variables and then constrain them because:
   (1) `MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported
 (1) `MOI.VectorAffineFunction{$T}`-in-`MOI.ExponentialCone` constraints are not supported because:
   Cannot use `$(MOI.Bridges.Constraint.VectorSlackBridge{T,MOI.VectorAffineFunction{T},MOI.ExponentialCone})` because:
   [1] constrained variables in `MOI.ExponentialCone` are not supported
   (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.Zeros` constraints are not supported because no added bridge supports bridging it.
""",
        )
    end
end

function _test_continuous_SDPAModel(T)
    model = SDPAModel{T}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, T)
    # For `ScalarAffineFunction`-in-`GreaterThan`,
    # `Constraint.ScalarSlackBridge` -> `Variable.VectorizeBridge`
    # is equivalent to
    # `Constraint.VectorizeBridge` -> `Constraint.VectorSlackBridge`
    # however, `Variable.VectorizeBridge` do not support modification of the
    # set hence it makes some tests of `contlineartest` fail so we disable it.
    MOI.Bridges.remove_bridge(
        bridged,
        MOI.Bridges.Constraint.ScalarSlackBridge{T},
    )
    MOI.Test.runtests(
        bridged,
        MOI.Test.Config(T; exclude = Any[MOI.optimize!]),
        include = ["test_linear_"],
        exclude = [
            "test_linear_Indicator",
            "test_linear_Semi",
            "test_linear_SOS",
            "test_linear_integer",
        ],
    )
    return
end

function test_continuous_SDPAModel()
    _test_continuous_SDPAModel(Float64)
    _test_continuous_SDPAModel(Rational{Int})
    return
end

function test_SDPAModel_with_bridges_and_caching()
    # This tests that the computation of the reverse dict in the
    # caching optimizer works with negative indices
    cached = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        MOI.Utilities.MANUAL,
    )
    vi_cache = MOI.add_variable(cached)
    f(vi) = 1.0 * MOI.SingleVariable(vi)
    ci_cache = MOI.add_constraint(cached, f(vi_cache), MOI.EqualTo(1.0))
    model = SDPAModel{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    MOI.Utilities.reset_optimizer(cached, bridged)
    MOI.Utilities.attach_optimizer(cached)
    vi_bridged = first(MOI.get(bridged, MOI.ListOfVariableIndices()))
    @test vi_bridged == MOI.VariableIndex(-1)
    @test cached.model_to_optimizer_map[vi_cache] == vi_bridged
    @test cached.optimizer_to_model_map[vi_bridged] == vi_cache
    vis_sdpa = MOI.get(model, MOI.ListOfVariableIndices())
    F = typeof(f(vi_cache))
    S = MOI.EqualTo{Float64}
    ci_bridged = first(MOI.get(model, MOI.ListOfConstraintIndices{F,S}()))
    attr = MOI.ConstraintFunction()
    @test MOI.get(bridged, attr, ci_bridged) ≈ f(vi_bridged)
    cis = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    @test length(cis) == 1
    ci_sdpa = first(cis)
    func = [1.0, -1.0]'MOI.SingleVariable.(vis_sdpa)
    @test MOI.get(model, attr, ci_sdpa) ≈ func
    return
end

function test_conic_SDPAModel()
    model = SDPAModel{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    MOI.Test.test_conic_PositiveSemidefiniteConeSquare_VectorOfVariables(
        bridged,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
    )
    return
end

# Model not supporting RotatedSecondOrderCone
MOI.Utilities.@model(
    NoRSOCModel,
    (),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
    (
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
        MOI.ExponentialCone,
        MOI.PositiveSemidefiniteConeTriangle,
    ),
    (MOI.PowerCone,),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

function _test_constrained_variables_in_RSOC(T)
    bridged = MOI.Bridges.full_bridge_optimizer(NoRSOCModel{T}(), T)
    @test MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    )
    # It should be selected over `MOI.Bridges.Variable.RSOCtoPSDBridge` even if they
    # are tied in terms of number of bridges because it is added first in
    # `MOI.Bridges.full_bridge_optimizer`.
    @test MOI.Bridges.bridge_type(bridged, MOI.RotatedSecondOrderCone) ==
          MOI.Bridges.Variable.RSOCtoSOCBridge{T}
    x, cx =
        MOI.add_constrained_variables(bridged, MOI.RotatedSecondOrderCone(3))
    for i in 1:3
        @test MOI.Bridges.bridge(bridged, x[i]) isa
              MOI.Bridges.Variable.RSOCtoSOCBridge{T}
    end
    @test MOI.Bridges.bridge(bridged, cx) isa
          MOI.Bridges.Variable.RSOCtoSOCBridge{T}
    return
end

function test_constrained_variables_in_RSOC()
    # We only use floating point types as there is √2
    _test_constrained_variables_in_RSOC(Float64)
    _test_constrained_variables_in_RSOC(BigFloat)
    return
end

# Model not supporting VectorOfVariables and SingleVariable
MOI.Utilities.@model(
    NoVariableModel,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
    (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    (MOI.VectorAffineFunction,)
)
function MOI.supports_constraint(
    ::NoVariableModel{T},
    ::Type{MOI.SingleVariable},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end

function _test_continuous_conic_with_NoVariableModel(T)
    model = NoVariableModel{T}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, T)
    # The best variable bridge for SOC and RSOC are respectively `SOCtoRSOC` and `RSOCtoSOC`.
    # This forms a cycle because using the variable bridges is not in the shortest path.
    # Therefore they should not be used when calling `add_constrained_variables`.
    # Moreover, the printing should say that the variable bridge is not used.
    @test MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.SecondOrderCone,
    )
    @test MOI.Bridges.bridge_type(bridged, MOI.SecondOrderCone) ==
          MOI.Bridges.Variable.SOCtoRSOCBridge{T}
    @test !MOI.Bridges.is_variable_bridged(bridged, MOI.RotatedSecondOrderCone)
    @test MOI.supports_constraint(
        bridged,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    )
    @test MOI.Bridges.bridge_type(bridged, MOI.RotatedSecondOrderCone) ==
          MOI.Bridges.Variable.RSOCtoSOCBridge{T}
    @test !MOI.Bridges.is_variable_bridged(bridged, MOI.RotatedSecondOrderCone)
    x, cx = MOI.add_constrained_variables(bridged, MOI.SecondOrderCone(3))
    @test !any(v -> MOI.Bridges.is_bridged(bridged, v), x)
    @test !MOI.Bridges.is_variable_bridged(bridged, cx)
    @test MOI.Bridges.bridge(bridged, cx) isa
          MOI.Bridges.Constraint.VectorFunctionizeBridge{T,MOI.SecondOrderCone}
    y, cy =
        MOI.add_constrained_variables(bridged, MOI.RotatedSecondOrderCone(4))
    @test !any(v -> MOI.Bridges.is_bridged(bridged, v), y)
    @test !MOI.Bridges.is_variable_bridged(bridged, cy)
    @test MOI.Bridges.bridge(bridged, cy) isa
          MOI.Bridges.Constraint.RSOCtoSOCBridge{T}
    @test sprint(MOI.Bridges.print_graph, bridged) ==
          MOI.Utilities.replace_acronym(
        """
Bridge graph with 5 variable nodes, 11 constraint nodes and 0 objective nodes.
 [1] constrained variables in `MOI.RotatedSecondOrderCone` are supported (distance 2) by adding free variables and then constrain them, see (3).
 [2] constrained variables in `MOI.PositiveSemidefiniteConeTriangle` are not supported
 [3] constrained variables in `MOI.SecondOrderCone` are supported (distance 2) by adding free variables and then constrain them, see (1).
 [4] constrained variables in `MOI.Nonnegatives` are supported (distance 2) by adding free variables and then constrain them, see (6).
 [5] constrained variables in `MOI.Interval{$T}` are supported (distance 3) by adding free variables and then constrain them, see (8).
 (1) `MOI.VectorOfVariables`-in-`MOI.SecondOrderCone` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.VectorFunctionizeBridge{T,MOI.SecondOrderCone}).
 (2) `MOI.VectorAffineFunction{$T}`-in-`MOI.RotatedSecondOrderCone` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.RSOCtoSOCBridge{T,MOI.VectorAffineFunction{T},MOI.VectorAffineFunction{T}}).
 (3) `MOI.VectorOfVariables`-in-`MOI.RotatedSecondOrderCone` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.RSOCtoSOCBridge{T,MOI.VectorAffineFunction{T},MOI.VectorOfVariables}).
 (4) `MOI.VectorAffineFunction{$T}`-in-`MOI.PositiveSemidefiniteConeTriangle` constraints are not supported
 (5) `MOI.VectorOfVariables`-in-`MOI.PositiveSemidefiniteConeTriangle` constraints are not supported
 (6) `MOI.VectorOfVariables`-in-`MOI.Nonnegatives` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.NonnegToNonposBridge{T,MOI.VectorAffineFunction{T},MOI.VectorOfVariables}).
 (7) `MOI.SingleVariable`-in-`MOI.GreaterThan{$T}` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.GreaterToLessBridge{T,MOI.ScalarAffineFunction{T},MOI.SingleVariable}).
 (8) `MOI.SingleVariable`-in-`MOI.Interval{$T}` constraints are bridged (distance 2) by $(MOI.Bridges.Constraint.ScalarFunctionizeBridge{T,MOI.Interval{T}}).
 (9) `MOI.ScalarAffineFunction{$T}`-in-`MOI.Interval{$T}` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.SplitIntervalBridge{T,MOI.ScalarAffineFunction{T},MOI.Interval{T},MOI.GreaterThan{T},MOI.LessThan{T}}).
 (10) `MOI.SingleVariable`-in-`MOI.LessThan{$T}` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.LessToGreaterBridge{T,MOI.ScalarAffineFunction{T},MOI.SingleVariable}).
 (11) `MOI.SingleVariable`-in-`MOI.EqualTo{$T}` constraints are bridged (distance 1) by $(MOI.Bridges.Constraint.VectorizeBridge{T,MOI.VectorAffineFunction{T},MOI.Zeros,MOI.SingleVariable}).
""",
    )
    return
end

function test_continuous_conic_with_NoVariableModel()
    _test_continuous_conic_with_NoVariableModel(Float64)
    _test_continuous_conic_with_NoVariableModel(Float32)
    return
end

MOI.Utilities.@model(
    OnlyNonnegVAF,
    (),
    (),
    (MOI.Nonnegatives,),
    (),
    (),
    (),
    (),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::OnlyNonnegVAF{T},
    ::Type{MOI.SingleVariable},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end

struct InvariantUnderFunctionConversionAttribute <:
       MOI.AbstractConstraintAttribute end

function MOI.Bridges.Constraint.invariant_under_function_conversion(
    ::InvariantUnderFunctionConversionAttribute,
)
    return true
end

function _dict(model::OnlyNonnegVAF)
    if !haskey(model.ext, :InvariantUnderFunctionConversionAttribute)
        model.ext[:InvariantUnderFunctionConversionAttribute] =
            Dict{MOI.ConstraintIndex,Any}()
    end
    return model.ext[:InvariantUnderFunctionConversionAttribute]
end

function MOI.set(
    model::OnlyNonnegVAF,
    ::InvariantUnderFunctionConversionAttribute,
    ci::MOI.ConstraintIndex,
    value,
)
    return _dict(model)[ci] = value
end

function MOI.get(
    model::OnlyNonnegVAF,
    ::InvariantUnderFunctionConversionAttribute,
    ci::MOI.ConstraintIndex,
)
    return _dict(model)[ci]
end

function _test_context_substitution(T)
    # Two Variable bridge in context
    model = OnlyNonnegVAF{T}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    MOI.Bridges.add_bridge(
        bridged,
        MOI.Bridges.Constraint.VectorFunctionizeBridge{T},
    )
    MOI.Bridges.add_bridge(
        bridged,
        MOI.Bridges.Variable.NonposToNonnegBridge{T},
    )
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.VectorizeBridge{T})
    x, cx = MOI.add_constrained_variable(bridged, MOI.LessThan(one(T)))
    fx = MOI.SingleVariable(x)
    vectorize = MOI.Bridges.bridge(bridged, x)
    @test vectorize isa MOI.Bridges.Variable.VectorizeBridge
    y = vectorize.variable
    fy = MOI.SingleVariable(y)
    flip = MOI.Bridges.bridge(bridged, y)
    @test flip isa MOI.Bridges.Variable.NonposToNonnegBridge
    Z = flip.variables
    z = Z[1]
    fz = MOI.SingleVariable(z)
    vov_ci = flip.constraint
    functionize = MOI.Bridges.bridge(bridged, vov_ci)
    @test functionize isa MOI.Bridges.Constraint.VectorFunctionizeBridge
    aff_ci = functionize.constraint
    function f(vi)
        return MOI.Bridges.Variable.unbridged_function(
            MOI.Bridges.Variable.bridges(bridged),
            vi,
        )
    end
    @test f(y) ≈ one(T) * fx - one(T)
    @test MOI.Bridges.call_in_context(bridged, x, bridge -> f(y)) === nothing
    @test MOI.Bridges.call_in_context(bridged, y, bridge -> f(y)) === nothing
    @test f(z) ≈ -one(T) * fy
    @test MOI.Bridges.call_in_context(bridged, x, bridge -> f(z)) ≈ f(z)
    @test MOI.Bridges.call_in_context(bridged, y, bridge -> f(z)) === nothing
    attr = InvariantUnderFunctionConversionAttribute()
    for func in [T(2) * fx, T(2) * fy, T(2) * fz]
        MOI.set(model, attr, aff_ci, func)
        @test MOI.get(model, attr, aff_ci) ≈ func
        @test MOI.Bridges.call_in_context(
            bridged,
            vov_ci,
            bridge -> MOI.get(bridged, attr, vov_ci),
        ) ≈ func
        @test MOI.Bridges.call_in_context(
            bridged,
            y,
            bridge -> MOI.get(bridged, attr, vov_ci),
        ) ≈ func
        #@test MOI.Bridges.call_in_context(bridged, z, bridge -> MOI.get(bridged, attr, vov_ci)) ≈ func
    end
    # One Variable bridge in context
    model = NoVariableModel{T}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(model)
    MOI.Bridges.add_bridge(bridged, MOI.Bridges.Variable.RSOCtoSOCBridge{T})
    MOI.Bridges.add_bridge(
        bridged,
        MOI.Bridges.Constraint.VectorFunctionizeBridge{T},
    )
    x, cx =
        MOI.add_constrained_variables(bridged, MOI.RotatedSecondOrderCone(4))
    y = MOI.add_variable(bridged)
    @test MOI.get(bridged, MOI.NumberOfVariables()) == 5
    @test MOI.is_valid(bridged, y)
    MOI.delete(bridged, y)
    @test MOI.get(bridged, MOI.NumberOfVariables()) == 4
    @test !MOI.is_valid(bridged, y)
    _test_delete_bridged_variables(
        bridged,
        x,
        MOI.RotatedSecondOrderCone,
        4,
        (
            (MOI.VectorOfVariables, MOI.SecondOrderCone, 0),
            (MOI.VectorAffineFunction{T}, MOI.SecondOrderCone, 0),
        ),
    )
    return
end

function test_context_substitution()
    _test_context_substitution(Float64)
    _test_context_substitution(Float32)
    return
end

# Only supports GreaterThan and Nonnegatives
MOI.Utilities.@model(
    GreaterNonnegModel,
    (),
    (MOI.GreaterThan,),
    (MOI.Nonnegatives,),
    (),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

function MOI.supports_constraint(
    ::GreaterNonnegModel{T},
    ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.EqualTo{T},MOI.LessThan{T},MOI.Interval{T}}},
) where {T}
    return false
end

MOI.Utilities.@model(
    ModelNoVAFinSOC,
    (),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.ExponentialCone,
    ),
    (MOI.PowerCone, MOI.DualPowerCone),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

function MOI.supports_constraint(
    ::ModelNoVAFinSOC{Float64},
    ::Type{MOI.VectorAffineFunction{Float64}},
    ::Type{MOI.SecondOrderCone},
)
    return false
end

# Model supporting nothing
MOI.Utilities.@model NothingModel () () () () () () () ()

function MOI.supports_constraint(
    ::NothingModel{T},
    ::Type{MOI.SingleVariable},
    ::Type{
        <:Union{
            MOI.EqualTo{T},
            MOI.GreaterThan{T},
            MOI.LessThan{T},
            MOI.Interval{T},
            MOI.Integer,
            MOI.ZeroOne,
        },
    },
) where {T}
    return false
end

struct BridgeAddingNoConstraint{T} <: MOI.Bridges.Constraint.AbstractBridge end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:BridgeAddingNoConstraint},
)
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(::Type{<:BridgeAddingNoConstraint})
    return Tuple{Type,Type}[]
end

function MOI.supports_constraint(
    ::Type{<:BridgeAddingNoConstraint},
    ::Type{MOI.SingleVariable},
    ::Type{MOI.Integer},
)
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:BridgeAddingNoConstraint{T}},
    ::Type{MOI.SingleVariable},
    ::Type{MOI.Integer},
) where {T}
    return BridgeAddingNoConstraint{T}
end

const LessThanIndicatorOne{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}

MOI.Utilities.@model(
    ModelNoZeroIndicator,
    (MOI.ZeroOne, MOI.Integer),
    (
        MOI.EqualTo,
        MOI.GreaterThan,
        MOI.LessThan,
        MOI.Interval,
        MOI.Semicontinuous,
        MOI.Semiinteger,
    ),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.RelativeEntropyCone,
        MOI.NormSpectralCone,
        MOI.NormNuclearCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.PositiveSemidefiniteConeSquare,
        MOI.RootDetConeTriangle,
        MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare,
    ),
    (
        MOI.PowerCone,
        MOI.DualPowerCone,
        MOI.SOS1,
        MOI.SOS2,
        LessThanIndicatorOne,
    ),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

MOI.Utilities.@model(
    ModelNoIndicator,
    (MOI.ZeroOne, MOI.Integer),
    (
        MOI.EqualTo,
        MOI.GreaterThan,
        MOI.LessThan,
        MOI.Interval,
        MOI.Semicontinuous,
        MOI.Semiinteger,
    ),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.NormInfinityCone,
        MOI.NormOneCone,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.GeometricMeanCone,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.RelativeEntropyCone,
        MOI.NormSpectralCone,
        MOI.NormNuclearCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.PositiveSemidefiniteConeSquare,
        MOI.RootDetConeTriangle,
        MOI.RootDetConeSquare,
        MOI.LogDetConeTriangle,
        MOI.LogDetConeSquare,
    ),
    (MOI.PowerCone, MOI.DualPowerCone, MOI.SOS1, MOI.SOS2),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction)
)

function test_bridge_adding_no_constraint()
    mock = MOI.Utilities.MockOptimizer(NothingModel{Int}())
    bridged = MOI.Bridges.LazyBridgeOptimizer(mock)
    MOI.Bridges.add_bridge(bridged, BridgeAddingNoConstraint{Float64})
    @test MOI.Bridges.supports_bridging_constraint(
        bridged,
        MOI.SingleVariable,
        MOI.Integer,
    )
end

function test_unsupported_constraint_with_cycles()
    # Test that `supports_constraint` works correctly when it is not
    # supported but the bridges forms a cycle
    mock = MOI.Utilities.MockOptimizer(NothingModel{Float64}())
    bridged = MOI.Bridges.full_bridge_optimizer(mock, Float64)
    @test !MOI.supports_constraint(
        bridged,
        MOI.SingleVariable,
        MOI.GreaterThan{Float64},
    )
    @test !MOI.supports_constraint(
        bridged,
        MOI.VectorAffineFunction{Float64},
        MOI.Nonpositives,
    )
end

function test_UnsupportedConstraint_when_it_cannot_be_bridged()
    mock = MOI.Utilities.MockOptimizer(NoRSOCModel{Float64}())
    bridged_mock = MOI.Bridges.LazyBridgeOptimizer(mock)
    x = MOI.add_variables(bridged_mock, 4)
    err = MOI.UnsupportedConstraint{
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    }()
    @test_throws err begin
        MOI.add_constraint(
            bridged_mock,
            MOI.VectorOfVariables(x),
            MOI.RotatedSecondOrderCone(4),
        )
    end
end

function test_MOI_runtests_No_RSOCModel()
    mock = MOI.Utilities.MockOptimizer(NoRSOCModel{Float64}())
    bridged_mock = MOI.Bridges.LazyBridgeOptimizer(mock)

    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.SplitIntervalBridge{Float64},
    )
    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.RSOCtoPSDBridge{Float64},
    )
    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.SOCtoPSDBridge{Float64},
    )
    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.RSOCtoSOCBridge{Float64},
    )
    MOI.Test.runtests(
        bridged_mock,
        MOI.Test.Config(),
        include = ["ConstraintName", "VariableName"],
    )
    return
end

# Test that RSOCtoPSD is used instead of RSOC+SOCtoPSD as it is a shortest path.
function test_bridge_selection()
    mock = MOI.Utilities.MockOptimizer(NoRSOCModel{Float64}())
    bridged_mock = MOI.Bridges.LazyBridgeOptimizer(mock)

    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.SplitIntervalBridge{Float64},
    )
    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.RSOCtoPSDBridge{Float64},
    )
    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.SOCtoPSDBridge{Float64},
    )
    MOI.Bridges.add_bridge(
        bridged_mock,
        MOI.Bridges.Constraint.RSOCtoSOCBridge{Float64},
    )
    @test !(MOI.supports_constraint(
        bridged_mock,
        MOI.VectorAffineFunction{Float64},
        MOI.LogDetConeTriangle,
    ))
    x = MOI.add_variables(bridged_mock, 3)
    err = MOI.UnsupportedConstraint{
        MOI.VectorAffineFunction{Float64},
        MOI.LogDetConeTriangle,
    }()
    @test_throws err begin
        MOI.Bridges.bridge_type(
            bridged_mock,
            MOI.VectorAffineFunction{Float64},
            MOI.LogDetConeTriangle,
        )
    end
    c = MOI.add_constraint(
        bridged_mock,
        MOI.VectorOfVariables(x),
        MOI.RotatedSecondOrderCone(3),
    )
    @test MOI.Bridges.bridge_type(
        bridged_mock,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    ) == MOI.Bridges.Constraint.RSOCtoPSDBridge{
        Float64,
        MOI.VectorAffineFunction{Float64},
        MOI.VectorOfVariables,
    }
    @test MOI.Bridges.bridge(bridged_mock, c) isa
          MOI.Bridges.Constraint.RSOCtoPSDBridge
    @test bridged_mock.graph.constraint_dist[MOI.Bridges.node(
        bridged_mock,
        MOI.VectorOfVariables,
        MOI.RotatedSecondOrderCone,
    ).index] == 1
    return
end

function test_supports()
    mock = MOI.Utilities.MockOptimizer(NoRSOCModel{Float64}())
    full_bridged_mock = MOI.Bridges.full_bridge_optimizer(mock, Float64)
    for S in [MOI.Nonnegatives, MOI.Nonpositives, MOI.Zeros]
        @test !MOI.supports_constraint(full_bridged_mock, MOI.SingleVariable, S)
    end
    for S in
        [MOI.GreaterThan{Float64}, MOI.LessThan{Float64}, MOI.EqualTo{Float64}]
        @test !MOI.supports_constraint(
            full_bridged_mock,
            MOI.VectorOfVariables,
            S,
        )
    end
    greater_nonneg_mock =
        MOI.Utilities.MockOptimizer(GreaterNonnegModel{Float64}())
    full_bridged_greater_nonneg =
        MOI.Bridges.full_bridge_optimizer(greater_nonneg_mock, Float64)
    for F in [
        MOI.SingleVariable,
        MOI.ScalarAffineFunction{Float64},
        MOI.ScalarQuadraticFunction{Float64},
    ]
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.Interval{Float64},
        )
        @test !MOI.supports_constraint(
            greater_nonneg_mock,
            F,
            MOI.LessThan{Float64},
        )
        @test MOI.supports_constraint(
            full_bridged_greater_nonneg,
            F,
            MOI.LessThan{Float64},
        )
    end
    for F in [
        MOI.VectorOfVariables,
        MOI.VectorAffineFunction{Float64},
        MOI.VectorQuadraticFunction{Float64},
    ]
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.PositiveSemidefiniteConeSquare,
        )
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.GeometricMeanCone,
        )
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.RelativeEntropyCone,
        )
        @test !MOI.supports_constraint(greater_nonneg_mock, F, MOI.Nonpositives)
        @test MOI.supports_constraint(
            full_bridged_greater_nonneg,
            F,
            MOI.Nonnegatives,
        )
    end
    for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}]
        # The bridges in this for loop do not support yet
        # VectorQuadraticFunction. See TODO's for the reason.
        # TODO: Missing vcat for quadratic for supporting quadratic.
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.RotatedSecondOrderCone,
        )
        # TODO: Det bridges need to use MOI.Utilities.operate to support quadratic.
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.LogDetConeTriangle,
        )
        @test MOI.supports_constraint(
            full_bridged_mock,
            F,
            MOI.RootDetConeTriangle,
        )
    end
    mock2 = MOI.Utilities.MockOptimizer(ModelNoVAFinSOC{Float64}())
    @test !MOI.supports_constraint(
        mock2,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    full_bridged_mock2 = MOI.Bridges.full_bridge_optimizer(mock2, Float64)
    @test MOI.supports_constraint(
        full_bridged_mock2,
        MOI.VectorAffineFunction{Float64},
        MOI.SecondOrderCone,
    )
    mock_indicator =
        MOI.Utilities.MockOptimizer(ModelNoZeroIndicator{Float64}())
    full_bridged_mock_indicator =
        MOI.Bridges.full_bridge_optimizer(mock_indicator, Float64)
    @test !MOI.supports_constraint(
        mock_indicator,
        MOI.VectorAffineFunction{Float64},
        MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{Float64}},
    )
    @test MOI.supports_constraint(
        full_bridged_mock_indicator,
        MOI.VectorAffineFunction{Float64},
        MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{Float64}},
    )

    mock_sos_indicator =
        MOI.Utilities.MockOptimizer(ModelNoIndicator{Float64}())
    full_bridged_mock_sos_indicator =
        MOI.Bridges.full_bridge_optimizer(mock_sos_indicator, Float64)
    @test !MOI.supports_constraint(
        mock_sos_indicator,
        MOI.VectorAffineFunction{Float64},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    )
    @test !MOI.supports_constraint(
        mock_sos_indicator,
        MOI.VectorAffineFunction{Float64},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{Float64}},
    )
    @test MOI.supports_constraint(
        full_bridged_mock_sos_indicator,
        MOI.VectorAffineFunction{Float64},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    )
    @test MOI.supports_constraint(
        full_bridged_mock_sos_indicator,
        MOI.VectorAffineFunction{Float64},
        MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{Float64}},
    )
    for T in [Float64, Int]
        no_variable_mock = MOI.Utilities.MockOptimizer(NoVariableModel{T}())
        full_bridged_no_variable =
            MOI.Bridges.full_bridge_optimizer(no_variable_mock, T)
        for S in [
            MOI.LessThan{T},
            MOI.GreaterThan{T},
            MOI.EqualTo{T},
            MOI.ZeroOne,
            MOI.Integer,
        ]
            @test MOI.supports_constraint(
                full_bridged_no_variable,
                MOI.SingleVariable,
                S,
            )
        end
        for S in
            [MOI.Nonpositives, MOI.Nonnegatives, MOI.Zeros, MOI.SecondOrderCone]
            @test MOI.supports_constraint(
                full_bridged_no_variable,
                MOI.VectorOfVariables,
                S,
            )
        end
    end
    return
end

struct CustomVectorSet <: MOI.AbstractVectorSet
    dimension::Int
end

struct CustomScalarSet <: MOI.AbstractScalarSet end

function test_wrong_coefficient()
    for (S, T) in [(Complex{Float64}, Float64), (Float64, Int)]
        model = MOI.Utilities.Model{T}()
        bridged = MOI.Bridges.full_bridge_optimizer(model, T)
        x = MOI.add_variable(bridged)
        fx = MOI.SingleVariable(x)
        f_scalar = one(S) * fx
        f_vector = MOI.Utilities.vectorize([f_scalar])
        function _test(func, set)
            @test_throws(
                MOI.UnsupportedConstraint{typeof(func),typeof(set)},
                MOI.add_constraint(bridged, func, set),
            )
            @test !MOI.supports_constraint(bridged, typeof(func), typeof(set))
            return
        end
        _test(f_scalar, MOI.EqualTo(one(S)))
        _test(f_vector, MOI.Zeros(1))
        _test(f_scalar, CustomScalarSet())
        _test(f_vector, CustomVectorSet(1))
    end
    return
end

struct OptimizerWithBridgeToAdd <: MOI.AbstractOptimizer end
struct BridgeToAdd{T} <: MOI.Bridges.Constraint.AbstractBridge end
function MOI.get(::OptimizerWithBridgeToAdd, ::MOI.Bridges.ToAdd{T}) where {T}
    return [BridgeToAdd{T}]
end

function test_toadd()
    b = MOI.Bridges.full_bridge_optimizer(OptimizerWithBridgeToAdd(), Int)
    @test MOI.Bridges.has_bridge(b, BridgeToAdd{Int})
    @test !MOI.Bridges.has_bridge(b, BridgeToAdd{Float64})
end

end  # module

TestBridgesLazyBridgeOptimizer.runtests()
