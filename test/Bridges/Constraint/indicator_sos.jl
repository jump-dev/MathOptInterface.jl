module TestConstraintIndicatorSOS1

using Test

using MathOptInterface
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

include("../utilities.jl")

function test_indicator_by_SOS1()
    # linear problem with indicator constraints
    # max  2x1 + 3x2
    # s.t. x1 + x2 <= 10
    #      z1 == 1 ==> x2 <= 8
    #      z2 == 1 ==> x1 + x2 == 9
    #      z3 == 1 ==> x1 >= 5
    #      z1 + z2 >= 1
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    z3 = MOI.add_variable(model)

    vc2 = MOI.add_constraint(model, z2, MOI.ZeroOne())
    vc1 = MOI.add_constraint(model, z1, MOI.ZeroOne())
    vc3 = MOI.add_constraint(model, z3, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset1 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(8.0))

    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset2 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.EqualTo(9.0))

    f3 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z3)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x1)),
        ],
        [0.0, 0.0],
    )
    iset3 = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(5.0))

    BT1 = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64},
        typeof(f1),
        typeof(iset1),
    )
    bridge1 = MOI.Bridges.Constraint.bridge_constraint(BT1, model, f1, iset1)
    @test BT1 <:
          MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64,<:MOI.LessThan}
    @test bridge1 isa BT1

    BT2 = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64},
        typeof(f2),
        typeof(iset2),
    )
    bridge2 = MOI.Bridges.Constraint.bridge_constraint(BT2, model, f2, iset2)
    @test BT2 <:
          MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64,<:MOI.EqualTo}
    @test bridge2 isa BT2

    BT3 = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64},
        typeof(f3),
        typeof(iset3),
    )
    bridge3 = MOI.Bridges.Constraint.bridge_constraint(BT3, model, f3, iset3)
    @test BT3 <:
          MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64,<:MOI.GreaterThan}
    @test bridge3 isa BT3

    w1 = bridge1.slack
    w_ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}(
        bridge1.slack.value,
    )
    @test MOI.get(model, MOI.ConstraintFunction(), w_ci) == w1
    @test MOI.get(model, MOI.ConstraintSet(), w_ci) == MOI.LessThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), bridge1.sos_index) ==
          MOI.VectorOfVariables([w1, z1])
    lin_cons1 = bridge1.affine_index
    lin_func1 = MOI.get(model, MOI.ConstraintFunction(), lin_cons1)
    @test lin_func1 ≈ MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x2), MOI.ScalarAffineTerm(1.0, w1)],
        0.0,
    )
    @test MOI.get(model, MOI.ConstraintSet(), lin_cons1) == MOI.LessThan(8.0)

    w2 = bridge2.slack
    @test MOI.get(model, MOI.ConstraintFunction(), bridge2.sos_index) ==
          MOI.VectorOfVariables([w2, z2])
    lin_cons2 = bridge2.affine_index
    lin_func2 = MOI.get(model, MOI.ConstraintFunction(), lin_cons2)
    @test lin_func2 ≈ MOI.ScalarAffineFunction(
        [
            MOI.ScalarAffineTerm(1.0, x1),
            MOI.ScalarAffineTerm(1.0, x2),
            MOI.ScalarAffineTerm(1.0, w2),
        ],
        0.0,
    )
    @test MOI.get(model, MOI.ConstraintSet(), lin_cons2) == MOI.EqualTo(9.0)

    w3 = bridge3.slack
    MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), bridge3.sos_index) ==
          MOI.VectorOfVariables([w3, z3])
    lin_cons3 = bridge3.affine_index
    lin_func3 = MOI.get(model, MOI.ConstraintFunction(), lin_cons3)
    @test lin_func3 ≈ MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, w3)],
        0.0,
    )
    @test MOI.get(model, MOI.ConstraintSet(), lin_cons3) == MOI.GreaterThan(5.0)

    ## MOI.get on bridge
    @test MOI.get(model, MOI.ConstraintSet(), bridge3) == iset3
    @test MOI.get(model, MOI.ConstraintFunction(), bridge3) ≈ f3
    return
end

function test_basic()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    MOI.Test.test_basic_VectorAffineFunction_Indicator_GreaterThan(
        MOI.Bridges.Constraint.IndicatortoSOS1{Float64}(mock),
        MOI.Test.Config(),
    )
    MOI.Test.test_basic_VectorAffineFunction_Indicator_LessThan(
        MOI.Bridges.Constraint.IndicatortoSOS1{Float64}(mock),
        MOI.Test.Config(),
    )
    return
end

function test_model_equality()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.IndicatortoSOS1{Float64}(mock)
    z, _ = MOI.add_constrained_variable(bridged_mock, MOI.ZeroOne())
    MOI.set(bridged_mock, MOI.VariableName(), z, "z")
    x = MOI.add_variable(bridged_mock)
    MOI.set(bridged_mock, MOI.VariableName(), x, "x")
    var_names = ["z", "x", "w"]
    f = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x)),
        ],
        [0.0, 0.0],
    )
    iset = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(8.0))
    ci = MOI.add_constraint(bridged_mock, f, iset)
    @test length(MOI.get(mock, MOI.ListOfVariableIndices())) == 3
    MOI.set(
        mock,
        MOI.VariableName(),
        MOI.get(mock, MOI.ListOfVariableIndices()),
        var_names,
    )
    sos_cons_list = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SOS1{Float64}}(),
    )
    @test length(sos_cons_list) == 1
    MOI.set(mock, MOI.ConstraintName(), sos_cons_list[1], "sos1")
    single_less_cons = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    )
    @test length(single_less_cons) == 1

    inequality_list = MOI.get(
        mock,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    )
    @test length(inequality_list) == 1
    MOI.set(mock, MOI.ConstraintName(), inequality_list[1], "ineq")
    MOI.set(mock, MOI.ObjectiveFunction{MOI.VariableIndex}(), z)
    MOI.set(mock, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    s = """
    variables: z, x, w
    sos1: [w, z] in MathOptInterface.SOS1([0.4, 0.6])
    w <= 0.0
    ineq: x + w <= 8.0
    z in MathOptInterface.ZeroOne()
    maxobjective: z
    """
    model = MOI.Utilities.Model{Float64}()
    MOI.Utilities.loadfromstring!(model, s)
    MOI.Test.util_test_models_equal(
        mock,
        model,
        var_names,
        ["sos1", "ineq"],
        [("w", MOI.LessThan{Float64}(0.0)), ("z", MOI.ZeroOne())],
    )

    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.VariableIndex, MOI.ZeroOne, 1),
            (MOI.VariableIndex, MathOptInterface.LessThan{Float64}, 0),
            (MOI.VectorOfVariables, MathOptInterface.SOS1{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
        ),
        used_bridges = 1,
        num_bridged = 1,
    )

    model = MOI.Utilities.Model{Float64}()
    sbridged = """
    variables: x, z
    z in MathOptInterface.ZeroOne()
    maxobjective: z
    """
    MOI.Utilities.loadfromstring!(model, sbridged)
    MOI.Test.util_test_models_equal(
        bridged_mock,
        model,
        ["z", "x"],
        String[],
        [("z", MOI.ZeroOne())],
    )
    return
end

function test_getting_primal_attributes()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    z, _ = MOI.add_constrained_variable(mock, MOI.ZeroOne())
    x = MOI.add_variable(mock)
    f = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x)),
        ],
        [0.0, 0.0],
    )
    iset = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(8.0))
    BT = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64},
        typeof(f),
        typeof(iset),
    )
    bridge1 = MOI.Bridges.Constraint.bridge_constraint(BT, mock, f, iset)
    # w value should be defaulted to 0
    MOI.set(mock, MOI.VariablePrimalStart(), bridge1.slack, 0.0)
    affine_value = 6.0
    MOI.set(mock, MOI.ConstraintPrimalStart(), bridge1, [1.0, affine_value])
    @test MOI.get(mock, MOI.VariablePrimalStart(), z) ≈ 1.0
    @test MOI.get(mock, MOI.ConstraintPrimalStart(), bridge1.affine_index) ≈ 6.0

    # after setting the w value
    w_value = 3.0
    MOI.set(mock, MOI.VariablePrimalStart(), bridge1.slack, w_value)
    # linear function should not move
    @test all(
        MOI.get(mock, MOI.ConstraintPrimalStart(), bridge1) .≈
        (1.0, affine_value - w_value),
    )
    iseteq = MOI.Indicator{MOI.ACTIVATE_ON_ONE}(MOI.EqualTo(8.0))
    BT = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorSOS1Bridge{Float64},
        typeof(f),
        typeof(iseteq),
    )
    bridge_eq = MOI.Bridges.Constraint.bridge_constraint(BT, mock, f, iseteq)
    @test MOI.get(
        bridge_eq,
        MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
    ) == 0
    @test MOI.get(
        bridge_eq,
        MOI.NumberOfConstraints{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(),
    ) == 1
    @test isempty(
        MOI.get(
            bridge_eq,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.EqualTo{Float64},
            }(),
        ),
    )
    @test MOI.supports(
        mock,
        MOI.ConstraintPrimalStart(),
        MOI.Bridges.Constraint.IndicatorSOS1Bridge{
            Float64,
            MOI.LessThan{Float64},
        },
    )
    # VariablePrimal
    MOI.set(mock, MOI.VariablePrimal(), bridge1.slack, 33.0)
    z_value = 1.0
    MOI.set(mock, MOI.VariablePrimal(), bridge1.z, z_value)
    MOI.set(mock, MOI.VariablePrimal(), x, affine_value)

    # linear function should not move
    @test all(
        MOI.get(mock, MOI.ConstraintPrimal(), bridge1) .≈ (1.0, affine_value),
    )
    return
end

end  # module

TestConstraintIndicatorSOS1.runtests()
