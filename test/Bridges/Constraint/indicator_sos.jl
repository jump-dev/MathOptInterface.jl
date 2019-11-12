using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

@testset "Indicator by SOS1" begin
    # linear problem with indicator constraints
    # max  2x1 + 3x2
    # s.t. x1 + x2 <= 10
    #      z1 == 1 ==> x2 <= 8
    #      z2 == 1 ==> x1 + x2 == 9
    #      z3 == 1 ==> x1 >= 5
    #      z1 + z2 >= 1
    model = MOIU.MockOptimizer(MOIU.Model{Float64}());
    config = MOIT.TestConfig()

    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    z3 = MOI.add_variable(model)

    vc1 = MOI.add_constraint(model, z1, MOI.ZeroOne())
    vc2 = MOI.add_constraint(model, z2, MOI.ZeroOne())
    vc3 = MOI.add_constraint(model, z3, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
         MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0]
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(8.0))

    f2 = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
         MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x1)),
         MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0]
    )
    iset2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.EqualTo(9.0))

    f3 = MOI.VectorAffineFunction(
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z3)),
         MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x1)),
        ],
        [0.0, 0.0]
    )
    iset3 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(5.0))


    BT1 = MOIB.Constraint.concrete_bridge_type(MOIB.Constraint.IndicatorSOS1Bridge{Float64}, typeof(f1), typeof(iset1))
    bridge1 = MOIB.Constraint.bridge_constraint(BT1, model, f1, iset1)
    @test BT1 <: MOIB.Constraint.IndicatorSOS1Bridge{Float64, <:MOI.LessThan, <:MOI.ConstraintIndex}
    @test bridge1 isa BT1

    BT2 = MOIB.Constraint.concrete_bridge_type(MOIB.Constraint.IndicatorSOS1Bridge{Float64}, typeof(f2), typeof(iset2))
    bridge2 = MOIB.Constraint.bridge_constraint(BT2, model, f2, iset2)
    @test BT2 <: MOIB.Constraint.IndicatorSOS1Bridge{Float64, <:MOI.EqualTo, Nothing}
    @test bridge2 isa BT2

    BT3 = MOIB.Constraint.concrete_bridge_type(MOIB.Constraint.IndicatorSOS1Bridge{Float64}, typeof(f3), typeof(iset3))
    bridge3 = MOIB.Constraint.bridge_constraint(BT3, model, f3, iset3)
    @test BT3 <: MOIB.Constraint.IndicatorSOS1Bridge{Float64, <:MOI.GreaterThan, <:MOI.ConstraintIndex}
    @test bridge3 isa BT3

    w1 = bridge1.w_variable_index
    @test MOI.get(model, MOI.ConstraintFunction(), bridge1.bound_constraint_index) == MOI.SingleVariable(w1)
    @test MOI.get(model, MOI.ConstraintSet(), bridge1.bound_constraint_index) == MOI.LessThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), bridge1.sos_constraint_index) == MOI.VectorOfVariables([w1, z1])
    lin_cons1 = bridge1.linear_constraint_index
    lin_func1 = MOI.get(model, MOI.ConstraintFunction(), lin_cons1)
    @test lin_func1 ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x2), MOI.ScalarAffineTerm(1.0, w1)], 0.0)
    @test MOI.get(model, MOI.ConstraintSet(), lin_cons1) == MOI.LessThan(8.0)

    w2 = bridge2.w_variable_index
    @test bridge2.bound_constraint_index === nothing
    @test MOI.get(model, MOI.ConstraintFunction(), bridge2.sos_constraint_index) == MOI.VectorOfVariables([w2, z2])
    lin_cons2 = bridge2.linear_constraint_index
    lin_func2 = MOI.get(model, MOI.ConstraintFunction(), lin_cons2)
    @test lin_func2 ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, x2), MOI.ScalarAffineTerm(1.0, w2)], 0.0)
    @test MOI.get(model, MOI.ConstraintSet(), lin_cons2) == MOI.EqualTo(9.0)

    w3 = bridge3.w_variable_index
    @test MOI.get(model, MOI.ConstraintFunction(), bridge3.bound_constraint_index) == MOI.SingleVariable(w3)
    @test MOI.get(model, MOI.ConstraintSet(), bridge3.bound_constraint_index) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), bridge3.sos_constraint_index) == MOI.VectorOfVariables([w3, z3])
    lin_cons3 = bridge3.linear_constraint_index
    lin_func3 = MOI.get(model, MOI.ConstraintFunction(), lin_cons3)
    @test lin_func3 ≈ MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, w3)], 0.0)
    @test MOI.get(model, MOI.ConstraintSet(), lin_cons3) == MOI.GreaterThan(5.0)

    ## MOI.get on bridge
    @test MOI.get(model, MOI.ConstraintSet(), bridge3) == iset3
    @test MOI.get(model, MOI.ConstraintFunction(), bridge3) ≈ f3
end

@testset "Basic constraint test" begin
    mock = MOIU.MockOptimizer(MOIU.UniversalFallback(MOIU.Model{Float64}()))
    config = MOIT.TestConfig()
    for BC in [MOI.LessThan{Float64}, MOI.GreaterThan{Float64}]
        bridged_mock = MOIB.Constraint.IndicatortoSOS1{Float64, BC, MOI.ConstraintIndex{MOI.SingleVariable, BC}}(mock)
        MOIT.basic_constraint_tests(bridged_mock, config,
            include=[
                (MOI.VectorAffineFunction{Float64}, MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE, BC}),
            ],
        )
    end
end
