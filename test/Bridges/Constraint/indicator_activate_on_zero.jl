using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("../utilities.jl")

@testset "Indicator activated on 0" begin
    # linear problem with indicator constraint
    # similar to indicator1_test with reversed z1
    # max  2x1 + 3x2
    # s.t. x1 + x2 <= 10
    #      z1 == 0 ==> x2 <= 8
    #      z2 == 1 ==> x2 + x1/5 <= 9
    #      (1-z1) + z2 >= 1 <=> z2 - z1 >= 0
    model = MOIU.MockOptimizer(MOIU.Model{Float64}())
    config = MOIT.TestConfig()

    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)

    vc1 = MOI.add_constraint(model, z1, MOI.ZeroOne())
    @test vc1.value == z1.value
    vc2 = MOI.add_constraint(model, z2, MOI.ZeroOne())
    @test vc2.value == z2.value
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}(MOI.LessThan(8.0))

    BT = MOIB.Constraint.concrete_bridge_type(
        MOIB.Constraint.IndicatorActiveOnFalseBridge{Float64},
        typeof(f1),
        typeof(iset1),
    )
    BT2 = MOIB.Constraint.concrete_bridge_type(
        MOIB.Constraint.IndicatorActiveOnFalseBridge,
        typeof(f1),
        typeof(iset1),
    )
    bridge = MOIB.Constraint.bridge_constraint(BT, model, f1, iset1)

    @test BT === BT2
    @test bridge isa BT

    z1comp = bridge.variable
    @test MOI.get(model, MOI.ConstraintFunction(), bridge.zero_one_cons) ==
          MOI.SingleVariable(z1comp)
    @test MOI.get(model, MOI.ConstraintSet(), bridge.disjunction_cons) ==
          MOI.EqualTo(1.0)
    disjunction_cons =
        MOI.get(model, MOI.ConstraintFunction(), bridge.disjunction_cons)
    for t in disjunction_cons.terms
        @test t.variable == z1 || t.variable == z1comp
        @test t.coefficient ≈ 1.0
    end
end
