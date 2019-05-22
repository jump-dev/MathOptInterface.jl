using Test

using MathOptInterface
const MOI = MathOptInterface
const MOIT = MathOptInterface.Test
const MOIU = MathOptInterface.Utilities
const MOIB = MathOptInterface.Bridges

include("utilities.jl")

include("../model.jl")

@testset "Indicator bridge" begin
    # linear problem with indicator constraint
    # similar to indicator1_test with reversed z1
    # max  2x1 + 3x2
    # s.t. x1 + x2 <= 10
    #      z1 == 0 ==> x2 <= 8
    #      z2 == 1 ==> x2 + x1/5 <= 9
    #      (1-z1) + z2 >= 1 <=> z2 - z1 >= 0
    model = MOIU.MockOptimizer(Model{Float64}());
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
        [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
         MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0]
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}(MOI.LessThan(8.0))

    bridge = MOIB.bridge_constraint(MOIB.IndicatorActiveOnFalseBridge, model, f1, iset1)

    z1comp = bridge.variable_index
    @test MOI.get(model, MOI.ConstraintFunction(), bridge.zero_one_cons) == MOI.SingleVariable(z1comp)
    @test MOI.get(model, MOI.ConstraintSet(), bridge.disjunction_cons) == MOI.EqualTo(1.0)
    disjunction_cons = MOI.get(model, MOI.ConstraintFunction(), bridge.disjunction_cons)
    for t in disjunction_cons.terms
        @test t.variable_index == z1 || t.variable_index == z1comp
        @test t.coefficient ≈ 1.0
    end
end
