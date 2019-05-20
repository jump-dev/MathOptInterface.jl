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

    # MOI.add_constraint(model, f1, iset1)
    #
    # f2 = MOI.VectorAffineFunction(
    #   [MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
    #    MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.2, x1)),
    #    MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
    #   ],
    #   [0.0, 0.0],
    # )
    # iset2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(9.0))
    #
    # MOI.add_constraint(model, f2, iset2)
    #
    # # Additional regular constraint.
    # MOI.add_constraint(model,
    #   MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, x2)], 0.0),
    #   MOI.LessThan(10.0),
    # )
    #
    # # Disjunction (1-z1) ⋁ z2
    # MOI.add_constraint(model,
    #   MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(-1.0, z1), MOI.ScalarAffineTerm(1.0, z2)], 0.0),
    #   MOI.GreaterThan(0.0),
    # )
    #
    # MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    #   MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 3.0], [x1, x2]), 0.0)
    # )
    # MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    #
    #
    #
    # test_delete_bridge(bridged_mock, ci, 4,
    #                    ((MOI.VectorAffineFunction{Float64},
    #                      MOI.PositiveSemidefiniteConeTriangle, 0),
    #                     (MOI.ScalarAffineFunction{Float64},
    #                      MOI.EqualTo{Float64}, 1)))
end
