# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintIndicatorActiveOnFalse

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

"""
    test_indicator_activate_on_zero()

Linear problem with indicator constraint, similar to indicator1_test with
reversed z1.
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 == 0 ==> x2 <= 8
     z2 == 1 ==> x2 + x1/5 <= 9
     (1-z1) + z2 >= 1 <=> z2 - z1 >= 0
```
"""
function test_indicator_activate_on_zero()
    model = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}())
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
    iset1 = MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(MOI.LessThan(8.0))
    BT = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorActiveOnFalseBridge{Float64},
        typeof(f1),
        typeof(iset1),
    )
    BT2 = MOI.Bridges.Constraint.concrete_bridge_type(
        MOI.Bridges.Constraint.IndicatorActiveOnFalseBridge,
        typeof(f1),
        typeof(iset1),
    )
    bridge = MOI.Bridges.Constraint.bridge_constraint(BT, model, f1, iset1)
    @test BT === BT2
    @test bridge isa BT
    z1comp = bridge.variable
    @test MOI.get(model, MOI.ConstraintFunction(), bridge.zero_one_cons) ==
          z1comp
    @test MOI.get(model, MOI.ConstraintSet(), bridge.disjunction_cons) ==
          MOI.EqualTo(1.0)
    disjunction_cons =
        MOI.get(model, MOI.ConstraintFunction(), bridge.disjunction_cons)
    for t in disjunction_cons.terms
        @test t.variable == z1 || t.variable == z1comp
        @test t.coefficient ≈ 1.0
    end
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.IndicatorActiveOnFalseBridge,
        """
        variables: x, z
        [z, 2.0 * x] in Indicator{ACTIVATE_ON_ZERO}(LessThan(2.0))
        z in ZeroOne()
        """,
        """
        variables: x, z, y
        [y, 2.0 * x] in Indicator{ACTIVATE_ON_ONE}(LessThan(2.0))
        z + y == 1.0
        z in ZeroOne()
        y in ZeroOne()
        """,
    )
    return
end

end  # module

TestConstraintIndicatorActiveOnFalse.runtests()
