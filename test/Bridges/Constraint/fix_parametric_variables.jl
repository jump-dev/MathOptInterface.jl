# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintFixParametricVariables

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

function test_runtests_x_fixed()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + x + y >= 1.0
        x == 2.0
        """,
        """
        variables: x, y
        c: 3.0 * y + x >= 1.0
        x == 2.0
        """,
    )
    return
end

function test_runtests_y_fixed()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + x + y >= 1.0
        y == 2.0
        """,
        """
        variables: x, y
        c: 3.0 * x + y >= 1.0
        y == 2.0
        """,
    )
    return
end

function test_runtests_both_fixed()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + x + y >= 1.0
        x == 3.0
        y == 2.0
        """,
        """
        variables: x, y
        c: 4.0 * y + x >= 1.0
        x == 3.0
        y == 2.0
        """,
    )
    return
end

function test_runtests_duplicates()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 1.0 * x * y + 2.0 * x * y + x + y + x >= 1.0
        x == 3.0
        """,
        """
        variables: x, y
        c: 10.0 * y + 2.0 * x >= 1.0
        x == 3.0
        """,
    )
    return
end

function test_runtests_squared()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.FixParametricVariablesBridge,
        """
        variables: x, y
        c: 2.0 * x * x + y >= 1.0
        x == 3.0
        """,
        """
        variables: x, y
        c: 6.0 * x + y >= 1.0
        x == 3.0
        """,
    )
    return
end

function test_at_least_one_variable_is_not_fixed()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.FixParametricVariables{Int}(inner)
    x, y = MOI.add_variables(model, 2)
    f = 1 * x * y + 2 * x + 3 * y
    MOI.add_constraint(model, f, MOI.EqualTo(0))
    @test_throws(
        ErrorException("At least one variable is not fixed"),
        MOI.Bridges.final_touch(model),
    )
    return
end

function test_resolve_with_modified()
    inner = MOI.Utilities.Model{Int}()
    model = MOI.Bridges.Constraint.FixParametricVariables{Int}(inner)
    x, y = MOI.add_variables(model, 2)
    f = 1 * x * y + 2 * x + 3 * y
    c = MOI.add_constraint(model, f, MOI.EqualTo(0))
    MOI.add_constraint(model, x, MOI.EqualTo(2))
    MOI.Bridges.final_touch(model)
    z = MOI.get(inner, MOI.ListOfVariableIndices())
    cis = MOI.get(inner, MOI.ListOfConstraintIndices{F,MOI.EqualTo{Int}}())
    @test length(cis) == 1
    f = MOI.get(inner, MOI.ConstraintFunction(), cis[1])
    @test f ≈ 2 * z[1] + 5 * z[2]
    MOI.modify(model, c, MOI.ScalarCoefficientChange(y, 4))
    MOI.Bridges.final_touch(model)
    F = MOI.ScalarAffineFunction{Int}
    z = MOI.get(inner, MOI.ListOfVariableIndices())
    cis = MOI.get(inner, MOI.ListOfConstraintIndices{F,MOI.EqualTo{Int}}())
    @test length(cis) == 1
    f = MOI.get(inner, MOI.ConstraintFunction(), cis[1])
    @test f ≈ 2 * z[1] + 6 * z[2]
    return
end

end  # module

TestConstraintFixParametricVariables.runtests()
