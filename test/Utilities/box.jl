module TestBox

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

function test_equal()
    a = MOI.Utilities.Box([0x00, 0x00], [1, 2], [3, 4])
    b = MOI.Utilities.Box([0x00, 0x00], [1.0, 2.0], [3.0, 4.0])
    c = MOI.Utilities.Box([0x00, 0x02], [1.0, 2.0], [3.0, 4.0])
    d = MOI.Utilities.Box([0x00, 0x00], [1.0, 3.0], [3.0, 4.0])
    e = MOI.Utilities.Box([0x00, 0x00], [1.0, 2.0], [3.0, 5.0])
    @test a == a
    @test a == b
    @test a != c
    @test a != d
    @test a != e
    return
end

function test_empty()
    a = MOI.Utilities.Box([0x00, 0x00], [1, 2], [3, 4])
    empty!(a)
    @test a == MOI.Utilities.Box{Int}()
    return
end

function test_resize()
    a = MOI.Utilities.Box([0x00, 0x00], [1, 2], [3, 4])
    @test length(a.set_mask) == 2
    @test length(a.lower) == 2
    @test length(a.upper) == 2
    resize!(a, 4)
    @test length(a.set_mask) == 4
    @test length(a.lower) == 4
    @test length(a.upper) == 4
    return
end

function test_add_variable()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], [0], [0])
    a = MOI.Utilities.Box{Float64}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Float64}([0x00], [-Inf], [Inf])
    return
end

function test__flag_to_set_type()
    T = Int
    @test_throws AssertionError MOI.Utilities._flag_to_set_type(0x11, T)
    @test MOI.Utilities._flag_to_set_type(0x10, T) == MOI.Integer
    @test MOI.Utilities._flag_to_set_type(0x20, T) == MOI.ZeroOne
    return
end

function test_add_constraint()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    MOI.add_constraint(a, f, MOI.GreaterThan(3))
    @test a == MOI.Utilities.Box{Int}([0x02], [3], [0])
    MOI.add_constraint(a, f, MOI.LessThan(4))
    @test a == MOI.Utilities.Box{Int}([0x06], [3], [4])
    return
end

function test_add_constraint_LowerBoundAlreadySet()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    MOI.add_constraint(a, f, MOI.GreaterThan(3))
    @test_throws(
        MOI.LowerBoundAlreadySet{MOI.GreaterThan{Int},MOI.GreaterThan{Int}},
        MOI.add_constraint(a, f, MOI.GreaterThan(3)),
    )
    return
end

function test_add_constraint_UpperBoundAlreadySet()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    MOI.add_constraint(a, f, MOI.LessThan(3))
    @test_throws(
        MOI.UpperBoundAlreadySet{MOI.LessThan{Int},MOI.LessThan{Int}},
        MOI.add_constraint(a, f, MOI.LessThan(3)),
    )
    return
end

function test_delete_constraint_LessThan()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    c = MOI.add_constraint(a, f, MOI.LessThan(3))
    @test MOI.is_valid(a, c)
    MOI.delete(a, c)
    @test !MOI.is_valid(a, c)
    return
end

function test_delete_variable()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    c = MOI.add_constraint(a, f, MOI.LessThan(3))
    MOI.delete(a, MOI.VariableIndex(1))
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[3])
    return
end

function test_delete_constraint_GreaterThan()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    c = MOI.add_constraint(a, f, MOI.GreaterThan(3))
    @test MOI.is_valid(a, c)
    MOI.delete(a, c)
    @test !MOI.is_valid(a, c)
    return
end

function test_set_ConstraintSet()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_variable(a)
    @test a == MOI.Utilities.Box{Int}([0x00], Int[0], Int[0])
    f = MOI.SingleVariable(MOI.VariableIndex(1))
    c = MOI.add_constraint(a, f, MOI.GreaterThan(3))
    @test a == MOI.Utilities.Box{Int}([0x02], Int[3], Int[0])
    MOI.set(a, MOI.ConstraintSet(), c, MOI.GreaterThan(2))
    @test a == MOI.Utilities.Box{Int}([0x02], Int[2], Int[0])
    return
end

function test_NumberOfConstraints()
    b = MOI.Utilities.Box(
        [0x08, 0x02, 0x04, 0x06],
        [1.0, 3.0, -Inf, -1.0],
        [2.0, Inf, 4.0, 1.0],
    )
    get(S) = MOI.get(b, MOI.NumberOfConstraints{MOI.SingleVariable,S}())
    @test get(MOI.ZeroOne) == 0
    @test get(MOI.GreaterThan{Float64}) == 2
    @test get(MOI.LessThan{Float64}) == 2
    @test get(MOI.Interval{Float64}) == 1
    return
end

function test_ListOfConstraintTypesPresent()
    b = MOI.Utilities.Box(
        [0x08, 0x02, 0x04, 0x06],
        [1.0, 3.0, -Inf, -1.0],
        [2.0, Inf, 4.0, 1.0],
    )
    @test MOI.get(b, MOI.ListOfConstraintTypesPresent()) == [
        (MOI.SingleVariable, MOI.GreaterThan{Float64}),
        (MOI.SingleVariable, MOI.LessThan{Float64}),
        (MOI.SingleVariable, MOI.Interval{Float64}),
    ]
    return
end

function test_ListOfConstraintIndices()
    b = MOI.Utilities.Box(
        [0x08, 0x02, 0x04, 0x06],
        [1.0, 3.0, -Inf, -1.0],
        [2.0, Inf, 4.0, 1.0],
    )
    get(S) = MOI.get(b, MOI.ListOfConstraintIndices{MOI.SingleVariable,S}())
    @test get(MOI.ZeroOne) == []
    @test get(MOI.GreaterThan{Float64}) == [
        MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(2),
        MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}(4),
    ]
    @test get(MOI.LessThan{Float64}) ==
          MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}.([3, 4])
    @test get(MOI.Interval{Float64}) ==
          [MOI.ConstraintIndex{MOI.SingleVariable,MOI.Interval{Float64}}(1)]
    return
end

function test_load_constants()
    a = MOI.Utilities.Box(
        [0x00, 0x00, 0x00],
        [-Inf, -Inf, -Inf],
        [Inf, Inf, Inf],
    )
    MOI.Utilities.load_constants(a, 0, MOI.Interval(1.0, 2.0))
    MOI.Utilities.load_constants(a, 1, MOI.GreaterThan(3.0))
    MOI.Utilities.load_constants(a, 2, MOI.LessThan(4.0))
    @test a == MOI.Utilities.Box(
        [0x08, 0x02, 0x04],
        [1.0, 3.0, -Inf],
        [2.0, Inf, 4.0],
    )
    return
end

function test_function_constants()
    a = MOI.Utilities.Box(
        [0x00, 0x00, 0x00],
        [-Inf, -Inf, -Inf],
        [Inf, Inf, Inf],
    )
    MOI.Utilities.function_constants(a, 0) == 0.0
    return
end

function test_set_from_constants()
    a = MOI.Utilities.Box([0x00, 0x00, 0x00], [1.0, 3.0, -Inf], [2.0, Inf, 4.0])
    @test MOI.Utilities.set_from_constants(a, MOI.Interval{Float64}, 1) ==
          MOI.Interval(1.0, 2.0)
    @test MOI.Utilities.set_from_constants(a, MOI.GreaterThan{Float64}, 2) ==
          MOI.GreaterThan(3.0)
    @test MOI.Utilities.set_from_constants(a, MOI.LessThan{Float64}, 3) ==
          MOI.LessThan(4.0)
    @test MOI.Utilities.set_from_constants(a, MOI.ZeroOne, 2) == MOI.ZeroOne()
    return
end

end  # module

TestBox.runtests()
