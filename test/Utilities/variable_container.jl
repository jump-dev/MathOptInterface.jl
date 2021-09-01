module TestVariableContainer

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

function test_empty()
    a = MOI.Utilities.VariablesContainer([0x0000, 0x0000], [1, 2], [3, 4])
    MOI.empty!(a)
    @test MOI.is_empty(a)
    return
end

function test_resize()
    a = MOI.Utilities.VariablesContainer([0x0000, 0x0000], [1, 2], [3, 4])
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
    a = MOI.Utilities.VariablesContainer{Int}()
    MOI.add_variable(a)
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0000], [0], [0])
    a = MOI.Utilities.VariablesContainer{Float64}()
    MOI.add_variable(a)
    @test a ==
          MOI.Utilities.VariablesContainer{Float64}([0x0000], [-Inf], [Inf])
    return
end

function test__flag_to_set_type()
    T = Int
    @test_throws AssertionError MOI.Utilities._flag_to_set_type(0x0011, T)
    @test MOI.Utilities._flag_to_set_type(0x0010, T) == MOI.Integer
    @test MOI.Utilities._flag_to_set_type(0x0020, T) == MOI.ZeroOne
    return
end

function test_add_constraint()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0000], Int[0], Int[0])
    MOI.add_constraint(a, x, MOI.GreaterThan(3))
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0002], [3], [0])
    MOI.add_constraint(a, x, MOI.LessThan(4))
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0006], [3], [4])
    return
end

function test_add_constraint_LowerBoundAlreadySet()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    MOI.add_constraint(a, x, MOI.GreaterThan(3))
    @test_throws(
        MOI.LowerBoundAlreadySet{MOI.GreaterThan{Int},MOI.GreaterThan{Int}},
        MOI.add_constraint(a, x, MOI.GreaterThan(3)),
    )
    return
end

function test_add_constraint_UpperBoundAlreadySet()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    MOI.add_constraint(a, x, MOI.LessThan(3))
    @test_throws(
        MOI.UpperBoundAlreadySet{MOI.LessThan{Int},MOI.LessThan{Int}},
        MOI.add_constraint(a, x, MOI.LessThan(3)),
    )
    return
end

function test_delete_constraint_LessThan()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    c = MOI.add_constraint(a, x, MOI.LessThan(3))
    @test MOI.is_valid(a, c)
    MOI.delete(a, c)
    @test !MOI.is_valid(a, c)
    return
end

function test_delete_variable()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0000], Int[0], Int[0])
    c = MOI.add_constraint(a, x, MOI.LessThan(3))
    MOI.delete(a, x)
    @test a == MOI.Utilities.VariablesContainer{Int}([0x8000], Int[0], Int[3])
    return
end

function test_delete_constraint_GreaterThan()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    c = MOI.add_constraint(a, x, MOI.GreaterThan(3))
    @test MOI.is_valid(a, c)
    MOI.delete(a, c)
    @test !MOI.is_valid(a, c)
    return
end

function test_set_ConstraintSet()
    a = MOI.Utilities.VariablesContainer{Int}()
    x = MOI.add_variable(a)
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0000], Int[0], Int[0])
    c = MOI.add_constraint(a, x, MOI.GreaterThan(3))
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0002], Int[3], Int[0])
    MOI.set(a, MOI.ConstraintSet(), c, MOI.GreaterThan(2))
    @test a == MOI.Utilities.VariablesContainer{Int}([0x0002], Int[2], Int[0])
    return
end

function test_NumberOfConstraints()
    b = MOI.Utilities.VariablesContainer(
        [0x0008, 0x0002, 0x0004, 0x0006],
        [1.0, 3.0, -Inf, -1.0],
        [2.0, Inf, 4.0, 1.0],
    )
    get(S) = MOI.get(b, MOI.NumberOfConstraints{MOI.VariableIndex,S}())
    @test get(MOI.ZeroOne) == 0
    @test get(MOI.GreaterThan{Float64}) == 2
    @test get(MOI.LessThan{Float64}) == 2
    @test get(MOI.Interval{Float64}) == 1
    return
end

function test_ListOfConstraintTypesPresent()
    b = MOI.Utilities.VariablesContainer(
        [0x0008, 0x0002, 0x0004, 0x0006],
        [1.0, 3.0, -Inf, -1.0],
        [2.0, Inf, 4.0, 1.0],
    )
    @test MOI.get(b, MOI.ListOfConstraintTypesPresent()) == [
        (MOI.VariableIndex, MOI.GreaterThan{Float64}),
        (MOI.VariableIndex, MOI.LessThan{Float64}),
        (MOI.VariableIndex, MOI.Interval{Float64}),
    ]
    return
end

function test_ListOfConstraintIndices()
    b = MOI.Utilities.VariablesContainer{Float64}(
        [0x0008, 0x0002, 0x0004, 0x0006],
        [1.0, 3.0, -Inf, -1.0],
        [2.0, Inf, 4.0, 1.0],
    )
    get(S) = MOI.get(b, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
    @test get(MOI.ZeroOne) == []
    @test get(MOI.GreaterThan{Float64}) == [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}(2),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}}(4),
    ]
    @test get(MOI.LessThan{Float64}) ==
          MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}}.([3, 4])
    @test get(MOI.Interval{Float64}) ==
          [MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}}(1)]
    return
end

###
### Hyperrectangle
###

function test_Hyperrectangle_equal()
    a = MOI.Utilities.Hyperrectangle([1, 2], [3, 4])
    b = MOI.Utilities.Hyperrectangle([1.0, 2.0], [3.0, 4.0])
    c = MOI.Utilities.Hyperrectangle([1.0, 3.0], [3.0, 4.0])
    d = MOI.Utilities.Hyperrectangle([1.0, 2.0], [3.0, 5.0])
    @test a == a
    @test a == b
    @test a != c
    @test a != d
    return
end

function test_Hyperrectangle_empty()
    a = MOI.Utilities.Hyperrectangle([1, 2], [3, 4])
    empty!(a)
    @test a.lower == Int[]
    @test a.upper == Int[]
    return
end

function test_Hyperrectangle_resize()
    a = MOI.Utilities.Hyperrectangle([1, 2], [3, 4])
    @test length(a.lower) == 2
    @test length(a.upper) == 2
    resize!(a, 4)
    @test length(a.lower) == 4
    @test length(a.upper) == 4
    return
end

function test_load_constants()
    a = MOI.Utilities.Hyperrectangle([-Inf, -Inf, -Inf], [Inf, Inf, Inf])
    MOI.Utilities.load_constants(a, 0, MOI.Interval(1.0, 2.0))
    MOI.Utilities.load_constants(a, 1, MOI.GreaterThan(3.0))
    MOI.Utilities.load_constants(a, 2, MOI.LessThan(4.0))
    @test a == MOI.Utilities.Hyperrectangle([1.0, 3.0, -Inf], [2.0, Inf, 4.0])
    return
end

function test_function_constants()
    a = MOI.Utilities.Hyperrectangle([-Inf, -Inf, -Inf], [Inf, Inf, Inf])
    MOI.Utilities.function_constants(a, 0) == 0.0
    return
end

function test_set_from_constants()
    a = MOI.Utilities.Hyperrectangle([1.0, 3.0, -Inf], [2.0, Inf, 4.0])
    @test MOI.Utilities.set_from_constants(a, MOI.Interval{Float64}, 1) ==
          MOI.Interval(1.0, 2.0)
    @test MOI.Utilities.set_from_constants(a, MOI.GreaterThan{Float64}, 2) ==
          MOI.GreaterThan(3.0)
    @test MOI.Utilities.set_from_constants(a, MOI.LessThan{Float64}, 3) ==
          MOI.LessThan(4.0)
    @test MOI.Utilities.set_from_constants(a, MOI.ZeroOne, 2) == MOI.ZeroOne()
    return
end

function _test_bound_vectors(::Type{T}, nolb, noub) where {T}
    variable_bounds = MOI.Utilities.VariablesContainer{T}()
    @test variable_bounds.lower == T[]
    @test variable_bounds.upper == T[]
    x = MOI.add_variable(variable_bounds)
    @test variable_bounds.lower == [nolb]
    @test variable_bounds.upper == [noub]
    ux = MOI.add_constraint(variable_bounds, x, MOI.LessThan(T(1)))
    @test variable_bounds.lower == [nolb]
    @test variable_bounds.upper == [T(1)]
    y = MOI.add_variable(variable_bounds)
    @test variable_bounds.lower == [nolb, nolb]
    @test variable_bounds.upper == [T(1), noub]
    cy = MOI.add_constraint(variable_bounds, y, MOI.Interval(T(2), T(3)))
    @test variable_bounds.lower == [nolb, T(2)]
    @test variable_bounds.upper == [T(1), T(3)]
    lx = MOI.add_constraint(variable_bounds, x, MOI.GreaterThan(T(0)))
    @test variable_bounds.lower == [T(0), T(2)]
    @test variable_bounds.upper == [T(1), T(3)]
    MOI.delete(variable_bounds, lx)
    @test variable_bounds.lower == [nolb, T(2)]
    @test variable_bounds.upper == [T(1), T(3)]
    MOI.delete(variable_bounds, ux)
    @test variable_bounds.lower == [nolb, T(2)]
    @test variable_bounds.upper == [noub, T(3)]
    cx = MOI.add_constraint(variable_bounds, x, MOI.Semicontinuous(T(3), T(4)))
    @test variable_bounds.lower == [T(3), T(2)]
    @test variable_bounds.upper == [T(4), T(3)]
    MOI.delete(variable_bounds, cy)
    @test variable_bounds.lower == [T(3), nolb]
    @test variable_bounds.upper == [T(4), noub]
    sy = MOI.add_constraint(variable_bounds, y, MOI.Semiinteger(T(-2), T(-1)))
    @test variable_bounds.lower == [T(3), T(-2)]
    @test variable_bounds.upper == [T(4), T(-1)]
    MOI.delete(variable_bounds, sy)
    @test variable_bounds.lower == [T(3), nolb]
    @test variable_bounds.upper == [T(4), noub]
    ey = MOI.add_constraint(variable_bounds, y, MOI.EqualTo(T(-3)))
    @test variable_bounds.lower == [T(3), T(-3)]
    @test variable_bounds.upper == [T(4), T(-3)]
    MOI.delete(variable_bounds, ey)
    @test variable_bounds.lower == [T(3), nolb]
    @test variable_bounds.upper == [T(4), noub]
    MOI.delete(variable_bounds, cx)
    @test variable_bounds.lower == [nolb, nolb]
    @test variable_bounds.upper == [noub, noub]
end

function test_bound_vectors()
    _test_bound_vectors(Int, 0, 0)
    _test_bound_vectors(Float64, -Inf, Inf)
    return
end

function test_ListOfConstraintTypesPresent_2()
    for set in (
        MOI.EqualTo(1.0),
        MOI.GreaterThan(1.0),
        MOI.LessThan(1.0),
        MOI.Interval(1.0, 2.0),
        MOI.Semicontinuous(1.0, 2.0),
        MOI.Semiinteger(1.0, 2.0),
        MOI.Integer(),
        MOI.ZeroOne(),
    )
        model = MOI.Utilities.VariablesContainer{Float64}()
        x = MOI.add_variable(model)
        MOI.add_constraint(model, x, set)
        @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) ==
              [(MOI.VariableIndex, typeof(set))]
    end
    return
end

end  # module

TestVariableContainer.runtests()
