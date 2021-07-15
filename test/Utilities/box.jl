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

function test_flag_to_set_type()
    T = Int
    @test_throws AssertionError MOI.Utilities.flag_to_set_type(0x11, T)
    @test MOI.Utilities.flag_to_set_type(0x10, T) == MOI.Integer
    @test MOI.Utilities.flag_to_set_type(0x20, T) == MOI.ZeroOne
    return
end

function test_equal()
    a = MOI.Utilities.Box([1, 2], [3, 4])
    b = MOI.Utilities.Box([1.0, 2.0], [3.0, 4.0])
    c = MOI.Utilities.Box([1.0, 2.0], [3.0, 5.0])
    @test a == a
    @test a == b
    @test a != c
    return
end

function test_empty()
    a = MOI.Utilities.Box([1, 2], [3, 4])
    empty!(a)
    @test a == MOI.Utilities.Box{Int}()
    return
end

function test_resize()
    a = MOI.Utilities.Box([1, 2], [3, 4])
    @test length(a.lower) == 2
    @test length(a.upper) == 2
    resize!(a, 4)
    @test length(a.lower) == 4
    @test length(a.upper) == 4
    return
end

function test_load_constants()
    a = MOI.Utilities.Box([-Inf, -Inf, -Inf], [Inf, Inf, Inf])
    MOI.Utilities.load_constants(a, 0, MOI.Interval(1.0, 2.0))
    MOI.Utilities.load_constants(a, 1, MOI.GreaterThan(3.0))
    MOI.Utilities.load_constants(a, 2, MOI.LessThan(4.0))
    @test a == MOI.Utilities.Box([1.0, 3.0, -Inf], [2.0, Inf, 4.0])
    return
end

function test_function_constants()
    a = MOI.Utilities.Box([-Inf, -Inf, -Inf], [Inf, Inf, Inf])
    MOI.Utilities.function_constants(a, 0) == 0.0
    return
end

function test_set_from_constants()
    a = MOI.Utilities.Box([1.0, 3.0, -Inf], [2.0, Inf, 4.0])
    @test MOI.Utilities.set_from_constants(a, MOI.Interval{Float64}, 1) ==
          MOI.Interval(1.0, 2.0)
    @test MOI.Utilities.set_from_constants(a, MOI.GreaterThan{Float64}, 2) ==
          MOI.GreaterThan(3.0)
    @test MOI.Utilities.set_from_constants(a, MOI.LessThan{Float64}, 3) ==
          MOI.LessThan(4.0)
    @test MOI.Utilities.set_from_constants(a, MOI.ZeroOne, 2) == MOI.ZeroOne()
    return
end

function test_merge_bounds()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_free(a)
    @test a == MOI.Utilities.Box{Int}(Int[0], Int[0])
    MOI.Utilities.load_constants(a, 0, MOI.GreaterThan(3))
    @test a == MOI.Utilities.Box{Int}(Int[3], Int[0])
    MOI.Utilities._merge_bounds(a, 1, MOI.LessThan(4))
    @test a == MOI.Utilities.Box{Int}(Int[3], Int[4])
    return
end

function test_add_free()
    a = MOI.Utilities.Box{Int}()
    MOI.Utilities._add_free(a)
    @test a == MOI.Utilities.Box{Int}(Int[0], Int[0])
    a = MOI.Utilities.Box{Float64}()
    MOI.Utilities._add_free(a)
    @test a == MOI.Utilities.Box{Float64}(Float64[-Inf], Float64[Inf])
    return
end

end  # module

TestBox.runtests()
