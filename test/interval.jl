@testset "Interval" begin
    @test MOI.Interval(MOI.GreaterThan(1.0)) === MOI.Interval(1.0, Inf)
    @test MOI.Interval(MOI.LessThan(2.0)) === MOI.Interval(-Inf, 2.0)
    @test MOI.Interval(MOI.EqualTo(3.0)) === MOI.Interval(3.0, 3.0)

    @test MOI.Interval(MOI.GreaterThan(1.0f0)) === MOI.Interval(1.0f0, Inf32)
    @test MOI.Interval(MOI.LessThan(2.0f0)) === MOI.Interval(-Inf32, 2.0f0)
    @test MOI.Interval(MOI.EqualTo(3.0f0)) === MOI.Interval(3.0f0, 3.0f0)

    @test_throws MethodError MOI.Interval(MOI.GreaterThan(false))
    @test_throws MethodError MOI.Interval(MOI.LessThan(true))
    @test MOI.Interval(MOI.EqualTo(true)) === MOI.Interval(true, true)

    @test_throws MethodError MOI.Interval(MOI.GreaterThan(1))
    @test_throws MethodError MOI.Interval(MOI.LessThan(2))
    @test MOI.Interval(MOI.EqualTo(3)) === MOI.Interval(3, 3)

    @test MOI.Interval(MOI.Interval(1, 2)) == MOI.Interval(1, 2)
end
