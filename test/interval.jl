@testset "Interval" begin
    @test MOI.Interval(MOI.GreaterThan(1.)) === MOI.Interval(1., Inf)
    @test MOI.Interval(MOI.LessThan(2.)) === MOI.Interval(-Inf, 2.)
    @test MOI.Interval(MOI.EqualTo(3.)) === MOI.Interval(3., 3.)

    @test MOI.Interval(MOI.GreaterThan(1.f0)) === MOI.Interval(1.f0, Inf32)
    @test MOI.Interval(MOI.LessThan(2.f0)) === MOI.Interval(-Inf32, 2.f0)
    @test MOI.Interval(MOI.EqualTo(3.f0)) === MOI.Interval(3.f0, 3.f0)

    @test_throws MethodError MOI.Interval(MOI.GreaterThan(false))
    @test_throws MethodError MOI.Interval(MOI.LessThan(true))
    @test MOI.Interval(MOI.EqualTo(true)) === MOI.Interval(true, true)

    @test_throws MethodError MOI.Interval(MOI.GreaterThan(1))
    @test_throws MethodError MOI.Interval(MOI.LessThan(2))
    @test MOI.Interval(MOI.EqualTo(3)) === MOI.Interval(3, 3)

    @test MOI.Interval(MOI.Interval(1, 2)) == MOI.Interval(1, 2)
end
