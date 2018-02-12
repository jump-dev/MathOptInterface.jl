@testset "Interval" begin
    @test MOI.Interval(MOI.GreaterThan(1.)) === MOI.Interval(1., Inf)
    @test MOI.Interval(MOI.LessThan(2.)) === MOI.Interval(-Inf, 2.)
    @test MOI.Interval(MOI.EqualTo(3.)) === MOI.Interval(3., 3.)

    @test MOI.Interval(MOI.GreaterThan(1.f0)) === MOI.Interval(1.f0, Inf32)
    @test MOI.Interval(MOI.LessThan(2.f0)) === MOI.Interval(-Inf32, 2.f0)
    @test MOI.Interval(MOI.EqualTo(3.f0)) === MOI.Interval(3.f0, 3.f0)
end
