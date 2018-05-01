@testset "Sets constant" begin
    @test MOIU.getconstant(MOI.EqualTo(3)) == 3
    @test MOIU.getconstant(MOI.GreaterThan(6)) == 6
    @test MOIU.getconstant(MOI.LessThan(2)) == 2
end

@testset "Set dimension" begin
    @test MOI.dimension(MOI.EqualTo(3.0)) === 1
    @test MOI.dimension(MOI.Reals(8)) === 8
    @test MOI.dimension(MOI.DualExponentialCone()) === 3
    @test MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(4)) === 10
    @test MOI.dimension(MOI.PositiveSemidefiniteConeSquare(5)) === 25
    @test MOI.dimension(MOI.RootDetConeTriangle(6)) === 22
    @test MOI.dimension(MOI.LogDetConeTriangle(6)) === 22
    @test MOI.dimension(MOI.RootDetConeSquare(4)) === 17
    @test MOI.dimension(MOI.LogDetConeSquare(4)) === 17
    @test MOI.dimension(MOI.SOS2(collect(1 : 6))) === 6
end
