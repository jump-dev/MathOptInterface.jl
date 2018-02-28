@testset "Sets constant" begin
    @test MOIU.getconstant(MOI.EqualTo(3)) == 3
    @test MOIU.getconstant(MOI.GreaterThan(6)) == 6
    @test MOIU.getconstant(MOI.LessThan(2)) == 2
end
