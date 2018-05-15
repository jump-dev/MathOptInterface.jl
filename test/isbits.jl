# Test isbit-ness of VariableIndex and terms
# It is important that these struct remain isbits as otherwise,
# the performance of function will be deteriored.
# These tests explicit this design choice and makes sure that it remains the case
@testset "isbits" begin
    x = @inferred MOI.VariableIndex(1)
    @test isbits(x)
    at = @inferred MOI.ScalarAffineTerm(1.0, x)
    @test isbits(at)
    @test isbits(@inferred MOI.VectorAffineTerm(1, at))
    qt = @inferred MOI.ScalarQuadraticTerm(1.0, x, x)
    @test isbits(qt)
    @test isbits(@inferred MOI.VectorQuadraticTerm(1, qt))
end
