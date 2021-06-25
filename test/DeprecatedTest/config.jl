using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.DeprecatedTest
const MOIU = MOI.Utilities

function atest(model::MOI.ModelLike, config::MOIT.Config{T}) where {T<:Real}
    @test config.atol == Base.rtoldefault(T)
    @test config.rtol == Base.rtoldefault(T)
    @test config.solve
    @test config.query
    @test config.duals
    @test config.infeas_certificates
end

function btest(model::MOI.ModelLike, config::MOIT.Config)
    @test false # b is in exclude
end

const customtests = Dict("a" => atest, "b" => btest)

MOIT.@moitestset custom

@testset "Config" begin
    mock = MOIU.MockOptimizer(MOIU.Model{Float64}())
    config = MOIT.Config()
    customtest(mock, config, ["b"])
end
