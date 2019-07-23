using Test

@testset "Functions" begin
    include("functions.jl")
end
@testset "Sets" begin
    include("sets.jl")
end
@testset "Constraints" begin
    include("constraints.jl")
end
@testset "Model" begin
    include("model.jl")
end
@testset "Universal Fallback" begin
    include("universalfallback.jl")
end
@testset "Parser" begin
    include("parser.jl")
end
@testset "Mock Optimizer" begin
    include("mockoptimizer.jl")
end
@testset "Caching Optimizer" begin
    include("cachingoptimizer.jl")
end
@testset "Copy" begin
    include("copy.jl")
end

@testset "CleverDicts" begin
    include("CleverDicts.jl")
end
