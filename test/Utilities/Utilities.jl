using Test

@testset "Functions" begin
    include("functions.jl")
end
@testset "Mutable Arithmetics" begin
    include("mutable_arithmetics.jl")
end
@testset "Sets" begin
    include("sets.jl")
end
@testset "Constraints" begin
    include("constraints.jl")
end
@testset "Variables" begin
    include("variables.jl")
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
@testset "DenseDict" begin
    include("dense_dict.jl")
end
@testset "Copy" begin
    include("copy.jl")
end

@testset "CleverDicts" begin
    include("CleverDicts.jl")
end
@testset "DoubleDicts" begin
    include("DoubleDicts.jl")
end
@testset "Lazy iterators" begin
    include("lazy_iterators.jl")
end

@testset "Print with acronym" begin
    include("print_with_acronym.jl")
end
