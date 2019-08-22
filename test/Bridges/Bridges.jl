using Test

function _timed_include(file)
    println("Testing ", file)
    start = time()
    include(file)
    run_time = round(time() - start, digits=1)
    println("    Took $(run_time) seconds")
end

@testset "$(file)" for file in ["bridge_optimizer.jl", "lazy_bridge_optimizer.jl"]
    _timed_include(file)
end
@testset "Variable bridges" begin
    variable_dir = joinpath(@__DIR__, "Variable")
    @testset "$(file)" for file in readdir(variable_dir)
        _timed_include(joinpath(variable_dir, file))
    end
end
@testset "Constraint bridges" begin
    constraint_dir = joinpath(@__DIR__, "Constraint")
    @testset "$(file)" for file in readdir(constraint_dir)
        _timed_include(joinpath(constraint_dir, file))
    end
end
