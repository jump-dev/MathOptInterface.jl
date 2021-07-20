using Test

function _timed_include(file)
    println("Testing ", file)
    start = time()
    include(file)
    run_time = round(time() - start, digits = 1)
    return println("    Took $(run_time) seconds")
end

@testset "$(dir)" for dir in [".", "Variable", "Constraint", "Objective"]
    @testset "$(file)" for file in readdir(joinpath(@__DIR__, dir))
        if !endswith(file, ".jl")
            continue
        elseif (dir == ".") && (file == "Bridges.jl" || file == "utilities.jl")
            continue
        end
        _timed_include(joinpath(@__DIR__, dir, file))
    end
end
