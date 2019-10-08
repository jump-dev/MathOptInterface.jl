using Test

@testset "$(file)" for file in readdir(@__DIR__)
    if file == "Test.jl"
        continue
    end
    include(file)
end
