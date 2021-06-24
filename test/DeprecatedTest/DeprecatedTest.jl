using Test

@testset "$(file)" for file in readdir(@__DIR__)
    if file == "DeprecatedTest.jl"
        continue
    end
    include(file)
end
