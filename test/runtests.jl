using Test

# This file gets called first. It it doesn't crash, all is well.
include("issue980.jl")

for file in readdir(@__DIR__)
    if file in ["issue980.jl", "dummy.jl", "hygiene.jl", "runtests.jl"]
        continue
    elseif !endswith(file, ".jl")
        continue
    end
    @testset "$(file)" begin
        include(file)
    end
end

@testset "MOI.$(submodule)" for submodule in [
    "Bridges",
    "FileFormats",
    "Test",
    "Utilities",
    "Benchmarks",
]
    include("$(submodule)/$(submodule).jl")
end

# Test hygiene of @model macro
include("hygiene.jl")
