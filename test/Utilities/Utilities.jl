using Test

for file in readdir(@__DIR__)
    if file in ["Utilities.jl"]
        continue
    end
    @testset "$(file)" begin
        include(file)
    end
end
