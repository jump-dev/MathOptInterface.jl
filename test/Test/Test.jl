@testset "Config" begin
    include("config.jl")
end
@testset "Unit" begin
    include("unit.jl")
end
@testset "Continuous Linear" begin
    include("contlinear.jl")
end
@testset "Continuous Conic" begin
    include("contconic.jl")
end
@testset "Continuous Quadratic" begin
    include("contquadratic.jl")
end
@testset "Integer Linear" begin
    include("intlinear.jl")
end
@testset "Integer Conic" begin
    include("intconic.jl")
end
