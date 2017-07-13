using MathOptInterface, Base.Test
# TODO using solvers?

const MOI = MathOptInterface

include("function_utilities.jl")

include("contlinear.jl")
@testset "Continuous linear problems" begin
    # contlineartest(GLPKSolverLP())
end

include("contconic.jl")
@testset "Continuous conic problems" begin
    # contconictest(SCSSolver(verbose=0))
end

include("intlinear.jl")
@testset "Mixed-integer linear problems" begin
    # intlineartest(GLPKSolverMIP())
end
