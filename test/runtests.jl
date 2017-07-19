using MathOptInterface, Base.Test
# TODO using solvers?

const MOI = MathOptInterface

include("function_utilities.jl")

include("function_modification.jl")

include("contlinear.jl")
@testset "Continuous linear problems" begin
    # contlineartest(GLPKSolverLP())
end

include("contquadratic.jl")
@testset "Continuous quadratic problems" begin
    # contquadratictest(GurobiSolver())
end

include("contconic.jl")
@testset "Continuous conic problems" begin
    # contconictest(SCSSolver(verbose=0))
end

include("intlinear.jl")
@testset "Mixed-integer linear problems" begin
    # intlineartest(GLPKSolverMIP())
end
