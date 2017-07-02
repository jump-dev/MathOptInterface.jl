using MathOptInterface, Base.Test
# TODO using solvers?


include("contlinear.jl")
@testset "Continuous linear problems"
    # contlineartest(GLPKSolverLP())
end

include("contconic.jl")
@testset "Continuous conic problems"
    # contconictest(SCSSolver(verbose=0))
end

include("intlinear.jl")
@testset "Mixed-integer linear problems"
    # intlineartest(GLPKSolverMIP())
end
