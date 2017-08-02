using MathOptInterface, Base.Test

const MOI = MathOptInterface

include("solvers.jl")

include("contlinear.jl")
@testset "Continuous linear problems with $solver" for (solver, ɛ) in contlinear_solvers
    contlineartest(solver, ɛ)
end

include("contquadratic.jl")
@testset "Continuous quadratic problems with $solver" for (solver, ɛ) in contquadratic_solvers
    contquadratictest(solver, ɛ)
end

include("contconic.jl")
@testset "Continuous conic problems with $solver" for (solver, ɛ) in contconic_solvers
    contconictest(solver, ɛ)
end

include("intlinear.jl")
@testset "Mixed-integer linear problems with $solver" for (solver, ɛ) in intlinear_solvers
    intlineartest(solver, ɛ)
end
