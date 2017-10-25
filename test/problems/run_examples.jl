using MathOptInterface, MathOptFormat, CPLEX, Base.Test
const MOI = MathOptInterface

const solver = CplexSolver()

for prob in [
        "1.mof.json",
        "2.mof.json",
        "LIN1.mof.json",
        "LIN2.mof.json",
        "linear1.mof.json",
        "linear7.mof.json",
        "qp1.mof.json",
        "mip01.mof.json"
    ]
    @testset "$(prob)" begin
        m = MOI.SolverInstance(joinpath(@__DIR__, prob), solver)
        MOI.optimize!(m)
        @test MOI.get(m, MOI.TerminationStatus()) == MOI.Success
    end
end
