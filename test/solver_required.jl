using CPLEX
const solver = CplexSolver()

@testset "1.mof.json" begin
    m = MOI.SolverInstance(joinpath("problems", "1.mof.json"), solver)
    MOI.optimize!(m)
    @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
end

@testset "1.mof.json" begin
    m = MOI.SolverInstance(joinpath("problems", "2.mof.json"), solver)
    MOI.optimize!(m)
    @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
end

@testset "linear7.mof.json" begin
    m = MOI.SolverInstance(joinpath("problems", "linear7.mof.json"), solver)
    MOI.optimize!(m)
    @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
end

@testset "qp1.mof.json" begin
    m = MOI.SolverInstance(joinpath("problems", "qp1.mof.json"), solver)
    MOI.optimize!(m)
    @test MOI.getattribute(m, MOI.TerminationStatus()) == MOI.Success
end
