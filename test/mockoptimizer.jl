# Sets variable primal to varprim
function mock_dual!(optimizer::MOIU.MockOptimizer) end
function mock_dual!(optimizer::MOIU.MockOptimizer, condual::Pair, conduals...)
    F, S = condual.first
    duals = condual.second
    for (i, ci) in enumerate(MOI.get(optimizer, MOI.ListOfConstraintIndices{F, S}()))
        MOI.set!(optimizer, MOI.ConstraintDual(), ci, duals[i])
    end
    mock_dual!(optimizer, conduals...)
end
function mock_dual!(optimizer::MOIU.MockOptimizer, dualstatus::MOI.ResultStatusCode, conduals::Pair...)
    MOI.set!(optimizer, MOI.DualStatus(), dualstatus)
    mock_dual!(optimizer, conduals...)
end
dual_args(dualstatus::MOI.ResultStatusCode, conduals::Pair...) = dualstatus, conduals...
function dual_args(conduals::Pair...)
    # Feasible dual solution
    MOI.FeasiblePoint, conduals...
end
function mock_primal!(optimizer::MOIU.MockOptimizer, primstatus::MOI.ResultStatusCode, varprim::Vector, dual...)
    MOI.set!(optimizer, MOI.PrimalStatus(), primstatus)
    MOI.set!(optimizer, MOI.VariablePrimal(), MOI.get(optimizer, MOI.ListOfVariableIndices()), varprim)
    mock_dual!(optimizer, dual_args(dual...)...)
end
function mock_primal!(optimizer::MOIU.MockOptimizer, varprim::Vector, dual...)
    # Feasible primal solution
    mock_primal!(optimizer, MOI.FeasiblePoint, varprim, dual...)
end
function mock_primal!(optimizer::MOIU.MockOptimizer, conduals::Pair...)
    # No primal solution
    MOI.set!(optimizer, MOI.PrimalStatus(), MOI.InfeasiblePoint)
    mock_dual!(optimizer, MOI.InfeasibilityCertificate, conduals...)
end

"""
    mock_optimize!(optimizer::MOIU.MockOptimizer, primstatus::MOI.ResultStatusCode, varprim::Vector, dualstatus::MOI.ResultStatusCode, conduals::Pair...)

Sets the termination status of `optimizer` to `MOI.Success`, the result count to 1, the primal (resp. dual) status to `primstatus` (resp. `dualstatus`).
The primal values of the variables in the order returned by `ListOfVariableIndices` are set to `varprim`.
If `primstatus` (resp. `dualstatus`) is missing, it is assumed to be `MOI.FeasiblePoint`.
The dual values are set to the values specified by `conduals`. Each pair is of the form `(F,S)=>[...]` where `[...]` is the the vector of dual values for the constraints `F`-in-`S` in the order returned by `ListOfConstraintIndices{F,S}`.
If `primstatus`, `varprim` and `dualstatus`, the problem is assumed to be infeasible with the infeasibility certificate contained in `conduals`.
"""
function mock_optimize!(optimizer::MOIU.MockOptimizer, primdual...)
    MOI.set!(optimizer, MOI.TerminationStatus(), MOI.Success)
    MOI.set!(optimizer, MOI.ResultCount(), 1)
    mock_primal!(optimizer, primdual...)
end

@testset "Mock optimizer continuous linear tests" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())
    config = MOIT.TestConfig(solve=false)
    MOIT.contlineartest(optimizer, config)
end

# This both tests the ConstraintPrimal value requested in the tests and the ConstraintPrimal implemented in MockOptimizer
@testset "Mock optimizer automatic constraint primal" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())
    config = MOIT.TestConfig()
    optimizer.evalobjective = true
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> mock_optimize!(optimizer, [1.0, 0.0, 2.0], (MOI.VectorOfVariables, MOI.Nonnegatives) => [[0, 2, 0]], (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-3, -1]])
    MOIT.lin1vtest(optimizer, config)
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> mock_optimize!(optimizer, [1.0, 0.0, 2.0], (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0, 2, 0]], (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[-3, -1]])
    MOIT.lin1ftest(optimizer, config)
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> mock_optimize!(optimizer, [-4, -3, 16, 0], (MOI.VectorOfVariables, MOI.Nonnegatives) => [[0]], (MOI.VectorOfVariables, MOI.Nonpositives) => [[0]], (MOI.VectorOfVariables, MOI.Zeros) => [[7]], (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[7, 2, -4]])
    MOIT.lin2vtest(optimizer, config)
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> mock_optimize!(optimizer, [-4, -3, 16, 0], (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]], (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[0]], (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[7, 2, -4], [7]], )
    MOIT.lin2ftest(optimizer, config)
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> mock_optimize!(optimizer)
    MOIT.lin3test(optimizer, config)
    optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> mock_optimize!(optimizer)
    MOIT.lin4test(optimizer, config)
end

@testset "Mock optimizer optimizer attributes" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())
    @test MOI.canset(optimizer, MOIU.MockModelAttribute())
    MOI.set!(optimizer, MOIU.MockModelAttribute(), 10)
    @test MOI.canget(optimizer, MOIU.MockModelAttribute())
    @test MOI.get(optimizer, MOIU.MockModelAttribute()) == 10

    v1 = MOI.addvariable!(optimizer)
    @test MOI.canset(optimizer, MOIU.MockVariableAttribute(), typeof(v1))
    MOI.set!(optimizer, MOIU.MockVariableAttribute(), v1, 11)
    @test MOI.canget(optimizer, MOIU.MockVariableAttribute(), typeof(v1))
    @test MOI.get(optimizer, MOIU.MockVariableAttribute(), v1) == 11
    MOI.set!(optimizer, MOIU.MockVariableAttribute(), [v1], [-11])
    @test MOI.get(optimizer, MOIU.MockVariableAttribute(), [v1]) == [-11]

    @test MOI.canaddconstraint(optimizer, MOI.SingleVariable, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(optimizer, MOI.SingleVariable(v1), MOI.GreaterThan(1.0))
    @test MOI.canset(optimizer, MOIU.MockConstraintAttribute(), typeof(c1))
    MOI.set!(optimizer, MOIU.MockConstraintAttribute(), c1, 12)
    @test MOI.canget(optimizer, MOIU.MockConstraintAttribute(), typeof(c1))
    @test MOI.get(optimizer, MOIU.MockConstraintAttribute(), c1) == 12
    MOI.set!(optimizer, MOIU.MockConstraintAttribute(), [c1], [-12])
    @test MOI.get(optimizer, MOIU.MockConstraintAttribute(), [c1]) == [-12]
end

@testset "Mock optimizer optimizer solve no result" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())

    v1 = MOI.addvariable!(optimizer)

    # Load fake solution
    MOI.set!(optimizer, MOI.TerminationStatus(), MOI.InfeasibleNoResult)

    # Attributes are hidden until after optimize!()
    @test !MOI.canget(optimizer, MOI.TerminationStatus())
    @test !MOI.canget(optimizer, MOI.ResultCount())
    @test !MOI.canget(optimizer, MOI.VariablePrimal(), MOI.VariableIndex)

    MOI.optimize!(optimizer)
    @test MOI.canget(optimizer, MOI.TerminationStatus())
    @test MOI.canget(optimizer, MOI.ResultCount())
    @test !MOI.canget(optimizer, MOI.VariablePrimal(), typeof(v1))
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.InfeasibleNoResult
    @test MOI.get(optimizer, MOI.ResultCount()) == 0
end

@testset "Mock optimizer optimizer solve with result" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())

    v = MOI.addvariables!(optimizer, 2)
    c1 = MOI.addconstraint!(optimizer, MOI.SingleVariable(v[1]), MOI.GreaterThan(1.0))
    soc = MOI.addconstraint!(optimizer, MOI.VectorOfVariables(v), MOI.SecondOrderCone(2))


    # Load fake solution
    # TODO: Provide a more compact API for this.
    MOI.set!(optimizer, MOI.TerminationStatus(), MOI.Success)
    MOI.set!(optimizer, MOI.ObjectiveValue(), 1.0)
    MOI.set!(optimizer, MOI.ResultCount(), 1)
    MOI.set!(optimizer, MOI.PrimalStatus(), MOI.FeasiblePoint)
    MOI.set!(optimizer, MOI.DualStatus(), MOI.FeasiblePoint)
    MOI.set!(optimizer, MOI.VariablePrimal(), v, [1.0, 2.0])
    MOI.set!(optimizer, MOI.VariablePrimal(), v[1], 3.0)
    MOI.set!(optimizer, MOI.ConstraintDual(), c1, 5.9)
    MOI.set!(optimizer, MOI.ConstraintDual(), soc, [1.0,2.0])

    # Attributes are hidden until after optimize!()
    @test !MOI.canget(optimizer, MOI.TerminationStatus())
    @test !MOI.canget(optimizer, MOI.ResultCount())
    @test !MOI.canget(optimizer, MOI.VariablePrimal(), typeof(v[1]))
    @test !MOI.canget(optimizer, MOI.ConstraintDual(), typeof(c1))
    @test !MOI.canget(optimizer, MOI.ConstraintDual(), typeof(soc))

    MOI.optimize!(optimizer)
    @test MOI.canget(optimizer, MOI.TerminationStatus())
    @test MOI.canget(optimizer, MOI.ResultCount())
    @test MOI.canget(optimizer, MOI.ObjectiveValue())
    @test MOI.canget(optimizer, MOI.PrimalStatus())
    @test MOI.canget(optimizer, MOI.DualStatus())
    @test MOI.canget(optimizer, MOI.VariablePrimal(), typeof(v[1]))
    @test MOI.canget(optimizer, MOI.ConstraintDual(), typeof(c1))
    @test MOI.canget(optimizer, MOI.ConstraintDual(), typeof(soc))
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(optimizer, MOI.ResultCount()) == 1
    @test MOI.get(optimizer, MOI.ObjectiveValue()) == 1.0
    @test MOI.get(optimizer, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(optimizer, MOI.DualStatus()) == MOI.FeasiblePoint
    @test MOI.get(optimizer, MOI.VariablePrimal(), v) == [3.0, 2.0]
    @test MOI.get(optimizer, MOI.VariablePrimal(), v[1]) == 3.0
    @test MOI.get(optimizer, MOI.ConstraintDual(), c1) == 5.9
    @test MOI.get(optimizer, MOI.ConstraintDual(), soc) == [1.0,2.0]
end
