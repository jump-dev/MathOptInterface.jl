"""
    test_integer_integration(model::MOI.ModelLike, config::Config)

Run an integration test on the MILP:
```
maximize 1.1x + 2 y + 5 z
s.t.  x + y + z <= 10
      x + 2 y + z <= 15
      x is continuous: 0 <= x <= 5
      y is integer: 0 <= y <= 10
      z is binary
```
"""
function test_integer_integration(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.GreaterThan{Float64},
    )
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    cf =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0, 1.0], v), 0.0)
    c = MOI.add_constraint(model, cf, MOI.LessThan(10.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    cf2 =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 2.0, 1.0], v), 0.0)
    c2 = MOI.add_constraint(model, cf2, MOI.LessThan(15.0))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 2
    end
    vc1 = MOI.add_constraint(
        model,
        MOI.SingleVariable(v[1]),
        MOI.Interval(0.0, 5.0),
    )
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test vc1.value == v[1].value
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}(),
        ) == 1
    end
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(v[2]),
        MOI.Interval(0.0, 10.0),
    )
    @test vc2.value == v[2].value
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Interval{Float64}}(),
        ) == 2
    end
    vc3 = MOI.add_constraint(model, MOI.SingleVariable(v[2]), MOI.Integer())
    @test vc3.value == v[2].value
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Integer}(),
        ) == 1
    end
    vc4 = MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.ZeroOne())
    @test vc4.value == v[3].value
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}(),
        ) == 1
    end
    objf =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.1, 2.0, 5.0], v), 0.0)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        objf,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) in
              [MOI.FEASIBLE_POINT, MOI.NEARLY_FEASIBLE_POINT]
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 19.4 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [4, 5, 1] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 10 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c2) ≈ 15 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ObjectiveBound()) >= 19.4 - atol
        # FIXME the following are currently not implemented in MockOptimizer
        #        @test MOI.get(model, MOI.RelativeGap()) >= 0.0
        #        @test MOI.get(model, MOI.SolveTimeSec()) >= 0.0
        #        @test MOI.get(model, MOI.SimplexIterations()) >= 0
        #        @test MOI.get(model, MOI.BarrierIterations()) >= 0
        #        @test MOI.get(model, MOI.NodeCount()) >= 0
    end
    return
end

function setup_test(
    ::typeof(test_integer_integration),
    model::MOI.Utilities.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 20.0)
            MOIU.mock_optimize!(mock, [4, 5, 1])
        end,
    )
    return
end

"""
    test_SOS1_integration(model::MOI.ModelLike, config::Config)

Test Special Ordered Sets of type 1.
"""
function test_SOS1_integration(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SOS1{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.LessThan{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 3)
    @test MOI.get(model, MOI.NumberOfVariables()) == 3
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(v[1]), MOI.LessThan(1.0))
    @test vc1.value == v[1].value
    vc2 = MOI.add_constraint(model, MOI.SingleVariable(v[2]), MOI.LessThan(1.0))
    @test vc2.value == v[2].value
    vc3 = MOI.add_constraint(model, MOI.SingleVariable(v[3]), MOI.LessThan(2.0))
    @test vc3.value == v[3].value
    c1 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([v[1], v[2]]),
        MOI.SOS1([1.0, 2.0]),
    )
    c2 = MOI.add_constraint(
        model,
        MOI.VectorOfVariables([v[1], v[3]]),
        MOI.SOS1([1.0, 2.0]),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.SOS1{Float64}}(),
        ) == 2
    end
    #=
        To allow for permutations in the sets and variable vectors
        we're going to sort according to the weights
    =#
    cs_sos = MOI.get(model, MOI.ConstraintSet(), c2)
    cf_sos = MOI.get(model, MOI.ConstraintFunction(), c2)
    p = sortperm(cs_sos.weights)
    @test cs_sos.weights[p] ≈ [1.0, 2.0] atol = atol rtol = rtol
    @test cf_sos.variables[p] == v[[1, 3]]
    objf =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2.0, 1.0, 1.0], v), 0.0)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        objf,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 1, 2] atol = atol rtol =
            rtol
    end
    MOI.delete(model, c1)
    MOI.delete(model, c2)
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 5 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 1, 2] atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_SOS1_integration),
    model::MOI.Utilities.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 2]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 2]),
    )
    return
end

"""
    test_SOS2_integration(model::MOI.ModelLike, config::Config)

Test Special Ordered Sets of type 2.
"""
function test_SOS2_integration(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol

    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SOS1{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.SOS2{Float64},
    )
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 10)
    @test MOI.get(model, MOI.NumberOfVariables()) == 10
    bin_constraints = []
    for i in 1:8
        vc = MOI.add_constraint(
            model,
            MOI.SingleVariable(v[i]),
            MOI.Interval(0.0, 2.0),
        )
        @test vc.value == v[i].value
        push!(
            bin_constraints,
            MOI.add_constraint(model, MOI.SingleVariable(v[i]), MOI.ZeroOne()),
        )
        @test bin_constraints[i].value == v[i].value
    end
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 2.0, 3.0, -1.0], v[[1, 2, 3, 9]]),
            0.0,
        ),
        MOI.EqualTo(0.0),
    )
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                [5.0, 4.0, 7.0, 2.0, 1.0, -1.0],
                v[[4, 5, 6, 7, 8, 10]],
            ),
            0.0,
        ),
        MOI.EqualTo(0.0),
    )
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(v[[1, 2, 3]]),
        MOI.SOS1([1.0, 2.0, 3.0]),
    )
    vv = MOI.VectorOfVariables(v[[4, 5, 6, 7, 8]])
    sos2 = MOI.SOS2([5.0, 4.0, 7.0, 2.0, 1.0])
    c = MOI.add_constraint(model, vv, sos2)
    #=
        To allow for permutations in the sets and variable vectors
        we're going to sort according to the weights
    =#
    cs_sos = MOI.get(model, MOI.ConstraintSet(), c)
    cf_sos = MOI.get(model, MOI.ConstraintFunction(), c)
    p = sortperm(cs_sos.weights)
    @test cs_sos.weights[p] ≈ [1.0, 2.0, 4.0, 5.0, 7.0] atol = atol rtol = rtol
    @test cf_sos.variables[p] == v[[8, 7, 5, 4, 6]]
    objf = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1.0, 1.0], [v[9], v[10]]),
        0.0,
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        objf,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 15.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈
              [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 12.0] atol = atol rtol =
            rtol
    end
    for cref in bin_constraints
        MOI.delete(model, cref)
    end
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 30.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈
              [0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 0.0, 6.0, 24.0] atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_SOS2_integration),
    model::MOI.Utilities.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 3.0, 12.0],
        ),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            [0.0, 0.0, 2.0, 2.0, 0.0, 2.0, 0.0, 0.0, 6.0, 24.0],
        ),
    )
    return
end

"""
    test_integer_solve_twice(model::MOI.ModelLike, config::Config)

Test solving a model twice on the integer knapsack problem.
The problem is:
```
max   z - 0.5 ( b1 + b2 + b3) / 40
s.t.  0 <= z - 0.5 eᵀ b / 40 <= 0.999
      b1, b2, ... b10 ∈ {0, 1}
      z in {0, 1, 2, ..., 100}
```
"""
function test_integer_solve_twice(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.Integer)
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    z = MOI.add_variable(model)
    vc1 = MOI.add_constraint(model, MOI.SingleVariable(z), MOI.Integer())
    @test vc1.value == z.value
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(z),
        MOI.Interval(0.0, 100.0),
    )
    @test vc2.value == z.value
    b = MOI.add_variables(model, 10)
    for bi in b
        vc = MOI.add_constraint(model, MOI.SingleVariable(bi), MOI.ZeroOne())
        @test vc.value == bi.value
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(vcat(1.0, fill(-0.5 / 40, 10)), vcat(z, b)),
            0.0,
        ),
        MOI.Interval(0.0, 0.999),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.(
                vcat(1.0, fill(-0.5 / 40, 3)),
                vcat(z, b[1:3]),
            ),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol = atol rtol = rtol
        # test for CPLEX.jl #76
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1 atol = atol rtol = rtol
    end
    return
end

function setup_test(
    ::typeof(test_integer_solve_twice),
    model::MOI.Utilities.MockOptimizer,
    ::Config,
)
    # FIXME [1, 0...] is not the correct optimal solution but it passes the test
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.0; zeros(10)]),
    )
    return
end

"""
    test_integer_knapsack(model::MOI.ModelLike, config::Config)

Test the integer knapsack problem
```
max 5a + 3b + 2c + 7d + 4e
st  2a + 8b + 4c + 2d + 5e <= 10
     a,   b,   c,   d,   e ∈ binary
```
"""
function test_integer_knapsack(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    v = MOI.add_variables(model, 5)
    @test MOI.get(model, MOI.NumberOfVariables()) == 5
    for vi in v
        vc = MOI.add_constraint(model, MOI.SingleVariable(vi), MOI.ZeroOne())
        @test vc.value == vi.value
    end
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.ZeroOne}(),
        ) == 5
    end
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2.0, 8.0, 4.0, 2.0, 5.0], v),
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 1
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([5.0, 3.0, 2.0, 7.0, 4.0], v),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if MOI.supports(model, MOI.VariablePrimalStart(), MOI.VariableIndex)
        MOI.set(model, MOI.VariablePrimalStart(), v, [0.0, 0.0, 0.0, 0.0, 0.0])
    end
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.PrimalStatus()) in
              [MOI.FEASIBLE_POINT, MOI.NEARLY_FEASIBLE_POINT]
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 16 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [1, 0, 0, 1, 1] atol =
            atol rtol = rtol
    end
    return
end

function setup_test(
    ::typeof(test_integer_knapsack),
    model::MOI.Utilities.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1, 0, 0, 1, 1]),
    )
    return
end

"""
    test_Indicator_integration(model::MOI.ModelLike, config::Config)

Test the problem:
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 ==> x2 <= 8
     z2 ==> x2 + x1/5 <= 9
     z1 + z2 >= 1
```
"""
function test_Indicator_integration(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    )
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    MOI.add_constraint(model, z1, MOI.ZeroOne())
    MOI.add_constraint(model, z2, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(8.0))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.2, x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(9.0))
    MOI.add_constraint(model, f2, iset2)
    # Additional regular constraint.
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, x2)],
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    # Disjunction z1 ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, z1), MOI.ScalarAffineTerm(1.0, z2)],
            0.0,
        ),
        MOI.GreaterThan(1.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2.0, 3.0], [x1, x2]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 28.75 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x1) ≈ 1.25 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x2) ≈ 8.75 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z1) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z2) ≈ 1.0 atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_Indicator_integration),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.25, 8.75, 0.0, 1.0]),
    )
    return
end

"""
    test_Indicator_ON_ONE(model::MOI.ModelLike, config::Config)

Test the problem:
```
max  2x1 + 3x2 - 30 z2
s.t. x1 + x2 <= 10
     z1 ==> x2 <= 8
     z2 ==> x2 + x1/5 <= 9
     z1 + z2 >= 1
```
"""
function test_Indicator_ON_ONE(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    MOI.add_constraint(model, z1, MOI.ZeroOne())
    MOI.add_constraint(model, z2, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(8.0))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.2, x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(9.0))
    MOI.add_constraint(model, f2, iset2)
    # additional regular constraint
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, x2)],
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    # disjunction z1 ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, z1), MOI.ScalarAffineTerm(1.0, z2)],
            0.0,
        ),
        MOI.GreaterThan(1.0),
    )
    # objective penalized on z2
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2.0, 3.0, -30.0], [x1, x2, z2]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 28.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x1) ≈ 2.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x2) ≈ 8.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z1) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z2) ≈ 0.0 atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_Indicator_ON_ONE),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [2.0, 8.0, 1.0, 0.0]),
    )
    return
end

"""
    test_Indicator_ON_ZERO(model::MOI.ModelLike, config::Config)

Test the problem:
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 == 0 ==> x2 <= 8
     z2 == 1 ==> x2 + x1/5 <= 9
     (1-z1) + z2 >= 1 <=> z2 - z1 >= 0
```
"""
function test_Indicator_ON_ZERO(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    )
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    vc1 = MOI.add_constraint(model, z1, MOI.ZeroOne())
    @test vc1.value == z1.value
    vc2 = MOI.add_constraint(model, z2, MOI.ZeroOne())
    @test vc2.value == z2.value
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ZERO}(MOI.LessThan(8.0))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.2, x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 0.0],
    )
    iset2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(9.0))
    MOI.add_constraint(model, f2, iset2)
    # Additional regular constraint.
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, x2)],
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    # Disjunction (1-z1) ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(-1.0, z1), MOI.ScalarAffineTerm(1.0, z2)],
            0.0,
        ),
        MOI.GreaterThan(0.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2.0, 3.0], [x1, x2]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 28.75 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x1) ≈ 1.25 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x2) ≈ 8.75 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z1) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z2) ≈ 1.0 atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_Indicator_ON_ZERO),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.25, 8.75, 1.0, 1.0]),
    )
    return
end

"""
    test_Indicator_constant_term(model::MOI.ModelLike, config::Config)

Test Indicator constraints with a constant term on the left-hand side. The
problem is:
```
max  2x1 + 3x2
s.t. x1 + x2 <= 10
     z1 ==> x2 - 1 <= 7
     z2 ==> x2 + x1/5 + 1 <= 10
     z1 + z2 >= 1
```
"""
function test_Indicator_constant_term(model::MOI.ModelLike, config::Config)
    atol = config.atol
    rtol = config.rtol
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.ZeroOne)
    @test MOI.supports_constraint(
        model,
        MOI.SingleVariable,
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.Interval{Float64},
    )
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    )
    x1 = MOI.add_variable(model)
    x2 = MOI.add_variable(model)
    z1 = MOI.add_variable(model)
    z2 = MOI.add_variable(model)
    MOI.add_constraint(model, z1, MOI.ZeroOne())
    MOI.add_constraint(model, z2, MOI.ZeroOne())
    f1 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, -1.0],
    )
    iset1 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(7.0))
    MOI.add_constraint(model, f1, iset1)
    f2 = MOI.VectorAffineFunction(
        [
            MOI.VectorAffineTerm(1, MOI.ScalarAffineTerm(1.0, z2)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(0.2, x1)),
            MOI.VectorAffineTerm(2, MOI.ScalarAffineTerm(1.0, x2)),
        ],
        [0.0, 1.0],
    )
    iset2 = MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(10.0))
    MOI.add_constraint(model, f2, iset2)
    # Additional regular constraint.
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, x1), MOI.ScalarAffineTerm(1.0, x2)],
            0.0,
        ),
        MOI.LessThan(10.0),
    )
    # Disjunction z1 ⋁ z2
    MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(
            [MOI.ScalarAffineTerm(1.0, z1), MOI.ScalarAffineTerm(1.0, z2)],
            0.0,
        ),
        MOI.GreaterThan(1.0),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([2.0, 3.0], [x1, x2]),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMAL
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 28.75 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x1) ≈ 1.25 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), x2) ≈ 8.75 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z1) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.VariablePrimal(), z2) ≈ 1.0 atol = atol rtol =
            rtol
    end
    return
end

function setup_test(
    ::typeof(test_Indicator_constant_term),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) ->
            MOIU.mock_optimize!(mock, [1.25, 8.75, 0.0, 1.0]),
    )
    return
end

function _test_SemiXXX_integration(
    model::MOI.ModelLike,
    config::Config{T},
    use_semiinteger::Bool,
) where {T}
    atol = config.atol
    rtol = config.rtol
    @test MOI.supports_incremental_interface(model, false) #=copy_names=#
    @test MOI.supports(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
    )
    @test MOI.supports(model, MOI.ObjectiveSense())
    if use_semiinteger
        @test MOI.supports_constraint(
            model,
            MOI.SingleVariable,
            MOI.Semiinteger{T},
        )
    else
        @test MOI.supports_constraint(
            model,
            MOI.SingleVariable,
            MOI.Semicontinuous{T},
        )
    end
    # 2 variables
    #
    # min  x
    # st   x >= y
    #      if !int
    #           x ∈ {0.0} U [2.0,3.0]
    #      if int
    #           x ∈ {0.0} U {2.0} U {3.0}
    #      y = 0.0
    MOI.empty!(model)
    @test MOI.is_empty(model)
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    if !use_semiinteger
        vc1 = MOI.add_constraint(
            model,
            MOI.SingleVariable(v[1]),
            MOI.Semicontinuous(T(2), T(3)),
        )
        if config.query_number_of_constraints
            @test MOI.get(
                model,
                MOI.NumberOfConstraints{
                    MOI.SingleVariable,
                    MOI.Semicontinuous{T},
                }(),
            ) == 1
        end
    else
        vc1 = MOI.add_constraint(
            model,
            MOI.SingleVariable(v[1]),
            MOI.Semiinteger(T(2), T(3)),
        )
        if config.query_number_of_constraints
            @test MOI.get(
                model,
                MOI.NumberOfConstraints{MOI.SingleVariable,MOI.Semiinteger{T}}(),
            ) == 1
        end
    end
    vc2 = MOI.add_constraint(
        model,
        MOI.SingleVariable(v[2]),
        MOI.EqualTo(zero(T)),
    )
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{T}}(),
        ) == 1
    end
    cf = MOI.ScalarAffineFunction{T}(
        MOI.ScalarAffineTerm{T}.([one(T), -one(T)], v),
        zero(T),
    )
    c = MOI.add_constraint(model, cf, MOI.GreaterThan(zero(T)))
    if config.query_number_of_constraints
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{T},
                MOI.GreaterThan{T},
            }(),
        ) == 1
    end
    objf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 0.0], v), 0.0)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        objf,
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MIN_SENSE
    if config.solve
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) in
              [MOI.FEASIBLE_POINT, MOI.NEARLY_FEASIBLE_POINT]
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [0, 0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ObjectiveBound()) <= 0.0 + atol
    end
    # Change y fixed value
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(one(T)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2.0, 1.0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 1.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ObjectiveBound()) <= 2.0 + atol
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(2)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2.0, 2.0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ObjectiveBound()) <= 2.0 + atol
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(5 // 2)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        if !use_semiinteger
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 2.5 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [2.5, 2.5] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 0.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ObjectiveBound()) <= 2.5 + atol
        else
            @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3.0 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [3.0, 2.5] atol =
                atol rtol = rtol
            @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 0.5 atol = atol rtol =
                rtol
            @test MOI.get(model, MOI.ObjectiveBound()) <= 3.0 + atol
        end
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(3)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == config.optimal_status
        @test MOI.get(model, MOI.ResultCount()) >= 1
        @test MOI.get(model, MOI.PrimalStatus()) == MOI.FEASIBLE_POINT
        @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3.0 atol = atol rtol = rtol
        @test MOI.get(model, MOI.VariablePrimal(), v) ≈ [3.0, 3.0] atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ConstraintPrimal(), c) ≈ 0.0 atol = atol rtol =
            rtol
        @test MOI.get(model, MOI.ObjectiveBound()) <= 3.0 + atol
    end
    MOI.set(model, MOI.ConstraintSet(), vc2, MOI.EqualTo(T(4)))
    if config.solve
        MOI.optimize!(model)
        @test MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE ||
              MOI.get(model, MOI.TerminationStatus()) ==
              MOI.INFEASIBLE_OR_UNBOUNDED
    end
    return
end

"""
    test_Semicontinuous_integration(model::MOI.ModelLike, config::Config)

Run an integration test on Semicontinuous constraints.
"""
function test_Semicontinuous_integration(model::MOI.ModelLike, config::Config)
    _test_SemiXXX_integration(model, config, false)
    return
end

function setup_test(
    ::typeof(test_Semicontinuous_integration),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 0.0)
            MOIU.mock_optimize!(mock, [0.0, 0.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 2.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.5)
            MOIU.mock_optimize!(mock, [2.5, 2.5])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, [3.0, 3.0])
        end,
        (mock::MOIU.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    return
end

"""
    test_Semiinteger_integration(model::MOI.ModelLike, config::Config)

Run an integration test on Semiinteger constraints.
"""
function test_Semiinteger_integration(model::MOI.ModelLike, config::Config)
    _test_SemiXXX_integration(model, config, true)
    return
end

function setup_test(
    ::typeof(test_Semiinteger_integration),
    model::MOIU.MockOptimizer,
    ::Config,
)
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 0.0)
            MOIU.mock_optimize!(mock, [0.0, 0.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 1.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 2.0)
            MOIU.mock_optimize!(mock, [2.0, 2.0])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, [3.0, 2.5])
        end,
        (mock::MOIU.MockOptimizer) -> begin
            MOI.set(mock, MOI.ObjectiveBound(), 3.0)
            MOIU.mock_optimize!(mock, [3.0, 3.0])
        end,
        (mock::MOIU.MockOptimizer) ->
            MOI.set(mock, MOI.TerminationStatus(), MOI.INFEASIBLE),
    )
    return
end
