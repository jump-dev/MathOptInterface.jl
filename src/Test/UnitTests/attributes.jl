"""
    solver_name(model::MOI.ModelLike, config::Config)

Test that the [`MOI.SolverName`](@ref) attribute is implemented for `model`.
"""
function solver_name(model::MOI.ModelLike, config::Config)
    if config.solve
        @test MOI.get(model, MOI.SolverName()) isa AbstractString
    end
end
unittests["solver_name"] = solver_name

"""
    silent(model::MOI.ModelLike, config::Config)

Test that the [`MOI.Silent`](@ref) attribute is implemented for `model`.
"""
function silent(model::MOI.ModelLike, config::Config)
    if config.solve
        @test MOI.supports(model, MOI.Silent())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.Silent())
        MOI.set(model, MOI.Silent(), !value)
        @test !value == MOI.get(model, MOI.Silent())
        # Check that `set` does not just take `!` of the current value
        MOI.set(model, MOI.Silent(), !value)
        @test !value == MOI.get(model, MOI.Silent())
        MOI.set(model, MOI.Silent(), value)
        @test value == MOI.get(model, MOI.Silent())
    end
end
unittests["silent"] = silent

"""
    time_limit_sec(model::MOI.ModelLike, config::Config)

Test that the [`MOI.TimeLimitSec`](@ref) attribute is implemented for `model`.
"""
function time_limit_sec(model::MOI.ModelLike, config::Config)
    if config.solve
        @test MOI.supports(model, MOI.TimeLimitSec())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.TimeLimitSec())
        MOI.set(model, MOI.TimeLimitSec(), 0.0)
        @test MOI.get(model, MOI.TimeLimitSec()) == 0.0
        MOI.set(model, MOI.TimeLimitSec(), 1.0)
        @test MOI.get(model, MOI.TimeLimitSec()) == 1.0
        MOI.set(model, MOI.TimeLimitSec(), value)
        @test value == MOI.get(model, MOI.TimeLimitSec()) # Equality should hold
    end
end
unittests["time_limit_sec"] = time_limit_sec

"""
    number_threads(model::MOI.ModelLike, config::Config)

Test that the [`MOI.NumberOfThreads`](@ref) attribute is implemented for `model`.
"""
function number_threads(model::MOI.ModelLike, config::Config)
    if config.solve
        @test MOI.supports(model, MOI.NumberOfThreads())
        # Get the current value to restore it at the end of the test
        value = MOI.get(model, MOI.NumberOfThreads())
        MOI.set(model, MOI.NumberOfThreads(), 1)
        @test MOI.get(model, MOI.NumberOfThreads()) == 1
        MOI.set(model, MOI.NumberOfThreads(), 3)
        @test MOI.get(model, MOI.NumberOfThreads()) == 3
        MOI.set(model, MOI.NumberOfThreads(), value)
        @test value == MOI.get(model, MOI.NumberOfThreads())
    end
end
unittests["number_threads"] = number_threads

"""
    raw_status_string(model::MOI.ModelLike, config::Config)

Test that the [`MOI.RawStatusString`](@ref) attribute is implemented for
`model`.
"""
function raw_status_string(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    test_model_solution(
        model,
        config,
        objective_value = 0.0,
        variable_primal = [(x, 0.0)],
    )
    if config.solve
        @test MOI.get(model, MOI.RawStatusString()) isa AbstractString
    end
end
unittests["raw_status_string"] = raw_status_string

"""
    solve_time(model::MOI.ModelLike, config::Config)

Test that the [`MOI.SolveTimeSec`](@ref) attribute is implemented for `model`.
"""
function solve_time(model::MOI.ModelLike, config::Config)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    MOI.add_constraint(model, MOI.SingleVariable(x), MOI.LessThan(0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.SingleVariable}(),
        MOI.SingleVariable(x),
    )
    test_model_solution(
        model,
        config,
        objective_value = 0.0,
        variable_primal = [(x, 0.0)],
    )
    if config.solve
        time = MOI.get(model, MOI.SolveTimeSec())
        @test time ≥ 0.0
    end
end
unittests["solve_time"] = solve_time

# Model and Optimizer attributes.
for attr in (
    :BarrierIterations,
    :ConflictStatus,
    :DualStatus,
    :Name,
    :NodeCount,
    :NumberOfThreads,
    :NumberOfVariables,
    :ObjectiveFunctionType,
    :ObjectiveSense,
    :PrimalStatus,
    :RelativeGap,
    :ResultCount,
    :Silent,
    :SimplexIterations,
    :SolverName,
    :SolveTimeSec,
    :TerminationStatus,
    :TimeLimitSec,
)
    f = Symbol("test_attribute_$(attr)")
    unittests["test_attribute_$(attr)"] = @eval begin
        function $(f)(model::MOI.ModelLike, config::Config)
            MOI.empty!(model)
            attribute = MOI.$(attr)()
            if MOI.is_set_by_optimize(attribute)
                if !config.solve
                    return
                end
                MOI.optimize!(model)
            end
            T = MOI.attribute_value_type(attribute)
            try
                MOI.get(model, attribute)
            catch err
                if err isa ArgumentError
                    return  # Solver does not support accessing the attribute.
                end
            end

            @static if VERSION < v"1.5"
                @test MOI.get(model, attribute) isa T
            else
                @test @inferred(T, MOI.get(model, attribute)) isa T
            end
        end
    end
end

# Variable attributes.
for attr in (:VariableName,)
    f = Symbol("test_attribute_$(attr)")
    unittests["test_attribute_$(attr)"] = @eval begin
        function $(f)(model::MOI.ModelLike, config::Config)
            MOI.empty!(model)
            x = MOI.add_variable(model)
            MOI.set(model, MOI.VariableName(), x, "x")
            attribute = MOI.$(attr)()
            if MOI.is_set_by_optimize(attribute)
                if !config.solve
                    return
                end
                MOI.optimize!(model)
            end
            T = MOI.attribute_value_type(attribute)
            @static if VERSION < v"1.5"
                @test MOI.get(model, attribute, x) isa T
            else
                @test @inferred(T, MOI.get(model, attribute, x)) isa T
            end
        end
    end
end

# Constraint attributes.
for attr in (
    :CanonicalConstraintFunction,
    :ConstraintFunction,
    :ConstraintName,
    :ConstraintSet,
)
    f = Symbol("test_attribute_$(attr)")
    unittests["test_attribute_$(attr)"] = @eval begin
        function $(f)(model::MOI.ModelLike, config::Config{T}) where {T}
            MOI.empty!(model)
            x = MOI.add_variable(model)
            ci =
                if MOI.supports_constraint(
                    model,
                    MOI.ScalarAffineFunction{T},
                    MOI.GreaterThan{T},
                )
                    MOI.add_constraint(
                        model,
                        MOI.ScalarAffineFunction(
                            [MOI.ScalarAffineTerm(one(T), x)],
                            zero(T),
                        ),
                        MOI.GreaterThan(zero(T)),
                    )
                elseif MOI.supports_constraint(
                    model,
                    MOI.VectorOfVariables,
                    MOI.Nonnegatives,
                )
                    MOI.add_constraint(
                        model,
                        MOI.VectorOfVariables([x]),
                        MOI.Nonnegatives(1),
                    )
                else
                    return
                end
            attribute = MOI.$(attr)()
            if MOI.is_set_by_optimize(attribute)
                if !config.solve
                    return
                end
                MOI.optimize!(model)
            end
            VT = MOI.attribute_value_type(attribute)
            @static if VERSION < v"1.5"
                @test MOI.get(model, attribute, ci) isa VT
            else
                @test @inferred(T, MOI.get(model, attribute, ci)) isa VT
            end
        end
    end
end

# TODO(odow): attributes not tested:
# CallbackNodeStatus()
# LazyConstraintCallback()
# NumberOfConstraints{F,S}()
# ObjectiveFunction{F}()
# ConstraintBasisStatus()
# ConstraintConflictStatus()
# VariableBridgingCost()
# ConstraintBridgingCost()
