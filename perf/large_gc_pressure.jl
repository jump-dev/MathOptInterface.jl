using Revise
using ProfileView
using BenchmarkTools
using MathOptInterface
const MOI = MathOptInterface
using GLPK

function create_model(I, T = 10000)
    model = MOI.Utilities.CachingOptimizer(
        MOI.Utilities.Model{Float64}(),
        GLPK.Optimizer(),
    )
    v = MOI.VariableIndex[]
    for _ in 1:I
        x = MOI.add_variables(model, T)
        MOI.add_constraint.(model, x, MOI.GreaterThan(0.0))
        MOI.add_constraint.(model, x, MOI.LessThan(100.0))
        for t in 2:T
            f = MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([1.0, -1.0], [x[t], x[t-1]]),
                0.0,
            )
            MOI.add_constraint(model, f, MOI.GreaterThan(-10.0))
            MOI.add_constraint(model, f, MOI.LessThan(10.0))
        end
        append!(v, x)
    end
    g = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, v), 0.0)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(model, MOI.ObjectiveFunction{typeof(g)}(), g)
    MOI.Utilities.attach_optimizer(model)
    return model
end

using Revise
using JuMP, BenchmarkTools, GLPK

function create_model(I, T = 10000)
    model = Model(GLPK.Optimizer)
    @variable(model, 0 <= x[1:I, 1:T] <= 100)
    @constraint(model, [i in 1:I, t in 2:T], x[i, t] - x[i, t-1] <= 10)
    @constraint(model, [i in 1:I, t in 2:T], x[i, t] - x[i, t-1] >= -10)
    @objective(model, Min, sum(x))
    return model
end
@benchmark create_model(100, 1000)

function create_model_2(I, T = 10000)
    model = Model(GLPK.Optimizer)
    x = @variable(model, [1:I, 1:T], lower_bound = 0, upper_bound = 100)
    @constraint(model, [i in 1:I, t in 2:T], x[i, t] - x[i, t-1] <= 10)
    @constraint(model, [i in 1:I, t in 2:T], x[i, t] - x[i, t-1] >= -10)
    @objective(model, Min, sum(x))
    return model
end
@benchmark create_model_2(100, 1000)


using ProfileView
@profview create_model(100, 1000)


using Revise
using JuMP, BenchmarkTools, GLPK

function variables(I, T)
    model = Model()
    @variable(model, [1:I, 1:T], lower_bound = 0, upper_bound = 100)
    return model
end

@benchmark variables(1000, 10000)

function variables_with_names(I, T)
    model = Model()
    @variable(model, 0 <= x[1:I, 1:T] <= 100)
    return model
end

@profview variables_with_names(100, 1000)


function test_names(N)
    x = Dict{Int,String}()
    for i in 1:N
        x[i] = string("x[", i, "]")
    end
    return x
end

function test_names_jump(N)
    model = Model()
    @variable(model, x[1:N])
    return model
end

