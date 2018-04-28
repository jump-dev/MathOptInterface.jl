"""
    This function tests adding a single variable.
"""
function add_variable(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canaddvariable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.addvariable!(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
end

"""
    This function tests adding multiple variables.
"""
function add_variables(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canaddvariable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.addvariables!(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
end

"""
    This function tests adding, and then deleting,
    a single variable.
"""
function delete_variable(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canaddvariable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.addvariable!(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test MOI.candelete(model, v)
    MOI.delete!(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
end

"""
    This function tests adding, and then deleting,
    multiple variables.
"""
function delete_variables(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canaddvariable(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.addvariables!(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    @test MOI.candelete(model, v)
    MOI.delete!(model, v)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    v = MOI.addvariables!(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2
    @test MOI.candelete(model, v[1])
    MOI.delete!(model, v[1])
    @test MOI.get(model, MOI.NumberOfVariables()) == 1
    @test !MOI.candelete(model, v[1])
    @test MOI.candelete(model, v[2])
    @test !MOI.isvalid(model, v[1])
    @test MOI.isvalid(model, v[2])
end

"""
    Set objective to MaxSense
"""
function max_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(model, MOI.ObjectiveSense())
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MaxSense
end

"""
    Set objective to MinSense
"""
function min_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)
    @test MOI.canget(model, MOI.ObjectiveSense())
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.MinSense
end

"""
    Test the setting of an upper bound
"""
function upperbound(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.supportsconstraint(model,
        MOI.SingleVariable,
        MOI.LessThan{Float64}
    )
    v = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.LessThan{Float64})
    c = MOI.addconstraint!(model,
        MOI.SingleVariable(v),
        MOI.LessThan(1.0)
    )
    @test MOI.get(model,
        MOI.NumberOfConstraints{MOI.SingleVariable,MOI.LessThan{Float64}}()
    ) == 1

    @test MOI.canset(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    MOI.set!(model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction([v], [1.0], 0.0)
    )
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MaxSense)

    if config.solve

    end
end

const atomictests = Dict(
    "add_variable"     => add_variable,
    "add_variables"    => add_variables,
    "delete_variable"  => delete_variable,
    "delete_variables" => delete_variables,
    "min_sense"        => min_sense,
    "max_sense"        => max_sense,
    "upperbound"       => upperbound
)
@moitestset atomic
