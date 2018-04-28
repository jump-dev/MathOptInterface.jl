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
    Test constant in objective.
"""
function constantobj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 2.0x + 1.0
        c: x >= 1.0
    """)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) ≈ 3.0 atol=atol rtol=rtol
end

"""
    Test blank objective.
"""
function blankobj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 0.0x + 0.0
        c: x >= 1.0
    """)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) ≈ 0.0 atol=atol rtol=rtol
end

"""
    Test the setting of an upper bound
"""
function upperbound(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        maxobjective: 2.0x
        c1: x <= 1.0
        c2: x >= 0.0
    """)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
    v = MOI.get(model, MOI.VariableIndex, "x")
    @test MOI.get(model, MOI.VariablePrimal(), v) ≈ 1 atol=atol rtol=rtol
    if config.duals
        @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        c1 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}, "c1")
        @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ -2.0 atol=atol rtol=rtol
        c2 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}, "c2")
        @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0.0 atol=atol rtol=rtol
    end
end

"""
    Test the setting of an lower bound
"""
function lowerbound(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 2.0x
        c1: x >= 1.0
        c2: x <= 2.0
    """)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.FeasiblePoint
    v = MOI.get(model, MOI.VariableIndex, "x")
    @test MOI.get(model, MOI.VariablePrimal(), v) ≈ 1 atol=atol rtol=rtol
    if config.duals
        @test MOI.get(model, MOI.DualStatus()) == MOI.FeasiblePoint
        c1 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable,MOI.GreaterThan{Float64}}, "c1")
        @test MOI.get(model, MOI.ConstraintDual(), c1) ≈ 2.0 atol=atol rtol=rtol
        c2 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}, "c2")
        @test MOI.get(model, MOI.ConstraintDual(), c2) ≈ 0.0 atol=atol rtol=rtol
    end
end

"""
    Test getting variables by name.
"""
function getvariable(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 2.0x
        c1: x >= 1.0
        c2: x <= 2.0
    """)
    @test MOI.canget(model, MOI.VariableIndex, "x")
    @test !MOI.canget(model, MOI.VariableIndex, "y")
    x = MOI.get(model, MOI.VariableIndex, "x")
    @test MOI.isvalid(model, x)
end

"""
    Test getting constraints by name.
"""
function getconstraint(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: 2.0x
        c1: x >= 1.0
        c2: x <= 2.0
    """)
    @test !MOI.canget(model, MOI.ConstraintIndex, "c3")
    @test MOI.canget(model, MOI.ConstraintIndex, "c1")
    @test MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c1")
    @test !MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c1")
    @test MOI.canget(model, MOI.ConstraintIndex, "c2")
    @test !MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c2")
    @test MOI.canget(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c2")
    c1 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.GreaterThan{Float64}}, "c1")
    @test MOI.isvalid(model, c1)
    c2 = MOI.get(model, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "c2")
    @test MOI.isvalid(model, c2)
end

const atomictests = Dict(
    "add_variable"     => add_variable,
    "add_variables"    => add_variables,
    "delete_variable"  => delete_variable,
    "delete_variables" => delete_variables,
    "min_sense"        => min_sense,
    "max_sense"        => max_sense,
    "upperbound"       => upperbound,
    "lowerbound"       => lowerbound,
    "getvariable"      => getvariable,
    "getconstraint"    => getconstraint,
    "constantobj"      => constantobj,
    "blankobj"         => blankobj
)
@moitestset atomic
