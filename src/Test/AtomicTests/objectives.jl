#=
    Functions in this file test functionality relating to objectives in MOI.

### Requires
    - optimize!

### Functionality currently tested
    - get/set ObjectiveSense
    - a constant in a affine objective
    - a blank objective

### Functionality not yet tested
    - Quadratic Objectives
    - Modifications
=#

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
atomictests["max_sense"] = max_sense

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
atomictests["min_sense"] = min_sense

"""
    Set objective to FeasibilitySense
"""
function feasibility_sense(model::MOI.ModelLike, config::TestConfig)
    MOI.empty!(model)
    @test MOI.isempty(model)
    @test MOI.canset(model, MOI.ObjectiveSense())
    MOI.set!(model, MOI.ObjectiveSense(), MOI.FeasibilitySense)
    @test MOI.canget(model, MOI.ObjectiveSense())
    @test MOI.get(model, MOI.ObjectiveSense()) == MOI.FeasibilitySense
end
atomictests["feasibility_sense"] = feasibility_sense

"""
    Test constant in objective.
"""
function constant_obj(model::MOI.ModelLike, config::TestConfig)
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
atomictests["constant_obj"] = constant_obj

"""
    Test blank objective.
"""
function blank_obj(model::MOI.ModelLike, config::TestConfig)
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
atomictests["blank_obj"] = blank_obj

"""
    Test SingleVariable objective.
"""
function singlevariable_obj(model::MOI.ModelLike, config::TestConfig)
    atol, rtol = config.atol, config.rtol
    MOI.empty!(model)
    @test MOI.isempty(model)
    MOIU.loadfromstring!(model,"""
        variables: x
        minobjective: x
        c: x >= 1.0
    """)
    MOI.optimize!(model)
    @test MOI.get(model, MOI.ObjectiveValue()) ≈ 1.0 atol=atol rtol=rtol
end
atomictests["singlevariable_obj"] = singlevariable_obj
