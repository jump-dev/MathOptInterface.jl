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
unittests["getconstraint"]    = getconstraint
