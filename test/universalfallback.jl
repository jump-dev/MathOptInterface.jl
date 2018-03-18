function test_optmodattrs(uf, model, attr)
    @test !MOI.canset(model, attr)
    @test MOI.canset(uf, attr)
    @test !MOI.canget(model, attr)
    @test !MOI.canget(uf, attr)
    MOI.set!(uf, attr, 0)
    @test !MOI.canget(model, attr)
    @test MOI.canget(uf, attr)
    @test MOI.get(uf, attr) == 0
    @test !MOI.isempty(uf)
    MOI.empty!(uf)
    @test MOI.isempty(uf)
end
function test_varconattrs(uf, model, attr, I::Type{<:MOI.Index}, addfun, x, y, z)
    @test !MOI.canset(model, attr, I)
    @test MOI.canset(uf, attr, I)
    @test !MOI.canget(model, attr, I)
    @test !MOI.canget(uf, attr, I)
    MOI.set!(uf, attr, [x, y], [2, 0])
    @test !MOI.isempty(uf)
    @test !MOI.canget(model, attr, I)
    @test !MOI.canget(uf, attr, I)
    MOI.set!(uf, attr, z, 5)
    @test !MOI.canget(model, attr, I)
    @test MOI.canget(uf, attr, I)
    @test MOI.get(uf, attr, y) == 0
    @test MOI.get(uf, attr, [z, x]) == [5, 2]

    u = addfun(uf)
    @test !MOI.canget(model, attr, I)
    @test !MOI.canget(uf, attr, I)
    MOI.set!(uf, attr, u, 8)
    @test !MOI.canget(model, attr, I)
    @test MOI.canget(uf, attr, I)

    w = addfun(uf)
    @test !MOI.canget(model, attr, I)
    @test !MOI.canget(uf, attr, I)
    MOI.delete!(uf, u)
    @test !MOI.canget(model, attr, I)
    @test !MOI.canget(uf, attr, I)

    MOI.set!(uf, attr, [w, z], [9, 4])
    @test !MOI.canget(model, attr, I)
    @test MOI.canget(uf, attr, I)
    @test MOI.get(uf, attr, w) == 9
    @test MOI.get(uf, attr, x) == 2
    @test MOI.get(uf, attr, z) == 4
    @test MOI.get(uf, attr, y) == 0
end

struct BadOptimizerAttribute <: MOI.AbstractOptimizerAttribute end

@testset "UniversalFallback" begin
    model = Model{Float64}()
    uf = MOIU.UniversalFallback(model)
    @test MOI.isempty(uf)
    @testset "Copy Test" begin
        MOIT.copytest(uf, Model{Float64}())
        @test !MOI.isempty(uf)
        MOI.empty!(uf)
        @test MOI.isempty(uf)
    end
    @testset "Optimizer Attribute" begin
        attr = BadOptimizerAttribute()
        test_optmodattrs(uf, model, attr)
    end
    @testset "Model Attribute" begin
        attr = MOIT.BadModelAttribute()
        test_optmodattrs(uf, model, attr)
    end
    x = MOI.addvariable!(model)
    y, z = MOI.addvariables!(model, 2)
    @testset "Variable Attribute" begin
        VI = MOI.VariableIndex
        attr = MOIT.BadVariableAttribute()
        test_varconattrs(uf, model, attr, VI, MOI.addvariable!, x, y, z)
    end
    cx = MOI.addconstraint!(uf, x, MOI.EqualTo(0.))
    cy = MOI.addconstraint!(uf, y, MOI.EqualTo(1.))
    cz = MOI.addconstraint!(uf, z, MOI.EqualTo(2.))
    @testset "Constraint Attribute" begin
        CI = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}
        attr = MOIT.BadConstraintAttribute()
        test_varconattrs(uf, model, attr, CI, uf -> MOI.addconstraint!(uf, x, MOI.EqualTo(0.)), cx, cy, cz)
    end
end
