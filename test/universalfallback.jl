@testset "UniversalFallback" begin
    model = Model{Float64}()
    uf = MOIU.UniversalFallback(model)
    @testset "Model Attribute" begin
        attr = MOIT.BadModelAttribute()
        @test !MOI.canset(model, attr)
        @test MOI.canset(uf, attr)
        @test !MOI.canget(model, attr)
        @test !MOI.canget(uf, attr)
        MOI.set!(uf, attr, 0)
        @test !MOI.canget(model, attr)
        @test MOI.canget(uf, attr)
        @test MOI.get(uf, attr) == 0
    end
    x = MOI.addvariable!(model)
    y, z = MOI.addvariables!(model, 2)
    @testset "Variable Attribute" begin
        VI = MOI.VariableIndex
        attr = MOIT.BadVariableAttribute()
        @test !MOI.canset(model, attr, VI)
        @test MOI.canset(uf, attr, VI)
        @test !MOI.canget(model, attr, VI)
        @test !MOI.canget(uf, attr, VI)
        MOI.set!(uf, attr, [x, y], [2, 0])
        @test !MOI.canget(model, attr, VI)
        @test !MOI.canget(uf, attr, VI)
        MOI.set!(uf, attr, z, 5)
        @test !MOI.canget(model, attr, VI)
        @test MOI.canget(uf, attr, VI)
        @test MOI.get(uf, attr, y) == 0
        @test MOI.get(uf, attr, [z, x]) == [5, 2]
    end
end
