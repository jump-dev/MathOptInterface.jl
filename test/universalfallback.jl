function test_optmodattrs(uf, model, attr, listattr)
    @test !MOI.supports(model, attr)
    @test MOI.supports(uf, attr)
    @test isempty(MOI.get(uf, listattr))
    MOI.set(uf, attr, 0)
    @test MOI.get(uf, attr) == 0
    @test MOI.get(uf, listattr) == [attr]
    @test !MOI.is_empty(uf)
    MOI.empty!(uf)
    @test MOI.is_empty(uf)
end
function test_varconattrs(uf, model, attr, listattr, I::Type{<:MOI.Index}, addfun, x, y, z)
    @test !MOI.supports(model, attr, I)
    @test MOI.supports(uf, attr, I)
    @test isempty(MOI.get(uf, listattr))
    MOI.set(uf, attr, [x, y], [2, 0])
    @test !MOI.is_empty(uf)
    @test MOI.get(uf, listattr) == [attr]
    MOI.set(uf, attr, z, 5)
    @test MOI.get(uf, attr, y) == 0
    @test MOI.get(uf, attr, [z, x]) == [5, 2]
    @test MOI.get(uf, listattr) == [attr]

    u = addfun(uf)
    @test MOI.get(uf, listattr) == [attr]
    MOI.set(uf, attr, u, 8)
    @test MOI.get(uf, listattr) == [attr]

    w = addfun(uf)
    @test MOI.get(uf, listattr) == [attr]

    @test MOI.is_valid(uf, u)
    MOI.delete(uf, u)
    @test !MOI.is_valid(uf, u)
    @test_throws MOI.InvalidIndex{typeof(u)} MOI.delete(uf, u)
    @test MOI.get(uf, listattr) == [attr]

    MOI.set(uf, attr, [w, z], [9, 4])
    @test MOI.get(uf, listattr) == [attr]
    @test MOI.get(uf, attr, w) == 9
    @test MOI.get(uf, attr, x) == 2
    @test MOI.get(uf, attr, z) == 4
    @test MOI.get(uf, attr, y) == 0
end

struct UnknownOptimizerAttribute <: MOI.AbstractOptimizerAttribute end

# A few constraint types are supported to test both the fallback and the
# delegation to the internal model
@MOIU.model ModelForUniversalFallback () (MOI.LessThan,) () () (MOI.SingleVariable,) (MOI.ScalarAffineFunction,) () ()

@testset "UniversalFallback" begin
    model = ModelForUniversalFallback{Float64}()
    uf = MOIU.UniversalFallback(model)
    @test MOI.is_empty(uf)
    @testset "Copy Test" begin
        MOIT.copytest(uf, Model{Float64}())
        @test !MOI.is_empty(uf)
        MOI.empty!(uf)
        @test MOI.is_empty(uf)
    end
    @testset "Valid Test" begin
        MOIT.validtest(uf)
        @test !MOI.is_empty(uf)
        MOI.empty!(uf)
        @test MOI.is_empty(uf)
    end
    @testset "Empty Test" begin
        MOIT.emptytest(uf)
        @test MOI.is_empty(uf)
    end
    @testset "Name Test" begin
        MOIT.nametest(uf)
        @test !MOI.is_empty(uf)
        MOI.empty!(uf)
        @test MOI.is_empty(uf)
    end
    @testset "Optimizer Attribute" begin
        attr = UnknownOptimizerAttribute()
        listattr = MOI.ListOfOptimizerAttributesSet()
        test_optmodattrs(uf, model, attr, listattr)
    end
    @testset "Model Attribute" begin
        attr = MOIT.UnknownModelAttribute()
        listattr = MOI.ListOfModelAttributesSet()
        test_optmodattrs(uf, model, attr, listattr)
    end
    x = MOI.add_variable(uf)
    y, z = MOI.add_variables(uf, 2)
    @testset "Variable Attribute" begin
        VI = MOI.VariableIndex
        attr = MOIT.UnknownVariableAttribute()
        listattr = MOI.ListOfVariableAttributesSet()
        test_varconattrs(uf, model, attr, listattr, VI, MOI.add_variable, x, y, z)
    end
    @testset "Constraint Attribute" begin
        attr = MOIT.UnknownConstraintAttribute()
        @testset "Supported constraint" begin
            cx = MOI.add_constraint(uf, x, MOI.LessThan(0.))
            cy = MOI.add_constraint(uf, y, MOI.LessThan(1.))
            cz = MOI.add_constraint(uf, z, MOI.LessThan(2.))
            CI = MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}
            listattr = MOI.ListOfConstraintAttributesSet{MOI.SingleVariable, MOI.LessThan{Float64}}()
            test_varconattrs(uf, model, attr, listattr, CI, uf -> MOI.add_constraint(uf, x, MOI.LessThan(0.)), cx, cy, cz)

            @test MOI.supports(uf, MOI.ConstraintFunction(), typeof(cx))
            MOI.set(uf, MOI.ConstraintFunction(), cx, MOI.SingleVariable(y))
            @test MOI.get(uf, MOI.ConstraintFunction(), cx) == MOI.SingleVariable(y)

            @test MOI.supports(uf, MOI.ConstraintName(), typeof(cx))
            MOI.set(uf, MOI.ConstraintName(), cx, "LessThan")
            @test MOI.get(uf, MOI.ConstraintName(), cx) == "LessThan"
            @test MOI.get(uf, typeof(cx), "LessThan") == cx
            MOI.delete(uf, cx)
            @test MOI.get(uf, typeof(cx), "LessThan") === nothing
        end
        # To remove the constraint attributes added in the previous testset
        MOI.empty!(uf)
        x = MOI.add_variable(uf)
        y, z = MOI.add_variables(uf, 2)
        @testset "Unsupported constraint" begin
            cx = MOI.add_constraint(uf, x, MOI.EqualTo(0.))
            cy = MOI.add_constraint(uf, y, MOI.EqualTo(1.))
            cz = MOI.add_constraint(uf, z, MOI.EqualTo(2.))
            CI = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}
            listattr = MOI.ListOfConstraintAttributesSet{MOI.SingleVariable, MOI.EqualTo{Float64}}()
            test_varconattrs(uf, model, attr, listattr, CI, uf -> MOI.add_constraint(uf, x, MOI.EqualTo(0.)), cx, cy, cz)

            @test MOI.supports(uf, MOI.ConstraintFunction(), typeof(cx))
            MOI.set(uf, MOI.ConstraintFunction(), cx, MOI.SingleVariable(y))
            @test MOI.get(uf, MOI.ConstraintFunction(), cx) == MOI.SingleVariable(y)

            @test MOI.supports(uf, MOI.ConstraintName(), typeof(cx))
            MOI.set(uf, MOI.ConstraintName(), cx, "EqualTo")
            @test MOI.get(uf, MOI.ConstraintName(), cx) == "EqualTo"
            @test MOI.get(uf, typeof(cx), "EqualTo") == cx
            MOI.delete(uf, cx)
            @test MOI.get(uf, typeof(cx), "EqualTo") === nothing
        end
    end
    config = MOIT.TestConfig(solve=false)
    @testset "empty" begin
        MOI.empty!(uf)
        @test MOI.is_empty(uf)
    end
    @testset "Unit" begin
        MOIT.unittest(uf, config)
    end
    @testset "Modification" begin
        MOIT.modificationtest(uf, config)
    end
    @testset "Continuous Linear" begin
        MOIT.contlineartest(uf, config)
    end
end
