using Test
import MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIU = MOI.Utilities

function test_optmodattrs(uf, model, attr, listattr)
    @test !MOI.supports(model, attr)
    @test MOI.supports(uf, attr)
    @test isempty(MOI.get(uf, listattr))
    MOI.set(uf, attr, 0)
    @test MOI.get(uf, attr) == 0
    @test MOI.get(uf, listattr) == [attr]
end
function test_varconattrs(uf, model, attr, listattr, I::Type{<:MOI.Index},
                          addfun, x, y, z)
    @test !MOI.supports(model, attr, I)
    @test MOI.supports(uf, attr, I)
    @test isempty(MOI.get(uf, listattr))
    MOI.set(uf, attr, [x, y], [2, 0])
    @test MOI.get(uf, attr, z) === nothing
    @test !MOI.is_empty(uf)
    @test MOI.get(uf, listattr) == [attr]
    MOI.set(uf, attr, z, 5)
    @test MOI.get(uf, attr, y) == 0
    @test MOI.get(uf, attr, [z, x]) == [5, 2]
    @test MOI.get(uf, listattr) == [attr]

    u = addfun(uf)
    @test MOI.get(uf, attr, u) === nothing
    @test MOI.get(uf, listattr) == [attr]
    MOI.set(uf, attr, u, 8)
    @test MOI.get(uf, listattr) == [attr]

    w = addfun(uf)
    @test MOI.get(uf, listattr) == [attr]
    @test MOI.get(uf, attr, w) === nothing

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
@MOIU.model(ModelForUniversalFallback,
            (),
            (MOI.LessThan,),
            (),
            (),
            (),
            (MOI.ScalarAffineFunction,),
            (),
            ())
function MOI.supports_constraint(
    ::ModelForUniversalFallback{T}, ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.EqualTo{T}, MOI.GreaterThan{T}, MOI.Interval{T},
                   MOI.Integer, MOI.ZeroOne}}) where T
    return false
end

# TODO: Restructure to avoid sharing state across testsets. It doesn't seem
# necessary to use the same uf object for all the tests.
model = ModelForUniversalFallback{Float64}()
uf = MOIU.UniversalFallback(model)
@test MOI.is_empty(uf)
@testset "Copy Test" begin
    MOIT.copytest(uf, MOIU.Model{Float64}())
    @test !MOI.is_empty(uf)
    MOI.empty!(uf)
    @test MOI.is_empty(uf)
end
@testset "Start Values Test" begin
    src = MOIU.UniversalFallback(MOIU.Model{Float64}())
    dest = MOIU.UniversalFallback(MOIU.Model{Float64}())
    MOIT.start_values_test(dest, src)
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
    @test !MOI.is_empty(uf)
    MOI.empty!(uf)
    @test MOI.is_empty(uf)
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
    global x, y, z
    _affine(vi) = convert(MOI.ScalarAffineFunction{Float64}, MOI.SingleVariable(vi))
    function _add_affine_constraint(vi, ub)
        return MOI.add_constraint(uf, _affine(vi), MOI.LessThan(ub))
    end
    attr = MOIT.UnknownConstraintAttribute()
    @testset "Supported constraint" begin
        cx = _add_affine_constraint(x, 0.)
        cy = _add_affine_constraint(y, 1.)
        cz = _add_affine_constraint(z, 2.)
        CI = MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}
        listattr = MOI.ListOfConstraintAttributesSet{MOI.SingleVariable, MOI.LessThan{Float64}}()
        test_varconattrs(uf, model, attr, listattr, CI,
                         uf -> _add_affine_constraint(x, 0.), cx, cy, cz)

        MOI.set(uf, MOI.ConstraintFunction(), cx, _affine(y))
        @test MOI.get(uf, MOI.ConstraintFunction(), cx) ≈ _affine(y)

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
        cx = _add_affine_constraint(x, 0.)
        cy = _add_affine_constraint(y, 1.)
        cz = _add_affine_constraint(z, 2.)
        CI = MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}
        listattr = MOI.ListOfConstraintAttributesSet{MOI.SingleVariable, MOI.EqualTo{Float64}}()
        test_varconattrs(uf, model, attr, listattr, CI,
                         uf -> _add_affine_constraint(x, 0.), cx, cy, cz)

        MOI.set(uf, MOI.ConstraintFunction(), cx, _affine(y))
        @test MOI.get(uf, MOI.ConstraintFunction(), cx) ≈ _affine(y)

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

@testset "Duplicate names" begin
    model = ModelForUniversalFallback{Float64}()
    uf = MOIU.UniversalFallback(model)
    x = MOI.add_variable(uf)
    # These are both intercepted by the fallback.
    _affine(vi) = convert(MOI.ScalarAffineFunction{Float64}, MOI.SingleVariable(vi))
    x_eq = MOI.add_constraint(uf, _affine(x), MOI.EqualTo(1.0))
    x_gt = MOI.add_constraint(uf, _affine(x), MOI.GreaterThan(1.0))
    MOI.set(uf, MOI.ConstraintName(), x_eq, "a name")
    MOI.set(uf, MOI.ConstraintName(), x_gt, "a name")
    @test_throws Exception MOI.get(uf,
                                   MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},
                                                       MOI.EqualTo{Float64}},
                                   "a name")
    @test_throws Exception MOI.get(uf, MOI.ConstraintIndex, "a name")
end
