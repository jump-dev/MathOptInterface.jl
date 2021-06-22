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
function test_varconattrs(
    uf,
    model,
    attr,
    listattr,
    I::Type{<:MOI.Index},
    addfun,
    x,
    y,
    z,
)
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

# A few objective/constraint types are supported to test both the fallback and the
# delegation to the internal model
MOIU.@model(
    ModelForUniversalFallback,
    (),
    (MOI.LessThan,),
    (),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (),
    ()
)
function MOI.supports(
    ::ModelForUniversalFallback{T},
    ::Type{MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}},
) where {T}
    return false
end
function MOI.supports_constraint(
    ::ModelForUniversalFallback{T},
    ::Type{MOI.SingleVariable},
    ::Type{
        <:Union{
            MOI.EqualTo{T},
            MOI.GreaterThan{T},
            MOI.Interval{T},
            MOI.Integer,
            MOI.ZeroOne,
        },
    },
) where {T}
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
    empty!(uf.optattr)
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
@testset "Objective Attribute" begin
    global x, y, z
    _single(vi) = MOI.SingleVariable(vi)
    function _affine(vi)
        return convert(
            MOI.ScalarAffineFunction{Float64},
            MOI.SingleVariable(vi),
        )
    end
    function _add_single_objective(vi)
        return MOI.set(
            uf,
            MOI.ObjectiveFunction{MOI.SingleVariable}(),
            _single(vi),
        )
    end
    function _add_affine_objective(vi)
        return MOI.set(
            uf,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            _affine(vi),
        )
    end
    @testset "Supported objective" begin
        F = MOI.SingleVariable
        @test MOI.supports(model, MOI.ObjectiveFunction{F}())
        _add_single_objective(x)
        @test MOI.get(uf, MOI.ObjectiveFunction{F}()) ≈ _single(x)
        MOI.set(uf, MOI.ObjectiveFunction{F}(), _single(y))
        @test MOI.get(uf, MOI.ObjectiveFunction{F}()) ≈ _single(y)
    end
    @testset "Unsupported objective" begin
        F = MOI.ScalarAffineFunction{Float64}
        @test !MOI.supports(model, MOI.ObjectiveFunction{F})
        @test MOI.supports(uf, MOI.ObjectiveFunction{F}())
        _add_affine_objective(x)
        @test MOI.get(uf, MOI.ObjectiveFunction{F}()) ≈ _affine(x)
        MOI.modify(
            uf,
            MOI.ObjectiveFunction{F}(),
            MOI.ScalarCoefficientChange(y, 1.0),
        )
        fx = MOI.SingleVariable(x)
        fy = MOI.SingleVariable(y)
        obj = 1fx + 1fy
        @test MOI.get(uf, MOI.ObjectiveFunction{F}()) ≈ obj
    end
end
@testset "Constraint Attribute" begin
    global x, y, z
    function _affine(vi)
        return convert(
            MOI.ScalarAffineFunction{Float64},
            MOI.SingleVariable(vi),
        )
    end
    function _add_affine_constraint(vi, ub)
        return MOI.add_constraint(uf, _affine(vi), MOI.LessThan(ub))
    end
    attr = MOIT.UnknownConstraintAttribute()
    @testset "Supported constraint" begin
        cx = _add_affine_constraint(x, 0.0)
        cy = _add_affine_constraint(y, 1.0)
        cz = _add_affine_constraint(z, 2.0)
        CI = MOI.ConstraintIndex{MOI.SingleVariable,MOI.LessThan{Float64}}
        listattr = MOI.ListOfConstraintAttributesSet{
            MOI.SingleVariable,
            MOI.LessThan{Float64},
        }()
        test_varconattrs(
            uf,
            model,
            attr,
            listattr,
            CI,
            uf -> _add_affine_constraint(x, 0.0),
            cx,
            cy,
            cz,
        )

        MOI.set(uf, MOI.ConstraintFunction(), cx, _affine(y))
        @test MOI.get(uf, MOI.ConstraintFunction(), cx) ≈ _affine(y)
        @test MOI.get(uf, MOI.CanonicalConstraintFunction(), cx) ≈ _affine(y)
        @test MOIU.is_canonical(
            MOI.get(uf, MOI.CanonicalConstraintFunction(), cx),
        )

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
        cx = _add_affine_constraint(x, 0.0)
        cy = _add_affine_constraint(y, 1.0)
        cz = _add_affine_constraint(z, 2.0)
        CI = MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}
        listattr = MOI.ListOfConstraintAttributesSet{
            MOI.SingleVariable,
            MOI.EqualTo{Float64},
        }()
        test_varconattrs(
            uf,
            model,
            attr,
            listattr,
            CI,
            uf -> _add_affine_constraint(x, 0.0),
            cx,
            cy,
            cz,
        )

        MOI.set(uf, MOI.ConstraintFunction(), cx, _affine(y))
        @test MOI.get(uf, MOI.ConstraintFunction(), cx) ≈ _affine(y)
        @test MOI.get(uf, MOI.CanonicalConstraintFunction(), cx) ≈ _affine(y)
        @test MOIU.is_canonical(
            MOI.get(uf, MOI.CanonicalConstraintFunction(), cx),
        )

        @test MOI.supports(uf, MOI.ConstraintName(), typeof(cx))
        MOI.set(uf, MOI.ConstraintName(), cx, "EqualTo")
        @test MOI.get(uf, MOI.ConstraintName(), cx) == "EqualTo"
        @test MOI.get(uf, typeof(cx), "EqualTo") == cx
        MOI.delete(uf, cx)
        @test MOI.get(uf, typeof(cx), "EqualTo") === nothing
    end
end
config = MOIT.Config(solve = false)
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
    MOI.Test.runtests(uf, config, include = ["test_linear_"])
end

@testset "Duplicate names" begin
    model = ModelForUniversalFallback{Float64}()
    uf = MOIU.UniversalFallback(model)
    x = MOI.add_variable(uf)
    # These are both intercepted by the fallback.
    function _affine(vi)
        return convert(
            MOI.ScalarAffineFunction{Float64},
            MOI.SingleVariable(vi),
        )
    end
    x_eq = MOI.add_constraint(uf, _affine(x), MOI.EqualTo(1.0))
    x_gt = MOI.add_constraint(uf, _affine(x), MOI.GreaterThan(1.0))
    MOI.set(uf, MOI.ConstraintName(), x_eq, "a name")
    MOI.set(uf, MOI.ConstraintName(), x_gt, "a name")
    @test_throws Exception MOI.get(
        uf,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        },
        "a name",
    )
    @test_throws Exception MOI.get(uf, MOI.ConstraintIndex, "a name")
end

@testset "Deterministic constraint ordering" begin
    F = MOI.ScalarAffineFunction{Float64}
    _affine(vi) = convert(F, MOI.SingleVariable(vi))
    set1 = MOI.EqualTo(1.0)
    set2 = MOI.GreaterThan(1.0)
    for sets in [[set1, set2], [set2, set1]]
        model = ModelForUniversalFallback{Float64}()
        uf = MOIU.UniversalFallback(model)
        x = MOI.add_variable(uf)
        y = MOI.add_variable(uf)
        cx1 = MOI.add_constraint(uf, _affine(x), sets[1])
        cx2 = MOI.add_constraint(uf, _affine(x), sets[2])
        cy1 = MOI.add_constraint(uf, _affine(y), sets[1])
        cy2 = MOI.add_constraint(uf, _affine(y), sets[2])
        # check that the constraint types are in the order they were added in
        @test MOI.get(uf, MOI.ListOfConstraintTypesPresent()) ==
              [(F, typeof(sets[1])), (F, typeof(sets[2]))]
        # check that the constraints given the constraint type are in the order they were added in
        for set in sets
            @test MOI.get(uf, MOI.ListOfConstraintIndices{F,typeof(set)}()) == [
                MOI.ConstraintIndex{F,typeof(set)}(1),
                MOI.ConstraintIndex{F,typeof(set)}(2),
            ]
        end
    end
end

@testset "Delete" begin
    model = ModelForUniversalFallback{Float64}()
    uf = MOIU.UniversalFallback(model)
    MOIT.delete_test(uf)
end

@testset "Show" begin
    model = ModelForUniversalFallback{Float64}()
    uf = MOIU.UniversalFallback(model)
    @test sprint(show, uf) == MOI.Utilities.replace_acronym("""
    $(MOIU.UniversalFallback{ModelForUniversalFallback{Float64}})
    fallback for $(ModelForUniversalFallback{Float64})""")
end
