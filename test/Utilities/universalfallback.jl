module TestUniversalFallback

using Test

import MathOptInterface

const MOI = MathOptInterface

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

###
### Utilities for running the tests
###

struct UnknownOptimizerAttribute <: MOI.AbstractOptimizerAttribute end

# A few objective/constraint types are supported to test both the fallback and the
# delegation to the internal model
MOI.Utilities.@model(
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
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::ModelForUniversalFallback{T},
    ::Type{MOI.VariableIndex},
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

###
### The tests
###

function test_MOI_Test()
    inner = ModelForUniversalFallback{Float64}()
    model = MOI.Utilities.UniversalFallback(inner)
    MOI.Test.runtests(
        model,
        MOI.Test.Config(exclude = Any[MOI.optimize!]),
        exclude = String[
            # UniversalFallback fails all these tests because it supports
            # everything...
            "test_attribute_",
            "test_model_supports_constraint_",
            "test_model_copy_to_Unsupported",
            # Bugs in UniversalFallback
            "test_model_LowerBoundAlreadySet",
            "test_model_UpperBoundAlreadySet",
        ],
    )
    return
end

function _test_Optimizer_Model_attributes(uf, model, attr, listattr)
    @test !MOI.supports(model, attr)
    @test MOI.supports(uf, attr)
    @test isempty(MOI.get(uf, listattr))
    MOI.set(uf, attr, 0)
    @test MOI.get(uf, attr) == 0
    @test MOI.get(uf, listattr) == [attr]
    return
end

function _test_Variable_Constraint_attributes(
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
    return
end

function test_optimizer_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    _test_Optimizer_Model_attributes(
        uf,
        model,
        UnknownOptimizerAttribute(),
        MOI.ListOfOptimizerAttributesSet(),
    )
    return
end

function test_model_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    _test_Optimizer_Model_attributes(
        uf,
        model,
        MOI.Test.UnknownModelAttribute(),
        MOI.ListOfModelAttributesSet(),
    )
    # Test that emptying the uf get's rid of the model attributes!
    @test !MOI.is_empty(uf)
    MOI.empty!(uf)
    @test MOI.is_empty(uf)
    return
end

function test_variable_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    _test_Variable_Constraint_attributes(
        uf,
        model,
        MOI.Test.UnknownVariableAttribute(),
        MOI.ListOfVariableAttributesSet(),
        MOI.VariableIndex,
        MOI.add_variable,
        MOI.add_variables(uf, 3)...,
    )
    return
end

function test_supported_objective_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    x, y = MOI.add_variables(uf, 2)
    attr = MOI.ObjectiveFunction{MOI.VariableIndex}()
    @test MOI.supports(model, attr)
    MOI.set(uf, attr, x)
    @test MOI.get(uf, attr) ≈ x
    MOI.set(uf, attr, y)
    @test MOI.get(uf, attr) ≈ y
    return
end

function test_unsupported_objective_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    x, y = MOI.add_variables(uf, 2)
    attr = MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    @test !MOI.supports(model, attr)
    @test MOI.supports(uf, attr)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    MOI.set(uf, attr, f)
    @test MOI.get(uf, attr) ≈ f
    MOI.modify(uf, attr, MOI.ScalarCoefficientChange(y, 1.0))
    new_obj = MOI.ScalarAffineFunction(
        [MOI.ScalarAffineTerm(1.0, x), MOI.ScalarAffineTerm(1.0, y)],
        0.0,
    )
    @test MOI.get(uf, attr) ≈ new_obj
    @test attr in MOI.get(uf, MOI.ListOfModelAttributesSet())
    @test MOI.get(uf, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarAffineFunction{Float64}
    return
end

function test_supported_constraint_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    x, y, z = MOI.add_variables(uf, 3)
    function _add_constraint(uf, x::MOI.VariableIndex, ub::Float64)
        return MOI.add_constraint(
            uf,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.LessThan(ub),
        )
    end
    cx = _add_constraint(uf, x, 0.0)
    cy = _add_constraint(uf, y, 1.0)
    cz = _add_constraint(uf, z, 2.0)
    _test_Variable_Constraint_attributes(
        uf,
        model,
        MOI.Test.UnknownConstraintAttribute(),
        MOI.ListOfConstraintAttributesSet{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        },
        uf -> _add_constraint(uf, x, 0.0),
        cx,
        cy,
        cz,
    )
    fy = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, y)], 0.0)
    MOI.set(uf, MOI.ConstraintFunction(), cx, fy)
    @test MOI.get(uf, MOI.ConstraintFunction(), cx) ≈ fy
    @test MOI.get(uf, MOI.CanonicalConstraintFunction(), cx) ≈ fy
    @test MOI.Utilities.is_canonical(
        MOI.get(uf, MOI.CanonicalConstraintFunction(), cx),
    )
    @test MOI.supports(uf, MOI.ConstraintName(), typeof(cx))
    MOI.set(uf, MOI.ConstraintName(), cx, "LessThan")
    @test MOI.get(uf, MOI.ConstraintName(), cx) == "LessThan"
    @test MOI.get(uf, typeof(cx), "LessThan") == cx
    MOI.delete(uf, cx)
    @test MOI.get(uf, typeof(cx), "LessThan") === nothing
    return
end

function test_unsupported_constraint_attributes()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    x, y, z = MOI.add_variables(uf, 3)
    function _add_constraint(uf, x::MOI.VariableIndex, ub::Float64)
        return MOI.add_constraint(
            uf,
            MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
            MOI.EqualTo(ub),
        )
    end
    cx = _add_constraint(uf, x, 0.0)
    cy = _add_constraint(uf, y, 1.0)
    cz = _add_constraint(uf, z, 2.0)
    _test_Variable_Constraint_attributes(
        uf,
        model,
        MOI.Test.UnknownConstraintAttribute(),
        MOI.ListOfConstraintAttributesSet{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(),
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        },
        uf -> _add_constraint(uf, x, 0.0),
        cx,
        cy,
        cz,
    )
    fy = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, y)], 0.0)
    MOI.set(uf, MOI.ConstraintFunction(), cx, fy)
    @test MOI.get(uf, MOI.ConstraintFunction(), cx) ≈ fy
    @test MOI.get(uf, MOI.CanonicalConstraintFunction(), cx) ≈ fy
    @test MOI.Utilities.is_canonical(
        MOI.get(uf, MOI.CanonicalConstraintFunction(), cx),
    )
    @test MOI.supports(uf, MOI.ConstraintName(), typeof(cx))
    MOI.set(uf, MOI.ConstraintName(), cx, "EqualTo")
    @test MOI.get(uf, MOI.ConstraintName(), cx) == "EqualTo"
    @test MOI.get(uf, typeof(cx), "EqualTo") == cx
    MOI.delete(uf, cx)
    @test MOI.get(uf, typeof(cx), "EqualTo") === nothing
    return
end

function test_duplicate_names()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    x = MOI.add_variable(uf)
    # These are both intercepted by the fallback.
    f = MOI.ScalarAffineFunction{Float64}([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    x_eq = MOI.add_constraint(uf, f, MOI.EqualTo(1.0))
    x_gt = MOI.add_constraint(uf, f, MOI.GreaterThan(1.0))
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
    return
end

function test_deterministic_constraint_ordering()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    set1 = MOI.EqualTo(1.0)
    set2 = MOI.GreaterThan(1.0)
    F = MOI.ScalarAffineFunction{Float64}
    for sets in [[set1, set2], [set2, set1]]
        model = ModelForUniversalFallback{Float64}()
        uf = MOI.Utilities.UniversalFallback(model)
        x = MOI.add_variable(uf)
        y = MOI.add_variable(uf)
        fx = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
        fy = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, y)], 0.0)
        MOI.add_constraint(uf, fx, sets[1])
        MOI.add_constraint(uf, fx, sets[2])
        MOI.add_constraint(uf, fy, sets[1])
        MOI.add_constraint(uf, fy, sets[2])
        # check that the constraint types are in the order they were added in
        @test MOI.get(uf, MOI.ListOfConstraintTypesPresent()) ==
              [(F, typeof(sets[1])), (F, typeof(sets[2]))]
        # check that the constraints given the constraint type are in the order
        # they were added in
        for set in sets
            @test MOI.get(uf, MOI.ListOfConstraintIndices{F,typeof(set)}()) == [
                MOI.ConstraintIndex{F,typeof(set)}(1),
                MOI.ConstraintIndex{F,typeof(set)}(2),
            ]
        end
    end
    return
end

function test_show()
    model = ModelForUniversalFallback{Float64}()
    uf = MOI.Utilities.UniversalFallback(model)
    @test sprint(show, uf) == MOI.Utilities.replace_acronym("""
    $(MOI.Utilities.UniversalFallback{ModelForUniversalFallback{Float64}})
    fallback for $(ModelForUniversalFallback{Float64})""")
    return
end

end  # module

TestUniversalFallback.runtests()
