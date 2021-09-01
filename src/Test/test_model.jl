###
### Define data structures usesd by later tests.
###

struct UnknownScalarSet{T} <: MOI.AbstractScalarSet
    constant::T
end

MOI.constant(set::UnknownScalarSet) = set.constant

Base.copy(set::UnknownScalarSet) = UnknownScalarSet(copy(MOI.constant(set)))

function MOIU.shift_constant(set::UnknownScalarSet, offset)
    return UnknownScalarSet(MOI.constant(set) + offset)
end

struct UnknownVectorSet <: MOI.AbstractVectorSet end

abstract type BadModel <: MOI.ModelLike end

function MOI.get(::BadModel, ::MOI.ListOfModelAttributesSet)
    return MOI.AbstractModelAttribute[]
end

MOI.get(::BadModel, ::MOI.ListOfVariableIndices) = [MOI.VariableIndex(1)]

function MOI.get(::BadModel, ::MOI.ListOfVariableAttributesSet)
    return MOI.AbstractVariableAttribute[]
end

function MOI.get(::BadModel, ::MOI.ListOfConstraintTypesPresent)
    return [(MOI.VariableIndex, MOI.EqualTo{Float64})]
end

function MOI.get(::BadModel, ::MOI.ListOfConstraintIndices{F,S}) where {F,S}
    return [MOI.ConstraintIndex{F,S}(1)]
end

function MOI.get(
    ::BadModel,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
)
    return MOI.VariableIndex(1)
end

function MOI.get(
    ::BadModel,
    ::MOI.ConstraintSet,
    ::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
)
    return MOI.EqualTo(0.0)
end

struct BadConstraintModel <: BadModel end

function MOI.get(::BadConstraintModel, ::MOI.ListOfConstraintTypesPresent)
    return [
        (MOI.VariableIndex, MOI.EqualTo{Float64}),
        (MOI.VariableIndex, UnknownScalarSet{Float64}),
    ]
end

function MOI.get(
    ::BadModel,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.VariableIndex,<:UnknownScalarSet},
)
    return MOI.VariableIndex(1)
end

function MOI.get(
    ::BadModel,
    ::MOI.ConstraintSet,
    ::MOI.ConstraintIndex{MOI.VariableIndex,UnknownScalarSet{T}},
) where {T}
    return UnknownScalarSet(one(T))
end

struct UnknownModelAttribute <: MOI.AbstractModelAttribute end

struct BadModelAttributeModel <: BadModel end

MOI.get(src::BadModelAttributeModel, ::UnknownModelAttribute) = 0

function MOI.get(::BadModelAttributeModel, ::MOI.ListOfModelAttributesSet)
    return MOI.AbstractModelAttribute[UnknownModelAttribute()]
end

struct UnknownVariableAttribute <: MOI.AbstractVariableAttribute end

struct BadVariableAttributeModel <: BadModel end

function MOI.get(
    ::BadVariableAttributeModel,
    ::UnknownVariableAttribute,
    ::MOI.VariableIndex,
)
    return 0
end

function MOI.get(::BadVariableAttributeModel, ::MOI.ListOfVariableAttributesSet)
    return MOI.AbstractVariableAttribute[UnknownVariableAttribute()]
end

struct UnknownConstraintAttribute <: MOI.AbstractConstraintAttribute end

struct BadConstraintAttributeModel <: BadModel end

function MOI.get(
    ::BadConstraintAttributeModel,
    ::UnknownConstraintAttribute,
    ::MOI.ConstraintIndex,
)
    return 0
end

function MOI.get(
    ::BadConstraintAttributeModel,
    ::MOI.ListOfConstraintAttributesSet,
)
    return MOI.AbstractConstraintAttribute[UnknownConstraintAttribute()]
end

###
### The actual tests
###

"""
    test_model_default_ObjectiveSense(model::MOI.ModelLike, ::Config)

Test that the default ObjectiveSense is FEASIBILITY_SENSE.
"""
function test_model_default_ObjectiveSense(model::MOI.ModelLike, ::Config)
    MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
    _test_attribute_value_type(model, MOI.ObjectiveSense())
    return
end

"""
    test_model_default_TerminationStatus(model::MOI.AbstractOptimizer, ::Config)

Test that the default TerminationStatus is OPTIMIZE_NOT_CALLED.
"""
function test_model_default_TerminationStatus(
    model::MOI.AbstractOptimizer,
    ::Config,
)
    MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    _test_attribute_value_type(model, MOI.TerminationStatus())
    return
end

test_model_default_TerminationStatus(::MOI.ModelLike, ::Config) = nothing

"""
    test_model_default_PrimalStatus(model::MOI.AbstractOptimizer, ::Config)

Test that the default PrimalStatus is NO_SOLUTION.
"""
function test_model_default_PrimalStatus(model::MOI.AbstractOptimizer, ::Config)
    MOI.get(model, MOI.PrimalStatus()) == MOI.NO_SOLUTION
    _test_attribute_value_type(model, MOI.PrimalStatus())
    return
end

test_model_default_PrimalStatus(::MOI.ModelLike, ::Config) = nothing

"""
    test_model_default_DualStatus(model::MOI.AbstractOptimizer, ::Config)

Test that the default DualStatus is NO_SOLUTION.
"""
function test_model_default_DualStatus(model::MOI.AbstractOptimizer, ::Config)
    MOI.get(model, MOI.DualStatus()) == MOI.NO_SOLUTION
    _test_attribute_value_type(model, MOI.DualStatus())
    return
end

test_model_default_DualStatus(::MOI.ModelLike, ::Config) = nothing

"""
    test_model_VariableName(model::MOI.ModelLike, ::Config)

Test MOI.VariableName.
"""
function test_model_VariableName(model::MOI.ModelLike, ::Config)
    @requires MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
    x = MOI.add_variables(model, 2)
    MOI.set(model, MOI.VariableName(), x[1], "x1")
    @test MOI.get(model, MOI.VariableIndex, "x1") == x[1]
    MOI.set(model, MOI.VariableName(), x[1], "x2")
    @test MOI.get(model, MOI.VariableIndex, "x1") === nothing
    @test MOI.get(model, MOI.VariableIndex, "x2") == x[1]
    MOI.set(model, MOI.VariableName(), x[2], "x1")
    @test MOI.get(model, MOI.VariableIndex, "x1") == x[2]
    MOI.set(model, MOI.VariableName(), x[1], "x1")
    @test_throws ErrorException MOI.get(model, MOI.VariableIndex, "x1")
    _test_attribute_value_type(model, MOI.VariableName(), x[1])
    return
end

"""
    test_model_VariableIndex_ConstraintName(
        model::MOI.ModelLike,
        ::Config,
    )

Test ConstraintName for VariableIndex constraints.
"""
function test_model_VariableIndex_ConstraintName(model::MOI.ModelLike, ::Config)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
    @test_throws(
        MOI.VariableIndexConstraintNameError(),
        MOI.set(model, MOI.ConstraintName(), c, "c1"),
    )
    return
end

"""
    test_model_ScalarAffineFunction_ConstraintName(
        model::MOI.ModelLike,
        ::Config,
    )

Test ConstraintName for ScalarAffineFunction constraints.
"""
function test_model_ScalarAffineFunction_ConstraintName(
    model::MOI.ModelLike,
    ::Config,
)
    x = MOI.add_variable(model)
    f = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0)
    c1 = MOI.add_constraint(model, f, MOI.GreaterThan(0.0))
    c2 = MOI.add_constraint(model, f, MOI.LessThan(1.0))
    MOI.set(model, MOI.ConstraintName(), c1, "c1")
    @test MOI.get(model, MOI.ConstraintIndex, "c1") == c1
    MOI.set(model, MOI.ConstraintName(), c1, "c2")
    @test MOI.get(model, MOI.ConstraintIndex, "c1") === nothing
    @test MOI.get(model, MOI.ConstraintIndex, "c2") == c1
    MOI.set(model, MOI.ConstraintName(), c2, "c1")
    @test MOI.get(model, MOI.ConstraintIndex, "c1") == c2
    MOI.set(model, MOI.ConstraintName(), c1, "c1")
    @test_throws ErrorException MOI.get(model, MOI.ConstraintIndex, "c1")
    return
end

"""
    test_model_Name(model::MOI.ModelLike, ::Config)

Test the `MOI.Name` attribute.
"""
function test_model_Name(model::MOI.ModelLike, ::Config)
    @requires MOI.supports(model, MOI.Name())
    @test !(MOI.Name() in MOI.get(model, MOI.ListOfModelAttributesSet()))
    @test MOI.get(model, MOI.Name()) == ""
    MOI.set(model, MOI.Name(), "Name1")
    @test MOI.Name() in MOI.get(model, MOI.ListOfModelAttributesSet())
    @test MOI.get(model, MOI.Name()) == "Name1"
    MOI.set(model, MOI.Name(), "Name2")
    @test MOI.Name() in MOI.get(model, MOI.ListOfModelAttributesSet())
    @test MOI.get(model, MOI.Name()) == "Name2"
    _test_attribute_value_type(model, MOI.Name())
    return
end

"""
    test_model_VariableName_ConstraintName(
        model::MOI.ModelLike,
        config::Config,
    )

Test a variety of name attribute behavior.
"""
function test_model_Name_VariableName_ConstraintName(
    model::MOI.ModelLike,
    config::Config,
)
    @requires MOI.supports_incremental_interface(model)
    @requires MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test MOI.get(
        model,
        MOI.NumberOfConstraints{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    ) == 0
    v = MOI.add_variables(model, 2)
    @test MOI.get(model, MOI.VariableName(), v[1]) == ""
    x, cx = MOI.add_constrained_variable(model, MOI.GreaterThan(0.0))
    @test MOI.get(model, MOI.VariableName(), x) == ""
    y, cy = MOI.add_constrained_variables(model, MOI.Nonpositives(4))
    for yi in y
        @test MOI.get(model, MOI.VariableName(), yi) == ""
    end
    @test MOI.get(model, MOI.ConstraintName(), cy) == ""
    MOI.set(model, MOI.VariableName(), v[1], "")
    MOI.set(model, MOI.VariableName(), v[2], "") # Shouldn't error with duplicate empty name
    MOI.set(model, MOI.VariableName(), x, "")
    for yi in y
        MOI.set(model, MOI.VariableName(), yi, "")
    end
    MOI.set(model, MOI.VariableName(), v[1], "Var1")
    MOI.set(model, MOI.VariableName(), v[2], "Var1")
    # Lookup must fail when there are multiple variables with the same name.
    @test_throws Exception MOI.get(model, MOI.VariableIndex, "Var1")
    MOI.set(model, MOI.VariableName(), v[2], "Var2")
    @test MOI.get(model, MOI.VariableIndex, "Var1") == v[1]
    @test MOI.get(model, MOI.VariableIndex, "Var2") == v[2]
    @test MOI.get(model, MOI.VariableIndex, "Var3") === nothing
    MOI.set(model, MOI.VariableName(), x, "Var1")
    @test_throws Exception MOI.get(model, MOI.VariableIndex, "Var1")
    MOI.set(model, MOI.VariableName(), x, "Varx")
    @test MOI.get(model, MOI.VariableIndex, "Var1") == v[1]
    @test MOI.get(model, MOI.VariableIndex, "Var2") == v[2]
    @test MOI.get(model, MOI.VariableIndex, "Varx") == x
    @test MOI.get(model, MOI.VariableIndex, "Var3") === nothing
    vynames = ["VarX", "Var2", "Vary1", "Vary2", "Vary3", "Vary4"]
    MOI.set(model, MOI.VariableName(), [v; y], vynames)
    @test MOI.get(model, MOI.VariableName(), v) == vynames[1:2]
    @test MOI.get(model, MOI.VariableName(), y) == vynames[3:6]
    @test MOI.get(model, MOI.VariableName(), [v; y]) == vynames
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], v), 0.0),
        MOI.LessThan(1.0),
    )
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{Float64},
        MOI.EqualTo{Float64},
    )
    c2 = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0, 1.0], v), 0.0),
        MOI.EqualTo(0.0),
    )
    @test MOI.get(model, MOI.ConstraintName(), c) == ""
    @test MOI.get(model, MOI.ConstraintName(), c2) == ""
    @test MOI.get(model, MOI.ConstraintName(), cy) == ""
    @requires MOI.supports(model, MOI.ConstraintName(), typeof(c))
    MOI.set(model, MOI.ConstraintName(), c, "")
    @requires MOI.supports(model, MOI.ConstraintName(), typeof(c2))
    MOI.set(model, MOI.ConstraintName(), c2, "") # Shouldn't error with duplicate empty name
    @requires MOI.supports(model, MOI.ConstraintName(), typeof(cy))
    MOI.set(model, MOI.ConstraintName(), cy, "")
    MOI.set(model, MOI.ConstraintName(), c, "Con0")
    @test MOI.get(model, MOI.ConstraintName(), c) == "Con0"
    MOI.set(model, MOI.ConstraintName(), c2, "Con0")
    # Lookup must fail when multiple constraints have the same name.
    @test_throws Exception MOI.get(model, MOI.ConstraintIndex, "Con0")
    @test_throws Exception MOI.get(model, typeof(c), "Con0")
    MOI.set(model, MOI.ConstraintName(), [c], ["Con1"])
    @test MOI.get(model, MOI.ConstraintName(), [c]) == ["Con1"]
    @test MOI.get(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        },
        "Con1",
    ) == c
    @test MOI.get(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        },
        "Con1",
    ) === nothing
    @test MOI.get(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.GreaterThan{Float64},
        },
        "Con1",
    ) === nothing
    @test MOI.get(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        },
        "Con2",
    ) === nothing
    @test MOI.get(model, MOI.ConstraintIndex, "Con1") == c
    @test MOI.get(model, MOI.ConstraintIndex, "Con2") === nothing
    MOI.set(model, MOI.ConstraintName(), [c2, cy], ["Con2", "Con2"])
    @test_throws Exception MOI.get(model, MOI.ConstraintIndex, "Con2")
    @test_throws Exception MOI.get(model, typeof(c2), "Con2")
    @test_throws Exception MOI.get(model, typeof(cy), "Con2")
    MOI.set(model, MOI.ConstraintName(), cy, "Con4")
    for (i, ca) in zip([1, 2, 4], [c, c2, cy])
        namea = "Con$i"
        @test MOI.get(model, MOI.ConstraintName(), ca) == namea
        @test MOI.get(model, typeof(ca), namea) == ca
        @test MOI.get(model, MOI.ConstraintIndex, namea) == ca
        for cb in [c, c2, cy]
            if ca === cb
                continue
            end
            nameb = MOI.get(model, MOI.ConstraintName(), cb)
            @test MOI.get(model, typeof(cb), namea) === nothing
            @test MOI.get(model, typeof(ca), nameb) === nothing
        end
    end
    if _supports(config, MOI.delete)
        MOI.delete(model, v[2])
        @test MOI.get(model, MOI.VariableIndex, "Var2") === nothing

        MOI.delete(model, c)
        @test MOI.get(model, typeof(c), "Con1") === nothing
        @test MOI.get(model, MOI.ConstraintIndex, "Con1") === nothing

        MOI.delete(model, x)
        @test MOI.get(model, MOI.VariableIndex, "Varx") === nothing
        @test MOI.get(model, MOI.ConstraintIndex, "Con3") === nothing
        @test MOI.get(model, typeof(c2), "Con2") === c2
        @test MOI.get(model, MOI.ConstraintIndex, "Con2") === c2

        MOI.delete(model, y)
        @test MOI.get(model, typeof(cy), "Con4") === nothing
        @test MOI.get(model, MOI.ConstraintIndex, "Con4") === nothing
        for i in 1:4
            @test MOI.get(model, MOI.VariableIndex, "Vary$i") === nothing
        end
        MOI.set(model, MOI.ConstraintName(), c2, "Con4")
        @test MOI.get(model, typeof(c2), "Con4") === c2
        @test MOI.get(model, MOI.ConstraintIndex, "Con4") === c2
    end
    return
end

"""
    test_model_duplicate_VariableName(
        model::MOI.ModelLike,
        config::Config,
    )

Test duplicate variable names.
"""
function test_model_duplicate_VariableName(model::MOI.ModelLike, config::Config)
    (x, y, z) = MOI.add_variables(model, 3)
    MOI.set(model, MOI.VariableName(), x, "x")
    MOI.set(model, MOI.VariableName(), y, "x")
    MOI.set(model, MOI.VariableName(), z, "z")
    @test MOI.get(model, MOI.VariableIndex, "z") == z
    @test_throws ErrorException MOI.get(model, MOI.VariableIndex, "x")
    MOI.set(model, MOI.VariableName(), y, "y")
    @test MOI.get(model, MOI.VariableIndex, "x") == x
    @test MOI.get(model, MOI.VariableIndex, "y") == y
    MOI.set(model, MOI.VariableName(), z, "x")
    @test_throws ErrorException MOI.get(model, MOI.VariableIndex, "x")
    if _supports(config, MOI.delete)
        MOI.delete(model, x)
        @test MOI.get(model, MOI.VariableIndex, "x") == z
    end
    return
end

"""
    test_model_duplicate_ScalarAffineFunction_ConstraintName(
        model::MOI.ModelLike,
        config::Config,
    )

Test duplicate names in ScalarAffineFunction constraints.
"""
function test_model_duplicate_ScalarAffineFunction_ConstraintName(
    model::MOI.ModelLike,
    config::Config,
)
    x = MOI.add_variables(model, 3)
    fs = [
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, xi)], 0.0) for
        xi in x
    ]
    c = MOI.add_constraints(model, fs, MOI.GreaterThan(0.0))
    MOI.set(model, MOI.ConstraintName(), c[1], "x")
    MOI.set(model, MOI.ConstraintName(), c[2], "x")
    MOI.set(model, MOI.ConstraintName(), c[3], "z")
    @test MOI.get(model, MOI.ConstraintIndex, "z") == c[3]
    @test_throws ErrorException MOI.get(model, MOI.ConstraintIndex, "x")
    MOI.set(model, MOI.ConstraintName(), c[2], "y")
    @test MOI.get(model, MOI.ConstraintIndex, "x") == c[1]
    @test MOI.get(model, MOI.ConstraintIndex, "y") == c[2]
    MOI.set(model, MOI.ConstraintName(), c[3], "x")
    @test_throws ErrorException MOI.get(model, MOI.ConstraintIndex, "x")
    if _supports(config, MOI.delete)
        MOI.delete(model, c[1])
        @test MOI.get(model, MOI.ConstraintIndex, "x") == c[3]
    end
    return
end

"""
    test_model_is_valid(model::MOI.ModelLike, config::Config)

Test various parts of `MOI.is_valid`.

Taken from https://github.com/jump-dev/MathOptInterfaceUtilities.jl/issues/41
"""
function test_model_is_valid(model::MOI.ModelLike, config::Config)
    @requires MOI.supports_incremental_interface(model)
    v = MOI.add_variables(model, 2)
    @test MOI.is_valid(model, v[1])
    @test MOI.is_valid(model, v[2])
    x = MOI.add_variable(model)
    @test MOI.is_valid(model, x)
    if _supports(config, MOI.delete)
        MOI.delete(model, x)
        @test !MOI.is_valid(model, x)
    end
    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], v), 0.0)
    @requires MOI.supports_constraint(model, typeof(cf), MOI.LessThan{Float64})
    c = MOI.add_constraint(model, cf, MOI.LessThan(1.0))
    @test MOI.is_valid(model, c)
    @test !MOI.is_valid(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float32},
            MOI.LessThan{Float32},
        }(
            1,
        ),
    )
    @test !MOI.is_valid(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float32},
            MOI.LessThan{Float64},
        }(
            1,
        ),
    )
    @test !MOI.is_valid(
        model,
        MOI.ConstraintIndex{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float32},
        }(
            1,
        ),
    )
    @test !MOI.is_valid(
        model,
        MOI.ConstraintIndex{
            MOI.VectorQuadraticFunction{Float64},
            MOI.SecondOrderCone,
        }(
            1,
        ),
    )
    return
end

"""
    test_model_empty(model::MOI.ModelLike, ::Config)

Test `MOI.empty` and `MOI.is_empty!`.
"""
function test_model_empty(model::MOI.ModelLike, ::Config)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    return
end

"""
    test_model_copy_to_UnsupportedConstraint(
        model::MOI.ModelLike,
        ::Config,
    )

Test `MOI.copy_to` when a constraint is unsupported.
"""
function test_model_copy_to_UnsupportedConstraint(
    model::MOI.ModelLike,
    ::Config,
)
    @test !MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        UnknownScalarSet{Float64},
    )
    @test_throws(
        MOI.UnsupportedConstraint,
        MOI.copy_to(model, BadConstraintModel()),
    )
    return
end

"""
    test_model_copy_to_UnsupportedAttribute(
        model::MOI.ModelLike,
        ::Config,
    )

Test `MOI.copy_to` when an attribute is unsupported.
"""
function test_model_copy_to_UnsupportedAttribute(model::MOI.ModelLike, ::Config)
    # ModelAttribute
    @test !MOI.supports(model, UnknownModelAttribute())
    @test_throws(
        MOI.UnsupportedAttribute,
        MOI.copy_to(model, BadModelAttributeModel()),
    )
    # VariableAttribute
    @test !MOI.supports(model, UnknownVariableAttribute(), MOI.VariableIndex)
    @test_throws(
        MOI.UnsupportedAttribute,
        MOI.copy_to(model, BadVariableAttributeModel()),
    )
    # ConstraintAttribute
    @test !MOI.supports(
        model,
        UnknownConstraintAttribute(),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
    )
    @test_throws(
        MOI.UnsupportedAttribute,
        MOI.copy_to(model, BadConstraintAttributeModel()),
    )
    return
end

function test_model_supports_constraint_VariableIndex_EqualTo(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{T})
    # Pick a "bad" coefficient type that should fail tests.
    @test !MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{UInt8})
    # Scalar-in-vector
    @test !MOI.supports_constraint(model, MOI.VariableIndex, MOI.Zeros)
    return
end

function test_model_supports_constraint_ScalarAffineFunction_EqualTo(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{T},
        MOI.EqualTo{T},
    )
    # Pick a "bad" coefficient type that should fail tests.
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{UInt8},
        MOI.EqualTo{UInt8},
    )
    return
end

function test_model_supports_constraint_VectorOfVariables_Nonnegatives(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Nonnegatives,
    )
    # Pick a "bad" coefficient type that should fail tests.
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        UnknownVectorSet,
    )
    @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.EqualTo{T})
    return
end

"""
    test_model_ordered_indices(model::MOI.ModelLike, ::Config)

Test whether the model returns ListOfVariableIndices and ListOfConstraintIndices
sorted by creation time.
"""
function test_model_ordered_indices(model::MOI.ModelLike, ::Config)
    @requires MOI.supports_incremental_interface(model)
    v1 = MOI.add_variable(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v1]
    v2 = MOI.add_variable(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v1, v2]
    MOI.delete(model, v1)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v2]
    v3 = MOI.add_variable(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v2, v3]
    v4 = MOI.add_variable(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v2, v3, v4]
    # Note: there are too many combinations to test, so we're just going to
    # check VariableIndex-in-LessThan and hope it works for the rest
    c1 = MOI.add_constraint(model, v2, MOI.LessThan(1.0))
    @test c1.value == v2.value
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == [c1]
    c2 = MOI.add_constraint(model, v3, MOI.LessThan(2.0))
    @test c2.value == v3.value
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == [c1, c2]
    MOI.delete(model, c1)
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == [c2]
    c3 = MOI.add_constraint(model, v4, MOI.LessThan(3.0))
    @test c3.value == v4.value
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.LessThan{Float64}}(),
    ) == [c2, c3]
    return
end

"""
    test_model_ScalarFunctionConstantNotZero(
        model::MOI.ModelLike,
        config::Config,
    )

Test adding a linear constraint with a non-zero function constant.

This should either work, or error with `MOI.ScalarFunctionConstantNotZero` if
the model does not support it.

To skip this test, pass `MOI.ScalarFunctionConstantNotZero` to the `exclude`
argument of [`Config`](@ref).
"""
function test_model_ScalarFunctionConstantNotZero(
    model::MOI.ModelLike,
    config::Config,
)
    @requires _supports(config, MOI.ScalarFunctionConstantNotZero)
    function _error(S, value)
        return MOI.ScalarFunctionConstantNotZero{
            Float64,
            MOI.ScalarAffineFunction{Float64},
            S,
        }(
            value,
        )
    end
    try
        f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 1.0)
        c = MOI.add_constraint(model, f, MOI.EqualTo(2.0))
        @requires _supports(config, MOI.ConstraintFunction)
        @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ f
    catch err
        @test err == _error(MOI.EqualTo{Float64}, 1.0)
    end
    try
        f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 2.0)
        c = MOI.add_constraint(model, f, MOI.GreaterThan(1.0))
        @requires _supports(config, MOI.ConstraintFunction)
        @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ f
    catch err
        @test err == _error(MOI.GreaterThan{Float64}, 2.0)
    end
    return
end

"""
    test_model_LowerBoundAlreadySet(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test that setting a lower-bound twice throws `LowerBoundAlreadySet`.
"""
function test_model_LowerBoundAlreadySet(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VariableIndex,
        MOI.GreaterThan{T},
    )
    @requires _supports(config, MOI.delete)
    x = MOI.add_variable(model)
    lb = zero(T)
    sets = [
        MOI.EqualTo(lb),
        MOI.Interval(lb, lb),
        MOI.Semicontinuous(lb, lb),
        MOI.Semiinteger(lb, lb),
    ]
    set2 = MOI.GreaterThan(lb)
    for set1 in sets
        if !MOI.supports_constraint(model, MOI.VariableIndex, typeof(set1))
            continue
        end
        ci = MOI.add_constraint(model, x, set1)
        err = MOI.LowerBoundAlreadySet{typeof(set1),typeof(set2)}(x)
        @test_throws err MOI.add_constraint(model, x, set2)
        MOI.delete(model, ci)
        ci = MOI.add_constraint(model, x, set2)
        err = MOI.LowerBoundAlreadySet{typeof(set2),typeof(set1)}(x)
        @test_throws err MOI.add_constraint(model, x, set1)
        MOI.delete(model, ci)
    end
    return
end

"""
    test_model_UpperBoundAlreadySet(
        model::MOI.ModelLike,
        ::Config{T},
    ) where {T}

Test that setting a lower-bound twice throws `UpperBoundAlreadySet`.
"""
function test_model_UpperBoundAlreadySet(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    x = MOI.add_variable(model)
    ub = zero(T)
    @requires MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
    sets = [
        MOI.EqualTo(ub),
        MOI.Interval(ub, ub),
        MOI.Semicontinuous(ub, ub),
        MOI.Semiinteger(ub, ub),
    ]
    set2 = MOI.LessThan(ub)
    for set1 in sets
        if !MOI.supports_constraint(model, MOI.VariableIndex, typeof(set1))
            continue
        end
        ci = MOI.add_constraint(model, x, set1)
        err = MOI.UpperBoundAlreadySet{typeof(set1),typeof(set2)}(x)
        @test_throws err MOI.add_constraint(model, x, set2)
        MOI.delete(model, ci)
        ci = MOI.add_constraint(model, x, set2)
        err = MOI.UpperBoundAlreadySet{typeof(set2),typeof(set1)}(x)
        @test_throws err MOI.add_constraint(model, x, set1)
        MOI.delete(model, ci)
    end
    return
end

"""
    test_model_delete(model::MOI.ModelLike, config::Config)

Test various operations to do with deleting variables and constraints.
"""
function test_model_delete(model::MOI.ModelLike, config::Config)
    @requires _supports(config, MOI.delete)
    x = MOI.add_variable(model)
    cx = MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
    y = MOI.add_variables(model, 4)
    cy = MOI.add_constraint(model, y, MOI.Nonpositives(4))
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cx) == x
    @test MOI.get(model, MOI.ConstraintSet(), cx) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) ==
          MOI.VectorOfVariables(y)
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(4)
    @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == Set([
        (MOI.VariableIndex, MOI.GreaterThan{Float64}),
        (MOI.VectorOfVariables, MOI.Nonpositives),
    ])
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.VariableIndex,
            MOI.GreaterThan{Float64},
        }(),
    ) == [cx]
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonpositives}(),
    ) == [cy]
    MOI.delete(model, y[3])
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cx) == x
    @test MOI.get(model, MOI.ConstraintSet(), cx) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) ==
          MOI.VectorOfVariables(y[[1, 2, 4]])
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(3)
    @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == Set([
        (MOI.VariableIndex, MOI.GreaterThan{Float64}),
        (MOI.VectorOfVariables, MOI.Nonpositives),
    ])
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.VariableIndex,
            MOI.GreaterThan{Float64},
        }(),
    ) == [cx]
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonpositives}(),
    ) == [cy]
    MOI.delete(model, y[1])
    @test MOI.is_valid(model, x)
    @test !MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cx) == x
    @test MOI.get(model, MOI.ConstraintSet(), cx) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) ==
          MOI.VectorOfVariables(y[[2, 4]])
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(2)
    @test Set(MOI.get(model, MOI.ListOfConstraintTypesPresent())) == Set([
        (MOI.VariableIndex, MOI.GreaterThan{Float64}),
        (MOI.VectorOfVariables, MOI.Nonpositives),
    ])
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.VariableIndex,
            MOI.GreaterThan{Float64},
        }(),
    ) == [cx]
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonpositives}(),
    ) == [cy]
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    @test !MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test !MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) ==
          MOI.VectorOfVariables(y[[2, 4]])
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(2)
    @test MOI.get(model, MOI.ListOfConstraintTypesPresent()) ==
          [(MOI.VectorOfVariables, MOI.Nonpositives)]
    @test isempty(
        MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    @test MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonpositives}(),
    ) == [cy]
    MOI.delete(model, y[[2, 4]])
    @test !MOI.is_valid(model, x)
    @test !MOI.is_valid(model, y[1])
    @test !MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test !MOI.is_valid(model, y[4])
    @test !MOI.is_valid(model, cx)
    @test !MOI.is_valid(model, cy)
    @test isempty(MOI.get(model, MOI.ListOfConstraintTypesPresent()))
    @test isempty(
        MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.VariableIndex,
                MOI.GreaterThan{Float64},
            }(),
        ),
    )
    @test isempty(
        MOI.get(
            model,
            MOI.ListOfConstraintIndices{
                MOI.VectorOfVariables,
                MOI.Nonpositives,
            }(),
        ),
    )
end

"""
    test_model_ListOfConstraintAttributesSet(
        model::MOI.ModelLike,
        config::Config{T},
    ) where {T}

Test issue #1429: ConstraintName should only be included in
`ListOfConstraintAttributesSet` if it actually is set.
"""
function test_model_ListOfConstraintAttributesSet(
    model::MOI.ModelLike,
    ::Config{T},
) where {T}
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, MOI.GreaterThan(zero(T)))
    c = MOI.add_constraint(
        model,
        MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(one(T), x)], zero(T)),
        MOI.EqualTo(one(T)),
    )
    MOI.set(model, MOI.ConstraintName(), c, "c")
    @test MOI.get(
        model,
        MOI.ListOfConstraintAttributesSet{
            MOI.ScalarAffineFunction{T},
            MOI.EqualTo{T},
        }(),
    ) == [MOI.ConstraintName()]
    @test MOI.get(
        model,
        MOI.ListOfConstraintAttributesSet{
            MOI.VariableIndex,
            MOI.GreaterThan{T},
        }(),
    ) == []
    return
end
