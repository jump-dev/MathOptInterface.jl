# TODO: Move generic model tests from MOIU to here

struct UnknownScalarSet{T} <: MOI.AbstractScalarSet
    constant::T
end
MOI.constant(set::UnknownScalarSet) = set.constant
Base.copy(set::UnknownScalarSet) = UnknownScalarSet(copy(MOI.constant(set)))
function MOIU.shift_constant(set::UnknownScalarSet, offset)
    return UnknownScalarSet(MOI.constant(set) + offset)
end

struct UnknownVectorSet <: MOI.AbstractVectorSet end

function default_objective_test(model::MOI.ModelLike)
    @testset "Test default objective" begin
        MOI.empty!(model)
        MOI.get(model, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
    end
end

function default_status_test(model::MOI.ModelLike)
    MOI.empty!(model)
    @test MOI.get(model, MOI.TerminationStatus()) == MOI.OPTIMIZE_NOT_CALLED
    @test MOI.get(model, MOI.PrimalStatus()) == MOI.NO_SOLUTION
    @test MOI.get(model, MOI.DualStatus()) == MOI.NO_SOLUTION
end

function nametest(model::MOI.ModelLike; delete::Bool = true)
    @testset "Variables" begin
        MOI.empty!(model)
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
    end
    @testset "Variable bounds" begin
        MOI.empty!(model)
        x = MOI.add_variable(model)
        c1 = MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
        c2 = MOI.add_constraint(model, x, MOI.LessThan(1.0))
        @test_throws(
            MOI.VariableIndexConstraintNameError(),
            MOI.set(model, MOI.ConstraintName(), c1, "c1"),
        )
    end
    @testset "Affine constraints" begin
        MOI.empty!(model)
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
    end
    @testset "Name" begin
        MOI.empty!(model)
        @test MOI.supports_incremental_interface(model)
        @test MOI.supports(model, MOI.Name())
        @test !(MOI.Name() in MOI.get(model, MOI.ListOfModelAttributesSet()))
        @test MOI.get(model, MOI.Name()) == ""
        MOI.set(model, MOI.Name(), "Name1")
        @test MOI.Name() in MOI.get(model, MOI.ListOfModelAttributesSet())
        @test MOI.get(model, MOI.Name()) == "Name1"
        MOI.set(model, MOI.Name(), "Name2")
        @test MOI.Name() in MOI.get(model, MOI.ListOfModelAttributesSet())
        @test MOI.get(model, MOI.Name()) == "Name2"
        @test MOI.get(model, MOI.NumberOfVariables()) == 0
        @test MOI.get(
            model,
            MOI.NumberOfConstraints{
                MOI.ScalarAffineFunction{Float64},
                MOI.LessThan{Float64},
            }(),
        ) == 0
        @test MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
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
        @test MOI.supports_constraint(
            model,
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        )
        c = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], v), 0.0),
            MOI.LessThan(1.0),
        )
        @test MOI.supports_constraint(
            model,
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        )
        c2 = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([-1.0, 1.0], v),
                0.0,
            ),
            MOI.EqualTo(0.0),
        )
        @test MOI.get(model, MOI.ConstraintName(), c) == ""
        @test MOI.get(model, MOI.ConstraintName(), c2) == ""
        @test MOI.get(model, MOI.ConstraintName(), cy) == ""
        @test MOI.supports(model, MOI.ConstraintName(), typeof(c))
        MOI.set(model, MOI.ConstraintName(), c, "")
        @test MOI.supports(model, MOI.ConstraintName(), typeof(c2))
        MOI.set(model, MOI.ConstraintName(), c2, "") # Shouldn't error with duplicate empty name
        @test MOI.supports(model, MOI.ConstraintName(), typeof(cy))
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
        if delete
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
    end
    @testset "Duplicate names" begin
        @testset "Variables" begin
            MOI.empty!(model)
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
            if delete
                MOI.delete(model, x)
                @test MOI.get(model, MOI.VariableIndex, "x") == z
            end
        end
        @testset "ScalarAffineFunction" begin
            MOI.empty!(model)
            x = MOI.add_variables(model, 3)
            fs = [
                MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, xi)], 0.0) for xi in x
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
            if delete
                MOI.delete(model, c[1])
                @test MOI.get(model, MOI.ConstraintIndex, "x") == c[3]
            end
        end
    end
end

# Taken from https://github.com/jump-dev/MathOptInterfaceUtilities.jl/issues/41
function validtest(model::MOI.ModelLike; delete::Bool = true)
    MOI.empty!(model)
    @test MOI.supports_incremental_interface(model)
    v = MOI.add_variables(model, 2)
    @test MOI.is_valid(model, v[1])
    @test MOI.is_valid(model, v[2])
    x = MOI.add_variable(model)
    @test MOI.is_valid(model, x)
    if delete
        MOI.delete(model, x)
        @test !MOI.is_valid(model, x)
    end
    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, 1.0], v), 0.0)
    @test MOI.supports_constraint(model, typeof(cf), MOI.LessThan{Float64})
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
end

function emptytest(model::MOI.ModelLike)
    MOI.empty!(model)
    @test MOI.supports_incremental_interface(model)
    # Taken from LIN1
    v = MOI.add_variables(model, 3)
    @test MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Nonnegatives,
    )
    vc =
        MOI.add_constraint(model, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    @test MOI.supports_constraint(
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    c = MOI.add_constraint(
        model,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 1, 1, 2, 2],
                MOI.ScalarAffineTerm.(1.0, [v; v[2]; v[3]]),
            ),
            [-3.0, -2.0],
        ),
        MOI.Zeros(2),
    )
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v),
            0.0,
        ),
    )
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test !MOI.is_empty(model)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    ) == 0
    @test MOI.get(
        model,
        MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}(),
    ) == 0
    @test isempty(MOI.get(model, MOI.ListOfConstraintTypesPresent()))
    @test !MOI.is_valid(model, v[1])
    @test !MOI.is_valid(model, vc)
    @test !MOI.is_valid(model, c)
end

abstract type BadModel <: MOI.ModelLike end
function MOI.get(::BadModel, ::MOI.ListOfModelAttributesSet)
    return MOI.AbstractModelAttribute[]
end
MOI.get(::BadModel, ::MOI.NumberOfVariables)::Int64 = 1
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
function MOI.get(::BadModel, ::MOI.ListOfConstraintAttributesSet)
    return MOI.AbstractConstraintAttribute[]
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

function failcopytestc(dest::MOI.ModelLike)
    @test !MOI.supports_constraint(
        dest,
        MOI.VariableIndex,
        UnknownScalarSet{Float64},
    )
    @test_throws MOI.UnsupportedConstraint MOI.copy_to(
        dest,
        BadConstraintModel(),
    )
end
function failcopytestia(dest::MOI.ModelLike)
    @test !MOI.supports(dest, UnknownModelAttribute())
    @test_throws MOI.UnsupportedAttribute MOI.copy_to(
        dest,
        BadModelAttributeModel(),
    )
end
function failcopytestva(dest::MOI.ModelLike)
    @test !MOI.supports(dest, UnknownVariableAttribute(), MOI.VariableIndex)
    @test_throws MOI.UnsupportedAttribute MOI.copy_to(
        dest,
        BadVariableAttributeModel(),
    )
end
function failcopytestca(dest::MOI.ModelLike)
    @test !MOI.supports(
        dest,
        UnknownConstraintAttribute(),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
    )
    @test_throws MOI.UnsupportedAttribute MOI.copy_to(
        dest,
        BadConstraintAttributeModel(),
    )
end

function start_values_unset_test(model::MOI.ModelLike, config)
    MOI.empty!(model)
    x, y, z = MOI.add_variables(model, 3)
    fz = z
    a = MOI.add_constraint(model, x, MOI.EqualTo(1.0))
    b = MOI.add_constraint(model, y, MOI.EqualTo(2.0))
    F1 = MOI.VariableIndex
    S1 = MOI.EqualTo{Float64}
    c = MOI.add_constraint(
        model,
        MOIU.operate(vcat, Float64, 2.0fz + 1.0),
        MOI.Nonnegatives(1),
    )
    F2 = MOI.VectorAffineFunction{Float64}
    S2 = MOI.Nonnegatives
    vpattr = MOI.VariablePrimalStart()
    cpattr = MOI.ConstraintPrimalStart()
    cdattr = MOI.ConstraintDualStart()
    @testset "Newly created variables have start values set to nothing" begin
        @test !(vpattr in MOI.get(model, MOI.ListOfVariableAttributesSet()))
        @test MOI.get(model, vpattr, x) === nothing
        @test MOI.get(model, vpattr, y) === nothing
        @test MOI.get(model, vpattr, z) === nothing
        @test cpattr ∉
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F1,S1}())
        @test MOI.get(model, cpattr, a) === nothing
        @test MOI.get(model, cpattr, b) === nothing
        @test cpattr ∉
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F2,S2}())
        @test MOI.get(model, cpattr, c) === nothing
        @test cdattr ∉
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F1,S1}())
        @test MOI.get(model, cdattr, a) === nothing
        @test MOI.get(model, cdattr, b) === nothing
        @test cdattr ∉
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F2,S2}())
        @test MOI.get(model, cdattr, c) === nothing
    end
    @testset "Allows unsetting by nothing" begin
        # First set the attributes to some values.
        MOI.set.((model,), (vpattr,), (x, y, z), (0.0, 1.0, 2.0))
        MOI.set.((model,), (cpattr,), (a, b), (0.0, 1.0))
        MOI.set.((model,), (cdattr,), (a, b), (0.0, 1.0))
        MOI.set(model, cpattr, c, [2.0])
        MOI.set(model, cdattr, c, [2.0])
        @test vpattr ∈ MOI.get(model, MOI.ListOfVariableAttributesSet())
        @test cpattr ∈
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F1,S1}())
        @test cpattr ∈
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F2,S2}())
        @test cdattr ∈
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F1,S1}())
        @test cdattr ∈
              MOI.get(model, MOI.ListOfConstraintAttributesSet{F2,S2}())
        @test all(MOI.get.((model,), (vpattr,), (x, y, z)) .== (0.0, 1.0, 2.0))
        @test all(MOI.get.((model,), (cpattr,), (a, b)) .== (0.0, 1.0))
        @test all(MOI.get.((model,), (cdattr,), (a, b)) .== (0.0, 1.0))
        @test MOI.get(model, cpattr, c) == [2.0]
        @test MOI.get(model, cdattr, c) == [2.0]
        MOI.set(model, vpattr, y, nothing)
        MOI.set(model, cpattr, a, nothing)
        MOI.set(model, cdattr, a, nothing)
        MOI.set(model, cpattr, c, nothing)
        MOI.set(model, cdattr, c, nothing)
        # The tests below are commented because it was decided to not pin a
        # determined behaviour, there are two possibilities about the behaviour
        # of ListOfConstraintAttributesSet and ListOfVariableAttributesSet:
        #
        # 1) They list all attributes ever set, even after attributes are unset.
        # 2) They list only the attributes that, at the moment, are set.
        #
        # The current behavior is (1), but we are not sure if this will not
        # be changed in the future, so we do not include any tests for it.
        # The commented tests assume (2) and if uncommented will fail.
        # Ref.: https://github.com/jump-dev/MathOptInterface.jl/pull/1136#discussion_r496466058
        #@test cpattr ∉ MOI.get(model, MOI.ListOfConstraintAttributesSet{F2, S2}())
        #@test cdattr ∉ MOI.get(model, MOI.ListOfConstraintAttributesSet{F2, S2}())
        @test all(
            MOI.get.((model,), (vpattr,), (x, y, z)) .=== (0.0, nothing, 2.0),
        )
        @test all(MOI.get.((model,), (cpattr,), (a, b)) .=== (nothing, 1.0))
        @test all(MOI.get.((model,), (cdattr,), (a, b)) .=== (nothing, 1.0))
        @test MOI.get(model, cpattr, c) === nothing
        @test MOI.get(model, cdattr, c) === nothing
        MOI.set(model, vpattr, x, nothing)
        MOI.set(model, vpattr, z, nothing)
        MOI.set(model, cpattr, b, nothing)
        MOI.set(model, cdattr, b, nothing)
        # The three tests below are commented for the same reason the two
        # commented tests above.
        #@test vpattr ∉ MOI.get(model, MOI.ListOfVariableAttributesSet())
        #@test cpattr ∉ MOI.get(model, MOI.ListOfConstraintAttributesSet{F1, S1}())
        #@test cdattr ∉ MOI.get(model, MOI.ListOfConstraintAttributesSet{F1, S1}())
    end
end

function start_values_test(dest::MOI.ModelLike, src::MOI.ModelLike)
    MOI.empty!(dest)
    @test MOI.supports_incremental_interface(src)
    x, y, z = MOI.add_variables(src, 3)
    fz = z
    a = MOI.add_constraint(src, x, MOI.EqualTo(1.0))
    b = MOI.add_constraint(src, y, MOI.EqualTo(2.0))
    F1 = MOI.VariableIndex
    S1 = MOI.EqualTo{Float64}
    c = MOI.add_constraint(
        src,
        MOIU.operate(vcat, Float64, 2.0fz + 1.0),
        MOI.Nonnegatives(1),
    )
    F2 = MOI.VectorAffineFunction{Float64}
    S2 = MOI.Nonnegatives
    vpattr = MOI.VariablePrimalStart()
    cpattr = MOI.ConstraintPrimalStart()
    cdattr = MOI.ConstraintDualStart()
    @testset "Supports" begin
        @test MOI.supports(dest, vpattr, MOI.VariableIndex)
        @test MOI.supports(dest, cpattr, MOI.ConstraintIndex{F1,S1})
        @test MOI.supports(dest, cpattr, MOI.ConstraintIndex{F2,S2})
        @test MOI.supports(dest, cdattr, MOI.ConstraintIndex{F1,S1})
        @test MOI.supports(dest, cdattr, MOI.ConstraintIndex{F2,S2})
    end
    @testset "Attribute set to no indices" begin
        filtered = MOI.Utilities.ModelFilter(name_filter, src)
        dict = MOI.copy_to(dest, filtered)
        @test !(vpattr in MOI.get(dest, MOI.ListOfVariableAttributesSet()))
        @test MOI.get(dest, vpattr, dict[x]) === nothing
        @test MOI.get(dest, vpattr, dict[y]) === nothing
        @test MOI.get(dest, vpattr, dict[z]) === nothing
        @test !(
            cpattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F1,S1}())
        )
        @test MOI.get(dest, cpattr, dict[a]) === nothing
        @test MOI.get(dest, cpattr, dict[b]) === nothing
        @test !(
            cpattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F2,S2}())
        )
        @test MOI.get(dest, cpattr, dict[c]) === nothing
        @test !(
            cdattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F1,S1}())
        )
        @test MOI.get(dest, cdattr, dict[a]) === nothing
        @test MOI.get(dest, cdattr, dict[b]) === nothing
        @test !(
            cdattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F2,S2}())
        )
        @test MOI.get(dest, cdattr, dict[c]) === nothing
    end
    @testset "Attribute set to some indices" begin
        MOI.set(src, vpattr, x, 1.0)
        MOI.set(src, vpattr, z, 3.0)
        MOI.set(src, cpattr, a, 1.0)
        MOI.set(src, cpattr, b, 2.0)
        MOI.set(src, cdattr, b, 2.0)
        MOI.set(src, cdattr, c, [3.0])
        filtered = MOI.Utilities.ModelFilter(name_filter, src)
        dict = MOI.copy_to(dest, filtered)
        @test vpattr in MOI.get(dest, MOI.ListOfVariableAttributesSet())
        @test MOI.get(dest, vpattr, dict[x]) == 1.0
        @test MOI.get(dest, vpattr, dict[y]) === nothing
        @test MOI.get(dest, vpattr, dict[z]) == 3.0
        @test cpattr in
              MOI.get(dest, MOI.ListOfConstraintAttributesSet{F1,S1}())
        @test MOI.get(dest, cpattr, dict[a]) == 1.0
        @test MOI.get(dest, cpattr, dict[b]) == 2.0
        @test MOI.get(dest, cpattr, dict[c]) === nothing
        @test cdattr in
              MOI.get(dest, MOI.ListOfConstraintAttributesSet{F1,S1}())
        @test MOI.get(dest, cdattr, dict[a]) === nothing
        @test MOI.get(dest, cdattr, dict[b]) == 2.0
        @test cdattr in
              MOI.get(dest, MOI.ListOfConstraintAttributesSet{F2,S2}())
        @test MOI.get(dest, cdattr, dict[c]) == [3.0]
    end
end

name_filter(::Any) = true
name_filter(::MOI.Name) = false
name_filter(::MOI.VariableName) = false
name_filter(::MOI.ConstraintName) = false

function copytest(dest::MOI.ModelLike, src::MOI.ModelLike; copy_names = false)
    @test MOI.supports_incremental_interface(src)
    MOI.empty!(src)
    MOI.empty!(dest)
    MOI.set(src, MOI.Name(), "ModelName")
    v = MOI.add_variables(src, 3)
    w = MOI.add_variable(src)
    MOI.set(src, MOI.VariableName(), v, ["var1", "var2", "var3"])
    csv = MOI.add_constraint(src, w, MOI.EqualTo(2.0))
    # We test this after the creation of every `VariableIndex` constraint
    # to ensure a good coverage of corner cases.
    @test csv.value == w.value
    cvv = MOI.add_constraint(src, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    MOI.set(src, MOI.ConstraintName(), cvv, "cvv")
    csa = MOI.add_constraint(
        src,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 3.0], [v[3], v[1]]),
            0.0,
        ),
        MOI.LessThan(2.0),
    )
    MOI.set(src, MOI.ConstraintName(), csa, "csa")
    cva = MOI.add_constraint(
        src,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2],
                MOI.ScalarAffineTerm.(1.0, [v[3], v[2]]),
            ),
            [-3.0, -2.0],
        ),
        MOI.Zeros(2),
    )
    MOI.set(src, MOI.ConstraintName(), cva, "cva")
    MOI.set(
        src,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v),
            2.0,
        ),
    )
    MOI.set(src, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    @test MOI.supports(
        dest,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    @test MOI.supports_constraint(dest, MOI.VariableIndex, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(dest, MOI.VectorOfVariables, MOI.Nonnegatives)
    @test MOI.supports_constraint(
        dest,
        MOI.ScalarAffineFunction{Float64},
        MOI.LessThan{Float64},
    )
    @test MOI.supports_constraint(
        dest,
        MOI.VectorAffineFunction{Float64},
        MOI.Zeros,
    )
    if copy_names
        dict = MOI.copy_to(dest, src)
    else
        filtered = MOI.Utilities.ModelFilter(name_filter, src)
        dict = MOI.copy_to(dest, filtered)
    end
    dest_name(src_name) = copy_names ? src_name : ""
    @test !MOI.supports(dest, MOI.Name()) ||
          MOI.get(dest, MOI.Name()) == dest_name("ModelName")
    @test MOI.get(dest, MOI.NumberOfVariables()) == 4
    if MOI.supports(dest, MOI.VariableName(), MOI.VariableIndex)
        for i in eachindex(v)
            MOI.get(dest, MOI.VariableName(), dict[v[i]]) == dest_name("var$i")
        end
    end
    @test MOI.get(
        dest,
        MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
    ) == 1
    @test MOI.get(
        dest,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{Float64}}(),
    ) == [dict[csv]]
    @test MOI.get(
        dest,
        MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    ) == 1
    @test MOI.get(
        dest,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}(),
    ) == [dict[cvv]]
    @test MOI.get(
        dest,
        MOI.NumberOfConstraints{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    ) == 1
    @test MOI.get(
        dest,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64},
            MOI.LessThan{Float64},
        }(),
    ) == [dict[csa]]
    @test MOI.get(
        dest,
        MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}(),
    ) == 1
    @test MOI.get(
        dest,
        MOI.ListOfConstraintIndices{
            MOI.VectorAffineFunction{Float64},
            MOI.Zeros,
        }(),
    ) == [dict[cva]]
    loc = MOI.get(dest, MOI.ListOfConstraintTypesPresent())
    @test length(loc) == 4
    @test (MOI.VariableIndex, MOI.EqualTo{Float64}) in loc
    @test (MOI.VectorOfVariables, MOI.Nonnegatives) in loc
    @test (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) in loc
    @test (MOI.VectorAffineFunction{Float64}, MOI.Zeros) in loc
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csv]) == dict[w]
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csv]) == MOI.EqualTo(2.0)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(cvv)) ||
          MOI.get(dest, MOI.ConstraintName(), dict[cvv]) == dest_name("cvv")
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cvv]) ==
          MOI.VectorOfVariables(getindex.(Ref(dict), v))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cvv]) == MOI.Nonnegatives(3)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(csa)) ||
          MOI.get(dest, MOI.ConstraintName(), dict[csa]) == dest_name("csa")
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csa]) ≈
          MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1.0, 3.0], [dict[v[3]], dict[v[1]]]),
        0.0,
    )
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csa]) == MOI.LessThan(2.0)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(cva)) ||
          MOI.get(dest, MOI.ConstraintName(), dict[cva]) == dest_name("cva")
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cva]) ≈
          MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(
            [1, 2],
            MOI.ScalarAffineTerm.(1.0, [dict[v[3]], dict[v[2]]]),
        ),
        [-3.0, -2.0],
    )
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cva]) == MOI.Zeros(2)
    @test MOI.get(
        dest,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    ) ≈ MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.(
            [-3.0, -2.0, -4.0],
            [dict[v[1]], dict[v[2]], dict[v[3]]],
        ),
        2.0,
    )
    @test MOI.get(dest, MOI.ObjectiveFunctionType()) ==
          MOI.ScalarAffineFunction{Float64}
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MIN_SENSE
end

function supports_constrainttest(
    model::MOI.ModelLike,
    ::Type{GoodT},
    ::Type{BadT},
) where {GoodT,BadT}
    v = MOI.add_variable(model)
    @test MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{GoodT})
    @test MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{GoodT},
        MOI.EqualTo{GoodT},
    )
    # Bad type
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{BadT},
        MOI.EqualTo{GoodT},
    )
    @test !MOI.supports_constraint(
        model,
        MOI.ScalarAffineFunction{BadT},
        MOI.EqualTo{BadT},
    )
    @test !MOI.supports_constraint(model, MOI.VariableIndex, MOI.EqualTo{BadT})
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.EqualTo{GoodT},
    ) # vector in scalar
    @test !MOI.supports_constraint(model, MOI.VariableIndex, MOI.Zeros) # scalar in vector
    @test !MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        UnknownVectorSet,
    ) # set not supported
end

"""
    orderedindicestest(model::MOI.ModelLike)

Test whether the model returns ListOfVariableIndices and ListOfConstraintIndices
sorted by creation time.
"""
function orderedindicestest(model::MOI.ModelLike)
    @test MOI.supports_incremental_interface(model)
    MOI.empty!(model)
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
end

# Test that `MOI.ScalarFunctionConstantNotZero` is thrown when a constraint with
# a function with nonzero constant is added. This test is optional because
# solvers could choose to support scalar functions with nonzero constants.
function scalar_function_constant_not_zero(model::MOI.ModelLike)
    @testset "Constraint with nonzero function constant" begin
        err = MOI.ScalarFunctionConstantNotZero{
            Float64,
            MOI.ScalarAffineFunction{Float64},
            MOI.EqualTo{Float64},
        }(
            1.0,
        )
        @test_throws err begin
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 1.0),
                MOI.EqualTo(2.0),
            )
        end
        err = MOI.ScalarFunctionConstantNotZero{
            Float64,
            MOI.ScalarAffineFunction{Float64},
            MOI.GreaterThan{Float64},
        }(
            2.0,
        )
        @test_throws err begin
            MOI.add_constraint(
                model,
                MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 2.0),
                MOI.GreaterThan(1.0),
            )
        end
    end
end

function set_lower_bound_twice(model::MOI.ModelLike, T::Type)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    f = x
    lb = zero(T)
    @test MOI.supports_constraint(model, MOI.VariableIndex, MOI.GreaterThan{T})
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
        ci = MOI.add_constraint(model, f, set1)
        err = MOI.LowerBoundAlreadySet{typeof(set1),typeof(set2)}(x)
        @test_throws err MOI.add_constraint(model, f, set2)
        MOI.delete(model, ci)
        ci = MOI.add_constraint(model, f, set2)
        err = MOI.LowerBoundAlreadySet{typeof(set2),typeof(set1)}(x)
        @test_throws err MOI.add_constraint(model, f, set1)
        MOI.delete(model, ci)
    end
end

function set_upper_bound_twice(model::MOI.ModelLike, T::Type)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    f = x
    ub = zero(T)
    @test MOI.supports_constraint(model, MOI.VariableIndex, MOI.LessThan{T})
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
        ci = MOI.add_constraint(model, f, set1)
        err = MOI.UpperBoundAlreadySet{typeof(set1),typeof(set2)}(x)
        @test_throws err MOI.add_constraint(model, f, set2)
        MOI.delete(model, ci)
        ci = MOI.add_constraint(model, f, set2)
        err = MOI.UpperBoundAlreadySet{typeof(set2),typeof(set1)}(x)
        @test_throws err MOI.add_constraint(model, f, set1)
        MOI.delete(model, ci)
    end
end

function delete_test(model::MOI.ModelLike)
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
