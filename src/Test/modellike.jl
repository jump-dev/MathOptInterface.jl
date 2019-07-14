# TODO: Move generic model tests from MOIU to here

struct UnknownScalarSet <: MOI.AbstractScalarSet end
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

function nametest(model::MOI.ModelLike)
    @testset "Name test with $(typeof(model))" begin
        MOI.empty!(model)
        @test MOIU.supports_default_copy_to(model, #=copy_names=# true)
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
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0

        @test MOI.supports(model, MOI.VariableName(), MOI.VariableIndex)
        v = MOI.add_variables(model, 2)
        @test MOI.get(model, MOI.VariableName(), v[1]) == ""

        MOI.set(model, MOI.VariableName(), v[1], "")
        MOI.set(model, MOI.VariableName(), v[2], "") # Shouldn't error with duplicate empty name

        MOI.set(model, MOI.VariableName(), v[1], "Var1")
        MOI.set(model, MOI.VariableName(), v[2], "Var1")
        # Lookup must fail when there are multiple variables with the same name.
        @test_throws Exception MOI.get(model, MOI.VariableIndex, "Var1")

        MOI.set(model, MOI.VariableName(), v[2], "Var2")

        @test MOI.get(model, MOI.VariableIndex, "Var1") == v[1]
        @test MOI.get(model, MOI.VariableIndex, "Var2") == v[2]
        @test MOI.get(model, MOI.VariableIndex, "Var3") === nothing

        MOI.set(model, MOI.VariableName(), v, ["VarX","Var2"])
        @test MOI.get(model, MOI.VariableName(), v) == ["VarX", "Var2"]

        @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
        c = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0), MOI.LessThan(1.0))
        @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
        c2 = MOI.add_constraint(model, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-1.0,1.0], v), 0.0), MOI.EqualTo(0.0))
        @test MOI.get(model, MOI.ConstraintName(), c) == ""

        @test MOI.supports(model, MOI.ConstraintName(), typeof(c))
        MOI.set(model, MOI.ConstraintName(), c, "")
        @test MOI.supports(model, MOI.ConstraintName(), typeof(c2))
        MOI.set(model, MOI.ConstraintName(), c2, "") # Shouldn't error with duplicate empty name

        MOI.set(model, MOI.ConstraintName(), c, "Con0")
        @test MOI.get(model, MOI.ConstraintName(), c) == "Con0"
        MOI.set(model, MOI.ConstraintName(), c2, "Con0")
        # Lookup must fail when multiple constraints have the same name.
        @test_throws Exception MOI.get(model, MOI.ConstraintIndex, "Con0")
        @test_throws Exception MOI.get(model,
                                       MOI.ConstraintIndex{
                                        MOI.ScalarAffineFunction{Float64},
                                        MOI.LessThan{Float64}},
                                       "Con0")

        MOI.set(model, MOI.ConstraintName(), [c], ["Con1"])
        @test MOI.get(model, MOI.ConstraintName(), [c]) == ["Con1"]

        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1") == c
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}, "Con1") === nothing
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}, "Con1") === nothing
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con2") === nothing
        @test MOI.get(model, MOI.ConstraintIndex, "Con1") == c
        @test MOI.get(model, MOI.ConstraintIndex, "Con2") === nothing

        MOI.set(model, MOI.ConstraintName(), c2, "Con0")

        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con0") === nothing
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}, "Con1") === nothing
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1") == c
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}, "Con0") == c2
        @test MOI.get(model, MOI.ConstraintIndex, "Con0") == c2
        @test MOI.get(model, MOI.ConstraintIndex, "Con1") == c

        MOI.delete(model, v[2])
        @test MOI.get(model, MOI.VariableIndex, "Var2") === nothing

        MOI.delete(model, c)
        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1") === nothing
        @test MOI.get(model, MOI.ConstraintIndex, "Con1") === nothing
    end
end

# Taken from https://github.com/JuliaOpt/MathOptInterfaceUtilities.jl/issues/41
function validtest(model::MOI.ModelLike)
    MOI.empty!(model)
    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    v = MOI.add_variables(model, 2)
    @test MOI.is_valid(model, v[1])
    @test MOI.is_valid(model, v[2])
    x = MOI.add_variable(model)
    @test MOI.is_valid(model, x)
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0,1.0], v), 0.0)
    @test MOI.supports_constraint(model, typeof(cf), MOI.LessThan{Float64})
    c = MOI.add_constraint(model, cf, MOI.LessThan(1.0))
    @test MOI.is_valid(model, c)
    @test !MOI.is_valid(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float32},MOI.LessThan{Float32}}(1))
    @test !MOI.is_valid(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float32},MOI.LessThan{Float64}}(1))
    @test !MOI.is_valid(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float32}}(1))
    @test !MOI.is_valid(model, MOI.ConstraintIndex{MOI.VectorQuadraticFunction{Float64},MOI.SecondOrderCone}(1))
end

function emptytest(model::MOI.ModelLike)
    MOI.empty!(model)
    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
    # Taken from LIN1
    v = MOI.add_variables(model, 3)
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
    vc = MOI.add_constraint(model, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    c = MOI.add_constraint(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1,1,1,2,2], MOI.ScalarAffineTerm.(1.0, [v;v[2];v[3]])), [-3.0, -2.0]), MOI.Zeros(2))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    @test !MOI.is_empty(model)

    MOI.empty!(model)

    @test MOI.is_empty(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 0
    @test isempty(MOI.get(model, MOI.ListOfConstraints()))

    @test !MOI.is_valid(model, v[1])
    @test !MOI.is_valid(model, vc)
    @test !MOI.is_valid(model, c)
end

abstract type BadModel <: MOI.ModelLike end
MOI.get(::BadModel, ::MOI.ListOfModelAttributesSet) = MOI.AbstractModelAttribute[]
MOI.get(::BadModel, ::MOI.NumberOfVariables) = 1
MOI.get(::BadModel, ::MOI.ListOfVariableIndices) = [MOI.VariableIndex(1)]
MOI.get(::BadModel, ::MOI.ListOfVariableAttributesSet) = MOI.AbstractVariableAttribute[]
MOI.get(::BadModel, ::MOI.ListOfConstraints) = [(MOI.SingleVariable, MOI.EqualTo{Float64})]
MOI.get(::BadModel, ::MOI.ListOfConstraintIndices{F,S}) where {F,S} = [MOI.ConstraintIndex{F,S}(1)]
MOI.get(::BadModel, ::MOI.ConstraintFunction, ::MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}) = MOI.SingleVariable(MOI.VariableIndex(1))
MOI.get(::BadModel, ::MOI.ConstraintSet, ::MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}) = MOI.EqualTo(0.0)
MOI.get(::BadModel, ::MOI.ListOfConstraintAttributesSet) = MOI.AbstractConstraintAttribute[]

struct BadConstraintModel <: BadModel end
MOI.get(::BadConstraintModel, ::MOI.ListOfConstraints) = [(MOI.SingleVariable, MOI.EqualTo{Float64}), (MOI.SingleVariable, UnknownScalarSet)]
MOI.get(::BadModel, ::MOI.ConstraintFunction, ::MOI.ConstraintIndex{MOI.SingleVariable,UnknownScalarSet}) = MOI.SingleVariable(MOI.VariableIndex(1))
MOI.get(::BadModel, ::MOI.ConstraintSet, ::MOI.ConstraintIndex{MOI.SingleVariable,UnknownScalarSet}) = UnknownScalarSet()

struct UnknownModelAttribute <: MOI.AbstractModelAttribute end
struct BadModelAttributeModel <: BadModel end
MOI.get(src::BadModelAttributeModel, ::UnknownModelAttribute) = 0
MOI.get(::BadModelAttributeModel, ::MOI.ListOfModelAttributesSet) = MOI.AbstractModelAttribute[UnknownModelAttribute()]

struct UnknownVariableAttribute <: MOI.AbstractVariableAttribute end
struct BadVariableAttributeModel <: BadModel end
MOI.get(::BadVariableAttributeModel, ::UnknownVariableAttribute, ::MOI.VariableIndex) = 0
MOI.get(::BadVariableAttributeModel, ::MOI.ListOfVariableAttributesSet) = MOI.AbstractVariableAttribute[UnknownVariableAttribute()]

struct UnknownConstraintAttribute <: MOI.AbstractConstraintAttribute end
struct BadConstraintAttributeModel <: BadModel end
MOI.get(::BadConstraintAttributeModel, ::UnknownConstraintAttribute, ::MOI.ConstraintIndex) = 0
MOI.get(::BadConstraintAttributeModel, ::MOI.ListOfConstraintAttributesSet) = MOI.AbstractConstraintAttribute[UnknownConstraintAttribute()]

function failcopytestc(dest::MOI.ModelLike)
    @test !MOI.supports_constraint(dest, MOI.SingleVariable, UnknownScalarSet)
    @test_throws MOI.UnsupportedConstraint MOI.copy_to(dest, BadConstraintModel())
end
function failcopytestia(dest::MOI.ModelLike)
    @test !MOI.supports(dest, UnknownModelAttribute())
    @test_throws MOI.UnsupportedAttribute MOI.copy_to(dest, BadModelAttributeModel())
end
function failcopytestva(dest::MOI.ModelLike)
    @test !MOI.supports(dest, UnknownVariableAttribute(), MOI.VariableIndex)
    @test_throws MOI.UnsupportedAttribute MOI.copy_to(dest, BadVariableAttributeModel())
end
function failcopytestca(dest::MOI.ModelLike)
    @test !MOI.supports(dest, UnknownConstraintAttribute(), MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}})
    @test_throws MOI.UnsupportedAttribute MOI.copy_to(dest, BadConstraintAttributeModel())
end

function start_values_test(dest::MOI.ModelLike, src::MOI.ModelLike)
    MOI.empty!(dest)
    @test MOIU.supports_default_copy_to(src, #=copy_names=# false)
    x, y, z = MOI.add_variables(src, 3)
    a = MOI.add_constraint(src, x, MOI.EqualTo(1.0))
    b = MOI.add_constraint(src, y, MOI.EqualTo(2.0))
    c = MOI.add_constraint(src, z, MOI.EqualTo(3.0))
    F = MOI.SingleVariable
    S = MOI.EqualTo{Float64}

    vpattr = MOI.VariablePrimalStart()
    cpattr = MOI.ConstraintPrimalStart()
    cdattr = MOI.ConstraintDualStart()

    @testset "Supports" begin
        @test MOI.supports(dest, vpattr, MOI.VariableIndex)
        @test MOI.supports(dest, cpattr, MOI.ConstraintIndex{F, S})
        @test MOI.supports(dest, cdattr, MOI.ConstraintIndex{F, S})
    end

    @testset "Attribute set to no indices" begin
        dict = MOI.copy_to(dest, src, copy_names=false)

        @test !(vpattr in MOI.get(dest, MOI.ListOfVariableAttributesSet()))
        @test MOI.get(dest, vpattr, dict[x]) === nothing
        @test MOI.get(dest, vpattr, dict[y]) === nothing
        @test MOI.get(dest, vpattr, dict[z]) === nothing
        @test !(cpattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F, S}()))
        @test MOI.get(dest, cpattr, dict[a]) === nothing
        @test MOI.get(dest, cpattr, dict[b]) === nothing
        @test MOI.get(dest, cpattr, dict[c]) === nothing
        @test !(cdattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F, S}()))
        @test MOI.get(dest, cdattr, dict[a]) === nothing
        @test MOI.get(dest, cdattr, dict[b]) === nothing
        @test MOI.get(dest, cdattr, dict[c]) === nothing
    end

    @testset "Attribute set to some indices" begin
        MOI.set(src, vpattr, x, 1.0)
        MOI.set(src, vpattr, z, 3.0)
        MOI.set(src, cpattr, a, 1.0)
        MOI.set(src, cpattr, b, 2.0)
        MOI.set(src, cdattr, b, 2.0)
        MOI.set(src, cdattr, c, 3.0)

        dict = MOI.copy_to(dest, src, copy_names=false)

        @test vpattr in MOI.get(dest, MOI.ListOfVariableAttributesSet())
        @test MOI.get(dest, vpattr, dict[x]) == 1.0
        @test MOI.get(dest, vpattr, dict[y]) === nothing
        @test MOI.get(dest, vpattr, dict[z]) == 3.0
        @test cpattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F, S}())
        @test MOI.get(dest, cpattr, dict[a]) == 1.0
        @test MOI.get(dest, cpattr, dict[b]) == 2.0
        @test MOI.get(dest, cpattr, dict[c]) === nothing
        @test cdattr in MOI.get(dest, MOI.ListOfConstraintAttributesSet{F, S}())
        @test MOI.get(dest, cdattr, dict[a]) === nothing
        @test MOI.get(dest, cdattr, dict[b]) == 2.0
        @test MOI.get(dest, cdattr, dict[c]) == 3.0
    end
end

function copytest(dest::MOI.ModelLike, src::MOI.ModelLike)
    @test MOIU.supports_default_copy_to(src, #=copy_names=# true)
    MOI.empty!(src)
    MOI.empty!(dest)
    MOI.set(src, MOI.Name(), "ModelName")
    v = MOI.add_variables(src, 3)
    w = MOI.add_variable(src)
    MOI.set(src, MOI.VariableName(), v, ["var1", "var2", "var3"])
    csv = MOI.add_constraint(src, MOI.SingleVariable(w), MOI.EqualTo(2.))
    # We test this after the creation of every `SingleVariable` constraint
    # to ensure a good coverage of corner cases.
    @test csv.value == w.value
    MOI.set(src, MOI.ConstraintName(), csv, "csv")
    cvv = MOI.add_constraint(src, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    MOI.set(src, MOI.ConstraintName(), cvv, "cvv")
    csa = MOI.add_constraint(
        src,
        MOI.ScalarAffineFunction(
            MOI.ScalarAffineTerm.([1.0, 3.0], [v[3], v[1]]), 0.0),
        MOI.LessThan(2.))
    MOI.set(src, MOI.ConstraintName(), csa, "csa")
    cva = MOI.add_constraint(
        src,
        MOI.VectorAffineFunction(
            MOI.VectorAffineTerm.(
                [1, 2],
                MOI.ScalarAffineTerm.(1.0, [v[3], v[2]])),
            [-3.0, -2.0]),
        MOI.Zeros(2))
    MOI.set(src, MOI.ConstraintName(), cva, "cva")
    MOI.set(src, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            MOI.ScalarAffineFunction(
                MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v), 2.0))
    MOI.set(src, MOI.ObjectiveSense(), MOI.MIN_SENSE)

    @test MOI.supports(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports_constraint(dest, MOI.SingleVariable, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(dest, MOI.VectorOfVariables, MOI.Nonnegatives)
    @test MOI.supports_constraint(dest, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(dest, MOI.VectorAffineFunction{Float64}, MOI.Zeros)

    dict = MOI.copy_to(dest, src, copy_names=false)

    @test !MOI.supports(dest, MOI.Name()) || MOI.get(dest, MOI.Name()) == ""
    @test MOI.get(dest, MOI.NumberOfVariables()) == 4
    if MOI.supports(dest, MOI.VariableName(), MOI.VariableIndex)
        for vi in v
            MOI.get(dest, MOI.VariableName(), dict[vi]) == ""
        end
    end
    @test MOI.get(dest, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(dest, MOI.ListOfConstraintIndices{MOI.SingleVariable,MOI.EqualTo{Float64}}()) == [dict[csv]]
    @test MOI.get(dest, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()) == 1
    @test MOI.get(dest, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.Nonnegatives}()) == [dict[cvv]]
    @test MOI.get(dest, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
    @test MOI.get(dest, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == [dict[csa]]
    @test MOI.get(dest, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 1
    @test MOI.get(dest, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == [dict[cva]]
    loc = MOI.get(dest, MOI.ListOfConstraints())
    @test length(loc) == 4
    @test (MOI.SingleVariable,MOI.EqualTo{Float64}) in loc
    @test (MOI.VectorOfVariables,MOI.Nonnegatives) in loc
    @test (MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}) in loc
    @test (MOI.VectorAffineFunction{Float64},MOI.Zeros) in loc

    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(csv)) || MOI.get(dest, MOI.ConstraintName(), dict[csv]) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csv]) == MOI.SingleVariable(dict[w])
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csv]) == MOI.EqualTo(2.)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(cvv)) || MOI.get(dest, MOI.ConstraintName(), dict[cvv]) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cvv]) == MOI.VectorOfVariables(getindex.(Ref(dict), v))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cvv]) == MOI.Nonnegatives(3)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(csa)) || MOI.get(dest, MOI.ConstraintName(), dict[csa]) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csa]) ≈ MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1., 3.], [dict[v[3]], dict[v[1]]]), 0.0)
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csa]) == MOI.LessThan(2.)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(cva)) || MOI.get(dest, MOI.ConstraintName(), dict[cva]) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cva]) ≈ MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [dict[v[3]], dict[v[2]]])), [-3.0, -2.0])
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cva]) == MOI.Zeros(2)

    @test MOI.get(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()) ≈ MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0],
                              [dict[v[1]], dict[v[2]], dict[v[3]]]), 2.0)
    @test MOI.get(dest, MOI.ObjectiveFunctionType()) == MOI.ScalarAffineFunction{Float64}
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MIN_SENSE
end

function supports_constrainttest(model::MOI.ModelLike, ::Type{GoodT}, ::Type{BadT}) where {GoodT, BadT}
    v = MOI.add_variable(model)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{GoodT})
    @test MOI.supports_constraint(model, MOI.ScalarAffineFunction{GoodT}, MOI.EqualTo{GoodT})
    # Bad type
    @test !MOI.supports_constraint(model, MOI.ScalarAffineFunction{BadT}, MOI.EqualTo{GoodT})
    @test !MOI.supports_constraint(model, MOI.ScalarAffineFunction{BadT}, MOI.EqualTo{BadT})
    @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.EqualTo{BadT})

    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Zeros)
    @test !MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.EqualTo{GoodT}) # vector in scalar
    @test !MOI.supports_constraint(model, MOI.SingleVariable, MOI.Zeros) # scalar in vector
    @test !MOI.supports_constraint(model, MOI.VectorOfVariables, UnknownVectorSet) # set not supported
end

"""
    orderedindicestest(model::MOI.ModelLike)

Test whether the model returns ListOfVariableIndices and ListOfConstraintIndices
sorted by creation time.
"""
function orderedindicestest(model::MOI.ModelLike)
    @test MOIU.supports_default_copy_to(model, #=copy_names=# false)
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
    # check SingleVariable-in-LessThan and hope it works for the rest
    c1 = MOI.add_constraint(model, MOI.SingleVariable(v2), MOI.LessThan(1.0))
    @test c1.value == v2.value
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c1]
    c2 = MOI.add_constraint(model, MOI.SingleVariable(v3), MOI.LessThan(2.0))
    @test c2.value == v3.value
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c1, c2]
    MOI.delete(model, c1)
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c2]
    c3 = MOI.add_constraint(model, MOI.SingleVariable(v4), MOI.LessThan(3.0))
    @test c3.value == v4.value
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c2, c3]
end

# Test that `MOI.ScalarFunctionConstantNotZero` is thrown when a constraint with
# a function with nonzero constant is added. This test is optional because
# solvers could choose to support scalar functions with nonzero constants.
function scalar_function_constant_not_zero(model::MOI.ModelLike)
    @testset "Constraint with nonzero function constant" begin
        err = MOI.ScalarFunctionConstantNotZero{Float64,
               MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}(1.0)
        @test_throws err begin
            MOI.add_constraint(model,
                 MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 1.0),
                 MOI.EqualTo(2.0))
        end
        err = MOI.ScalarFunctionConstantNotZero{Float64,
               MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}}(2.0)
        @test_throws err begin
            MOI.add_constraint(model,
                 MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{Float64}[], 2.0),
                 MOI.GreaterThan(1.0))
        end
    end
end

function set_lower_bound_twice(model::MOI.ModelLike, T::Type)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    f = MOI.SingleVariable(x)
    lb = zero(T)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{T})
    sets = [MOI.EqualTo(lb), MOI.Interval(lb, lb),
             MOI.Semicontinuous(lb, lb), MOI.Semiinteger(lb, lb)]
    set2 = MOI.GreaterThan(lb)
    for set1 in sets
        if !MOI.supports_constraint(model, MOI.SingleVariable, typeof(set1))
            continue
        end
        ci = MOI.add_constraint(model, f, set1)
        err = MOI.LowerBoundAlreadySet{typeof(set1), typeof(set2)}(x)
        @test_throws err MOI.add_constraint(model, f, set2)
        MOI.delete(model, ci)
        ci = MOI.add_constraint(model, f, set2)
        err = MOI.LowerBoundAlreadySet{typeof(set2), typeof(set1)}(x)
        @test_throws err MOI.add_constraint(model, f, set1)
        MOI.delete(model, ci)
    end
end

function set_upper_bound_twice(model::MOI.ModelLike, T::Type)
    MOI.empty!(model)
    @test MOI.is_empty(model)
    x = MOI.add_variable(model)
    f = MOI.SingleVariable(x)
    ub = zero(T)
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.LessThan{T})
    sets = [MOI.EqualTo(ub), MOI.Interval(ub, ub),
             MOI.Semicontinuous(ub, ub), MOI.Semiinteger(ub, ub)]
    set2 = MOI.LessThan(ub)
    for set1 in sets
        if !MOI.supports_constraint(model, MOI.SingleVariable, typeof(set1))
            continue
        end
        ci = MOI.add_constraint(model, f, set1)
        err = MOI.UpperBoundAlreadySet{typeof(set1), typeof(set2)}(x)
        @test_throws err MOI.add_constraint(model, f, set2)
        MOI.delete(model, ci)
        ci = MOI.add_constraint(model, f, set2)
        err = MOI.UpperBoundAlreadySet{typeof(set2), typeof(set1)}(x)
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
    @test MOI.get(model, MOI.ConstraintFunction(), cx) == MOI.SingleVariable(x)
    @test MOI.get(model, MOI.ConstraintSet(), cx) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) == MOI.VectorOfVariables(y)
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(4)
    MOI.delete(model, y[3])
    @test MOI.is_valid(model, x)
    @test MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cx) == MOI.SingleVariable(x)
    @test MOI.get(model, MOI.ConstraintSet(), cx) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) == MOI.VectorOfVariables(y[[1, 2, 4]])
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(3)
    MOI.delete(model, y[1])
    @test MOI.is_valid(model, x)
    @test !MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cx) == MOI.SingleVariable(x)
    @test MOI.get(model, MOI.ConstraintSet(), cx) == MOI.GreaterThan(0.0)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) == MOI.VectorOfVariables(y[[2, 4]])
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(2)
    MOI.delete(model, x)
    @test !MOI.is_valid(model, x)
    @test !MOI.is_valid(model, y[1])
    @test MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test MOI.is_valid(model, y[4])
    @test !MOI.is_valid(model, cx)
    @test MOI.is_valid(model, cy)
    @test MOI.get(model, MOI.ConstraintFunction(), cy) == MOI.VectorOfVariables(y[[2, 4]])
    @test MOI.get(model, MOI.ConstraintSet(), cy) == MOI.Nonpositives(2)
    MOI.delete(model, y[[2, 4]])
    @test !MOI.is_valid(model, x)
    @test !MOI.is_valid(model, y[1])
    @test !MOI.is_valid(model, y[2])
    @test !MOI.is_valid(model, y[3])
    @test !MOI.is_valid(model, y[4])
    @test !MOI.is_valid(model, cx)
    @test !MOI.is_valid(model, cy)
end
