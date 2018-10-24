# TODO: Move generic model tests from MOIU to here

struct UnknownSet <: MOI.AbstractSet end

function nametest(model::MOI.ModelLike)
    @testset "Name test" begin
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
        @test_throws Exception begin
            # An implementation may detect duplicate names wither when they're
            # set or on lookup.
            MOI.set(model, MOI.VariableName(), v[2], "Var1")
            MOI.get(model, MOI.VariableIndex, "Var1")
        end
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
        @test_throws Exception begin
            # An implementation may detect duplicate names wither when they're
            # set or on lookup.
            MOI.set(model, MOI.ConstraintName(), c2, "Con0")
            MOI.get(model, MOI.ConstraintIndex, "Con1")
        end

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
    # Taken from LIN1
    v = MOI.add_variables(model, 3)
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
    vc = MOI.add_constraint(model, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    @test MOI.supports_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    c = MOI.add_constraint(model, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1,1,1,2,2], MOI.ScalarAffineTerm.(1.0, [v;v[2];v[3]])), [-3.0,-2.0]), MOI.Zeros(2))
    MOI.set(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v), 0.0))
    MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)

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
MOI.get(::BadConstraintModel, ::MOI.ListOfConstraints) = [(MOI.SingleVariable, MOI.EqualTo{Float64}), (MOI.SingleVariable, UnknownSet)]
MOI.get(::BadModel, ::MOI.ConstraintFunction, ::MOI.ConstraintIndex{MOI.SingleVariable,UnknownSet}) = MOI.SingleVariable(MOI.VariableIndex(1))
MOI.get(::BadModel, ::MOI.ConstraintSet, ::MOI.ConstraintIndex{MOI.SingleVariable,UnknownSet}) = UnknownSet()

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
    @test !MOI.supports_constraint(dest, MOI.SingleVariable, UnknownSet)
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

function copytest(dest::MOI.ModelLike, src::MOI.ModelLike)
    MOI.set(src, MOI.Name(), "ModelName")
    v = MOI.add_variables(src, 3)
    w = MOI.add_variable(src)
    MOI.set(src, MOI.VariableName(), v, ["var1", "var2", "var3"])
    csv = MOI.add_constraint(src, MOI.SingleVariable(w), MOI.EqualTo(2.))
    MOI.set(src, MOI.ConstraintName(), csv, "csv")
    cvv = MOI.add_constraint(src, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    MOI.set(src, MOI.ConstraintName(), cvv, "cvv")
    csa = MOI.add_constraint(src, MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1., 3.], [v[3], v[1]]), 2.), MOI.LessThan(2.))
    MOI.set(src, MOI.ConstraintName(), csa, "csa")
    cva = MOI.add_constraint(src, MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [v[3], v[2]])), [-3.0,-2.0]), MOI.Zeros(2))
    MOI.set(src, MOI.ConstraintName(), cva, "cva")
    MOI.set(src, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], v), 0.0))
    MOI.set(src, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.supports(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supports_constraint(dest, MOI.SingleVariable, MOI.EqualTo{Float64})
    @test MOI.supports_constraint(dest, MOI.VectorOfVariables, MOI.Nonnegatives)
    @test MOI.supports_constraint(dest, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supports_constraint(dest, MOI.VectorAffineFunction{Float64}, MOI.Zeros)

    dict = MOI.copy_to(dest, src, copy_names=false)

    @test !MOI.supports(dest, MOI.Name()) || MOI.get(dest, MOI.Name()) == ""
    @test MOI.get(dest, MOI.NumberOfVariables()) == 4
    @test !MOI.supports(dest, MOI.VariableName(), MOI.VariableIndex) || MOI.get(dest, MOI.VariableName(), v) == ["", "", ""]
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

    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(csv)) || MOI.get(dest, MOI.ConstraintName(), csv) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csv]) == MOI.SingleVariable(dict[w])
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csv]) == MOI.EqualTo(2.)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(cvv)) || MOI.get(dest, MOI.ConstraintName(), cvv) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cvv]) == MOI.VectorOfVariables(getindex.(Ref(dict), v))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cvv]) == MOI.Nonnegatives(3)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(csa)) || MOI.get(dest, MOI.ConstraintName(), csa) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csa]) ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1., 3.], [dict[v[3]], dict[v[1]]]), 2.)
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csa]) == MOI.LessThan(2.)
    @test !MOI.supports(dest, MOI.ConstraintName(), typeof(cva)) || MOI.get(dest, MOI.ConstraintName(), cva) == ""
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cva]) ≈ MOI.VectorAffineFunction(MOI.VectorAffineTerm.([1, 2], MOI.ScalarAffineTerm.(1.0, [dict[v[3]], dict[v[2]]])), [-3.0,-2.0])
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cva]) == MOI.Zeros(2)

    @test MOI.get(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()) ≈ MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([-3.0, -2.0, -4.0], [dict[v[1]], dict[v[2]], dict[v[3]]]), 0.0)
    @test MOI.get(dest, MOI.ObjectiveFunctionType()) == MOI.ScalarAffineFunction{Float64}
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MinSense
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
    @test !MOI.supports_constraint(model, MOI.VectorOfVariables, UnknownSet) # set not supported
end

"""
    orderedindicestest(model::MOI.ModelLike)

Test whether the model returns ListOfVariableIndices and ListOfConstraintIndices
sorted by creation time.
"""
function orderedindicestest(model::MOI.ModelLike)
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
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c1]
    c2 = MOI.add_constraint(model, MOI.SingleVariable(v3), MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c1, c2]
    MOI.delete(model, c1)
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c2]
    c3 = MOI.add_constraint(model, MOI.SingleVariable(v4), MOI.LessThan(3.0))
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c2, c3]
end
