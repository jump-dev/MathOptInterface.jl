# TODO: Move generic model tests from MOIU to here

struct UnknownSet <: MOI.AbstractSet end

function nametest(model::MOI.ModelLike)
    @testset "Name test" begin
        @test MOI.get(model, MOI.NumberOfVariables()) == 0
        @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0

        MOI.canaddvariable(model)
        v = MOI.addvariables!(model, 2)
        @test MOI.canget(model, MOI.VariableName(), typeof(v[1]))
        @test MOI.get(model, MOI.VariableName(), v[1]) == ""

        @test MOI.canset(model, MOI.VariableName(), typeof(v[1]))
        MOI.set!(model, MOI.VariableName(), v[1], "")
        MOI.set!(model, MOI.VariableName(), v[2], "") # Shouldn't error with duplicate empty name

        MOI.set!(model, MOI.VariableName(), v[1], "Var1")
        @test_throws Exception MOI.set!(model, MOI.VariableName(), v[2], "Var1")
        MOI.set!(model, MOI.VariableName(), v[2], "Var2")

        @test MOI.canget(model, MOI.VariableIndex, "Var1")
        @test !MOI.canget(model, MOI.VariableIndex, "Var3")

        @test MOI.get(model, MOI.VariableIndex, "Var1") == v[1]
        @test MOI.get(model, MOI.VariableIndex, "Var2") == v[2]
        @test_throws KeyError MOI.get(model, MOI.VariableIndex, "Var3")

        MOI.set!(model, MOI.VariableName(), v, ["VarX","Var2"])
        @test MOI.get(model, MOI.VariableName(), v) == ["VarX", "Var2"]

        if MOI.candelete(model, v[2])
            MOI.delete!(model, v[2])
            @test !MOI.canget(model, MOI.VariableIndex, "Var2")
            @test_throws KeyError MOI.get(model, MOI.VariableIndex, "Var2")
        end

        @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
        c = MOI.addconstraint!(model, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.LessThan(1.0))
        @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})
        c2 = MOI.addconstraint!(model, MOI.ScalarAffineFunction(v, [-1.0,1.0], 0.0), MOI.EqualTo(0.0))
        @test MOI.canget(model, MOI.ConstraintName(), typeof(c))
        @test MOI.get(model, MOI.ConstraintName(), c) == ""

        @test MOI.canset(model, MOI.ConstraintName(), typeof(c))
        MOI.set!(model, MOI.ConstraintName(), c, "")
        MOI.set!(model, MOI.ConstraintName(), c2, "") # Shouldn't error with duplicate empty name

        MOI.set!(model, MOI.ConstraintName(), c, "Con0")
        @test MOI.get(model, MOI.ConstraintName(), c) == "Con0"
        @test_throws Exception MOI.set!(model, MOI.ConstraintName(), c2, "Con0")

        MOI.set!(model, MOI.ConstraintName(), [c], ["Con1"])
        @test MOI.get(model, MOI.ConstraintName(), [c]) == ["Con1"]


        @test MOI.canget(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1")
        @test !MOI.canget(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con2")
        @test MOI.canget(model, MOI.ConstraintIndex, "Con1")
        @test !MOI.canget(model, MOI.ConstraintIndex, "Con2")

        @test MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1") == c
        @test_throws KeyError MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con2")
        @test MOI.get(model, MOI.ConstraintIndex, "Con1") == c
        @test_throws KeyError MOI.get(model, MOI.ConstraintIndex, "Con2")

        if MOI.candelete(model, c)
            MOI.delete!(model, c)
            @test !MOI.canget(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1")
            @test !MOI.canget(model, MOI.ConstraintIndex, "Con1")
            @test_throws KeyError MOI.get(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}, "Con1")
            @test_throws KeyError MOI.get(model, MOI.ConstraintIndex, "Con1")
        end
    end
end

# Taken from https://github.com/JuliaOpt/MathOptInterfaceUtilities.jl/issues/41
function validtest(model::MOI.ModelLike)
    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 2)
    @test MOI.isvalid(model, v[1])
    @test MOI.isvalid(model, v[2])
    MOI.canaddvariable(model)
    x = MOI.addvariable!(model)
    @test MOI.isvalid(model, x)
    MOI.delete!(model, x)
    @test !MOI.isvalid(model, x)
    cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
    @test MOI.canaddconstraint(model, typeof(cf), MOI.LessThan{Float64})
    c = MOI.addconstraint!(model, cf, MOI.LessThan(1.0))
    @test MOI.isvalid(model, c)
    @test !MOI.isvalid(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float32},MOI.LessThan{Float32}}(1))
    @test !MOI.isvalid(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float32},MOI.LessThan{Float64}}(1))
    @test !MOI.isvalid(model, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float32}}(1))
    @test !MOI.isvalid(model, MOI.ConstraintIndex{MOI.VectorQuadraticFunction{Float64},MOI.SecondOrderCone}(1))
end

function emptytest(model::MOI.ModelLike)
    # Taken from LIN1
    MOI.canaddvariable(model)
    v = MOI.addvariables!(model, 3)
    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Nonnegatives)
    vc = MOI.addconstraint!(model, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    @test MOI.canaddconstraint(model, MOI.VectorAffineFunction{Float64}, MOI.Zeros)
    c = MOI.addconstraint!(model, MOI.VectorAffineFunction([1,1,1,2,2], [v;v[2];v[3]], ones(5), [-3.0,-2.0]), MOI.Zeros(2))
    MOI.set!(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(v, [-3.0, -2.0, -4.0], 0.0))
    MOI.set!(model, MOI.ObjectiveSense(), MOI.MinSense)

    @test !MOI.isempty(model)

    MOI.empty!(model)

    @test MOI.isempty(model)

    @test MOI.get(model, MOI.NumberOfVariables()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonnegatives}()) == 0
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64},MOI.Zeros}()) == 0
    @test isempty(MOI.get(model, MOI.ListOfConstraints()))

    @test !MOI.isvalid(model, v[1])
    @test !MOI.isvalid(model, vc)
    @test !MOI.isvalid(model, c)
end

abstract type BadModel <: MOI.ModelLike end
MOI.canget(::BadModel, ::MOI.ListOfModelAttributesSet) = true
MOI.get(::BadModel, ::MOI.ListOfModelAttributesSet) = MOI.AbstractModelAttribute[]
MOI.get(::BadModel, ::MOI.NumberOfVariables) = 1
MOI.get(::BadModel, ::MOI.ListOfVariableIndices) = [MOI.VariableIndex(1)]
MOI.canget(::BadModel, ::MOI.ListOfVariableAttributesSet) = true
MOI.get(::BadModel, ::MOI.ListOfVariableAttributesSet) = MOI.AbstractVariableAttribute[]
MOI.get(::BadModel, ::MOI.ListOfConstraints) = [(MOI.SingleVariable, MOI.EqualTo{Float64})]
MOI.get(::BadModel, ::MOI.ListOfConstraintIndices{F,S}) where {F,S} = [MOI.ConstraintIndex{F,S}(1)]
MOI.get(::BadModel, ::MOI.ConstraintFunction, ::MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}) = MOI.SingleVariable(MOI.VariableIndex(1))
MOI.get(::BadModel, ::MOI.ConstraintSet, ::MOI.ConstraintIndex{MOI.SingleVariable,MOI.EqualTo{Float64}}) = MOI.EqualTo(0.0)
MOI.canget(::BadModel, ::MOI.ListOfConstraintAttributesSet) = true
MOI.get(::BadModel, ::MOI.ListOfConstraintAttributesSet) = MOI.AbstractConstraintAttribute[]

struct BadConstraintModel <: BadModel end
MOI.get(::BadConstraintModel, ::MOI.ListOfConstraints) = [(MOI.SingleVariable, MOI.EqualTo{Float64}), (MOI.SingleVariable, UnknownSet)]
MOI.get(::BadModel, ::MOI.ConstraintFunction, ::MOI.ConstraintIndex{MOI.SingleVariable,UnknownSet}) = MOI.SingleVariable(MOI.VariableIndex(1))
MOI.get(::BadModel, ::MOI.ConstraintSet, ::MOI.ConstraintIndex{MOI.SingleVariable,UnknownSet}) = UnknownSet()

struct BadModelAttribute <: MOI.AbstractModelAttribute end
struct BadModelAttributeModel <: BadModel end
MOI.canget(::BadModelAttributeModel, ::BadModelAttribute) = true
# During copy(dest, src::BadModelAttributeModel), canget(src, ...) will return true but canset(dest, ...) will return false.
# In this case, a correct implementation of copy shouldn't call get(src, ...) since the result will not be used as it won't do set!(dest, ...).
# If get(src::BadModelAttributeModel, ::BadModelAttribute) is defined here, a bad implementation of copy would pass the test.
# As it is not defined, the bad implementation will get UndefinedMethod
MOI.get(::BadModelAttributeModel, ::MOI.ListOfModelAttributesSet) = MOI.AbstractModelAttribute[BadModelAttribute()]

struct BadVariableAttribute <: MOI.AbstractVariableAttribute end
struct BadVariableAttributeModel <: BadModel end
MOI.canget(::BadVariableAttributeModel, ::BadVariableAttribute, ::Type{MOI.VariableIndex}) = true
# MOI.get is not defined for BadVariableAttribute for the same reason get is not defined BadModelAttribute
MOI.get(::BadVariableAttributeModel, ::MOI.ListOfVariableAttributesSet) = MOI.AbstractVariableAttribute[BadVariableAttribute()]

struct BadConstraintAttribute <: MOI.AbstractConstraintAttribute end
struct BadConstraintAttributeModel <: BadModel end
MOI.canget(::BadConstraintAttributeModel, ::BadConstraintAttribute, ::Type{<:MOI.ConstraintIndex}) = true
# MOI.get is not defined for BadConstraintAttribute for the same reason get is not defined BadModelAttribute
MOI.get(::BadConstraintAttributeModel, ::MOI.ListOfConstraintAttributesSet) = MOI.AbstractConstraintAttribute[BadConstraintAttribute()]

function failcopytest(dest::MOI.ModelLike, src::MOI.ModelLike, expected_status)
    copyresult = MOI.copy!(dest, src)
    @test copyresult.status == expected_status
end

function failcopytestc(dest::MOI.ModelLike)
    @test !MOI.supportsconstraint(dest, MOI.SingleVariable, UnknownSet)
    failcopytest(dest, BadConstraintModel(), MOI.CopyUnsupportedConstraint)
end
function failcopytestia(dest::MOI.ModelLike)
    @test !MOI.supports(dest, BadModelAttribute())
    failcopytest(dest, BadModelAttributeModel(), MOI.CopyUnsupportedAttribute)
end
function failcopytestva(dest::MOI.ModelLike)
    @test !MOI.supports(dest, BadVariableAttribute(), MOI.VariableIndex)
    failcopytest(dest, BadVariableAttributeModel(), MOI.CopyUnsupportedAttribute)
end
function failcopytestca(dest::MOI.ModelLike)
    @test !MOI.supports(dest, BadConstraintAttribute(), MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}})
    failcopytest(dest, BadConstraintAttributeModel(), MOI.CopyUnsupportedAttribute)
end

function copytest(dest::MOI.ModelLike, src::MOI.ModelLike)
    MOI.canaddvariable(src)
    v = MOI.addvariables!(src, 3)
    csv = MOI.addconstraint!(src, MOI.SingleVariable(v[2]), MOI.EqualTo(2.))
    cvv = MOI.addconstraint!(src, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
    csa = MOI.addconstraint!(src, MOI.ScalarAffineFunction([v[3], v[1]], [1., 3.], 2.), MOI.LessThan(2.))
    cva = MOI.addconstraint!(src, MOI.VectorAffineFunction([1, 2], [v[3], v[2]], ones(2), [-3.0,-2.0]), MOI.Zeros(2))
    MOI.set!(src, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(v, [-3.0, -2.0, -4.0], 0.0))
    MOI.set!(src, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.supports(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.supportsconstraint(dest, MOI.SingleVariable, MOI.EqualTo{Float64})
    @test MOI.supportsconstraint(dest, MOI.VectorOfVariables, MOI.Nonnegatives)
    @test MOI.supportsconstraint(dest, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})
    @test MOI.supportsconstraint(dest, MOI.VectorAffineFunction{Float64}, MOI.Zeros)

    copyresult = MOI.copy!(dest, src)
    @test copyresult.status == MOI.CopySuccess
    dict = copyresult.indexmap

    @test MOI.get(dest, MOI.NumberOfVariables()) == 3
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

    @test MOI.canget(dest, MOI.ConstraintFunction(), typeof(dict[csv]))
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csv]) == MOI.SingleVariable(dict[v[2]])
    @test MOI.canget(dest, MOI.ConstraintSet(), typeof(dict[csv]))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csv]) == MOI.EqualTo(2.)
    @test MOI.canget(dest, MOI.ConstraintFunction(), typeof(dict[cvv]))
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cvv]) == MOI.VectorOfVariables(getindex.(dict, v))
    @test MOI.canget(dest, MOI.ConstraintSet(), typeof(dict[cvv]))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cvv]) == MOI.Nonnegatives(3)
    @test MOI.canget(dest, MOI.ConstraintFunction(), typeof(dict[csa]))
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[csa]) ≈ MOI.ScalarAffineFunction([dict[v[3]], dict[v[1]]], [1., 3.], 2.)
    @test MOI.canget(dest, MOI.ConstraintSet(), typeof(dict[csa]))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[csa]) == MOI.LessThan(2.)
    @test MOI.canget(dest, MOI.ConstraintFunction(), typeof(dict[cva]))
    @test MOI.get(dest, MOI.ConstraintFunction(), dict[cva]) ≈ MOI.VectorAffineFunction([1, 2], [dict[v[3]], dict[v[2]]], ones(2), [-3.0,-2.0])
    @test MOI.canget(dest, MOI.ConstraintSet(), typeof(dict[cva]))
    @test MOI.get(dest, MOI.ConstraintSet(), dict[cva]) == MOI.Zeros(2)

    @test MOI.canget(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    @test MOI.get(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()) ≈ MOI.ScalarAffineFunction([dict[v[1]], dict[v[2]], dict[v[3]]], [-3.0, -2.0, -4.0], 0.0)
    @test MOI.canget(dest, MOI.ObjectiveSense())
    @test MOI.get(dest, MOI.ObjectiveSense()) == MOI.MinSense
end

function canaddconstrainttest(model::MOI.ModelLike, ::Type{GoodT}, ::Type{BadT}) where {GoodT, BadT}
    MOI.canaddvariable(model)
    v = MOI.addvariable!(model)
    @test MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{GoodT})
    @test MOI.canaddconstraint(model, MOI.ScalarAffineFunction{GoodT}, MOI.EqualTo{GoodT})
    # Bad type
    @test !MOI.canaddconstraint(model, MOI.ScalarAffineFunction{BadT}, MOI.EqualTo{GoodT})
    @test !MOI.canaddconstraint(model, MOI.ScalarAffineFunction{BadT}, MOI.EqualTo{BadT})
    @test !MOI.canaddconstraint(model, MOI.SingleVariable, MOI.EqualTo{BadT})

    @test MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.Zeros)
    @test !MOI.canaddconstraint(model, MOI.VectorOfVariables, MOI.EqualTo{GoodT}) # vector in scalar
    @test !MOI.canaddconstraint(model, MOI.SingleVariable, MOI.Zeros) # scalar in vector
    @test !MOI.canaddconstraint(model, MOI.VectorOfVariables, UnknownSet) # set not supported
end

"""
    orderedindicestest(model::MOI.ModelLike)

Test whether the model returns ListOfVariableIndices and ListOfConstraintIndices
sorted by creation time.
"""
function orderedindicestest(model::MOI.ModelLike)
    MOI.empty!(model)
    v1 = MOI.addvariable!(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v1]
    v2 = MOI.addvariable!(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v1, v2]
    MOI.delete!(model, v1)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v2]
    v3 = MOI.addvariable!(model)
    @test MOI.get(model, MOI.ListOfVariableIndices()) == [v2, v3]

    # Note: there are too many combinations to
    # test, so we're just going to check
    # SingleVariable-in-LessThan and hope it
    # works for the rest
    c1 = MOI.addconstraint!(model, MOI.SingleVariable(v2), MOI.LessThan(1.0))
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c1]
    c2 = MOI.addconstraint!(model, MOI.SingleVariable(v2), MOI.LessThan(2.0))
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c1, c2]
    MOI.delete!(model, c1)
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c2]
    c3 = MOI.addconstraint!(model, MOI.SingleVariable(v2), MOI.LessThan(3.0))
    @test MOI.get(model, MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.LessThan{Float64}}()) == [c2, c3]
end
