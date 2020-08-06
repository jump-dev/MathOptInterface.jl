# include("C:/Users/joaquimgarcia/.julia/dev/MathOptInterface/test/Utilities/DoubleDicts.jl")
# include("C:/Users/joaquimgarcia/.julia/dev/MathOptInterface/src/Utilities/DoubleDicts.jl")

using MathOptInterface, Test
const MOI = MathOptInterface
const CI{F,S} = MOI.ConstraintIndex{F,S}

const DoubleDicts = MathOptInterface.Utilities.DoubleDicts

const CI_I = CI{MOI.SingleVariable, MOI.Integer}
const CI_B = CI{MOI.SingleVariable, MOI.ZeroOne}

function test_iterator(dict)
    kk, vv = [], []
    for (k,v) in dict
        push!(kk, k)
        push!(vv, v)
    end
    @test length(kk) == length(keys(dict))
    @test isempty(setdiff(kk, keys(dict)))
    @test length(vv) == length(values(dict))
    @test isempty(setdiff(vv, values(dict)))
end

function basic_functionality(dict, k_values, v_values)
    @test_throws ErrorException sizehint!(dict, 1)

    @test isempty(dict)
    @test length(dict) == 0

    dict[k_values[1]] = v_values[1]
    delete!(dict, k_values[1])
    test_iterator(dict)

    dict[k_values[3]] = v_values[3]
    test_iterator(dict)

    empty!(dict)
    @test isempty(dict)
    @test !haskey(dict, k_values[1])

    dict[k_values[1]] = v_values[1]
    @test haskey(dict, k_values[1])
    @test values(dict) == [v_values[1]]
    @test keys(dict) == [k_values[1]]
    @test dict[k_values[1]] == v_values[1]

    test_iterator(dict)

    delete!(dict, k_values[1])
    @test !haskey(dict, k_values[1])
    @test isempty(dict)

    dict[k_values[1]] = v_values[1]
    @test length(dict) == 1

    for (k,v) in zip(k_values, v_values)
        dict[k] = v
    end

    test_iterator(dict)

    empty!(dict)
    @test isempty(dict)
    @test length(dict) == 0

    dict[k_values[1]] = v_values[1]
    idict = DoubleDicts.with_type(dict, MOI.SingleVariable, MOI.Integer)
    bdict = DoubleDicts.with_type(dict, MOI.SingleVariable, MOI.ZeroOne)
    idict_ = dict[MOI.SingleVariable, MOI.Integer]
    @test idict.dict === idict_.dict
    sizehint!(idict, 2)
    @test length(idict) == 1
    @test length(bdict) == 0
    @test values(bdict) == typeof(v_values[3])[]
    @test keys(bdict) == typeof(k_values[3])[]

    @test haskey(idict, k_values[1])
    @test !haskey(idict, k_values[2])
    @test !haskey(bdict, k_values[3])

    @test values(idict) == [v_values[1]]
    @test keys(idict) == [k_values[1]]
    @test idict[k_values[1]] == v_values[1]

    idict[k_values[2]] = v_values[2]
    @test haskey(idict, k_values[2])
    test_iterator(idict)

    delete!(idict, k_values[2])
    @test !haskey(idict, k_values[2])
    test_iterator(idict)

    @test !isempty(idict)
    @test isempty(bdict)
    empty!(idict)
    empty!(bdict)
    @test isempty(idict)
    @test isempty(bdict)

    bdict[k_values[3]] = v_values[3]
    length(bdict) == 1

    edict = DoubleDicts.with_type(dict, MOI.SingleVariable, MOI.EqualTo{Bool})
    ek = CI{MOI.SingleVariable, MOI.EqualTo{Bool}}(1)
    delete!(edict, ek)
    @test_throws KeyError edict[ek]
    sizehint!(edict, 0)
    @test length(edict) == 0
    @test_throws KeyError edict[ek]
    delete!(edict, ek)
    test_iterator(edict)
end

@testset "DoubleDict" begin
    dict = DoubleDicts.DoubleDict{Float64}()
    keys = [
        CI_I(1),
        CI_I(2),
        CI_B(1),
    ]
    vals = [
        1.0,
        2.0,
        1.0,
    ]
    basic_functionality(dict, keys, vals)
end

@testset "IndexDoubleDict" begin
    dict = DoubleDicts.IndexDoubleDict()
    keys = [
        CI_I(1),
        CI_I(2),
        CI_B(1),
    ]
    vals = keys
    basic_functionality(dict, keys, vals)

    src = DoubleDicts.IndexDoubleDict()
    for (k,v) in zip(keys, vals)
        dict[k] = v
    end
    dest = DoubleDicts.IndexDoubleDict()
    MOIU._reverse_dict(dest, src)
    for (k, v) in src
        @test dest[v] == k
    end
end

@testset "FunctionSetDoubleDict" begin
    dict = DoubleDicts.FunctionSetDoubleDict()
    keys = [
        CI_I(1),
        CI_I(2),
        CI_B(1),
    ]
    vals = [
        (MOI.SingleVariable(MOI.VariableIndex(1)), MOI.Integer()),
        (MOI.SingleVariable(MOI.VariableIndex(2)), MOI.Integer()),
        (MOI.SingleVariable(MOI.VariableIndex(1)), MOI.ZeroOne()),
    ]
    basic_functionality(dict, keys, vals)
end