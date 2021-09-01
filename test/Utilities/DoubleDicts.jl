module TestDoubleDicts

using Test

using MathOptInterface
const MOI = MathOptInterface

const DoubleDicts = MathOptInterface.Utilities.DoubleDicts

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

function _test_iterator(dict)
    kk, vv = [], []
    for (k, v) in dict
        push!(kk, k)
        push!(vv, v)
    end
    @test length(kk) == length(keys(dict))
    @test isempty(setdiff(kk, keys(dict)))
    @test length(vv) == length(values(dict))
    @test isempty(setdiff(vv, values(dict)))
    @test dict == Dict(kk .=> vv)
    return
end

function _test_basic_functionality(dict, k_values, v_values)
    @test_throws ErrorException sizehint!(dict, 1)

    @test isempty(dict)
    @test length(dict) == 0
    _test_iterator(dict)

    dict[k_values[1]] = v_values[1]
    delete!(dict, k_values[1])
    _test_iterator(dict)

    dict[k_values[3]] = v_values[3]
    _test_iterator(dict)

    empty!(dict)
    @test isempty(dict)
    @test !haskey(dict, k_values[1])

    dict[k_values[1]] = v_values[1]
    @test haskey(dict, k_values[1])
    @test values(dict) == [v_values[1]]
    @test keys(dict) == [k_values[1]]
    @test dict[k_values[1]] == v_values[1]
    @test get(dict, k_values[1], nothing) == v_values[1]
    @test get(dict, k_values[1], v_values[2]) == v_values[1]
    for i in eachindex(k_values)
        if i != 1
            @test get(dict, k_values[i], nothing) === nothing
            # Test that the implementation does not only work with `nothing`
            @test get(dict, k_values[i], v_values[i]) == v_values[i]
        end
    end

    _test_iterator(dict)

    delete!(dict, k_values[1])
    @test !haskey(dict, k_values[1])
    for i in eachindex(k_values)
        @test get(dict, k_values[i], nothing) === nothing
        # Test that the implementation does not only work with `nothing`
        @test get(dict, k_values[i], v_values[i]) == v_values[i]
    end
    @test isempty(dict)

    dict[k_values[1]] = v_values[1]
    @test length(dict) == 1

    for (k, v) in zip(k_values, v_values)
        dict[k] = v
    end

    _test_iterator(dict)

    empty!(dict)
    @test isempty(dict)
    @test length(dict) == 0

    dict[k_values[1]] = v_values[1]
    idict = dict[MOI.VariableIndex, MOI.Integer]
    bdict = dict[MOI.VariableIndex, MOI.ZeroOne]
    idict_ = dict[MOI.VariableIndex, MOI.Integer]
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
    _test_iterator(idict)

    delete!(idict, k_values[2])
    @test !haskey(idict, k_values[2])
    _test_iterator(idict)

    @test !isempty(idict)
    @test isempty(bdict)
    empty!(idict)
    empty!(bdict)
    @test isempty(idict)
    @test isempty(bdict)

    bdict[k_values[3]] = v_values[3]
    length(bdict) == 1

    edict = dict[MOI.VariableIndex, MOI.EqualTo{Bool}]
    ek = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Bool}}(1)
    delete!(edict, ek)
    @test_throws KeyError edict[ek]
    sizehint!(edict, 0)
    @test length(edict) == 0
    @test_throws KeyError edict[ek]
    delete!(edict, ek)
    _test_iterator(edict)
    return
end

function test_DoubleDict()
    dict = DoubleDicts.DoubleDict{Float64}()
    keys = [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(1),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(2),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(1),
    ]
    vals = [1.0, 2.0, 1.0]
    _test_basic_functionality(dict, keys, vals)
    return
end

function test_IndexDoubleDict()
    dict = DoubleDicts.IndexDoubleDict()
    keys = [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(1),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(2),
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(1),
    ]
    vals = keys
    _test_basic_functionality(dict, keys, vals)
    src = DoubleDicts.IndexDoubleDict()
    for (k, v) in zip(keys, vals)
        dict[k] = v
    end
    dest = DoubleDicts.IndexDoubleDict()
    MOI.Utilities._reverse_dict(dest, src)
    for (k, v) in src
        @test dest[v] == k
    end
    return
end

end  # module

TestDoubleDicts.runtests()
