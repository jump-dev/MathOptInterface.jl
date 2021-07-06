module TestCleverDicts

using MathOptInterface
using Test

const CleverDicts = MathOptInterface.Utilities.CleverDicts
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

# Note: `MyKey` is just for testing. You wouldn't want to use it in practice
# because the key type of the dictionary isn't a concrete type.
struct MyKey{X}
    MyKey(x) = new{Int64(x)}()
end

CleverDicts.key_to_index(::MyKey{X}) where {X} = X
CleverDicts.index_to_key(::Type{MyKey}, index::Int64) = MyKey(index)

function test_MyKey()
    d = CleverDicts.CleverDict{MyKey,String}()
    key = CleverDicts.add_item(d, "first")
    @test key == MyKey(1)
    @test d[MyKey(1)] == "first"
end

function test_Abstract_Value()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,Any}()
    key = CleverDicts.add_item(d, :a)
    @test key == MathOptInterface.VariableIndex(1)
    @test d[MathOptInterface.VariableIndex(1)] == :a
    key = CleverDicts.add_item(d, "b")
    @test key == MathOptInterface.VariableIndex(2)
    @test d[MathOptInterface.VariableIndex(2)] == "b"
    for (k, v) in d
        if k.value == 1
            @test v == :a
        else
            @test k.value == 2
            @test v == "b"
        end
    end
    return
end

function test_get_set()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    @test key == MathOptInterface.VariableIndex(1)
    @test get(d, key, nothing) == "first"
    @test get(d, MathOptInterface.VariableIndex(2), nothing) === nothing
    @test Dict(key => "first") == d
    @test Dict(key => "second") != d
    sizehint!(d, 1)
    @test d[key] == "first"
    @test haskey(d, key) == true
    @test_throws KeyError d[MathOptInterface.VariableIndex(2)]
    delete!(d, key)
    sizehint!(d, 2)
    @test_throws KeyError d[key]
    # set index is valid now
    # @test_throws KeyError d[key] = "key"
    @test haskey(d, key) == false
    key2 = CleverDicts.add_item(d, "second")
    @test key2 == MathOptInterface.VariableIndex(2)
    @test d[key2] == "second"
    d[key2] = "third"
    @test d[key2] == "third"
    @test get(d, key, nothing) === nothing
    @test get(d, key2, nothing) === "third"
    @test Dict(key2 => "second") != d
    @test Dict(key2 => "third") == d

    empty!(d)

    key = CleverDicts.add_item(d, "first")
    @test key == MathOptInterface.VariableIndex(1)
    @test d[key] == "first"
    d[key] = "zeroth"
    @test d[key] == "zeroth"
    @test haskey(d, key) == true
    @test_throws KeyError d[MathOptInterface.VariableIndex(2)]
    delete!(d, key)
    @test_throws KeyError d[key]
    # set index is valid now
    # @test_throws KeyError d[key] = "key"
    @test haskey(d, key) == false
    key2 = CleverDicts.add_item(d, "second")
    @test key2 == MathOptInterface.VariableIndex(2)
    @test d[key2] == "second"
    return
end

function test_LinearIndex()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    @test d[CleverDicts.LinearIndex(1)] == "first"
    key2 = CleverDicts.add_item(d, "second")
    @test d[CleverDicts.LinearIndex(2)] == "second"
    @test length(d) == 2
    delete!(d, key)
    @test d[CleverDicts.LinearIndex(1)] == "second"
    @test_throws KeyError d[CleverDicts.LinearIndex(2)]
    @test length(d) == 1
    return
end

function test_keys_values()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    key2 = CleverDicts.add_item(d, "second")
    @test collect(keys(d)) == [
        MathOptInterface.VariableIndex(1),
        MathOptInterface.VariableIndex(2),
    ]
    @test collect(values(d)) == ["first", "second"]
    delete!(d, key)
    key3 = CleverDicts.add_item(d, "third")
    @test collect(keys(d)) == [
        MathOptInterface.VariableIndex(2),
        MathOptInterface.VariableIndex(3),
    ]
    @test collect(values(d)) == ["second", "third"]
    return
end

function test_iterate()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    key2 = CleverDicts.add_item(d, "second")
    my_keys = MathOptInterface.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [
        MathOptInterface.VariableIndex(1),
        MathOptInterface.VariableIndex(2),
    ]
    @test my_values == ["first", "second"]
    delete!(d, key)
    key3 = CleverDicts.add_item(d, "third")
    my_keys = MathOptInterface.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [
        MathOptInterface.VariableIndex(2),
        MathOptInterface.VariableIndex(3),
    ]
    @test my_values == ["second", "third"]
    return
end

function test_iterate_ii()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    key2 = CleverDicts.add_item(d, "second")
    my_keys = MathOptInterface.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [
        MathOptInterface.VariableIndex(1),
        MathOptInterface.VariableIndex(2),
    ]
    @test my_values == ["first", "second"]
    delete!(d, key)
    @test d[CleverDicts.LinearIndex(1)] == "second"
    key3 = CleverDicts.add_item(d, "third")
    my_keys = MathOptInterface.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [
        MathOptInterface.VariableIndex(2),
        MathOptInterface.VariableIndex(3),
    ]
    @test my_values == ["second", "third"]
    return
end

function test_iterate_iii()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    y = 0
    for (k, v) in d
        y += 1
    end
    @test y == 0
    return
end

function test_haskey()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    @test !haskey(d, 1)
    k = CleverDicts.add_item(d, "a")
    @test haskey(d, k)
    j = CleverDicts.add_item(d, "b")
    @test haskey(d, j)
    delete!(d, k)
    @test !haskey(d, k)
    @test haskey(d, j)
    return
end

function test_isempty()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    @test isempty(d) == true
    k = CleverDicts.add_item(d, "a")
    @test isempty(d) == false
    delete!(d, k)
    @test isempty(d) == true
    j = CleverDicts.add_item(d, "b")
    @test isempty(d) == false
    return
end

function test_delete!()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    @test length(d) == 0
    @test delete!(d, MathOptInterface.VariableIndex(0)) == d
    k1 = CleverDicts.add_item(d, "a")
    k2 = CleverDicts.add_item(d, "b")
    d[CleverDicts.LinearIndex(2)] == "b"
    delete!(d, k1)
    d[CleverDicts.LinearIndex(1)] == "b"
    k3 = CleverDicts.add_item(d, "c")
    @test d[k3] == "c"
    @test d[CleverDicts.LinearIndex(1)] == "b"
    @test d[CleverDicts.LinearIndex(2)] == "c"
    return
end

function test_dense_operations()
    # inverse_hash :: Int64 -> Int
    inverse_hash(x::Int64) = Int(x * 2)
    # hash :: Int -> Int64
    hash(x::Int) = Int64(div(x, 2))
    d = CleverDicts.CleverDict{Int,Float64}(hash, inverse_hash, 3)

    d[4] = 0.25
    @test !haskey(d, 2)
    @test haskey(d, 4)
    @test !haskey(d, 6)
    @test length(d) == 1
    @test collect(d) == [4 => 0.25]
    @test d[4] == 0.25

    d[2] = 1.5
    @test haskey(d, 2)
    @test haskey(d, 4)
    @test !haskey(d, 6)
    @test length(d) == 2
    @test collect(d) == [2 => 1.5, 4 => 0.25]
    @test d[2] == 1.5
    @test d[4] == 0.25

    d[6] = 0.75
    @test haskey(d, 2)
    @test haskey(d, 4)
    @test haskey(d, 6)
    @test length(d) == 3
    @test collect(d) == [2 => 1.5, 4 => 0.25, 6 => 0.75]
    @test d[2] == 1.5
    @test d[4] == 0.25
    @test d[6] == 0.75
    return
end

function test_negative_index()
    d = CleverDicts.CleverDict{MathOptInterface.VariableIndex,String}()
    d[MathOptInterface.VariableIndex(-3)] = "a"
    @test d[MathOptInterface.VariableIndex(-3)] == "a"
    @test_throws ErrorException CleverDicts.add_item(d, "b")
    d[MathOptInterface.VariableIndex(0)] = "b"
    @test d[MathOptInterface.VariableIndex(-3)] == "a"
    @test d[MathOptInterface.VariableIndex(0)] == "b"
    @test_throws ErrorException CleverDicts.add_item(d, "c")
    return
end

function test_convert()
    vals = [MathOptInterface.VariableIndex(-i) for i in 1:10]
    d = Dict(MathOptInterface.VariableIndex(i) => vals[i] for i in 1:10)
    T = CleverDicts.CleverDict{
        MOI.VariableIndex,
        MOI.VariableIndex,
        typeof(CleverDicts.key_to_index),
        typeof(CleverDicts.index_to_key),
    }
    c = convert(T, d)
    @test c isa T
    @test c.is_dense
    @test c.last_index == 10
    @test c.vector == vals
    @test c === convert(T, c)
    return
end

end  # module

TestCleverDicts.runtests()
