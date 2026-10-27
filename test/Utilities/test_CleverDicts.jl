# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestCleverDicts

using Test

import MathOptInterface as MOI
import MathOptInterface.Utilities.CleverDicts

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
    d = CleverDicts.CleverDict{MOI.VariableIndex,Any}()
    key = CleverDicts.add_item(d, :a)
    @test key == MOI.VariableIndex(1)
    @test d[MOI.VariableIndex(1)] == :a
    key = CleverDicts.add_item(d, "b")
    @test key == MOI.VariableIndex(2)
    @test d[MOI.VariableIndex(2)] == "b"
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
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    @test key == MOI.VariableIndex(1)
    @test get(d, key, nothing) == "first"
    @test get(d, MOI.VariableIndex(2), nothing) === nothing
    @test Dict(key => "first") == d
    @test Dict(key => "second") != d
    sizehint!(d, 1)
    @test d[key] == "first"
    @test haskey(d, key) == true
    @test_throws KeyError d[MOI.VariableIndex(2)]
    delete!(d, key)
    sizehint!(d, 2)
    @test_throws KeyError d[key]
    # set index is valid now
    # @test_throws KeyError d[key] = "key"
    @test haskey(d, key) == false
    key2 = CleverDicts.add_item(d, "second")
    @test key2 == MOI.VariableIndex(2)
    @test d[key2] == "second"
    d[key2] = "third"
    @test d[key2] == "third"
    @test get(d, key, nothing) === nothing
    @test get(d, key2, nothing) === "third"
    @test Dict(key2 => "second") != d
    @test Dict(key2 => "third") == d

    empty!(d)

    key = CleverDicts.add_item(d, "first")
    @test key == MOI.VariableIndex(1)
    @test d[key] == "first"
    d[key] = "zeroth"
    @test d[key] == "zeroth"
    @test haskey(d, key) == true
    @test_throws KeyError d[MOI.VariableIndex(2)]
    delete!(d, key)
    @test_throws KeyError d[key]
    # set index is valid now
    # @test_throws KeyError d[key] = "key"
    @test haskey(d, key) == false
    key2 = CleverDicts.add_item(d, "second")
    @test key2 == MOI.VariableIndex(2)
    @test d[key2] == "second"
    return
end

function test_LinearIndex()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
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
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    key2 = CleverDicts.add_item(d, "second")
    @test collect(keys(d)) == [MOI.VariableIndex(1), MOI.VariableIndex(2)]
    @test collect(values(d)) == ["first", "second"]
    delete!(d, key)
    key3 = CleverDicts.add_item(d, "third")
    @test collect(keys(d)) == [MOI.VariableIndex(2), MOI.VariableIndex(3)]
    @test collect(values(d)) == ["second", "third"]
    return
end

function test_iterate()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    key2 = CleverDicts.add_item(d, "second")
    my_keys = MOI.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [MOI.VariableIndex(1), MOI.VariableIndex(2)]
    @test my_values == ["first", "second"]
    delete!(d, key)
    key3 = CleverDicts.add_item(d, "third")
    my_keys = MOI.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [MOI.VariableIndex(2), MOI.VariableIndex(3)]
    @test my_values == ["second", "third"]
    return
end

function test_iterate_2()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    key = CleverDicts.add_item(d, "first")
    key2 = CleverDicts.add_item(d, "second")
    my_keys = MOI.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [MOI.VariableIndex(1), MOI.VariableIndex(2)]
    @test my_values == ["first", "second"]
    delete!(d, key)
    @test d[CleverDicts.LinearIndex(1)] == "second"
    key3 = CleverDicts.add_item(d, "third")
    my_keys = MOI.VariableIndex[]
    my_values = String[]
    for (k, v) in d
        push!(my_keys, k)
        push!(my_values, v)
    end
    @test my_keys == [MOI.VariableIndex(2), MOI.VariableIndex(3)]
    @test my_values == ["second", "third"]
    return
end

function test_iterate_3()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    y = 0
    for (k, v) in d
        y += 1
    end
    @test y == 0
    return
end

function test_haskey()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
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
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
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
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    @test length(d) == 0
    @test delete!(d, MOI.VariableIndex(0)) == d
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

function test_negative_index()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    d[MOI.VariableIndex(-3)] = "a"
    @test d[MOI.VariableIndex(-3)] == "a"
    @test_throws ErrorException CleverDicts.add_item(d, "b")
    d[MOI.VariableIndex(0)] = "b"
    @test d[MOI.VariableIndex(-3)] == "a"
    @test d[MOI.VariableIndex(0)] == "b"
    @test_throws ErrorException CleverDicts.add_item(d, "c")
    return
end

function test_convert()
    vals = [MOI.VariableIndex(-i) for i in 1:10]
    d = Dict(MOI.VariableIndex(i) => vals[i] for i in 1:10)
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

function test_inverse_hash()
    d = CleverDicts.CleverDict{Int64,String}(identity, identity)
    key = CleverDicts.add_item(d, "a")
    @test d[key] == "a"
    return
end

function test_resize()
    d = CleverDicts.CleverDict{MOI.VariableIndex,String}()
    resize!(d, 1)
    @test length(d.vector) == 1
    x = CleverDicts.add_item(d, "a")
    @test_throws(
        ErrorException(
            "CleverDict cannot be resized to a size smaller than the current",
        ),
        resize!(d, 0),
    )
    delete!(d, x)
    resize!(d, 0)
    return
end

function test_identity()
    d = CleverDicts.CleverDict{MOI.VariableIndex,MOI.VariableIndex}()
    @test CleverDicts.is_identity(d)
    @test isempty(d)
    @test iterate(d) === nothing
    x = MOI.VariableIndex.(1:3)
    for xi in x
        d[xi] = xi
    end
    d[x[2]] = x[2]
    @test CleverDicts.is_identity(d)
    @test isempty(d.vector)
    @test isempty(d.dict)
    @test !isempty(d)
    @test length(d) == 3
    @test haskey(d, x[3])
    @test !haskey(d, MOI.VariableIndex(4))
    @test !haskey(d, MOI.VariableIndex(0))
    @test d[x[2]] == x[2]
    @test_throws KeyError(MOI.VariableIndex(4)) d[MOI.VariableIndex(4)]
    @test get(d, x[1], nothing) == x[1]
    @test get(d, MOI.VariableIndex(4), nothing) === nothing
    @test d[CleverDicts.LinearIndex(3)] == x[3]
    @test_throws KeyError(CleverDicts.LinearIndex(4)) d[CleverDicts.LinearIndex(
        4,
    )]
    @test collect(d) == [xi => xi for xi in x]
    @test collect(keys(d)) == x
    @test collect(values(d)) == x
    sizehint!(d, 10)
    @test CleverDicts.is_identity(d)
    @test CleverDicts.add_item(d, MOI.VariableIndex(4)) == MOI.VariableIndex(4)
    @test CleverDicts.is_identity(d)
    @test length(d) == 4
    empty!(d)
    @test CleverDicts.is_identity(d)
    @test isempty(d)
    return
end

function test_identity_not_same_types()
    d = CleverDicts.CleverDict{MOI.VariableIndex,Int}()
    @test !CleverDicts.is_identity(d)
    empty!(d)
    @test !CleverDicts.is_identity(d)
    return
end

function _identity_dict(n)
    d = CleverDicts.CleverDict{MOI.VariableIndex,MOI.VariableIndex}()
    for i in 1:n
        d[MOI.VariableIndex(i)] = MOI.VariableIndex(i)
    end
    @test CleverDicts.is_identity(d)
    return d
end

function _test_identity_materialized(d, n)
    @test !CleverDicts.is_identity(d)
    for i in 1:n
        x = MOI.VariableIndex(i)
        @test d[x] == x
    end
    return
end

function test_identity_set_not_identity()
    d = _identity_dict(3)
    x = MOI.VariableIndex(2)
    d[x] = MOI.VariableIndex(3)
    @test !CleverDicts.is_identity(d)
    @test d[x] == MOI.VariableIndex(3)
    @test d.vector == MOI.VariableIndex.([1, 3, 3])
    d = _identity_dict(3)
    x = MOI.VariableIndex(4)
    d[x] = MOI.VariableIndex(1)
    _test_identity_materialized(d, 3)
    @test d[x] == MOI.VariableIndex(1)
    @test length(d.vector) == 4
    return
end

function test_identity_set_out_of_order()
    d = _identity_dict(3)
    x = MOI.VariableIndex(5)
    d[x] = x
    _test_identity_materialized(d, 3)
    @test d[x] == x
    @test !CleverDicts._is_dense(d)
    @test length(d) == 4
    return
end

function test_identity_delete()
    d = _identity_dict(3)
    delete!(d, MOI.VariableIndex(2))
    @test !CleverDicts.is_identity(d)
    @test collect(keys(d)) == MOI.VariableIndex.([1, 3])
    @test d[CleverDicts.LinearIndex(2)] == MOI.VariableIndex(3)
    return
end

function test_identity_resize()
    d = _identity_dict(2)
    resize!(d, 3)
    _test_identity_materialized(d, 2)
    @test length(d.vector) == 3
    return
end

function test_identity_map_values()
    d = _identity_dict(2)
    CleverDicts.map_values!(x -> MOI.VariableIndex(x.value + 1), d)
    @test !CleverDicts.is_identity(d)
    @test d.vector == MOI.VariableIndex.([2, 3])
    return
end

end  # module

TestCleverDicts.runtests()
