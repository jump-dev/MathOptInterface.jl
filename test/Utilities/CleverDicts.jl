using MathOptInterface, Test

const CleverDicts = MathOptInterface.Utilities.CleverDicts

struct MyKey
    x::Int
end

CleverDicts.key_to_index(key::MyKey) = key.x
CleverDicts.index_to_key(::Type{MyKey}, index::Int) = MyKey(index)

@testset "CleverDict" begin
    @testset "get/set" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        key = CleverDicts.new_item(d, "first")
        @test key == MyKey(1)
        @test d[key] == "first"
        @test haskey(d, key) == true
        @test_throws KeyError d[MyKey(2)]
        delete!(d, key)
        @test_throws KeyError d[key]
        @test_throws KeyError d[key] = "key"
        @test haskey(d, key) == false
        key2 = CleverDicts.new_item(d, "second")
        @test key2 == MyKey(2)
        @test d[key2] == "second"
        @test d.vector === nothing
        @test d.dict !== nothing
        d[key2] = "third"
        @test d[key2] == "third"

        empty!(d)

        key = CleverDicts.new_item(d, "first")
        @test key == MyKey(1)
        @test d[key] == "first"
        d[key] = "zeroth"
        @test d[key] == "zeroth"
        @test haskey(d, key) == true
        @test_throws KeyError d[MyKey(2)]
        delete!(d, key)
        @test_throws KeyError d[key]
        @test_throws KeyError d[key] = "key"
        @test haskey(d, key) == false
        key2 = CleverDicts.new_item(d, "second")
        @test key2 == MyKey(2)
        @test d[key2] == "second"
        @test d.vector === nothing
        @test d.dict !== nothing
    end

    @testset "LinearIndex" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        key = CleverDicts.new_item(d, "first")
        @test d[CleverDicts.LinearIndex(1)] == "first"
        key2 = CleverDicts.new_item(d, "second")
        @test d[CleverDicts.LinearIndex(2)] == "second"
        @test length(d) == 2
        delete!(d, key)
        @test d.vector === nothing
        @test d[CleverDicts.LinearIndex(1)] == "second"
        @test_throws KeyError d[CleverDicts.LinearIndex(2)]
        @test length(d) == 1
        @test d.vector !== nothing
    end

    @testset "keys/values" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        key = CleverDicts.new_item(d, "first")
        key2 = CleverDicts.new_item(d, "second")
        @test collect(keys(d)) == [MyKey(1), MyKey(2)]
        @test collect(values(d)) == ["first", "second"]
        delete!(d, key)
        key3 = CleverDicts.new_item(d, "third")
        @test collect(keys(d)) == [MyKey(2), MyKey(3)]
        @test collect(values(d)) == ["second", "third"]
    end

    @testset "iterate" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        key = CleverDicts.new_item(d, "first")
        key2 = CleverDicts.new_item(d, "second")
        my_keys = MyKey[]
        my_values = String[]
        for (k, v) in d
           push!(my_keys, k)
           push!(my_values, v)
        end
        @test my_keys == [MyKey(1), MyKey(2)]
        @test my_values == ["first", "second"]
        delete!(d, key)
        key3 = CleverDicts.new_item(d, "third")
        my_keys = MyKey[]
        my_values = String[]
        for (k, v) in d
           push!(my_keys, k)
           push!(my_values, v)
        end
        @test my_keys == [MyKey(2), MyKey(3)]
        @test my_values == ["second", "third"]
    end

    @testset "iterate ii" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        key = CleverDicts.new_item(d, "first")
        key2 = CleverDicts.new_item(d, "second")
        my_keys = MyKey[]
        my_values = String[]
        for (k, v) in d
            push!(my_keys, k)
            push!(my_values, v)
        end
        @test my_keys == [MyKey(1), MyKey(2)]
        @test my_values == ["first", "second"]
        delete!(d, key)
        @test d[CleverDicts.LinearIndex(1)] == "second"
        key3 = CleverDicts.new_item(d, "third")
        my_keys = MyKey[]
        my_values = String[]
        for (k, v) in d
            push!(my_keys, k)
            push!(my_values, v)
        end
        @test my_keys == [MyKey(2), MyKey(3)]
        @test my_values == ["second", "third"]
    end

    @testset "delete!" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        @test length(d) == 0
        @test delete!(d, MyKey(0)) == nothing
        k1 = CleverDicts.new_item(d, "a")
        k2 = CleverDicts.new_item(d, "b")
        d[CleverDicts.LinearIndex(2)] == "b"
        delete!(d, k1)
        d[CleverDicts.LinearIndex(1)] == "b"
        k3 = CleverDicts.new_item(d, "c")
        @test d[k3] == "c"
        @test d[CleverDicts.LinearIndex(1)] == "b"
        @test d[CleverDicts.LinearIndex(2)] == "c"
    end
end
