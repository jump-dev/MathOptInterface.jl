using MathOptInterface, Test

const CleverDicts = MathOptInterface.Utilities.CleverDicts

# Note: `MyKey` is just for testing. You wouldn't want to use it in practice
# because the key type of the dictionary isn't a concrete type.
struct MyKey{X} end
CleverDicts.key_to_index(::MyKey{X}) where {X} = X
CleverDicts.index_to_key(::Type{MyKey}, index::Int) = MyKey{index}()

@testset "CleverDict" begin
    @testset "MyKey type" begin
        d = CleverDicts.CleverDict{MyKey, String}()
        key = CleverDicts.add_item(d, "first")
        @test key == MyKey{1}()
        @test d[MyKey{1}()] == "first"
    end

    @testset "Abstract Value" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, Any}()
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
    end

    @testset "get/set" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
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
    end

    @testset "LinearIndex" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        key = CleverDicts.add_item(d, "first")
        @test d[CleverDicts.LinearIndex(1)] == "first"
        key2 = CleverDicts.add_item(d, "second")
        @test d[CleverDicts.LinearIndex(2)] == "second"
        @test length(d) == 2
        delete!(d, key)
        @test d[CleverDicts.LinearIndex(1)] == "second"
        @test_throws KeyError d[CleverDicts.LinearIndex(2)]
        @test length(d) == 1
    end

    @testset "keys/values" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        key = CleverDicts.add_item(d, "first")
        key2 = CleverDicts.add_item(d, "second")
        @test collect(keys(d)) == [MathOptInterface.VariableIndex(1), MathOptInterface.VariableIndex(2)]
        @test collect(values(d)) == ["first", "second"]
        delete!(d, key)
        key3 = CleverDicts.add_item(d, "third")
        @test collect(keys(d)) == [MathOptInterface.VariableIndex(2), MathOptInterface.VariableIndex(3)]
        @test collect(values(d)) == ["second", "third"]
    end

    @testset "iterate" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        key = CleverDicts.add_item(d, "first")
        key2 = CleverDicts.add_item(d, "second")
        my_keys = MathOptInterface.VariableIndex[]
        my_values = String[]
        for (k, v) in d
           push!(my_keys, k)
           push!(my_values, v)
        end
        @test my_keys == [MathOptInterface.VariableIndex(1), MathOptInterface.VariableIndex(2)]
        @test my_values == ["first", "second"]
        delete!(d, key)
        key3 = CleverDicts.add_item(d, "third")
        my_keys = MathOptInterface.VariableIndex[]
        my_values = String[]
        for (k, v) in d
           push!(my_keys, k)
           push!(my_values, v)
        end
        @test my_keys == [MathOptInterface.VariableIndex(2), MathOptInterface.VariableIndex(3)]
        @test my_values == ["second", "third"]
    end

    @testset "iterate ii" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        key = CleverDicts.add_item(d, "first")
        key2 = CleverDicts.add_item(d, "second")
        my_keys = MathOptInterface.VariableIndex[]
        my_values = String[]
        for (k, v) in d
            push!(my_keys, k)
            push!(my_values, v)
        end
        @test my_keys == [MathOptInterface.VariableIndex(1), MathOptInterface.VariableIndex(2)]
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
        @test my_keys == [MathOptInterface.VariableIndex(2), MathOptInterface.VariableIndex(3)]
        @test my_values == ["second", "third"]
    end

    @testset "iterate iii" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        y = 0
        for (k, v) in d
            y += 1
        end
        @test y == 0
    end

    @testset "haskey" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        @test !haskey(d, 1)
        k = CleverDicts.add_item(d, "a")
        @test haskey(d, k)
        j = CleverDicts.add_item(d, "b")
        @test haskey(d, j)
        delete!(d, k)
        @test !haskey(d, k)
        @test haskey(d, j)
    end

    @testset "haskey" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        @test isempty(d) == true
        k = CleverDicts.add_item(d, "a")
        @test isempty(d) == false
        delete!(d, k)
        @test isempty(d) == true
        j = CleverDicts.add_item(d, "b")
        @test isempty(d) == false
    end

    @testset "delete!" begin
        d = CleverDicts.CleverDict{MathOptInterface.VariableIndex, String}()
        @test length(d) == 0
        @test delete!(d, MathOptInterface.VariableIndex(0)) == nothing
        k1 = CleverDicts.add_item(d, "a")
        k2 = CleverDicts.add_item(d, "b")
        d[CleverDicts.LinearIndex(2)] == "b"
        delete!(d, k1)
        d[CleverDicts.LinearIndex(1)] == "b"
        k3 = CleverDicts.add_item(d, "c")
        @test d[k3] == "c"
        @test d[CleverDicts.LinearIndex(1)] == "b"
        @test d[CleverDicts.LinearIndex(2)] == "c"
    end

    @testset "Dense Operations" begin
        mul2(x) = x * 2
        div2(x) = div(x, 2)
        d = CleverDicts.CleverDict{Int, Float64}(div2, mul2, 3)

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
    end
end
