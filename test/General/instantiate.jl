# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestInstantiate

using Test
import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$name", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
end

struct DummyOptimizer <: MOI.AbstractOptimizer end

MOI.is_empty(::DummyOptimizer) = true

function MOI.default_cache(::DummyOptimizer, ::Type{T}) where {T}
    return MOI.Utilities.Model{T}()
end

function _test_instantiate(T)
    function f()
        return MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
        )
    end
    optimizer_constructor =
        MOI.OptimizerWithAttributes(f, MOI.Silent() => true, "a" => 1, "b" => 2)
    optimizer = MOI.instantiate(optimizer_constructor)
    @test optimizer isa MOI.Utilities.MockOptimizer{
        MOI.Utilities.UniversalFallback{MOI.Utilities.Model{T}},
        Float64,
    }
    @test MOI.get(optimizer, MOI.Silent())
    optimizer = MOI.instantiate(optimizer_constructor, with_bridge_type = T)
    @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{
        MOI.Utilities.MockOptimizer{
            MOI.Utilities.UniversalFallback{MOI.Utilities.Model{T}},
            Float64,
        },
    }
    @test MOI.get(optimizer, MOI.Silent())
    @test MOI.get(optimizer, MOI.RawOptimizerAttribute("a")) == 1
    @test MOI.get(optimizer, MOI.RawOptimizerAttribute("b")) == 2

    optimizer_constructor = MOI.OptimizerWithAttributes(DummyOptimizer, [])
    optimizer = MOI.instantiate(optimizer_constructor)
    @test optimizer isa DummyOptimizer
    optimizer = MOI.instantiate(optimizer_constructor, with_bridge_type = T)
    @test optimizer isa MOI.Bridges.LazyBridgeOptimizer{
        MOI.Utilities.CachingOptimizer{DummyOptimizer,MOI.Utilities.Model{T}},
    }
    err = ErrorException(
        "The provided `optimizer_constructor` returned a non-empty optimizer.",
    )
    function g()
        model = f()
        MOI.add_variable(model)
        return model
    end
    @test_throws err MOI.instantiate(g)
    @test_throws err MOI.instantiate(g, with_bridge_type = T)
    optimizer_constructor = MOI.OptimizerWithAttributes(g)
    @test_throws err MOI.instantiate(optimizer_constructor)
    @test_throws err MOI.instantiate(
        optimizer_constructor,
        with_bridge_type = T,
    )

    err = ErrorException(MOI._INSTANTIATE_NOT_CALLABLE_MESSAGE)
    @test_throws err MOI.OptimizerWithAttributes(1)
    @test_throws err MOI.OptimizerWithAttributes(1, MOI.Silent() => true)
    @test_throws err MOI.instantiate(1)
    @test_throws err MOI.instantiate(1, with_bridge_type = T)

    err = ErrorException(
        "The provided `optimizer_constructor` returned an object of type " *
        "$Int. Expected a MathOptInterface.ModelLike.",
    )
    h() = 1
    @test_throws err MOI.instantiate(h)
    @test_throws err MOI.instantiate(h, with_bridge_type = T)
    optimizer_constructor = MOI.OptimizerWithAttributes(h)
    @test_throws err MOI.instantiate(optimizer_constructor)
    @test_throws err MOI.instantiate(
        optimizer_constructor,
        with_bridge_type = T,
    )
end

function test_instantiate_Float64()
    _test_instantiate(Float64)
    return
end

function test_instantiate_Int()
    _test_instantiate(Int)
    return
end

function test_get_set_Optimizer_with_attributes()
    opt = MOI.OptimizerWithAttributes() do
        return MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    end
    @test MOI.get(opt, MOI.Silent()) === nothing
    MOI.set(opt, MOI.Silent(), true)
    @test MOI.get(opt, MOI.Silent()) == true
    @test MOI.get(opt, MOI.RawOptimizerAttribute("a")) === nothing
    MOI.set(opt, MOI.RawOptimizerAttribute("a"), 1.0)
    @test MOI.get(opt, MOI.RawOptimizerAttribute("a")) == 1.0
    MOI.set(opt, MOI.RawOptimizerAttribute("a"), 2.0)
    @test MOI.get(opt, MOI.RawOptimizerAttribute("a")) == 2.0
    return
end

function test_instantiate_with_cache_type()
    function f(::Type{T}) where {T}
        return MOI.Utilities.MockOptimizer(
            MOI.Utilities.UniversalFallback(MOI.Utilities.Model{T}()),
        )
    end
    function inner_type(::Type{T}) where {T}
        return MOI.Utilities.UniversalFallback{MOI.Utilities.Model{T}}
    end
    function mock_type(::Type{T}) where {T}
        return MOI.Utilities.MockOptimizer{inner_type(T),Float64}
    end
    for T in (Int, Float64)
        mock, inner = mock_type(T), inner_type(T)
        # Check with no arguments provided
        model = MOI.instantiate(() -> f(T))
        @test typeof(model) == mock
        # Check with only with_bridge_type
        model = MOI.instantiate(() -> f(T); with_bridge_type = T)
        @test typeof(model) == MOI.Bridges.LazyBridgeOptimizer{mock}
        # Check with only with_cache_type
        model = MOI.instantiate(() -> f(T); with_cache_type = T)
        @test typeof(model) == MOI.Utilities.CachingOptimizer{mock,inner}
        # Check with both with_cache_type and with_bridge_type
        model = MOI.instantiate(; with_cache_type = T, with_bridge_type = T) do
            return f(T)
        end
        @test typeof(model) == MOI.Bridges.LazyBridgeOptimizer{
            MOI.Utilities.CachingOptimizer{mock,inner},
        }
    end
    @test_throws(
        ErrorException(
            "If both provided, `with_bridge_type` and `with_cache_type` must " *
            "be the same type. Got `with_bridge_type = $Float64` and " *
            "`with_cache_type = $Int`",
        ),
        MOI.instantiate(; with_cache_type = Int, with_bridge_type = Float64) do
            return f(Int)
        end
    )
    return
end

function test_to_param_OptimizerWithAttributes()
    @test_throws(
        ErrorException(
            "Expected an optimizer attribute or a string, got `1` which is a `$(Int)`.",
        ),
        MOI.OptimizerWithAttributes(MOI.Utilities.Model{Float64}, 1 => 2),
    )
    return
end

end

TestInstantiate.runtests()
