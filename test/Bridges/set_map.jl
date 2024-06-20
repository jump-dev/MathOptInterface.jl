# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Test with a bridge for which the map is defined on the bridge value and not
# the bridge type
module TestSetMapBridge

using Test

import MathOptInterface as MOI

# Constraints `[f[2], f[1]]` if `swap` and otherwise `f` to `Nonnegatives`
struct SwapSet <: MOI.AbstractVectorSet
    swap::Bool
    invertible::Bool
end

MOI.dimension(::SwapSet) = 2

struct SwapBridge{T} <: MOI.Bridges.Constraint.SetMapBridge{
    T,
    MOI.Nonnegatives,
    SwapSet,
    MOI.VectorOfVariables,
    MOI.VectorOfVariables,
}
    constraint::MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
    set::SwapSet
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{SwapBridge{T}},
    model::MOI.ModelLike,
    func::MOI.VectorOfVariables,
    set::SwapSet,
) where {T}
    ci = MOI.add_constraint(
        model,
        MOI.VectorOfVariables(swap(func.variables, set.swap)),
        MOI.Nonnegatives(2),
    )
    return SwapBridge{T}(ci, set)
end

function MOI.Bridges.map_set(bridge::SwapBridge, set::SwapSet)
    if set.swap != bridge.set.swap
        error("Cannot change swap set")
    end
    return MOI.Nonnegatives(2)
end

MOI.Bridges.inverse_map_set(bridge::SwapBridge, ::MOI.Nonnegatives) = bridge.set

function MOI.Bridges.map_function(bridge::SwapBridge, func)
    return swap(func, bridge.set.swap)
end

function MOI.Bridges.inverse_map_function(bridge::SwapBridge, func)
    if !bridge.set.invertible
        throw(MOI.Bridges.MapNotInvertible("no luck"))
    end
    return swap(func, bridge.set.swap)
end

function MOI.Bridges.adjoint_map_function(bridge::SwapBridge, func)
    return swap(func, bridge.set.swap)
end

function MOI.Bridges.inverse_adjoint_map_function(bridge::SwapBridge, func)
    return swap(func, bridge.set.swap)
end

swap(x, swap::Bool) = swap ? [x[2], x[1]] : x

function swap(f::MOI.VectorOfVariables, do_swap::Bool)
    return MOI.VectorOfVariables(swap(f.variables, do_swap))
end

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

function test_set_set()
    model = MOI.Bridges.Constraint.SingleBridgeOptimizer{SwapBridge{Float64}}(
        MOI.Utilities.Model{Float64}(),
    )
    x = MOI.add_variables(model, 2)
    func = MOI.VectorOfVariables(x)
    ci = MOI.add_constraint(model, func, SwapSet(true, false))
    @test_throws(
        ErrorException("Cannot change swap set"),
        MOI.set(model, MOI.ConstraintSet(), ci, SwapSet(false, false)),
    )
    @test_throws(
        MOI.GetAttributeNotAllowed(
            MOI.ConstraintFunction(),
            "Cannot get MathOptInterface.ConstraintFunction() as the constraint is reformulated through a linear transformation that is not invertible.no luck no luck",
        ),
        MOI.get(model, MOI.ConstraintFunction(), ci),
    )
    return
end

function test_runtests()
    for do_swap in [false, true]
        MOI.Bridges.runtests(
            SwapBridge,
            model -> begin
                x = MOI.add_variables(model, 2)
                func = MOI.VectorOfVariables(x)
                set = SwapSet(do_swap, true)
                MOI.add_constraint(model, func, set)
            end,
            model -> begin
                x = MOI.add_variables(model, 2)
                func = MOI.VectorOfVariables(swap(x, do_swap))
                set = MOI.Nonnegatives(2)
                MOI.add_constraint(model, func, set)
            end,
        )
    end
    return
end

end  # module

TestSetMapBridge.runtests()
