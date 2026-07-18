# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestFinalTouchOrder

using Test

import MathOptInterface as MOI

# The order in which `final_touch` is called on the bridges, as `(tag, id)`
# where `tag` identifies the bridge and `id` the constrained variable.
const RECORD = Tuple{Symbol,Int64}[]

# When `true`, `BridgeA` adds a `SetB` constraint in its `final_touch`. This is
# used to test that a bridge of a *new* type added while `final_touch` is being
# called still gets its own `final_touch`.
const ADD_B = Ref(false)

# When `true`, the next `BridgeA.final_touch` adds another `SetA` constraint (the
# flag is consumed so only one is added). This tests that a bridge of an
# *already-processed* type added while `final_touch` is being called still gets
# its own `final_touch` (MathOptInterface issue #1980).
const ADD_A = Ref(false)

struct SetA <: MOI.AbstractScalarSet end

struct SetB <: MOI.AbstractScalarSet end

# BridgeB: `VariableIndex`-in-`SetB` -> `VariableIndex`-in-`GreaterThan`.
struct BridgeB{T} <: MOI.Bridges.Constraint.AbstractBridge
    x::MOI.VariableIndex
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{T}}
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{BridgeB{T}},
    model::MOI.ModelLike,
    func::MOI.VariableIndex,
    ::SetB,
) where {T}
    ci = MOI.add_constraint(model, func, MOI.GreaterThan(zero(T)))
    return BridgeB{T}(func, ci)
end

function MOI.supports_constraint(
    ::Type{<:BridgeB},
    ::Type{MOI.VariableIndex},
    ::Type{SetB},
)
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:BridgeB{T}},
    ::Type{MOI.VariableIndex},
    ::Type{SetB},
) where {T}
    return BridgeB{T}
end

MOI.Bridges.added_constrained_variable_types(::Type{<:BridgeB}) = Tuple{Type}[]

function MOI.Bridges.added_constraint_types(::Type{BridgeB{T}}) where {T}
    return Tuple{Type,Type}[(MOI.VariableIndex, MOI.GreaterThan{T})]
end

function MOI.get(
    ::BridgeB{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}},
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::BridgeB{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.GreaterThan{T}},
) where {T}
    return [bridge.ci]
end

MOI.Bridges.needs_final_touch(::BridgeB) = true

function MOI.Bridges.final_touch(bridge::BridgeB, ::MOI.ModelLike)
    push!(RECORD, (:B, bridge.x.value))
    return
end

MOI.delete(model::MOI.ModelLike, bridge::BridgeB) = MOI.delete(model, bridge.ci)

# BridgeA: `VariableIndex`-in-`SetA`. It optionally adds a `VariableIndex`-in-
# `SetB` constraint in its `final_touch` (which is itself bridged by `BridgeB`).
mutable struct BridgeA{T} <: MOI.Bridges.Constraint.AbstractBridge
    x::MOI.VariableIndex
    ci::Union{Nothing,MOI.ConstraintIndex{MOI.VariableIndex,SetB}}
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{BridgeA{T}},
    ::MOI.ModelLike,
    func::MOI.VariableIndex,
    ::SetA,
) where {T}
    return BridgeA{T}(func, nothing)
end

function MOI.supports_constraint(
    ::Type{<:BridgeA},
    ::Type{MOI.VariableIndex},
    ::Type{SetA},
)
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:BridgeA{T}},
    ::Type{MOI.VariableIndex},
    ::Type{SetA},
) where {T}
    return BridgeA{T}
end

MOI.Bridges.added_constrained_variable_types(::Type{<:BridgeA}) = Tuple{Type}[]

function MOI.Bridges.added_constraint_types(::Type{<:BridgeA})
    return Tuple{Type,Type}[(MOI.VariableIndex, SetB)]
end

function MOI.get(
    bridge::BridgeA,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,SetB},
)::Int64
    return bridge.ci === nothing ? 0 : 1
end

function MOI.get(
    bridge::BridgeA,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,SetB},
)
    if bridge.ci === nothing
        return MOI.ConstraintIndex{MOI.VariableIndex,SetB}[]
    end
    return [bridge.ci]
end

MOI.Bridges.needs_final_touch(::BridgeA) = true

function MOI.Bridges.final_touch(bridge::BridgeA, model::MOI.ModelLike)
    push!(RECORD, (:A, bridge.x.value))
    if ADD_B[] && bridge.ci === nothing
        bridge.ci = MOI.add_constraint(model, bridge.x, SetB())
    end
    if ADD_A[]
        ADD_A[] = false  # consume the flag so only one constraint is added
        y = MOI.add_variable(model)
        MOI.add_constraint(model, y, SetA())
    end
    return
end

function MOI.delete(model::MOI.ModelLike, bridge::BridgeA)
    if bridge.ci !== nothing
        MOI.delete(model, bridge.ci)
    end
    return
end

# BridgeC: `VariableIndex`-in-`SetC`. Unlike `BridgeA`, it adds its `SetB`
# constraint in `bridge_constraint` (not `final_touch`), so `BridgeB` is created
# and registered *before* `BridgeC` (registration is post-order). Its
# `final_touch` must still be called *before* `BridgeB`'s, because `BridgeC`
# created the constraint that `BridgeB` bridges. This is the case that motivated
# ordering `final_touch` by creation order.
struct SetC <: MOI.AbstractScalarSet end

struct BridgeC{T} <: MOI.Bridges.Constraint.AbstractBridge
    x::MOI.VariableIndex
    ci::MOI.ConstraintIndex{MOI.VariableIndex,SetB}
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{BridgeC{T}},
    model::MOI.ModelLike,
    func::MOI.VariableIndex,
    ::SetC,
) where {T}
    ci = MOI.add_constraint(model, func, SetB())
    return BridgeC{T}(func, ci)
end

function MOI.supports_constraint(
    ::Type{<:BridgeC},
    ::Type{MOI.VariableIndex},
    ::Type{SetC},
)
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{<:BridgeC{T}},
    ::Type{MOI.VariableIndex},
    ::Type{SetC},
) where {T}
    return BridgeC{T}
end

MOI.Bridges.added_constrained_variable_types(::Type{<:BridgeC}) = Tuple{Type}[]

function MOI.Bridges.added_constraint_types(::Type{<:BridgeC})
    return Tuple{Type,Type}[(MOI.VariableIndex, SetB)]
end

function MOI.get(::BridgeC, ::MOI.NumberOfConstraints{MOI.VariableIndex,SetB})::Int64
    return 1
end

function MOI.get(
    bridge::BridgeC,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,SetB},
)
    return [bridge.ci]
end

MOI.Bridges.needs_final_touch(::BridgeC) = true

function MOI.Bridges.final_touch(bridge::BridgeC, ::MOI.ModelLike)
    push!(RECORD, (:C, bridge.x.value))
    return
end

MOI.delete(model::MOI.ModelLike, bridge::BridgeC) = MOI.delete(model, bridge.ci)

function _model()
    inner = MOI.Utilities.Model{Float64}()
    b = MOI.Bridges.LazyBridgeOptimizer(inner)
    MOI.Bridges.add_bridge(b, BridgeA{Float64})
    MOI.Bridges.add_bridge(b, BridgeB{Float64})
    MOI.Bridges.add_bridge(b, BridgeC{Float64})
    return b
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

# `final_touch` must be called in the order the bridges were added, not grouped
# by bridge type. Adding `A`, `B`, `A` must give the order `A, B, A` and not
# `A, A, B`.
function test_final_touch_in_addition_order()
    empty!(RECORD)
    ADD_B[] = false
    model = _model()
    x = MOI.add_variables(model, 3)
    MOI.add_constraint(model, x[1], SetA())
    MOI.add_constraint(model, x[2], SetB())
    MOI.add_constraint(model, x[3], SetA())
    MOI.Utilities.final_touch(model, MOI.Utilities.IndexMap())
    @test RECORD ==
          [(:A, x[1].value), (:B, x[2].value), (:A, x[3].value)]
    return
end

# A bridge must get its `final_touch` before the bridges of the constraints it
# created in its `bridge_constraint`, even though those are registered first
# (registration is post-order but `final_touch` follows creation order).
function test_final_touch_parent_before_child()
    empty!(RECORD)
    model = _model()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, SetC())
    MOI.Utilities.final_touch(model, MOI.Utilities.IndexMap())
    # `BridgeC` created the `SetB` constraint bridged by `BridgeB`, so
    # `BridgeC.final_touch` must run before `BridgeB.final_touch`.
    @test RECORD == [(:C, x.value), (:B, x.value)]
    return
end

# A bridge of a *new* type added while `final_touch` is being iterated must still
# get its `final_touch` called.
function test_final_touch_new_type_added_during_iteration()
    empty!(RECORD)
    ADD_B[] = true
    model = _model()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, SetA())
    MOI.Utilities.final_touch(model, MOI.Utilities.IndexMap())
    ADD_B[] = false
    # `BridgeA.final_touch` added a `SetB` constraint bridged by `BridgeB`; its
    # `final_touch` must have been called after `BridgeA`'s.
    @test RECORD == [(:A, x.value), (:B, x.value)]
    return
end

# A bridge of an *already-processed* type added while `final_touch` is being
# iterated must still get its `final_touch` called (MathOptInterface issue
# #1980).
function test_final_touch_same_type_added_during_iteration()
    empty!(RECORD)
    ADD_A[] = true
    model = _model()
    x = MOI.add_variable(model)
    MOI.add_constraint(model, x, SetA())
    MOI.Utilities.final_touch(model, MOI.Utilities.IndexMap())
    ADD_A[] = false
    # The first `BridgeA.final_touch` added a second `SetA` constraint (a bridge
    # of the same, already-processed type); it must also get its `final_touch`.
    @test length(RECORD) == 2
    @test all(r -> r[1] == :A, RECORD)
    @test RECORD[1][2] != RECORD[2][2]  # two different variables
    return
end

end  # module

TestFinalTouchOrder.runtests()
