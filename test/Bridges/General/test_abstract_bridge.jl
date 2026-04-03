# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Test with a bridge for which the map is defined on the bridge value and not
# the bridge type
module TestAbstractBridge

using Test

import MathOptInterface as MOI

# A minimal constraint bridge that doesn't implement ConstraintFunction getter.
# Used to test that `cannot_unbridge = true` works with the default bridge
# `MOI.get` (which throws `GetAttributeNotAllowed`).
struct _NoUnbridgeTestBridge{T} <: MOI.Bridges.Constraint.AbstractBridge
    ci::MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}
end

function MOI.Bridges.Constraint.bridge_constraint(
    ::Type{_NoUnbridgeTestBridge{T}},
    model::MOI.ModelLike,
    f::MOI.ScalarAffineFunction{T},
    ::MOI.GreaterThan{T},
) where {T}
    ci = MOI.add_constraint(model, f, MOI.EqualTo(zero(T)))
    return _NoUnbridgeTestBridge{T}(ci)
end

function MOI.supports_constraint(
    ::Type{_NoUnbridgeTestBridge{T}},
    ::Type{MOI.ScalarAffineFunction{T}},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    return true
end

function MOI.Bridges.Constraint.concrete_bridge_type(
    ::Type{_NoUnbridgeTestBridge{T}},
    ::Type{MOI.ScalarAffineFunction{T}},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    return _NoUnbridgeTestBridge{T}
end

function MOI.Bridges.added_constraint_types(
    ::Type{_NoUnbridgeTestBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})]
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:_NoUnbridgeTestBridge},
)
    return Tuple{Type}[]
end

function MOI.get(
    ::_NoUnbridgeTestBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return 1
end

function MOI.get(
    bridge::_NoUnbridgeTestBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return [bridge.ci]
end

function MOI.delete(
    model::MOI.ModelLike,
    bridge::_NoUnbridgeTestBridge,
)
    MOI.delete(model, bridge.ci)
    return
end

# Note: ConstraintFunction and ConstraintSet getters are NOT implemented.
# The default MOI.get on AbstractBridge throws GetAttributeNotAllowed.

function test_cannot_unbridge_with_default_getter()
    # This test verifies that `cannot_unbridge = true` works when a constraint
    # bridge does not implement `ConstraintFunction` getter. Before the fix,
    # the default bridge `MOI.get` threw `ArgumentError` which was not caught
    # by `_runtests_error_handler`.
    MOI.Bridges.runtests(
        _NoUnbridgeTestBridge,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(model, 1.0 * x, MOI.GreaterThan(0.0))
        end,
        model -> begin
            x = MOI.add_variable(model)
            MOI.add_constraint(model, 1.0 * x, MOI.EqualTo(0.0))
        end;
        cannot_unbridge = true,
    )
    return
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

end

TestAbstractBridge.runtests()
