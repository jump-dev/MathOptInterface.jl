# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

using BenchmarkTools
import MathOptInterface as MOI
import MathOptInterface.Utilities as MOIU

# Model similar to SDPA format, it gives a good example because it does not
# support a lot hence need a lot of bridges
MOIU.@model(
    SDPAModel,
    (),
    (MOI.EqualTo,),
    (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)
function MOI.supports_constraint(
    ::SDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.GreaterThan{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::SDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.LessThan{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::SDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.EqualTo{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::SDPAModel{T},
    ::Type{MOI.VariableIndex},
    ::Type{MOI.Interval{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::SDPAModel,
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.Reals},
)
    return false
end

function MOI.supports(
    ::SDPAModel{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}},
) where {T}
    return false
end
MOI.supports(::SDPAModel, ::MOI.ObjectiveFunction{MOI.VariableIndex}) = false

function interval_constraint()
    model = SDPAModel{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    F = MOI.ScalarAffineFunction{Float64}
    S = MOI.Interval{Float64}
    MOI.Bridges.bridge_index(bridged, F, S)
    display(@benchmark begin
        MOI.Bridges._reset_bridge_graph($bridged)
        MOI.Bridges.node($bridged, $F, $S)
    end)
    return display(@benchmark begin
        MOI.Bridges._reset_bridge_graph($bridged)
        MOI.Bridges.bridge_index($bridged, $F, $S)
    end)
end

interval_constraint()

function quadratic_objective()
    model = SDPAModel{Float64}()
    bridged = MOI.Bridges.full_bridge_optimizer(model, Float64)
    F = MOI.ScalarQuadraticFunction{Float64}
    MOI.Bridges.bridge_index(bridged, F)
    display(@benchmark begin
        MOI.Bridges._reset_bridge_graph($bridged)
        MOI.Bridges.node($bridged, $F)
    end)
    return display(@benchmark begin
        MOI.Bridges._reset_bridge_graph($bridged)
        MOI.Bridges.bridge_index($bridged, $F)
    end)
end

display(quadratic_objective())
