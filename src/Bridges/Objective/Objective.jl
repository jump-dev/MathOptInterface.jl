# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Objective

using MathOptInterface

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

include("bridge.jl")
include("map.jl")
include("single_bridge_optimizer.jl")

include("bridges/functionize.jl")
include("bridges/slack.jl")

"""
    add_all_bridges(model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Objective` submodule to `model`.

The coefficient type used is `T`.
"""
function add_all_bridges(model, ::Type{T}) where {T}
    MOI.Bridges.add_bridge(model, FunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, SlackBridge{T})
    return
end

end
