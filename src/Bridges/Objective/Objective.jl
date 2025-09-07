# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Objective

import MathOptInterface as MOI

include("bridge.jl")
include("map.jl")
include("single_bridge_optimizer.jl")

for filename in readdir(joinpath(@__DIR__, "bridges"); join = true)
    if endswith(filename, ".jl")
        include(filename)
    end
end

"""
    add_all_bridges(model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Objective` submodule to `model`.

The coefficient type used is `T`.
"""
function add_all_bridges(model, ::Type{T}) where {T}
    MOI.Bridges.add_bridge(model, FunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, QuadratizeBridge{T})
    MOI.Bridges.add_bridge(model, ToScalarNonlinearBridge{T})
    MOI.Bridges.add_bridge(model, SlackBridge{T})
    MOI.Bridges.add_bridge(model, VectorFunctionizeBridge{T})
    MOI.Bridges.add_bridge(model, VectorSlackBridge{T})
    return
end

end
