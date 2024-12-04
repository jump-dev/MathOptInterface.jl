# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Variable

import MathOptInterface as MOI

include("bridge.jl")
include("map.jl")
include("set_map.jl")
include("single_bridge_optimizer.jl")

for filename in readdir(joinpath(@__DIR__, "bridges"); join = true)
    include(filename)
end

"""
    add_all_bridges(model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Variable` submodule to `model`.

The coefficient type used is `T`.
"""
function add_all_bridges(model, ::Type{T}) where {T}
    MOI.Bridges.add_bridge(model, ZerosBridge{T})
    MOI.Bridges.add_bridge(model, FreeBridge{T})
    MOI.Bridges.add_bridge(model, NonposToNonnegBridge{T})
    MOI.Bridges.add_bridge(model, VectorizeBridge{T})
    MOI.Bridges.add_bridge(model, SOCtoRSOCBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoSOCBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(model, HermitianToSymmetricPSDBridge{T})
    MOI.Bridges.add_bridge(model, ParameterToEqualToBridge{T})
    return
end

end
