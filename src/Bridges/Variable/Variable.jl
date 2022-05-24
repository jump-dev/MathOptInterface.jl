# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Variable

using MathOptInterface

const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

include("bridge.jl")
include("map.jl")
include("set_map.jl")
include("single_bridge_optimizer.jl")

# TODO(odow): the compiler in Julia <= 1.2 (and in later versions unless
# fixed) gets stuck compiling add_constrained_variable for some inputs. This
# method seemed necessary to fix it.
# See https://github.com/JuliaLang/julia/issues/32167 for more.
function MOI.Bridges.Variable.bridge_constrained_variable(BridgeType, b, s)
    return throw(
        MOI.UnsupportedConstraint{
            MOIU.variable_function_type(typeof(s)),
            typeof(s),
        }(),
    )
end

include("bridges/flip_sign.jl")
include("bridges/free.jl")
include("bridges/rsoc_to_psd.jl")
include("bridges/soc_rsoc.jl")
include("bridges/vectorize.jl")
include("bridges/zeros.jl")

"""
    add_all_bridges(model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Variable` submodule to `model`.

The coefficient type used is `T`.
"""
function add_all_bridges(model, ::Type{T}) where {T}
    MOI.Bridges.add_bridge(model, NonposToNonnegBridge{T})
    MOI.Bridges.add_bridge(model, FreeBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoPSDBridge{T})
    MOI.Bridges.add_bridge(model, SOCtoRSOCBridge{T})
    MOI.Bridges.add_bridge(model, RSOCtoSOCBridge{T})
    MOI.Bridges.add_bridge(model, VectorizeBridge{T})
    MOI.Bridges.add_bridge(model, ZerosBridge{T})
    return
end

end
