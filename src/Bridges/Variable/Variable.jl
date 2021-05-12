module Variable

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

# Definition of a variable bridge
include("bridge.jl")

# Mapping between variable indices and bridges
include("map.jl")

# Bridge optimizer bridging a specific variable bridge
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

# Variable bridges
include("zeros.jl")
const Zeros{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{ZerosBridge{T},OT}
include("free.jl")
const Free{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{FreeBridge{T},OT}
include("flip_sign.jl")
const NonposToNonneg{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{NonposToNonnegBridge{T},OT}
include("vectorize.jl")
const Vectorize{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{VectorizeBridge{T},OT}
include("soc_to_rsoc.jl")
const SOCtoRSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOCtoRSOCBridge{T},OT}
include("rsoc_to_soc.jl")
const RSOCtoSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoSOCBridge{T},OT}
include("rsoc_to_psd.jl")
const RSOCtoPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoPSDBridge{T},OT}

"""
    add_all_bridges(bridged_model, ::Type{T}) where {T}

Add all bridges defined in the `Bridges.Variable` submodule to `bridged_model`.
The coefficient type used is `T`.
"""
function add_all_bridges(bridged_model, ::Type{T}) where {T}
    MOIB.add_bridge(bridged_model, ZerosBridge{T})
    MOIB.add_bridge(bridged_model, FreeBridge{T})
    MOIB.add_bridge(bridged_model, NonposToNonnegBridge{T})
    MOIB.add_bridge(bridged_model, VectorizeBridge{T})
    MOIB.add_bridge(bridged_model, SOCtoRSOCBridge{T})
    MOIB.add_bridge(bridged_model, RSOCtoSOCBridge{T})
    MOIB.add_bridge(bridged_model, RSOCtoPSDBridge{T})
    return
end

end
