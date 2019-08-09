module Bridges

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

const CI = MOI.ConstraintIndex

include("bridge.jl")
include("bridge_optimizer.jl")

# Variable bridges
include("Variable/Variable.jl")
# Constraint bridges
include("Constraint/Constraint.jl")

include("lazy_bridge_optimizer.jl")

"""
    full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where T

Returns a `LazyBridgeOptimizer` bridging `model` for every bridge defined in
this package and for the coefficient type `T`.
"""
function full_bridge_optimizer(model::MOI.ModelLike, T::Type)
    bridged_model = LazyBridgeOptimizer(model)
    Variable.add_all_bridges(bridged_model, T)
    Constraint.add_all_bridges(bridged_model, T)
    return bridged_model
end

end # module
