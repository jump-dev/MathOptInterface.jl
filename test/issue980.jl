module Issue980

import MathOptInterface
const MOI = MathOptInterface

abstract type StaticArray end
(::Type{SA})(x...) where {SA<:StaticArray} = SA(x)

function crash()
    model = MOI.Utilities.Model{Float64}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(MOI.Utilities.Model{Float64}())
    return MOI.copy_to(bridged, model)
end

crash()

end
