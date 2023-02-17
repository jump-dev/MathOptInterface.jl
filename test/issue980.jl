# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module Issue980

import MathOptInterface as MOI

abstract type StaticArray end
(::Type{SA})(x...) where {SA<:StaticArray} = SA(x)

function crash()
    model = MOI.Utilities.Model{Float64}()
    bridged = MOI.Bridges.LazyBridgeOptimizer(MOI.Utilities.Model{Float64}())
    return MOI.copy_to(bridged, model)
end

crash()

end
