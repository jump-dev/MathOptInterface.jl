# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(
        full_bridge_optimizer,
        (MOI.AbstractOptimizer, Type{Float64}),
    )
    Base.precompile(
        unbridged_function,
        (LazyBridgeOptimizer, MOI.AbstractScalarFunction),
    )   # time: 0.1381832
    return
end

_precompile_()
