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
