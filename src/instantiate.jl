"""
    struct UninstantiatedOptimizer
        optimizer
        params::Vector{Pair{AbstractOptimizerAttribute,<:Any}}
    end

Object representing an optimizer, created by `optimizer()` parametrized by
parameters listed in `params`. Instances are created with [`instantiate`](@ref).
"""
struct UninstantiatedOptimizer
    # Function that takes zero arguments and returns a new optimizer.
    # The type of the function could be
    # * `Function`: a function, or
    # * `DataType`: a type, or
    # * `UnionAll`: a type with missing parameters.
    optimizer
    params::Vector{Pair{AbstractOptimizerAttribute,Any}}
end

"""
    instantiate(uninstantiated::UninstantiatedOptimizer)

Create an instance of optimizer represented by [`UninstantiatedOptimizer`](@ref).
"""
function instantiate(uninstantiated::UninstantiatedOptimizer)
    optimizer = uninstantiated.optimizer()
    for param in uninstantiated.params
        set(optimizer, param.first, param.second)
    end
    return optimizer
end

"""
    instantiate(uninstantiated::UninstantiatedOptimizer,
                with_names::Bool=false, T::Type=Float64)

Create an instance of optimizer represented by [`UninstantiatedOptimizer`](@ref)
with all the bridges defined in the MathOptInterface.Bridges submodule enabled
with coefficient type `T`.
If `!MOI.Utilities.supports_default_copy_to(optimizer, with_names)` then a
[`CachingOptimizer`](@ref) is added to store a cache of the bridged model.
"""
function instantiate_with_bridges(uninstantiated::UninstantiatedOptimizer,
                                  with_names::Bool=false, T::Type=Float64)
    optimizer = instantiate(uninstantiated)
    if !Utilities.supports_default_copy_to(optimizer, with_names)
        universal_fallback = Utilities.UniversalFallback(Utilities.Model{T}())
        optimizer = Utilities.CachingOptimizer(universal_fallback, optimizer)
    end
    return Bridges.full_bridge_optimizer(optimizer, T)
end
