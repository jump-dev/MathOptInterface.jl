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

_to_param(param::Pair{<:AbstractOptimizerAttribute}) = param
_to_param(param::Pair) = RawParameter(param.first) => param.second
_to_param(param::Pair{Symbol}) = _to_param(string(param.first) => param.second)
function _add_params(params::Vector{Pair{AbstractOptimizerAttribute,Any}}) end
function _add_params(params::Vector{Pair{AbstractOptimizerAttribute,Any}},
                     param::Pair, other_params::Vararg{Pair, N}) where N
    push!(params, _to_param(param))
    _add_params(params, other_params...)
end

"""
    parametrize(uninstantiated, params::Pair...)

Create an [`UninstantiatedOptimizer`](@ref) with the parameters `params`.
"""
function parametrize(uninstantiated, args::Vararg{Pair, N}) where N
    if !applicable(uninstantiated)
        error(_instantiate_not_callable_message)
    end
    params = Pair{AbstractOptimizerAttribute,Any}[]
    _add_params(params, args...)
    return UninstantiatedOptimizer(uninstantiated, params)
end

const _instantiate_not_callable_message =
    "The provided `uninstantiated` is invalid. It must be callable with zero" *
    "arguments. For example, \"Ipopt.Optimizer\" or" *
    "\"() -> ECOS.Optimizer()\". It should not be an instantiated optimizer " *
    "like \"Ipopt.Optimizer()\" or \"ECOS.Optimizer()\"." *
    "(Note the difference in parentheses!)"


"""
    instantiate(uninstantiated)

Create an instance of optimizer by calling `uninstantiated`.
"""
function instantiate(uninstantiated)
    if !applicable(uninstantiated)
        error(_instantiate_not_callable_message)
    end
    optimizer = uninstantiated()
    if !isa(optimizer, AbstractOptimizer)
        error("The provided `uninstantiated` returned an object of type " *
              "$(typeof(optimizer)). Expected a " *
              "MathOptInterface.AbstractOptimizer.")
    end
    if !is_empty(optimizer)
        error("The provided `uninstantiated` returned a non-empty optimizer.")
    end
    return optimizer
end

"""
    instantiate(uninstantiated::UninstantiatedOptimizer)

Create an instance of optimizer represented by [`UninstantiatedOptimizer`](@ref).
"""
function instantiate(uninstantiated::UninstantiatedOptimizer)
    optimizer = instantiate(uninstantiated.optimizer)
    for param in uninstantiated.params
        set(optimizer, param.first, param.second)
    end
    return optimizer
end

"""
    instantiate_with_bridges(uninstantiated, with_names::Bool=false, T::Type=Float64)

Create an instance of optimizer represented by [`UninstantiatedOptimizer`](@ref)
with all the bridges defined in the MathOptInterface.Bridges submodule enabled
with coefficient type `T`.
If `!MOI.Utilities.supports_default_copy_to(optimizer, with_names)` then a
[`Utilities.CachingOptimizer`](@ref) is added to store a cache of the bridged
model.
"""
function instantiate_with_bridges(uninstantiated, with_names::Bool=false,
                                  T::Type=Float64)
    optimizer = instantiate(uninstantiated)
    if !Utilities.supports_default_copy_to(optimizer, with_names)
        universal_fallback = Utilities.UniversalFallback(Utilities.Model{T}())
        optimizer = Utilities.CachingOptimizer(universal_fallback, optimizer)
    end
    return Bridges.full_bridge_optimizer(optimizer, T)
end
