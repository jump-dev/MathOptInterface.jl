"""
    struct OptimizerWithAttributes
        optimizer
        params::Vector{Pair{AbstractOptimizerAttribute,<:Any}}
    end

Object grouping an optimizer constructor and a list of optimizer attributes.
Instances are created with [`instantiate`](@ref).
"""
struct OptimizerWithAttributes
    # Function that takes zero arguments and returns a new optimizer.
    # The type of the function could be
    # * `Function`: a function, or
    # * `DataType`: a type, or
    # * `UnionAll`: a type with missing parameters.
    optimizer
    params::Vector{Pair{AbstractOptimizerAttribute,Any}}
end

_to_param(param::Pair{<:AbstractOptimizerAttribute}) = param
_to_param(param::Pair{String}) = RawParameter(param.first) => param.second
function _to_param(param::Pair)
    error("Expected an optimizer attribute or a string, got `$(param.first)` which is a `$(typeof(param.first))`.")
end
function _add_params(params::Vector{Pair{AbstractOptimizerAttribute,Any}}) end
function _add_params(params::Vector{Pair{AbstractOptimizerAttribute,Any}},
                     param::Pair, other_params::Vararg{Pair, N}) where N
    push!(params, _to_param(param))
    _add_params(params, other_params...)
end

"""
    OptimizerWithAttributes(optimizer_constructor, params::Pair...)

Create an [`OptimizerWithAttributes`](@ref) with the parameters `params`.
"""
function OptimizerWithAttributes(optimizer_constructor, args::Vararg{Pair, N}) where N
    if !applicable(optimizer_constructor)
        error(_instantiate_not_callable_message)
    end
    params = Pair{AbstractOptimizerAttribute,Any}[]
    _add_params(params, args...)
    return OptimizerWithAttributes(optimizer_constructor, params)
end

function (optimizer_constructor::OptimizerWithAttributes)(;
    with_bridges::Bool=false, with_names::Bool=false, T::Type=Float64)
    if with_bridges
        return instantiate_with_bridges(optimizer_constructor;
                                        with_names=with_names, T=T)
    else
        return instantiate(optimizer_constructor)
    end
end

const _instantiate_not_callable_message =
    "The provided `optimizer_constructor` is invalid. It must be callable with zero" *
    "arguments. For example, \"Ipopt.Optimizer\" or" *
    "\"() -> ECOS.Optimizer()\". It should not be an instantiated optimizer " *
    "like \"Ipopt.Optimizer()\" or \"ECOS.Optimizer()\"." *
    "(Note the difference in parentheses!)"

"""
    instantiate(optimizer_constructor)

Create an instance of optimizer by calling `optimizer_constructor`.
"""
function instantiate(optimizer_constructor)
    if !applicable(optimizer_constructor)
        error(_instantiate_not_callable_message)
    end
    optimizer = optimizer_constructor()
    if !isa(optimizer, AbstractOptimizer)
        error("The provided `optimizer_constructor` returned an object of type " *
              "$(typeof(optimizer)). Expected a " *
              "MathOptInterface.AbstractOptimizer.")
    end
    if !is_empty(optimizer)
        error("The provided `optimizer_constructor` returned a non-empty optimizer.")
    end
    return optimizer
end

"""
    instantiate(optimizer_constructor::OptimizerWithAttributes)

Create an instance of optimizer represented by [`OptimizerWithAttributes`](@ref).
"""
function instantiate(optimizer_constructor::OptimizerWithAttributes)
    optimizer = instantiate(optimizer_constructor.optimizer)
    for param in optimizer_constructor.params
        set(optimizer, param.first, param.second)
    end
    return optimizer
end

"""
    instantiate_with_bridges(optimizer_constructor, with_names::Bool=false, T::Type=Float64)

Create an instance of optimizer represented by [`OptimizerWithAttributes`](@ref)
with all the bridges defined in the MathOptInterface.Bridges submodule enabled
with coefficient type `T`.
If `!MOI.Utilities.supports_default_copy_to(optimizer, with_names)` then a
[`Utilities.CachingOptimizer`](@ref) is added to store a cache of the bridged
model.
"""
function instantiate_with_bridges(
    optimizer_constructor;
    with_names::Bool=false, T::Type=Float64)

    optimizer = instantiate(optimizer_constructor)
    if !Utilities.supports_default_copy_to(optimizer, with_names)
        universal_fallback = Utilities.UniversalFallback(Utilities.Model{T}())
        optimizer = Utilities.CachingOptimizer(universal_fallback, optimizer)
    end
    return Bridges.full_bridge_optimizer(optimizer, T)
end
