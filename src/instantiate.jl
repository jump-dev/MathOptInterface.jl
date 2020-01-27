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

"""
    OptimizerWithAttributes(optimizer_constructor, params::Pair...)

Create an [`OptimizerWithAttributes`](@ref) with the parameters `params`.
"""
function OptimizerWithAttributes(optimizer_constructor, args::Vararg{Pair, N}) where N
    if !applicable(optimizer_constructor)
        error(_INSTANTIATE_NOT_CALLABLE_MESSAGE)
    end
    params = Pair{AbstractOptimizerAttribute,Any}[_to_param(arg) for arg in args]
    return OptimizerWithAttributes(optimizer_constructor, params)
end

const _INSTANTIATE_NOT_CALLABLE_MESSAGE =
    "The provided `optimizer_constructor` is invalid. It must be callable with zero" *
    "arguments. For example, \"Ipopt.Optimizer\" or" *
    "\"() -> ECOS.Optimizer()\". It should not be an instantiated optimizer " *
    "like \"Ipopt.Optimizer()\" or \"ECOS.Optimizer()\"." *
    "(Note the difference in parentheses!)"

"""
    instantiate_and_check(optimizer_constructor)

Create an instance of optimizer by calling `optimizer_constructor`.
Then check that the type returned is an empty [`AbstractOptimizer`](@ref).
"""
function instantiate_and_check(optimizer_constructor)
    if !applicable(optimizer_constructor)
        error(_INSTANTIATE_NOT_CALLABLE_MESSAGE)
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
    instantiate_and_check(optimizer_constructor::OptimizerWithAttributes)

Create an instance of optimizer represented by [`OptimizerWithAttributes`](@ref).
Then check that the type returned is an empty [`AbstractOptimizer`](@ref).
"""
function instantiate_and_check(optimizer_constructor::OptimizerWithAttributes)
    optimizer = instantiate(optimizer_constructor.optimizer)
    for param in optimizer_constructor.params
        set(optimizer, param.first, param.second)
    end
    return optimizer
end

"""
    instantiate(optimizer_constructor,
                with_bridge_type::Union{Nothing, Type}=nothing,
                with_names::Bool=false)

If `with_bridge_type` is `nothing`, it is equivalent to
`instance(optimizer_constructor)`. Otherwise, it creates an instance of
optimizer represented by [`OptimizerWithAttributes`](@ref) with all the bridges
defined in the MathOptInterface.Bridges submodule enabled with coefficient type
`with_bridge_type`. If
`!MOI.Utilities.supports_default_copy_to(optimizer, with_names)` then a
[`Utilities.CachingOptimizer`](@ref) is added to store a cache of the bridged
model.
"""
function instantiate(
    optimizer_constructor; with_bridge_type::Union{Nothing, Type}=nothing,
    with_names::Bool=false)
    optimizer = instantiate_and_check(optimizer_constructor)
    if with_bridge_type === nothing
        return optimizer
    end
    if !Utilities.supports_default_copy_to(optimizer, with_names)
        universal_fallback = Utilities.UniversalFallback(Utilities.Model{with_bridge_type}())
        optimizer = Utilities.CachingOptimizer(universal_fallback, optimizer)
    end
    return Bridges.full_bridge_optimizer(optimizer, with_bridge_type)
end
