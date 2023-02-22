# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    struct OptimizerWithAttributes
        optimizer_constructor
        params::Vector{Pair{AbstractOptimizerAttribute,<:Any}}
    end

Object grouping an optimizer constructor and a list of optimizer attributes.
Instances are created with [`instantiate`](@ref).
"""
struct OptimizerWithAttributes
    # Function that takes zero arguments and returns a new optimizer.
    # The type of the function could be
    # * `Function`: a function, or
    # * `Type`: a type, or
    # * `UnionAll`: a type with missing parameters.
    optimizer_constructor::Any
    params::Vector{Pair{AbstractOptimizerAttribute,Any}}
end

_to_param(param::Pair{<:AbstractOptimizerAttribute}) = param
function _to_param(param::Pair{String})
    return RawOptimizerAttribute(param.first) => param.second
end
function _to_param(param::Pair)
    return error(
        "Expected an optimizer attribute or a string, got `$(param.first)` which is a `$(typeof(param.first))`.",
    )
end

"""
    OptimizerWithAttributes(optimizer_constructor, params::Pair...)

Create an [`OptimizerWithAttributes`](@ref) with the parameters `params`.
"""
function OptimizerWithAttributes(
    optimizer_constructor,
    args::Vararg{Pair,N},
) where {N}
    if !applicable(optimizer_constructor)
        error(_INSTANTIATE_NOT_CALLABLE_MESSAGE)
    end
    params =
        Pair{AbstractOptimizerAttribute,Any}[_to_param(arg) for arg in args]
    return OptimizerWithAttributes(optimizer_constructor, params)
end

function get(opt::OptimizerWithAttributes, attr::AbstractOptimizerAttribute)
    for (param, value) in opt.params
        if param == attr
            return value
        end
    end
    return nothing
end

function set(
    opt::OptimizerWithAttributes,
    attr::AbstractOptimizerAttribute,
    value,
)
    for (i, (param, _)) in enumerate(opt.params)
        if param == attr
            opt.params[i] = attr => value
            return
        end
    end
    push!(opt.params, attr => value)
    return
end

const _INSTANTIATE_NOT_CALLABLE_MESSAGE =
    "The provided `optimizer_constructor` is invalid. It must be callable with zero " *
    "arguments. For example, \"Ipopt.Optimizer\" or " *
    "\"() -> ECOS.Optimizer()\". It should not be an instantiated optimizer " *
    "like \"Ipopt.Optimizer()\" or \"ECOS.Optimizer()\". " *
    "(Note the difference in parentheses!)"

"""
    _instantiate_and_check(optimizer_constructor)

Create an instance of optimizer by calling `optimizer_constructor`.
Then check that the type returned is an empty [`ModelLike`](@ref).
"""
function _instantiate_and_check((@nospecialize optimizer_constructor))
    if !applicable(optimizer_constructor)
        error(_INSTANTIATE_NOT_CALLABLE_MESSAGE)
    end
    optimizer = optimizer_constructor()
    if !isa(optimizer, ModelLike)
        error(
            "The provided `optimizer_constructor` returned an object of type " *
            "$(typeof(optimizer)). Expected a " *
            "MathOptInterface.ModelLike.",
        )
    end
    if !is_empty(optimizer)
        error(
            "The provided `optimizer_constructor` returned a non-empty optimizer.",
        )
    end
    return optimizer
end

"""
    _instantiate_and_check(optimizer_constructor::OptimizerWithAttributes)

Create an instance of optimizer represented by [`OptimizerWithAttributes`](@ref).
Then check that the type returned is an empty [`AbstractOptimizer`](@ref).
"""
function _instantiate_and_check(optimizer_constructor::OptimizerWithAttributes)
    optimizer =
        _instantiate_and_check(optimizer_constructor.optimizer_constructor)
    for param in optimizer_constructor.params
        set(optimizer, param.first, param.second)
    end
    return optimizer
end

"""
    instantiate(
        optimizer_constructor,
        with_cache_type::Union{Nothing,Type} = nothing,
        with_bridge_type::Union{Nothing,Type} = nothing,
    )

Create an instance of an optimizer by either:

 * calling `optimizer_constructor.optimizer_constructor()` and setting the
   parameters in `optimizer_constructor.params` if `optimizer_constructor` is a
   [`OptimizerWithAttributes`](@ref)
*  calling `optimizer_constructor()` if `optimizer_constructor` is callable.

## with_cache_type

If `with_cache_type` is not `nothing`, then the optimizer is wrapped in a
[`Utilities.CachingOptimizer`](@ref) to store a cache of the model. This is most
useful if the optimizer you are constructing does not support the incremental
interface (see [`supports_incremental_interface`](@ref)).

## with_bridge_type

If `with_bridge_type` is not `nothing`, the optimizer is wrapped in a
[`Bridges.full_bridge_optimizer`](@ref), enabling all the bridges defined in
the MOI.Bridges submodule with coefficient type `with_bridge_type`.

In addition, if the optimizer created by `optimizer_constructor` does not
support the incremental interface (see [`supports_incremental_interface`](@ref)),
then, irrespective of `with_cache_type`, the optimizer is wrapped in a
[`Utilities.CachingOptimizer`](@ref) to store a cache of the bridged model.

If `with_cache_type` and `with_bridge_type` are both not `nothing`, then they
must be the same type.
"""
function instantiate(
    (@nospecialize optimizer_constructor);
    with_bridge_type::Union{Nothing,Type} = nothing,
    with_cache_type::Union{Nothing,Type} = nothing,
)
    if with_bridge_type !== nothing && with_cache_type !== nothing
        if with_bridge_type != with_cache_type
            error(
                "If both provided, `with_bridge_type` and `with_cache_type` " *
                "must be the same type. Got " *
                "`with_bridge_type = $with_bridge_type` and " *
                "`with_cache_type = $with_cache_type`",
            )
        end
    end
    optimizer = _instantiate_and_check(optimizer_constructor)
    if with_bridge_type === nothing
        if with_cache_type === nothing
            return optimizer
        end
        cache = default_cache(optimizer, with_cache_type)
        return Utilities.CachingOptimizer(cache, optimizer)
    else
        if with_cache_type !== nothing ||
           !supports_incremental_interface(optimizer)
            cache = default_cache(optimizer, with_bridge_type)
            optimizer = Utilities.CachingOptimizer(cache, optimizer)
        end
        return Bridges.full_bridge_optimizer(optimizer, with_bridge_type)
    end
end

"""
    default_cache(optimizer::ModelLike, ::Type{T}) where {T}

Return a new instance of the default model type to be used as cache for
`optimizer` in a [`Utilities.CachingOptimizer`](@ref) for holding constraints
of coefficient type `T`. By default, this returns
`Utilities.UniversalFallback(Utilities.Model{T}())`. If copying from a instance
of a given model type is faster for `optimizer` then a new method returning
an instance of this model type should be defined.
"""
function default_cache(::ModelLike, ::Type{T}) where {T}
    return Utilities.UniversalFallback(Utilities.Model{T}())
end
