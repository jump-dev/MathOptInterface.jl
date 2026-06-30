# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end

An abstract type that implements generic functions for bridges.

## Implementation notes

By convention, the inner optimizer should be stored in a `model` field. If not,
the optimizer must implement [`MOI.optimize!`](@ref).
"""
abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end

# AbstractBridgeOptimizer interface

"""
    recursive_model(b::AbstractBridgeOptimizer)

If a variable, constraint, or objective is bridged, return the context of the
inner variables. For most optimizers, this should be `b.model`.
"""
function recursive_model end

"""
    supports_constraint_bridges(b::AbstractBridgeOptimizer)::Bool

Return a `Bool` indicating if `b` supports
[`MOI.Bridges.Constraint.AbstractBridge`](@ref).
"""
function supports_constraint_bridges end

"""
    is_bridged(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )::Bool

Return a `Bool` indicating whether `b` tries to bridge `F`-in-`S` constraints
instead of passing it as is to its internal model.

    is_bridged(b::AbstractBridgeOptimizer, S::Type{<:MOI.AbstractSet})::Bool

Return a `Bool` indicating whether `b` tries to bridge constrained variables in
`S` instead of passing it as is to its internal model.

    is_bridged(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
    )::Bool

Return a `Bool` indicating whether `b` tries to bridge objective functions of
type `F` instead of passing it as is to its internal model.
"""
function is_bridged end

"""
    is_bridged(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)

Return a `Bool` indicating whether `vi` is bridged. The variable is said to be
bridged if it is a variable of `b` but not a variable of `b.model`.
"""
function is_bridged(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    return Variable.is_bridged_variable(Variable.bridges(b), vi)
end

"""
    is_bridged(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)

Return a `Bool` indicating whether `ci` is bridged. The constraint is said to be
bridged if it is a constraint of `b` but not a constraint of `b.model`.
"""
function is_bridged(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    return is_bridged(b, F, S)
end

function is_bridged(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    # By MOI convention, a `CI{VariableIndex, S}` shares its `value` with the
    # constrained variable. There are three possibilities:
    #  * the variable is bridged: the constraint was either created by a
    #    variable bridge or force-bridged because it was added on a bridged
    #    variable. Either way it is bridged.
    #  * the variable is not bridged but the constraint was added with a
    #    constraint bridge: it lives in `Constraint.bridges(b)`.
    #  * otherwise it passes through to `b.model` and is not bridged.
    if is_bridged(b, MOI.VariableIndex(ci.value))
        return true
    end
    return haskey(Constraint.bridges(b), ci)
end

function is_bridged(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    # A `CI{VectorOfVariables, S}` is bridged if it is a vector of constrained
    # variables (stored in `Variable.bridges(b)`) or a force-/constraint-
    # bridged constraint (stored in `Constraint.bridges(b)`).
    if MOI.is_valid(Variable.bridges(b), ci)
        return true
    end
    return haskey(Constraint.bridges(b), ci)
end

"""
    is_bridged(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)

Return a `Bool` indicating whether `attr` is bridged. The objective function is
said to be bridged the objective function attribute passed to `b.model` is
different to `attr`.
"""
function is_bridged(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)
    return haskey(Objective.bridges(b), attr)
end

"""
    supports_bridging_constrained_variable(
        ::AbstractBridgeOptimizer,
        ::Type{<:MOI.AbstractSet},
    )

Return a `Bool` indicating whether `b` supports bridging constrained variable in
`S`.
"""
function supports_bridging_constrained_variable(
    ::AbstractBridgeOptimizer,
    ::Type{<:MOI.AbstractSet},
)
    return false
end

"""
    is_variable_bridged(
        b::AbstractBridgeOptimizer,
        S::Type{<:MOI.AbstractSet},
    )

Return a `Bool` indicating whether `b` bridges constrained variable in
`S` using a variable bridge, assuming `is_bridged(b, S)`. If it returns `false`,
it means that free variables and a constraint on these variables should be
added instead. The constraint is then bridged by a constraint bridge.
"""
function is_variable_bridged(
    ::AbstractBridgeOptimizer,
    ::Type{<:MOI.AbstractSet},
)
    return false
end

"""
    is_variable_bridged(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)

Returns whether `ci` is the constraint of a bridged constrained variable. That
is, if it was returned by `Variable.add_key_for_bridge` or
`Variable.add_keys_for_bridge`. This is *not* the same as a constraint that
was merely force-bridged on a bridged variable (that one lives in
`Constraint.bridges(b)`).
"""
is_variable_bridged(::AbstractBridgeOptimizer, ::MOI.ConstraintIndex) = false

function is_variable_bridged(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex},
)
    # The constraint of a bridged constrained scalar variable shares its
    # `value` with the variable. Exclude force-bridged constraints, which are
    # stored in `Constraint.bridges(b)`.
    return is_bridged(b, MOI.VariableIndex(ci.value)) &&
           !haskey(Constraint.bridges(b), ci)
end

function is_variable_bridged(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
)
    return MOI.is_valid(Variable.bridges(b), ci) &&
           !haskey(Constraint.bridges(b), ci)
end

"""
    supports_bridging_constraint(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )::Bool

Return a `Bool` indicating whether `b` supports bridging `F`-in-`S` constraints.
"""
function supports_bridging_constraint(
    ::AbstractBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
    ::Type{<:MOI.AbstractSet},
)
    return false
end

"""
    supports_bridging_objective_function(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
    )::Bool

Return a `Bool` indicating whether `b` supports bridging objective functions of
type `F`.
"""
function supports_bridging_objective_function(
    ::AbstractBridgeOptimizer,
    ::Type{<:MOI.AbstractFunction},
)
    return false
end

"""
    bridge_type(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )

Return the `AbstractBridge` type to be used to bridge `F`-in-`S` constraints.
This function should only be called if `is_bridged(b, F, S)`.
"""
function bridge_type end

"""
    bridge(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)

Return the `Variable.AbstractBridge` used to bridge the variable with index
`vi`.
"""
function bridge(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    map = Variable.bridges(b)::Variable.Map
    return map[vi]
end

"""
    bridge(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)

Return the `AbstractBridge` used to bridge the constraint with index `ci`.
"""
function bridge(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)
    if is_variable_bridged(b, ci)
        map = Variable.bridges(b)::Variable.Map
        return bridge(b, Variable.first_variable(map, ci))
    else
        return Constraint.bridges(b)[ci]
    end
end

"""
    bridge(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)

Return the `Objective.AbstractBridge` used to bridge the objective function
`attr`.
"""
function bridge(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)
    map = Objective.bridges(b)::Objective.Map
    return map[attr]
end

"""
    call_in_context(
        b::AbstractBridgeOptimizer,
        vi::MOI.VariableIndex,
        f::Function,
    )

Call `f(bridge)` where `vi` is bridged by `bridge` in its context, see
[`Variable.call_in_context`](@ref).
"""
function call_in_context(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
    f::Function,
)
    return Variable.call_in_context(Variable.bridges(b)::Variable.Map, vi, f)
end

"""
    call_in_context(
        b::AbstractBridgeOptimizer,
        ci::MOI.ConstraintIndex,
        f::Function,
    )

Call `f(bridge)` where `ci` is bridged by `bridge` in its context, see
[`Variable.call_in_context`](@ref).
"""
function call_in_context(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex,
    f::Function,
)
    if is_variable_bridged(b, ci)
        map = Variable.bridges(b)::Variable.Map
        return call_in_context(b, Variable.first_variable(map, ci), f)
    else
        return Variable.call_in_context(
            Variable.bridges(b),
            ci,
            () -> f(Constraint.bridges(b)[ci]),
        )
    end
end

function call_in_context(
    f::F,
    b::AbstractBridgeOptimizer,
    index::MOI.Index,
    attr::MOI.AnyAttribute,
    args::Vararg{Any,N},
) where {F<:Union{typeof(MOI.get),typeof(MOI.set)},N}
    return call_in_context(
        b,
        index,
        bridge -> f(recursive_model(b), attr, bridge, args...),
    )
end

function call_in_context(
    f::F,
    b::AbstractBridgeOptimizer,
    index::MOI.Index,
    args::Vararg{Any,N},
) where {F<:Function,N}
    return call_in_context(
        b,
        index,
        bridge -> f(recursive_model(b), bridge, args...),
    )
end

function _functionize_bridge(b::AbstractBridgeOptimizer, bridge_type)
    func, name = _func_name(bridge_type)
    return error(
        "Need to apply a `$bridge_type` to a `$func` $name because the",
        " variable is bridged but $name bridges are not supported by",
        " `$(typeof(b))`.",
    )
end

function constraint_scalar_functionize_bridge(b::AbstractBridgeOptimizer)
    return _functionize_bridge(b, Constraint.ScalarFunctionizeBridge)
end

function constraint_vector_functionize_bridge(b::AbstractBridgeOptimizer)
    return _functionize_bridge(b, Constraint.VectorFunctionizeBridge)
end

function objective_functionize_bridge(b::AbstractBridgeOptimizer)
    return _functionize_bridge(b, Objective.FunctionizeBridge)
end

# Implementation of the MOI interface for AbstractBridgeOptimizer

# By convention, the model should be stored in a `model` field
function MOI.optimize!(b::AbstractBridgeOptimizer)
    final_touch(b)
    MOI.optimize!(b.model)
    return
end

function MOI.compute_conflict!(b::AbstractBridgeOptimizer)
    MOI.compute_conflict!(b.model)
    return
end

function MOI.is_empty(b::AbstractBridgeOptimizer)
    return isempty(Variable.bridges(b)) &&
           isempty(Constraint.bridges(b)) &&
           MOI.is_empty(b.model)
end

function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.model)
    if Variable.has_bridges(Variable.bridges(b))
        empty!(b.var_to_name)
        b.name_to_var = nothing
    end
    if Variable.has_bridges(Variable.bridges(b)) ||
       Constraint.has_bridges(Constraint.bridges(b))
        empty!(b.con_to_name)
        b.name_to_con = nothing
    end
    empty!(Variable.bridges(b))
    empty!(Constraint.bridges(b))
    empty!(Objective.bridges(b))
    return
end

function MOI.supports(
    b::AbstractBridgeOptimizer,
    attr::Union{MOI.AbstractModelAttribute,MOI.AbstractOptimizerAttribute},
)
    return MOI.supports(b.model, attr)
end

function MOI.Utilities.pass_nonvariable_constraints(
    dest::AbstractBridgeOptimizer,
    src::MOI.ModelLike,
    idxmap::MOI.Utilities.IndexMap,
    constraint_types,
)
    if Variable.has_bridges(Variable.bridges(dest))
        # The functions may contain bridged variables which needs to be
        # substituted so we use the fallback.
        MOI.Utilities.pass_nonvariable_constraints_fallback(
            dest,
            src,
            idxmap,
            constraint_types,
        )
        return
    end
    not_bridged_types = eltype(constraint_types)[]
    bridged_types = eltype(constraint_types)[]
    for (F, S) in constraint_types
        if is_bridged(dest, F, S)
            push!(bridged_types, (F, S))
        else
            push!(not_bridged_types, (F, S))
        end
    end
    MOI.Utilities.pass_nonvariable_constraints(
        dest.model,
        src,
        idxmap,
        not_bridged_types,
    )
    MOI.Utilities.pass_nonvariable_constraints_fallback(
        dest,
        src,
        idxmap,
        bridged_types,
    )
    return
end

function MOI.copy_to(dest::AbstractBridgeOptimizer, src::MOI.ModelLike)
    return MOI.Utilities.default_copy_to(dest, src)
end

function MOI.supports_incremental_interface(b::AbstractBridgeOptimizer)
    return MOI.supports_incremental_interface(b.model)
end

function final_touch(b::AbstractBridgeOptimizer)
    final_touch(Constraint.bridges(b), recursive_model(b))
    return
end

function MOI.Utilities.final_touch(b::AbstractBridgeOptimizer, index_map)
    final_touch(b)
    MOI.Utilities.final_touch(b.model, index_map)
    return
end

# References
"""
    outer_to_inner(b::AbstractBridgeOptimizer, idx::MOI.Index)::typeof(idx)

Translate an outer index (in `b`'s user-facing namespace) to its inner
counterpart (`b.model`'s namespace). Returns `idx` unchanged when no
translation is in effect at this layer:

* For a `VariableIndex`, only when variable bridges are used
  (`Variable.has_bridges(map)`). The caller is responsible for having
  already established that `idx` is not bridged at `b`'s layer.
* For a `ConstraintIndex{F, S}`, only when the constraint mapping for
  `(F, S)` has been activated (see
  [`Variable.is_constraint_mapping_active`](@ref)).

This is the canonical "if variable bridges are used, use the index map;
else return the index unchanged" helper.
"""
function outer_to_inner(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    map = Variable.bridges(b)
    if Variable.has_bridges(map)
        return map.outer_to_inner[vi]
    end
    return vi
end

function outer_to_inner(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    map = Variable.bridges(b)
    if Variable.has_bridges(map) &&
       Variable.is_constraint_mapping_active(map, F, S)
        return map.outer_to_inner[ci]
    end
    return ci
end

# By MOI convention, the `value` of a `CI{VariableIndex,S}` equals the
# `value` of the constrained variable, so its translation is derived from
# the variable translation instead of being stored separately.
function outer_to_inner(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    vi = outer_to_inner(b, MOI.VariableIndex(ci.value))
    return MOI.ConstraintIndex{MOI.VariableIndex,S}(vi.value)
end

"""
    inner_to_outer(b::AbstractBridgeOptimizer, idx::MOI.Index)::typeof(idx)

Inverse of [`outer_to_inner`](@ref): translate `idx` from `b.model`'s
namespace to `b`'s user-facing namespace. Returns `idx` unchanged when no
translation is in effect at this layer.
"""
function inner_to_outer(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    map = Variable.bridges(b)
    if Variable.has_bridges(map)
        return map.inner_to_outer[vi]
    end
    return vi
end

function inner_to_outer(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    map = Variable.bridges(b)
    if Variable.has_bridges(map) &&
       Variable.is_constraint_mapping_active(map, F, S)
        return map.inner_to_outer[ci]
    end
    return ci
end

function inner_to_outer(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    vi = inner_to_outer(b, MOI.VariableIndex(ci.value))
    return MOI.ConstraintIndex{MOI.VariableIndex,S}(vi.value)
end

"""
    _OuterToInner(b::AbstractBridgeOptimizer)

Callable wrapper around [`outer_to_inner`](@ref) for use with
[`MOI.Utilities.map_indices`](@ref) when translating every index of a
function or attribute value at once.
"""
struct _OuterToInner{B<:AbstractBridgeOptimizer} <: Function
    b::B
end

(f::_OuterToInner)(idx::MOI.Index) = outer_to_inner(f.b, idx)

"""
    _InnerToOuter(b::AbstractBridgeOptimizer)

Callable wrapper around [`inner_to_outer`](@ref).
"""
struct _InnerToOuter{B<:AbstractBridgeOptimizer} <: Function
    b::B
end

(f::_InnerToOuter)(idx::MOI.Index) = inner_to_outer(f.b, idx)

"""
    _TotalInnerToOuter(b::AbstractBridgeOptimizer)

Like [`_InnerToOuter`](@ref) but indices with no recorded outer counterpart
are returned unchanged instead of throwing. The unrecorded indices are
inner variables created by a bridge directly in `b.model` (they are hidden
from the user); they are subsequently replaced by their expression in terms
of user variables by [`unbridged_function`](@ref).
"""
struct _TotalInnerToOuter{B<:AbstractBridgeOptimizer} <: Function
    b::B
end

function (f::_TotalInnerToOuter)(vi::MOI.VariableIndex)
    map = Variable.bridges(f.b)::Variable.Map
    return get(map.inner_to_outer.var_map, vi, vi)
end

function (f::_TotalInnerToOuter)(ci::MOI.ConstraintIndex{F,S}) where {F,S}
    map = Variable.bridges(f.b)::Variable.Map
    if haskey(map.inner_to_outer.con_map, ci)
        return map.inner_to_outer.con_map[ci]
    end
    return ci
end

function (f::_TotalInnerToOuter)(
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    vi = f(MOI.VariableIndex(ci.value))
    return MOI.ConstraintIndex{MOI.VariableIndex,S}(vi.value)
end

"""
    _to_inner_value(b::AbstractBridgeOptimizer, value)

Translate every index in `value` from `b`'s outer namespace to `b.model`'s
inner namespace, right before crossing the `b.model` boundary.

This is needed only when the variable mapping is active AND
`recursive_model(b) === b` (for example, `LazyBridgeOptimizer`): in that
case [`bridged_function`](@ref) keeps substituted values in the outer
namespace. When `recursive_model(b) !== b` (for example,
`SingleBridgeOptimizer`), the substitution already translated every index
(see [`bridged_variable_function`](@ref)) so this is the identity.
"""
function _to_inner_value(b::AbstractBridgeOptimizer, value)
    if Variable.has_bridges(Variable.bridges(b)) && recursive_model(b) === b
        return MOI.Utilities.map_indices(_OuterToInner(b), value)
    end
    return value
end

"""
    _from_inner_value(b::AbstractBridgeOptimizer, value)

Translate every index in `value` from `b.model`'s inner namespace to `b`'s
outer namespace, right after crossing the `b.model` boundary. Indices with
no outer counterpart (inner variables created by bridges) are left
unchanged; the caller is expected to substitute them out via
[`unbridged_function`](@ref).
"""
function _from_inner_value(b::AbstractBridgeOptimizer, value)
    if Variable.has_bridges(Variable.bridges(b))
        return MOI.Utilities.map_indices(_TotalInnerToOuter(b), value)
    end
    return value
end

"""
    _unbridged_result_from_inner(b::AbstractBridgeOptimizer, value)

Process an attribute `value` returned by `b.model`: translate its indices
from the inner to the outer namespace, then substitute any remaining
bridge-created variables by their expression in user variables.
"""
function _unbridged_result_from_inner(b::AbstractBridgeOptimizer, value)
    return unbridged_function(b, _from_inner_value(b, value))
end

"""
    _unbridged_result_from_bridge(b::AbstractBridgeOptimizer, value)

Process an attribute `value` returned by a bridge of `b`. The value is
expressed in `recursive_model(b)`'s namespace: when that is `b.model`
(`SingleBridgeOptimizer`), translate as for
[`_unbridged_result_from_inner`](@ref); when it is `b` itself
(`LazyBridgeOptimizer`), the value is already in the outer namespace.
"""
function _unbridged_result_from_bridge(b::AbstractBridgeOptimizer, value)
    if recursive_model(b) !== b
        value = _from_inner_value(b, value)
    end
    return unbridged_function(b, value)
end

"""
    _to_inner_index_function(b::AbstractBridgeOptimizer, f)

Strictly translate every variable index of the `MOI.VariableIndex` or
`MOI.VectorOfVariables` function `f` from the outer to the inner namespace.
Unlike [`_to_inner_value`](@ref) this applies whenever the variable mapping
is active, regardless of `recursive_model(b)`, because such functions are
never rewritten by [`bridged_function`](@ref).
"""
function _to_inner_index_function(b::AbstractBridgeOptimizer, f)
    if Variable.has_bridges(Variable.bridges(b))
        return MOI.Utilities.map_indices(_OuterToInner(b), f)
    end
    return f
end

"""
    _record_inner_constraint!(b::AbstractBridgeOptimizer, inner_ci)

Map the constraint index returned by `b.model` into the outer namespace:

* `CI{VariableIndex,S}`: derived from the variable translation (no
  recording needed).
* other `CI{F,S}` with the `(F, S)` constraint mapping active: allocate a
  fresh outer value, record the bidirectional entry, and return it.
* otherwise: identity.
"""
function _record_inner_constraint!(
    b::AbstractBridgeOptimizer,
    inner_ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    map = Variable.bridges(b)
    if Variable.has_bridges(map) &&
       Variable.is_constraint_mapping_active(map, F, S)
        outer_ci =
            MOI.ConstraintIndex{F,S}(Variable.next_outer_constraint!(map, F, S))
        map.outer_to_inner[outer_ci] = inner_ci
        map.inner_to_outer[inner_ci] = outer_ci
        return outer_ci
    end
    return inner_ci
end

function _record_inner_constraint!(
    b::AbstractBridgeOptimizer,
    inner_ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    return inner_to_outer(b, inner_ci)
end

function MOI.is_valid(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    if is_bridged(b, vi)
        return haskey(Variable.bridges(b), vi)
    end
    map = Variable.bridges(b)
    if Variable.has_bridges(map)
        # Outer/inner translation is in effect. Outer `vi` must be in
        # `outer_to_inner` to be valid; the entry then points to the inner
        # index to forward.
        haskey(map.outer_to_inner, vi) || return false
    end
    return MOI.is_valid(b.model, outer_to_inner(b, vi))
end

function MOI.is_valid(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if is_bridged(b, ci)
        if is_variable_bridged(b, ci)
            v_map = Variable.bridges(b)::Variable.Map
            return MOI.is_valid(v_map, ci)
        else
            # This return value has the potential to be a false positive: it
            # doesn't discriminate between constraints that the user added, and
            # constraints that a bridge added that were themselves bridged.
            #
            # "Fixing" this particular call has a number of wide reaching
            # effects because bridges need this to be "true" so that they can
            # query attributes of the constraint from `b`.
            #
            # In most cases a false positive doesn't matter, because we really
            # do support querying stuff about it. And also the user needs some
            # way of obtaining the correct index, which they won't have except
            # by luck/enumeration.
            #
            # The main place that this is problematic is when we come to delete
            # constraints, and in particular VariableIndex constraints, because we
            # triviallly have their `.value` field from the `.value` of the
            # VariableIndex.
            #
            # Instead of fixing everything though, we implement some extra
            # checks when deleting, and we leave the false-positive as-is for
            # now. If you, future reader, hit this comment while debugging, we
            # might need to revisit this decision.
            #
            # x-ref https://github.com/jump-dev/MathOptInterface.jl/issues/2696
            # x-ref https://github.com/jump-dev/MathOptInterface.jl/issues/2817
            # x-ref https://github.com/jump-dev/MathOptInterface.jl/pull/2818
            return haskey(Constraint.bridges(b), ci)
        end
    else
        inner_ci = _inner_index_or_nothing(b, ci)
        if inner_ci === nothing
            return false
        end
        return MOI.is_valid(b.model, inner_ci)
    end
end

"""
    _inner_index_or_nothing(b::AbstractBridgeOptimizer, idx::MOI.Index)

Like [`outer_to_inner`](@ref) but return `nothing` instead of throwing when
`idx` has no inner counterpart (for example, an invalid index supplied by
the user). Used by validity checks.
"""
function _inner_index_or_nothing(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    map = Variable.bridges(b)
    if Variable.has_bridges(map)
        return get(map.outer_to_inner.var_map, vi, nothing)
    end
    return vi
end

function _inner_index_or_nothing(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    map = Variable.bridges(b)
    if Variable.has_bridges(map) &&
       Variable.is_constraint_mapping_active(map, F, S)
        if haskey(map.outer_to_inner.con_map, ci)
            return map.outer_to_inner.con_map[ci]
        end
        return nothing
    end
    return ci
end

function _inner_index_or_nothing(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    vi = _inner_index_or_nothing(b, MOI.VariableIndex(ci.value))
    if vi === nothing
        return nothing
    end
    return MOI.ConstraintIndex{MOI.VariableIndex,S}(vi.value)
end

function _delete_variables_in_vector_of_variables_constraint(
    b::AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables,S},
) where {S}
    func = MOI.get(b, MOI.ConstraintFunction(), ci)
    if vis == func.variables
        MOI.delete(b, ci)
    else
        variables = copy(func.variables)
        for vi in vis
            i = findfirst(isequal(vi), variables)
            if i !== nothing
                if MOI.supports_dimension_update(S)
                    call_in_context(MOI.delete, b, ci, IndexInVector(i))
                else
                    MOI.Utilities.throw_delete_variable_in_vov(vi)
                end
            end
        end
    end
    return
end

"""
    _is_added_by_bridge(
        c_map,
        cache::Dict{Any,Set{Int64}},
        ci::MOI.ConstraintIndex{F,S},
    ) where {F,S}

Return `true` if `ci` was added by one of the bridges in `c_map`.

For performance reasons, we store the index values associated with
`MOI.ListOfConstraintIndices{F,S}` in `cache` so that we don't have to keep
looping through the bridges.
"""
function _is_added_by_bridge(
    c_map,
    cache::Dict{Any,Set{Int64}},
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    ret = get!(cache, (F, S)) do
        set = Set{Int64}()
        for bridge in values(c_map)
            for ci in MOI.get(
                bridge,
                MOI.ListOfConstraintIndices{F,S}(),
            )
                push!(set, ci.value)
            end
        end
        return set
    end::Set{Int64}
    return ci.value in ret
end

"""
    _delete_variables_in_variables_constraints(
        b::AbstractBridgeOptimizer,
        vis::Vector{MOI.VariableIndex},
    )

The point of this function is to delete constraints associated with the
variables in `vis`.

## Warning

Because of the false positive potential in
`is_valid(::AbstractBridgeOptimizer, MOI.ConstraintIndex)`, we need to ensure
that we delete constraints only if they were not added by a different constraint
bridge, otherwise when we come to delete the parent constraint we'll hit a
runtime error where we have already deleted part of the bridged constraint.

x-ref https://github.com/jump-dev/MathOptInterface.jl/issues/2817
x-ref https://github.com/jump-dev/MathOptInterface.jl/pull/2818
"""
function _delete_variables_in_variables_constraints(
    b::AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
)
    c_map = Constraint.bridges(b)::Constraint.Map
    cache = Dict{Any,Set{Int64}}()
    for vi in vis
        for ci in Constraint.variable_constraints(c_map, vi)
            if !_is_added_by_bridge(c_map, cache, ci)
                MOI.delete(b, ci)
            end
        end
    end
    for ci in Constraint.vector_of_variables_constraints(c_map)
        if !_is_added_by_bridge(c_map, cache, ci)
            _delete_variables_in_vector_of_variables_constraint(b, vis, ci)
        end
    end
    return
end

function _delete_variables_in_bridged_objective(
    b::AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
)
    F = MOI.get(b, MOI.ObjectiveFunctionType())
    _delete_variables_in_bridged_objective(b, vis, F)
    return
end

function _delete_variables_in_bridged_objective(
    b::AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
    ::Type{MOI.VectorOfVariables},
)
    obj_f = MOI.get(b, MOI.ObjectiveFunction{MOI.VectorOfVariables}())
    discard = Base.Fix2(in, vis)
    if !any(discard, obj_f.variables)
        # There are no variables in the objective function to delete, so we
        # don't need to do anything.
        return
    end
    new_obj_f = MOI.VectorOfVariables(filter(!discard, obj_f.variables))
    if MOI.output_dimension(new_obj_f) == 0
        # We've deleted all variables in the objective. Zero the objective by
        # setting FEASIBILITY_SENSE, but restore the sense afterwards.
        sense = MOI.get(b, MOI.ObjectiveSense())
        MOI.set(b, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        MOI.set(b, MOI.ObjectiveSense(), sense)
    else
        MOI.set(b, MOI.ObjectiveFunction{MOI.VectorOfVariables}(), new_obj_f)
    end
    return
end

function _delete_variables_in_bridged_objective(
    b::AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
    ::Type{MOI.VariableIndex},
)
    obj_f = MOI.get(b, MOI.ObjectiveFunction{MOI.VariableIndex}())
    if obj_f in vis
        sense = MOI.get(b, MOI.ObjectiveSense())
        MOI.set(b, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        MOI.set(b, MOI.ObjectiveSense(), sense)
    end
    return
end

function _delete_variables_in_bridged_objective(
    ::AbstractBridgeOptimizer,
    ::Vector{MOI.VariableIndex},
    ::Type{F},
) where {F}
    # Nothing to do here. The variables can be deleted without changing the type
    # of the objective, or whether one exists.
    return
end

function MOI.delete(b::AbstractBridgeOptimizer, vis::Vector{MOI.VariableIndex})
    if is_objective_bridged(b)
        _delete_variables_in_bridged_objective(b, vis)
    end
    if Constraint.has_bridges(Constraint.bridges(b))
        _delete_variables_in_variables_constraints(b, vis)
    end
    if any(vi -> is_bridged(b, vi), vis)
        for vi in vis
            MOI.throw_if_not_valid(b, vi)
        end
        if all(vi -> is_bridged(b, vi), vis) &&
           Variable.has_keys(Variable.bridges(b)::Variable.Map, vis)
            call_in_context(MOI.delete, b, first(vis))
            b.name_to_var = nothing
            for vi in vis
                delete!(b.var_to_name, vi)
            end
            ci = Variable.constraint(
                Variable.bridges(b)::Variable.Map,
                first(vis),
            )
            b.name_to_con = nothing
            delete!(b.con_to_name, ci)
            delete!(Variable.bridges(b)::Variable.Map, vis)
        else
            for vi in vis
                MOI.delete(b, vi)
            end
        end
    else
        inner_vis = [outer_to_inner(b, vi) for vi in vis]
        MOI.delete(b.model, inner_vis)
        _remove_inner_variable_mapping!(b, vis)
    end
    return
end

function MOI.delete(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    if is_objective_bridged(b)
        _delete_variables_in_bridged_objective(b, [vi])
    end
    if Constraint.has_bridges(Constraint.bridges(b))
        _delete_variables_in_variables_constraints(b, [vi])
    end
    if is_bridged(b, vi)
        MOI.throw_if_not_valid(b, vi)
        v_map = Variable.bridges(b)::Variable.Map
        if Variable.length_of_vector_of_variables(v_map, vi) > 1
            set = Variable.constrained_set(v_map, vi)
            if MOI.supports_dimension_update(set)
                call_in_context(MOI.delete, b, vi, _index(b, vi)...)
            else
                MOI.Utilities.throw_delete_variable_in_vov(vi)
            end
        else
            call_in_context(MOI.delete, b, vi)
            ci = Variable.constraint(v_map, vi)
            b.name_to_con = nothing
            delete!(b.con_to_name, ci)
        end
        delete!(v_map, vi)
        b.name_to_var = nothing
        delete!(b.var_to_name, vi)
    else
        inner_vi = outer_to_inner(b, vi)
        MOI.delete(b.model, inner_vi)
        _remove_inner_variable_mapping!(b, vi)
    end
    return
end

"""
    _remove_inner_variable_mapping!(b, vi)
    _remove_inner_variable_mapping!(b, vis)

Drop the outer↔inner translation entries for the just-deleted variable(s).
No-op when the mapping isn't active or `b` doesn't own a `Variable.Map`.
"""
function _remove_inner_variable_mapping!(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    map = Variable.bridges(b)
    if !Variable.has_bridges(map)
        return
    end
    inner_vi = get(map.outer_to_inner.var_map, vi, nothing)
    if inner_vi !== nothing
        delete!(map.outer_to_inner, vi)
        delete!(map.inner_to_outer, inner_vi)
    end
    return
end

function _remove_inner_variable_mapping!(
    b::AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
)
    for vi in vis
        _remove_inner_variable_mapping!(b, vi)
    end
    return
end

"""
    _remove_inner_constraint_mapping!(b, outer_ci, inner_ci)

Drop the outer↔inner translation entries for the just-deleted constraint.
No-op for `CI{VariableIndex,S}` (derived from the variable mapping) and
when the `(F, S)` constraint mapping isn't active.
"""
function _remove_inner_constraint_mapping!(
    b::AbstractBridgeOptimizer,
    outer_ci::MOI.ConstraintIndex{F,S},
    inner_ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    map = Variable.bridges(b)
    if map isa Variable.Map &&
       Variable.is_constraint_mapping_active(map, F, S)
        delete!(map.outer_to_inner, outer_ci)
        delete!(map.inner_to_outer, inner_ci)
    end
    return
end

function _remove_inner_constraint_mapping!(
    ::AbstractBridgeOptimizer,
    ::MOI.ConstraintIndex{MOI.VariableIndex,S},
    ::MOI.ConstraintIndex{MOI.VariableIndex,S},
) where {S}
    return
end

function MOI.delete(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F},
) where {F}
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        br = bridge(b, ci)
        if is_variable_bridged(b, ci)
            throw(
                MOI.DeleteNotAllowed(
                    ci,
                    "Cannot delete constraint index of bridged constrained " *
                    "variables. Delete the scalar variable or the vector of " *
                    "variables instead.",
                ),
            )
        else
            delete!(Constraint.bridges(b)::Constraint.Map, ci)
        end
        if F === MOI.VariableIndex &&
           is_bridged(b, MOI.VariableIndex(ci.value))
            # Constraint on a bridged variable so we need to remove the flag
            # if it is a bound
            MOI.delete(Variable.bridges(b), ci)
        end
        Variable.call_in_context(
            Variable.bridges(b),
            ci,
            () -> MOI.delete(recursive_model(b), br),
        )
        b.name_to_con = nothing
        delete!(b.con_to_name, ci)
    else
        inner_ci = outer_to_inner(b, ci)
        MOI.delete(b.model, inner_ci)
        _remove_inner_constraint_mapping!(b, ci, inner_ci)
    end
    return
end

function MOI.delete(
    b::AbstractBridgeOptimizer,
    ci::Vector{<:MOI.ConstraintIndex},
)
    if any(c -> is_bridged(b, c), ci)
        # This is a potentially complicated case, so default to the slow case of
        # deleting each constraint one-by-one.
        MOI.delete.(b, ci)
    else
        inner_cis = [outer_to_inner(b, c) for c in ci]
        MOI.delete(b.model, inner_cis)
        for (outer_c, inner_c) in zip(ci, inner_cis)
            _remove_inner_constraint_mapping!(b, outer_c, inner_c)
        end
    end
    return
end

# Attributes

function _get_all_including_bridged(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfVariableIndices,
)
    # `bridge_var_map` is going to map variable indices in `b.model` to their
    # bridged variable indices. If the bridge adds multiple variables, we need
    # only to map the first variable, and we can skip the rest. To mark this
    # distinction, the tail variables are set to `nothing`.
    map = Variable.bridges(b)
    bridge_var_map = Dict{MOI.VariableIndex,Union{Nothing,MOI.VariableIndex}}()
    # These are variables which appear in `b` but do NOT appear in `b.model`.
    # One reason might be the Zero bridge in which they are replaced by `0.0`.
    user_only_variables = MOI.VariableIndex[]
    for (user_variable, bridge) in map
        variables = MOI.get(bridge, MOI.ListOfVariableIndices())
        if isempty(variables)
            # This bridge maps `user_variable` to constants in `b.model`. We
            # still need to report back `user_variable` to the user. In
            # addition, it might represent the start of a VectorOfVariables
            # function, so we may need to report multiple variables.
            push!(user_only_variables, user_variable)
            n = Variable.length_of_vector_of_variables(map, user_variable)
            for i in 1:(n-1)
                # Members of a bridged vector of variables have consecutive
                # ascending outer values.
                push!(
                    user_only_variables,
                    MOI.VariableIndex(user_variable.value + i),
                )
            end
        else
            # This bridge maps `user_variable` to a list of `variables`. We need
            # to swap out only the `first` variable. The others can be flagged
            # with `nothing`. To simplify the first/tail loop, we set
            # `first(variables)` twice; first to `nothing` and then to
            # `user_variable`.
            for bridged_variable in variables
                bridge_var_map[bridged_variable] = nothing
            end
            bridge_var_map[first(variables)] = user_variable
        end
    end
    # We're about to loop over the variables in `.model`, ordered by when they
    # were added to the model. We need to return a list of the original
    # user-space variables in the order that they were added. To do so, we need
    # to undo any Variable.bridges transformations.
    ret = MOI.VariableIndex[]
    for inner_variable in MOI.get(b.model, attr)
        # `bridge_var_map` is keyed by the indices the bridges received from
        # `recursive_model(b)`. For a `SingleBridgeOptimizer` that is
        # `b.model` (inner indices); for a `LazyBridgeOptimizer` it is `b`
        # itself (outer indices). Translate `inner_variable` to the outer
        # namespace (identity for a bridge-created inner variable, which has
        # no outer counterpart) before the lookup. We must NOT "try the raw
        # inner index first": under positive indexing an inner variable may
        # coincidentally share a value with an unrelated outer key, which
        # would produce a wrong (duplicate) match.
        map_b = Variable.bridges(b)
        key = if Variable.has_bridges(map_b)
            get(map_b.inner_to_outer.var_map, inner_variable, inner_variable)
        else
            inner_variable
        end
        outer_variable = get(bridge_var_map, key, missing)
        # If there is a chain of variable bridges, the `outer_variable` may need
        # to be mapped further. This only happens for a `LazyBridgeOptimizer`
        # (`recursive_model(b) === b`), where the chained variables all live in
        # the distinct outer namespace. For a `SingleBridgeOptimizer` the map
        # is a single hop from an inner created variable to its outer user
        # variable, and following the chain would loop forever when the two
        # share a value (for example, an identity bridge: `bridge_var_map[1]
        # == 1`).
        if recursive_model(b) === b
            while haskey(bridge_var_map, outer_variable)
                outer_variable = bridge_var_map[outer_variable]
            end
        end
        if ismissing(outer_variable)
            # Not a bridge-created variable: report its outer translation
            # (`key` is already the outer index, or the unchanged inner index
            # when no translation is in effect).
            push!(ret, key)
        elseif isnothing(outer_variable)
            # inner_variable exists in bridge_var_map, but it is set to `nothing`
            # which means that it is not the first variable in the bridge. Skip
            # it because it should be hidden from the user.
        else
            # inner_variable exists in bridge_var_map. It must be the first
            # variable in the bridge. Report it back to the user.
            push!(ret, outer_variable)
            # `outer_variable` might represent the start of a VectorOfVariables
            # if multiple user-variables were bridged. Add them all. Members
            # have consecutive ascending outer values.
            n = Variable.length_of_vector_of_variables(map, outer_variable)
            for i in 1:(n-1)
                push!(ret, MOI.VariableIndex(outer_variable.value + i))
            end
        end
    end
    # Since these were replaced by constants, we don't actually know when they
    # were added to the model. Tack them on at the end...
    #
    # If you, future reader, find this troublesome, come up with a better
    # solution.
    append!(ret, user_only_variables)
    return ret
end

function _get_all_including_bridged(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    list = MOI.ConstraintIndex{F,S}[]
    is_variable_function = F == MOI.Utilities.variable_function_type(S)
    # A `F`-in-`S` constraint could be added to the model either if it this
    # constraint is not bridged or if variables constrained on creations to `S`
    # are not bridged and `F` is `VariableIndex` or `VectorOfVariables`.
    if !is_bridged(b, F, S) || (is_variable_function && !is_bridged(b, S))
        append!(list, MOI.get(b.model, attr))
    end
    # If variable bridged, get the indices from the variable bridges.
    if is_variable_function && is_bridged(b, S) && is_variable_bridged(b, S)
        append!(list, Variable.constraints_with_set(Variable.bridges(b), S))
    end
    # If constraint bridged, get the indices from the constraint bridges.
    if is_bridged(b, F, S) ||
       (is_variable_function && supports_constraint_bridges(b))
        append!(
            list,
            Constraint.keys_of_type(
                Constraint.bridges(b)::Constraint.Map,
                MOI.ConstraintIndex{F,S},
            ),
        )
    end
    return list
end

function _remove_bridged(list, bridge, attr)
    for c in MOI.get(bridge, attr)
        i = findfirst(isequal(c), list)
        if i !== nothing
            MOI.deleteat!(list, i)
        end
    end
    return
end

# The variable list returned by `_get_all_including_bridged` is already
# expressed in the outer namespace and excludes the bridges' internal
# variables, so there is nothing to remove here. (Removing the bridges' inner
# variables would be incorrect under positive indexing, where an inner
# variable may share a value with an outer user variable; under the old
# negative indexing this method was a no-op because the namespaces were
# value-disjoint.)
_remove_bridged(list, bridge, ::MOI.ListOfVariableIndices) = nothing

# The tactic for this function is to first query all possible indices, and then
# to filter out the indices that have been bridged.
#
# TODO(odow): this seems slightly inefficient, but no one seems to have
# complained that it is a bottleneck.
function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::Union{MOI.ListOfConstraintIndices,MOI.ListOfVariableIndices},
)
    list = _get_all_including_bridged(b, attr)
    for bridge in values(Variable.bridges(b))
        _remove_bridged(list, bridge, attr)
    end
    for bridge in values(Constraint.bridges(b))
        _remove_bridged(list, bridge, attr)
    end
    for bridge in values(Objective.bridges(b))
        _remove_bridged(list, bridge, attr)
    end
    return list
end

function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.NumberOfVariables)::Int64
    s = MOI.get(b.model, attr)
    s += Variable.number_of_variables(Variable.bridges(b))
    for bridge in values(Variable.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    for bridge in values(Constraint.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    for bridge in values(Objective.bridges(b))
        s -= MOI.get(bridge, attr)
    end
    return s
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.NumberOfConstraints{F,S},
)::Int64 where {F,S}
    num = Int64(0)
    is_variable_function = F == MOI.Utilities.variable_function_type(S)
    # A `F`-in-`S` constraint could be added to the model either if it this
    # constraint is not bridged or if variables constrained on creations to `S`
    # are not bridged and `F` is `VariableIndex` or `VectorOfVariables`.
    if !is_bridged(b, F, S) || (is_variable_function && !is_bridged(b, S))
        num += MOI.get(b.model, attr)
    end
    # If variable bridged, get the indices from the variable bridges.
    if is_variable_function && is_bridged(b, S) && is_variable_bridged(b, S)
        num += Variable.number_with_set(Variable.bridges(b), S)
    end
    # If constraint bridged, get the indices from the constraint bridges.
    if is_bridged(b, F, S) ||
       (is_variable_function && supports_constraint_bridges(b))
        num += Constraint.number_of_type(
            Constraint.bridges(b),
            MOI.ConstraintIndex{F,S},
        )
    end
    for bridge in values(Variable.bridges(b))
        num -= MOI.get(bridge, attr)
    end
    for bridge in values(Constraint.bridges(b))
        num -= MOI.get(bridge, attr)
    end
    for bridge in values(Objective.bridges(b))
        num -= MOI.get(bridge, attr)
    end
    return num
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfConstraintTypesPresent,
)
    list_of_types = MOI.get(b.model, attr)
    if Constraint.has_bridges(Constraint.bridges(b))
        append!(
            list_of_types,
            Constraint.list_of_key_types(Constraint.bridges(b)::Constraint.Map),
        )
    end
    if Variable.has_bridges(Variable.bridges(b))
        append!(
            list_of_types,
            Variable.list_of_constraint_types(
                Variable.bridges(b)::Variable.Map,
            ),
        )
    end
    unique!(list_of_types)
    # Some constraint types show up in `list_of_types` including when all the
    # constraints of that type have been created by bridges and not by the user.
    # The code in `NumberOfConstraints` takes care of removing these constraints
    # from the counter so we can rely on it to remove these constraint types.
    filter!(list_of_types) do (F, S)
        return MOI.get(b, MOI.NumberOfConstraints{F,S}()) > 0
    end
    return list_of_types
end

# Model an optimizer attributes
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfModelAttributesSet)
    list = MOI.get(b.model, attr)
    if is_objective_bridged(b)
        list = copy(list)
        # There should be a `MOI.ObjectiveFunction` in `list` otherwise
        # `is_objective_bridged` would return `false`.
        deleteat!(list, findfirst(attr -> attr isa MOI.ObjectiveFunction, list))
        push!(
            list,
            MOI.ObjectiveFunction{MOI.get(b, MOI.ObjectiveFunctionType())}(),
        )
    end
    return unbridged_function(b, list)
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::Union{MOI.AbstractModelAttribute,MOI.AbstractOptimizerAttribute},
)
    return _unbridged_result_from_inner(b, MOI.get(b.model, attr))
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfVariablesWithAttributeSet,
)
    if Variable.has_bridges(Variable.bridges(b))
        # If there are variable bridges, `MOI.get(b.model, attr)`
        # will return a list containing solver variables that do not
        # correspond to any user variables.
        # We choose the easy option of simply returning all variables
        # for now.
        return MOI.get(b, MOI.ListOfVariableIndices())
    else
        return unbridged_function(b, MOI.get(b.model, attr))
    end
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfConstraintsWithAttributeSet{F,S,MOI.ConstraintName},
) where {F,S}
    if !is_bridged(b, F, S) &&
       MOI.supports(b.model, MOI.ConstraintName(), MOI.ConstraintIndex{F,S})
        return MOI.get(b.model, attr)
    end
    return MOI.ConstraintIndex{F,S}[
        ci for ci in keys(b.con_to_name) if ci isa MOI.ConstraintIndex{F,S}
    ]
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ListOfConstraintAttributesSet{F,S},
) where {F,S}
    list = unbridged_function(b, MOI.get(b.model, attr))
    if !(MOI.ConstraintName() in list)
        for key in keys(b.con_to_name)
            if key isa MOI.ConstraintIndex{F,S}
                push!(list, MOI.ConstraintName())
                break
            end
        end
    end
    return list
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::Union{MOI.AbstractModelAttribute,MOI.AbstractOptimizerAttribute},
    value,
)
    MOI.set(b.model, attr, _to_inner_value(b, bridged_function(b, value)))
    return
end

"""
    bridging_cost(b::AbstractBridgeOptimizer, S::Type{<:MOI.AbstractSet}})

Return the cost of bridging variables constrained in `S` on creation,
`is_bridged(b, S)` is assumed to be `true`.

    bridging_cost(
        b::AbstractBridgeOptimizer,
        F::Type{<:MOI.AbstractFunction},
        S::Type{<:MOI.AbstractSet},
    )

Return the cost of bridging `F`-in-`S` constraints.

`is_bridged(b, S)` is assumed to be `true`.
"""
function bridging_cost end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.VariableBridgingCost{S},
) where {S}
    if is_bridged(b, S)
        return bridging_cost(b, S)
    else
        return MOI.get(b.model, attr)
    end
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintBridgingCost{F,S},
) where {F,S}
    if is_bridged(b, F, S)
        return bridging_cost(b, F, S)
    else
        return MOI.get(b.model, attr)
    end
end

"""
    _bridged_function(b::AbstractBridgeOptimizer, ::Union{MOI.ObjectiveFunction, MOI.ConstraintIndex})

Returns the objective function (resp constraint function) as seen by the
objective bridge (resp. constraint bridge) if it is bridged or by the inner
model otherwise. That is, all the bridged variables (except those created by the
bridge or by one of the bridges downstream of this bridge) have been
substituted.

This function cannot be called for a constraint index that is variable bridged.
"""
function _bridged_function end

function _bridged_function(
    b::AbstractBridgeOptimizer,
    attr::MOI.ObjectiveFunction,
)
    if is_bridged(b, attr)
        return MOI.get(recursive_model(b), attr, bridge(b, attr))
    else
        return MOI.get(b.model, attr)
    end
end

function _bridged_function(b::AbstractBridgeOptimizer, ci::MOI.ConstraintIndex)
    attr = MOI.ConstraintFunction()
    if is_bridged(b, ci)
        @assert !is_variable_bridged(b, ci)
        return call_in_context(MOI.get, b, ci, MOI.ConstraintFunction())
    else
        return MOI.get(b.model, attr, ci)
    end
end

"""
    _bridged_function(b::AbstractBridgeOptimizer, ::Union{MOI.ObjectiveFunction, MOI.ConstraintIndex})

Returns the objective function (resp constraint function) as seen by the
user. That is, none of the bridged variables (except those created by any
bridge upstream) have been substituted.
"""
function _unbridged_function end

_unbridged_function(b, obj::MOI.ObjectiveFunction) = MOI.get(b, obj)

function _unbridged_function(b, ci::MOI.ConstraintIndex)
    return MOI.get(b, MOI.ConstraintFunction(), ci)
end

# Objective

"""
    is_objective_bridged(b::AbstractBridgeOptimizer)

Return a `Bool` indicating whether the objective is bridged. The objective is
said to be bridged if the value of `MOI.ObjectiveFunctionType` is different for
`b` and `b.model`.
"""
is_objective_bridged(b) = !isempty(Objective.bridges(b))

function _delete_objective_bridges(b)
    MOI.delete(b, Objective.root_bridge(Objective.bridges(b)::Objective.Map))
    empty!(Objective.bridges(b))
    return
end

function MOI.supports(
    b::AbstractBridgeOptimizer,
    attr::MOI.ObjectiveFunction{F},
) where {F}
    if is_bridged(b, F)
        return supports_bridging_objective_function(b, F)
    else
        return MOI.supports(b.model, attr)
    end
end

"""
    struct ObjectiveFunctionValue{F<:MOI.AbstractFunction} end

Attribute for the value of the objective function of type `F`. If the objective
of the objective function does not depend on `F`, the type `F` determines
whether the computation is redirected to an objective bridge or to the
underlying model.
"""
struct ObjectiveFunctionValue{F<:MOI.AbstractFunction}
    result_index::Int
end

# `recursive_model(b::Objective.SingleBridgeOptimizer)` returns
# `b.model` so any model should implement `ObjectiveFunctionValue`.
function MOI.get(model::MOI.ModelLike, attr::ObjectiveFunctionValue)
    return MOI.get(model, MOI.ObjectiveValue(attr.result_index))
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::ObjectiveFunctionValue{F},
) where {F<:MOI.AbstractFunction} # Need `<:` to avoid ambiguity
    obj_attr = MOI.ObjectiveFunction{F}()
    if is_bridged(b, obj_attr)
        return MOI.get(recursive_model(b), attr, bridge(b, obj_attr))
    else
        return MOI.get(b.model, MOI.ObjectiveValue(attr.result_index))
    end
end

function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveValue)
    if is_objective_bridged(b)
        F = Objective.function_type(Objective.bridges(b))
        return MOI.get(b, ObjectiveFunctionValue{F}(attr.result_index))
    else
        return MOI.get(b.model, attr)
    end
end

function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunctionType)
    if is_objective_bridged(b)
        return Objective.function_type(Objective.bridges(b))
    else
        return MOI.get(b.model, attr)
    end
end

function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveSense)
    return MOI.get(b.model, attr)
end

function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ObjectiveFunction)
    if is_bridged(b, attr)
        value = MOI.get(recursive_model(b), attr, bridge(b, attr))
        return _unbridged_result_from_bridge(b, value)
    end
    return _unbridged_result_from_inner(b, MOI.get(b.model, attr))
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.ObjectiveSense,
    value::MOI.OptimizationSense,
)
    MOI.set(b.model, attr, value)
    if is_objective_bridged(b)
        if value == MOI.FEASIBILITY_SENSE
            _delete_objective_bridges(b)
        else
            for bridge in values(Objective.bridges(b))
                MOI.set(recursive_model(b), attr, bridge, value)
            end
        end
    end
    return
end

function _bridge_objective(b, BridgeType, f)
    bridge = Objective.bridge_objective(BridgeType, recursive_model(b), f)
    Objective.add_key_for_bridge(Objective.bridges(b)::Objective.Map, bridge, f)
    return
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.ObjectiveFunction,
    func::MOI.AbstractFunction,
)
    if is_objective_bridged(b)
        # Clear objective function by setting sense to `MOI.FEASIBILITY_SENSE`
        # first. This is needed if the objective function of `b.model` is
        # `slack` where `slack` is the slack variable
        # created by `Objective.SlackBridge`.
        sense = MOI.get(b.model, MOI.ObjectiveSense())
        MOI.set(b.model, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
        _delete_objective_bridges(b)
        if sense != MOI.FEASIBILITY_SENSE
            MOI.set(b.model, MOI.ObjectiveSense(), sense)
        end
    end
    if Variable.has_bridges(Variable.bridges(b))
        if func isa MOI.VariableIndex
            if is_bridged(b, func)
                BridgeType = Objective.concrete_bridge_type(
                    objective_functionize_bridge(b),
                    typeof(func),
                )
                _bridge_objective(b, BridgeType, func)
                return
            end
        else
            func = bridged_function(b, func)::typeof(func)
        end
    end
    if is_bridged(b, typeof(func))
        BridgeType = Objective.concrete_bridge_type(b, typeof(func))
        _bridge_objective(b, BridgeType, func)
    else
        MOI.set(b.model, attr, func)
    end
    return
end

struct ModifyBridgeNotAllowed{C<:MOI.AbstractFunctionModification} <:
       MOI.NotAllowedError
    change::C
    message::String

    function ModifyBridgeNotAllowed(
        change::MOI.AbstractFunctionModification,
        msg::String = "",
    )
        return new{typeof(change)}(change, msg)
    end
end

function MOI.operation_name(err::ModifyBridgeNotAllowed)
    return "Modifying the bridge with $(err.change)"
end

function MOI.modify(
    ::MOI.ModelLike,
    ::AbstractBridge,
    change::MOI.AbstractFunctionModification,
)
    return throw(ModifyBridgeNotAllowed(change))
end

"""
    _to_inner_change(b::AbstractBridgeOptimizer, change)

Translate the variable indices referenced by a function modification from
the outer to the inner namespace. The caller has already established that
the referenced variables are not bridged.
"""
_to_inner_change(::AbstractBridgeOptimizer, change) = change

function _to_inner_change(
    b::AbstractBridgeOptimizer,
    change::MOI.ScalarCoefficientChange,
)
    return MOI.ScalarCoefficientChange(
        outer_to_inner(b, change.variable),
        change.new_coefficient,
    )
end

function _to_inner_change(
    b::AbstractBridgeOptimizer,
    change::MOI.MultirowChange,
)
    return MOI.MultirowChange(
        outer_to_inner(b, change.variable),
        change.new_coefficients,
    )
end

function _to_inner_change(
    b::AbstractBridgeOptimizer,
    change::MOI.ScalarQuadraticCoefficientChange,
)
    return MOI.ScalarQuadraticCoefficientChange(
        outer_to_inner(b, change.variable_1),
        outer_to_inner(b, change.variable_2),
        change.new_coefficient,
    )
end

function _modify_bridged_function(
    b::AbstractBridgeOptimizer,
    ci_or_obj,
    change::MOI.AbstractFunctionModification,
)
    if is_bridged(b, ci_or_obj)
        MOI.modify(recursive_model(b), bridge(b, ci_or_obj), change)
    else
        MOI.modify(b.model, ci_or_obj, _to_inner_change(b, change))
    end
    return
end

function MOI.modify(
    b::AbstractBridgeOptimizer,
    obj::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if is_bridged(b, change)
        modify_bridged_change(b, obj, change)
    else
        _modify_bridged_function(b, obj, change)
    end
    return
end

# Variable attributes
function _index(b::AbstractBridgeOptimizer, vi::MOI.VariableIndex)
    map = Variable.bridges(b)::Variable.Map
    i = Variable.index_in_vector_of_variables(map, vi)
    if iszero(i.value)
        return tuple()
    else
        return (i,)
    end
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractVariableAttribute,
    index::MOI.VariableIndex,
)
    if is_bridged(b, index)
        value = call_in_context(MOI.get, b, index, attr, _index(b, index)...)
        return _unbridged_result_from_bridge(b, value)
    end
    value = MOI.get(b.model, attr, outer_to_inner(b, index))
    return _unbridged_result_from_inner(b, value)
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractVariableAttribute,
    indices::Vector{MOI.VariableIndex},
)
    # `Variable.has_bridges` is used as a shortcut to speedup in case no variable bridge is used
    if Variable.has_bridges(Variable.bridges(b)) &&
       any(index -> is_bridged(b, index), indices)
        return MOI.get.(b, attr, indices)
    else
        inner_indices = [outer_to_inner(b, vi) for vi in indices]
        values = MOI.get(b.model, attr, inner_indices)
        return _unbridged_result_from_inner.(b, values)
    end
end

function MOI.supports(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractVariableAttribute,
    ::Type{MOI.VariableIndex},
)
    # `supports` should only return `false` in case `MOI.set` always errors.
    # If some variables are bridged by bridges that do not support `attr`, we
    # should therefore still return `true`.
    return MOI.supports(b.model, attr, MOI.VariableIndex)
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractVariableAttribute,
    index::MOI.VariableIndex,
    value,
)
    value = bridged_function(b, value)
    if is_bridged(b, index)
        call_in_context(MOI.set, b, index, attr, value, _index(b, index)...)
    else
        MOI.set(
            b.model,
            attr,
            outer_to_inner(b, index),
            _to_inner_value(b, value),
        )
    end
    return
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractVariableAttribute,
    indices::Vector{MOI.VariableIndex},
    values::Vector,
)
    if any(index -> is_bridged(b, index), indices)
        MOI.set.(b, attr, indices, values)
    else
        inner_indices = [outer_to_inner(b, vi) for vi in indices]
        inner_values =
            [_to_inner_value(b, bridged_function(b, v)) for v in values]
        MOI.set(b.model, attr, inner_indices, inner_values)
    end
    return
end

## MOI.ConstraintFunction

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{F},
    func,
) where {F}
    if !(func isa F)
        throw(MOI.FunctionTypeMismatch{F,typeof(func)}())
    end
    # If there are variable bridges, the function we are setting might need
    # to be mapped through the bridge substitution. This includes moving
    # constants into the set.
    if Variable.has_bridges(Variable.bridges(b))
        set = MOI.get(b, MOI.ConstraintSet(), ci)
        # This moves any constant in func over to set, and returns new functions
        # and sets.
        new_func, new_set = bridged_constraint_function(b, func, set)
        _set_substituted(b, attr, ci, new_func)
        if new_set !== set
            # We only need to update the set if it has changed.
            _set_substituted(b, MOI.ConstraintSet(), ci, new_set)
        end
    else
        _set_substituted(b, attr, ci, func)
    end
    return
end

"""
    _set_substituted(
        b::AbstractBridgeOptimizer,
        attr::MOI.AbstractConstraintAttribute,
        ci::MOI.ConstraintIndex,
        value,
    )

A helpful wrapper that calls `call_in_context` if `ci` is bridged, and `MOI.set`
if not.
"""
function _set_substituted(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
    value,
)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        call_in_context(MOI.set, b, ci, attr, value)
    else
        MOI.set(b.model, attr, outer_to_inner(b, ci), _to_inner_value(b, value))
    end
    return
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex,
)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        if is_variable_bridged(b, ci)
            # If it is a variable bridge, we can get the original variable quite
            # easily.
            return Variable.function_for(Variable.bridges(b)::Variable.Map, ci)
        else
            # Otherwise, we need to query ConstraintFunction in the context of
            # the bridge...
            func = call_in_context(MOI.get, b, ci, attr)
            if recursive_model(b) !== b
                # The bridge expressed `func` in `b.model`'s namespace;
                # translate the non-bridge-created indices to the outer
                # namespace before unbridging.
                func = _from_inner_value(b, func)
            end
            # and then unbridge this function (because it may contain variables
            # that are themselves bridged).
            return unbridged_constraint_function(b, func)
        end
    else
        # This constraint is not bridged, but it might contain variables that
        # are.
        func = _from_inner_value(
            b,
            MOI.get(b.model, attr, outer_to_inner(b, ci)),
        )
        return unbridged_constraint_function(b, func)
    end
end

## MOI.ConstraintSet

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,S},
    value,
) where {S<:MOI.AbstractScalarSet}
    if !(value isa S)
        throw(MOI.SetTypeMismatch{S,typeof(value)}())
    end
    _set_substituted(b, attr, ci, value)
    return
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{MOI.VariableIndex},
)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        return call_in_context(MOI.get, b, ci, attr)
    else
        return MOI.get(b.model, attr, outer_to_inner(b, ci))
    end
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction,S},
    set,
) where {S<:MOI.AbstractScalarSet}
    if !(set isa S)
        throw(MOI.SetTypeMismatch{S,typeof(set)}())
    end
    # If there are variable bridges, the set we are setting might need to be
    # mapped through the bridge substitution. This includes moving constants
    # into the set.
    if Variable.has_bridges(Variable.bridges(b))
        if is_bridged(b, ci)
            # If `ci` is also ConstraintBridged, then we give up as the current code path is known to contain unresolved issues, see https://github.com/jump-dev/MathOptInterface.jl/issues/2452
            throw(MOI.SetAttributeNotAllowed(attr))
        end
        func = MOI.get(b, MOI.ConstraintFunction(), ci)
        # Updating the set will not modify the function, so we don't care about
        # the first argument. We only care about the new set.
        _, set = bridged_constraint_function(b, func, set)
    end
    _set_substituted(b, attr, ci, set)
    return
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction,S},
) where {S}
    set = if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        call_in_context(MOI.get, b, ci, attr)
    else
        MOI.get(b.model, attr, outer_to_inner(b, ci))
    end
    # This is a scalar function, so if there are variable bridges, it might
    # contain constants that have been moved into the set.
    if !Variable.has_bridges(Variable.bridges(b))
        # If there are no variable bridges, return the set.
        return set
    elseif !MOI.Utilities.supports_shift_constant(S)
        # If it doesn't support shift_constant, then return the set
        return set
    end
    # When the constraint is added with function `f` and set `set_f`,
    # the function is bridged into `g` with set `set` by
    # `g, set = bridged_constraint_function(b, f, set_f)`.
    # By doing so, the function constant of the bridged function (if it exists)
    # was moved to `set`, we need to remove it to recover `set_f`.
    # The function `f` contains the variables in the context of `ci`
    # (which should match `Variable.bridges(b).current_context` since no code
    # outside of that context has references to `ci`) and
    # the constraint `g` contains the variables of `b.model`.
    # The following line recovers `f` in the context of
    # `Variables.map(b).current_context`.
    f = MOI.get(b, MOI.ConstraintFunction(), ci)
    # We need to substitute the variable bridges to recover the function `g`
    # that was given at the creation of the bridge.
    g = bridged_function(b, f)
    # Since `bridged_constraint_function(b, f, set_f)` used
    # `set = shift_constant(set_f, -MOI.constant(g))`, we need
    # to do the opposite to recover `set_f`.
    return MOI.Utilities.shift_constant(set, MOI.constant(g))
end

## Other constraint attributes

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
)
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        func = call_in_context(MOI.get, b, ci, attr)
        return _unbridged_result_from_bridge(b, func)
    end
    func = MOI.get(b.model, attr, outer_to_inner(b, ci))
    return _unbridged_result_from_inner(b, func)
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintPrimal,
    ci::MOI.ConstraintIndex{F,S},
) where {F<:MOI.AbstractScalarFunction,S<:MOI.AbstractScalarSet}
    if is_bridged(b, ci)
        MOI.throw_if_not_valid(b, ci)
        return call_in_context(MOI.get, b, ci, attr)
    end
    if Variable.has_bridges(Variable.bridges(b))
        # In this case, the scalar constraint might contain bridged variables
        # that include constants, like `x >= 1` being mapped to `y >= 0`,
        # `x := y + 1`. If this is true, then `normalize_and_add_constraint`
        # may move the constants into the set, and querying `ConstraintPrimal`
        # from `b.model` will return the value of `y`, not `y + 1`. As a
        # work-around, we use `get_fallback`, which first queries
        # `ConstraintFunction` and then evaluates the function at the point
        # defined by `VariablePrimal`. Querying `ConstraintFunction` accounts
        # for the case where constants were moved to the set, so we return the
        # correct value.
        return MOI.Utilities.get_fallback(b, attr, ci)
    end
    return MOI.get(b.model, attr, outer_to_inner(b, ci))
end

function MOI.supports(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    ::Type{MOI.ConstraintIndex{F,S}},
) where {F,S}
    # !!! warning
    #     This function is slightly confusing, because we need to account for
    #     the different ways in which a constraint might be added.
    if F == MOI.Utilities.variable_function_type(S)
        # These are VariableIndex and VectorOfVariable constraints.
        if is_bridged(b, S)
            # If S needs to be bridged, it usually means that either there is a
            # variable bridge, or that there is a free variable followed by a
            # constraint bridge (that is, the two cases handled below).
            #
            # However, it might be the case, like the tests in
            # Variable/flip_sign.jl, that the model supports F-in-S constraints,
            # but force-bridges S sets. If so, we might be in the unusual
            # situation where we support the attribute if the index was added
            # via add_constraint, but not if it was added via
            # add_constrained_variable. Because MOI lacks the ability to tell
            # which happened just based on the type, we're going to default to
            # asking the variable bridge, at the risk of a false negative.
            if is_variable_bridged(b, S)
                bridge = Variable.concrete_bridge_type(b, S)
                return MOI.supports(recursive_model(b), attr, bridge)
            else
                bridge = Constraint.concrete_bridge_type(b, F, S)
                return MOI.supports(recursive_model(b), attr, bridge)
            end
        else
            # If S doesn't need to be bridged, it usually means that either the
            # solver supports add_constrained_variable, or it supports free
            # variables and add_constraint.
            #
            # In some cases, it might be that the solver supports
            # add_constrained_variable, but ends up bridging add_constraint.
            # Because MOI lacks the ability to tell which one was called based
            # on the index type, asking the model might give a false negative
            # (we support the attribute via add_constrained_variable, but the
            # bridge doesn't via add_constraint because it will be bridged).
            return MOI.supports(b.model, attr, MOI.ConstraintIndex{F,S})
        end
    else
        # These are normal add_constraints, so we just check if they are
        # bridged.
        if is_bridged(b, F, S)
            bridge = Constraint.concrete_bridge_type(b, F, S)
            return MOI.supports(recursive_model(b), attr, bridge)
        else
            return MOI.supports(b.model, attr, MOI.ConstraintIndex{F,S})
        end
    end
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
    value,
)
    _set_substituted(b, attr, ci, bridged_function(b, value))
    return
end

function MOI.set(
    ::AbstractBridgeOptimizer,
    ::MOI.ConstraintFunction,
    ::MOI.ConstraintIndex{MOI.VariableIndex},
    ::MOI.VariableIndex,
)
    return throw(MOI.SettingVariableIndexNotAllowed())
end

_f_type(::Type{S}) where {S<:MOI.AbstractScalarSet} = MOI.VariableIndex
_f_type(::Type{S}) where {S<:MOI.AbstractVectorSet} = MOI.VectorOfVariables

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintConflictStatus,
    bridge::AbstractBridge,
)
    ret = MOI.NOT_IN_CONFLICT
    for (F, S) in MOI.Bridges.added_constraint_types(typeof(bridge))
        for ci in MOI.get(bridge, MOI.ListOfConstraintIndices{F,S}())
            status = MOI.get(model, attr, ci)
            if status == MOI.IN_CONFLICT
                return status
            elseif status == MOI.MAYBE_IN_CONFLICT
                ret = status
            end
        end
    end
    for (S,) in MOI.Bridges.added_constrained_variable_types(typeof(bridge))
        F = _f_type(S)
        for ci in MOI.get(bridge, MOI.ListOfConstraintIndices{F,S}())
            status = MOI.get(model, attr, ci)
            if status == MOI.IN_CONFLICT
                return status
            elseif status == MOI.MAYBE_IN_CONFLICT
                ret = status
            end
        end
    end
    return ret
end

## Getting and Setting names
function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.VariableName,
    vi::MOI.VariableIndex,
)
    if is_bridged(b, vi)
        bridge_ = bridge(b, vi)
        if MOI.supports(b, MOI.VariableName(), typeof(bridge_))
            # The bridge's indices live in `recursive_model(b)`'s index
            # space, so its attribute getters must be called with it.
            return MOI.get(recursive_model(b), MOI.VariableName(), bridge_)
        end
        return get(b.var_to_name, vi, "")
    else
        return MOI.get(b.model, attr, outer_to_inner(b, vi))
    end
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.VariableName,
    vi::MOI.VariableIndex,
    name::String,
)
    if is_bridged(b, vi)
        bridge_ = bridge(b, vi)
        if MOI.supports(b, MOI.VariableName(), typeof(bridge_))
            # The bridge's indices live in `recursive_model(b)`'s index
            # space, so its attribute setters must be called with it.
            MOI.set(recursive_model(b), MOI.VariableName(), bridge_, name)
        else
            b.var_to_name[vi] = name
            b.name_to_var = nothing # Invalidate the name map.
        end
    else
        MOI.set(b.model, attr, outer_to_inner(b, vi), name)
    end
    return
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintName,
    constraint_index::MOI.ConstraintIndex,
)
    if is_bridged(b, constraint_index)
        return get(b.con_to_name, constraint_index, "")
    else
        return MOI.get(b.model, attr, outer_to_inner(b, constraint_index))
    end
end

function MOI.supports(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintName,
    Index::Type{MOI.ConstraintIndex{F,S}},
) where {F,S}
    return is_bridged(b, F, S) || MOI.supports(b.model, attr, Index)
end

function MOI.set(
    b::AbstractBridgeOptimizer,
    attr::MOI.ConstraintName,
    constraint_index::MOI.ConstraintIndex,
    name::String,
)
    if is_bridged(b, constraint_index)
        b.con_to_name[constraint_index] = name
        b.name_to_con = nothing # Invalidate the name map.
    else
        MOI.set(b.model, attr, outer_to_inner(b, constraint_index), name)
    end
    return
end

function MOI.supports(
    ::AbstractBridgeOptimizer,
    ::MOI.ConstraintName,
    ::Type{MOI.ConstraintIndex{MOI.VariableIndex,S}},
) where {S}
    return throw(MOI.VariableIndexConstraintNameError())
end

function MOI.set(
    ::AbstractBridgeOptimizer,
    ::MOI.ConstraintName,
    ::MOI.ConstraintIndex{MOI.VariableIndex,<:MOI.AbstractScalarSet},
    ::String,
)
    return throw(MOI.VariableIndexConstraintNameError())
end

"""
    _outer_variable_for_name_lookup(b::AbstractBridgeOptimizer, vi)

Translate the inner variable `vi` found by a name lookup in `b.model` to
the outer namespace. When the variable mapping is inactive, return `vi`
unchanged. A recorded passthrough variable translates through the index
map. An unrecorded inner variable was created by a bridge whose
`MOI.VariableName` support delegated the user's name to it; reverse that
delegation by searching the variable bridges.
"""
function _outer_variable_for_name_lookup(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    map = Variable.bridges(b)
    if !Variable.has_bridges(map)
        return vi
    end
    if haskey(map.inner_to_outer.var_map, vi)
        return map.inner_to_outer.var_map[vi]
    end
    for (outer_vi, bridge_) in map
        variables = MOI.get(bridge_, MOI.ListOfVariableIndices())
        position = findfirst(isequal(vi), variables)
        if position !== nothing
            # Bridged vectors of variables use consecutive ascending outer
            # values starting at the first variable.
            return MOI.VariableIndex(outer_vi.value + (position - 1))
        end
    end
    return vi
end

# Query index from name (similar to `UniversalFallback`)
function MOI.get(
    b::AbstractBridgeOptimizer,
    ::Type{MOI.VariableIndex},
    name::String,
)
    vi = MOI.get(b.model, MOI.VariableIndex, name)
    if !Variable.has_bridges(Variable.bridges(b))
        return vi
    end
    if vi !== nothing
        # The index returned by `b.model` is in the inner namespace.
        vi = _outer_variable_for_name_lookup(b, vi)
    end
    if b.name_to_var === nothing
        b.name_to_var = MOI.Utilities.build_name_to_var_map(b.var_to_name)
    end
    vi_bridged = get(b.name_to_var, name, nothing)
    MOI.Utilities.throw_if_multiple_with_name(vi_bridged, name)
    return MOI.Utilities.check_type_and_multiple_names(
        MOI.VariableIndex,
        vi_bridged,
        vi,
        name,
    )
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    IdxT::Type{MOI.ConstraintIndex{F,S}},
    name::String,
) where {F,S}
    if !Constraint.has_bridges(Constraint.bridges(b)) &&
       !Variable.has_bridges(Variable.bridges(b))
        # `name_to_con` is not defined for `Objective.SingleBridgeOptimizer`.
        return MOI.get(b.model, IdxT, name)
    end
    if b.name_to_con === nothing
        b.name_to_con = MOI.Utilities.build_name_to_con_map(b.con_to_name)
    end
    if is_bridged(b, F, S)
        # There is no `F`-in-`S` constraint in `b.model`, `ci` is only got
        # to check for duplicate names.
        ci = MOI.get(b.model, MOI.ConstraintIndex, name)
    else
        ci = MOI.get(b.model, IdxT, name)
    end
    if ci !== nothing
        # The index returned by `b.model` is in the inner namespace.
        ci = inner_to_outer(b, ci)
    end
    ci_bridged = get(b.name_to_con, name, nothing)
    MOI.Utilities.throw_if_multiple_with_name(ci_bridged, name)
    return MOI.Utilities.check_type_and_multiple_names(
        IdxT,
        ci_bridged,
        ci,
        name,
    )
end

function MOI.get(
    b::AbstractBridgeOptimizer,
    IdxT::Type{MOI.ConstraintIndex},
    name::String,
)
    if !Constraint.has_bridges(Constraint.bridges(b)) &&
       !Variable.has_bridges(Variable.bridges(b))
        # `name_to_con` is not defined for `Objective.SingleBridgeOptimizer`.
        return MOI.get(b.model, IdxT, name)
    end
    if b.name_to_con === nothing
        b.name_to_con = MOI.Utilities.build_name_to_con_map(b.con_to_name)
    end
    ci = MOI.get(b.model, IdxT, name)
    if ci !== nothing
        # The index returned by `b.model` is in the inner namespace.
        ci = inner_to_outer(b, ci)
    end
    ci_bridged = get(b.name_to_con, name, nothing)
    MOI.Utilities.throw_if_multiple_with_name(ci_bridged, name)
    return MOI.Utilities.check_type_and_multiple_names(
        IdxT,
        ci_bridged,
        ci,
        name,
    )
end

# Constraints
function MOI.supports_constraint(
    b::AbstractBridgeOptimizer,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    if is_bridged(b, F, S)
        return supports_bridging_constraint(b, F, S)
    else
        return MOI.supports_constraint(b.model, F, S)
    end
end

"""
    _is_available_constraint_index(b, ci::MOI.ConstraintIndex)

Return `true` if `ci` can be allocated (by `Constraint.add_key_for_bridge`
for a force-bridged constraint, or by `Variable.add_keys_for_bridge` for a
vector of constrained variables) without clashing with an index already in
use in the outer `(F, S)` namespace, namely:

  * a vector of constrained variables in `Variable.bridges(b)`,
  * a force-/constraint-bridged constraint in `Constraint.bridges(b)`,
  * an inner constraint translated into the outer namespace when the
    `(F, S)` constraint mapping was activated.
"""
function _is_available_constraint_index(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    if MOI.is_valid(Variable.bridges(b), ci)
        return false
    end
    if haskey(Constraint.bridges(b), ci)
        return false
    end
    map = Variable.bridges(b)
    if map isa Variable.Map &&
       Variable.is_constraint_mapping_active(map, F, S) &&
       haskey(map.outer_to_inner.con_map, ci)
        return false
    end
    return true
end

function add_bridged_constraint(b, BridgeType, f, s)
    bridge = Constraint.bridge_constraint(BridgeType, recursive_model(b), f, s)
    # `MOI.VectorOfVariables` constraint indices have negative indices
    # to distinguish between the indices of the inner model.
    # However, they can clash with the indices created by the variable
    # bridges or with inner indices copied into the outer namespace when the
    # constraint mapping was activated, so we use the last argument to
    # inform the constraint bridge mapping about indices already taken.
    ci = Constraint.add_key_for_bridge(
        Constraint.bridges(b)::Constraint.Map,
        bridge,
        f,
        s,
        Base.Fix1(_is_available_constraint_index, b),
    )
    Variable.register_context(Variable.bridges(b), ci)
    return ci
end

function _throw_if_bound_already_set(b, x, ::S2, ::Type{S1}) where {S1,S2}
    ErrorType = _bound_error_type(S1, S2)
    if ErrorType === nothing
        return
    end
    ci = MOI.ConstraintIndex{MOI.VariableIndex,S1}(x.value)
    if (is_bridged(b, S1) && MOI.is_valid(b, ci)) || MOI.is_valid(b.model, ci)
        throw(ErrorType(x))
    end
    return
end

function _bound_error_type(
    ::Type{S1},
    ::Type{S2},
) where {S1<:MOI.LessThan,S2<:MOI.LessThan}
    return MOI.UpperBoundAlreadySet{S1,S2}
end

function _bound_error_type(::Type{S1}, ::Type{S2}) where {S1<:MOI.LessThan,S2}
    return MOI.UpperBoundAlreadySet{S1,S2}
end

function _bound_error_type(::Type{S1}, ::Type{S2}) where {S1,S2<:MOI.LessThan}
    return MOI.UpperBoundAlreadySet{S1,S2}
end

function _bound_error_type(::Type{S1}, ::Type{S2}) where {S1,S2}
    return MOI.LowerBoundAlreadySet{S1,S2}
end

_bound_error_type(::Type{<:MOI.LessThan}, ::Type{<:MOI.GreaterThan}) = nothing

_bound_error_type(::Type{<:MOI.GreaterThan}, ::Type{<:MOI.LessThan}) = nothing

function _check_double_single_variable(
    b::AbstractBridgeOptimizer,
    x::MOI.VariableIndex,
    s::S,
) where {
    T,
    S<:Union{
        MOI.Interval{T},
        MOI.EqualTo{T},
        MOI.Semicontinuous{T},
        MOI.Semiinteger{T},
        MOI.LessThan{T},
        MOI.GreaterThan{T},
    },
}
    # !!! warning
    #     The order here is _very_ important because an Interval constraint
    #     might get re-written into a LessThan and GreaterThan. To throw the
    #     appropriate error, we _must_ check sets like `Interval` and `EqualTo`
    #     _before_ `LessThan` and `GreaterThan`.
    _throw_if_bound_already_set(b, x, s, MOI.Interval{T})
    _throw_if_bound_already_set(b, x, s, MOI.EqualTo{T})
    _throw_if_bound_already_set(b, x, s, MOI.Semicontinuous{T})
    _throw_if_bound_already_set(b, x, s, MOI.Semiinteger{T})
    _throw_if_bound_already_set(b, x, s, MOI.LessThan{T})
    _throw_if_bound_already_set(b, x, s, MOI.GreaterThan{T})
    return
end

_check_double_single_variable(::AbstractBridgeOptimizer, ::Any, ::Any) = nothing

function MOI.add_constraint(
    b::AbstractBridgeOptimizer,
    f::F,
    s::S,
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    if Variable.has_bridges(Variable.bridges(b))
        if F <: MOI.VariableIndex
            if is_bridged(b, f)
                ci = MOI.ConstraintIndex{MOI.VariableIndex,S}(f.value)
                if MOI.is_valid(b, ci)
                    # The other constraint could have been through a variable bridge.
                    msg = "Cannot add two `VariableIndex`-in-`$S` on the same variable $f."
                    throw(MOI.AddConstraintNotAllowed{F,S}(msg))
                end
                BridgeType = Constraint.concrete_bridge_type(
                    constraint_scalar_functionize_bridge(b),
                    F,
                    S,
                )
                MOI.add_constraint(Variable.bridges(b), f, s)
                return add_bridged_constraint(b, BridgeType, f, s)
            end
        elseif F <: MOI.VectorOfVariables
            if any(vi -> is_bridged(b, vi), f.variables)
                # This is the first (or another) force-bridged `F`-in-`S`:
                # from now on the outer and inner `CI{F,S}` namespaces are
                # distinct, so materialize identity entries for all inner
                # constraints of this type before they can drift apart.
                v_map = Variable.bridges(b)
                if v_map isa Variable.Map
                    Variable.activate_constraint_mapping!(v_map, b.model, F, S)
                end
                BridgeType = Constraint.concrete_bridge_type(
                    constraint_vector_functionize_bridge(b),
                    F,
                    S,
                )
                return add_bridged_constraint(b, BridgeType, f, s)
            end
        else
            f, s = bridged_constraint_function(b, f, s)
        end
    end
    if is_bridged(b, F, S)
        _check_double_single_variable(b, f, s)
        # We compute `BridgeType` first as `concrete_bridge_type` calls
        # `bridge_type` which might throw an `UnsupportedConstraint` error in
        # which case, we do not want any modification to have been done
        BridgeType = Constraint.concrete_bridge_type(b, F, S)
        # `add_constraint` might throw an `UnsupportedConstraint` but no
        # modification has been done in the previous line
        return add_bridged_constraint(b, BridgeType, f, s)
    else
        if F <: MOI.VariableIndex || F <: MOI.VectorOfVariables
            # Index functions are never rewritten by
            # `bridged_constraint_function` above, so their variable indices
            # are still in the outer namespace.
            f = _to_inner_index_function(b, f)
        else
            # `bridged_constraint_function` left the substituted function in
            # the outer namespace when `recursive_model(b) === b`.
            f = _to_inner_value(b, f)
        end
        inner_ci = MOI.add_constraint(b.model, f, s)
        return _record_inner_constraint!(b, inner_ci)
    end
end

function MOI.add_constraints(
    b::AbstractBridgeOptimizer,
    f::Vector{F},
    s::Vector{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    if is_bridged(b, F, S)
        return MOI.add_constraint.(b, f, s)
    end
    if Variable.has_bridges(Variable.bridges(b))
        if F == MOI.VariableIndex
            if any(func -> is_bridged(b, func), f)
                return MOI.add_constraint.(b, f, s)
            end
            f = F[_to_inner_index_function(b, func)::F for func in f]
        elseif F == MOI.VectorOfVariables
            if any(func -> any(vi -> is_bridged(b, vi), func.variables), f)
                return MOI.add_constraint.(b, f, s)
            end
            f = F[_to_inner_index_function(b, func)::F for func in f]
        else
            f = F[
                _to_inner_value(b, bridged_function(b, func))::F for func in f
            ]
        end
    end
    inner_cis = MOI.add_constraints(b.model, f, s)
    return [_record_inner_constraint!(b, ci) for ci in inner_cis]
end

function is_bridged(
    b::AbstractBridgeOptimizer,
    ::Union{MOI.ScalarConstantChange,MOI.VectorConstantChange},
)
    return Variable.has_bridges(Variable.bridges(b))
end

function is_bridged(
    b::AbstractBridgeOptimizer,
    change::Union{MOI.ScalarCoefficientChange,MOI.MultirowChange},
)
    return is_bridged(b, change.variable)
end

function is_bridged(
    b::AbstractBridgeOptimizer,
    change::MOI.ScalarQuadraticCoefficientChange,
)
    return is_bridged(b, change.variable_1) || is_bridged(b, change.variable_2)
end

"""
    _modify_substituted_change(b::AbstractBridgeOptimizer, ci_or_obj, change)

Apply a function modification produced by expanding a bridged variable via
[`bridged_variable_function`](@ref). When `recursive_model(b) === b` the
expansion is in the outer namespace, so we re-enter `MOI.modify` for the
usual dispatch. Otherwise the expansion is already in the inner namespace
and we must dispatch directly, bypassing the outer-to-inner translation.
"""
function _modify_substituted_change(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    if recursive_model(b) === b
        MOI.modify(b, ci, change)
    elseif is_bridged(b, ci)
        call_in_context(MOI.modify, b, ci, change)
    else
        MOI.modify(b.model, outer_to_inner(b, ci), change)
    end
    return
end

function _modify_substituted_change(
    b::AbstractBridgeOptimizer,
    obj::MOI.ObjectiveFunction,
    change::MOI.AbstractFunctionModification,
)
    if recursive_model(b) === b
        MOI.modify(b, obj, change)
    elseif is_bridged(b, obj)
        MOI.modify(recursive_model(b), bridge(b, obj), change)
    else
        MOI.modify(b.model, obj, change)
    end
    return
end

function modify_bridged_change(
    b::AbstractBridgeOptimizer,
    ci,
    change::MOI.MultirowChange,
)
    func =
        bridged_variable_function(b, change.variable)::MOI.ScalarAffineFunction
    if !iszero(func.constant)
        # We would need to get the constant in the function, and the
        # coefficient of `change.variable` to remove its contribution
        # to the constant and then modify the constant.
        MOI.throw_modify_not_allowed(
            ci,
            change,
            "The change $change contains variables bridged into" *
            " a function with nonzero constant.",
        )
    end
    for t in func.terms
        coefs =
            [(i, coef * t.coefficient) for (i, coef) in change.new_coefficients]
        _modify_substituted_change(b, ci, MOI.MultirowChange(t.variable, coefs))
    end
    return
end

_constant_change(new_constant) = MOI.ScalarConstantChange(new_constant)
_constant_change(new_constant::Vector) = MOI.VectorConstantChange(new_constant)

function modify_bridged_change(
    b::AbstractBridgeOptimizer,
    ci_or_obj,
    change::Union{MOI.ScalarConstantChange,MOI.VectorConstantChange},
)
    bridged_func = _bridged_function(b, ci_or_obj)
    unbridged_func = _unbridged_function(b, ci_or_obj)
    bridged_const =
        MOI.constant(bridged_func) + change.new_constant -
        MOI.constant(unbridged_func)
    bridged_change = _constant_change(bridged_const)
    _modify_bridged_function(b, ci_or_obj, bridged_change)
    return
end

function modify_bridged_change(
    b::AbstractBridgeOptimizer,
    ci_or_obj,
    change::MOI.ScalarCoefficientChange,
)
    func =
        bridged_variable_function(b, change.variable)::MOI.ScalarAffineFunction
    if !iszero(func.constant)
        # We would need to get the constant in the set, and the
        # coefficient of `change.variable` to remove its contribution
        # to the constant and then modify the constant.
        MOI.throw_modify_not_allowed(
            ci_or_obj,
            change,
            "The change $change contains variables bridged into" *
            " a function with nonzero constant.",
        )
    end
    for t in func.terms
        coef = t.coefficient * change.new_coefficient
        _modify_substituted_change(
            b,
            ci_or_obj,
            MOI.ScalarCoefficientChange(t.variable, coef),
        )
    end
    return
end

function modify_bridged_change(
    b::AbstractBridgeOptimizer,
    ci_or_obj,
    change::MOI.ScalarQuadraticCoefficientChange,
)
    return MOI.throw_modify_not_allowed(
        ci_or_obj,
        change,
        "Cannot bridge `ScalarQuadraticCoefficientChange`.",
    )
end

function MOI.modify(
    b::AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex,
    change::MOI.AbstractFunctionModification,
)
    if is_bridged(b, change)
        modify_bridged_change(b, ci, change)
    else
        if is_bridged(b, ci)
            call_in_context(MOI.modify, b, ci, change)
        else
            MOI.modify(
                b.model,
                outer_to_inner(b, ci),
                _to_inner_change(b, change),
            )
        end
    end
    return
end

# Variables

"""
    _record_inner_variable!(b::AbstractBridgeOptimizer, inner_vi::MOI.VariableIndex)

If `b` uses variable bridges (`Variable.has_bridges`), allocate a fresh
outer `VariableIndex` value and record the bidirectional mapping.
Otherwise (identity mode, or `b` does not own a `Variable.Map` at all),
return `inner_vi` unchanged.
"""
function _record_inner_variable!(
    b::AbstractBridgeOptimizer,
    inner_vi::MOI.VariableIndex,
)
    map = Variable.bridges(b)
    if !Variable.has_bridges(map)
        return inner_vi
    end
    outer_vi = MOI.VariableIndex(Variable.next_outer_variable!(map))
    map.outer_to_inner[outer_vi] = inner_vi
    map.inner_to_outer[inner_vi] = outer_vi
    return outer_vi
end

# Variables
function MOI.add_variable(b::AbstractBridgeOptimizer)
    if is_bridged(b, MOI.Reals)
        variables, constraint = MOI.add_constrained_variables(b, MOI.Reals(1))
        @assert isone(length(variables))
        return first(variables)
    end
    inner_vi = MOI.add_variable(b.model)
    return _record_inner_variable!(b, inner_vi)
end

function MOI.add_variables(b::AbstractBridgeOptimizer, n)
    if is_bridged(b, MOI.Reals)
        variables, constraint = MOI.add_constrained_variables(b, MOI.Reals(n))
        return variables
    end
    inner_vis = MOI.add_variables(b.model, n)
    return MOI.VariableIndex[_record_inner_variable!(b, vi) for vi in inner_vis]
end

# Split in two to avoid ambiguity
function MOI.supports_add_constrained_variables(
    b::AbstractBridgeOptimizer,
    ::Type{MOI.Reals},
)
    if is_bridged(b, MOI.Reals)
        return supports_bridging_constrained_variable(b, MOI.Reals)
    else
        return MOI.supports_add_constrained_variables(b.model, MOI.Reals)
    end
end

function MOI.supports_add_constrained_variables(
    b::AbstractBridgeOptimizer,
    S::Type{<:MOI.AbstractVectorSet},
)
    if is_bridged(b, S)
        return supports_bridging_constrained_variable(b, S)
    else
        return MOI.supports_add_constrained_variables(b.model, S)
    end
end

function MOI.add_constrained_variables(
    b::AbstractBridgeOptimizer,
    set::MOI.AbstractVectorSet,
)
    if !is_bridged(b, typeof(set))
        inner_vis, inner_ci = MOI.add_constrained_variables(b.model, set)
        outer_vis = MOI.VariableIndex[
            _record_inner_variable!(b, vi) for vi in inner_vis
        ]
        return outer_vis, _record_inner_constraint!(b, inner_ci)
    end
    if set isa MOI.Reals || is_variable_bridged(b, typeof(set))
        BridgeType = Variable.concrete_bridge_type(b, typeof(set))
        v_map = Variable.bridges(b)::Variable.Map
        # Activate the outer/inner variable translation map at this layer
        # before allocating the new bridged variables. This ensures every
        # variable visible to the user from now on lives in the outer
        # namespace.
        Variable.activate_variable_mapping!(v_map, b.model)
        # The constrained-variable `CI{VectorOfVariables, S}` shares the outer
        # `(VOV, S)` namespace with inner passthrough constraints and force-
        # bridged constraints. Activate the constraint mapping for `(VOV, S)`
        # so that inner indices are translated out of the way, and pass
        # `_is_available_constraint_index` so the allocation skips any value
        # already taken by `Constraint.bridges` or by an inner constraint.
        Variable.activate_constraint_mapping!(
            v_map,
            b.model,
            MOI.VectorOfVariables,
            typeof(set),
        )
        return Variable.add_keys_for_bridge(
            v_map,
            () -> Variable.bridge_constrained_variable(
                BridgeType,
                recursive_model(b),
                set,
            ),
            set,
            Base.Fix1(_is_available_constraint_index, b),
        )
    else
        variables = MOI.add_variables(b, MOI.dimension(set))
        constraint =
            MOI.add_constraint(b, MOI.VectorOfVariables(variables), set)
        return variables, constraint
    end
end

function MOI.supports_add_constrained_variable(
    b::AbstractBridgeOptimizer,
    S::Type{<:MOI.AbstractScalarSet},
)
    if is_bridged(b, S)
        return supports_bridging_constrained_variable(b, S)
    else
        return MOI.supports_add_constrained_variable(b.model, S)
    end
end

function MOI.add_constrained_variable(
    b::AbstractBridgeOptimizer,
    set::MOI.AbstractScalarSet,
)
    if !is_bridged(b, typeof(set))
        inner_vi, inner_ci = MOI.add_constrained_variable(b.model, set)
        outer_vi = _record_inner_variable!(b, inner_vi)
        # `CI{VariableIndex, S}.value == vi.value` by MOI convention, so the
        # translated constraint index is derived from the variable one.
        return outer_vi, _record_inner_constraint!(b, inner_ci)
    end
    if is_variable_bridged(b, typeof(set))
        BridgeType = Variable.concrete_bridge_type(b, typeof(set))
        # Activate the outer/inner variable translation at this layer before
        # allocating the new bridged variable.
        Variable.activate_variable_mapping!(
            Variable.bridges(b)::Variable.Map,
            b.model,
        )
        return Variable.add_key_for_bridge(
            Variable.bridges(b)::Variable.Map,
            () -> Variable.bridge_constrained_variable(
                BridgeType,
                recursive_model(b),
                set,
            ),
            set,
        )
    else
        variable = MOI.add_variable(b)
        constraint = MOI.add_constraint(b, variable, set)
        return variable, constraint
    end
end

function MOI.supports(b::AbstractBridgeOptimizer, sub::MOI.AbstractSubmittable)
    return MOI.supports(b.model, sub)
end

function MOI.submit(
    b::AbstractBridgeOptimizer,
    sub::MOI.AbstractSubmittable,
    args...,
)
    return MOI.submit(b.model, sub, bridged_function.(b, args)...)
end

"""
    bridged_variable_function(
        b::AbstractBridgeOptimizer,
        vi::MOI.VariableIndex,
    )

Return a `MOI.AbstractScalarFunction` of variables of `b.model` that equals
`vi`. That is, if the variable `vi` is bridged, it returns its expression in
terms of the variables of `b.model`. Otherwise, it returns
`vi`.
"""
function bridged_variable_function(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    if is_bridged(b, vi)
        func = bridged_function(
            bridge(b, vi)::Variable.AbstractBridge,
            _index(b, vi)...,
        )
        if recursive_model(b) === b
            # The bridge created its variables through `b` itself (for
            # example, `LazyBridgeOptimizer`), so `func` is expressed in
            # `b`'s (outer) namespace: chained bridged variables may remain
            # and need further substitution.
            return bridged_function(b, func)
        else
            # The bridge created its variables directly in `b.model` (for
            # example, `SingleBridgeOptimizer`), so `func` is already
            # expressed in the inner namespace: do not reinterpret its
            # indices in the outer namespace.
            return func
        end
    elseif recursive_model(b) === b
        # The substituted function stays in the outer namespace; the
        # translation to the inner namespace happens at the `b.model`
        # boundary (see `_to_inner_value`).
        return vi
    else
        # The substituted function is consumed in the inner namespace
        # (either by `b.model` or by a bridge constructed with `b.model`),
        # so translate the non-bridged variable now.
        return outer_to_inner(b, vi)
    end
end

"""
    bridged_function(b::AbstractBridgeOptimizer, value)::typeof(value)

Substitute any bridged [`MOI.VariableIndex`](@ref) in `value` by an equivalent
expression in terms of variables of `b.model`.
"""
function bridged_function(
    bridge::AbstractBridgeOptimizer,
    value::V,
)::V where {V}
    if !Variable.has_bridges(Variable.bridges(bridge))
        # Shortcut, this allows performance to be unaltered when no variable
        # bridges are used.
        return value
    end
    # We assume that the type of `value` is not altered. This restricts
    # variable bridges to only return `ScalarAffineFunction` but otherwise,
    # the performance would be bad.
    return MOI.Utilities.substitute_variables(value) do vi
        return bridged_variable_function(bridge, vi)
    end
end

function bridged_function(b::AbstractBridgeOptimizer, func::MOI.VariableIndex)
    # Should not be called by `add_constraint` as it force-bridges it
    # but could be called by attributes
    if is_bridged(b, func)
        # It could be solved by force-bridging the attribues (for example, objective).
        error("Using bridged variable in `VariableIndex` function.")
    end
    return func
end

"""
    unbridged_variable_function(
        b::AbstractBridgeOptimizer,
        vi::MOI.VariableIndex,
    )

Return a `MOI.AbstractScalarFunction` of variables of `b` that equals `vi`.
That is, if the variable `vi` is an internal variable of `b.model` created by a
bridge but not visible to the user, it returns its expression in terms of the
variables of bridged variables. Otherwise, it returns `vi`.
"""
function unbridged_variable_function(
    b::AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
)
    func = Variable.unbridged_function(Variable.bridges(b)::Variable.Map, vi)
    if func === nothing
        return vi
    elseif recursive_model(b) === b
        # `LazyBridgeOptimizer`: the bridges created their variables through
        # `b`, so `func` is expressed in `b`'s outer namespace and may chain
        # through further bridge-created (outer) variables that still need
        # unbridging. These all have distinct outer values.
        return unbridged_function(b, func)
    else
        # `SingleBridgeOptimizer`: the bridge created its variables directly
        # in `b.model`, so the `unbridged_function` map is keyed by inner
        # variables while `func` is already expressed in terms of the final
        # outer bridged variables. We must NOT re-process `func`: an outer
        # bridged variable and an inner bridge-created variable can share the
        # same positive value, so looking `func`'s variables up in the map
        # would wrongly match and recurse forever.
        return func
    end
end

"""
    unbridged_function(b::AbstractBridgeOptimizer, value)::typeof(value)

Substitute any internal [`MOI.VariableIndex`](@ref) of `b.model` created by a
variable bridge but not visible to the user in `value` by an equivalent
expression in terms of bridged variables.
"""
function unbridged_function(b::AbstractBridgeOptimizer, value)
    if !Variable.has_bridges(Variable.bridges(b))
        return value
    end
    # If `value` does not contain any variable, this will never call
    # `unbridged_variable_function` hence it might silently return an incorrect
    # value so we call `throw_if_cannot_unbridge` here.
    Variable.throw_if_cannot_unbridge(Variable.bridges(b)::Variable.Map)
    return MOI.Utilities.substitute_variables(
        vi -> unbridged_variable_function(b, vi),
        value,
    )::typeof(value)
end

function unbridged_function(
    ::AbstractBridgeOptimizer,
    func::Union{MOI.VariableIndex,MOI.VectorOfVariables},
)
    return func # bridged variables are not allowed in non-bridged constraints
end

# Shortcut to avoid `Variable.throw_if_cannot_unbridge(Variable.bridges(b))`
function unbridged_function(
    ::AbstractBridgeOptimizer,
    value::MOI.Utilities.ObjectOrTupleOrArrayWithoutIndex,
)
    return value
end

"""
    bridged_constraint_function(
       b::AbstractBridgeOptimizer,
       func::MOI.AbstractFunction,
       set::MOI.AbstractSet
    )

Similar to `bridged_function(b, func)` but move function constant to the set if
it is scalar. Also return the modified set.
"""
function bridged_constraint_function end

function bridged_constraint_function(
    b::AbstractBridgeOptimizer,
    func::MOI.AbstractVectorFunction,
    set::MOI.AbstractVectorSet,
)
    return bridged_function(b, func), set
end

function bridged_constraint_function(
    b::AbstractBridgeOptimizer,
    func::MOI.AbstractScalarFunction,
    set::MOI.AbstractScalarSet,
)
    if !Variable.has_bridges(Variable.bridges(b))
        return func, set
    end
    # We use the fact that the initial function constant was zero to
    # implement getters for `MOI.ConstraintFunction` and `MOI.ConstraintSet`.
    #
    # See `unbridged_constraint_function` for more details.
    MOI.throw_if_scalar_and_constant_not_zero(func, typeof(set))
    f = bridged_function(b, func)::typeof(func)
    return MOI.Utilities.normalize_constant(f, set)
end

"""
    unbridged_constraint_function(
       b::AbstractBridgeOptimizer,
       func::MOI.AbstractFunction
    )

Similar to `unbridged_function(b, func)` but zero the function constant if it is
scalar.
"""
function unbridged_constraint_function end

# The purpose of unbridged_constraint_function is to convert the result of
# ConstraintFunction into the un-bridged form. Since variable bridges create
# substitution rules which are scalar functions, for example, `x => y_1 - y_2`, if the
# result is a VariableIndex or an AbstractVectorFunction we can return the
# unbridged function:

function unbridged_constraint_function(
    b::AbstractBridgeOptimizer,
    func::MOI.VariableIndex,
)
    return unbridged_function(b, func)
end

function unbridged_constraint_function(
    b::AbstractBridgeOptimizer,
    func::MOI.AbstractVectorFunction,
)
    return unbridged_function(b, func)
end

# (These are separate methods to avoid ambiguity issues.)

# But if the result is an AbstractScalarFunction, we need to be more careful.

function unbridged_constraint_function(
    b::AbstractBridgeOptimizer,
    func::MOI.AbstractScalarFunction,
)
    # If `b` has no variable bridges, no substitution can occur, so we return
    # func.
    if !Variable.has_bridges(Variable.bridges(b))
        return func
    end
    # Otherwise, first unbridge the function:
    f = unbridged_function(b, func)::typeof(func)
    # But now we have to deal with an issue. Something like x in [1, ∞) might
    # get bridged into y in R₊, with x => y + 1, so if the original constraint is
    # 2x >= 1, the bridged function is 2y >= -1. Unbridging this with y = x - 1
    # gives 2x - 2, but we only care about 2x. Where did the -2 come from? It
    # was moved into the set. This gets handled separately, so for
    # ConstraintFunction it is sufficient to drop any non-zero constant terms.
    #
    # It's also safe to do this because ScalarFunctionConstantNotZero will be
    # thrown if we can't move the constant into the set, and there is a test to
    # catch this.
    c = MOI.constant(f)
    if !iszero(c)
        f = MOI.Utilities.operate(-, typeof(c), f, c)
    end
    return f
end

# TODO add transform
