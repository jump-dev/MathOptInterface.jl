## Storage of constraints
#
# All `F`-in-`S` constraints are stored in a vector of `ConstraintEntry{F, S}`.
# The index in this vector of a constraint of index
# `ci::MOI.ConstraintIndex{F, S}` is given by `model.constrmap[ci.value]`. The
# advantage of this representation is that it does not require any dictionary
# hence it never needs to compute a hash.
#
# It may seem redundant to store the constraint index `ci` as well as the
# function and sets in the tuple but it is used to efficiently implement the
# getter for `MOI.ListOfConstraintIndices{F, S}`. It is also used to implement
# `MOI.delete`. Indeed, when a constraint is deleted, it is removed from the
# vector hence the index in the vector of all the functions that were stored
# after must be decreased by one. As the constraint index is stored in the
# vector, it readily gives the entries of `model.constrmap` that need to be
# updated.

mutable struct VectorOfConstraints{
    F<:MOI.AbstractFunction,
    S<:MOI.AbstractSet,
} <: MOI.ModelLike
    constraints::CleverDicts.CleverDict{
        MOI.ConstraintIndex{F,S},
        Tuple{F,S},
        typeof(CleverDicts.key_to_index),
        typeof(CleverDicts.index_to_key),
    }

    function VectorOfConstraints{F,S}() where {F,S}
        return new{F,S}(
            CleverDicts.CleverDict{MOI.ConstraintIndex{F,S},Tuple{F,S}}(),
        )
    end
end

MOI.is_empty(v::VectorOfConstraints) = isempty(v.constraints)
MOI.empty!(v::VectorOfConstraints) = empty!(v.constraints)

function MOI.supports_constraint(
    ::VectorOfConstraints{F,S},
    ::Type{F},
    ::Type{S},
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    return true
end

function MOI.add_constraint(
    v::VectorOfConstraints{F,S},
    func::F,
    set::S,
) where {F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    # We canonicalize the constraint so that solvers can avoid having to
    # canonicalize it most of the time (they can check if they need to with
    # `is_canonical`.
    # Note that the canonicalization is not guaranteed if for instance
    # `modify` is called and adds a new term.
    # See https://github.com/jump-dev/MathOptInterface.jl/pull/1118
    return CleverDicts.add_item(v.constraints, (canonical(func), copy(set)))
end

function MOI.is_valid(
    v::VectorOfConstraints{F,S},
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    return haskey(v.constraints, ci)
end

function MOI.delete(
    v::VectorOfConstraints{F,S},
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    MOI.throw_if_not_valid(v, ci)
    delete!(v.constraints, ci)
    return
end

function MOI.get(
    v::VectorOfConstraints{F,S},
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    MOI.throw_if_not_valid(v, ci)
    return v.constraints[ci][1]
end

function MOI.get(
    v::VectorOfConstraints{F,S},
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{F,S},
) where {F,S}
    MOI.throw_if_not_valid(v, ci)
    return v.constraints[ci][2]
end

function MOI.set(
    v::VectorOfConstraints{F,S},
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{F,S},
    func::F,
) where {F,S}
    MOI.throw_if_not_valid(v, ci)
    v.constraints[ci] = (func, v.constraints[ci][2])
    return
end

function MOI.set(
    v::VectorOfConstraints{F,S},
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{F,S},
    set::S,
) where {F,S}
    MOI.throw_if_not_valid(v, ci)
    v.constraints[ci] = (v.constraints[ci][1], set)
    return
end

function MOI.get(
    v::VectorOfConstraints{F,S},
    ::MOI.ListOfConstraintTypesPresent,
)::Vector{Tuple{DataType,DataType}} where {F,S}
    return isempty(v.constraints) ? [] : [(F, S)]
end

function MOI.get(
    v::VectorOfConstraints{F,S},
    ::MOI.NumberOfConstraints{F,S},
) where {F,S}
    return length(v.constraints)
end

function MOI.get(
    v::VectorOfConstraints{F,S},
    ::MOI.ListOfConstraintIndices{F,S},
) where {F,S}
    return keys(v.constraints)
end

function MOI.modify(
    v::VectorOfConstraints{F,S},
    ci::MOI.ConstraintIndex{F,S},
    change::MOI.AbstractFunctionModification,
) where {F,S}
    func, set = v.constraints[ci]
    v.constraints[ci] = (modify_function(func, change), set)
    return
end

function _add_variable(::VectorOfConstraints) end
function _add_variables(::VectorOfConstraints, ::Int64) end

# Deletion of variables in vector of variables

function _remove_variable(v::VectorOfConstraints, vi::MOI.VariableIndex)
    CleverDicts.map_values!(v.constraints) do (f, s)
        return remove_variable(f, s, vi)
    end
    return
end

function _filter_variables(keep::Function, v::VectorOfConstraints)
    CleverDicts.map_values!(v.constraints) do (f, s)
        return filter_variables(keep, f, s)
    end
    return
end

function throw_delete_variable_in_vov(vi::MOI.VariableIndex)
    message = string(
        "Cannot delete variable as it is constrained with other",
        " variables in a `MOI.VectorOfVariables`.",
    )
    return throw(MOI.DeleteNotAllowed(vi, message))
end

# Nothing to do as it's not `VectorOfVariables` constraints
_throw_if_cannot_delete(::VectorOfConstraints, vis, fast_in_vis) = nothing

_fast_in(vi1::MOI.VariableIndex, vi2::MOI.VariableIndex) = vi1 == vi2
_fast_in(vi::MOI.VariableIndex, vis::Set{MOI.VariableIndex}) = vi in vis

function _throw_if_cannot_delete(
    v::VectorOfConstraints{MOI.VectorOfVariables,S},
    vis,
    fast_in_vis,
) where {S<:MOI.AbstractVectorSet}
    if MOI.supports_dimension_update(S) || MOI.is_empty(v)
        return
    end
    for fs in values(v.constraints)
        f = fs[1]::MOI.VectorOfVariables
        if length(f.variables) > 1 && f.variables != vis
            for vi in f.variables
                if _fast_in(vi, fast_in_vis)
                    # If `supports_dimension_update(S)` then the variable
                    # will be removed in `_filter_variables`.
                    throw_delete_variable_in_vov(vi)
                end
            end
        end
    end
    return
end

function _delete_variables(
    ::Function,
    ::VectorOfConstraints,
    ::Vector{MOI.VariableIndex},
)
    return  # Nothing to do as it's not `VectorOfVariables` constraints
end

function _delete_variables(
    callback::Function,
    v::VectorOfConstraints{MOI.VectorOfVariables,<:MOI.AbstractVectorSet},
    vis::Vector{MOI.VariableIndex},
)
    filter!(v.constraints) do p
        f = p.second[1]
        del = if length(f.variables) == 1
            first(f.variables) in vis
        else
            vis == f.variables
        end
        if del
            callback(p.first)
        end
        return !del
    end
    return
end

function _deleted_constraints(
    callback::Function,
    v::VectorOfConstraints,
    vi::MOI.VariableIndex,
)
    _delete_variables(callback, v, [vi])
    _remove_variable(v, vi)
    return
end

function _deleted_constraints(
    callback::Function,
    v::VectorOfConstraints,
    vis::Vector{MOI.VariableIndex},
)
    _delete_variables(callback, v, vis)
    removed = Set(vis)
    _filter_variables(vi -> !(vi in removed), v)
    return
end
