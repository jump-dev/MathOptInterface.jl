# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    CountBelongsToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`CountBelongsToMILPBridge` implements the following reformulation:

  * ``(n, x) \\in \\textsf{CountBelongs}(1+d, \\mathcal{S})`` into a
    mixed-integer linear program.

## Reformulation

The reformulation is non-trivial, and it depends on the finite domain of each
variable ``x_i``, which we as define ``S_i = \\{l_i,\\ldots,u_i\\}``.

First, we introduce new binary variables ``z_{ij}``, which are ``1`` if variable
``x_i`` takes the value ``j`` in the optimal solution and ``0`` otherwise:
```math
\\begin{aligned}
z_{ij} \\in \\{0, 1\\}                              & \\;\\; \\forall i \\in 1\\ldots d, j \\in S_i  \\\\
x_i - \\sum\\limits_{j\\in S_i} j \\cdot z_{ij} = 0 & \\;\\; \\forall i \\in 1\\ldots d              \\\\
\\sum\\limits_{j\\in S_i} z_{ij} = 1                & \\;\\; \\forall i \\in 1\\ldots d              \\\\
\\end{aligned}
```

Finally, ``n`` is constrained to be the number of ``z_{ij}`` elements that are
in ``\\mathcal{S}``:
```math
n - \\sum\\limits_{i\\in 1\\ldots d, j \\in \\mathcal{S}} z_{ij} = 0
```

## Source node

`CountBelongsToMILPBridge` supports:

  * `F` in [`MOI.CountBelongs`](@ref)

where `F` is [`MOI.VectorOfVariables`](@ref) or
[`MOI.VectorAffineFunction{T}`](@ref).

## Target nodes

`CountBelongsToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
"""
mutable struct CountBelongsToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    set::MOI.CountBelongs
    variables::Vector{MOI.VariableIndex}
    equal_to::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
    bounds::Vector{NTuple{2,T}}
    function CountBelongsToMILPBridge{T}(
        f::Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
        s::MOI.CountBelongs,
    ) where {T}
        return new{T,typeof(f)}(
            f,
            s,
            MOI.VariableIndex[],
            MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[],
            NTuple{2,T}[],
        )
    end
end

const CountBelongsToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{CountBelongsToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{CountBelongsToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.CountBelongs,
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    # !!! info
    #     Postpone creation until final_touch.
    return CountBelongsToMILPBridge{T}(f, s)
end

function MOI.supports_constraint(
    ::Type{<:CountBelongsToMILPBridge{T}},
    ::Type{<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}},
    ::Type{MOI.CountBelongs},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:CountBelongsToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:CountBelongsToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})]
end

function concrete_bridge_type(
    ::Type{<:CountBelongsToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.CountBelongs},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return CountBelongsToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::CountBelongsToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::CountBelongsToMILPBridge,
)
    return bridge.set
end

function MOI.delete(model::MOI.ModelLike, bridge::CountBelongsToMILPBridge)
    for ci in bridge.equal_to
        MOI.delete(model, ci)
    end
    empty!(bridge.equal_to)
    for x in bridge.variables
        MOI.delete(model, x)
    end
    empty!(bridge.variables)
    empty!(bridge.bounds)
    return
end

function MOI.get(
    bridge::CountBelongsToMILPBridge,
    ::MOI.NumberOfVariables,
)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::CountBelongsToMILPBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::CountBelongsToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.variables)
end

function MOI.get(
    bridge::CountBelongsToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}[
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value) for
        x in bridge.variables
    ]
end

function MOI.get(
    bridge::CountBelongsToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.equal_to)
end

function MOI.get(
    bridge::CountBelongsToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.equal_to)
end

MOI.Bridges.needs_final_touch(::CountBelongsToMILPBridge) = true

"""
    _unit_expansion(
        ::CountBelongsToMILPBridge{T},
        model::MOI.ModelLike,
        f::Vector{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}}
    ) where {T}

Reformulates a vector of input functions ``f`` into a binary unit expansion.
This is useful when writing constraint programming bridges.
"""
function _unit_expansion(
    bridge::CountBelongsToMILPBridge{T},
    model::MOI.ModelLike,
    f::Vector{<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}},
) where {T}
    S = Dict{T,Vector{MOI.VariableIndex}}()
    bounds = Dict{MOI.VariableIndex,Tuple{T,T}}()
    ci = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    for i in 1:length(f)
        ret = MOI.Utilities.get_bounds(bridge, model, bounds, f[i])
        if ret === nothing
            BT = typeof(bridge)
            error(
                "Unable to use $BT because an element in the function has a " *
                "non-finite domain: $(f[i])",
            )
        end
        if length(bridge.bounds) < i
            # This is the first time calling final_touch
            push!(bridge.bounds, ret)
        elseif bridge.bounds[i] == ret
            # We've called final_touch before, and the bounds match. No need to
            # reformulate a second time.
            continue
        elseif bridge.bounds[i] != ret
            # There is a stored bound, and the current bounds do not match. This
            # means the model has been modified since the previous call to
            # final_touch. We need to delete the bridge and start again.
            MOI.delete(model, bridge)
            MOI.Bridges.final_touch(bridge, model)
            break
        end
        unit_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        convex_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        for xi in ret[1]:ret[2]
            z, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
            if !haskey(S, xi)
                S[xi] = MOI.VariableIndex[]
            end
            push!(S[xi], z)
            push!(unit_f.terms, MOI.ScalarAffineTerm(T(-xi), z))
            push!(convex_f.terms, MOI.ScalarAffineTerm(one(T), z))
        end
        push!(
            ci,
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate(+, T, f[i], unit_f),
                MOI.EqualTo(zero(T));
                allow_modify_function = true,
            ),
        )
        push!(ci, MOI.add_constraint(model, convex_f, MOI.EqualTo(one(T))))
    end
    return S, ci
end

function MOI.Bridges.final_touch(
    bridge::CountBelongsToMILPBridge{T,F},
    model::MOI.ModelLike,
) where {T,F}
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    S, ci = _unit_expansion(bridge, model, scalars[2:end])
    if isempty(S) && isempty(ci)
        return # Nothing to bridge. We must have already called final_touch.
    end
    append!(bridge.equal_to, ci)
    for (_, s) in S
        append!(bridge.variables, s)
    end
    f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
    for s in bridge.set.set
        if haskey(S, s)
            for x in S[s]
                push!(f.terms, MOI.ScalarAffineTerm(one(T), x))
            end
        end
    end
    f = MOI.Utilities.operate!(-, T, f, scalars[1])
    push!(
        bridge.equal_to,
        MOI.Utilities.normalize_and_add_constraint(
            model,
            f,
            MOI.EqualTo(zero(T));
            allow_modify_function = true,
        ),
    )
    return
end
