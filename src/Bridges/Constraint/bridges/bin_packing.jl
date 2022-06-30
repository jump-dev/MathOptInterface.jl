# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    BinPackingToMILPBridge{T,F} <: Bridges.Constraint.AbstractBridge

`BinPackingToMILPBridge` implements the following reformulation:

  * ``x \\in BinPacking(c, w)`` into a mixed-integer linear program.

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

Then, we add the capacity constraint for all possible bins ``j``:
```math
\\sum\\limits_{i} w_i z_{ij} \\le c \\forall j \\in \\bigcup_{i=1,\\ldots,d} S_i
```

## Source node

`BinPackingToMILPBridge` supports:

  * `F` in [`MOI.BinPacking{T}`](@ref)

## Target nodes

`BinPackingToMILPBridge` creates:

  * [`MOI.VariableIndex`](@ref) in [`MOI.ZeroOne`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
"""
struct BinPackingToMILPBridge{
    T,
    F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}},
} <: AbstractBridge
    f::F
    s::MOI.BinPacking{T}

    variables::Vector{MOI.VariableIndex}
    less_than::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
    }
    equal_to::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
end

const BinPackingToMILP{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{BinPackingToMILPBridge{T},OT}

function bridge_constraint(
    ::Type{BinPackingToMILPBridge{T,F}},
    model::MOI.ModelLike,
    f::F,
    s::MOI.BinPacking{T},
) where {T,F}
    return BinPackingToMILPBridge{T,F}(
        f,
        s,
        MOI.VariableIndex[],
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.LessThan{T}}[],
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[],
    )
end

function MOI.supports_constraint(
    ::Type{<:BinPackingToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.BinPacking{T}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:BinPackingToMILPBridge},
)
    return Tuple{Type}[(MOI.ZeroOne,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:BinPackingToMILPBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{<:BinPackingToMILPBridge{T}},
    ::Type{F},
    ::Type{MOI.BinPacking{T}},
) where {T,F<:Union{MOI.VectorOfVariables,MOI.VectorAffineFunction{T}}}
    return BinPackingToMILPBridge{T,F}
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::BinPackingToMILPBridge,
)
    return bridge.f
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::BinPackingToMILPBridge,
)
    return bridge.s
end

function MOI.delete(model::MOI.ModelLike, bridge::BinPackingToMILPBridge)
    MOI.delete.(model, bridge.less_than)
    empty!(bridge.less_than)
    MOI.delete.(model, bridge.equal_to)
    empty!(bridge.equal_to)
    MOI.delete.(model, bridge.variables)
    empty!(bridge.variables)
    return
end

function MOI.get(bridge::BinPackingToMILPBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::BinPackingToMILPBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::BinPackingToMILPBridge,
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.ZeroOne},
)::Int64
    return length(bridge.variables)
end

function MOI.get(
    bridge::BinPackingToMILPBridge,
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.ZeroOne},
)
    return [
        MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}.(z.value) for
        z in bridge.variables
    ]
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.equal_to)
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.equal_to)
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
)::Int64 where {T}
    return length(bridge.less_than)
end

function MOI.get(
    bridge::BinPackingToMILPBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.LessThan{T}},
) where {T}
    return copy(bridge.less_than)
end

MOI.Bridges.needs_final_touch(::BinPackingToMILPBridge) = true

# We use the bridge as the first argument to avoid type piracy of other methods.
function _get_bounds(
    bridge::BinPackingToMILPBridge{T},
    model::MOI.ModelLike,
    bounds::Dict{MOI.VariableIndex,NTuple{2,T}},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    lb = ub = f.constant
    for term in f.terms
        ret = _get_bounds(bridge, model, bounds, term.variable)
        if ret === nothing
            return nothing
        end
        lb += term.coefficient * ret[1]
        ub += term.coefficient * ret[2]
    end
    return lb, ub
end

# We use the bridge as the first argument to avoid type piracy of other methods.
function _get_bounds(
    ::BinPackingToMILPBridge{T},
    model::MOI.ModelLike,
    bounds::Dict{MOI.VariableIndex,NTuple{2,T}},
    x::MOI.VariableIndex,
) where {T}
    if haskey(bounds, x)
        return bounds[x]
    end
    ret = MOI.Utilities.get_bounds(model, T, x)
    if ret == (typemin(T), typemax(T))
        return nothing
    end
    bounds[x] = ret
    return ret
end

function MOI.Bridges.final_touch(
    bridge::BinPackingToMILPBridge{T,F},
    model::MOI.ModelLike,
) where {T,F}
    # Clear any existing reformulations!
    MOI.delete(model, bridge)
    S = Dict{T,Vector{Tuple{Float64,MOI.VariableIndex}}}()
    scalars = collect(MOI.Utilities.eachscalar(bridge.f))
    bounds = Dict{MOI.VariableIndex,NTuple{2,T}}()
    for i in 1:length(scalars)
        x = scalars[i]
        ret = _get_bounds(bridge, model, bounds, x)
        if ret === nothing
            error(
                "Unable to use $(typeof(bridge)) because an element in the " *
                "function has a non-finite domain: $x",
            )
        end
        unit_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        convex_f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
        for xi in ret[1]::T:ret[2]::T
            new_var, _ = MOI.add_constrained_variable(model, MOI.ZeroOne())
            push!(bridge.variables, new_var)
            if !haskey(S, xi)
                S[xi] = Tuple{Float64,MOI.VariableIndex}[]
            end
            push!(S[xi], (bridge.s.weights[i], new_var))
            push!(unit_f.terms, MOI.ScalarAffineTerm(T(-xi), new_var))
            push!(convex_f.terms, MOI.ScalarAffineTerm(one(T), new_var))
        end
        push!(
            bridge.equal_to,
            MOI.Utilities.normalize_and_add_constraint(
                model,
                MOI.Utilities.operate(+, T, x, unit_f),
                MOI.EqualTo(zero(T));
                allow_modify_function = true,
            ),
        )
        push!(
            bridge.equal_to,
            MOI.add_constraint(model, convex_f, MOI.EqualTo(one(T))),
        )
    end
    # We use a sort so that the model order is deterministic.
    for s in sort!(collect(keys(S)))
        ci = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                [MOI.ScalarAffineTerm(w, z) for (w, z) in S[s]],
                zero(T),
            ),
            MOI.LessThan(bridge.s.capacity),
        )
        push!(bridge.less_than, ci)
    end
    return
end
