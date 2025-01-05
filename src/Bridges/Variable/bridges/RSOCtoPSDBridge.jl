# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    RSOCtoPSDBridge{T} <: Bridges.Variable.AbstractBridge

`RSOCtoPSDBridge` implements the following reformulation:

 * ``||x||_2^2 \\le 2tu`` where ``t, u \\ge 0`` into ``Y \\succeq 0``, with the
   substitution rule:
   ``Y = \\left[\\begin{array}{c c}t & x^\\top \\\\ x & 2u \\mathbf{I}\\end{array}\\right].``

Additional bounds are added to ensure the off-diagonals of the ``2uI`` submatrix
are `0`, and linear constraints are added to ensure the diagonal of ``2uI``
takes the same values.

As a special case, if ``|x|| = 0``, then `RSOCtoPSDBridge` reformulates into
``(t, u) \\in \\mathbb{R}_+``.

## Source node

`RSOCtoPSDBridge` supports:

 * [`MOI.VectorOfVariables`](@ref) in [`MOI.RotatedSecondOrderCone`](@ref)

## Target nodes

`RSOCtoPSDBridge` creates:

 * One variable node that depends on the input dimension:
   * [`MOI.VectorOfVariables`](@ref) in [`MOI.Nonnegatives`](@ref) if dimension
     is `1` or `2`
   * [`MOI.VectorOfVariables`](@ref) in
   [`MOI.PositiveSemidefiniteConeTriangle`](@ref) otherwise
 * The constraint node [`MOI.VariableIndex`](@ref) in [`MOI.EqualTo`](@ref)
 * The constant node [`MOI.ScalarAffineFunction`](@ref) in [`MOI.EqualTo`](@ref)
"""
struct RSOCtoPSDBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    psd::Union{
        MOI.ConstraintIndex{
            MOI.VectorOfVariables,
            MOI.PositiveSemidefiniteConeTriangle,
        },
        MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives},
    }
    off_diag::Vector{MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}}
    diag::Vector{
        MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
    }
end

const RSOCtoPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoPSDBridge{T},OT}

# This bridge destorys a lot of structure and adding PSD variables is almost
# always undesirable. We give this bridge a high cost so that it is used only if
# necessary.
MOI.Bridges.bridging_cost(::Type{<:RSOCtoPSDBridge}) = 10.0

function bridge_constrained_variable(
    ::Type{RSOCtoPSDBridge{T}},
    model::MOI.ModelLike,
    set::MOI.RotatedSecondOrderCone,
) where {T}
    off_diag = MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{T}}[]
    diag = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    # A special case: if there are no `x` variables, then a
    # RotatedSecondOrderCone is equivalent to [t, u] >= 0.
    if set.dimension <= 2
        variables, psd = MOI.add_constrained_variables(
            model,
            MOI.Nonnegatives(set.dimension),
        )
        return RSOCtoPSDBridge{T}(variables, psd, off_diag, diag)
    end
    psd_set = MOI.PositiveSemidefiniteConeTriangle((set.dimension - 1))
    variables, psd = MOI.add_constrained_variables(model, psd_set)
    zero_set = MOI.EqualTo(zero(T))
    k = 0
    for col in 1:(set.dimension-1), row in 1:col
        k += 1
        if row == 1 || col <= 2
            continue  # These are free variables
        elseif col == row
            # X[2, 2] - X[i, i] == 0 for i = 3:N
            f = MOI.Utilities.operate(-, T, variables[3], variables[k])
            push!(diag, MOI.add_constraint(model, f, zero_set))
        else
            push!(off_diag, MOI.add_constraint(model, variables[k], zero_set))
        end
    end
    return RSOCtoPSDBridge{T}(variables, psd, off_diag, diag)
end

function supports_constrained_variable(
    ::Type{<:RSOCtoPSDBridge},
    ::Type{MOI.RotatedSecondOrderCone},
)
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:RSOCtoPSDBridge})
    return Tuple{Type}[
        (MOI.PositiveSemidefiniteConeTriangle,),
        (MOI.Nonnegatives,),
    ]
end

function MOI.Bridges.added_constraint_types(
    ::Type{RSOCtoPSDBridge{T}},
) where {T}
    return Tuple{Type,Type}[
        (MOI.VariableIndex, MOI.EqualTo{T}),
        (MOI.ScalarAffineFunction{T}, MOI.EqualTo{T}),
    ]
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::RSOCtoPSDBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.variables)
end

function MOI.get(bridge::RSOCtoPSDBridge, ::MOI.ListOfVariableIndices)
    return copy(bridge.variables)
end

function MOI.get(
    bridge::RSOCtoPSDBridge,
    ::MOI.NumberOfConstraints{MOI.VectorOfVariables,S},
)::Int64 where {S<:Union{MOI.PositiveSemidefiniteConeTriangle,MOI.Nonnegatives}}
    if bridge.psd isa MOI.ConstraintIndex{MOI.VectorOfVariables,S}
        return 1
    end
    return 0
end

function MOI.get(
    bridge::RSOCtoPSDBridge,
    ::MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S},
) where {S<:Union{MOI.PositiveSemidefiniteConeTriangle,MOI.Nonnegatives}}
    if bridge.psd isa MOI.ConstraintIndex{MOI.VectorOfVariables,S}
        return [bridge.psd]
    end
    return MOI.ConstraintIndex{MOI.VectorOfVariables,S}[]
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.NumberOfConstraints{MOI.VariableIndex,MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.off_diag)
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.EqualTo{T}},
) where {T}
    return copy(bridge.off_diag)
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
)::Int64 where {T}
    return length(bridge.diag)
end

function MOI.get(
    bridge::RSOCtoPSDBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.diag)
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::RSOCtoPSDBridge)
    for ci in bridge.diag
        MOI.delete(model, ci)
    end
    MOI.delete(model, bridge.variables)
    return
end

# Attributes, Bridge acting as a constraint

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::RSOCtoPSDBridge)
    CI = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
    if bridge.psd isa CI
        return MOI.RotatedSecondOrderCone(length(bridge.variables))
    end
    return MOI.RotatedSecondOrderCone(length(bridge.diag) + 3)
end

function _variable_map(bridge::RSOCtoPSDBridge, i::MOI.Bridges.IndexInVector)
    CI = MOI.ConstraintIndex{MOI.VectorOfVariables,MOI.Nonnegatives}
    if bridge.psd isa CI
        return i.value
    elseif i.value == 1
        return 1
    elseif i.value == 2
        return 3
    else
        return MOI.Utilities.trimap(1, i.value - 1)
    end
end

function _variable(bridge::RSOCtoPSDBridge, i::MOI.Bridges.IndexInVector)
    return bridge.variables[_variable_map(bridge, i)]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::RSOCtoPSDBridge,
)
    values = MOI.get(model, attr, bridge.psd)
    set = MOI.get(model, MOI.ConstraintSet(), bridge)
    n = MOI.dimension(set)
    primal = zeros(n)
    for i in 1:n
        primal[i] = values[_variable_map(bridge, MOI.Bridges.IndexInVector(i))]
    end
    if n >= 2
        primal[2] /= 2
    end
    return primal
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::RSOCtoPSDBridge,
)
    values = MOI.get(model, attr, bridge.psd)
    n = MOI.dimension(MOI.get(model, MOI.ConstraintSet(), bridge))
    dual = zeros(n)
    for i in 1:n
        dual[i] = values[_variable_map(bridge, MOI.Bridges.IndexInVector(i))]
    end
    for ci in bridge.diag
        dual[2] += MOI.get(model, attr, ci)
    end
    # For `i = 2`, we multiply by 2 because it is `2u`.
    # For `i > 2`, we multiply by 2 because to account for the difference
    # of scalar product `MOI.Utilities.set_dot`.
    for i in 2:length(dual)
        dual[i] *= 2
    end
    return dual
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:RSOCtoPSDBridge},
)
    return MOI.supports(model, attr, MOI.VariableIndex)
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::RSOCtoPSDBridge,
    i::MOI.Bridges.IndexInVector,
)
    value = MOI.get(model, attr, _variable(bridge, i))
    if value !== nothing && i.value == 2
        return value / 2
    end
    return value
end

_u_start_values(::Nothing) = nothing, nothing
_u_start_values(value::T) where {T} = 2 * value, zero(T)

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::RSOCtoPSDBridge{T},
    value,
    i::MOI.Bridges.IndexInVector,
) where {T}
    if i.value != 2
        MOI.set(model, attr, _variable(bridge, i), value)
        return
    end
    diag_value, offdiag_value = _u_start_values(value)
    n = length(bridge.variables)
    for col in 2:MOI.Utilities.side_dimension_for_vectorized_dimension(n)
        for row in 2:col
            k = MOI.Utilities.trimap(row, col)
            v = row == col ? diag_value : offdiag_value
            MOI.set(model, attr, bridge.variables[k], v)
        end
    end
    return
end

function MOI.Bridges.bridged_function(
    bridge::RSOCtoPSDBridge{T},
    i::MOI.Bridges.IndexInVector,
) where {T}
    func = _variable(bridge, i)
    if i.value == 2
        return MOI.Utilities.operate(/, T, func, convert(T, 2))
    end
    return convert(MOI.ScalarAffineFunction{T}, func)
end

function unbridged_map(
    bridge::RSOCtoPSDBridge{T},
    vi::MOI.VariableIndex,
    i::MOI.Bridges.IndexInVector,
) where {T}
    if i.value == 2
        func = MOI.Utilities.operate(*, T, convert(T, 2), vi)
        return (_variable(bridge, i) => func,)
    end
    return (_variable(bridge, i) => convert(MOI.ScalarAffineFunction{T}, vi),)
end
