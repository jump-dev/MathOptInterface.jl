# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    HermitianToSymmetricPSDBridge{T} <: Bridges.Variable.AbstractBridge

`HermitianToSymmetricPSDBridge` implements the following reformulation:

  * Hermitian positive semidefinite `n x n` complex matrix to a symmetric
    positive semidefinite `2n x 2n` real matrix satisfying equality constraints
    described below.

## Source node

`HermitianToSymmetricPSDBridge` supports:

  * [`MOI.VectorOfVariables`](@ref) in
    [`MOI.HermitianPositiveSemidefiniteConeTriangle`](@ref)

## Target node

`HermitianToSymmetricPSDBridge` creates:

  * [`MOI.VectorOfVariables`](@ref) in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.EqualTo{T}`](@ref)

## Reformulation

The reformulation is best described by example.

The Hermitian matrix:
```math
\\begin{bmatrix}
  x_{11}            & x_{12} + y_{12}im & x_{13} + y_{13}im\\\\
  x_{12} - y_{12}im & x_{22}            & x_{23} + y_{23}im\\\\
  x_{13} - y_{13}im & x_{23} - y_{23}im & x_{33}
\\end{bmatrix}
```
is positive semidefinite if and only if the symmetric matrix:
```math
\\begin{bmatrix}
    x_{11} & x_{12} & x_{13} & 0       & y_{12}  & y_{13} \\\\
           & x_{22} & x_{23} & -y_{12} & 0       & y_{23} \\\\
           &        & x_{33} & -y_{13} & -y_{23} & 0      \\\\
           &        &        & x_{11}  & x_{12}  & x_{13} \\\\
           &        &        &         & x_{22}  & x_{23} \\\\
           &        &        &         &         & x_{33}
\\end{bmatrix}
```
is positive semidefinite.

The bridge achieves this reformulation by adding a new set of variables in
`MOI.PositiveSemidefiniteConeTriangle(6)`, and then adding three groups of
equality constraints to:

 * constrain the two `x` blocks to be equal
 * force the diagonal of the `y` blocks to be `0`
 * force the lower triangular of the `y` block to be the negative of the upper
   triangle.
"""
mutable struct HermitianToSymmetricPSDBridge{T} <: AbstractBridge
    variables::Vector{MOI.VariableIndex}
    psd::MOI.ConstraintIndex{
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    }
    n::Int
    ceq::Vector{MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}}
    imag_diag_start_set::Bool
end

const HermitianToSymmetricPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{HermitianToSymmetricPSDBridge{T},OT}

function bridge_constrained_variable(
    ::Type{HermitianToSymmetricPSDBridge{T}},
    model::MOI.ModelLike,
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
) where {T}
    n = set.side_dimension
    variables, psd_ci = MOI.add_constrained_variables(
        model,
        MOI.PositiveSemidefiniteConeTriangle(2n),
    )
    ceq = MOI.ConstraintIndex{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}}[]
    k11 = 0
    k12 = k22 = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(n))
    for j in 1:n
        k22 += n
        for i in 1:j
            k11 += 1
            k12 += 1
            k22 += 1
            f_x = MOI.Utilities.operate(-, T, variables[k11], variables[k22])
            push!(ceq, MOI.add_constraint(model, f_x, MOI.EqualTo(zero(T))))
            if i == j  # y_{ii} = 0
                f_0 = convert(MOI.ScalarAffineFunction{T}, variables[k12])
                push!(ceq, MOI.add_constraint(model, f_0, MOI.EqualTo(zero(T))))
            else       # y_{ij} = -y_{ji}
                k21 = MOI.Utilities.trimap(j, n + i)
                f_y =
                    MOI.Utilities.operate(+, T, variables[k21], variables[k12])
                push!(ceq, MOI.add_constraint(model, f_y, MOI.EqualTo(zero(T))))
            end
        end
        k12 += n
    end
    return HermitianToSymmetricPSDBridge(variables, psd_ci, n, ceq, false)
end

function supports_constrained_variable(
    ::Type{<:HermitianToSymmetricPSDBridge},
    ::Type{MOI.HermitianPositiveSemidefiniteConeTriangle},
)
    return true
end

function MOI.Bridges.added_constrained_variable_types(
    ::Type{<:HermitianToSymmetricPSDBridge},
)
    return Tuple{Type}[(MOI.PositiveSemidefiniteConeTriangle,)]
end

function MOI.Bridges.added_constraint_types(
    ::Type{HermitianToSymmetricPSDBridge{T}},
) where {T}
    return Tuple{Type,Type}[(MOI.ScalarAffineFunction{T}, MOI.EqualTo{T})]
end

function MOI.get(bridge::HermitianToSymmetricPSDBridge, ::MOI.NumberOfVariables)
    return length(bridge.variables)
end

function MOI.get(
    bridge::HermitianToSymmetricPSDBridge,
    ::MOI.ListOfVariableIndices,
)
    return copy(bridge.variables)
end

function MOI.get(
    ::HermitianToSymmetricPSDBridge,
    ::MOI.NumberOfConstraints{
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    },
)::Int64
    return 1
end

function MOI.get(
    bridge::HermitianToSymmetricPSDBridge,
    ::MOI.ListOfConstraintIndices{
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    },
)
    return [bridge.psd]
end

function MOI.get(
    bridge::HermitianToSymmetricPSDBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return length(bridge.ceq)
end

function MOI.get(
    bridge::HermitianToSymmetricPSDBridge{T},
    ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T},MOI.EqualTo{T}},
) where {T}
    return copy(bridge.ceq)
end

function MOI.delete(model::MOI.ModelLike, bridge::HermitianToSymmetricPSDBridge)
    MOI.delete(model, bridge.ceq)
    MOI.delete(model, bridge.variables)
    return
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::HermitianToSymmetricPSDBridge,
)
    return MOI.HermitianPositiveSemidefiniteConeTriangle(bridge.n)
end

function _matrix_indices(k)
    # If `k` is a diagonal index, `s(k)` is odd and 1 + 8k is a perfect square.
    n = 1 + 8k
    s = isqrt(n)
    j = if s^2 == n
        div(s, 2)
    else
        # Otherwise, if it is after the diagonal index `k` but before the
        # diagonal index `k'` with `s(k') = s(k) + 2`, we have
        # `s(k) <= s < s(k) + 2`.
        # By shifting by `+1` before `div`, we make sure to have the right
        # column.
        div(s + 1, 2)
    end
    i = k - MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(j - 1))
    return i, j
end

function _variable_map(idx::MOI.Bridges.IndexInVector, n)
    N = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(n))
    if idx.value <= N
        return idx.value
    end
    i, j = _matrix_indices(idx.value - N)
    d = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(j))
    return N + j * n + d + i
end

function _variable(
    bridge::HermitianToSymmetricPSDBridge,
    i::MOI.Bridges.IndexInVector,
)
    return bridge.variables[_variable_map(i, bridge.n)]
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::HermitianToSymmetricPSDBridge{T},
) where {T}
    values = MOI.get(model, attr, bridge.psd)
    M = MOI.dimension(MOI.get(model, MOI.ConstraintSet(), bridge))
    n = bridge.n
    return [values[_variable_map(MOI.Bridges.IndexInVector(i), n)] for i in 1:M]
end

# We don't need to take into account the equality constraints. We just need to
# sum up (with appropriate +/-) each dual variable associated with the original
# x or y element.
# The reason for this is as follows:
# Suppose for simplicity that the elements of a `2n x 2n` matrix are ordered as:
# ```
# \\ 1 |\\ 2
#  \\  | 3
#   \\ |4_\\
#    \\  5
#     \\
#      \\
# ```
# Let `H = HermitianToSymmetricPSDBridge(n)`,
# `S = PositiveSemidefiniteConeTriangle(2n)` and `ceq` be the linear space of
# `2n x 2n` symmetric matrices such that the block `1` and `5` are equal, `2` and `4` are opposite and `3` is zero.
# We consider the cone `P = S ∩ ceq`.
# We have `P = A * H` where
# ```
#     [I  0]
#     [0  I]
# A = [0  0]
#     [0 -I]
#     [I  0]
# ```
# Therefore, `H* = A* * P*` where
# ```
#      [I 0 0  0 I]
# A* = [0 I 0 -I 0]
# ```
# Moreover, as `(S ∩ T)* = S* + T*` for cones `S` and `T`, we have
# ```
# P* = S* + ceq*
# ```
# the dual vector of `P*` is the dual vector of `S*` for which we add in the corresponding
# entries the dual of the three constraints, multiplied by the coefficients for the `EqualTo` constraints.
# Note that these contributions cancel out when we multiply them by `A*`:
# A* * (S* + ceq*) = A* * S*
# so we can just ignore them.
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::HermitianToSymmetricPSDBridge{T},
) where {T}
    dual = MOI.get(model, attr, bridge.psd)
    M = MOI.dimension(MOI.get(model, MOI.ConstraintSet(), bridge))
    result = zeros(T, M)
    n = bridge.n
    N = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(n))
    k11, k12, k22 = 0, N, N
    k21 = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(2n)) + 1
    k = 0
    for j in 1:n
        k21 -= n + 1 - j
        k22 += n
        for i in 1:j
            k11 += 1
            k12 += 1
            k21 -= 1
            k22 += 1
            result[k11] += dual[k11] + dual[k22]
            if i != j
                k += 1
                result[N+k] += dual[k12] - dual[k21]
            end
        end
        k12 += n
        k21 -= n - j
    end
    return result
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.VariablePrimal,MOI.VariablePrimalStart},
    bridge::HermitianToSymmetricPSDBridge{T},
    i::MOI.Bridges.IndexInVector,
) where {T}
    return MOI.get(model, attr, _variable(bridge, i))
end

function MOI.Bridges.bridged_function(
    bridge::HermitianToSymmetricPSDBridge{T},
    i::MOI.Bridges.IndexInVector,
) where {T}
    return convert(MOI.ScalarAffineFunction{T}, _variable(bridge, i))
end

function unbridged_map(
    bridge::HermitianToSymmetricPSDBridge{T},
    vi::MOI.VariableIndex,
    i::MOI.Bridges.IndexInVector,
) where {T}
    return (_variable(bridge, i) => convert(MOI.ScalarAffineFunction{T}, vi),)
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    ::Type{<:HermitianToSymmetricPSDBridge},
)
    return MOI.set(model, attr, MOI.VariableIndex)
end

function MOI.set(
    model::MOI.ModelLike,
    attr::MOI.VariablePrimalStart,
    bridge::HermitianToSymmetricPSDBridge,
    value,
    index::MOI.Bridges.IndexInVector,
)
    d = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(bridge.n))
    if index.value > d  # Imaginary part
        i, j = MOI.Utilities.inverse_trimap(index.value - d)
        j += 1  # Increment `j` by `1` to account for the zero diagonal
        k12 = MOI.Utilities.trimap(i, bridge.n + j)
        MOI.set(model, attr, bridge.variables[k12], value)
        k21 = MOI.Utilities.trimap(j, bridge.n + i)
        minus_value = value === nothing ? nothing : -value
        MOI.set(model, attr, bridge.variables[k21], minus_value)
    else  # Real part
        i, j = MOI.Utilities.inverse_trimap(index.value)
        k11 = MOI.Utilities.trimap(i, j)
        MOI.set(model, attr, bridge.variables[k11], value)
        k22 = MOI.Utilities.trimap(bridge.n + i, bridge.n + j)
        MOI.set(model, attr, bridge.variables[k22], value)
    end
    # The variables on the imaginary diagonal have a value 0, and they cannot be
    # referenced by the user. So if we are setting the VariablePrimalStart for
    # some components, assume that we also want to set it for the imaginary
    # diagonal (so that every variable has a primal start). But we should do
    # this once and only once, so cache whether we have in the
    # `imag_diag_start_set` field.
    if value === nothing && bridge.imag_diag_start_set
        bridge.imag_diag_start_set = false
        for i in 1:bridge.n
            k = MOI.Utilities.trimap(i, bridge.n + i)
            MOI.set(model, attr, bridge.variables[k], nothing)
        end
    elseif value !== nothing && !bridge.imag_diag_start_set
        bridge.imag_diag_start_set = true
        for i in 1:bridge.n
            k = MOI.Utilities.trimap(i, bridge.n + i)
            MOI.set(model, attr, bridge.variables[k], zero(value))
        end
    end
    return
end
