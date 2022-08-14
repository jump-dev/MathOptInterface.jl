# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    HermitianToSymmetricPSDBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`HermitianToSymmetricPSDBridge` implements the following reformulation:

  * Hermitian positive semidefinite `n x n` complex matrix to a symmetric
    positive semidefinite `2n x 2n` real matrix.

See also [`MOI.Bridges.Variable.HermitianToSymmetricPSDBridge`](@ref).

## Source node

`HermitianToSymmetricPSDBridge` supports:

  * `G` in [`MOI.HermitianPositiveSemidefiniteConeTriangle`](@ref)

## Target node

`HermitianToSymmetricPSDBridge` creates:

  * `F` in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)

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

The bridge achieves this reformulation by constraining the above matrix to
belong to the `MOI.PositiveSemidefiniteConeTriangle(6)`.
"""
struct HermitianToSymmetricPSDBridge{T,F,G} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.HermitianPositiveSemidefiniteConeTriangle,
    F,
    G,
}
    constraint::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
end

const HermitianToSymmetricPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{HermitianToSymmetricPSDBridge{T},OT}

function _promote_minus_vcat(::Type{T}, ::Type{G}) where {T,G}
    S = MOI.Utilities.scalar_type(G)
    M = MOI.Utilities.promote_operation(-, T, S)
    F = MOI.Utilities.promote_operation(vcat, T, S, M, T)
    return F
end

function concrete_bridge_type(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.HermitianPositiveSemidefiniteConeTriangle},
) where {T}
    F = _promote_minus_vcat(T, G)
    return HermitianToSymmetricPSDBridge{T,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:HermitianToSymmetricPSDBridge},
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
)
    return MOI.PositiveSemidefiniteConeTriangle(2set.side_dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:HermitianToSymmetricPSDBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    dim = set.side_dimension
    @assert iseven(dim)
    return MOI.HermitianPositiveSemidefiniteConeTriangle(div(dim, 2))
end

function MOI.Bridges.map_function(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    complex_scalars = MOI.Utilities.eachscalar(func)
    S = MOI.Utilities.scalar_type(_promote_minus_vcat(T, typeof(func)))
    complex_dim = length(complex_scalars)
    complex_set = MOI.Utilities.set_with_dimension(
        MOI.HermitianPositiveSemidefiniteConeTriangle,
        complex_dim,
    )
    n = complex_set.side_dimension
    real_set = MOI.PositiveSemidefiniteConeTriangle(2n)
    real_dim = MOI.dimension(real_set)
    real_scalars = Vector{S}(undef, real_dim)
    complex_index = 0
    half_real_set = MOI.PositiveSemidefiniteConeTriangle(n)
    half_real_dim = MOI.dimension(half_real_set)
    real_index_1 = 0
    real_index_2 = half_real_dim + n
    for j in 1:n
        for i in 1:j
            complex_index += 1
            real_index_1 += 1
            real_scalars[real_index_1] = complex_scalars[complex_index]
            real_index_2 += 1
            real_scalars[real_index_2] = complex_scalars[complex_index]
            if i == j
                real_index_2 += n
            end
        end
    end
    real_index_1 = half_real_dim
    real_index_2 = real_dim - n + 1
    for j in 1:n
        for i in 1:(j-1)
            complex_index += 1
            real_index_1 += 1
            real_scalars[real_index_1] = complex_scalars[complex_index]
            real_index_2 -= 1
            real_scalars[real_index_2] =
                MOI.Utilities.operate(-, T, complex_scalars[complex_index])
        end
        real_scalars[real_index_1+1] = zero(S)
        real_index_1 += n + 1
        real_index_2 -= 2 * (n - j) + 1
    end
    @assert length(complex_scalars) == complex_index
    return MOI.Utilities.vectorize(real_scalars)
end

function MOI.Bridges.inverse_map_function(
    BT::Type{<:HermitianToSymmetricPSDBridge},
    func,
)
    real_scalars = MOI.Utilities.eachscalar(func)
    real_set = MOI.Utilities.set_with_dimension(
        MOI.PositiveSemidefiniteConeTriangle,
        length(real_scalars),
    )
    @assert iseven(real_set.side_dimension)
    n = div(real_set.side_dimension, 2)
    complex_set = MOI.HermitianPositiveSemidefiniteConeTriangle(n)
    complex_scalars =
        Vector{eltype(real_scalars)}(undef, MOI.dimension(complex_set))
    real_index = 0
    complex_index = 0
    for j in 1:n
        for i in 1:j
            complex_index += 1
            real_index += 1
            complex_scalars[complex_index] = real_scalars[real_index]
        end
    end
    for j in 1:n
        for i in 1:(j-1)
            complex_index += 1
            real_index += 1
            complex_scalars[complex_index] = real_scalars[real_index]
        end
        real_index += n + 1
    end
    @assert length(complex_scalars) == complex_index
    return MOI.Utilities.vectorize(complex_scalars)
end

function MOI.Bridges.adjoint_map_function(
    BT::Type{<:HermitianToSymmetricPSDBridge},
    func,
)
    real_scalars = MOI.Utilities.eachscalar(func)
    real_dim = length(real_scalars)
    real_set = MOI.Utilities.set_with_dimension(
        MOI.PositiveSemidefiniteConeTriangle,
        real_dim,
    )
    @assert iseven(real_set.side_dimension)
    n = div(real_set.side_dimension, 2)
    complex_set = MOI.HermitianPositiveSemidefiniteConeTriangle(n)
    complex_scalars =
        Vector{eltype(real_scalars)}(undef, MOI.dimension(complex_set))
    complex_index = 0
    half_real_set = MOI.PositiveSemidefiniteConeTriangle(n)
    half_real_dim = MOI.dimension(half_real_set)
    real_index_1 = 0
    real_index_2 = half_real_dim + n
    for j in 1:n
        for i in 1:j
            complex_index += 1
            real_index_1 += 1
            real_index_2 += 1
            complex_scalars[complex_index] =
                real_scalars[real_index_1] + real_scalars[real_index_2]
            if i == j
                real_index_2 += n
            end
        end
    end
    real_index_1 = half_real_dim
    real_index_2 = real_dim - n + 1
    for j in 1:n
        for i in 1:(j-1)
            complex_index += 1
            real_index_1 += 1
            real_index_2 -= 1
            complex_scalars[complex_index] =
                real_scalars[real_index_1] - real_scalars[real_index_2]
        end
        real_index_1 += n + 1
        real_index_2 -= 2 * (n - j) + 1
    end
    @assert length(complex_scalars) == complex_index
    return MOI.Utilities.vectorize(complex_scalars)
end

# FIXME
# It's not so clear how to do this one since the adjoint is not invertible
# and it's not obvious how to generate a PSD matrix in the preimage.
# The following heuristic may not be the best:
function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    complex_scalars = MOI.Utilities.eachscalar(func)
    S = MOI.Utilities.scalar_type(_promote_minus_vcat(T, typeof(func)))
    complex_dim = length(complex_scalars)
    complex_set = MOI.Utilities.set_with_dimension(
        MOI.HermitianPositiveSemidefiniteConeTriangle,
        complex_dim,
    )
    n = complex_set.side_dimension
    real_set = MOI.PositiveSemidefiniteConeTriangle(2n)
    real_dim = MOI.dimension(real_set)
    real_scalars = Vector{S}(undef, real_dim)
    complex_index = 0
    half_real_set = MOI.PositiveSemidefiniteConeTriangle(n)
    half_real_dim = MOI.dimension(half_real_set)
    real_index_1 = 0
    real_index_2 = half_real_dim + n
    for j in 1:n
        for i in 1:j
            complex_index += 1
            real_index_1 += 1
            real_scalars[real_index_1] = complex_scalars[complex_index] / 2
            real_index_2 += 1
            real_scalars[real_index_2] = complex_scalars[complex_index] / 2
            if i == j
                real_index_2 += n
            end
        end
    end
    real_index_1 = half_real_dim
    real_index_2 = real_dim - n + 1
    for j in 1:n
        for i in 1:(j-1)
            complex_index += 1
            real_index_1 += 1
            real_scalars[real_index_1] = complex_scalars[complex_index]
            real_index_2 -= 1
            real_scalars[real_index_2] = zero(S)
        end
        real_scalars[real_index_1+1] = zero(S)
        real_index_1 += n + 1
        real_index_2 -= 2 * (n - j) + 1
    end
    @assert length(complex_scalars) == complex_index
    return MOI.Utilities.vectorize(real_scalars)
end
