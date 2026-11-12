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
    # `+ n` is for jumping over the indices corresponding to the imaginary part
    real_index_2 = half_real_dim + n
    for j in 1:n
        for i in 1:j
            complex_index += 1
            real_index_1 += 1
            real_scalars[real_index_1] = complex_scalars[complex_index]
            real_index_2 += 1
            real_scalars[real_index_2] = complex_scalars[complex_index]
            if i == j
                # We are at the end of a column so we do `+ n` for jumping over
                # the indices corresponding to the imaginary part
                real_index_2 += n
            end
        end
    end
    # Index corresponding to the upper triangular part
    real_index_1 = half_real_dim
    for j in 1:n
        for i in 1:(j-1)
            complex_index += 1
            real_index_1 += 1
            real_scalars[real_index_1] = complex_scalars[complex_index]
            real_scalars[MOI.Utilities.trimap(j, n + i)] =
                MOI.Utilities.operate(-, T, complex_scalars[complex_index])
        end
        real_scalars[real_index_1+1] = zero(S)
        # jump over the lower triangular part and the (2, 2) block
        real_index_1 += n + 1
    end
    @assert length(complex_scalars) == complex_index
    return MOI.Utilities.vectorize(real_scalars)
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:HermitianToSymmetricPSDBridge},
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
    BT::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    real_dim = length(MOI.Utilities.eachscalar(func))
    real_set = MOI.Utilities.set_with_dimension(
        MOI.PositiveSemidefiniteConeTriangle,
        real_dim,
    )
    @assert iseven(real_set.side_dimension)
    n = div(real_set.side_dimension, 2)
    S = MOI.Utilities.scalar_type(_promote_minus_vcat(T, typeof(func)))
    scalars = Vector{S}(undef, n^2)
    for i in eachindex(scalars)
        scalars[i] = MOI.Bridges.adjoint_map_function(
            BT,
            func,
            MOI.Bridges.IndexInVector(i),
        )
    end
    return MOI.Utilities.vectorize(scalars)
end

function MOI.Bridges.adjoint_map_function(
    ::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
    index::MOI.Bridges.IndexInVector,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    n = div(
        MOI.Utilities.side_dimension_for_vectorized_dimension(length(scalars)),
        2,
    )
    N = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(n))
    if index.value <= N
        i, j = MOI.Utilities.inverse_trimap(index.value)
        k = MOI.Utilities.trimap(n + i, n + j)
        return MOI.Utilities.operate(+, T, scalars[index.value], scalars[k])
    end
    i, j = MOI.Utilities.inverse_trimap(index.value - N)
    j += 1  # The imaginary part excludes the diagonal.
    k12 = MOI.Utilities.trimap(i, n + j)
    k21 = MOI.Utilities.trimap(j, n + i)
    return MOI.Utilities.operate(-, T, scalars[k12], scalars[k21])
end

function MOI.Bridges.inverse_adjoint_map_function(
    BT::Type{<:HermitianToSymmetricPSDBridge{T}},
    func,
) where {T}
    # F* F = 2I, so F / 2 is a right inverse of F* that preserves PSD.
    mapped = MOI.Bridges.map_function(BT, func)
    return MOI.Utilities.operate(/, T, mapped, T(2))
end
