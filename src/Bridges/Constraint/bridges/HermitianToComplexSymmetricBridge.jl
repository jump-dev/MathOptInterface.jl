# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    HermitianToComplexSymmetricBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`HermitianToSymmetricBridge` implements the following reformulation:

  * Hermitian positive semidefinite `n x n` represented as a vector of real
    entries with real and imaginary parts on different entries to a vector
    of complex entries.

See also [`MOI.Bridges.Constraint.HermitianToSymmetricPSDBridge`](@ref).

## Source node

`HermitianToComplexSymmetricBridge` supports:

  * `G` in [`MOI.HermitianPositiveSemidefiniteConeTriangle`](@ref)

## Target node

`HermitianToComplexSymmetricBridge` creates:

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
struct HermitianToComplexSymmetricBridge{T,F,G} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.HermitianPositiveSemidefiniteConeTriangle,
    F,
    G,
}
    constraint::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
end

# Should be favored over `HermitianToSymmetricPSDBridge`
MOI.Bridges.bridging_cost(::Type{<:SOCtoPSDBridge}) = 0.5

function _promote_complex_vcat(::Type{T}, ::Type{G}) where {T,G}
    S = MOI.Utilities.scalar_type(G)
    M = MOI.Utilities.promote_operation(*, Complex{T}, S)
    return MOI.Utilities.promote_operation(vcat, T, M)
end

function concrete_bridge_type(
    ::Type{<:HermitianToComplexSymmetricBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.HermitianPositiveSemidefiniteConeTriangle},
) where {T}
    F = _promote_complex_vcat(T, G)
    return HermitianToComplexSymmetricBridge{T,F,G}
end

function MOI.Bridges.map_set(
    ::Type{<:HermitianToComplexSymmetricBridge},
    set::MOI.HermitianPositiveSemidefiniteConeTriangle,
)
    return MOI.PositiveSemidefiniteConeTriangle(set.side_dimension)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:HermitianToComplexSymmetricBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.HermitianPositiveSemidefiniteConeTriangle(set.side_dimension)
end

function MOI.Bridges.map_function(
    ::Type{<:HermitianToComplexSymmetricBridge{T}},
    func,
) where {T}
    complex_scalars = MOI.Utilities.eachscalar(func)
    S = MOI.Utilities.scalar_type(_promote_complex_vcat(T, typeof(func)))
    complex_dim = length(complex_scalars)
    complex_set = MOI.Utilities.set_with_dimension(
        MOI.HermitianPositiveSemidefiniteConeTriangle,
        complex_dim,
    )
    n = complex_set.side_dimension
    real_set = MOI.PositiveSemidefiniteConeTriangle(n)
    real_dim = MOI.dimension(real_set)
    real_scalars = Vector{S}(undef, real_dim)
    real_index = 0
    imag_index = real_dim
    for j in 1:n
        for i in 1:j
            real_index += 1
            if i == j
                real_scalars[real_index] = complex_scalars[real_index]
            else
                imag_index += 1
                real_scalars[real_index] = complex_scalars[real_index] + (one(T) * im) * complex_scalars[imag_index]
            end
        end
    end
    @assert length(real_scalars) == real_index
    @assert length(complex_scalars) == imag_index
    return MOI.Utilities.vectorize(real_scalars)
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:HermitianToComplexSymmetricBridge},
    func,
)
    real_scalars = MOI.Utilities.eachscalar(func)
    real_set = MOI.Utilities.set_with_dimension(
        MOI.PositiveSemidefiniteConeTriangle,
        length(real_scalars),
    )
    n = real_set.side_dimension
    complex_set = MOI.HermitianPositiveSemidefiniteConeTriangle(n)
    complex_scalars =
        Vector{MA.promote_operation(real, MOI.Utilities.scalar_type(typeof(func)))}(undef, MOI.dimension(complex_set))
    real_index = 0
    imag_index = MOI.dimension(real_set)
    for j in 1:n
        for i in 1:j
            real_index += 1
            complex_scalars[real_index] = real(real_scalars[real_index])
            if i != j
                imag_index += 1
                complex_scalars[imag_index] = imag(real_scalars[real_index])
            end
        end
    end
    @assert length(real_scalars) == real_index
    @assert length(complex_scalars) == imag_index
    return MOI.Utilities.vectorize(complex_scalars)
end
