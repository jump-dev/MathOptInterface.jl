# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Let S₊ be the cone of symmetric semidefinite matrices in
# the n*(n+1)/2 dimensional space of symmetric R^{nxn} matrices.
# It is well known that S₊ is a self-dual proper cone.
# Let P₊ be the cone of symmetric semidefinite matrices in
# the n^2 dimensional space of R^{nxn} matrices and
# let D₊ be the cone of matrices A such that A+Aᵀ ∈ P₊.
# P₊ is not proper since it is not solid (as it is not n^2 dimensional) so it is not ensured that (P₊)** = P₊
# However this is the case since, as we will see, (P₊)* = D₊ and (D₊)* = P₊.
# * Let us first see why (P₊)* = D₊.
#   If B is symmetric, then ⟨A,B⟩ = ⟨Aᵀ,Bᵀ⟩ = ⟨Aᵀ,B⟩ so 2⟨A,B⟩ = ⟨A,B⟩ + ⟨Aᵀ,B⟩ = ⟨A+Aᵀ,B⟩
#   Therefore, ⟨A,B⟩ ⩾ 0 for all B ∈ P₊ if and only if ⟨A+Aᵀ,B⟩ ⩾ 0 for all B ∈ P₊
#   Since A+Aᵀ is symmetric and we know that S₊ is self-dual, we have shown that (P₊)*
#   is the set of matrices A such that A+Aᵀ is PSD
# * Let us now see why (D₊)* = P₊.
#   Since A ∈ D₊ implies that Aᵀ ∈ D₊, B ∈ (D₊)* means that ⟨A+Aᵀ,B⟩ ⩾ 0 for any A ∈ D₊ hence B is positive semi-definite.
#   To see why it should be symmetric, simply notice that if B[i,j] < B[j,i] then ⟨A,B⟩ can be made arbitrarily small by setting
#   A[i,j] += s
#   A[j,i] -= s
#   with s arbitrarilly large, and A stays in D₊ as A+Aᵀ does not change.
#
# Typically, SDP primal/dual are presented as
# min ⟨C, X⟩                                                                max ∑ b_ky_k
# ⟨A_k, X⟩ = b_k ∀k                                                         C - ∑ A_ky_k ∈ S₊
#        X ∈ S₊                                                                      y_k free ∀k
# Here, as we allow A_i to be non-symmetric, we should rather use
# min ⟨C, X⟩                                                                max ∑ b_ky_k
# ⟨A_k, X⟩ = b_k ∀k                                                         C - ∑ A_ky_k ∈ P₊
#        X ∈ D₊                                                                      y_k free ∀k
# which is implemented as
# min ⟨C, Z⟩ + (C[i,j]-C[j-i])s[i,j]                                        max ∑ b_ky_k
# ⟨A_k, Z⟩ + (A_k[i,j]-A_k[j,i])s[i,j] = b_k ∀k                   C+Cᵀ - ∑ (A_k+A_kᵀ)y_k ∈ S₊
#       s[i,j] free  1 ⩽ i,j ⩽ n with i > j     C[i,j]-C[j-i] - ∑ (A_k[i,j]-A_k[j,i])y_k = 0  1 ⩽ i,j ⩽ n with i > j
#        Z ∈ S₊                                                                      y_k free ∀k
# where "∈ S₊" only look at the diagonal and upper diagonal part.
# In the last primal program, we have the variables Z = X + Xᵀ and a upper triangular matrix S such that X = Z + S - Sᵀ

"""
    SquareBridge{T,F,G,TT,ST} <: Bridges.Constraint.AbstractBridge

`SquareBridge` reformulates the constraint of a square matrix belong to `ST`,
into the upper triangular part of the matrix belonging to `TT` and a list of
equality constraints constraining off-diagonal symmetry.

For example, the constraint for the matrix
```math
\\begin{pmatrix}
  1      & 1 + x & 2 - 3x\\\\
  1 +  x & 2 + x & 3 -  x\\\\
  2 - 3x & 2 + x &     2x
\\end{pmatrix}
```
to be PSD can be broken down to the constraint of the symmetric matrix
```math
\\begin{pmatrix}
  1      & 1 + x & 2 - 3x\\\\
  \\cdot & 2 + x & 3 -  x\\\\
  \\cdot & \\cdot &    2x
\\end{pmatrix}
```
and the equality constraint between the off-diagonal entries (2, 3) and (3, 2)
``3 - x == 2 + x``. Note that no symmetrization constraint needs to be added
between the off-diagonal entries (1, 2) and (2, 1) or between (1, 3) and (3, 1)
because the expressions are the same.

## Source node

`VectorizeBridge` supports:

  * `F` in `ST`

## Target nodes

`VectorizeBridge` creates:

  * `G` in `TT`
"""
struct SquareBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractScalarFunction,
    TT<:MOI.AbstractSymmetricMatrixSetTriangle,
    ST<:MOI.AbstractSymmetricMatrixSetSquare,
} <: AbstractBridge
    square_set::ST
    triangle::MOI.ConstraintIndex{F,TT}
    sym::Vector{Pair{Tuple{Int,Int},MOI.ConstraintIndex{G,MOI.EqualTo{T}}}}
end

const Square{T,OT<:MOI.ModelLike} = SingleBridgeOptimizer{SquareBridge{T},OT}

function bridge_constraint(
    ::Type{SquareBridge{T,F,G,TT,ST}},
    model::MOI.ModelLike,
    f::F,
    s::ST,
) where {T,F,G,TT,ST}
    f_scalars = MOI.Utilities.eachscalar(f)
    sym = Pair{Tuple{Int,Int},MOI.ConstraintIndex{G,MOI.EqualTo{T}}}[]
    dim = MOI.side_dimension(s)
    upper_triangle_indices = Int[]
    trilen = div(dim * (dim + 1), 2)
    sizehint!(upper_triangle_indices, trilen)
    k = 0
    for j in 1:dim
        for i in 1:j
            k += 1
            push!(upper_triangle_indices, k)
            # We constrain the entries (i, j) and (j, i) to be equal
            f_ij, f_ji = f_scalars[i+(j-1)*dim], f_scalars[j+(i-1)*dim]
            diff = MOI.Utilities.operate!(-, T, f_ij, f_ji)
            MOI.Utilities.canonicalize!(diff)
            # The value 1e-10 was decided in https://github.com/jump-dev/JuMP.jl/pull/976
            # This avoid generating symmetrization constraints when the
            # functions at entries (i, j) and (j, i) are almost identical
            if !MOI.Utilities.isapprox_zero(diff, 1e-10)
                if MOIU.isapprox_zero(diff, 1e-8)
                    @warn "The entries ($i, $j) and ($j, $i) of the" *
                          " positive semidefinite constraint are almost" *
                          " identical but a constraint is added to ensure their" *
                          " equality because the largest difference between the" *
                          " coefficients is smaller than 1e-8 but larger than" *
                          " 1e-10."
                end
                ci = MOI.Utilities.normalize_and_add_constraint(
                    model,
                    diff,
                    MOI.EqualTo(zero(T));
                    allow_modify_function = true,
                )
                push!(sym, (i, j) => ci)
            end
        end
        k += dim - j
    end
    @assert length(upper_triangle_indices) == trilen
    triangle = MOI.add_constraint(
        model,
        f_scalars[upper_triangle_indices],
        MOI.triangular_form(s),
    )
    return SquareBridge{T,F,G,TT,ST}(s, triangle, sym)
end

function MOI.supports_constraint(
    ::Type{SquareBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:MOI.AbstractSymmetricMatrixSetSquare},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:SquareBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{SquareBridge{T,F,G,TT,ST}},
) where {T,F,G,TT,ST}
    return Tuple{Type,Type}[(F, TT), (G, MOI.EqualTo{T})]
end

function concrete_bridge_type(
    ::Type{<:SquareBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ST::Type{<:MOI.AbstractSymmetricMatrixSetSquare},
) where {T}
    S = MOI.Utilities.scalar_type(F)
    G = MOI.Utilities.promote_operation(-, T, S, S)
    TT = MOI.triangular_form(ST)
    return SquareBridge{T,F,G,TT,ST}
end

function MOI.get(
    ::SquareBridge{T,F,G,TT},
    ::MOI.NumberOfConstraints{F,TT},
)::Int64 where {T,F,G,TT}
    return 1
end

function MOI.get(
    bridge::SquareBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.EqualTo{T}},
)::Int64 where {T,F,G}
    return length(bridge.sym)
end

function MOI.get(
    bridge::SquareBridge{T,F,G,TT},
    ::MOI.ListOfConstraintIndices{F,TT},
) where {T,F,G,TT}
    return [bridge.triangle]
end

function MOI.get(
    bridge::SquareBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.EqualTo{T}},
) where {T,F,G}
    return [ci for (_, ci) in bridge.sym]
end

function MOI.delete(model::MOI.ModelLike, bridge::SquareBridge)
    MOI.delete(model, bridge.triangle)
    for (_, ci) in bridge.sym
        MOI.delete(model, ci)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::SquareBridge{T},
) where {T}
    value = MOI.Utilities.eachscalar(MOI.get(model, attr, bridge.triangle))
    dim = MOI.side_dimension(bridge.square_set)
    f = Vector{eltype(value)}(undef, dim^2)
    k = 0
    for j in 1:dim, i in 1:j
        k += 1
        f[i+(j-1)*dim] = f[j+(i-1)*dim] = value[k]
    end
    for ((i, j), ci) in bridge.sym
        # diff is f_ij - f_ji = 0
        diff = MOI.get(model, MOI.ConstraintFunction(), ci)
        # f_ij - (fij - f_ji) = f_ji
        f_ji = MOI.Utilities.operate(-, T, f[i+(j-1)*dim], diff)
        # But we need to account for the constant moved into the set
        rhs = MOI.constant(MOI.get(model, MOI.ConstraintSet(), ci))
        f_ji = MOI.Utilities.operate!(-, T, f_ji, rhs)
        f[j+(i-1)*dim] = MOI.Utilities.convert_approx(eltype(f), f_ji)
    end
    return MOI.Utilities.vectorize(f)
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::SquareBridge)
    return bridge.square_set
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::SquareBridge{T},
) where {T}
    value = MOI.get(model, attr, bridge.triangle)
    dim = MOI.side_dimension(bridge.square_set)
    primal = Vector{eltype(value)}(undef, dim^2)
    k = 0
    for j in 1:dim, i in 1:j
        k += 1
        primal[i+(j-1)*dim] = primal[j+(i-1)*dim] = value[k]
    end
    return primal
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::SquareBridge,
)
    # The constraint dual of the triangular constraint.
    tri = MOI.get(model, attr, bridge.triangle)
    # Our output will be a dense square matrix.
    dim = MOI.side_dimension(bridge.square_set)
    dual = Vector{eltype(tri)}(undef, dim^2)
    # Start by converting the triangular dual to the square dual, assuming that
    # all elements are symmetrical.
    k = 0
    sym_index = 1
    for j in 1:dim, i in 1:j
        k += 1
        upper_index, lower_index = i + (j - 1) * dim, j + (i - 1) * dim
        if i == j
            dual[upper_index] = tri[k]
        elseif sym_index <= length(bridge.sym) &&
               bridge.sym[sym_index].first == (i, j)
            # The PSD constraint uses only the upper triangular part. Therefore,
            # for KKT to hold for the user model, the dual given by the user
            # needs to be attributed to the upper triangular entry. For example,
            # suppose the constraint is
            #   [0 x; y 0] in PositiveSemidefiniteConeSquare(2).
            # If the dual is
            #   [λ1 λ3; λ2 λ4]
            # then we have `y λ2 + x λ3` in the Lagrangian.
            #
            # In the bridged model, the constraint is
            #   [0, x, 0] in PositiveSemidefiniteConeTriangle(2).
            #   [x - y] in Zeros(1)
            # If the dual is
            #   [η1, η2, η3] in PositiveSemidefiniteConeTriangle(2).
            #   [π] in Reals(1)
            # then we have `2x η2 + x * π - y * π` in the Lagrangian.
            #
            # To have the same Lagrangian value, we should set `λ3 = 2η2 + π`
            # and `λ2 = 0 - π`.
            π = MOI.get(model, attr, bridge.sym[sym_index].second)
            dual[upper_index] = 2tri[k] + π
            dual[lower_index] = -π
            sym_index += 1
        else
            # If there are no symmetry constraint, it means that the entries are
            # symbolically the same so we can consider we have the average
            # of the lower and upper triangular entries to the bridged model
            # in which case we can give the dual to both upper and triangular
            # entries.
            dual[upper_index] = dual[lower_index] = tri[k]
        end
    end
    return dual
end
