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
    SquarePSDBridge{T}

The `SquarePSDBridge` reformulates the constraint of a square matrix to be PSD
and symmetric, i.e. belongs to the [`MOI.PositiveSemidefiniteConeSquare`](@ref),
to a list of equality constraints for pair or off-diagonal entries with
different expressions and a PSD constraint the upper triangular part of the
matrix.

For instance, the constraint for the matrix
```math
\\begin{pmatrix}
  1      & 1 + x & 2 - 3x\\
  1 +  x & 2 + x & 3 -  x\\
  2 - 3x & 2 + x &     2x
\\end{pmatrix}
```
to be PSD can be broken down to the constraint of the symmetric matrix
```math
\\begin{pmatrix}
  1      & 1 + x & 2 - 3x\\
  \\cdot & 2 + x & 3 -  x\\
  \\cdot & \\cdot &    2x
\\end{pmatrix}
```
and the equality constraint between the off-diagonal entries (2, 3) and (3, 2)
``2x == 1``. Note that now symmetrization constraint need to be added between
the off-diagonal entries (1, 2) and (2, 1) or between (1, 3) and (3, 1) since
the expressions are the same.
"""
struct SquarePSDBridge{T, F<:MOI.AbstractVectorFunction,
                       G<:MOI.AbstractScalarFunction} <: AbstractBridge
    side_dimension::Int
    psd::CI{F, MOI.PositiveSemidefiniteConeTriangle}
    sym::Vector{Pair{Tuple{Int, Int}, CI{G, MOI.EqualTo{T}}}}
end
function SquarePSDBridge{T, F, G}(model::MOI.ModelLike, f::F,
                                  s::MOI.PositiveSemidefiniteConeSquare) where {T,
                                                                                F <: MOI.AbstractVectorFunction,
                                                                                G <: MOI.AbstractScalarFunction}
    f_scalars = MOIU.eachscalar(f)
    sym = Pair{Tuple{Int, Int}, CI{G, MOI.EqualTo{T}}}[]
    dim = s.side_dimension
    upper_triangle_indices = Int[]
    trilen = div(dim * (dim + 1), 2)
    sizehint!(upper_triangle_indices, trilen)
    k = 0
    for j in 1:dim
        for i in 1:j
            k += 1
            push!(upper_triangle_indices, k)
            # We constrain the entries (i, j) and (j, i) to be equal
            upper = f_scalars[i + (j - 1) * dim]
            lower = f_scalars[j + (i - 1) * dim]
            diff = MOIU.operate!(-, T, upper, lower)
            MOIU.canonicalize!(diff)
            # The value 1e-10 was decided in https://github.com/JuliaOpt/JuMP.jl/pull/976
            # This avoid generating symmetrization constraints when the
            # functions at entries (i, j) and (j, i) are almost identical
            if !MOIU.isapprox_zero(diff, 1e-10)
                push!(sym, (i, j) => MOIU.add_scalar_constraint(model, diff,
                                                                MOI.EqualTo(zero(T)),
                                                                allow_modify_function=true))
            end
        end
        k += dim - j
    end
    @assert length(upper_triangle_indices) == trilen
    psd = MOI.add_constraint(model, f_scalars[upper_triangle_indices],
                             MOI.PositiveSemidefiniteConeTriangle(dim))
    return SquarePSDBridge(dim, psd, sym)
end

function MOI.supports_constraint(::Type{SquarePSDBridge{T}},
                                ::Type{<:MOI.AbstractVectorFunction},
                                ::Type{MOI.PositiveSemidefiniteConeSquare}) where T
    return true
end
function added_constraint_types(::Type{SquarePSDBridge{T, F, G}}) where {T, F, G}
    return [(F, MOI.PositiveSemidefiniteConeTriangle), (G, MOI.EqualTo{T})]
end
function concrete_bridge_type(::Type{<:SquarePSDBridge{T}},
                              F::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.PositiveSemidefiniteConeSquare}) where T
    S = MOIU.scalar_type(F)
    G = MOIU.promote_operation(-, T, S, S)
    SquarePSDBridge{T, F, G}
end

# Attributes, Bridge acting as an model
function MOI.get(::SquarePSDBridge{T, F},
                 ::MOI.NumberOfConstraints{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F}
    return 1
end
function MOI.get(bridge::SquarePSDBridge{T, F, G},
                 ::MOI.NumberOfConstraints{G, MOI.EqualTo{T}}) where {T, F, G}
    return length(bridge.sym)
end
function MOI.get(bridge::SquarePSDBridge{T, F},
                 ::MOI.ListOfConstraintIndices{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F}
    return [bridge.psd]
end
function MOI.get(bridge::SquarePSDBridge{T, F, G},
                 ::MOI.ListOfConstraintIndices{G, MOI.EqualTo{T}}) where {T, F, G}
    return map(pair -> pair.second, bridge.sym)
end

# Indices
function MOI.delete(model::MOI.ModelLike, bridge::SquarePSDBridge)
    MOI.delete(model, bridge.psd)
    for pair in bridge.sym
        MOI.delete(model, pair.second)
    end
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::SquarePSDBridge{T}) where T
    tri = MOI.get(model, attr, bridge.psd)
    dim = bridge.side_dimension
    sqr = Vector{eltype(tri)}(undef, dim^2)
    k = 0
    for j in 1:dim
        for i in 1:j
            k += 1
            sqr[i + (j - 1) * dim] = sqr[j + (i - 1) * dim] = tri[k]
        end
    end
    return sqr
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 bridge::SquarePSDBridge)
    tri = MOI.get(model, attr, bridge.psd)
    dim = bridge.side_dimension
    sqr = Vector{eltype(tri)}(undef, dim^2)
    k = 0
    for j in 1:dim
        for i in 1:j
            k += 1
            # The psd constraint uses only the upper triangular part
            if i == j
                sqr[i + (j - 1) * dim] = tri[k]
            else
                sqr[i + (j - 1) * dim] = 2tri[k]
                sqr[j + (i - 1) * dim] = zero(eltype(sqr))
            end
        end
    end
    for pair in bridge.sym
        i, j = pair.first
        dual = MOI.get(model, attr, pair.second)
        sqr[i + (j - 1) * dim] += dual
        sqr[j + (i - 1) * dim] -= dual
    end
    return sqr
end
