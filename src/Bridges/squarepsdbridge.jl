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
and symmetric, i.e. belongs to the [`MOI.PositiveDefiniteConeSquare`](@ref), to
a list of equality constraints for pair or off-diagonal entries with different
expressions and a PSD constraint the upper triangular part of the matrix.

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
    psd::CI{F, MOI.PositiveDefiniteConeTriangle}
    sym::Vector{CI{G, MOI.EqualTo}}
end
function SplitIntervalBridge{T, F, G}(model, f::F, s::MOI.Interval{T}) where {T, F}
    lower = MOI.addconstraint!(model, f, MOI.GreaterThan(s.lower))
    upper = MOI.addconstraint!(model, f, MOI.LessThan(s.upper))
    return SplitIntervalBridge(lower, upper)
end

MOI.supportsconstraint(::Type{SplitIntervalBridge{T}}, ::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.Interval{T}}) where T = true
addedconstrainttypes(::Type{SplitIntervalBridge{T}}, F::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.Interval{T}}) where T = [(F, MOI.GreaterThan{T}), (F, MOI.LessThan{T})]
concrete_bridge_type(::Type{<:SplitIntervalBridge}, F::Type{<:MOI.AbstractScalarFunction}, ::Type{MOI.Interval{T}}) where T = SplitIntervalBridge{T, F}

# Attributes, Bridge acting as an model
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.NumberOfConstraints{F, MOI.LessThan{T}}) where {T, F} = 1
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F} = 1
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F} = [b.lower]
MOI.get(b::SplitIntervalBridge{T, F}, ::MOI.ListOfConstraintIndices{F, MOI.LessThan{T}}) where {T, F} = [b.upper]

# Indices
function MOI.delete!(model::MOI.ModelLike, c::SplitIntervalBridge)
    MOI.delete!(model, c.lower)
    MOI.delete!(model, c.upper)
end

# Attributes, Bridge acting as a constraint
function MOI.canget(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, ::Type{SplitIntervalBridge{T, F}}) where {T, F}
    return MOI.canget(model, attr, CI{F, MOI.GreaterThan{T}})
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::SplitIntervalBridge)
    # lower and upper should give the same value
    return MOI.get(model, attr, c.lower)
end
function MOI.canget(model::MOI.ModelLike, attr::MOI.ConstraintDual, ::Type{SplitIntervalBridge{T, F}}) where {T, F}
    return MOI.canget(model, attr, CI{F, MOI.GreaterThan{T}}) &&
           MOI.canget(model, attr, CI{F, MOI.LessThan{T}})
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SplitIntervalBridge)
    # Should be nonnegative
    lower_dual = MOI.get(model, MOI.ConstraintDual(), c.lower)
    # Should be nonpositive
    upper_dual = MOI.get(model, MOI.ConstraintDual(), c.upper)
    return lower_dual > -upper_dual ? lower_dual : upper_dual
end

# Constraints
function MOI.modify!(model::MOI.ModelLike, c::SplitIntervalBridge, change::MOI.AbstractFunctionModification)
    MOI.modify!(model, c.lower, change)
    MOI.modify!(model, c.upper, change)
end

MOI.supports(model::MOI.ModelLike, ::MOI.ConstraintFunction, ::Type{<:SplitIntervalBridge}) = true
function MOI.set!(model::MOI.ModelLike, ::MOI.ConstraintFunction,
                  c::SplitIntervalBridge{T, F}, func::F) where {T, F}
    MOI.set!(model, MOI.ConstraintFunction(), c.lower, func)
    MOI.set!(model, MOI.ConstraintFunction(), c.upper, func)
end

MOI.supports(model::MOI.ModelLike, ::MOI.ConstraintSet, ::Type{<:SplitIntervalBridge}) = true
function MOI.set!(model::MOI.ModelLike, ::MOI.ConstraintSet, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.set!(model, MOI.ConstraintSet(), c.lower, MOI.GreaterThan(change.lower))
    MOI.set!(model, MOI.ConstraintSet(), c.upper, MOI.LessThan(change.upper))
end
