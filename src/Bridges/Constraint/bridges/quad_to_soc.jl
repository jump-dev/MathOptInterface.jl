# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    QuadtoSOCBridge{T} <: Bridges.Constraint.AbstractBridge

`QuadtoSOCBridge` converts quadratic inequalities
```math
\\frac{1}{2}x^T Q x + a^T x + b \\le 0
```
into [`MOI.RotatedSecondOrderCone`](@ref) constraints, but it only applies when
``Q`` is positive definite.

This is because, if `Q` is positive definite, there exists `U` such that
``Q = U^T U``, and so the inequality can then be rewritten as;
```math
\\|U x\\|_2^2 \\le 2 (-a^T x - b)
```

Therefore, `QuadtoSOCBridge` implements the following reformulation:

  * ``\\frac{1}{2}x^T Q x + a^T x + b \\le 0`` into
    ``(1, -a^T x - b, Ux) \\in RotatedSecondOrderCone``

## Source node

`QuadtoSOCBridge` supports:

  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.LessThan{T}`](@ref)
  * [`MOI.ScalarAffineFunction{T}`](@ref) in [`MOI.GreaterThan{T}`](@ref)

## Target nodes

`RelativeEntropyBridge` creates:

  * [`MOI.VectorAffineFunction{T}`](@ref) in [`MOI.RotatedSecondOrderCone`](@ref)

## Errors

This bridge errors if `Q` is not positive definite.
"""
struct QuadtoSOCBridge{T} <: AbstractBridge
    soc::MOI.ConstraintIndex{
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    }
    dimension::Int  # dimension of the SOC constraint
    less_than::Bool # whether the constraint was ≤ or ≥
    set_constant::T # the constant that was on the set
end

const QuadtoSOC{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{QuadtoSOCBridge{T},OT}

function bridge_constraint(
    ::Type{QuadtoSOCBridge{T}},
    model,
    func::MOI.ScalarQuadraticFunction{T},
    set::Union{MOI.LessThan{T},MOI.GreaterThan{T}},
) where {T}
    less_than = set isa MOI.LessThan{T}
    scale = less_than ? -1 : 1
    Q, index_to_variable_map =
        _matrix_from_quadratic_terms(func.quadratic_terms)
    if !less_than
        LinearAlgebra.rmul!(Q, -1)
    end
    F = try
        LinearAlgebra.cholesky(LinearAlgebra.Symmetric(Q))
    catch
        throw(
            MOI.UnsupportedConstraint{typeof(func),typeof(set)}(
                "Unable to transform a quadratic constraint into a " *
                "second-order cone constraint because the quadratic " *
                "constraint is not strongly convex.\n\nConvex constraints " *
                "that are not strongly convex (i.e., the matrix is positive " *
                "semidefinite but not positive definite) are not supported " *
                "yet.\n\nNote that a quadratic equality constraint is " *
                "non-convex.",
            ),
        )
    end
    # Construct the VectorAffineFunction. We're aiming for:
    #  |          1 |
    #  | -a^T x - b | ∈ RotatedSecondOrderCone()
    #  |     Lx + 0 |
    # Start with the -a^T x terms...
    vector_terms = MOI.VectorAffineTerm{T}[
        MOI.VectorAffineTerm(
            2,
            MOI.ScalarAffineTerm(scale * term.coefficient, term.variable),
        ) for term in func.affine_terms
    ]
    # Add the Lx terms...
    L, p = SparseArrays.sparse(F.L), F.p
    I, J, V = SparseArrays.findnz(L)
    for i in 1:length(V)
        # Cholesky is a pivoted decomposition, so L × L' == Q[p, p].
        # To get the variable, we need the row of L, I[i], then to map that
        # through the permutation vector, so `p[I[i]]`, and then through the
        # index_to_variable_map.
        xi = index_to_variable_map[p[I[i]]]
        push!(
            vector_terms,
            MOI.VectorAffineTerm(J[i] + 2, MOI.ScalarAffineTerm(V[i], xi)),
        )
    end
    # This is the [1, b, 0] vector...
    set_constant = MOI.constant(set)
    vector_constant = vcat(
        one(T),
        scale * (func.constant - set_constant),
        zeros(T, size(L, 1)),
    )
    f = MOI.VectorAffineFunction(vector_terms, vector_constant)
    dimension = MOI.output_dimension(f)
    soc = MOI.add_constraint(model, f, MOI.RotatedSecondOrderCone(dimension))
    return QuadtoSOCBridge(soc, dimension, less_than, set_constant)
end

function _matrix_from_quadratic_terms(
    terms::Vector{MOI.ScalarQuadraticTerm{T}},
) where {T}
    variable_to_index_map = Dict{MOI.VariableIndex,Int}()
    index_to_variable_map = Dict{Int,MOI.VariableIndex}()
    n = 0
    for term in terms
        for variable in (term.variable_1, term.variable_2)
            if !(variable in keys(variable_to_index_map))
                n += 1
                variable_to_index_map[variable] = n
                index_to_variable_map[n] = variable
            end
        end
    end
    I, J, V = Int[], Int[], T[]
    for term in terms
        i = variable_to_index_map[term.variable_1]
        j = variable_to_index_map[term.variable_2]
        push!(I, i)
        push!(J, j)
        push!(V, term.coefficient)
        if i != j
            push!(I, j)
            push!(J, i)
            push!(V, term.coefficient)
        end
    end
    # Duplicate terms are summed together in `sparse`
    return SparseArrays.sparse(I, J, V, n, n), index_to_variable_map
end

function MOI.supports_constraint(
    ::Type{QuadtoSOCBridge{T}},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{<:Union{MOI.LessThan{T},MOI.GreaterThan{T}}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:QuadtoSOCBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{QuadtoSOCBridge{T}},
) where {T}
    return Tuple{Type,Type}[(
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    ),]
end

function concrete_bridge_type(
    ::Type{<:QuadtoSOCBridge{T}},
    ::Type{MOI.ScalarQuadraticFunction{T}},
    ::Type{<:Union{MOI.LessThan{T},MOI.GreaterThan{T}}},
) where {T}
    return QuadtoSOCBridge{T}
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::QuadtoSOCBridge{T},
    ::MOI.NumberOfConstraints{
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    },
)::Int64 where {T}
    return 1
end

function MOI.get(
    bridge::QuadtoSOCBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.VectorAffineFunction{T},
        MOI.RotatedSecondOrderCone,
    },
) where {T}
    return [bridge.soc]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::QuadtoSOCBridge)
    MOI.delete(model, bridge.soc)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::QuadtoSOCBridge,
)
    soc = MOI.get(model, attr, bridge.soc)
    output = sum(soc[i]^2 for i in 3:bridge.dimension)
    output /= 2
    output -= soc[1] * soc[2]
    if !bridge.less_than
        output = -output
    end
    output += bridge.set_constant
    return output
end

# Lemma: If (1, s, x), (v, u, y) in RotatedSecondOrderCone and
#        (1, s, x) ⋅ (v, u, y) = 0, then we have
#        y = -u*x, v = u*||x||_2^2/2.
# Proof: We have
#        (1, s, x) ⋅ (v, u, y) = v + s * u + x ⋅ y
#        (Cauchy-Schwarz)      ≥ v + s * u - ||x||_2 * ||y||_2
#        (RotatedSOC)          ≥ v + s * u - 2 * √(s * u * v)
#        (AM-GM)               ≥ v + s * u - 2 * (v + s * u) / 2
#                              = 0
#        By assumption, the left-hand side is zero, hence all inequalities
#        are equalities. By Cauchy-Schwarz, this means that ∃σ ≥ 0 such
#        that `y = -σ*x`. By AM-GM, we have `v = s * u`.
#        By RotatedSOC, we have either:
#        1) `||y||_2^2 < 2 * u * v` and `||x||_2^2 = 2 * s = 0`: That implies
#           that `v = 0 * u = 0` hence `||y||_2^2 < 0` which is impossible.
#        2) `||x||_2^2 < 2 * s` and `||y||_2^2 = 2 * u * v = 0`: we have either:
#           a) `u = 0`: hence `v = s * u = 0` or
#           b) `v = 0`: since `s > 0`, `u = v / s = 0`.
#           In any case, `y = 0` and `u = v = 0` hence the statement holds.
#        3) `||x||_2^2 = 2 * s` and `||y||_2^2 = 2 * u * v`: we have
#           `σ^2 * ||x||_2^2 = ||y||_2^2 = 2 * u * v = 2 * u^2 * s` hence
#           `u = σ`. It follows that at `v = s * u = σ * ||x||_2^2/2`.         □
#
# It follows from the Lemma that
# (1, s, x) ⋅ (v, u, y) = u * (1, s, x) ⋅ (||x||_2^2/2, 1, -x)
#                       = u * (||x||_2^2/2 + s - ||x||_2^2)
#                       = u * (-||x||_2^2/2 + s)
# Given a constraint `z^T Q z/2 + a^T z + b ≤ 0` that was transformed,
# where Q = U^T * U$, we have `x = U * z` and `s = -a^T z - b` hence, we have
#                       = -u * (z^T Q z/2 + a^T z + b)
# So the dual of the quadratic constraint is `-u`, so that the contribution
# to the lagrangian function of both the quadratic and RotatedSOC formulation
# is exactly the same. Q.E.D.
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    bridge::QuadtoSOCBridge,
)
    λ = MOI.get(model, attr, bridge.soc)[2]
    return bridge.less_than ? -λ : λ
end

function MOI.get(
    ::MOI.ModelLike,
    ::MOI.ConstraintSet,
    b::QuadtoSOCBridge{T},
) where {T}
    if b.less_than
        return MOI.LessThan(b.set_constant)
    else
        return MOI.GreaterThan(b.set_constant)
    end
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    b::QuadtoSOCBridge{T},
) where {T}
    f = MOI.get(model, attr, b.soc)
    fs = MOI.Utilities.eachscalar(f)
    q = zero(MOI.ScalarQuadraticFunction{T})
    for i in 3:MOI.output_dimension(f)
        term = MOI.Utilities.operate(*, T, fs[i], fs[i])
        term = MOI.Utilities.operate!(/, T, term, 2 * one(T))
        MOI.Utilities.operate!(+, T, q, term)
    end
    MOI.Utilities.operate!(-, T, q, fs[2])
    if !b.less_than
        MOI.Utilities.operate!(-, T, q)
    end
    q.constant += b.set_constant
    return q
end
