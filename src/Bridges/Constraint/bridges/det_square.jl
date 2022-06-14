# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    DetSquareBridge{T,F,G,TT,ST} <: Bridges.Constraint.AbstractBridge

`DetSquareBridge` implements the following reformulations:

  * ``(t, u, X) \\in LogDetConeSquare`` into ``(t, u, Y) in LogDetConeTriangle``
  * ``(t, X) \\in RootDetConeSquare`` into ``(t, Y) in RootDetConeTriangle``

where ``Y`` is the upper triangluar component of ``X``.

In addition, constraints are added as necessary to constraint the matrix ``X``
to be symmetric. For example, the constraint for the matrix
```math
\\begin{pmatrix}
  1      & 1 + x & 2 - 3x\\\\
  1 +  x & 2 + x & 3 -  x\\\\
  2 - 3x & 2 + x &     2x
\\end{pmatrix}
```
can be broken down to the constraint of the symmetric matrix
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

`DetSquareBridge` supports:

  * `F` in `ST`

## Target nodes

`DetSquareBridge` creates:

  * `G` in `TT`
"""
struct DetSquareBridge{
    T,
    F<:MOI.AbstractVectorFunction,
    G<:MOI.AbstractScalarFunction,
    TT<:Union{MOI.LogDetConeTriangle,MOI.RootDetConeTriangle},
    ST<:Union{MOI.LogDetConeSquare,MOI.RootDetConeSquare},
} <: AbstractBridge
    square_set::ST
    triangle::MOI.ConstraintIndex{F,TT}
    sym::Vector{Pair{Tuple{Int,Int},MOI.ConstraintIndex{G,MOI.EqualTo{T}}}}
end

const DetSquare{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{DetSquareBridge{T},OT}

_det_square_offset(::MOI.RootDetConeSquare) = Int[1]

_det_square_offset(::MOI.LogDetConeSquare) = Int[1, 2]

function bridge_constraint(
    ::Type{DetSquareBridge{T,F,G,TT,ST}},
    model::MOI.ModelLike,
    f::F,
    s::ST,
) where {T,F,G,TT,ST}
    f_scalars = MOI.Utilities.eachscalar(f)
    sym = Pair{Tuple{Int,Int},MOI.ConstraintIndex{G,MOI.EqualTo{T}}}[]
    dim = s.side_dimension
    upper_triangle_indices = _det_square_offset(s)
    trilen = div(dim * (dim + 1), 2)
    sizehint!(upper_triangle_indices, trilen)
    offset = length(upper_triangle_indices)
    k = offset
    for j in 1:dim
        for i in 1:j
            k += 1
            push!(upper_triangle_indices, k)
            # We constrain the entries (i, j) and (j, i) to be equal
            f_ij = f_scalars[offset+i+(j-1)*dim]
            f_ji = f_scalars[offset+j+(i-1)*dim]
            diff = MOI.Utilities.operate!(-, T, f_ij, f_ji)
            MOI.Utilities.canonicalize!(diff)
            # The value 1e-10 was decided in https://github.com/jump-dev/JuMP.jl/pull/976
            # This avoid generating symmetrization constraints when the
            # functions at entries (i, j) and (j, i) are almost identical
            if !MOI.Utilities.isapprox_zero(diff, 1e-10)
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
    triangle = MOI.add_constraint(
        model,
        f_scalars[upper_triangle_indices],
        MOI.triangular_form(s),
    )
    return DetSquareBridge{T,F,G,TT,ST}(s, triangle, sym)
end

function MOI.supports_constraint(
    ::Type{<:DetSquareBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{<:Union{MOI.LogDetConeSquare,MOI.RootDetConeSquare}},
) where {T}
    return true
end

function MOI.Bridges.added_constrained_variable_types(::Type{<:DetSquareBridge})
    return Tuple{Type}[]
end

function MOI.Bridges.added_constraint_types(
    ::Type{DetSquareBridge{T,F,G,TT,ST}},
) where {T,F,G,TT,ST}
    return Tuple{Type,Type}[(F, TT), (G, MOI.EqualTo{T})]
end

function concrete_bridge_type(
    ::Type{<:DetSquareBridge{T}},
    F::Type{<:MOI.AbstractVectorFunction},
    ST::Type{<:Union{MOI.LogDetConeSquare,MOI.RootDetConeSquare}},
) where {T}
    S = MOI.Utilities.scalar_type(F)
    G = MOI.Utilities.promote_operation(-, T, S, S)
    TT = MOI.triangular_form(ST)
    return DetSquareBridge{T,F,G,TT,ST}
end

function MOI.get(
    ::DetSquareBridge{T,F,G,TT},
    ::MOI.NumberOfConstraints{F,TT},
)::Int64 where {T,F,G,TT}
    return 1
end

function MOI.get(
    bridge::DetSquareBridge{T,F,G},
    ::MOI.NumberOfConstraints{G,MOI.EqualTo{T}},
)::Int64 where {T,F,G}
    return length(bridge.sym)
end

function MOI.get(
    bridge::DetSquareBridge{T,F,G,TT},
    ::MOI.ListOfConstraintIndices{F,TT},
) where {T,F,G,TT}
    return [bridge.triangle]
end

function MOI.get(
    bridge::DetSquareBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{G,MOI.EqualTo{T}},
) where {T,F,G}
    return [ci for (_, ci) in bridge.sym]
end

function MOI.delete(model::MOI.ModelLike, bridge::DetSquareBridge)
    MOI.delete(model, bridge.triangle)
    for (_, ci) in bridge.sym
        MOI.delete(model, ci)
    end
    return
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    bridge::DetSquareBridge{T},
) where {T}
    value = MOI.Utilities.eachscalar(MOI.get(model, attr, bridge.triangle))
    dim = bridge.square_set.side_dimension
    offset = length(_det_square_offset(bridge.square_set))
    f = Vector{eltype(value)}(undef, offset + dim^2)
    for i in 1:offset
        f[i] = value[i]
    end
    k = offset
    for j in 1:dim, i in 1:j
        k += 1
        f[offset+i+(j-1)*dim] = f[offset+j+(i-1)*dim] = value[k]
    end
    for ((i, j), ci) in bridge.sym
        # diff is f_ij - f_ji = 0
        diff = MOI.get(model, MOI.ConstraintFunction(), ci)
        # f_ij - (fij - f_ji) = f_ji
        f_ji = MOI.Utilities.operate(-, T, f[offset+i+(j-1)*dim], diff)
        # But we need to account for the constant moved into the set
        rhs = MOI.constant(MOI.get(model, MOI.ConstraintSet(), ci))
        f_ji = MOI.Utilities.operate!(-, T, f_ji, rhs)
        f[offset+j+(i-1)*dim] = MOI.Utilities.convert_approx(eltype(f), f_ji)
    end
    return MOI.Utilities.vectorize(f)
end

function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::DetSquareBridge)
    return bridge.square_set
end
