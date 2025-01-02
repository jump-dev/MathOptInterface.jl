# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    _soc_to_psd_matrix(
        ::Type{T},
        f::MOI.VectorAffineFunction{T},
        g::MOI.ScalarAffineFunction{T},
    )

Builds a VectorAffineFunction representing the upper (or lower) triangular part
of the matrix:

    [ f[1]     f[2:end]' ]
    [ f[2:end] g * I     ]
"""
function _soc_to_psd_matrix(
    ::Type{T},
    f::Union{MOI.AbstractVectorFunction,AbstractVector},
    g::Union{MOI.AbstractScalarFunction,T},
) where {T}
    F = MOI.Utilities.promote_operation(vcat, T, typeof(g), T)
    f_scalars = MOI.Utilities.eachscalar(f)
    dim = length(f_scalars)
    n = div(dim * (dim + 1), 2)
    h = MOI.Utilities.zero_with_output_dimension(F, n)
    row = MOI.Utilities.trimap(1, 1)
    MOI.Utilities.operate_output_index!(+, T, row, h, f_scalars[1])
    for i in 2:dim
        row = MOI.Utilities.trimap(1, i)
        MOI.Utilities.operate_output_index!(+, T, row, h, f_scalars[i])
        diag = MOI.Utilities.trimap(i, i)
        MOI.Utilities.operate_output_index!(+, T, diag, h, g)
    end
    return h
end

"""
    SOCtoPSDBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`SOCtoPSDBridge` implements the following reformulation:

  * ``||x||_2 \\le t`` into
    ``\\left[\\begin{array}{c c}t & x^\\top \\\\ x & t \\mathbf{I}\\end{array}\\right]\\succeq 0``

!!! warning
    This bridge is not added by default by [`MOI.Bridges.full_bridge_optimizer`](@ref)
    because bridging second order cone constraints to semidefinite constraints
    can be achieved by the [`SOCtoRSOCBridge`](@ref) followed by the
    [`RSOCtoPSDBridge`](@ref), while creating a smaller semidefinite constraint.

## Source node

`SOCtoPSDBridge` supports:

  * `G` in [`MOI.SecondOrderCone`](@ref)

## Target nodes

`SOCtoPSDBridge` creates:

  * `F` in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)
"""
struct SOCtoPSDBridge{T,F,G} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.SecondOrderCone,
    F,
    G,
}
    constraint::MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle}
end

const SOCtoPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{SOCtoPSDBridge{T},OT}

# This bridge destorys a lot of structure and adding PSD variables is almost
# always undesirable. We give this bridge an arbitrarily hight cost so that it
# is used only if necessary.
bridging_cost(::Type{<:SOCtoPSDBridge}) = 10.0

function concrete_bridge_type(
    ::Type{<:SOCtoPSDBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.SecondOrderCone},
) where {T}
    SG = MOI.Utilities.scalar_type(G)
    F = MOI.Utilities.promote_operation(vcat, T, SG, T)
    return SOCtoPSDBridge{T,F,G}
end

function MOI.Bridges.map_set(::Type{<:SOCtoPSDBridge}, set::MOI.SecondOrderCone)
    return MOI.PositiveSemidefiniteConeTriangle(MOI.dimension(set))
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:SOCtoPSDBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.SecondOrderCone(MOI.side_dimension(set))
end

function MOI.Bridges.map_function(::Type{<:SOCtoPSDBridge{T}}, func) where {T}
    # func is (t, x), and we need [t x'; x tI]
    return _soc_to_psd_matrix(T, func, MOI.Utilities.eachscalar(func)[1])
end

function MOI.Bridges.inverse_map_function(::Type{<:SOCtoPSDBridge}, func)
    scalars = MOI.Utilities.eachscalar(func)
    dim = MOI.Utilities.side_dimension_for_vectorized_dimension(length(scalars))
    # The inverse function is the top row of [t x'; x tI]
    return scalars[[MOI.Utilities.trimap(1, i) for i in 1:dim]]
end

function MOI.Bridges.adjoint_map_function(
    ::Type{<:SOCtoPSDBridge{T}},
    func,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    dim = MOI.Utilities.side_dimension_for_vectorized_dimension(length(scalars))
    return MOI.Utilities.operate(
        vcat,
        T,
        # The dual of `t` variable is the sum of the diagonal
        sum(func[MOI.Utilities.trimap(i, i)] for i in 1:dim),
        # The dual of `x` is the top row or first column, excluding (1, 1).
        # There's a factor of 2 as well.
        [2 * func[MOI.Utilities.trimap(i, 1)] for i in 2:dim],
    )
end

# func is (t, x) such that x'x ≤ t^2 and we need to find a PSD matrix
#   [a   x'/2]
#   [x/2 Q   ]
# such that
#   a + tr(Q) == t
# By choosing
#   a = t/2 and Q = t/(2x'x) * xx',
# we get
#   a + tr(Q) = t/2 + t/(2x'x) * tr(xx') = t/2 + t/(2x'x) * tr(x'x) = t
# Moreover, by the Schur complement, the matrix is PSD iff
#   xx'/(2t) ⪯ t/(2x'x) * xx'
#   1/(2t) ≤ t/(2x'x)
#   x'x ≤ t^2
# which is the SOC inequality,
function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:SOCtoPSDBridge{T}},
    func,
) where {T}
    t, x = func[1], func[2:end]
    Q = [t / 2; t; x]
    return MOI.Bridges.inverse_adjoint_map_function(RSOCtoPSDBridge{T}, Q)
end

"""
    RSOCtoPSDBridge{T,F,G} <: Bridges.Constraint.AbstractBridge

`RSOCtoPSDBridge` implements the following reformulation:

  * ``||x||_2^2 \\le 2t\\cdot u`` into
    ``\\left[\\begin{array}{c c}t & x^\\top \\\\ x & 2tu \\mathbf{I}\\end{array}\\right]\\succeq 0``

## Source node

`RSOCtoPSDBridge` supports:

  * `G` in [`MOI.RotatedSecondOrderCone`](@ref)

## Target nodes

`RSOCtoPSDBridge` creates:

  * `F` in [`MOI.PositiveSemidefiniteConeTriangle`](@ref)
"""
struct RSOCtoPSDBridge{T,F,G} <: SetMapBridge{
    T,
    MOI.PositiveSemidefiniteConeTriangle,
    MOI.RotatedSecondOrderCone,
    F,
    G,
}
    constraint::Union{
        MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle},
        MOI.ConstraintIndex{G,MOI.Nonnegatives},
    }
end

const RSOCtoPSD{T,OT<:MOI.ModelLike} =
    SingleBridgeOptimizer{RSOCtoPSDBridge{T},OT}

# This bridge destorys a lot of structure and adding PSD variables is almost
# always undesirable. We give this bridge an arbitrarily hight cost so that it
# is used only if necessary.
bridging_cost(::Type{<:RSOCtoPSDBridge}) = 10.0

function concrete_bridge_type(
    ::Type{<:RSOCtoPSDBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.RotatedSecondOrderCone},
) where {T}
    S = MOI.Utilities.scalar_type(G)
    H = MOI.Utilities.promote_operation(*, T, T, S)
    F = MOI.Utilities.promote_operation(vcat, T, S, H, T)
    return RSOCtoPSDBridge{T,F,G}
end

function MOI.Bridges.added_constraint_types(
    ::Type{<:RSOCtoPSDBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[
        (F, MOI.PositiveSemidefiniteConeTriangle),
        (G, MOI.Nonnegatives),
    ]
end

function MOI.supports(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimalStart,MOI.ConstraintDualStart},
    ::Type{<:RSOCtoPSDBridge{T,F,G}},
) where {T,F,G}
    return MOI.supports(
        model,
        attr,
        MOI.ConstraintIndex{F,MOI.PositiveSemidefiniteConeTriangle},
    ) || MOI.supports(model, attr, MOI.ConstraintIndex{G,MOI.Nonnegatives})
end

function MOI.Bridges.map_set(
    ::Type{<:RSOCtoPSDBridge},
    set::MOI.RotatedSecondOrderCone,
)
    d = MOI.dimension(set)
    if d == 2
        return MOI.Nonnegatives(2)
    else
        return MOI.PositiveSemidefiniteConeTriangle(d - 1)
    end
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:RSOCtoPSDBridge},
    set::MOI.PositiveSemidefiniteConeTriangle,
)
    return MOI.RotatedSecondOrderCone(MOI.side_dimension(set) + 1)
end

function MOI.Bridges.inverse_map_set(
    ::Type{<:RSOCtoPSDBridge},
    ::MOI.Nonnegatives,
)
    return MOI.RotatedSecondOrderCone(2)
end

function MOI.Bridges.map_function(::Type{<:RSOCtoPSDBridge{T}}, func) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    if length(scalars) < 2
        error(
            "Unable to bridge RotatedSecondOrderCone to PSD because the ",
            "dimension is too small: got $(length(scalars)), expected >= 2.",
        )
    elseif length(scalars) == 2
        return func
    end
    # Input is (t, u, x), and we need [t x'; x 2uI]
    h = MOI.Utilities.operate!(*, T, scalars[2], convert(T, 2))
    return _soc_to_psd_matrix(T, scalars[[1; 3:length(scalars)]], h)
end

function MOI.Bridges.inverse_map_function(
    ::Type{<:RSOCtoPSDBridge{T}},
    func,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    if length(scalars) == 2
        return func
    end
    dim = MOI.Utilities.side_dimension_for_vectorized_dimension(length(scalars))
    t = scalars[1]
    # scalars[3] is 2u, so it needs to be divided by 2 to get u.
    u = MOI.Utilities.operate!(/, T, scalars[3], convert(T, 2))
    # x is the top row of the func, excluding (1, 1)
    x = scalars[[MOI.Utilities.trimap(1, i) for i in 2:dim]]
    return MOI.Utilities.operate(vcat, T, t, u, x)
end

function MOI.Bridges.adjoint_map_function(
    ::Type{<:RSOCtoPSDBridge{T}},
    func,
) where {T}
    scalars = MOI.Utilities.eachscalar(func)
    if length(scalars) == 2
        return func
    end
    dim = MOI.Utilities.side_dimension_for_vectorized_dimension(length(scalars))
    return MOI.Utilities.operate(
        vcat,
        T,
        # `t` is the (1, 1) element
        func[1],
        # `u` is sum of diagonals
        2 * sum(func[MOI.Utilities.trimap(i, i)] for i in 2:dim),
        # x is the first column, excluding (1, 1)
        [2 * func[MOI.Utilities.trimap(i, 1)] for i in 2:dim],
    )
end

# func is (t, u, x) such that x'x ≤ 2tu and we need to find a PSD matrix
#   [t   x'/2]
#   [x/2 Q   ]
# such that
#   2tr(Q) == u
# By choosing
#   Q = u/(2x'x) * xx',
# we get
#   2tr(Q) = u/(x'x) * tr(xx') = u/(x'x) * tr(x'x) = u
# Moreover, by the Schur complement, the matrix is PSD iff
#   xx'/(4t) ⪯ u/(2x'x) * xx'
#   1/(4t) ≤ u/(2x'x)
#   x'x ≤ 2tu
# which is the RSOC inequality
function MOI.Bridges.inverse_adjoint_map_function(
    ::Type{<:RSOCtoPSDBridge{T}},
    func,
) where {T}
    if length(func) == 2
        return func
    end
    t, u, x = func[1], func[2], func[3:end]
    Q = (u / (2 * (x' * x))) * (x * x')
    M = [t x'/2; x/2 Q]
    return [M[i, j] for j in axes(M, 2) for i in 1:j]
end
