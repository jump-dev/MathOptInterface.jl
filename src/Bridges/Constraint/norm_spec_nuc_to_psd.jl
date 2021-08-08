"""
    NormSpectralBridge{T}

The `NormSpectralCone` is representable with a PSD constraint, since
``t \\ge \\sigma_1(X)`` if and only if ``[tI X^\\top; X tI] \\succ 0``.
"""
struct NormSpectralBridge{T,F,G} <: AbstractBridge
    row_dim::Int # row dimension of X
    column_dim::Int # column dimension of X
    psd_index::CI{F,MOI.PositiveSemidefiniteConeTriangle}
end

function bridge_constraint(
    ::Type{NormSpectralBridge{T,F,G}},
    model::MOI.ModelLike,
    f::G,
    s::MOI.NormSpectralCone,
) where {T,F,G}
    f_scalars = MOIU.eachscalar(f)
    t = f_scalars[1]
    row_dim = s.row_dim
    column_dim = s.column_dim
    side_dim = row_dim + column_dim
    psd_set = MOI.PositiveSemidefiniteConeTriangle(side_dim)
    psd_func = MOIU.zero_with_output_dimension(F, MOI.dimension(psd_set))
    for i in 1:side_dim
        MOIU.operate_output_index!(+, T, trimap(i, i), psd_func, t)
    end
    X_idx = 2
    for j in 1:column_dim, i in (column_dim+1):side_dim
        MOIU.operate_output_index!(
            +,
            T,
            trimap(i, j),
            psd_func,
            f_scalars[X_idx],
        )
        X_idx += 1
    end
    psd_index = MOI.add_constraint(model, psd_func, psd_set)
    return NormSpectralBridge{T,F,G}(row_dim, column_dim, psd_index)
end

function MOI.supports_constraint(
    ::Type{NormSpectralBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormSpectralCone},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:NormSpectralBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{NormSpectralBridge{T,F,G}},
) where {T,F,G}
    return Tuple{Type,Type}[(F, MOI.PositiveSemidefiniteConeTriangle)]
end

function concrete_bridge_type(
    ::Type{<:NormSpectralBridge{T}},
    G::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormSpectralCone},
) where {T}
    F = MOIU.promote_operation(vcat, T, MOIU.scalar_type(G), T)
    return NormSpectralBridge{T,F,G}
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::NormSpectralBridge{T,F,G},
    ::MOI.NumberOfConstraints{F,MOI.PositiveSemidefiniteConeTriangle},
)::Int64 where {T,F,G}
    return 1
end

function MOI.get(
    bridge::NormSpectralBridge{T,F,G},
    ::MOI.ListOfConstraintIndices{F,MOI.PositiveSemidefiniteConeTriangle},
) where {T,F,G}
    return [bridge.psd_index]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::NormSpectralBridge)
    MOI.delete(model, bridge.psd_index)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::NormSpectralBridge{T,F,G},
) where {T,F,G}
    psd_func = MOIU.eachscalar(
        MOI.get(model, MOI.ConstraintFunction(), bridge.psd_index),
    )
    t = psd_func[1]
    side_dim = bridge.row_dim + bridge.column_dim
    X = psd_func[[
        trimap(i, j) for j in 1:bridge.column_dim for
        i in (bridge.column_dim+1):side_dim
    ]]
    return MOIU.convert_approx(G, MOIU.operate(vcat, T, t, X))
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::NormSpectralBridge,
)
    return MOI.NormSpectralCone(bridge.row_dim, bridge.column_dim)
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    ::Type{<:NormSpectralBridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintPrimal,MOI.ConstraintPrimalStart},
    bridge::NormSpectralBridge,
)
    primal = MOI.get(model, attr, bridge.psd_index)
    t = primal[1]
    side_dim = bridge.row_dim + bridge.column_dim
    X = primal[[
        trimap(i, j) for j in 1:bridge.column_dim for
        i in (bridge.column_dim+1):side_dim
    ]]
    return vcat(t, X)
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintPrimalStart,
    bridge::NormSpectralBridge{T},
    value,
) where {T}
    column_dim = bridge.column_dim
    side_dim = bridge.row_dim + column_dim
    primal = zeros(T, div(side_dim * (side_dim + 1), 2))
    for i in 1:side_dim
        primal[trimap(i, i)] = value[1]
    end
    X_idx = 2
    for j in 1:column_dim, i in (column_dim+1):side_dim
        primal[trimap(i, j)] = value[X_idx]
        X_idx += 1
    end
    MOI.set(model, MOI.ConstraintPrimalStart(), bridge.psd_index, primal)
    return
end
# Given [U X'; X V] is dual on PSD constraint, the dual on NormSpectralCone
# constraint is (tr(U) + tr(V), 2X) in NormNuclearCone.
function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintDual,
    bridge::NormSpectralBridge,
)
    dual = MOI.get(model, MOI.ConstraintDual(), bridge.psd_index)
    column_dim = bridge.column_dim
    side_dim = bridge.row_dim + column_dim
    t = sum(dual[trimap(i, i)] for i in 1:side_dim)
    X =
        2 * dual[[
            trimap(i, j) for j in 1:column_dim for i in (column_dim+1):side_dim
        ]]
    return vcat(t, X)
end

"""
    NormNuclearBridge{T}

The `NormNuclearCone` is representable with an SDP constraint and extra variables,
since ``t \\ge \\sum_i \\sigma_i (X)`` if and only if there exists symmetric
matrices ``U, V`` such that ``[U X^\\top; X V] \\succ 0`` and ``t \\ge (tr(U) + tr(V)) / 2``.
"""
struct NormNuclearBridge{T,F,G,H} <: AbstractBridge
    row_dim::Int # row dimension of X
    column_dim::Int # column dimension of X
    U::Vector{MOI.VariableIndex}
    V::Vector{MOI.VariableIndex}
    ge_index::CI{F,MOI.GreaterThan{T}}
    psd_index::CI{G,MOI.PositiveSemidefiniteConeTriangle}
end

function bridge_constraint(
    ::Type{NormNuclearBridge{T,F,G,H}},
    model::MOI.ModelLike,
    f::H,
    s::MOI.NormNuclearCone,
) where {T,F,G,H}
    f_scalars = MOIU.eachscalar(f)
    row_dim = s.row_dim
    column_dim = s.column_dim
    side_dim = row_dim + column_dim
    U_dim = div(column_dim * (column_dim + 1), 2)
    V_dim = div(row_dim * (row_dim + 1), 2)
    U = MOI.add_variables(model, U_dim)
    V = MOI.add_variables(model, V_dim)
    diag_vars = vcat(
        [U[trimap(i, i)] for i in 1:column_dim],
        [V[trimap(i, i)] for i in 1:row_dim],
    )
    rhs = MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.(one(T), diag_vars),
        zero(T),
    )
    ge_index = MOIU.normalize_and_add_constraint(
        model,
        MOIU.operate(-, T, f_scalars[1], MOIU.operate!(/, T, rhs, T(2))),
        MOI.GreaterThan(zero(T)),
        allow_modify_function = true,
    )
    psd_set = MOI.PositiveSemidefiniteConeTriangle(side_dim)
    psd_func = MOI.VectorOfVariables(U)
    nuc_dim = 1 + row_dim * column_dim
    for i in 1:row_dim
        row_i = (1+i):row_dim:nuc_dim
        psd_func = MOIU.operate(vcat, T, psd_func, f_scalars[row_i])
        psd_func = MOIU.operate(
            vcat,
            T,
            psd_func,
            MOI.VectorOfVariables(V[[trimap(i, j) for j in 1:i]]),
        )
    end
    psd_index = MOI.add_constraint(model, psd_func, psd_set)
    return NormNuclearBridge{T,F,G,H}(
        row_dim,
        column_dim,
        U,
        V,
        ge_index,
        psd_index,
    )
end

function MOI.supports_constraint(
    ::Type{NormNuclearBridge{T}},
    ::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormNuclearCone},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(::Type{<:NormNuclearBridge})
    return Tuple{Type}[]
end

function MOIB.added_constraint_types(
    ::Type{NormNuclearBridge{T,F,G,H}},
) where {T,F,G,H}
    return Tuple{Type,Type}[
        (F, MOI.GreaterThan{T}),
        (G, MOI.PositiveSemidefiniteConeTriangle),
    ]
end

function concrete_bridge_type(
    ::Type{<:NormNuclearBridge{T}},
    H::Type{<:MOI.AbstractVectorFunction},
    ::Type{MOI.NormNuclearCone},
) where {T}
    S = MOIU.scalar_type(H)
    F = MOIU.promote_operation(
        -,
        T,
        S,
        MOIU.promote_operation(/, T, MOIU.promote_operation(+, T, T, T), T),
    )
    G = MOIU.promote_operation(vcat, T, MOI.VectorOfVariables, H)
    return NormNuclearBridge{T,F,G,H}
end

# Attributes, Bridge acting as a model
function MOI.get(bridge::NormNuclearBridge, ::MOI.NumberOfVariables)::Int64
    return length(bridge.U) + length(bridge.V)
end

function MOI.get(bridge::NormNuclearBridge, ::MOI.ListOfVariableIndices)
    return vcat(bridge.U, bridge.V)
end

function MOI.get(
    ::NormNuclearBridge{T,F,G,H},
    ::MOI.NumberOfConstraints{F,MOI.GreaterThan{T}},
)::Int64 where {T,F,G,H}
    return 1
end

function MOI.get(
    ::NormNuclearBridge{T,F,G,H},
    ::MOI.NumberOfConstraints{G,MOI.PositiveSemidefiniteConeTriangle},
)::Int64 where {T,F,G,H}
    return 1
end

function MOI.get(
    bridge::NormNuclearBridge{T,F,G,H},
    ::MOI.ListOfConstraintIndices{F,MOI.GreaterThan{T}},
) where {T,F,G,H}
    return [bridge.ge_index]
end

function MOI.get(
    bridge::NormNuclearBridge{T,F,G,H},
    ::MOI.ListOfConstraintIndices{G,MOI.PositiveSemidefiniteConeTriangle},
) where {T,F,G,H}
    return [bridge.psd_index]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::NormNuclearBridge)
    MOI.delete(model, bridge.ge_index)
    MOI.delete(model, bridge.psd_index)
    MOI.delete(model, bridge.U)
    MOI.delete(model, bridge.V)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintFunction,
    bridge::NormNuclearBridge{T,F,G,H},
) where {T,F,G,H}
    ge_func = MOI.get(model, MOI.ConstraintFunction(), bridge.ge_index)
    psd_func = MOIU.eachscalar(
        MOI.get(model, MOI.ConstraintFunction(), bridge.psd_index),
    )
    column_dim = bridge.column_dim
    side_dim = bridge.row_dim + column_dim
    t = MOIU.operate(
        +,
        T,
        ge_func,
        MOIU.operate(
            /,
            T,
            MOIU.operate(+, T, [psd_func[trimap(i, i)] for i in 1:side_dim]...),
            T(2),
        ),
    )
    t = MOIU.remove_variable(MOIU.remove_variable(t, bridge.U), bridge.V)
    X = psd_func[[
        trimap(i, j) for j in 1:bridge.column_dim for
        i in (bridge.column_dim+1):side_dim
    ]]
    return MOIU.convert_approx(H, MOIU.operate(vcat, T, t, X))
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintSet,
    bridge::NormNuclearBridge,
)
    return MOI.NormNuclearCone(bridge.row_dim, bridge.column_dim)
end

function MOI.supports(
    ::MOI.ModelLike,
    ::MOI.ConstraintDualStart,
    ::Type{<:NormNuclearBridge},
)
    return true
end

function MOI.get(
    model::MOI.ModelLike,
    ::MOI.ConstraintPrimal,
    bridge::NormNuclearBridge,
)
    ge_primal = MOI.get(model, MOI.ConstraintPrimal(), bridge.ge_index)
    psd_primal = MOI.get(model, MOI.ConstraintPrimal(), bridge.psd_index)
    side_dim = bridge.row_dim + bridge.column_dim
    t = ge_primal + sum(psd_primal[trimap(i, i)] for i in 1:side_dim) / 2
    X = psd_primal[[
        trimap(i, j) for j in 1:bridge.column_dim for
        i in (bridge.column_dim+1):side_dim
    ]]
    return vcat(t, X)
end
# Given t is dual on GreaterThan constraint and [U X'; X V] is dual on PSD constraint,
# the dual on NormNuclearCone constraint is (t, 2X) in NormNuclearCone.
function MOI.get(
    model::MOI.ModelLike,
    attr::Union{MOI.ConstraintDual,MOI.ConstraintDualStart},
    bridge::NormNuclearBridge,
)
    t = MOI.get(model, attr, bridge.ge_index)
    psd_dual = MOI.get(model, attr, bridge.psd_index)
    side_dim = bridge.row_dim + bridge.column_dim
    X =
        2 * psd_dual[[
            trimap(i, j) for j in 1:bridge.column_dim for
            i in (bridge.column_dim+1):side_dim
        ]]
    return vcat(t, X)
end

function MOI.set(
    model::MOI.ModelLike,
    ::MOI.ConstraintDualStart,
    bridge::NormNuclearBridge{T},
    value,
) where {T}
    MOI.set(model, MOI.ConstraintDualStart(), bridge.ge_index, value[1])
    column_dim = bridge.column_dim
    side_dim = bridge.row_dim + column_dim
    dual = zeros(T, div(side_dim * (side_dim + 1), 2))
    for i in 1:side_dim
        dual[trimap(i, i)] = value[1]
    end
    X_idx = 2
    for j in 1:column_dim, i in (column_dim+1):side_dim
        dual[trimap(i, j)] = value[X_idx] / 2
        X_idx += 1
    end
    MOI.set(model, MOI.ConstraintDualStart(), bridge.psd_index, dual)
    return
end
