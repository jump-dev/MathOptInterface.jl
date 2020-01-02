"""
    NormSpectralBridge{T}

The `NormSpectralCone` is representable with a PSD constraint, since
``t \\ge \\sigma_1(X)`` if and only if ``[tI X; X^\\top tI] \\succ 0``.
"""
struct NormSpectralBridge{T, F, G} <: AbstractBridge
    row_dim::Int # row dimension of X
    column_dim::Int # column dimension of X
    psd_index::CI{F, MOI.PositiveSemidefiniteConeTriangle}
end
function bridge_constraint(::Type{NormSpectralBridge{T, F, G}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormSpectralCone) where {T, F, G}
    f_scalars = MOIU.eachscalar(f)
    t = f_scalars[1]
    row_dim = s.row_dim
    column_dim = s.column_dim
    @assert row_dim <= column_dim # TODO informative error if not
    side_dim = row_dim + column_dim
    psd_set = MOI.PositiveSemidefiniteConeTriangle(side_dim)
    psd_func = MOIU.zero_with_output_dimension(F, MOI.dimension(psd_set))
    for i in 1:side_dim
        MOIU.operate_output_index!(+, T, trimap(i, i), psd_func, t)
    end
    X_idx = 2
    for j in 1:column_dim, i in (column_dim + 1):side_dim
        MOIU.operate_output_index!(+, T, trimap(i, j), psd_func, f_scalars[X_idx])
        X_idx += 1
    end
    psd_index = MOI.add_constraint(model, psd_func, psd_set)
    return NormSpectralBridge{T, F, G}(row_dim, column_dim, psd_index)
end

MOI.supports_constraint(::Type{NormSpectralBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormSpectralCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormSpectralBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormSpectralBridge{T, F, G}}) where {T, F, G} = [(F, MOI.PositiveSemidefiniteConeTriangle)]
function concrete_bridge_type(::Type{<:NormSpectralBridge{T}}, G::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormSpectralCone}) where T
    F = MOIU.promote_operation(vcat, T, MOIU.scalar_type(G), T)
    return NormSpectralBridge{T, F, G}
end

# Attributes, Bridge acting as a model
MOI.get(b::NormSpectralBridge{T, F, G}, ::MOI.NumberOfConstraints{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F, G} = 1
MOI.get(b::NormSpectralBridge{T, F, G}, ::MOI.ListOfConstraintIndices{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F, G} = [b.psd_index]

# References
MOI.delete(model::MOI.ModelLike, c::NormSpectralBridge) = MOI.delete(model, c.psd_index)

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::NormSpectralBridge{T, F, G}) where {T, F, G}
    psd_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.psd_index))
    # TODO is it OK to just take the t value from first diagonal index?
    t = psd_func[1]
    side_dim = c.row_dim + c.column_dim
    X = psd_func[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    # TODO need the convert_approx?
    return MOIU.convert_approx(G, MOIU.operate(vcat, T, t, X))
end
MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::NormSpectralBridge) = MOI.NormSpectralCone(c.row_dim, c.column_dim)
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormSpectralBridge)
    primal = MOI.get(model, MOI.ConstraintPrimal(), c.psd_index)
    t = primal[1]
    side_dim = c.row_dim + c.column_dim
    X = primal[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return vcat(t, X)
end
# Given [U X; X' V] is dual on PSD constraint, the dual on NormSpectralCone
# constraint is (tr(U) + tr(V), 2X) in NormNuclearCone.
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormSpectralBridge)
    dual = MOI.get(model, MOI.ConstraintDual(), c.psd_index)
    side_dim = c.row_dim + c.column_dim
    t = sum(dual[trimap(i, i)] for i in 1:side_dim)
    X = 2 * dual[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return vcat(t, X)
end

"""
    NormNuclearBridge{T}

The `NormNuclearCone` is representable with an SDP constraint and extra variables,
since ``t \\ge \\sum_i \\sigma_i (X)`` if and only if there exists symmetric
matrices ``U, V`` such that ``[U X; X' V] \\succ 0`` and ``t \\ge (tr(U) + tr(V)) / 2``.
"""
struct NormNuclearBridge{T, F, G, H} <: AbstractBridge
    row_dim::Int # row dimension of X
    column_dim::Int # column dimension of X
    U::Vector{MOI.VariableIndex}
    V::Vector{MOI.VariableIndex}
    ge_index::CI{F, MOI.GreaterThan{T}}
    psd_index::CI{G, MOI.PositiveSemidefiniteConeTriangle}
end
function bridge_constraint(::Type{NormNuclearBridge{T, F, G, H}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormNuclearCone) where {T, F, G, H}
    f_scalars = MOIU.eachscalar(f)
    row_dim = s.row_dim
    column_dim = s.column_dim
    @assert row_dim <= column_dim # TODO informative error if not
    side_dim = row_dim + column_dim
    U_dim = div(column_dim * (column_dim + 1), 2)
    V_dim = div(row_dim * (row_dim + 1), 2)
    U = MOI.add_variables(model, U_dim)
    V = MOI.add_variables(model, V_dim)
    diag_vars = vcat([U[trimap(i, i)] for i in 1:column_dim], [V[trimap(i, i)] for i in 1:row_dim])
    ge_index = MOIU.normalize_and_add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOIU.operate!(/, T, MOIU.operate(sum, T, diag_vars), T(2))), MOI.GreaterThan(zero(T)), allow_modify_function=true)
    psd_set = MOI.PositiveSemidefiniteConeTriangle(side_dim)
    psd_func = MOI.VectorOfVariables(U)
    nuc_dim = 1 + row_dim * column_dim
    for i in 1:row_dim
        row_i = (1 + i):row_dim:nuc_dim
        psd_func = MOIU.operate(vcat, T, psd_func, f_scalars[row_i])
        psd_func = MOIU.operate(vcat, T, psd_func, MOI.VectorOfVariables(V[[trimap(i, j) for j in 1:i]]))
    end
    psd_index = MOI.add_constraint(model, psd_func, psd_set)
    return NormNuclearBridge{T, F, G, H}(row_dim, column_dim, U, V, ge_index, psd_index)
end

MOI.supports_constraint(::Type{NormNuclearBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormNuclearCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormNuclearBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormNuclearBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.GreaterThan{T}), (G, MOI.PositiveSemidefiniteConeTriangle)]
function concrete_bridge_type(::Type{<:NormNuclearBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormNuclearCone}) where T
    S = MOIU.scalar_type(H)
    F = MOIU.promote_operation(-, T, S, MOIU.promote_operation(/, T, MOIU.promote_operation(+, T, T, T), T))
    G = MOIU.promote_operation(vcat, T, MOI.VectorOfVariables, H)
    return NormNuclearBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(b::NormNuclearBridge, ::MOI.NumberOfVariables) = length(b.U) + length(b.V)
MOI.get(b::NormNuclearBridge, ::MOI.ListOfVariableIndices) = vcat(b.U, b.V)
MOI.get(b::NormNuclearBridge{T, F, G, H}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F, G, H} = 1
MOI.get(b::NormNuclearBridge{T, F, G, H}, ::MOI.NumberOfConstraints{G, MOI.PositiveSemidefiniteConeTriangle}) where {T, F, G, H} = 1
MOI.get(b::NormNuclearBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F, G, H} = [b.ge_index]
MOI.get(b::NormNuclearBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{G, MOI.PositiveSemidefiniteConeTriangle}) where {T, F, G, H} = [b.psd_index]

# References
function MOI.delete(model::MOI.ModelLike, c::NormNuclearBridge)
    MOI.delete(model, c.ge_index)
    MOI.delete(model, c.psd_index)
    MOI.delete(model, c.U)
    MOI.delete(model, c.V)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::NormNuclearBridge{T, F, G, H}) where {T, F, G, H}
    ge_func = MOI.get(model, MOI.ConstraintFunction(), c.ge_index)
    psd_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.psd_index))
    column_dim = c.column_dim
    side_dim = c.row_dim + column_dim
    t = MOIU.operate(+, T, ge_func, MOIU.operate(/, T, MOIU.operate(+, T, [psd_func[trimap(i, i)] for i in 1:side_dim]...), T(2)))
    t = MOIU.remove_variable(MOIU.remove_variable(t, c.U), c.V)
    X = psd_func[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return MOIU.convert_approx(H, MOIU.operate(vcat, T, t, X))
end
MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::NormNuclearBridge) = MOI.NormNuclearCone(c.row_dim, c.column_dim)
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormNuclearBridge)
    ge_primal = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
    psd_primal = MOI.get(model, MOI.ConstraintPrimal(), c.psd_index)
    side_dim = c.row_dim + c.column_dim
    t = ge_primal + sum(psd_primal[trimap(i, i)] for i in 1:side_dim) / 2
    X = psd_primal[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return vcat(t, X)
end
# Given t is dual on GreaterThan constraint and [U X; X' V] is dual on PSD constraint,
# the dual on NormNuclearCone constraint is (t, 2X) in NormNuclearCone.
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormNuclearBridge)
    t = MOI.get(model, MOI.ConstraintDual(), c.ge_index)
    psd_dual = MOI.get(model, MOI.ConstraintDual(), c.psd_index)
    side_dim = c.row_dim + c.column_dim
    X = 2 * psd_dual[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return vcat(t, X)
end
