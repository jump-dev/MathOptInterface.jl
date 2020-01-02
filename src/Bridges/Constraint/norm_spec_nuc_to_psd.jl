"""
    NormSpectralBridge{T}

The `NormSpectralCone` is representable with a PSD constraint, since
``t \\ge \\sigma_1(X)`` if and only if ``[tI X; X^\\top tI] \\succ 0``.
"""
struct NormSpectralBridge{T, F, G} <: AbstractBridge
    psd::CI{F, MOI.PositiveSemidefiniteConeTriangle}
    row_dim::Int # row dimension of X
    column_dim::Int # column dimension of X
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
    psd = MOI.add_constraint(model, psd_func, psd_set)
    return NormSpectralBridge{T, F, G}(psd, row_dim, column_dim)
end

MOI.supports_constraint(::Type{NormSpectralBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormSpectralCone}) where T = true
MOIB.added_constrained_variable_types(::Type{<:NormSpectralBridge}) = Tuple{DataType}[]
MOIB.added_constraint_types(::Type{NormSpectralBridge{T, F, G}}) where {T, F, G} = [(F, MOI.PositiveSemidefiniteConeTriangle)]
function concrete_bridge_type(::Type{<:NormSpectralBridge{T}}, G::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormSpectralCone}) where T
    S = MOIU.scalar_type(G)
    F = MOIU.promote_operation(vcat, T, S, T)
    return NormSpectralBridge{T, F, G}
end

# Attributes, Bridge acting as a model
MOI.get(b::NormSpectralBridge{T, F, G}, ::MOI.NumberOfConstraints{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F, G} = 1
MOI.get(b::NormSpectralBridge{T, F, G}, ::MOI.ListOfConstraintIndices{F, MOI.PositiveSemidefiniteConeTriangle}) where {T, F, G} = [b.psd]

# References
MOI.delete(model::MOI.ModelLike, c::NormSpectralBridge) = MOI.delete(model, c.psd)

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::NormSpectralBridge{T, F, G}) where {T, F, G}
    psd_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.psd))
    # TODO is it OK to just take the t value from first diagonal index?
    t = psd_func[1]
    side_dim = c.row_dim + c.column_dim
    X = psd_func[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    # TODO need the convert_approx?
    return MOIU.convert_approx(G, MOIU.operate(vcat, T, t, X))
end
MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::NormSpectralBridge) = MOI.NormSpectralCone(c.row_dim, c.column_dim)
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormSpectralBridge)
    primal = MOI.get(model, MOI.ConstraintPrimal(), c.psd)
    t = primal[1]
    side_dim = c.row_dim + c.column_dim
    X = primal[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return vcat(t, X)
end
# Given [U X; X' V] is dual on PSD constraint, the dual on NormSpectralCone
# constraint is (tr(U) + tr(V), 2X) in NormNuclearCone.
function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormSpectralBridge)
    dual = MOI.get(model, MOI.ConstraintDual(), c.psd)
    side_dim = c.row_dim + c.column_dim
    t = sum(dual[trimap(i, i)] for i in 1:side_dim)
    X = 2 * dual[[trimap(i, j) for j in 1:c.column_dim for i in (c.column_dim + 1):side_dim]]
    return vcat(t, X)
end




#
# """
#     NormNuclearBridge{T}
#
# The `NormNuclearCone` is representable with an SDP constraint and extra variables,
# since ``t \\ge \\sum_i \\sigma_i (X) `` if and only if there exists symmetric
# matrices ``U, V`` such that ``[U X; X' V] \\succ 0`` and ``t = (tr(U) + tr(V)) / 2``.
# """
# struct NormNuclearBridge{T, F, G, H} <: AbstractBridge
#
# end
# function bridge_constraint(::Type{NormNuclearBridge{T, F, G, H}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormNuclearCone) where {T, F, G, H}
#
# end
#
# function concrete_bridge_type(::Type{<:NormNuclearBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormNuclearCone}) where T
#
# end



# struct NormOneBridge{T, F, G, H} <: AbstractBridge
#     y::Vector{MOI.VariableIndex}
#     ge_index::CI{F, MOI.GreaterThan{T}}
#     nn_index::CI{G, MOI.Nonnegatives}
# end
# function bridge_constraint(::Type{NormOneBridge{T, F, G, H}}, model::MOI.ModelLike, f::MOI.AbstractVectorFunction, s::MOI.NormOneCone) where {T, F, G, H}
#     f_scalars = MOIU.eachscalar(f)
#     d = MOI.dimension(s)
#     y = MOI.add_variables(model, d - 1)
#     ge_index = MOIU.normalize_and_add_constraint(model, MOIU.operate(-, T, f_scalars[1], MOIU.operate(sum, T, y)), MOI.GreaterThan(zero(T)), allow_modify_function=true)
#     lb = f_scalars[2:d]
#     ub = MOIU.operate(-, T, lb)
#     lb = MOIU.operate!(+, T, lb, MOI.VectorOfVariables(y))
#     ub = MOIU.operate!(+, T, ub, MOI.VectorOfVariables(y))
#     f_new = MOIU.operate(vcat, T, ub, lb)
#     nn_index = MOI.add_constraint(model, f_new, MOI.Nonnegatives(2d - 2))
#     return NormOneBridge{T, F, G, H}(y, ge_index, nn_index)
# end
#
# MOI.supports_constraint(::Type{NormOneBridge{T}}, ::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormOneCone}) where T = true
# MOIB.added_constrained_variable_types(::Type{<:NormOneBridge}) = Tuple{DataType}[]
# MOIB.added_constraint_types(::Type{NormOneBridge{T, F, G, H}}) where {T, F, G, H} = [(F, MOI.GreaterThan{T}), (G, MOI.Nonnegatives)]
# function concrete_bridge_type(::Type{<:NormOneBridge{T}}, H::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormOneCone}) where T
#     S = MOIU.scalar_type(H)
#     F = MOIU.promote_operation(+, T, S, S)
#     G = MOIU.promote_operation(+, T, H, H)
#     return NormOneBridge{T, F, G, H}
# end
#
# # Attributes, Bridge acting as a model
# MOI.get(b::NormOneBridge, ::MOI.NumberOfVariables) = length(b.y)
# MOI.get(b::NormOneBridge, ::MOI.ListOfVariableIndices) = b.y
# MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.NumberOfConstraints{F, MOI.GreaterThan{T}}) where {T, F, G, H} = 1
# MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.NumberOfConstraints{G, MOI.Nonnegatives}) where {T, F, G, H} = 1
# MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{F, MOI.GreaterThan{T}}) where {T, F, G, H} = [b.ge_index]
# MOI.get(b::NormOneBridge{T, F, G, H}, ::MOI.ListOfConstraintIndices{G, MOI.Nonnegatives}) where {T, F, G, H} = [b.nn_index]
#
# # References
# function MOI.delete(model::MOI.ModelLike, c::NormOneBridge)
#     MOI.delete(model, c.nn_index)
#     MOI.delete(model, c.ge_index)
#     MOI.delete(model, c.y)
# end
#
# # Attributes, Bridge acting as a constraint
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::NormOneBridge{T, F, G, H}) where {T, F, G, H}
#     ge_func = MOI.get(model, MOI.ConstraintFunction(), c.ge_index)
#     nn_func = MOIU.eachscalar(MOI.get(model, MOI.ConstraintFunction(), c.nn_index))
#     t = MOIU.operate!(+, T, ge_func, MOIU.operate!(/, T, sum(nn_func), T(2)))
#     d = div(length(nn_func), 2)
#     x = MOIU.operate!(/, T, MOIU.operate!(-, T, nn_func[(d + 1):end], nn_func[1:d]), T(2))
#     return MOIU.convert_approx(H, MOIU.remove_variable(MOIU.operate(vcat, T, t, x), c.y))
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::NormOneBridge)
#     dim = 1 + div(MOI.dimension(MOI.get(model, MOI.ConstraintSet(), c.nn_index)), 2)
#     return MOI.NormOneCone(dim)
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormOneBridge)
#     ge_primal = MOI.get(model, MOI.ConstraintPrimal(), c.ge_index)
#     nn_primal = MOI.get(model, MOI.ConstraintPrimal(), c.nn_index)
#     t = ge_primal + sum(nn_primal) / 2
#     d = length(c.y)
#     x = (nn_primal[(d + 1):end] - nn_primal[1:d]) / 2
#     return vcat(t, x)
# end
# # Given a_i is dual on y_i - x_i >= 0 and b_i is dual on y_i + x_i >= 0 and c is dual on t - sum(y) >= 0,
# # the dual on (t, x) in NormOneCone is (u, v) in NormInfinityCone, where
# # v_i = -a_i + b_i and u = c.
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormOneBridge)
#     t = MOI.get(model, MOI.ConstraintDual(), c.ge_index)
#     nn_dual = MOI.get(model, MOI.ConstraintDual(), c.nn_index)
#     d = div(length(nn_dual), 2)
#     x = (nn_dual[(d + 1):end] - nn_dual[1:d])
#     return vcat(t, x)
# end
