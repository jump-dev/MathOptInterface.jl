"""
    NormSpectralBridge{T}

The `NormSpectralCone` is representable with a PSD constraint, since
``t \\ge \\sigma_1(X)`` if and only if ``[tI X; X' tI] \\succ 0``.
"""
# struct NormSpectralBridge{T, F, G} <: SetMapBridge{T, MOI.PositiveSemidefiniteConeTriangle, MOI.NormSpectralCone, F, G}
#     constraint::CI{F, MOI.PositiveSemidefiniteConeTriangle}
# end
# function concrete_bridge_type(::Type{<:NormSpectralBridge{T}}, G::Type{<:MOI.AbstractVectorFunction}, ::Type{MOI.NormSpectralCone}) where T
#
# end
#
# function map_set(::Type{<:NormSpectralBridge}, set::MOI.NormSpectralCone)
#     return MOI.PositiveSemidefiniteConeTriangle(set.row_dim + set.column_dim)
# end
# function inverse_map_set(::Type{<:NormSpectralBridge}, set::MOI.PositiveSemidefiniteConeTriangle)
#     return MOI.NormSpectralCone(?, ?)
# end
#
# function map_function(::Type{<:NormSpectralBridge{T}}, func) where T
#
# end
# function inverse_map_function(::Type{<:NormSpectralBridge{T}}, func) where T
#
# end

"""
    NormNuclearBridge{T}

The `NormNuclearCone` is representable with an SDP constraint and extra variables,
since ``t \\ge \\sum_i \\sigma_i (X) `` if and only if there exists symmetric
matrices ``U, V`` such that ``[U X; X' V] \\succ 0`` and ``t = (tr(U) + tr(V)) / 2``.
"""
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
#
# # References
# function MOI.delete(model::MOI.ModelLike, c::NormNuclearBridge)
#
# end
#
# # Attributes, Bridge acting as a constraint
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintFunction, c::NormNuclearBridge{T, F, G, H}) where {T, F, G, H}
#
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintSet, c::NormNuclearBridge)
#
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintPrimal, c::NormNuclearBridge)
#
# end
# function MOI.get(model::MOI.ModelLike, ::MOI.ConstraintDual, c::NormNuclearBridge)
#
# end
