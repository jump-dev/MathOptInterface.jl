# deprecate _result_index_field and accessing attr.N, no export
@deprecate _result_index_field(attr) attr.result_index false

function Base.getproperty(
    attr::Attr,
    f::Symbol,
) where {
    Attr<:Union{
        ObjectiveValue,
        DualObjectiveValue,
        VariablePrimal,
        ConstraintPrimal,
        ConstraintDual,
        ConstraintBasisStatus,
        PrimalStatus,
        DualStatus,
        NLPBlockDual,
    },
}
    if f === :N
        @warn "Field attr.N is deprecated, use attr.result_index"
        return getfield(attr, :result_index)
    end
    return getfield(attr, f)
end
