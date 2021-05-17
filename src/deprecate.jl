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

function Base.getproperty(f::ScalarAffineTerm, key::Symbol)
    if key == :variable_index
        @warn("`.variable_index` is deprecated in favor of `.variable`.")
        return getfield(f, :variable)
    end
    return getfield(f, key)
end

function Base.getproperty(f::ScalarQuadraticTerm, key::Symbol)
    if key == :variable_index_1
        @warn("`.variable_index_1` is deprecated in favor of `.variable_1`.")
        return getfield(f, :variable_1)
    elseif key == :variable_index_2
        @warn("`.variable_index_2` is deprecated in favor of `.variable_2`.")
        return getfield(f, :variable_2)
    end
    return getfield(f, key)
end

function RawParameter(name::Any)
    @warn(
        "RawParameter(::$(typeof(name)) is deprecated. Use " *
        "`RawOptimizerAttribute(::String)` instead.",
    )
    return RawOptimizerAttribute(string(name))
end
