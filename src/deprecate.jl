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

function ScalarQuadraticFunction(
    affine_terms::Vector{<:ScalarAffineTerm{T}},
    quadratic_terms::Vector{<:ScalarQuadraticTerm{T}},
    constant::T,
) where {T}
    @warn("Fields of ScalarQuadraticFunction have been re-ordered.", maxlog = 1)
    return ScalarQuadraticFunction(quadratic_terms, affine_terms, constant)
end

function VectorQuadraticFunction(
    affine_terms::Vector{<:VectorAffineTerm{T}},
    quadratic_terms::Vector{<:VectorQuadraticTerm{T}},
    constant::Vector{T},
) where {T}
    @warn("Fields of VectorQuadraticFunction have been re-ordered.", maxlog = 1)
    return VectorQuadraticFunction(quadratic_terms, affine_terms, constant)
end

function Utilities.CleverDicts.CleverDict{K,V}(::Base.Integer) where {K,V}
    @warn("The `n` argument to `CleverDict` has been removed.")
    return Utilities.CleverDicts.CleverDict{K,V}()
end

function Utilities.IndexMap(::Int)
    @warn("The number_of_variables argument to `IndexMap` has been removed.")
    return Utilities.IndexMap()
end
