# DummyModel{true} implements add_variable and add_constraint
struct DummyModel{A} <: MOI.ModelLike
end
MOI.add_variable(::DummyModel{true}) = MOI.VariableIndex(0)
function MOI.empty!(::DummyModel) end
function MOI.copy_to(dest::DummyModel, src::MOI.ModelLike; copy_names=true)
    return MOIU.default_copy_to(dest, src, copy_names)
end
MOI.supports(::DummyModel, ::MOI.ObjectiveSense) = true
MOI.supports(::DummyModel, ::MOI.ConstraintPrimalStart,
             ::Type{<:MOI.ConstraintIndex}) = true
MOI.supports_constraint(::DummyModel, ::Type{MOI.SingleVariable},
                       ::Type{MOI.EqualTo{Float64}}) = true
function MOI.add_constraint(::DummyModel{true}, ::MOI.SingleVariable,
                            ::MOI.EqualTo{Float64})
    return MOI.ConstraintIndex{MOI.SingleVariable, MOI.EqualTo{Float64}}(0)
end
MOI.supports_constraint(::DummyModel, ::Type{MOI.VectorOfVariables},
                       ::Type{MOI.Zeros}) = true
