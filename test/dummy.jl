struct DummyModel <: MOI.ModelLike
end
MOI.supports(::DummyModel, ::MOI.ObjectiveSense) = true
MOI.supports(::DummyModel, ::MOI.ConstraintPrimalStart,
             ::Type{<:MOI.ConstraintIndex}) = true
MOI.supports_constraint(::DummyModel, ::Type{MOI.SingleVariable},
                       ::Type{MOI.EqualTo{Float64}}) = true
MOI.supports_constraint(::DummyModel, ::Type{MOI.VectorOfVariables},
                       ::Type{MOI.Zeros}) = true
