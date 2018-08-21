function add_scalar_constraint(model::MOI.ModelLike, func::MOI.SingleVariable,
                               set::MOI.AbstractScalarSet)
    return MOI.addconstraint!(model, func, set)
end
function add_scalar_constraint(model::MOI.ModelLike,
                               func::Union{MOI.ScalarAffineFunction{T},
                                           MOI.ScalarQuadraticFunction{T}},
                               set::MOI.AbstractScalarSet) where T
    set = shift_constant(set, -func.constant)
    func.constant = zero(T)
    return MOI.addconstraint!(model, func, set)
end
