
function infeasible_constraints(model::MOI.ModelLike, x::Dict{MOI.VariableIndex, Float64}, tol::Float64 = eps(Float64))

    varvals = t->x[t]
    violated_constraints = Vector{Tuple{MOI.AbstractFunction, MOI.AbstractSet}}()

    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        for indx in MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
            constraint_function = MOI.get(model, MOI.ConstraintFunction(), indx)
            constraint_set = MOI.get(model, MOI.ConstraintSet(), indx)
            if distance_to_set(eval_variables(varvals, constraint_function), constraint_set) >= tol
                push!(violated_constraints, (constraint_function, constraint_set))
            end
        end
    end

    return violated_constraints
end
