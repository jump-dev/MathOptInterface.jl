
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


function return_dualprimalgap(model::MOI.ModelLike, x_primal::Dict{MOI.VariableIndex, Float64}, y_dual::Dict{MOI.VariableIndex, Float64}; tol::Float64 = eps(Float64))

    primal_objective = MOI.get(optimizer, MOI.ObjectiveFunction{MathOptInterface.ScalarAffineFunction{Float64}}())

    dual_model = dualize(model)
    dual_objective = MOI.get(dual_model.dual_model, MOI.ObjectiveFunction{MathOptInterface.ScalarAffineFunction{Float64}}())

    violated_primal_constraints = infeasible_constraints(model, x_primal, tol = tol)
    violated_dual_constraints =  infeasible_cosntraints(dual_model.dual_model, y_dual, tol = tol)

    if isempty(violated_primal_constraints) && isempty(violated_dual_constraints)       # true if test solutions for both dual and primal are feasible
        return eval_variables(t->x_primal[t], primal_objective) - eval_variables(t->y_dual[t], dual_objective)   # p* - d*
    elseif !isempty(violated_primal_constraints) && !isempty(violated_dual_constraints)
        println("Given test solutions for primal and dual is not feasible")
    elseif !isempty(violated_primal_constraints)
        println("Given test solution for primal is not feasible")
    else
        println("Given test solution for dual is not feasible")
    end

end
