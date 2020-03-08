
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


function infeasible_dual_constraints(model::Union{MOI.ModelLike, Dualization.DualProblem} , x::Dict{MOI.VariableIndex, Float64}; tol::Float64 = eps(Float64))

    if typeof(model) <: MOI.ModelLike
        model = dualize(model)
    end

    varvals = t->x[t]
    violated_constraints = Vector{Tuple{MOI.AbstractFunction, MOI.AbstractSet}}()

    for (ci, f, s) in model.dual_model.moi_scalaraffinefunction.moi_equalto
        if distance_to_set(eval_variables(varvals, f), s) >= tol
            push!(violated_constraints, (f, constraint_set))
        end
    end

    return violated_constraints
end

function return_dualprimalgap(model::MOI.ModelLike, x_primal::Dict{MOI.VariableIndex, Float64}, y_dual::Dict{MOI.VariableIndex, Float64}; tol::Float64 = eps(Float64))

    primal_objective = MOI.get(optimizer, MOI.ObjectiveFunction{MathOptInterface.ScalarAffineFunction{Float64}}())

    dual_model = dualize(model)
    dual_objective = dual_model.dual_model.objective

    violated_primal_constraints = infeasible_constraints(model, x_primal, tol = tol)
    violated_dual_constraints =  infeasible_dual_constraints(dual_model, y_dual, tol = tol)

    if isempty(violated_primal_constraints) && isempty(violated_dual_constraints)
        return eval_variables(t->x_primal[t], primal_objective) - eval_variables(t->y_dual[t], dual_objective)   # p* - d*
    elseif !isempty(violated_primal_constraints) && !isempty(violated_dual_constraints)
        println("Given solutions for primal and dual is not feasible")
    elseif !isempty(violated_primal_constraints)
        println("Given solution for primal is not feasible")
    else
        println("Given solution for dual is not feasible")
    end

end
