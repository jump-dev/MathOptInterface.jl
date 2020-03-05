function generate_varval(x)
    return  eval(quote
        t -> begin
            return $x[t.value]
            end
        end)
end

function return_infeasible_constraints(x::Vector{Float64}, constraints::Vector{Tuple{F, S}}, tol::Float64 = eps(Float64)) where{F, S}
    varval = generate_varval(x)

    dist_to_sets = [distance_to_set(invokelatest(eval_variables, varval, f), s) for (f, s) in constraints]
    
    infeasible_constraints = Vector{Tuple{F, S}}()

    for i in 1:length(dist_to_sets)
        if dist_to_sets[i] > tol
            push!(feasible_constraints, constraints[i])
        end
    end
    return infeasible_constraints
end
