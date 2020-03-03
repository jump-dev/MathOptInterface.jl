function f!(F, dual_coff, obj_term_coffs, x)
        for i in 1:length(dual_coff)
            F[i] = sum([dual_coff[i][j] * x[j] for j in 1:length(dual_coff[1])]) - obj_term_coffs[i]
        end
    end


function Optimality_Checker(obj::MOI.ScalarAffineFunction, opt::Float64, sense::MOI.OptimizationSense, constraints::Vector{Tuple{F, S}}) where{F, S}
    opt = opt - obj.constant

    dual_coff = Array{Float64,1}[]
    obj_term_coffs = Float64[]

    if sense == MOI.MAX_SENSE
        for constraint in constraints
            if typeof(constraint[2]) == MOI.GreaterThan
                for term in constraint.terms
                    term.coefficient = -1 * term.coefficient
                end
                constraint[2] = MOI.LessThan(constraint[2].lower)
            end
        end
    end

    if sense == MOI.MIN_SENSE
        for constraint in constraints
            if typeof(constraint[2]) == MOI.LessThan
                for term in constraint.terms
                    term.coefficient = -1 * term.coefficient
                end
                constraint[2] = MOI.GreaterThan(constraint[2].upper)
            end
        end
    end


    for obj_term in obj.terms
        vi = obj_term.variable_index.value
        term_coff = obj_term.coefficient
        append!(obj_term_coffs, term_coff)
        temp =  zeros(length(constraints))

        for j in 1:length(constraints)
            for constraint_term in constraints[j][1].terms
                if vi == constraint_term.variable_index.value
                    temp[j] = constraint_term.coefficient
                end
            end
        end
        push!(dual_coff, temp)
    end

    temp = Float64[]
    for constraint in constraints
        if sense == MOI.MAX_SENSE
            append!(temp, constraint[2].upper)
        elseif sense == MOI.MIN_SENSE
            append!(temp, constraint[2].lower)
        end
    end

    push!(dual_coff, temp)
    append!(obj_term_coffs, opt)

    initial_x = ones(length(constraints))
    res = nlsolve((F,x)->f!(F, dual_coff, obj_term_coffs, x), initial_x)

    if all(res.zero.>0)
        print("Optimal Solution")
    else
        print("Not an Optimal Solution")
    end
    return
end
