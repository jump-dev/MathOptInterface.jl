function eval_function(var::MOI.SingleVariable, x)
    return x[var.variable.value]
end

function eval_function(aff::MOI.ScalarAffineFunction, x)
    function_value = aff.constant
    for term in aff.terms
        function_value += term.coefficient*x[term.variable_index.value]
    end
    return function_value
end

function eval_function(quad::MOI.ScalarQuadraticFunction, x)
    function_value = quad.constant
    for term in quad.affine_terms
        function_value += term.coefficient*x[term.variable_index.value]
    end
    for term in quad.quadratic_terms
        row_idx = term.variable_index_1
        col_idx = term.variable_index_2
        coefficient = term.coefficient
        if row_idx == col_idx
            function_value += 0.5*coefficient*x[row_idx.value]*x[col_idx.value]
        else
            function_value += coefficient*x[row_idx.value]*x[col_idx.value]
        end
    end
    return function_value
end

function parse_constraint(ex::Expr)
    expr =
    @match ex begin
       ((a_ <= b_) | (a_ < b_) | (a_ ≤ b_)) => (a)
       ((a_ >= b_) | (a_ > b_) | (a_ ≥ b_)) => (a)
       ((a_ == b_) | (a_ = b_))   => (a)
       ((a_ <= b_ <= c_) | (a_ < b_ < c_) | (a_ <= b_ < c) | (a_ < b_ <= c)) => (b)
       ((a_ >= b_ >= c_) | (a_ > b_ > c_) | (a_ >= b_ > c_) | (a_ > b_ >= c)) => (b)
       ((a_ ∈ [b_, c_]) | (a_ in [b_, c_]) | (a_ ∈ b_ .. c_) | (a_ in b_ .. c_))  => (a)
   end
   return (expr)
end

function substitute_variables(expr::Expr)
    if expr.head == :ref && length(expr.args) == 2 && expr.args[1] == :x
        return :(x[$(expr.args[2].value)])
    else
        for (index, arg) in enumerate(expr.args)
            expr.args[index] = substitute_variables(arg)
        end
    end
    return expr
end

function eval_function(expr::Expr)    # For Non linear constraints expressions 
    expr = parse_constraint(expr)
    expr = substitute_variables(expr)
    eval_expr = eval(:(x -> $(expr)))
    return invokelatest(eval_expr, x)
end

function eval_variables(x, functions)
    return [eval_function(func, x) for func in functions]
end
