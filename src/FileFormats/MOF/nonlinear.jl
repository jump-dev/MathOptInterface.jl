# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# Overload for writing.
function moi_to_object(foo::Nonlinear, name_map::Dict{MOI.VariableIndex,String})
    node_list = OrderedObject[]
    foo_object = convert_expr_to_mof(foo.expr, node_list, name_map)
    return OrderedObject(
        "type" => "ScalarNonlinearFunction",
        "root" => foo_object,
        "node_list" => node_list,
    )
end

function lift_variable_indices(expr::Expr)
    if expr.head == :ref && length(expr.args) == 2 && expr.args[1] == :x
        return expr.args[2]
    else
        for (index, arg) in enumerate(expr.args)
            expr.args[index] = lift_variable_indices(arg)
        end
    end
    return expr
end

lift_variable_indices(arg) = arg  # Recursion fallback.

function extract_function_and_set(expr::Expr)
    if expr.head == :call  # One-sided constraint or foo-in-set.
        @assert length(expr.args) == 3
        if expr.args[1] == :in
            # return expr.args[2], expr.args[3]
            error("Constraints of the form foo-in-set aren't supported yet.")
        elseif expr.args[1] == :(<=)
            return expr.args[2], MOI.LessThan(expr.args[3])
        elseif expr.args[1] == :(>=)
            return expr.args[2], MOI.GreaterThan(expr.args[3])
        elseif expr.args[1] == :(==)
            return expr.args[2], MOI.EqualTo(expr.args[3])
        end
    elseif expr.head == :comparison  # Two-sided constraint.
        @assert length(expr.args) == 5
        if expr.args[2] == expr.args[4] == :(<=)
            return expr.args[3], MOI.Interval(expr.args[1], expr.args[5])
        elseif expr.args[2] == expr.args[4] == :(>=)
            return expr.args[3], MOI.Interval(expr.args[5], expr.args[1])
        end
    end
    return error("Oops. The constraint $(expr) wasn't recognised.")
end

function write_nlpblock(
    object::T,
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    nlp_block = MOI.get(model, MOI.NLPBlock())
    if nlp_block === nothing
        return
    end
    MOI.initialize(nlp_block.evaluator, [:ExprGraph])
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    if nlp_block.has_objective
        objective = MOI.objective_expr(nlp_block.evaluator)
        objective = lift_variable_indices(objective)
        sense = MOI.get(model, MOI.ObjectiveSense())
        object["objective"] = T(
            "sense" => moi_to_object(sense),
            "function" => moi_to_object(Nonlinear(objective), name_map),
        )
    end
    for (row, bounds) in enumerate(nlp_block.constraint_bounds)
        constraint = MOI.constraint_expr(nlp_block.evaluator, row)
        (func, set) = extract_function_and_set(constraint)
        func = lift_variable_indices(func)
        push!(
            object["constraints"],
            T(
                "function" => moi_to_object(Nonlinear(func), name_map),
                "set" => moi_to_object(set, name_map),
            ),
        )
    end
end

#=
Expr:
    2 * x + sin(x)^2 + y

Tree form:
                    +-- (2)
          +-- (*) --+
          |         +-- (x)
    (+) --+
          |         +-- (sin) --+-- (x)
          +-- (^) --+
          |         +-- (2)
          +-- (y)

MOF format:

    {
        "type": "nonlinear",
        "root": {"type": "node", "index": 4},
        "node_list": [
            {
                "type": "*", "args": [
                    {"type": "real", "value": 2},
                    {"type": "variable", "name": "x"}
                ]
            },
            {
                "type": "sin",
                "args": [
                    {"type": "variable", "name", "x"}
                ]
            }
            {
                "type": "^",
                "args": [
                    {"type": "node", "index": 2},
                    {"type": "real", "value": 2}
                ]
            },
            {
                "type": "+",
                "args": [
                    {"type": "node", "index": 1},
                    {"type": "node", "index": 3},
                    {"type": "variable", "name": "y"}
                ]
            }
        ]
    }
=#

"""
    convert_expr_to_mof(
        expr,
        node_list::Vector{T},
        name_map::Dict{MOI.VariableIndex, String}
    )

Convert a Julia expression into a MathOptFormat representation. Any intermediate
nodes that are required are appended to `node_list`. Variable indices are mapped
through `name_map` to their string name.
"""
function convert_expr_to_mof(
    expr::Expr,
    node_list::Vector{T},
    name_map::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    if expr.head != :call
        error("Expected an expression that was a function. Got $(expr).")
    end
    node = T("type" => string(expr.args[1]), "args" => T[])
    for i in 2:length(expr.args)
        arg = expr.args[i]
        push!(node["args"], convert_expr_to_mof(arg, node_list, name_map))
    end
    push!(node_list, node)
    return T("type" => "node", "index" => length(node_list))
end

function convert_expr_to_mof(
    f::MOI.ScalarNonlinearFunction,
    node_list::Vector{T},
    name_map::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    node = T("type" => string(f.head), "args" => T[])
    for arg in f.args
        push!(node["args"], convert_expr_to_mof(arg, node_list, name_map))
    end
    push!(node_list, node)
    return T("type" => "node", "index" => length(node_list))
end

# Recursion end for variables.
function convert_expr_to_mof(
    variable::MOI.VariableIndex,
    ::Vector{T},
    name_map::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    return T("type" => "variable", "name" => name_map[variable])
end

# Recursion end for real constants.
function convert_expr_to_mof(
    value::Real,
    ::Vector{T},
    name_map::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    return T("type" => "real", "value" => value)
end

# Recursion end for complex numbers.
function convert_expr_to_mof(
    value::Complex,
    ::Vector{T},
    ::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    return T("type" => "complex", "real" => real(value), "imag" => imag(value))
end

# Recursion fallback.
function convert_expr_to_mof(
    fallback,
    ::Vector{<:Object},
    ::Dict{MOI.VariableIndex,String},
)
    return error("Unexpected $(typeof(fallback)) encountered: $(fallback).")
end
