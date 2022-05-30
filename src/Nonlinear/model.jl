# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function Base.copy(::Model)
    return error("Copying nonlinear problems not yet implemented")
end

function Base.show(io::IO, model::Model)
    println(io, "A Nonlinear.Model with:")
    if model.objective !== nothing
    end
    _plural(s, n) = n == 1 ? " 1 $s" : " $n $(s)s"
    println(io, _plural("objective", model.objective !== nothing ? 1 : 0))
    println(io, _plural("parameter", length(model.parameters)))
    println(io, _plural("expression", length(model.expressions)))
    return print(io, _plural("constraint", length(model.constraints)))
end

"""
    set_objective(model::Model, obj)::Nothing

Parse `obj` into a [`Expression`](@ref) and set as the objective
function of `model`.

`obj` must be a type that is supported by [`parse_expression`](@ref).

To remove the objective, pass `nothing`.

## Examples

```julia
model = Model()
x = MOI.VariableIndex(1)
set_objective(model, :(\$x^2 + 1))
set_objective(model, x)
set_objective(model, nothing)
```
"""
function set_objective(model::Model, obj)
    model.objective = parse_expression(model, obj)
    return
end

function set_objective(model::Model, ::Nothing)
    model.objective = nothing
    return
end

"""
    add_expression(model::Model, expr)::ExpressionIndex

Parse `expr` into a [`Expression`](@ref) and add to `model`. Returns an
[`ExpressionIndex`](@ref) that can be interpolated into other input expressions.

`expr` must be a type that is supported by [`parse_expression`](@ref).

## Examples

```julia
model = Model()
x = MOI.VariableIndex(1)
ex = add_expression(model, :(\$x^2 + 1))
set_objective(model, :(sqrt(\$ex)))
```
"""
function add_expression(model::Model, expr)
    push!(model.expressions, parse_expression(model, expr))
    return ExpressionIndex(length(model.expressions))
end

function Base.getindex(model::Model, index::ExpressionIndex)
    return model.expressions[index.value]
end

"""
    add_constraint(
        model::Model,
        func,
        set::Union{
            MOI.GreaterThan{Float64},
            MOI.LessThan{Float64},
            MOI.Interval{Float64},
            MOI.EqualTo{Float64},
        },
    )

Parse `func` and `set` into a [`Constraint`](@ref) and add to `model`. Returns a
[`ConstraintIndex`](@ref) that can be used to delete the constraint or query
solution information.

## Examples

```julia
model = Model()
x = MOI.VariableIndex(1)
c = add_constraint(model, :(\$x^2), MOI.LessThan(1.0))
```
"""
function add_constraint(
    model::Model,
    func,
    set::Union{
        MOI.GreaterThan{Float64},
        MOI.LessThan{Float64},
        MOI.Interval{Float64},
        MOI.EqualTo{Float64},
    },
)
    f = parse_expression(model, func)
    model.last_constraint_index += 1
    index = ConstraintIndex(model.last_constraint_index)
    model.constraints[index] = Constraint(f, set)
    return index
end

"""
    delete(model::Model, c::ConstraintIndex)::Nothing

Delete the constraint index `c` from `model`.

## Examples

```julia
model = Model()
x = MOI.VariableIndex(1)
c = add_constraint(model, :(\$x^2), MOI.LessThan(1.0))
delete(model, c)
```
"""
function delete(model::Model, c::ConstraintIndex)
    delete!(model.constraints, c)
    return
end

function Base.getindex(model::Model, index::ConstraintIndex)
    return model.constraints[index]
end

function MOI.is_valid(model::Model, index::ConstraintIndex)
    return haskey(model.constraints, index)
end

"""
    add_parameter(model::Model, value::Float64)::ParameterIndex

Add a new parameter to `model` with the default value `value`. Returns a
[`ParameterIndex`](@ref) that can be interpolated into other input expressions
and used to modify the value of the parameter.

## Examples

```julia
model = Model()
x = MOI.VariableIndex(1)
p = add_parameter(model, 1.2)
c = add_constraint(model, :(\$x^2 - \$p), MOI.LessThan(0.0))
```
"""
function add_parameter(model::Model, value::Float64)
    push!(model.parameters, value)
    return ParameterIndex(length(model.parameters))
end

function Base.getindex(model::Model, p::ParameterIndex)
    return model.parameters[p.value]
end

function Base.setindex!(model::Model, value::Real, p::ParameterIndex)
    return model.parameters[p.value] = convert(Float64, value)::Float64
end

"""
    register_operator(
        model::Model,
        op::Symbol,
        nargs::Int,
        f::Function,
        [∇f::Function],
        [∇²f::Function],
    )

Register the user-defined operator `op` with `nargs` input arguments in `model`.

## Univariate functions

 * `f(x::T)::T` must be a function that takes a single input argument `x` and
   returns the function evaluated at `x`. If `∇f` and `∇²f` are not provided,
   `f` must support any `Real` input type `T`.
 * `∇f(x::T)::T` is a function that takes a single input argument `x` and
   returns the first derivative of `f` with respect to `x`. If `∇²f` is not
   provided, `∇f` must support any `Real` input type `T`.
 * `∇²f(x::T)::T` is a function that takes a single input argument `x` and
   returns the second derivative of `f` with respect to `x`.

## Multivariate functions

* `f(x::T...)::T` must be a function that takes a `nargs` input arguments `x`
  and returns the function evaluated at `x`. If `∇f` and `∇²f` are not provided,
  `f` must support any `Real` input type `T`.
* `∇f(g::AbstractVector{T}, x::T...)::T` is a function that takes a cache vector
  `g` of length `length(x)`, and fills each element `g[i]` with the partial
  derivative of `f` with respect to `x[i]`.
* `∇²f(H::AbstractMatrix, x::T...)::T` is a function that takes a matrix `H` and
  fills the lower-triangular components `H[i, j]` with the Hessian of `f` with
  respect to `x[i]` and `x[j]` for `i >= j`.

### Notes for multivariate Hessians

 * `H` has `size(H) == (length(x), length(x))`, but you must not access
   elements `H[i, j]` for `i > j`.
 * `H` is dense, but you do not need to fill structural zeros.
"""
function register_operator(model::Model, op::Symbol, nargs::Int, f::Function...)
    return register_operator(model.operators, op, nargs, f...)
end

"""
    evaluate(
        f::AbstractDict,
        model::Model,
        index::ExpressionIndex,
    )

Evaluate the nonlinear expression `index`, where `f[x]` returns the primal value
of decision variable `x::MOI.VariableIndex`.
"""
function evaluate(
    f::AbstractDict,
    model::Model,
    index::ExpressionIndex;
    kwargs...,
)
    return evaluate(f, model, model[index]; kwargs...)
end

"""
    evaluate(
        f::AbstractDict,
        model::Model,
        expr::Expression,
    )

Evaluate the nonlinear expression `expr`, where `f[x]` returns the primal value
of decision variable `x::MOI.VariableIndex`.
"""
function evaluate(
    f::AbstractDict,
    model::Model,
    expr::Expression;
    evaluated_expressions = Dict{Int,Float64}(),
)
    storage = zeros(length(expr.nodes))
    adj = adjacency_matrix(expr.nodes)
    children_arr = SparseArrays.rowvals(adj)
    # An arbitrary limit on the potential input size of a multivariate
    # operation. This will get resized if need-be.
    input_cache = zeros(10)
    for k in length(expr.nodes):-1:1
        node = expr.nodes[k]
        if node.type == NODE_MOI_VARIABLE
            storage[k] = f[MOI.VariableIndex(node.index)]
        elseif node.type == NODE_VALUE
            storage[k] = expr.values[node.index]
        elseif node.type == NODE_SUBEXPRESSION
            if !haskey(evaluated_expressions, node.index)
                evaluated_expressions[node.index] = evaluate(
                    f,
                    model,
                    ExpressionIndex(node.index);
                    evaluated_expressions = evaluated_expressions,
                )
            end
            storage[k] = evaluated_expressions[node.index]
        elseif node.type == NODE_PARAMETER
            storage[k] = model.parameters[node.index]
        elseif node.type == NODE_CALL_MULTIVARIATE
            children_indices = SparseArrays.nzrange(adj, k)
            N = length(children_indices)
            if length(input_cache) < N
                resize!(input_cache, N)
            end
            f_input = view(input_cache, 1:N)
            for (r, i) in enumerate(children_indices)
                f_input[r] = storage[children_arr[i]]
            end
            storage[k] = eval_multivariate_function(
                model.operators,
                model.operators.multivariate_operators[node.index],
                f_input,
            )
        elseif node.type == NODE_CALL_UNIVARIATE
            child_idx = children_arr[adj.colptr[k]]
            storage[k] = eval_univariate_function(
                model.operators,
                model.operators.univariate_operators[node.index],
                storage[child_idx],
            )
        elseif node.type == NODE_COMPARISON
            children_idx = SparseArrays.nzrange(adj, k)
            result = true
            for r in 2:length(children_idx)
                lhs = children_arr[children_idx[r-1]]
                rhs = children_arr[children_idx[r]]
                result &= eval_comparison_function(
                    model.operators,
                    model.operators.comparison_operators[node.index],
                    storage[lhs],
                    storage[rhs],
                )
            end
            storage[k] = result
        else
            @assert node.type == NODE_LOGIC
            children_idx = SparseArrays.nzrange(adj, k)
            lhs = children_arr[children_idx[1]]
            rhs = children_arr[children_idx[2]]
            storage[k] = eval_logic_function(
                model.operators,
                model.operators.logic_operators[node.index],
                storage[lhs] == 1,
                storage[rhs] == 1,
            )
        end
    end
    return storage[1]
end
