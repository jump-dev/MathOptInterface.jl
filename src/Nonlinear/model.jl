# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function MOI.empty!(model::Model)
    model.objective = nothing
    empty!(model.expressions)
    empty!(model.constraints)
    empty!(model.parameters)
    model.operators = OperatorRegistry()
    model.objective_sense = MOI.FEASIBILITY_SENSE
    model.moi_objective = nothing
    empty!(model.moi_functions)
    empty!(model.constraint_dual_start)
    model.last_constraint_index = 0
    return
end

function MOI.is_empty(model::Model)
    return model.objective === nothing &&
           isempty(model.expressions) &&
           isempty(model.constraints) &&
           isempty(model.parameters) &&
           isempty(model.operators.registered_univariate_operators) &&
           isempty(model.operators.registered_multivariate_operators) &&
           model.objective_sense == MOI.FEASIBILITY_SENSE &&
           model.moi_objective === nothing &&
           isempty(model.moi_functions) &&
           isempty(model.constraint_dual_start) &&
           model.last_constraint_index === Int64(0)
end

_parameter_values(model::Model) = model.parameters
_has_nonlinear_data(model::Model) =
    model.objective !== nothing ||
    !isempty(model.constraints) ||
    !isempty(model.parameters)
_is_nonlinear_input(::Model, ::MOI.AbstractFunction, ::MOI.AbstractSet) = true
_is_nonlinear_objective(::Model, ::MOI.AbstractFunction) = true

function Base.copy(::Model)
    return error("Copying nonlinear problems not yet implemented")
end

function Base.show(io::IO, model::Model)
    println(io, "A Nonlinear.Model with:")
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

## Example

```jldoctest
julia> model = MOI.Nonlinear.Model()
A Nonlinear.Model with:
 0 objectives
 0 parameters
 0 expressions
 0 constraints

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> MOI.Nonlinear.set_objective(model, :(\$x^2 + 1))

julia> MOI.Nonlinear.set_objective(model, x)

julia> MOI.Nonlinear.set_objective(model, nothing)
```
"""
function set_objective(model::Model, obj)
    model.objective = parse_expression(model, obj)
    model.moi_objective =
        obj isa MOI.ScalarNonlinearFunction ? obj : nothing
    if model.objective_sense == MOI.FEASIBILITY_SENSE
        model.objective_sense = MOI.MIN_SENSE
    end
    return
end

function set_objective(model::Model, ::Nothing)
    model.objective = nothing
    model.moi_objective = nothing
    return
end

"""
    model(backend::AbstractAutomaticDifferentiation)

Return a new MOI model appropriate for the automatic-differentiation
`backend`. Custom backends may overload this method to provide a model that
stores functions in a backend-specific representation.
"""
function model(::AbstractAutomaticDifferentiation)
    return ModelWithQuad(ModelWithOracles(Model()))
end

"""
    add_expression(model::Model, expr)::ExpressionIndex

Parse `expr` into a [`Expression`](@ref) and add to `model`. Returns an
[`ExpressionIndex`](@ref) that can be interpolated into other input expressions.

`expr` must be a type that is supported by [`parse_expression`](@ref).

## Example

```jldoctest
julia> model = MOI.Nonlinear.Model();

julia> x = MOI.VariableIndex(1);

julia> ex = MOI.Nonlinear.add_expression(model, :(\$x^2 + 1))
MathOptInterface.Nonlinear.ExpressionIndex(1)

julia> MOI.Nonlinear.set_objective(model, :(sqrt(\$ex)))
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

## Example

```jldoctest
julia> model = MOI.Nonlinear.Model();

julia> x = MOI.VariableIndex(1);

julia> c = MOI.Nonlinear.add_constraint(model, :(\$x^2), MOI.LessThan(1.0))
MathOptInterface.Nonlinear.ConstraintIndex(1)
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
    if func isa MOI.ScalarNonlinearFunction
        model.moi_functions[index] = func
    end
    return index
end

"""
    delete(model::Model, c::ConstraintIndex)::Nothing

Delete the constraint index `c` from `model`.

## Example

```jldoctest
julia> model = MOI.Nonlinear.Model()
A Nonlinear.Model with:
 0 objectives
 0 parameters
 0 expressions
 0 constraints

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> c = MOI.Nonlinear.add_constraint(model, :(\$x^2), MOI.LessThan(1.0))
MathOptInterface.Nonlinear.ConstraintIndex(1)

julia> model
A Nonlinear.Model with:
 0 objectives
 0 parameters
 0 expressions
 1 constraint

julia> MOI.Nonlinear.delete(model, c)

julia> model
A Nonlinear.Model with:
 0 objectives
 0 parameters
 0 expressions
 0 constraints
```
"""
function delete(model::Model, c::ConstraintIndex)
    delete!(model.constraints, c)
    delete!(model.moi_functions, c)
    delete!(model.constraint_dual_start, c)
    return
end

function Base.getindex(model::Model, index::ConstraintIndex)
    return model.constraints[index]
end

function MOI.is_valid(model::Model, index::ConstraintIndex)
    return haskey(model.constraints, index)
end

# MathOptInterface model API. The legacy `Nonlinear` API above remains
# available, but model layers and solvers communicate with this model only via
# these methods.

const _ScalarSet{T} = Union{
    MOI.GreaterThan{T},
    MOI.LessThan{T},
    MOI.EqualTo{T},
    MOI.Interval{T},
}

MOI.supports_constraint(
    ::Model,
    ::Type{MOI.ScalarNonlinearFunction},
    ::Type{<:_ScalarSet{Float64}},
) = true

function MOI.add_constraint(
    model::Model,
    f::MOI.ScalarNonlinearFunction,
    s::_ScalarSet{Float64},
)
    index = add_constraint(model, f, s)
    return MOI.ConstraintIndex{typeof(f),typeof(s)}(index.value)
end

_nonlinear_index(ci::MOI.ConstraintIndex) = ConstraintIndex(ci.value)

function MOI.is_valid(
    model::Model,
    ci::MOI.ConstraintIndex{MOI.ScalarNonlinearFunction,S},
) where {S<:_ScalarSet{Float64}}
    index = _nonlinear_index(ci)
    return haskey(model.constraints, index) && model.constraints[index].set isa S
end

function MOI.get(
    model::Model,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F<:MOI.ScalarNonlinearFunction,S<:_ScalarSet{Float64}}
    return MOI.ConstraintIndex{F,S}[
        MOI.ConstraintIndex{F,S}(index.value) for
        (index, constraint) in model.constraints if constraint.set isa S
    ]
end

function MOI.get(
    model::Model,
    ::MOI.NumberOfConstraints{F,S},
) where {F<:MOI.ScalarNonlinearFunction,S<:_ScalarSet{Float64}}
    return count(constraint -> constraint.set isa S, values(model.constraints))
end

function MOI.get(model::Model, ::MOI.ListOfConstraintTypesPresent)
    types = Tuple{Type,Type}[]
    for constraint in values(model.constraints)
        pair = (MOI.ScalarNonlinearFunction, typeof(constraint.set))
        pair in types || push!(types, pair)
    end
    return types
end

function MOI.get(model::Model, ::MOI.ConstraintFunction, ci::MOI.ConstraintIndex)
    MOI.throw_if_not_valid(model, ci)
    return model.moi_functions[_nonlinear_index(ci)]
end

function MOI.get(model::Model, ::MOI.ConstraintSet, ci::MOI.ConstraintIndex)
    MOI.throw_if_not_valid(model, ci)
    return model.constraints[_nonlinear_index(ci)].set
end

function MOI.set(model::Model, ::MOI.ConstraintSet, ci::MOI.ConstraintIndex, set)
    MOI.throw_if_not_valid(model, ci)
    index = _nonlinear_index(ci)
    constraint = model.constraints[index]
    model.constraints[index] = Constraint(constraint.expression, set)
    return
end

function MOI.delete(model::Model, ci::MOI.ConstraintIndex)
    MOI.throw_if_not_valid(model, ci)
    return delete(model, _nonlinear_index(ci))
end

function constraint_rows(model::Model, ci::MOI.ConstraintIndex)
    MOI.throw_if_not_valid(model, ci)
    index = _nonlinear_index(ci)
    return [findfirst(isequal(index), collect(keys(model.constraints)))]
end

function constraint_dual_starts(model::Model)
    return Union{Nothing,Float64}[
        get(model.constraint_dual_start, index, nothing) for
        index in keys(model.constraints)
    ]
end

MOI.supports(::Model, ::MOI.ObjectiveSense) = true
MOI.get(model::Model, ::MOI.ObjectiveSense) = model.objective_sense

function MOI.set(model::Model, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    model.objective_sense = sense
    return
end

MOI.supports(::Model, ::MOI.ObjectiveFunction{MOI.ScalarNonlinearFunction}) = true

function MOI.set(
    model::Model,
    ::MOI.ObjectiveFunction{MOI.ScalarNonlinearFunction},
    f::MOI.ScalarNonlinearFunction,
)
    sense = model.objective_sense
    set_objective(model, f)
    model.objective_sense = sense
    return
end

function MOI.get(model::Model, ::MOI.ObjectiveFunctionType)
    return model.objective === nothing ? nothing : MOI.ScalarNonlinearFunction
end

function MOI.get(
    model::Model,
    ::MOI.ObjectiveFunction{MOI.ScalarNonlinearFunction},
)
    return something(model.moi_objective)
end

MOI.supports(::Model, ::MOI.UserDefinedFunction) = true

function MOI.set(model::Model, attr::MOI.UserDefinedFunction, functions)
    return register_operator(model, attr.name, attr.arity, functions...)
end

function MOI.supports(
    ::Model,
    ::MOI.ConstraintDualStart,
    ::Type{<:MOI.ConstraintIndex{MOI.ScalarNonlinearFunction}},
)
    return true
end

function MOI.get(model::Model, ::MOI.ConstraintDualStart, ci::MOI.ConstraintIndex)
    return get(model.constraint_dual_start, _nonlinear_index(ci), nothing)
end

function MOI.set(
    model::Model,
    ::MOI.ConstraintDualStart,
    ci::MOI.ConstraintIndex,
    value::Union{Nothing,Real},
)
    index = _nonlinear_index(ci)
    if value === nothing
        delete!(model.constraint_dual_start, index)
    else
        model.constraint_dual_start[index] = Float64(value)
    end
    return
end

"""
    add_parameter(model::Model, value::Float64)::ParameterIndex

Add a new parameter to `model` with the default value `value`. Returns a
[`ParameterIndex`](@ref) that can be interpolated into other input expressions
and used to modify the value of the parameter.

## Example

```jldoctest
julia> model = MOI.Nonlinear.Model()
A Nonlinear.Model with:
 0 objectives
 0 parameters
 0 expressions
 0 constraints

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> p = MOI.Nonlinear.add_parameter(model, 1.2)
MathOptInterface.Nonlinear.ParameterIndex(1)

julia> c = MOI.Nonlinear.add_constraint(model, :(\$x^2 - \$p), MOI.LessThan(0.0))
MathOptInterface.Nonlinear.ConstraintIndex(1)
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
                node.index,
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

function MOI.get(model::Model, attr::MOI.ListOfSupportedNonlinearOperators)
    return MOI.get(model.operators, attr)
end
