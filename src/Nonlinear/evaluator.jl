# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function Base.copy(::Evaluator)
    return error("Copying nonlinear problems not yet implemented")
end

"""
    ordinal_index(evaluator::Evaluator, c::ConstraintIndex)::Int

Return the 1-indexed value of the constraint index `c` in `evaluator`.

## Examples

```julia
model = Model()
x = MOI.VariableIndex(1)
c1 = add_constraint(model, :(\$x^2), MOI.LessThan(1.0))
c2 = add_constraint(model, :(\$x^2), MOI.LessThan(1.0))
evaluator = Evaluator(model)
MOI.initialize(evaluator, Symbol[])
ordinal_index(evaluator, c2)  # Returns 2
delete(model, c1)
evaluator = Evaluator(model)
MOI.initialize(evaluator, Symbol[])
ordinal_index(model, c2)  # Returns 1
```
"""
function ordinal_index(evaluator::Evaluator, c::ConstraintIndex)
    # TODO(odow): replace with a cache that maps indices to their 1-indexed
    # row in the constraint matrix. But failing that, since we know that
    # constraints are added in increasing order and that they can be deleted, we
    # know that index `i` must appear as constraint `1` to `i`. So we start at
    # `i` and backtrack (to account for deleted constraints) until we find it.
    # In the typical case with no deletion, there should be no overhead.
    for i in min(c.value, length(evaluator.ordered_constraints)):-1:1
        if evaluator.ordered_constraints[i] == c
            return i
        end
    end
    return error("Invalid constraint index $(c)")
end

function MOI.features_available(evaluator::Evaluator)
    features = Symbol[]
    if evaluator.backend !== nothing
        append!(features, MOI.features_available(evaluator.backend))
    end
    if !(:ExprGraph in features)
        push!(features, :ExprGraph)
    end
    return features
end

function Base.show(io::IO, evaluator::Evaluator)
    Base.print(io, "Nonlinear.Evaluator with available features:")
    for feature in MOI.features_available(evaluator)
        print(io, "\n  * :", feature)
    end
    return
end

function MOI.initialize(evaluator::Evaluator, features::Vector{Symbol})
    empty!(evaluator.ordered_constraints)
    empty!(evaluator.julia_expressions)
    evaluator.eval_objective_timer = 0.0
    evaluator.eval_objective_gradient_timer = 0.0
    evaluator.eval_constraint_timer = 0.0
    evaluator.eval_constraint_jacobian_timer = 0.0
    evaluator.eval_hessian_lagrangian_timer = 0.0
    append!(evaluator.ordered_constraints, keys(evaluator.model.constraints))
    if :ExprGraph in features
        for i in 1:length(evaluator.model.expressions)
            push!(
                evaluator.julia_expressions,
                convert_to_expr(
                    evaluator,
                    evaluator.model.expressions[i];
                    moi_output_format = true,
                ),
            )
        end
        filter!(f -> f != :ExprGraph, features)
    end
    if evaluator.backend !== nothing
        MOI.initialize(evaluator.backend, features)
    end
    return
end

function MOI.objective_expr(evaluator::Evaluator)
    if evaluator.model.objective === nothing
        error(
            "Unable to query objective_expr because no nonlinear objective " *
            "was set",
        )
    end
    return convert_to_expr(
        evaluator,
        evaluator.model.objective;
        moi_output_format = true,
    )
end

function MOI.constraint_expr(evaluator::Evaluator, i::Int)
    constraint = evaluator.model[evaluator.ordered_constraints[i]]
    f = convert_to_expr(
        evaluator,
        constraint.expression;
        moi_output_format = true,
    )
    if constraint.set isa MOI.LessThan
        return :($f <= $(constraint.set.upper))
    elseif constraint.set isa MOI.GreaterThan
        return :($f >= $(constraint.set.lower))
    elseif constraint.set isa MOI.EqualTo
        return :($f == $(constraint.set.value))
    else
        @assert constraint.set isa MOI.Interval
        return :($(constraint.set.lower) <= $f <= $(constraint.set.upper))
    end
end

function MOI.eval_objective(evaluator::Evaluator, x)
    start = time()
    obj = MOI.eval_objective(evaluator.backend, x)
    evaluator.eval_objective_timer += time() - start
    return obj
end

function MOI.eval_objective_gradient(evaluator::Evaluator, g, x)
    start = time()
    MOI.eval_objective_gradient(evaluator.backend, g, x)
    evaluator.eval_objective_gradient_timer += time() - start
    return
end

function MOI.eval_constraint(evaluator::Evaluator, g, x)
    start = time()
    MOI.eval_constraint(evaluator.backend, g, x)
    evaluator.eval_constraint_timer += time() - start
    return
end

function MOI.jacobian_structure(evaluator::Evaluator)
    return MOI.jacobian_structure(evaluator.backend)
end

function MOI.eval_constraint_jacobian(evaluator::Evaluator, J, x)
    start = time()
    MOI.eval_constraint_jacobian(evaluator.backend, J, x)
    evaluator.eval_constraint_jacobian_timer += time() - start
    return
end

function MOI.hessian_lagrangian_structure(evaluator::Evaluator)
    return MOI.hessian_lagrangian_structure(evaluator.backend)
end

function MOI.eval_hessian_lagrangian(evaluator::Evaluator, H, x, σ, μ)
    start = time()
    MOI.eval_hessian_lagrangian(evaluator.backend, H, x, σ, μ)
    evaluator.eval_hessian_lagrangian_timer += time() - start
    return
end

function MOI.eval_constraint_jacobian_product(evaluator::Evaluator, y, x, w)
    start = time()
    MOI.eval_constraint_jacobian_product(evaluator.backend, y, x, w)
    evaluator.eval_constraint_jacobian_timer += time() - start
    return
end

function MOI.eval_constraint_jacobian_transpose_product(
    evaluator::Evaluator,
    y,
    x,
    w,
)
    start = time()
    MOI.eval_constraint_jacobian_transpose_product(evaluator.backend, y, x, w)
    evaluator.eval_constraint_jacobian_timer += time() - start
    return
end

function MOI.eval_hessian_lagrangian_product(
    evaluator::Evaluator,
    H,
    x,
    v,
    σ,
    μ,
)
    start = time()
    MOI.eval_hessian_lagrangian_product(evaluator.backend, H, x, v, σ, μ)
    evaluator.eval_hessian_lagrangian_timer += time() - start
    return
end

"""
    adjacency_matrix(nodes::Vector{Node})

Compute the sparse adjacency matrix describing the parent-child relationships in
`nodes`.

The element `(i, j)` is `true` if there is an edge *from* `node[j]` to
`node[i]`. Since we get a column-oriented matrix, this gives us a fast way to
look up the edges leaving any node (i.e., the children).
"""
function adjacency_matrix(nodes::Vector{Node})
    N = length(nodes)
    I, J = Vector{Int}(undef, N), Vector{Int}(undef, N)
    numnz = 0
    for (i, node) in enumerate(nodes)
        if node.parent < 0
            continue
        end
        numnz += 1
        I[numnz] = i
        J[numnz] = node.parent
    end
    resize!(I, numnz)
    resize!(J, numnz)
    return SparseArrays.sparse(I, J, ones(Bool, numnz), N, N)
end

"""
    convert_to_expr(
        evaluator::Evaluator,
        expr::Expression;
        moi_output_format::Bool,
    )

Convert the [`Expression`](@ref) `expr` into a Julia `Expr`.

If `moi_output_format = true`:
 * subexpressions will be converted to Julia `Expr` and substituted into the
   output expression.
 * the current value of each parameter will be interpolated into the expression
 * variables will be represented in the form `x[MOI.VariableIndex(i)]`

If `moi_output_format = false`:
 * subexpressions will be represented by a [`ExpressionIndex`](@ref) object.
 * parameters will be represented by a [`ParameterIndex`](@ref) object.
 * variables will be represennted by an [`MOI.VariableIndex`](@ref) object.

!!! warning
    To use `moi_output_format = true`, you must have first called
    [`MOI.initialize`](@ref) with `:ExprGraph` as a requested feature.
"""
function convert_to_expr(
    evaluator::Evaluator,
    expression::Expression;
    moi_output_format::Bool,
)
    expr = convert_to_expr(evaluator.model, expression)
    if moi_output_format
        return _convert_to_moi_format(evaluator, expr)
    end
    return expr
end

_convert_to_moi_format(::Evaluator, x::MOI.VariableIndex) = :(x[$x])

function _convert_to_moi_format(evaluator::Evaluator, p::ParameterIndex)
    return evaluator.model[p]
end

function _convert_to_moi_format(evaluator::Evaluator, x::ExpressionIndex)
    return evaluator.julia_expressions[x.value]
end

_convert_to_moi_format(::Evaluator, x) = x

function _convert_to_moi_format(evaluator::Evaluator, x::Expr)
    for i in 1:length(x.args)
        x.args[i] = _convert_to_moi_format(evaluator, x.args[i])
    end
    return x
end
