# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# This API is imported from MathProgBase and is meant to be transitional.
# A more ideal API would pass expressions (with vector-valued nodes) directly
# let the solver call out to AD tools.

# NLP = Nonlinear Programming.
# The non-MOI-like name is intentional because this part of the API is not
# MOI-like.

"""
    AbstractNLPEvaluator

Abstract supertype for the callback object that is used to query function
values, derivatives, and expression graphs.

It is used in [`NLPBlockData`](@ref).
"""
abstract type AbstractNLPEvaluator end

"""
    NLPBlock()

An [`AbstractModelAttribute`](@ref) that stores an [`NLPBlockData`](@ref),
representing a set of nonlinear constraints, and optionally a nonlinear
objective.
"""
struct NLPBlock <: AbstractModelAttribute end

"""
    NLPBlockDual(result_index::Int = 1)

An [`AbstractModelAttribute`](@ref) for the Lagrange multipliers on the
constraints from the [`NLPBlock`](@ref) in result `result_index`.

If `result_index` is omitted, it is `1` by default.
"""
struct NLPBlockDual <: AbstractModelAttribute
    result_index::Int
    NLPBlockDual(result_index::Int = 1) = new(result_index)
end

is_set_by_optimize(::NLPBlockDual) = true

"""
    NLPBlockDualStart()

An [`AbstractModelAttribute`](@ref) for the initial assignment of the Lagrange
multipliers on the constraints from the [`NLPBlock`](@ref) that the solver may
use to warm-start the solve.
"""
struct NLPBlockDualStart <: AbstractModelAttribute end

"""
    NLPBoundsPair(lower::Float64, upper::Float64)

A struct holding a pair of lower and upper bounds.

`-Inf` and `Inf` can be used to indicate no lower or upper bound, respectively.
"""
struct NLPBoundsPair
    lower::Float64
    upper::Float64
end

"""
    struct NLPBlockData
        constraint_bounds::Vector{NLPBoundsPair}
        evaluator::AbstractNLPEvaluator
        has_objective::Bool
    end

A struct encoding a set of nonlinear constraints of the form
``lb \\le g(x) \\le ub`` and, if `has_objective == true`, a nonlinear objective
function ``f(x)``.

Nonlinear objectives *override* any objective set by using the
[`ObjectiveFunction`](@ref) attribute.

The `evaluator` is a callback object that is used to query function values,
derivatives, and expression graphs. If `has_objective == false`, then it is an
error to query properties of the objective function, and in
Hessian-of-the-Lagrangian queries, `σ` must be set to zero.

!!! note
    Throughout the evaluator, all variables are ordered according to
    [`ListOfVariableIndices`](@ref). Hence, MOI copies of nonlinear problems
    must not re-order variables.
"""
struct NLPBlockData
    constraint_bounds::Vector{NLPBoundsPair}
    evaluator::AbstractNLPEvaluator
    has_objective::Bool
end

"""
    initialize(
        d::AbstractNLPEvaluator,
        requested_features::Vector{Symbol},
    )::Nothing

Initialize `d` with the set of features in `requested_features`. Check
[`features_available`](@ref) before calling `initialize` to see what features
are supported by `d`.

!!! warning
    This method must be called before any other methods.

## Features

The following features are defined:

 * `:Grad`: enables [`eval_objective_gradient`](@ref)
 * `:Jac`: enables [`eval_constraint_jacobian`](@ref)
 * `:JacVec`: enables [`eval_constraint_jacobian_product`](@ref) and
   [`eval_constraint_jacobian_transpose_product`](@ref)
 * `:Hess`: enables [`eval_hessian_lagrangian`](@ref)
 * `:HessVec`: enables [`eval_hessian_lagrangian_product`](@ref)
 * `:ExprGraph`: enables [`objective_expr`](@ref) and [`constraint_expr`](@ref).

In all cases, including when `requested_features` is empty,
[`eval_objective`](@ref) and [`eval_constraint`](@ref) are supported.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, [:Grad, :Jac])
```
"""
function initialize end

"""
    features_available(d::AbstractNLPEvaluator)::Vector{Symbol}

Returns the subset of features available for this problem instance.

See [`initialize`](@ref) for the list of defined features.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.features_available(evaluator)
4-element Vector{Symbol}:
 :Grad
 :Jac
 :JacVec
 :ExprGraph
 :Hess
```
"""
function features_available end

"""
    eval_objective(d::AbstractNLPEvaluator, x::AbstractVector{T})::T where {T}

Evaluate the objective ``f(x)``, returning a scalar value.

## Initialize

Before calling this function, you must call [`initialize`](@ref), but you do not
need to pass a value.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[])

julia> MOI.eval_objective(evaluator, [1.0, 2.0, 3.0, 4.0])
27.0
```
"""
function eval_objective end

"""
    eval_constraint(
        d::AbstractNLPEvaluator,
        g::AbstractVector{T},
        x::AbstractVector{T},
    )::Nothing where {T}

Given a set of vector-valued constraints ``l \\le g(x) \\le u``, evaluate the
constraint function ``g(x)``, storing the result in the vector `g`.

## Initialize

Before calling this function, you must call [`initialize`](@ref), but you do not
need to pass a value.

## Implementation notes

When implementing this method, you must not assume that `g` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[])

julia> g = fill(NaN, 2);

julia> MOI.eval_constraint(evaluator, g, [1.0, 2.0, 3.0, 4.0])

julia> g
2-element Vector{Float64}:
 24.0
 30.0
```
"""
function eval_constraint end

"""
    eval_objective_gradient(
        d::AbstractNLPEvaluator,
        grad::AbstractVector{T},
        x::AbstractVector{T},
    )::Nothing where {T}

Evaluate the gradient of the objective function ``grad = \\nabla f(x)`` as a
dense vector, storing the result in the vector `grad`.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Grad`.

## Implementation notes

When implementing this method, you must not assume that `grad` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Grad])

julia> grad = fill(NaN, 4);

julia> MOI.eval_objective_gradient(evaluator, grad, [1.0, 2.0, 3.0, 4.0])

julia> grad
4-element Vector{Float64}:
 28.0
  4.0
  5.0
  6.0
```
"""
function eval_objective_gradient end

"""
    jacobian_structure(d::AbstractNLPEvaluator)::Vector{Tuple{Int64,Int64}}

Returns a vector of tuples, `(row, column)`, where each indicates the position
of a structurally nonzero element in the Jacobian matrix:
``J_g(x) = \\left[ \\begin{array}{c} \\nabla g_1(x) \\\\ \\nabla g_2(x) \\\\ \\vdots \\\\ \\nabla g_m(x) \\end{array}\\right],``
where ``g_i`` is the ``i\\text{th}`` component of the nonlinear constraints
``g(x)``.

The indices are not required to be sorted and can contain duplicates, in which
case the solver should combine the corresponding elements by adding them
together.

The sparsity structure is assumed to be independent of the point ``x``.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Jac`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Jac])

julia> MOI.jacobian_structure(evaluator)
8-element Vector{Tuple{Int64, Int64}}:
 (1, 1)
 (1, 2)
 (1, 3)
 (1, 4)
 (2, 1)
 (2, 2)
 (2, 3)
 (2, 4)
```
"""
function jacobian_structure end

"""
    hessian_objective_structure(
        d::AbstractNLPEvaluator,
    )::Vector{Tuple{Int64,Int64}}

Returns a vector of tuples, `(row, column)`, where each indicates the position
of a structurally nonzero element in the Hessian matrix:
``\\nabla^2 f(x)``.

The indices are not required to be sorted and can contain duplicates, in which
case the solver should combine the corresponding elements by adding them
together.

Any mix of lower and upper-triangular indices is valid. Elements `(i, j)` and
`(j, i)`, if both present, should be treated as duplicates.

The sparsity structure is assumed to be independent of the point ``x``.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Hess])

julia> MOI.hessian_objective_structure(evaluator)
6-element Vector{Tuple{Int64, Int64}}:
 (1, 1)
 (2, 1)
 (3, 1)
 (4, 1)
 (4, 2)
 (4, 3)
```
"""
function hessian_objective_structure end

"""
    hessian_constraint_structure(
        d::AbstractNLPEvaluator,
        i::Int64,
    )::Vector{Tuple{Int64,Int64}}

Returns a vector of tuples, `(row, column)`, where each indicates the position
of a structurally nonzero element in the Hessian matrix:
``\\nabla^2 g_i(x)``.

The indices are not required to be sorted and can contain duplicates, in which
case the solver should combine the corresponding elements by adding them
together.

Any mix of lower and upper-triangular indices is valid. Elements `(i, j)` and
`(j, i)`, if both present, should be treated as duplicates.

The sparsity structure is assumed to be independent of the point ``x``.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Hess])

julia> MOI.hessian_constraint_structure(evaluator, 1)
6-element Vector{Tuple{Int64, Int64}}:
 (2, 1)
 (3, 1)
 (3, 2)
 (4, 1)
 (4, 2)
 (4, 3)

julia> MOI.hessian_constraint_structure(evaluator, 2)
4-element Vector{Tuple{Int64, Int64}}:
 (1, 1)
 (2, 2)
 (3, 3)
 (4, 4)
```
"""
function hessian_constraint_structure end

"""
    hessian_lagrangian_structure(
        d::AbstractNLPEvaluator,
    )::Vector{Tuple{Int64,Int64}}

Returns a vector of tuples, `(row, column)`, where each indicates the position
of a structurally nonzero element in the Hessian-of-the-Lagrangian matrix:
``\\nabla^2 f(x) + \\sum_{i=1}^m \\nabla^2 g_i(x)``.

The indices are not required to be sorted and can contain duplicates, in which
case the solver should combine the corresponding elements by adding them
together.

Any mix of lower and upper-triangular indices is valid. Elements `(i, j)` and
`(j, i)`, if both present, should be treated as duplicates.

The sparsity structure is assumed to be independent of the point ``x``.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Hess])

julia> MOI.hessian_lagrangian_structure(evaluator)
10-element Vector{Tuple{Int64, Int64}}:
 (1, 1)
 (2, 1)
 (2, 2)
 (3, 1)
 (3, 2)
 (3, 3)
 (4, 1)
 (4, 2)
 (4, 3)
 (4, 4)
```
"""
function hessian_lagrangian_structure end

"""
    constraint_gradient_structure(d::AbstractNLPEvaluator, i::Int)::Vector{Int64}

Returns a vector of indices, where each element indicates the position of a
structurally nonzero element in the gradient of constraint ``\\nabla g_i(x)``.

The indices are not required to be sorted and can contain duplicates, in which
case the solver should combine the corresponding elements by adding them
together.

The sparsity structure is assumed to be independent of the point ``x``.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Jac`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Jac])

julia> indices = MOI.constraint_gradient_structure(evaluator, 1)
4-element Vector{Int64}:
 1
 2
 3
 4
```
"""
function constraint_gradient_structure end

"""
    eval_constraint_gradient(
        d::AbstractNLPEvaluator,
        ∇g::AbstractVector{T},
        x::AbstractVector{T},
        i::Int,
    )::Nothing where {T}

Evaluate the gradient of constraint `i`, ``\\nabla g_i(x)``, and store the
non-zero values in `∇g`, corresponding to the structure returned by
[`constraint_gradient_structure`](@ref).

## Implementation notes

When implementing this method, you must not assume that `∇g` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Jac`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Jac])

julia> indices = MOI.constraint_gradient_structure(evaluator, 1);

julia> ∇g = zeros(length(indices));

julia> MOI.eval_constraint_gradient(evaluator, ∇g, [1.0, 2.0, 3.0, 4.0], 1)

julia> ∇g
4-element Vector{Float64}:
 24.0
 12.0
  8.0
  6.0
```
"""
function eval_constraint_gradient end

"""
    eval_constraint_jacobian(
        d::AbstractNLPEvaluator,
        J::AbstractVector{T},
        x::AbstractVector{T},
    )::Nothing where {T}

Evaluates the sparse Jacobian matrix
``J_g(x) = \\left[ \\begin{array}{c} \\nabla g_1(x) \\\\ \\nabla g_2(x) \\\\ \\vdots \\\\ \\nabla g_m(x) \\end{array}\\right]``.

The result is stored in the vector `J` in the same order as the indices returned
by [`jacobian_structure`](@ref).

## Implementation notes

When implementing this method, you must not assume that `J` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Jac])

julia> J_indices = MOI.jacobian_structure(evaluator);

julia> J = zeros(length(J_indices));

julia> MOI.eval_constraint_jacobian(evaluator, J, [1.0, 2.0, 3.0, 4.0])

julia> J
8-element Vector{Float64}:
 24.0
 12.0
  8.0
  6.0
  2.0
  4.0
  6.0
  8.0
```
"""
function eval_constraint_jacobian end

"""
    eval_constraint_jacobian_product(
        d::AbstractNLPEvaluator,
        y::AbstractVector{T},
        x::AbstractVector{T},
        w::AbstractVector{T},
    )::Nothing where {T}

Computes the Jacobian-vector product ``y = J_g(x)w``, storing the result in the
vector `y`.

The vectors have dimensions such that `length(w) == length(x)`, and `length(y)`
is the number of nonlinear constraints.

## Implementation notes

When implementing this method, you must not assume that `y` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:JacVec`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Jac, :JacVec])

julia> y = zeros(2);

julia> x = [1.0, 2.0, 3.0, 4.0];

julia> w = [1.5, 2.5, 3.5, 4.5];

julia> MOI.eval_constraint_jacobian_product(evaluator, y, x, w)

julia> y
2-element Vector{Float64}:
 121.0
  70.0
```
"""
function eval_constraint_jacobian_product end

"""
    eval_constraint_jacobian_transpose_product(
        d::AbstractNLPEvaluator,
        y::AbstractVector{T},
        x::AbstractVector{T},
        w::AbstractVector{T},
    )::Nothing where {T}

Computes the Jacobian-transpose-vector product ``y = J_g(x)^Tw``, storing the
result in the vector `y`.

The vectors have dimensions such that `length(y) == length(x)`, and `length(w)`
is the number of nonlinear constraints.

## Implementation notes

When implementing this method, you must not assume that `y` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:JacVec`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Jac, :JacVec])

julia> y = zeros(4);

julia> x = [1.0, 2.0, 3.0, 4.0];

julia> w = [1.5, 2.5];

julia> MOI.eval_constraint_jacobian_transpose_product(evaluator, y, x, w)

julia> y
4-element Vector{Float64}:
 41.0
 28.0
 27.0
 29.0
```
"""
function eval_constraint_jacobian_transpose_product end

"""
    eval_hessian_lagrangian_product(
        d::AbstractNLPEvaluator,
        h::AbstractVector{T},
        x::AbstractVector{T},
        v::AbstractVector{T},
        σ::T,
        μ::AbstractVector{T},
    )::Nothing where {T}

Given scalar weight `σ` and vector of constraint weights `μ`,
computes the Hessian-of-the-Lagrangian-vector product
``h = \\left(\\sigma\\nabla^2 f(x) + \\sum_{i=1}^m \\mu_i \\nabla^2 g_i(x)\\right)v``,
storing the result in the vector `h`.

The vectors have dimensions such that `length(h) == length(x) == length(v)`.

## Implementation notes

When implementing this method, you must not assume that `h` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:HessVec`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true, true);

julia> MOI.initialize(evaluator, Symbol[:HessVec])

julia> H = fill(NaN, 4);

julia> x = [1.0, 2.0, 3.0, 4.0];

julia> v = [1.5, 2.5, 3.5, 4.5];

julia> σ = 1.0;

julia> μ = [1.0, 1.0];

julia> MOI.eval_hessian_lagrangian_product(evaluator, H, x, v, σ, μ)

julia> H
4-element Vector{Float64}:
 155.5
  61.0
  48.5
  49.0
```
"""
function eval_hessian_lagrangian_product end

"""
    eval_hessian_objective(
        d::AbstractNLPEvaluator,
        H::AbstractVector{T},
        x::AbstractVector{T},
    )::Nothing where {T}

This function computes the sparse Hessian matrix:
``\\nabla^2 f(x)``,
storing the result in the vector `H` in the same order as the indices
returned by [`hessian_objective_structure`](@ref).

## Implementation notes

When implementing this method, you must not assume that `H` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true, true);

julia> MOI.initialize(evaluator, Symbol[:Hess])

julia> indices = MOI.hessian_objective_structure(evaluator);

julia> H = zeros(length(indices));

julia> x = [1.0, 2.0, 3.0, 4.0];

julia> MOI.eval_hessian_objective(evaluator, H, x)

julia> H
6-element Vector{Float64}:
 8.0
 4.0
 4.0
 7.0
 1.0
 1.0
```
"""
function eval_hessian_objective end

"""
    eval_hessian_constraint(
        d::AbstractNLPEvaluator,
        H::AbstractVector{T},
        x::AbstractVector{T},
        i::Int64,
    )::Nothing where {T}

This function computes the sparse Hessian matrix:
``\\nabla^2 g_i(x)``,
storing the result in the vector `H` in the same order as the indices
returned by [`hessian_constraint_structure`](@ref).

## Implementation notes

When implementing this method, you must not assume that `H` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true, true);

julia> MOI.initialize(evaluator, Symbol[:Hess])

julia> indices = MOI.hessian_constraint_structure(evaluator, 1);

julia> H = zeros(length(indices));

julia> x = [1.0, 2.0, 3.0, 4.0];

julia> MOI.eval_hessian_constraint(evaluator, H, x, 1)

julia> H
6-element Vector{Float64}:
 12.0
  8.0
  4.0
  6.0
  3.0
  2.0
```
"""
function eval_hessian_constraint end

"""
    eval_hessian_lagrangian(
        d::AbstractNLPEvaluator,
        H::AbstractVector{T},
        x::AbstractVector{T},
        σ::T,
        μ::AbstractVector{T},
    )::Nothing where {T}

Given scalar weight `σ` and vector of constraint weights `μ`, this function
computes the sparse Hessian-of-the-Lagrangian matrix:
``\\sigma\\nabla^2 f(x) + \\sum_{i=1}^m \\mu_i \\nabla^2 g_i(x)``,
storing the result in the vector `H` in the same order as the indices
returned by [`hessian_lagrangian_structure`](@ref).

## Implementation notes

When implementing this method, you must not assume that `H` is
`Vector{Float64}`, but you may assume that it supports `setindex!` and `length`.
For example, it may be the `view` of a vector.

## Initialize

Before calling this function, you must call [`initialize`](@ref) with `:Hess`.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, Symbol[:Hess])

julia> indices = MOI.hessian_lagrangian_structure(evaluator);

julia> H = zeros(length(indices));

julia> x = [1.0, 2.0, 3.0, 4.0];

julia> σ = 1.0;

julia> μ = [1.0, 1.0];

julia> MOI.eval_hessian_lagrangian(evaluator, H, x, σ, μ)

julia> H
10-element Vector{Float64}:
 10.0
 16.0
  2.0
 12.0
  4.0
  2.0
 13.0
  4.0
  3.0
  2.0
```
"""
function eval_hessian_lagrangian end

"""
    objective_expr(d::AbstractNLPEvaluator)::Expr

Returns a Julia `Expr` object representing the expression graph of the objective
function.

## Format

The expression has a number of limitations, compared with arbitrary Julia
expressions:

 * All sums and products are flattened out as simple `Expr(:+, ...)` and
   `Expr(:*, ...)` objects.
 * All decision variables must be of the form
   `Expr(:ref, :x, MOI.VariableIndex(i))`, where `i` is the ``i``th variable in
   [`ListOfVariableIndices`](@ref).
 * There are currently no restrictions on recognized functions; typically these
   will be built-in Julia functions like `^`, `exp`, `log`, `cos`, `tan`, `sqrt`,
   etc., but modeling interfaces may choose to extend these basic functions, or
   error if they encounter unsupported functions.

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, [:ExprGraph])

julia> MOI.objective_expr(evaluator)
:(x[MOI.VariableIndex(1)] * x[MOI.VariableIndex(4)] * (x[MOI.VariableIndex(1)] + x[MOI.VariableIndex(2)] + x[MOI.VariableIndex(3)]) + x[MOI.VariableIndex(3)])
```
"""
function objective_expr end

"""
    constraint_expr(d::AbstractNLPEvaluator, i::Integer)::Expr

Returns a Julia `Expr` object representing the expression graph for the
``i\\text{th}`` nonlinear constraint.

## Format

The format is the same as [`objective_expr`](@ref), with an additional
comparison operator indicating the sense of and bounds on the constraint.

For single-sided comparisons, the body of the constraint must be on the
left-hand side, and the right-hand side must be a constant.

For double-sided comparisons (that is, ``l \\le f(x) \\le u``), the body of the
constraint must be in the middle, and the left- and right-hand sides must be
constants.

The bounds on the constraints must match the [`NLPBoundsPair`](@ref)s passed to
[`NLPBlockData`](@ref).

## Example

```jldoctest
julia> evaluator = MOI.Test.HS071(true);

julia> MOI.initialize(evaluator, [:ExprGraph])

julia> MOI.constraint_expr(evaluator, 1)
:(x[MOI.VariableIndex(1)] * x[MOI.VariableIndex(2)] * x[MOI.VariableIndex(3)] * x[MOI.VariableIndex(4)] >= 25.0)

julia> MOI.constraint_expr(evaluator, 2)
:(x[MOI.VariableIndex(1)] ^ 2 + x[MOI.VariableIndex(2)] ^ 2 + x[MOI.VariableIndex(3)] ^ 2 + x[MOI.VariableIndex(4)] ^ 2 == 40.0)
```
"""
function constraint_expr end
