# This API is imported from MathProgBase and is meant to be transitional.
# A more ideal API would pass expressions (with vector-valued nodes) directly
# let the solver call out to AD tools.

# NLP = Nonlinear Programming.
# The non-MOI-like name is intentional because this part of the API is not
# MOI-like.

"""
    AbstractNLPEvaluator

Abstract supertype for the callback object used in `NLPBlock`.
"""
abstract type AbstractNLPEvaluator end

"""
    NLPBlock()

Holds the `NLPBlockData` that represents a set of nonlinear constraints, and
optionally a nonlinear objective.
"""
struct NLPBlock <: AbstractModelAttribute end

"""
    NLPBlockDual(N)
    NLPBlockDual()

The Lagrange multipliers on the constraints from the `NLPBlock` in result `N`.
If `N` is omitted, it is 1 by default.
"""
struct NLPBlockDual <: AbstractModelAttribute
    N::Int
end
NLPBlockDual() = NLPBlockDual(1)

"""
    NLPBlockDualStart()

An initial assignment of the Lagrange multipliers on the constraints from the
`NLPBlock` that the solver may use to warm-start the solve.
"""
struct NLPBlockDualStart <: AbstractModelAttribute end

"""
    NLPBoundsPair(lower,upper)

A struct holding a pair of lower and upper bounds. `-Inf` and `Inf`
can be used to indicate no lower or upper bound, respectively.
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

A `struct` encoding a set of nonlinear constraints of the form ``lb \\le g(x)
\\le ub`` and, if `has_objective == true`, a nonlinear objective function
``f(x)``. `constraint_bounds` holds the pairs of ``lb`` and ``ub`` elements. It
is an error to set both a nonlinear objective function and another objective
function using an `ObjectiveFunction` attribute. The `evaluator` is a callback
object that is used to query function values, derivatives, and expression
graphs. If `has_objective == false`, then it is an error to query properties of
the objective function, and in Hessian-of-the-Lagrangian queries, `σ` must be
set to zero. Throughout the evaluator, all variables are ordered according to
ListOfVariableIndices(). """
struct NLPBlockData
    constraint_bounds::Vector{NLPBoundsPair}
    evaluator::AbstractNLPEvaluator
    has_objective::Bool
end

"""
    initialize(d::AbstractNLPEvaluator, requested_features::Vector{Symbol})

Must be called before any other methods. The vector `requested_features`
lists features requested by the solver. These may include `:Grad` for gradients
of ``f``, `:Jac` for explicit Jacobians of ``g``, `:JacVec` for
Jacobian-vector products, `:HessVec` for Hessian-vector
and Hessian-of-Lagrangian-vector products, `:Hess` for explicit Hessians and
Hessian-of-Lagrangians, and `:ExprGraph` for expression graphs.
"""
function initialize end

"""
    features_available(d::AbstractNLPEvaluator)

Returns the subset of features available for this problem instance, as a
list of symbols in the same format as in `initialize`.
"""
function features_available end

"""
    eval_objective(d::AbstractNLPEvaluator, x)

Evaluate the objective ``f(x)``, returning a scalar value.
"""
function eval_objective end

"""
    eval_constraint(d::AbstractNLPEvaluator, g, x)

Evaluate the constraint function ``g(x)``, storing the result in the vector `g` which must be of the
appropriate size.
"""
function eval_constraint end

"""
    eval_objective_gradient(d::AbstractNLPEvaluator, g, x)

Evaluate ``\\nabla f(x)`` as a dense vector, storing the result in the vector
`g` which must be of the appropriate size.
"""
function eval_objective_gradient end

"""
    jacobian_structure(d::AbstractNLPEvaluator)::Vector{Tuple{Int64,Int64}}

Returns the sparsity structure of the Jacobian matrix
``J_g(x) = \\left[ \\begin{array}{c} \\nabla g_1(x) \\\\ \\nabla g_2(x) \\\\ \\vdots \\\\ \\nabla g_m(x) \\end{array}\\right]``
where ``g_i`` is the ``i\\text{th}`` component of ``g``. The sparsity structure
is assumed to be independent of the point ``x``. Returns a vector of tuples,
`(row, column)`, where each indicates the position of a structurally nonzero element.
These indices are not required to be sorted and can contain duplicates, in which
case the solver should combine the corresponding elements by adding them together.
"""
function jacobian_structure end

"""
    hessian_lagrangian_structure(d::AbstractNLPEvaluator)::Vector{Tuple{Int64,Int64}}

Returns the sparsity structure of the Hessian-of-the-Lagrangian matrix
``\\nabla^2 f + \\sum_{i=1}^m \\nabla^2 g_i`` as a vector of tuples, where
each indicates the position of a structurally nonzero element. These indices are
not required to be sorted and can contain duplicates, in which case the solver
should combine the corresponding elements by adding them together. Any mix of
lower and upper-triangular indices is valid. Elements `(i,j)` and
`(j,i)`, if both present, should be treated as duplicates.
"""
function hessian_lagrangian_structure end

"""
    eval_constraint_jacobian(d::AbstractNLPEvaluator, J, x)

Evaluates the sparse Jacobian matrix
``J_g(x) = \\left[ \\begin{array}{c} \\nabla g_1(x) \\\\ \\nabla g_2(x) \\\\ \\vdots \\\\ \\nabla g_m(x) \\end{array}\\right]``.
The result is stored in the vector `J` in the same order as the indices returned
by `jacobian_structure`.
"""
function eval_constraint_jacobian end

"""
    eval_constraint_jacobian_product(d::AbstractNLPEvaluator, y, x, w)

Computes the Jacobian-vector product ``J_g(x)w``, storing the result in the vector `y`.
"""
function eval_constraint_jacobian_product end

"""
    eval_constraint_jacobian_transpose_product(d::AbstractNLPEvaluator, y, x, w)

Computes the Jacobian-transpose-vector product ``J_g(x)^Tw``, storing the result
in the vector `y`.
"""
function eval_constraint_jacobian_transpose_product end

"""
    eval_hessian_lagrangian_prod(d::AbstractNLPEvaluator, h, x, v, σ, μ)

Given scalar weight `σ` and vector of constraint weights `μ`,
computes the Hessian-of-the-Lagrangian-vector product
``\\left(\\sigma\\nabla^2 f(x) + \\sum_{i=1}^m \\mu_i \\nabla^2 g_i(x)\\right)v``,
storing the result in the vector `h`.
"""
function eval_hessian_lagrangian_product end

"""
    eval_hessian_lagrangian(d::AbstractNLPEvaluator, H, x, σ, μ)

Given scalar weight `σ` and vector of constraint weights `μ`,
computes the sparse Hessian-of-the-Lagrangian matrix
``\\sigma\\nabla^2 f(x) + \\sum_{i=1}^m \\mu_i \\nabla^2 g_i(x)``,
storing the result in the vector `H` in the same order as the indices
returned by `hessian_lagrangian_structure`.
"""
function eval_hessian_lagrangian end

"""
    objective_expr(d::AbstractNLPEvaluator)

Returns an expression graph for the objective function as a standard Julia `Expr`
object. All sums and products are flattened out as simple `Expr(:+,...)` and
`Expr(:*,...)` objects. The symbol `x` is used as a placeholder for the
vector of decision variables. No other undefined symbols are permitted;
coefficients are embedded as explicit values. For example, the expression
``x_1+\\sin(x_2/\\exp(x_3))`` would be represented as the Julia object
`:(x[1] + sin(x[2]/exp(x[3])))`. See the
[Julia manual](https://docs.julialang.org/en/release-0.6/manual/metaprogramming/) for more information
on the structure of `Expr` objects. There are currently no restrictions on
recognized functions; typically these will be built-in Julia functions like
`^`, `exp`, `log`, `cos`, `tan`, `sqrt`, etc., but modeling
interfaces may choose to extend these basic functions.
"""
function objective_expr end

"""
    constraint_expr(d::AbstractNLPEvaluator, i)

Returns an expression graph for the ``i\\text{th}`` constraint in the same
format as described above, with an additional comparison operator indicating
the sense of and bounds on the constraint. The right-hand side of the comparison
must be a constant; that is, `:(x[1]^3 <= 1)` is allowed, while
`:(1 <= x[1]^3)` is not valid. Double-sided constraints are allowed, in which
case both the lower bound and upper bounds should be constants; for example,
`:(-1 <= cos(x[1]) + sin(x[2]) <= 1)` is valid.
"""
function constraint_expr end
