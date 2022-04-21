"""
    NodeType

An enum describing the possible node types. Each [`Node`](@ref) has a `.index`
field, which should be interpreted as follows:

 * `NODE_CALL_MULTIVARIATE`: the index into `operators.multivariate_operators`
 * `NODE_CALL_UNIVARIATE`: the index into `operators.univariate_operators`
 * `NODE_LOGIC`: the index into `operators.logic_operators`
 * `NODE_COMPARISON`: the index into `operators.comparison_operators`
 * `NODE_MOI_VARIABLE`: the value of `MOI.VariableIndex(index)` in the user's
   space of the model.
 * `NODE_VARIABLE`: the 1-based index of the internal vector
 * `NODE_VALUE`: the index into the `.values` field of `NonlinearExpression`
 * `NODE_PARAMETER`: the index into `data.parameters`
 * `NODE_SUBEXPRESSION`:  the index into `data.expressions`
"""
@enum(
    NodeType,
    # Index into multivariate operators
    NODE_CALL_MULTIVARIATE,
    # Index into univariate operators
    NODE_CALL_UNIVARIATE,
    # Index into logic operators
    NODE_LOGIC,
    # Index into comparison operators
    NODE_COMPARISON,
    # Index is the value of `MOI.VariableIndex`. This is from the original
    # model, and is not consecutive.
    NODE_MOI_VARIABLE,
    # Index of the internal, consecutive, and ordered `MOI.VariableIndex`.
    NODE_VARIABLE,
    # Index is into the list of constants
    NODE_VALUE,
    # Index is into the list of parameters
    NODE_PARAMETER,
    # Index is into the list of subexpressions
    NODE_SUBEXPRESSION,
)

"""
    struct Node
        type::NodeType
        index::Int
        parent::Int
    end

A single node in a nonlinear expression tree. Used by
[`NonlinearExpression`](@ref).

See the MathOptInterface documentation for information on how the nodes and
values form an expression tree.
"""
struct Node
    type::NodeType
    index::Int
    parent::Int
end

"""
    struct NonlinearExpression
        nodes::Vector{Node}
        values::Vector{Float64}
    end

The core type that represents a nonlinear expression. See the MathOptInterface
documentation for information on how the nodes and values form an expression
tree.
"""
struct NonlinearExpression
    nodes::Vector{Node}
    values::Vector{Float64}
    NonlinearExpression() = new(Node[], Float64[])
end

function Base.:(==)(x::NonlinearExpression, y::NonlinearExpression)
    return x.nodes == y.nodes && x.values == y.values
end

"""
    struct NonlinearConstraint
        expression::NonlinearExpression
        set::Union{
            MOI.LessThan{Float64},
            MOI.GreaterThan{Float64},
            MOI.EqualTo{Float64},
            MOI.Interval{Float64},
        }
    end

A type to hold information relating to the nonlinear constraint `f(x) in S`,
where `f(x)` is defined by `.expression`, and `S` is `.set`.
"""
struct NonlinearConstraint
    expression::NonlinearExpression
    set::Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    }
end

"""
    ParameterIndex

An index to a nonlinear parameter that is returned by [`add_parameter`](@ref).
Given `data::NonlinearData` and `p::ParameterIndex`, use `data[p]` to retrieve
the current value of the parameter and `data[p] = value` to set a new value.
"""
struct ParameterIndex
    value::Int
end

"""
    ExpressionIndex

An index to a nonlinear expression that is returned by [`add_expression`](@ref).

Given `data::NonlinearData` and `ex::ExpressionIndex`, use `data[ex]` to
retrieve the corresponding [`NonlinearExpression`](@ref).
"""
struct ExpressionIndex
    value::Int
end

"""
    ConstraintIndex

An index to a nonlinear constraint that is returned by [`add_constraint`](@ref).

Given `data::NonlinearData` and `c::ConstraintIndex`, use `data[c]` to
retrieve the corresponding [`NonlinearConstraint`](@ref).
"""
struct ConstraintIndex
    value::Int
end

"""
    NonlinearData()

Construct a new [`MOI.AbstractNLPEvaluator`](@ref) object.
"""
mutable struct NonlinearData <: MOI.AbstractNLPEvaluator
    inner::Union{Nothing,MOI.AbstractNLPEvaluator}
    objective::Union{Nothing,NonlinearExpression}
    expressions::Vector{NonlinearExpression}
    constraints::OrderedDict{ConstraintIndex,NonlinearConstraint}
    parameters::Vector{Float64}
    operators::OperatorRegistry
    last_constraint_index::Int64
    # Fields for initialize
    julia_expressions::Vector{Any}  # Any because expressions may be constants
    # ordered_constraints is needed because `OrderedDict` doesn't support
    # looking up a key by the linear index.
    ordered_constraints::Vector{ConstraintIndex}
    constraint_dual::Vector{Float64}
    # Timers
    eval_objective_timer::Float64
    eval_constraint_timer::Float64
    eval_objective_gradient_timer::Float64
    eval_constraint_jacobian_timer::Float64
    eval_hessian_lagrangian_timer::Float64

    function NonlinearData()
        return new(
            nothing,
            nothing,
            NonlinearExpression[],
            OrderedDict{ConstraintIndex,NonlinearConstraint}(),
            Float64[],
            OperatorRegistry(),
            0,
            Expr[],
            ConstraintIndex[],
            Float64[],
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
        )
    end
end

_bound(s::MOI.LessThan) = MOI.NLPBoundsPair(-Inf, s.upper)
_bound(s::MOI.GreaterThan) = MOI.NLPBoundsPair(s.lower, Inf)
_bound(s::MOI.EqualTo) = MOI.NLPBoundsPair(s.value, s.value)
_bound(s::MOI.Interval) = MOI.NLPBoundsPair(s.lower, s.upper)

"""
    MOI.NLPBlockData(data::NonlinearData)

Create an [`MOI.NLPBlockData`](@ref) object from a [`NonlinearData`](@ref)
object.
"""
function MOI.NLPBlockData(data::NonlinearData)
    return MOI.NLPBlockData(
        [_bound(c.set) for (_, c) in data.constraints],
        data,
        data.objective !== nothing,
    )
end

"""
    AbstractAutomaticDifferentiation

An abstract type for extending
"""
abstract type AbstractAutomaticDifferentiation end

"""
    set_differentiation_backend(
        data::NonlinearData,
        backend::AbstractAutomaticDifferentiation,
        ordered_variables::Vector{MOI.VariableIndex}
    )

Set the automatic differentiation backend of `data` to `backend`.

`ordered_variables` is an ordered vector containing all of the variables that
are present in the model. This order corresponds to the order of the primal
solution vector `x` that is passed to the various functions in MOI's nonlinear
API.
"""
function set_differentiation_backend end

"""
    ExprGraphOnly() <: AbstractAutomaticDifferentiation

The default implementation of `AbstractAutomaticDifferentiation`. This only
supported `:ExprGraph`.
"""
struct ExprGraphOnly <: AbstractAutomaticDifferentiation end

function set_differentiation_backend(
    data::NonlinearData,
    ::ExprGraphOnly,
    ::Vector{MOI.VariableIndex},
)
    data.inner = nothing
    return
end
