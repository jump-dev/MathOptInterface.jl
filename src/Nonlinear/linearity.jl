# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    Linearity

An enum describing the linearity of an expression with respect to the decision
variables, for fixed values of the parameters.

The classification is conservative: an expression may be classified less
strictly than the tightest class that applies (for example, an expression that
simplifies to an affine function may be classified as `NONLINEAR`), but never
more strictly.

## Values

 * `CONSTANT`: the value does not depend on the decision variables
 * `LINEAR`: the value is an affine function of the decision variables; the
   gradient is constant and the Hessian is zero
 * `PIECEWISE_LINEAR`: the gradient is piecewise constant and the Hessian is
   zero almost everywhere
 * `QUADRATIC`: the gradient is an affine function of the decision variables
   and the Hessian is constant
 * `NONLINEAR`: no guarantee

Because parameters are held fixed by this classification, consumers that cache
constant derivatives (for example, the Jacobian coefficients of `LINEAR` rows)
must invalidate their cache when parameter values change.
"""
const Linearity = ReverseAD.Linearity

const CONSTANT = ReverseAD.CONSTANT
const LINEAR = ReverseAD.LINEAR
const PIECEWISE_LINEAR = ReverseAD.PIECEWISE_LINEAR
const QUADRATIC = ReverseAD.QUADRATIC
const NONLINEAR = ReverseAD.NONLINEAR

"""
    num_constraints(evaluator::MOI.AbstractNLPEvaluator)::Int

Return the number of constraints in `evaluator`, that is, the length of the
vector `g` filled by [`MOI.eval_constraint`](@ref).

## Implementation

There is no default fallback: evaluators opt in to this query by adding a
method.
"""
function num_constraints end

num_constraints(evaluator::Evaluator) = length(evaluator.model.constraints)

num_constraints(d::ReverseAD.NLPEvaluator) = length(d.data.constraints)

"""
    constraint_linearity(
        evaluator::MOI.AbstractNLPEvaluator,
    )::Union{Nothing,Vector{Linearity}}

Return a vector of the [`Linearity`](@ref) of each constraint in `evaluator`,
aligned with the rows of [`MOI.eval_constraint`](@ref), or `nothing` if the
evaluator does not implement this query.

Callers must treat `nothing` as if every row were `NONLINEAR`.

The length of a non-`nothing` return value is [`num_constraints`](@ref).

## Initialize

Before querying this function, you must call [`MOI.initialize`](@ref).
"""
constraint_linearity(::MOI.AbstractNLPEvaluator) = nothing

function constraint_linearity(evaluator::Evaluator)
    if evaluator.backend === nothing
        return nothing
    end
    return constraint_linearity(evaluator.backend)
end

function constraint_linearity(d::ReverseAD.NLPEvaluator)
    if !isdefined(d, :constraints)
        error(
            "Unable to query constraint_linearity because MOI.initialize " *
            "has not been called.",
        )
    end
    return Linearity[c.linearity for c in d.constraints]
end

"""
    objective_linearity(evaluator::MOI.AbstractNLPEvaluator)::Linearity

Return the [`Linearity`](@ref) of the objective function in `evaluator`.

The default fallback returns `NONLINEAR`, which is always a valid (if
conservative) answer. If the evaluator has no objective, return `CONSTANT`.

## Initialize

Before querying this function, you must call [`MOI.initialize`](@ref).
"""
objective_linearity(::MOI.AbstractNLPEvaluator) = NONLINEAR

function objective_linearity(evaluator::Evaluator)
    if evaluator.model.objective === nothing
        return CONSTANT
    elseif evaluator.backend === nothing
        return NONLINEAR
    end
    return objective_linearity(evaluator.backend)
end

function objective_linearity(d::ReverseAD.NLPEvaluator)
    if !isdefined(d, :objective)
        error(
            "Unable to query objective_linearity because MOI.initialize " *
            "has not been called.",
        )
    end
    if d.objective === nothing
        return CONSTANT
    end
    return something(d.objective).linearity
end
