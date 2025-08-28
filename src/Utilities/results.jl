# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# This file contains the implementation of different methods for the
# `get_fallback` function. These methods can be used by solver wrappers as
# fallbacks for implementing the `get` method when the solver API does not
# provide the required result. For instance, if the solver does not provide the
# value of the constraints, the solver wrapper can write
# ```julia
# function MOI.get(model::Optimizer, attr::MOI.ConstraintPrimal,
#                  ci::MOI.ConstraintIndex)
#     return MOIU.get_fallback(model, attr, ci)
# end
# ```

"""
    is_ray(status::MOI.ResultStatusCode)

Returnn `true` if `status` is `INFEASIBILITY_CERTIFICATE` or
    `NEARLY_INFEASIBILITY_CERTIFICATE`.
"""
function is_ray(status::MOI.ResultStatusCode)
    return status == MOI.INFEASIBILITY_CERTIFICATE ||
           status == MOI.NEARLY_INFEASIBILITY_CERTIFICATE
end

# MOI.ObjectiveValue

"""
    get_fallback(model::MOI.ModelLike, ::MOI.ObjectiveValue)

Compute the objective function value using the `VariablePrimal` results and
the `ObjectiveFunction` value.
"""
function get_fallback(model::MOI.ModelLike, attr::MOI.ObjectiveValue)
    MOI.check_result_index_bounds(model, attr)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    obj = eval_variables(model, f) do vi
        return MOI.get(model, MOI.VariablePrimal(attr.result_index), vi)
    end
    if is_ray(MOI.get(model, MOI.PrimalStatus()))
        # Dual infeasibility certificates do not include the primal
        # objective constant.
        obj -= MOI.constant(f, typeof(obj))
    end
    return obj
end

# MOI.DualObjectiveValue

"""
    get_fallback(
        model::MOI.ModelLike,
        ::MOI.DualObjectiveValue,
        ::Type{T},
    )::T where {T}

Compute the dual objective value of type `T` using the `ConstraintDual` results
and the `ConstraintFunction` and `ConstraintSet` values.

Note that the nonlinear part of the model is ignored.
"""
function get_fallback(
    model::MOI.ModelLike,
    attr::MOI.DualObjectiveValue,
    ::Type{T},
)::T where {T}
    MOI.check_result_index_bounds(model, attr)
    value = zero(T) # sum will not work if there are zero constraints
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        value += _dual_objective_value(model, F, S, T, attr.result_index)::T
    end
    if MOI.get(model, MOI.ObjectiveSense()) != MOI.MAX_SENSE
        value = -value
    end
    if !is_ray(MOI.get(model, MOI.DualStatus()))
        # The objective constant should not be present in rays
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        f = MOI.get(model, MOI.ObjectiveFunction{F}())
        value += MOI.constant(f, T)
    end
    return value::T
end

function _dual_objective_value(
    model::MOI.ModelLike,
    ::Type{F},
    ::Type{S},
    ::Type{T},
    result_index::Integer,
)::T where {T,F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    value = zero(T)
    if F == variable_function_type(S) && !_variable_set_in_dual_objective(S)
        # Early return. This is a constraint like x in R_+, so no contribution
        # appears in the dual objective.
        return value
    end
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        constant = MOI.constant(MOI.get(model, MOI.ConstraintFunction(), ci), T)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        dual = MOI.get(model, MOI.ConstraintDual(result_index), ci)
        value += _dual_objective_dot(constant, dual, set)
    end
    return value
end

_variable_set_in_dual_objective(::Type{<:MOI.AbstractSet}) = false

_variable_set_in_dual_objective(::Type{<:MOI.EqualTo}) = true

_variable_set_in_dual_objective(::Type{<:MOI.GreaterThan}) = true

_variable_set_in_dual_objective(::Type{<:MOI.LessThan}) = true

_variable_set_in_dual_objective(::Type{<:MOI.Interval}) = true

_variable_set_in_dual_objective(::Type{<:MOI.HyperRectangle}) = true

_dual_objective_dot(x, y, set) = set_dot(x, y, set)

_dual_objective_dot(x, y, set::MOI.EqualTo) = (x - set.value) * y

_dual_objective_dot(x, y, set::MOI.LessThan) = (x - set.upper) * y

_dual_objective_dot(x, y, set::MOI.GreaterThan) = (x - set.lower) * y

function _dual_objective_dot(x, y, set::MOI.Interval)
    if isfinite(set.lower) && (!isfinite(set.upper) || y > zero(y))
        return (x - set.lower) * y
    elseif isfinite(set.upper) && (!isfinite(set.lower) || y < zero(y))
        return (x - set.upper) * y
    end
    return x * y
end

function _dual_objective_dot(x, y, set::MOI.HyperRectangle)
    @assert length(x) == length(y) == MOI.dimension(set)
    ret = zero(eltype(x))
    for (xi, yi, li, ui) in zip(x, y, set.lower, set.upper)
        if isfinite(li) && (!isfinite(ui) || yi > zero(yi))
            ret += (xi - li) * yi
        elseif isfinite(ui) && (!isfinite(li) || yi < zero(yi))
            ret += (xi - ui) * yi
        else
            ret += xi * yi
        end
    end
    return ret
end

# MOI.ConstraintPrimal

"""
    get_fallback(
        model::MOI.ModelLike,
        ::MOI.ConstraintPrimal,
        constraint_index::MOI.ConstraintIndex,
    )

Compute the value of the function of the constraint of index `constraint_index`
using the `VariablePrimal` results and the `ConstraintFunction` values.
"""
function get_fallback(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    idx::MOI.ConstraintIndex,
)
    MOI.check_result_index_bounds(model, attr)
    # If there is an error getting ConstraintFunction, we instead want to
    # re-throw the attribute for ConstraintPrimal, not ConstraintFunction.
    f = MOI.get(model, MOI.ConstraintFunction(), idx)
    c = eval_variables(model, f) do vi
        return MOI.get(model, MOI.VariablePrimal(attr.result_index), vi)
    end
    if is_ray(MOI.get(model, MOI.PrimalStatus()))
        c -= MOI.constant(f, typeof(c))
    end
    return c
end

# MOI.ConstraintDual

# In the primal we have
#
#   min a_0' x + b_0
#       A_i  x + b_i in C_i for all i
#
# In the dual we have
#
#   max b_0 - sum b_i' y
#       a_0 - sum A_i* y_i = 0
#                      y_i in C_i* for all i
#
# where A_i* is the adjoint operator of the linear operator A_i. That is, A*
# is the linear operator such that
#
#   ⟨A x, y⟩_{C_i} = ⟨x, A* y⟩_Rn
#
# where
#
# * ⟨., .⟩_Rn is the standard scalar product over Rn: ⟨., .⟩_Rn and
# * ⟨., .⟩_{C_i} is the scalar product `set_dot` defined for the set C_i
#
# Suppose we want to get the constraint variable of a variable-wise constraint:
#
#   A_j x in C_j
#
# where A_j is zero except on a submatrix which is the identity.
#
# We have
#
#   A_j* y_j = a_0 - sum_(i != j) A_i* y_i
#
# Thus to get the dual y_j, we simply have to compute the right-hand side and
# then invert A_j*.
#
# To get the kth element of A_i* y_i we need to compute
#
#   ⟨e_k, A_i* y_i⟩_Rn = ⟨A_i e_k, y_i⟩_{C_i}.
#
# A_i e_k is computed using `_variable_coefficient` and then it is combined with
# the dual y_i with `set_dot`.
#
# Once A_j* y_j is obtained, we invert A_j* with `dot_coefficients`.

function _variable_coefficient(
    func::MOI.ScalarAffineFunction{T},
    vi::MOI.VariableIndex,
    value_fn::Any,
) where {T}
    coef = zero(T)
    for term in func.terms
        if term.variable == vi
            coef += term.coefficient
        end
    end
    return coef
end

function _variable_coefficient(
    func::MOI.VectorAffineFunction{T},
    vi::MOI.VariableIndex,
    value_fn::Any,
) where {T}
    coef = zeros(T, MOI.output_dimension(func))
    for term in func.terms
        if term.scalar_term.variable == vi
            coef[term.output_index] += term.scalar_term.coefficient
        end
    end
    return coef
end

function _variable_coefficient(
    func::MOI.ScalarQuadraticFunction{T},
    vi::MOI.VariableIndex,
    value_fn::F,
) where {T,F<:Function}
    coef = zero(T)
    # `vi`'th row of `Qx + a` where `func` is `x'Qx/2 + a'x + b`.
    for term in func.affine_terms
        if term.variable == vi
            coef += term.coefficient
        end
    end
    for term in func.quadratic_terms
        if term.variable_1 == vi
            coef += term.coefficient * value_fn(term.variable_2)
        elseif term.variable_2 == vi
            coef += term.coefficient * value_fn(term.variable_1)
        end
    end
    return coef
end

function _variable_coefficient(
    func::MOI.VectorQuadraticFunction{T},
    vi::MOI.VariableIndex,
    value_fn::F,
) where {T,F<:Function}
    coef = zeros(T, MOI.output_dimension(func))
    # `vi`'th row of `Qx + a` where `func` is `x'Qx/2 + a'x + b`.
    for aff_term in func.affine_terms
        if aff_term.scalar_term.variable == vi
            coef[aff_term.output_index] += aff_term.scalar_term.coefficient
        end
    end
    for q_term in func.quadratic_terms
        index, term = q_term.output_index, q_term.scalar_term
        if term.variable_1 == vi
            coef[index] += term.coefficient * value_fn(term.variable_2)
        elseif q_term.scalar_term.variable_2 == vi
            coef[index] += term.coefficient * value_fn(term.variable_1)
        end
    end
    return coef
end

function _variable_dual(
    ::Type{T},
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{
        <:Union{
            MOI.ScalarAffineFunction,
            MOI.ScalarQuadraticFunction,
            MOI.VectorAffineFunction,
            MOI.VectorQuadraticFunction,
        },
    },
) where {T}
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    primal = MOI.VariablePrimal(attr.result_index)
    coef = _variable_coefficient(func, vi, vi -> MOI.get(model, primal, vi))
    dual = MOI.get(model, attr, ci)
    return set_dot(coef, dual, set)
end

function _variable_dual(
    ::Type{T},
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
    vi::MOI.VariableIndex,
    ::Type{F},
    ::Type{S},
) where {T,F<:MOI.AbstractFunction,S<:MOI.AbstractSet}
    dual = zero(T)
    for constraint_index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        dual += _variable_dual(T, model, attr, vi, constraint_index)
    end
    return dual
end

function _variable_dual(
    ::Type{T},
    model::MOI.ModelLike,
    ::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
    vi::MOI.VariableIndex,
    ::Type{F},
    ::Type{S},
) where {T,F<:Union{MOI.VariableIndex,MOI.VectorOfVariables},S<:MOI.AbstractSet}
    for constraint_index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        if constraint_index == ci
            continue
        end
        func = MOI.get(model, MOI.ConstraintFunction(), constraint_index)
        if (F == MOI.VariableIndex && func == vi) ||
           (F == MOI.VectorOfVariables && vi in func.variables)
            error(
                "Fallback getter for variable constraint dual does not",
                " support other variable-wise constraints on the variable.",
                " Please report this issue to the solver wrapper package.",
            )
        end
    end
    return zero(T)
end

function _variable_dual(
    ::Type{T},
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
    vi::MOI.VariableIndex,
) where {T}
    ray = is_ray(MOI.get(model, MOI.DualStatus()))
    dual = zero(T)
    if !ray
        sense = MOI.get(model, MOI.ObjectiveSense())
        # Dual definition for maximization problem corresponds to dual
        # definition for minimization problem with flipped objective in MOI
        sign = sense == MOI.MAX_SENSE ? T(-1) : T(1)
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        obj_attr = MOI.ObjectiveFunction{F}()
        if F == MOI.VariableIndex
            if MOI.get(model, obj_attr) == vi
                dual += sign
            end
        elseif F <: MOI.ScalarAffineFunction
            f = MOI.get(model, obj_attr)
            dual += sign * _variable_coefficient(f, vi, nothing)
        elseif F <: MOI.ScalarQuadraticFunction
            f = MOI.get(model, obj_attr)
            primal_attr = MOI.VariablePrimal(attr.result_index)
            dual +=
                sign * _variable_coefficient(
                    f,
                    vi,
                    vi -> MOI.get(model, primal_attr, vi),
                )
        else
            error(
                "Fallback getter for variable constraint dual does not",
                " support objective function of type $F.",
                " Please report this issue to the solver wrapper package.",
            )
        end
    end
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        dual -= _variable_dual(T, model, attr, ci, vi, F, S)
    end
    return dual
end

function _variable_dual(
    ::Type{T},
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
    func::MOI.VectorOfVariables,
) where {T}
    dual = map(vi -> _variable_dual(T, model, attr, ci, vi), func.variables)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    return dot_coefficients(dual, set)
end

"""
    get_fallback(
        model::MOI.ModelLike,
        attr::MOI.ConstraintDual,
        ci::MOI.ConstraintIndex{Union{MOI.VariableIndex,MOI.VectorOfVariables}},
        ::Type{T} = Float64,
    ) where {T}

Compute the dual of the constraint of index `ci` using the `ConstraintDual` of
other constraints and the `ConstraintFunction` values.

Throws an error if some constraints are quadratic or if there is one another
`MOI.VariableIndex`-in-`S` or `MOI.VectorOfVariables`-in-`S` constraint with one
of the variables in the function of the constraint `ci`.
"""
function get_fallback(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{<:Union{MOI.VariableIndex,MOI.VectorOfVariables}},
    ::Type{T} = Float64,
) where {T}
    MOI.check_result_index_bounds(model, attr)
    f = MOI.get(model, MOI.ConstraintFunction(), ci)
    return _variable_dual(T, model, attr, ci, f)
end
