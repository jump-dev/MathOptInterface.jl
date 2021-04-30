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
    get_fallback(model::MOI.ModelLike, ::MOI.ObjectiveValue)

Compute the objective function value using the `VariablePrimal` results and
the `ObjectiveFunction` value.
"""
function get_fallback(model::MOI.ModelLike, attr::MOI.ObjectiveValue)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    # TODO do not include constant if primal solution is a ray
    return eval_variables(
        vi -> MOI.get(model, MOI.VariablePrimal(attr.result_index), vi),
        f,
    )
end

function constraint_constant(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{
        <:MOI.AbstractVectorFunction,
        <:MOI.AbstractVectorSet,
    },
    T::Type,
)
    return MOI.constant(MOI.get(model, MOI.ConstraintFunction(), ci))
end
function constraint_constant(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{
        <:MOI.AbstractScalarFunction,
        <:MOI.AbstractScalarSet,
    },
    T::Type,
)
    return MOI.constant(MOI.get(model, MOI.ConstraintFunction(), ci), T) -
           MOI.constant(MOI.get(model, MOI.ConstraintSet(), ci))
end

"""
    dual_objective_value(model::MOI.ModelLike,
                         F::Type{<:MOI.AbstractFunction},
                         S::Type{<:MOI.AbstractSet},
                         T::Type,
                         result_index::Integer)

Return the part of `DualObjectiveValue` due to the constraint of index `ci` using
scalar type `T`.
"""
function dual_objective_value(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex,
    T::Type,
    result_index::Integer,
)
    return set_dot(
        constraint_constant(model, ci, T),
        MOI.get(model, MOI.ConstraintDual(result_index), ci),
        MOI.get(model, MOI.ConstraintSet(), ci),
    )
end

function dual_objective_value(
    model::MOI.ModelLike,
    ci::MOI.ConstraintIndex{<:MOI.AbstractScalarFunction,<:MOI.Interval},
    T::Type,
    result_index::Integer,
)
    constant = MOI.constant(MOI.get(model, MOI.ConstraintFunction(), ci), T)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    dual = MOI.get(model, MOI.ConstraintDual(result_index), ci)
    if dual < zero(dual)
        # The dual is negative so it is in the dual of the MOI.LessThan cone
        # hence the upper bound of the Interval set is tight
        constant -= set.upper
    else
        # the lower bound is tight
        constant -= set.lower
    end
    return set_dot(constant, dual, set)
end

"""
    dual_objective_value(model::MOI.ModelLike,
                    F::Type{<:MOI.AbstractFunction},
                    S::Type{<:MOI.AbstractSet},
                    T::Type,
                    result_index::Integer)

Return the part of `DualObjectiveValue` due to `F`-in-`S` constraints using scalar
type `T`.
"""
function dual_objective_value(
    model::MOI.ModelLike,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
    T::Type,
    result_index::Integer,
)
    value = zero(T) # sum won't work if there are no constraints.
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        value += dual_objective_value(model, ci, T, result_index)
    end
    return value
end

function dual_objective_value(
    model::MOI.ModelLike,
    F::Type{MOI.VectorOfVariables},
    S::Type{<:MOI.AbstractVectorSet},
    T::Type,
    result_index::Integer,
)
    # No constant in the function nor set so no contribution to the dual
    # objective value.
    return zero(T)
end

function is_ray(status::MOI.ResultStatusCode)
    return status == MOI.INFEASIBILITY_CERTIFICATE ||
           status == MOI.NEARLY_INFEASIBILITY_CERTIFICATE
end

"""
    get_fallback(model::MOI.ModelLike, ::MOI.DualObjectiveValue, T::Type)::T

Compute the dual objective value of type `T` using the `ConstraintDual` results
and the `ConstraintFunction` and `ConstraintSet` values. Note that the nonlinear
part of the model is ignored.
"""
function get_fallback(
    model::MOI.ModelLike,
    attr::MOI.DualObjectiveValue,
    T::Type,
)
    value = zero(T) # sum will not work if there are zero constraints
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        value += dual_objective_value(model, F, S, T, attr.result_index)::T
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

"""
    get_fallback(model::MOI.ModelLike, ::MOI.ConstraintPrimal,
                 constraint_index::MOI.ConstraintIndex)

Compute the value of the function of the constraint of index `constraint_index`
using the `VariablePrimal` results and the `ConstraintFunction` values.
"""
function get_fallback(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    idx::MOI.ConstraintIndex,
)
    f = MOI.get(model, MOI.ConstraintFunction(), idx)
    # TODO do not include constant if primal solution is a ray
    return eval_variables(
        vi -> MOI.get(model, MOI.VariablePrimal(attr.result_index), vi),
        f,
    )
end

################ Constraint Dual for Variable-wise constraints #################
#
# In the primal we have
#   min a_0' x + b_0
#       A_i  x + b_i in C_i for all i
# In the dual we have
#   max b_0 - sum b_i' y
#       a_0 - sum A_i* y_i = 0
#                      y_i in C_i* for all i
# where A_i* is the adjoint operator of the linear operator A_i. That is, A*
# is the linear operator such that
# ⟨A x, y⟩_{C_i} = ⟨x, A* y⟩_Rn
# where
# * ⟨., .⟩_Rn is the standard scalar product over Rn: ⟨., .⟩_Rn and
# * ⟨., .⟩_{C_i} is the scalar product `set_dot` defined for the set C_i
#
# Suppose we want to get the constraint variable of a variable-wise constraint:
#   A_j x in C_j
# where A_j is zero except on a submatrix which is the identity. We have
# A_j* y_j = a_0 - sum_(i != j) A_i* y_i
# Thus to get the dual y_j, we simply have to compute the right-hand side and
# then invert A_j*. To get the kth element of A_i* y_i we need to compute
# ⟨e_k, A_i* y_i⟩_Rn = ⟨A_i e_k, y_i⟩_{C_i}. A_i e_k is computed using
# `variable_coefficient` and then it is combined with the dual y_i with
# `set_dot`.
# Once A_j* y_j is obtained, we invert A_j* with `dot_coefficients`.

function variable_coefficient(
    func::MOI.ScalarAffineFunction{T},
    vi::MOI.VariableIndex,
) where {T}
    coef = zero(T)
    for term in func.terms
        if term.variable == vi
            coef += term.coefficient
        end
    end
    return coef
end
function variable_coefficient(
    func::MOI.VectorAffineFunction{T},
    vi::MOI.VariableIndex,
) where {T}
    coef = zeros(T, MOI.output_dimension(func))
    for vector_term in func.terms
        term = vector_term.scalar_term
        if term.variable == vi
            coef[vector_term.output_index] += term.coefficient
        end
    end
    return coef
end
function variable_coefficient(
    func::MOI.ScalarQuadraticFunction{T},
    vi::MOI.VariableIndex,
    value::F,
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
            coef += term.coefficient * value(term.variable_2)
        elseif term.variable_2 == vi
            coef += term.coefficient * value(term.variable_1)
        end
    end
    return coef
end
function variable_coefficient(
    func::MOI.VectorQuadraticFunction{T},
    vi::MOI.VariableIndex,
    value::F,
) where {T,F<:Function}
    coef = zeros(T, MOI.output_dimension(func))
    # `vi`'th row of `Qx + a` where `func` is `x'Qx/2 + a'x + b`.
    for vector_term in func.affine_terms
        term = vector_term.scalar_term
        if term.variable == vi
            coef[vector_term.output_index] += term.coefficient
        end
    end
    for vector_term in func.quadratic_terms
        term = vector_term.scalar_term
        oi = vector_term.output_index
        if term.variable_1 == vi
            coef[oi] += term.coefficient * value(term.variable_2)
        elseif term.variable_2 == vi
            coef[oi] += term.coefficient * value(term.variable_1)
        end
    end
    return coef
end

"""
    variable_dual(model::MOI.ModelLike,
                  attr::MOI.ConstraintDual,
                  vi::MOI.VariableIndex,
                  ci::MOI.ConstraintIndex{<:Union{MOI.ScalarAffineFunction,
                                                  MOI.VectorAffineFunction})

Return dual of the constraint of index `ci` multiplied by the coefficient of
`vi` in the `MOI.ConstraintFunction`.
"""
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{<:MOI.VectorAffineFunction},
)
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    coef = variable_coefficient(func, vi)
    dual = MOI.get(model, attr, ci)
    return set_dot(coef, dual, set)
end
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{<:MOI.VectorQuadraticFunction},
)
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    primal_attr = MOI.VariablePrimal(attr.result_index)
    coef = variable_coefficient(func, vi, vi -> MOI.get(model, primal_attr, vi))
    dual = MOI.get(model, attr, ci)
    return set_dot(coef, dual, set)
end
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction},
)
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    coef = variable_coefficient(func, vi)
    dual = MOI.get(model, attr, ci)
    return coef * dual
end
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    vi::MOI.VariableIndex,
    ci::MOI.ConstraintIndex{<:MOI.ScalarQuadraticFunction},
)
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    primal_attr = MOI.VariablePrimal(attr.result_index)
    coef = variable_coefficient(func, vi, vi -> MOI.get(model, primal_attr, vi))
    dual = MOI.get(model, attr, ci)
    return coef * dual
end

"""
    variable_dual(model::MOI.ModelLike,
                  attr::MOI.ConstraintDual,
                  ci::MOI.ConstraintIndex,
                  vi::MOI.VariableIndex,
                  F::Type{<:MOI.AbstractFunction},
                  S::Type{<:MOI.AbstractSet})

Return sum of the the dual of the `F`-in-`S` constraints except `ci` multiplied
by the coefficient of `vi` in the `MOI.ConstraintFunction`. It errors if another
variable-wise constraint different than `ci` uses `vi`.
"""
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
    vi::MOI.VariableIndex,
    F::Type{<:MOI.AbstractFunction},
    S::Type{<:MOI.AbstractSet},
)
    dual = 0.0
    for constraint_index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        dual += variable_dual(model, attr, vi, constraint_index)
    end
    return dual
end
function variable_dual(
    model::MOI.ModelLike,
    ::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
    vi::MOI.VariableIndex,
    F::Type{<:Union{MOI.SingleVariable,MOI.VectorOfVariables}},
    S::Type{<:MOI.AbstractSet},
)
    for constraint_index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        if constraint_index != ci
            func = MOI.get(model, MOI.ConstraintFunction(), constraint_index)
            if (F == MOI.SingleVariable && func.variable == vi) ||
               (F == MOI.VectorOfVariables && vi in func.variables)
                error(
                    "Fallback getter for variable constraint dual does not",
                    " support other variable-wise constraints on the variable.",
                    " Please report this issue to the solver wrapper package.",
                )
            end
        end
    end
    return 0.0
end

"""
    variable_dual(model::MOI.ModelLike,
                  attr::MOI.ConstraintDual,
                  ci::MOI.ConstraintIndex,
                  vi::MOI.VariableIndex)

Return the dual of the variable `vi` by using the duals of constraints
of index different than `ci`. It errors if another variable-wise constraint
different than `ci` uses `vi`.
"""
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex,
    vi::MOI.VariableIndex,
)
    ray = is_ray(MOI.get(model, MOI.DualStatus()))
    dual = 0.0
    if !ray
        sense = MOI.get(model, MOI.ObjectiveSense())
        # Dual definition for maximization problem corresponds to dual
        # definition for minimization problem with flipped objectived in MOI
        sign = sense == MOI.MAX_SENSE ? -1.0 : 1.0
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        obj_attr = MOI.ObjectiveFunction{F}()
        if F == MOI.SingleVariable
            if MOI.get(model, obj_attr).variable == vi
                dual += sign
            end
        elseif F <: MOI.ScalarAffineFunction
            f = MOI.get(model, obj_attr)
            dual += sign * variable_coefficient(f, vi)
        elseif F <: MOI.ScalarQuadraticFunction
            f = MOI.get(model, obj_attr)
            primal_attr = MOI.VariablePrimal(attr.result_index)
            dual +=
                sign * variable_coefficient(
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
    for FS in MOI.get(model, MOI.ListOfConstraints())
        dual -= variable_dual(model, attr, ci, vi, FS[1], FS[2])
    end
    return dual
end

"""
    variable_dual(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                  ci::MOI.ConstraintIndex{F},
                  func::F) where F <: Union{MOI.SingleVariable,
                                            MOI.VectorOfVariables}

Return the dual of the constraint of index `ci` for which the value of the
`MOI.ConstraintFunction` attribute is `func`.
"""
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.SingleVariable},
    func::MOI.SingleVariable,
)
    return variable_dual(model, attr, ci, func.variable)
end
function variable_dual(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.VectorOfVariables},
    func::MOI.VectorOfVariables,
)
    dual = map(vi -> variable_dual(model, attr, ci, vi), func.variables)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    return dot_coefficients(dual, set)
end

"""
    get_fallback(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                 ci::MOI.ConstraintIndex{Union{MOI.SingleVariable,
                                               MOI.VectorOfVariables}})

Compute the dual of the constraint of index `ci` using the `ConstraintDual` of
other constraints and the `ConstraintFunction` values. Throws an error if some
constraints are quadratic or if there is one another `MOI.SingleVariable`-in-`S`
or `MOI.VectorOfVariables`-in-`S` constraint with one of the variables in the
function of the constraint `ci`.
"""
function get_fallback(
    model::MOI.ModelLike,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{<:Union{MOI.SingleVariable,MOI.VectorOfVariables}},
)
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    return variable_dual(model, attr, ci, func)
end

# Scalar product. Any vector set defined that does not use the standard scalar
# product between vectors of ``R^n`` should redefine `set_dot` and
# `dot_coefficients`.

"""
    set_dot(x::AbstractVector, y::AbstractVector, set::AbstractVectorSet)

Return the scalar product between a vector `x` of the set `set` and a vector
`y` of the dual of the set `s`.
"""
function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.AbstractVectorSet,
)
    return dot(x, y)
end

"""
    set_dot(x, y, set::AbstractScalarSet)

Return the scalar product between a number `x` of the set `set` and a number
`y` of the dual of the set `s`.
"""
set_dot(x, y, set::MOI.AbstractScalarSet) = dot(x, y)

function triangle_dot(
    x::AbstractVector{S},
    y::AbstractVector{T},
    dim::Int,
    offset::Int,
) where {S,T}
    U = MA.promote_operation(MA.add_mul, S, T)
    result = zero(U)
    k = offset
    for i in 1:dim
        for j in 1:i
            k += 1
            if i == j
                result = MA.add_mul!(result, x[k], y[k])
            else
                result = MA.add_mul!(result, 2, x[k], y[k])
            end
        end
    end
    return result
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.AbstractSymmetricMatrixSetTriangle,
)
    return triangle_dot(x, y, MOI.side_dimension(set), 0)
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.RootDetConeTriangle,
)
    return x[1] * y[1] + triangle_dot(x, y, set.side_dimension, 1)
end

function set_dot(
    x::AbstractVector,
    y::AbstractVector,
    set::MOI.LogDetConeTriangle,
)
    return x[1] * y[1] + x[2] * y[2] + triangle_dot(x, y, set.side_dimension, 2)
end

"""
    dot_coefficients(a::AbstractVector, set::AbstractVectorSet)

Return the vector `b` such that for all vector `x` of the set `set`,
`set_dot(b, x, set)` is equal to `dot(a, x)`.
"""
function dot_coefficients(a::AbstractVector, set::MOI.AbstractVectorSet)
    return a
end

function triangle_coefficients!(
    b::AbstractVector{T},
    dim::Int,
    offset::Int,
) where {T}
    k = offset
    for i in 1:dim
        for j in 1:i
            k += 1
            if i != j
                b[k] /= 2
            end
        end
    end
end

function dot_coefficients(
    a::AbstractVector,
    set::MOI.AbstractSymmetricMatrixSetTriangle,
)
    b = copy(a)
    triangle_coefficients!(b, MOI.side_dimension(set), 0)
    return b
end

function dot_coefficients(a::AbstractVector, set::MOI.RootDetConeTriangle)
    b = copy(a)
    triangle_coefficients!(b, set.side_dimension, 1)
    return b
end

function dot_coefficients(a::AbstractVector, set::MOI.LogDetConeTriangle)
    b = copy(a)
    triangle_coefficients!(b, set.side_dimension, 2)
    return b
end
