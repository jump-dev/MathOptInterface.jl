# This file contains the implementation of different methods for the
# `get_fallback` function. These methods can be used by solver wrappers as
# fallbacks for implemented the `get` method when the solver API does not
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
function get_fallback(model::MOI.ModelLike, ::MOI.ObjectiveValue)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    # TODO do not include constant if primal solution is a ray
    return evalvariables(vi -> MOI.get(model, MOI.VariablePrimal(), vi), f)
end

"""
    get_fallback(model::MOI.ModelLike, ::MOI.ConstraintPrimal,
                 constraint_index::MOI.ConstraintIndex)

Compute the value of the function of the constraint of index `constraint_index`
using the `VariablePrimal` results and the `ConstraintFunction` values.
"""
function get_fallback(model::MOI.ModelLike, ::MOI.ConstraintPrimal,
                      idx::MOI.ConstraintIndex)
    f = MOI.get(model, MOI.ConstraintFunction(), idx)
    # TODO do not include constant if primal solution is a ray
    return evalvariables(vi -> MOI.get(model, MOI.VariablePrimal(), vi), f)
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
# `MOI.set_dot`.
# Once A_j* y_j is obtained, we invert A_j* with `MOI.dot_coefficients`.

function variable_coefficient(func::MOI.ScalarAffineFunction{T},
                              vi::MOI.VariableIndex) where T
    coef = zero(T)
    for term in func.terms
        if term.variable_index == vi
            coef += term.coefficient
        end
    end
    return coef
end
function variable_coefficient(func::MOI.VectorAffineFunction{T},
                              vi::MOI.VariableIndex) where T
    coef = zeros(T, MOI.output_dimension(func))
    for vector_term in func.terms
        term = vector_term.scalar_term
        if term.variable_index == vi
            coef[vector_term.output_index] += term.coefficient
        end
    end
    return coef
end

function variable_dual(model::MOI.ModelLike,
                       ::MOI.ConstraintDual,
                       ci::MOI.ConstraintIndex,
                       vi::MOI.VariableIndex,
                       F::Type{<:Union{MOI.SingleVariable,
                                       MOI.VectorOfVariables}},
                       S::Type{<:MOI.AbstractSet})
    for constraint_index in MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
        if constraint_index != ci
            func = MOI.get(model, MOI.ConstraintFunction(), constraint_index)
            if (F == MOI.SingleVariable && func.variable == vi) ||
               (F == MOI.VectorOfVariables && vi in func.variables)
               error("Fallback getter for variable constraint dual does not support other variable-wise constraints on the variable.")
            end
        end
    end
    return 0.0
end

function variable_dual(::MOI.ModelLike,
                       ::MOI.ConstraintDual,
                       ::MOI.ConstraintIndex,
                       ::MOI.VariableIndex,
                       ::Type{<:Union{MOI.ScalarQuadraticFunction,
                                      MOI.VectorQuadraticFunction}},
                       ::Type{<:MOI.AbstractSet})
    error("Fallback getter for variable constraint dual only supports affine constraint functions.")
end

function variable_dual(model::MOI.ModelLike,
                       attr::MOI.ConstraintDual,
                       vi::MOI.VariableIndex,
                       ci::MOI.ConstraintIndex{<:MOI.VectorAffineFunction})
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    coef = variable_coefficient(func, vi)
    dual = MOI.get(model, attr, ci)
    return MOI.set_dot(coef, dual, set)
end
function variable_dual(model::MOI.ModelLike,
                       attr::MOI.ConstraintDual,
                       vi::MOI.VariableIndex,
                       ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction})
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    coef = variable_coefficient(func, vi)
    dual = MOI.get(model, attr, ci)
    return coef * dual
end
function variable_dual(model::MOI.ModelLike,
                       attr::MOI.ConstraintDual,
                       ci::MOI.ConstraintIndex,
                       vi::MOI.VariableIndex,
                       F::Type{<:MOI.AbstractFunction},
                       S::Type{<:MOI.AbstractSet})
    dual = 0.0
    for constraint_index in MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
        dual += variable_dual(model, attr, vi, constraint_index)
    end
    return dual
end
function variable_dual(model::MOI.ModelLike,
                       attr::MOI.ConstraintDual,
                       ci::MOI.ConstraintIndex,
                       vi::MOI.VariableIndex)
    status = MOI.get(model, MOI.DualStatus())
    ray = status == MOI.InfeasibilityCertificate ||
          status == MOI.NearlyInfeasibilityCertificate
    dual = 0.0
    if !ray
        sense = MOI.get(model, MOI.ObjectiveSense())
        # Dual definition for maximization problem corresponds to dual
        # definition for minimization problem with flipped objectived in MOI
        sign = sense == MOI.MaxSense ? -1.0 : 1.0
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        obj_attr = MOI.ObjectiveFunction{F}()
        if F == MOI.SingleVariable
            if MOI.get(model, obj_attr).variable == vi
                dual += sign
            end
        elseif F <: MOI.ScalarAffineFunction
            f = MOI.get(model, obj_attr)
            dual += sign * variable_coefficient(f, vi)
        else
            error("Fallback getter for variable constraint dual only supports affine objective function.")
        end
    end
    for FS in MOI.get(model, MOI.ListOfConstraints())
        dual -= variable_dual(model, attr, ci, vi, FS[1], FS[2])
    end
    return dual
end

function variable_dual(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                       ci::MOI.ConstraintIndex, func::MOI.SingleVariable)
    return variable_dual(model, attr, ci, func.variable)
end
function variable_dual(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                       ci::MOI.ConstraintIndex, func::MOI.VectorOfVariables)
    dual = map(vi -> variable_dual(model, attr, ci, vi), func.variables)
    set = MOI.get(model, MOI.ConstraintSet(), ci)
    return MOI.dot_coefficients(dual, set)
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
function get_fallback(model::MOI.ModelLike, attr::MOI.ConstraintDual,
                      ci::MOI.ConstraintIndex{Union{MOI.SingleVariable,
                                                    MOI.VectorOfVariables}})
    func = MOI.get(model, MOI.ConstraintFunction(), ci)
    return variable_dual(model, attr, ci, func)
end
