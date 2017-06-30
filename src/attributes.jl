# Attributes

"""
    AbstractSolverOrModelAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the model or solver.
"""
abstract type AbstractSolverOrModelAttribute end

"""
    AbstractVariableAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of variables in the model.
"""
abstract type AbstractVariableAttribute end

"""
    AbstractConstraintAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of constraints in the model.
"""
abstract type AbstractConstraintAttribute end

const AnyAttribute = Union{AbstractSolverOrModelAttribute, AbstractVariableAttribute, AbstractConstraintAttribute}

"""
    getattribute(s::AbstractSolver, attr::AbstractSolverOrModelAttribute)

Return an attribute `attr` of the solver `s`.

    getattribute(m::AbstractModel, attr::AbstractSolverOrModelAttribute)

Return an attribute `attr` of the model `m`.

    getattribute(m::AbstractModel, attr::AbstractVariableAttribute, v::VariableReference)

Return an attribute `attr` of the variable `v` in model `m`.

    getattribute(m::AbstractModel, attr::AbstractVariableAttribute, v::Vector{VariableReference})

Return a vector of attributes corresponding to each variable in the collection `v` in the model `m`.

    getattribute(m::AbstractModel, attr::AbstractConstraintAttribute, c::ConstraintReference)

Return an attribute `attr` of the constraint `c` in model `m`.

    getattribute(m::AbstractModel, attr::AbstractConstraintAttribute, c::Vector{VariablewiseConstraintReference{T}})
    getattribute(m::AbstractModel, attr::AbstractConstraintAttribute, c::Vector{AffineConstraintReference{T}})
    getattribute(m::AbstractModel, attr::AbstractConstraintAttribute, c::Vector{QuadraticConstraintReference{T}})

Return a vector of attributes corresponding to each constraint in the collection `c` in the model `m`.

### Examples

```julia
getattribute(m, ObjectiveValue())
getattribute(m, VariableResult(), ref)
getattribute(m, VariableResult(5), [ref1, ref2])
getattribute(m, OtherAttribute("something specific to cplex"))
```
"""
function getattribute end

function getattribute(m, attr::AnyAttribute, args...)
    throw(ArgumentError("Model of type $(typeof(m)) does not support accessing the attribute $attr"))
end

"""
    getattribute!(output, m::AbstractModel, args...)

An in-place version of `getattribute`.
The signature matches that of `getattribute` except that the the result is placed in the vector `output`.
"""
function getattribute! end
function getattribute!(output, m, attr::AnyAttribute, args...)
    throw(ArgumentError("Model of type $(typeof(m)) does not support accessing the attribute $attr"))
end

"""
    cangetattribute(s::AbstractSolver, attr::AbstractSolverOrModelAttribute)::Bool

Return a `Bool` indicating whether it is possible to query attribute `attr` from the solver `s`.

    cangetattribute(m::AbstractModel, attr::AbstractVariableAttribute, R::Type{VariableReference})::Bool
    cangetattribute(m::AbstractModel, attr::AbstractConstraintAttribute, R::Type{VariablewiseConstraintReference{T})::Bool
    cangetattribute(m::AbstractModel, attr::AbstractConstraintAttribute, R::Type{AffineConstraintReference{T})::Bool
    cangetattribute(m::AbstractModel, attr::AbstractConstraintAttribute, R::Type{QuadraticConstraintReference{T})::Bool

Return a `Bool` indicating whether the model `m` currently has a value for the attributed specified by attribute type `attr` applied to the reference type `R`.

### Examples

```julia
cangetattribute(GurobiSolver(), SupportsAffineConstraint{Zero}())
cangetattribute(m, ObjectiveValue())
cangetattribute(m, VariablePrimalStart(), varref)
cangetattribute(m, ConstraintPrimal(), conref)
```
"""
function cangetattribute end
cangetattribute(m::AbstractModel, attr::AnyAttribute) = false

"""
    cansetattribute(s::AbstractSolver, attr::AbstractSolverOrModelAttribute)::Bool

Return a `Bool` indicating whether it is possible to set attribute `attr` in the solver `s`.

    cansetattribute(m::AbstractModel, attr::AbstractVariableAttribute, R::Type{VariableReference})::Bool
    cansetattribute(m::AbstractModel, attr::AbstractConstraintAttribute, R::Type{VariablewiseConstraintReference{T})::Bool
    cangetattribute(m::AbstractModel, attr::AbstractConstraintAttribute, R::Type{AffineConstraintReference{T})::Bool
    cangetattribute(m::AbstractModel, attr::AbstractConstraintAttribute, R::Type{QuadraticConstraintReference{T})::Bool

Return a `Bool` indicating whether it is possible to set attribute `attr` applied to the reference type `R` in the model `m`.

### Examples

```julia
cansetattribute(GurobiSolver(), SupportsAffineConstraint{Zero}())
cansetattribute(m, ObjectiveValue())
cansetattribute(m, VariablePrimalStart(), VariableReference)
cansetattribute(m, ConstraintPrimal(), AffineConstraintReference{NonNegative})
```
"""
function cansetattribute end
cansetattribute(m::AbstractModel, attr::AnyAttribute) = false

"""
    setattribute!(s::AbstractSolver, attr::AbstractSolverOrModelAttribute, value)

Assign `value` to the attribute `attr` of the solver `s`.

    setattribute!(m::AbstractModel, attr::AbstractSolverOrModelAttribute, value)

Assign `value` to the attribute `attr` of the model `m`.

    setattribute!(m::AbstractModel, attr::AbstractVariableAttribute, v::VariableReference, value)

Assign `value` to the attribute `attr` of variable `v` in model `m`.

    setattribute!(m::AbstractModel, attr::AbstractVariableAttribute, v::Vector{VariableReference}, vector_of_values)

Assign a value respectively to the attribute `attr` of each variable in the collection `v` in model `m`.

    setattribute!(m::AbstractModel, attr::AbstractConstraintAttribute, c::ConstraintReference, value)

Assign a value to the attribute `attr` of constraint `c` in model `m`.

    setattribute!(m::AbstractModel, attr::AbstractConstraintAttribute, c::Vector{VariablewiseConstraintReference{T}})
    setattribute!(m::AbstractModel, attr::AbstractConstraintAttribute, c::Vector{AffineConstraintReference{T}})
    setattribute!(m::AbstractModel, attr::AbstractConstraintAttribute, c::Vector{QuadraticConstraintReference{T}})

Assign a value respectively to the attribute `attr` of each constraint in the collection `c` in model `m`.
"""
function setattribute! end
function setattribute!(m, attr::AnyAttribute, args...)
    throw(ArgumentError("Model of type $(typeof(m)) does not support setting the attribute $attr"))
end

## Solver or model attributes

"""
    ReturnsDuals()

A `Bool` indicating if the solver should be expected to return dual solutions when appropriate.
A solver attribute.
"""
struct ReturnsDuals <: AbstractSolverOrModelAttribute end

"""
    SupportsAddConstraintAfterSolver()

A `Bool` indicating if the solver supports adding constraints after a solve.
If `false`, then a new model should be constructed instead.
A solver attribute.
"""
struct SupportsAddConstraintAfterSolve <: AbstractSolverOrModelAttribute end

"""
    SupportsDeleteConstraint()

A `Bool` indicating if the solver supports deleting constraints from a model.
A solver attribute.
"""
struct SupportsDeleteConstraint <: AbstractSolverOrModelAttribute end

"""
    SupportsAddVariableAfterSolve()

A `Bool` indicating if the solver supports adding variables after a solve.
In the context of linear programming, this is known as column generation.
A solver attribute.
"""
struct SupportsAddVariableAfterSolver <: AbstractSolverOrModelAttribute end

# TODO: solver-independent parameters

# TODO: supports modify objective

"""
    SupportsQuadraticObjective()

A `Bool` indicating if the solver supports quadratic objectives.
A solver attribute.
"""
struct SupportsQuadraticObjective <: AbstractSolverOrModelAttribute end

"""
    SupportsConicThroughQuadratic()

A `Bool` indicating if the solver interprets certain quadratic constraints as second-order cone constraints.
A solver attribute.
"""
struct SupportsConicThroughQuadratic <: AbstractSolverOrModelAttribute end

"""
    ObjectiveValue(resultidx::Int=1, objectiveindex::Int=1)

The objective value of the `resultindex`th primal result of the `objectiveindex`th objective.
A model attribute.

Both `resultindex` and `objectiveindex` default to 1.
"""
struct ObjectiveValue <: AbstractSolverOrModelAttribute
    resultindex::Int
    objectiveindex::Int
    (::Type{ObjectiveValue})(resultindex=1, objectiveindex=1) = new(resultindex, objectiveindex)
end

"""
    ObjectiveBound()

The best known bound on the optimal objective value.
A model attribute.
"""
struct ObjectiveBound <: AbstractSolverOrModelAttribute end

"""
    RelativeGap()

The final relative optimality gap, defined as ``\\frac{|b-f|}{|f|}``, where ``b`` is the best bound and ``f`` is the best feasible objective value.
A model attribute.
"""
struct RelativeGap <: AbstractSolverOrModelAttribute  end

"""
    SolveTime()

The total elapsed solution time (in seconds) as reported by the solver.
A model attribute.
"""
struct SolveTime <: AbstractSolverOrModelAttribute end

"""
    Sense()

The optimization sense of the model, an `OptimizationSense` with value `MinSense` or `MaxSense`.
A model attribute.
"""
struct Sense <: AbstractSolverOrModelAttribute end

@enum OptimizationSense MinSense MaxSense

"""
    SimplexIterations()

The cumulative number of simplex iterations during the optimization process.
In particular, for a mixed-integer program (MIP), the total simplex iterations for all nodes.
A model attribute.
"""
struct SimplexIterations <: AbstractSolverOrModelAttribute end

"""
    BarrierIterations()

The cumulative number of barrier iterations while solving a problem.
A model attribute.
"""
struct BarrierIterations <: AbstractSolverOrModelAttribute end

"""
    NodeCount()

The total number of branch-and-bound nodes explored while solving a mixed-integer program (MIP).
A model attribute.
"""
struct NodeCount <: AbstractSolverOrModelAttribute end

"""
    RawSolver()

An object that may be used to access a solver-specific API for this model.
A model attribute.
"""
struct RawSolver <: AbstractSolverOrModelAttribute end

"""
    ResultCount()

The number of results available.
A model attribute.
"""
struct ResultCount <: AbstractSolverOrModelAttribute end

"""
    NumberOfVariables()

The number of variables in the model.
A model attribute.
"""
struct NumberOfVariables <: AbstractSolverOrModelAttribute end

"""
    NumberOfVariablewiseConstraints{T}()

The number of variablewise constraints of type `T` in the model.
A model attribute.
"""
struct NumberOfVariablewiseConstraints{T} <: AbstractSolverOrModelAttribute end

"""
    NumberOfAffineConstraints{T}()

The number of affine constraints of type `T` in the model.
A model attribute.
"""
struct NumberOfAffineConstraints{T} <: AbstractSolverOrModelAttribute end

"""
    NumberOfQuadraticConstraints{T}()

The number of quadratic constraints of type `T` in the model.
A model attribute.
"""
struct NumberOfQuadraticConstraints{T} <: AbstractSolverOrModelAttribute end

"""
    SupportsVariablewiseConstraint{T}()

A `Bool` indicating whether the solver or model supports a variable-wise constraint in the set `s` of type `T`.
A solver and model attribute.
"""
struct SupportsVariablewiseConstraint{T} <: AbstractSolverOrModelAttribute end

"""
    SupportsAffineConstraint{T}()

A `Bool` indicating whether the solver or model supports an affine constraint in the set `s` of type `T`.
A solver and model attribute.
"""
struct SupportsAffineConstraint{T} <: AbstractSolverOrModelAttribute end

"""
    SupportsQuadraticConstraint{T}()

A `Bool` indicating whether the solver or model supports a quadratic constraint in the set `s` of type `T`.
A solver and model attribute.
"""
struct SupportsQuadraticConstraint{T} <: AbstractSolverOrModelAttribute end

## Variable attributes

"""
    VariablePrimalStart()

An initial assignment of the variables that the solver may use to warm-start the solve.
"""
struct VariablePrimalStart <: AbstractVariableAttribute end

"""
    VariablePrimal(N)
    VariablePrimal()

The assignment to the primal variables in result `N`.
If `N` is omitted, it is 1 by default.
"""
struct VariablePrimal <: AbstractVariableAttribute
    N::Int
end
VariablePrimal() = VariablePrimal(1)

"""
    VariableBasisStatus()

Returns the `BasisStatusCode` of a given variable, with respect to an available optimal solution basis.
"""
struct VariableBasisStatus <: AbstractVariableAttribute end

"""
    BasisStatusCode

An Enum of possible values for the `VariableBasisStatus` and `ConstraintBasisStatus` attribute.
This explains the status of a given element with respect to an optimal solution basis.
Possible values are:
* `Basic`: element is in the basis
* `Nonbasic`: element is not in the basis
* `NonbasicAtLower`: element is not in the basis and is at its lower bound
* `NonbasicAtUpper`: element is not in the basis and is at its upper bound
* `SuperBasic`: element is not in the basis but is also not at one of its bounds
"""
@enum BasisStatusCode Basic Nonbasic NonbasicAtLower NonbasicAtUpper SuperBasic

## Constraint attributes

"""
    ConstraintPrimalStart()

An initial assignment of the constraint primal values that the solver may use to warm-start the solve.
"""
struct ConstraintPrimalStart <: AbstractConstraintAttribute end

"""
    ConstraintDualStart()

An initial assignment of the constriant duals that the solver may use to warm-start the solve.
"""
struct ConstraintDualStart <: AbstractConstraintAttribute end

"""
    ConstraintPrimal(N)
    ConstraintPrimal()

The assignment to the constraint primal values in result `N`.
If `N` is omitted, it is 1 by default.
"""
struct ConstraintPrimal <: AbstractConstraintAttribute
    N::Int
end
ConstraintPrimal() = ConstraintPrimal(1)

"""
    ConstraintDual(N)
    ConstraintDual()

The assignment to the constraint dual values in result `N`.
If `N` is omitted, it is 1 by default.
"""
struct ConstraintDual <: AbstractConstraintAttribute
    N::Int
end
ConstraintDual() = ConstraintDual(1)

"""
    ConstraintBasisStatus()

Returns the `BasisStatusCode` of a given constraint, with respect to an available optimal solution basis.
"""
struct ConstraintBasisStatus <: AbstractConstraintAttribute end

## Termination status
"""
    TerminationStatus()

A `TerminationStatusCode` explaining why the solver stopped.
A model attribute.
"""
struct TerminationStatus <: AbstractSolverOrModelAttribute end

"""
    TerminationStatusCode

An Enum of possible values for the `TerminationStatus` attribute.
This attribute is meant to explain the reason why the solver stopped executing.

## OK

These are generally OK statuses.

* `Success`: the algorithm ran successfully and has a result; this includes cases where the algorithm converges to an infeasible point (NLP) or converges to a solution of a homogeneous self-dual problem and has a certificate of primal/dual infeasibility
* `AlmostSuccess`: the algorithm *almost* ran successfully (e.g., to relaxed convergence tolerances) and has a result
* `InfeasibleNoResult`: the algorithm stopped because it decided that the problem is infeasible but does not have a result to return
* `UnboundedNoResult`: the algorithm stopped because it decided that the problem is unbounded but does not have a result to return
* `InfeasibleOrUnbounded`: the algorithm stopped because it decided that the problem is infeasible or unbounded (no result is available); this occasionally happens during MIP presolve

## Limits

The solver stopped because of some user-defined limit.
To be documented: `IterationLimit`, `TimeLimit`, `NodeLimit`, `SolutionLimit`, `MemoryLimit`, `ObjectiveLimit`, `NormLimit`, `OtherLimit`.

## Problematic

This group of statuses means that something unexpected or problematic happened.

* `SlowProgress`: the algorithm stopped because it was unable to continue making progress towards the solution
* `AlmostSuccess` should be used if there is additional information that relaxed convergence tolerances are satisfied

To be documented: `NumericalError`, `InvalidModel`, `InvalidOption`, `Interrupted`, `OtherError`.

"""
@enum TerminationStatusCode Success AlmostSuccess InfeasibleNoResult UnboundedNoResult InfeasibleOrUnbounded IterationLimit TimeLimit NodeLimit SolutionLimit MemoryLimit ObjectiveLimit NormLimit OtherLimit SlowProgress NumericalError InvalidModel InvalidOption Interrupted OtherError

## Result status

"""
    ResultStatusCode

An Enum of possible values for the `PrimalStatus` and `DualStatus` attributes.
The values indicate how to interpret the result vector.

* `FeasiblePoint`
* `NearlyFeasiblePoint`
* `InfeasiblePoint`
* `InfeasibilityCertificate`
* `NearlyInfeasibilityCertificate`
* `ReductionCertificate`
* `NearlyReductionCertificate`
* `Unknown`
* `Other`
"""
@enum ResultStatusCode FeasiblePoint NearlyFeasiblePoint InfeasiblePoint InfeasibilityCertificate NearlyInfeasibilityCertificate ReductionCertificate NearlyReductionCertificate Unknown Other

"""
    PrimalStatus(N)
    PrimalStatus()

The `ResultStatusCode` of the primal result `N`.
If `N` is omitted, it defaults to 1.
A model attribute.
"""
struct PrimalStatus <: AbstractSolverOrModelAttribute
    N::Int
end
PrimalStatus() = PrimalStatus(1)

"""
    DualStatus(N)
    DualStatus()

The `ResultStatusCode` of the dual result `N`.
If `N` is omitted, it defaults to 1.
A model attribute.
"""
struct DualStatus <: AbstractSolverOrModelAttribute
    N::Int
end
DualStatus() = DualStatus(1)
