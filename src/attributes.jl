# Attributes

"""
    AbstractSolverAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the solver.
"""
abstract type AbstractSolverAttribute end

"""
    AbstractSolverInstanceAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the solver instance.
"""
abstract type AbstractSolverInstanceAttribute end

"""
    AbstractVariableAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of variables in the solver instance.
"""
abstract type AbstractVariableAttribute end

"""
    AbstractConstraintAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of constraints in the solver instance.
"""
abstract type AbstractConstraintAttribute end

const AnyAttribute = Union{AbstractSolverAttribute, AbstractSolverInstanceAttribute, AbstractVariableAttribute, AbstractConstraintAttribute}

"""
    getattribute(s::AbstractSolver, attr::AbstractSolverAttribute)

Return an attribute `attr` of the solver `s`.

    getattribute(m::AbstractSolverInstance, attr::AbstractSolverInstanceAttribute)

Return an attribute `attr` of the solver instance `m`.

    getattribute(m::AbstractSolverInstance, attr::AbstractVariableAttribute, v::VariableReference)

Return an attribute `attr` of the variable `v` in solver instance `m`.

    getattribute(m::AbstractSolverInstance, attr::AbstractVariableAttribute, v::Vector{VariableReference})

Return a vector of attributes corresponding to each variable in the collection `v` in the solver instance `m`.

    getattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::ConstraintReference)

Return an attribute `attr` of the constraint `c` in solver instance `m`.

    getattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::Vector{VariablewiseConstraintReference{T}})
    getattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::Vector{AffineConstraintReference{T}})
    getattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::Vector{QuadraticConstraintReference{T}})

Return a vector of attributes corresponding to each constraint in the collection `c` in the solver instance `m`.

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
    throw(ArgumentError("SolverInstance of type $(typeof(m)) does not support accessing the attribute $attr"))
end

"""
    getattribute!(output, m::AbstractSolverInstance, args...)

An in-place version of `getattribute`.
The signature matches that of `getattribute` except that the the result is placed in the vector `output`.
"""
function getattribute! end
function getattribute!(output, m, attr::AnyAttribute, args...)
    throw(ArgumentError("SolverInstance of type $(typeof(m)) does not support accessing the attribute $attr"))
end

"""
    cangetattribute(s::AbstractSolver, attr::AbstractSolverAttribute)::Bool

Return a `Bool` indicating whether it is possible to query attribute `attr` from the solver `s`.

    cangetattribute(m::AbstractSolverInstance, attr::AbstractVariableAttribute, R::Type{VariableReference})::Bool
    cangetattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, R::Type{VariablewiseConstraintReference{T})::Bool
    cangetattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, R::Type{AffineConstraintReference{T})::Bool
    cangetattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, R::Type{QuadraticConstraintReference{T})::Bool

Return a `Bool` indicating whether the solver instance `m` currently has a value for the attributed specified by attribute type `attr` applied to the reference type `R`.

### Examples

```julia
cangetattribute(GurobiSolver(), SupportsAffineConstraint{Zero}())
cangetattribute(m, ObjectiveValue())
cangetattribute(m, VariablePrimalStart(), varref)
cangetattribute(m, ConstraintPrimal(), conref)
```
"""
function cangetattribute end
cangetattribute(m::AbstractSolverInstance, attr::AnyAttribute) = false

"""
    cansetattribute(s::AbstractSolver, attr::AbstractSolverAttribute)::Bool

Return a `Bool` indicating whether it is possible to set attribute `attr` in the solver `s`.

    cansetattribute(m::AbstractSolverInstance, attr::AbstractVariableAttribute, R::Type{VariableReference})::Bool
    cansetattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, R::Type{VariablewiseConstraintReference{T})::Bool
    cangetattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, R::Type{AffineConstraintReference{T})::Bool
    cangetattribute(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, R::Type{QuadraticConstraintReference{T})::Bool

Return a `Bool` indicating whether it is possible to set attribute `attr` applied to the reference type `R` in the solver instance `m`.

### Examples

```julia
cansetattribute(GurobiSolver(), SupportsAffineConstraint{Zero}())
cansetattribute(m, ObjectiveValue())
cansetattribute(m, VariablePrimalStart(), VariableReference)
cansetattribute(m, ConstraintPrimal(), AffineConstraintReference{NonNegative})
```
"""
function cansetattribute end
cansetattribute(m::AbstractSolverInstance, attr::AnyAttribute) = false

"""
    setattribute!(s::AbstractSolver, attr::AbstractSolverAttribute, value)

Assign `value` to the attribute `attr` of the solver `s`.

    setattribute!(m::AbstractSolverInstance, attr::AbstractSolverInstanceAttribute, value)

Assign `value` to the attribute `attr` of the solver instance `m`.

    setattribute!(m::AbstractSolverInstance, attr::AbstractVariableAttribute, v::VariableReference, value)

Assign `value` to the attribute `attr` of variable `v` in solver instance `m`.

    setattribute!(m::AbstractSolverInstance, attr::AbstractVariableAttribute, v::Vector{VariableReference}, vector_of_values)

Assign a value respectively to the attribute `attr` of each variable in the collection `v` in solver instance `m`.

    setattribute!(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::ConstraintReference, value)

Assign a value to the attribute `attr` of constraint `c` in solver instance `m`.

    setattribute!(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::Vector{VariablewiseConstraintReference{T}})
    setattribute!(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::Vector{AffineConstraintReference{T}})
    setattribute!(m::AbstractSolverInstance, attr::AbstractConstraintAttribute, c::Vector{QuadraticConstraintReference{T}})

Assign a value respectively to the attribute `attr` of each constraint in the collection `c` in solver instance `m`.
"""
function setattribute! end
function setattribute!(m, attr::AnyAttribute, args...)
    throw(ArgumentError("SolverInstance of type $(typeof(m)) does not support setting the attribute $attr"))
end

## Solver attributes

"""
    ReturnsDuals()

A `Bool` indicating if the solver should be expected to return dual solutions when appropriate.
"""
struct ReturnsDuals <: AbstractSolverAttribute end

"""
    SupportsAddConstraintAfterSolve()

A `Bool` indicating if the solver supports adding constraints after a solve.
If `false`, then a new solver instance should be constructed instead.
"""
struct SupportsAddConstraintAfterSolve <: AbstractSolverAttribute end

"""
    SupportsDeleteConstraint()

A `Bool` indicating if the solver supports deleting constraints from a solver instance.
"""
struct SupportsDeleteConstraint <: AbstractSolverAttribute end

"""
    SupportsDeleteVariable()

A `Bool` indicating if the solver supports deleting variables from a solver instance.
"""
struct SupportsDeleteVariable <: AbstractSolverAttribute end

"""
    SupportsAddVariableAfterSolve()

A `Bool` indicating if the solver supports adding variables after a solve.
In the context of linear programming, this is known as column generation.
"""
struct SupportsAddVariableAfterSolve <: AbstractSolverAttribute end

# TODO: solver-independent parameters

# TODO: supports modify objective

"""
    SupportsConicThroughQuadratic()

A `Bool` indicating if the solver interprets certain quadratic constraints as second-order cone constraints.
"""
struct SupportsConicThroughQuadratic <: AbstractSolverAttribute end

## Solver instance attributes

"""
    ObjectiveValue(resultidx::Int=1)

The objective value of the `resultindex`th primal result.
"""
struct ObjectiveValue <: AbstractSolverInstanceAttribute
    resultindex::Int
    (::Type{ObjectiveValue})(resultindex=1) = new(resultindex)
end

"""
    ObjectiveBound()

The best known bound on the optimal objective value.
"""
struct ObjectiveBound <: AbstractSolverInstanceAttribute end

"""
    RelativeGap()

The final relative optimality gap, defined as ``\\frac{|b-f|}{|f|}``, where ``b`` is the best bound and ``f`` is the best feasible objective value.
"""
struct RelativeGap <: AbstractSolverInstanceAttribute  end

"""
    SolveTime()

The total elapsed solution time (in seconds) as reported by the solver.
"""
struct SolveTime <: AbstractSolverInstanceAttribute end

"""
    Sense()

The optimization sense of the solver instance, an `OptimizationSense` with value `MinSense` or `MaxSense`.
"""
struct Sense <: AbstractSolverInstanceAttribute end

@enum OptimizationSense MinSense MaxSense

"""
    SimplexIterations()

The cumulative number of simplex iterations during the optimization process.
In particular, for a mixed-integer program (MIP), the total simplex iterations for all nodes.
"""
struct SimplexIterations <: AbstractSolverInstanceAttribute end

"""
    BarrierIterations()

The cumulative number of barrier iterations while solving a problem.
"""
struct BarrierIterations <: AbstractSolverInstanceAttribute end

"""
    NodeCount()

The total number of branch-and-bound nodes explored while solving a mixed-integer program (MIP).
"""
struct NodeCount <: AbstractSolverInstanceAttribute end

"""
    RawSolver()

An object that may be used to access a solver-specific API for this solver instance.
"""
struct RawSolver <: AbstractSolverInstanceAttribute end

"""
    ResultCount()

The number of results available.
"""
struct ResultCount <: AbstractSolverInstanceAttribute end

"""
    NumberOfVariables()

The number of variables in the solver instance.
"""
struct NumberOfVariables <: AbstractSolverInstanceAttribute end

"""
    NumberOfConstraints{F,S}()

The number of constraints of the type `F`-in-`S` present in the solver instance.
"""
struct NumberOfConstraints{F,S} <: AbstractSolverInstanceAttribute end

"""
    ListOfPresentConstraints()

A list of tuples of the form `(F,S)`, where `F` is a function type
and `S` is a set type indicating that the attribute `NumberOfConstraints{F,S}()`
has value greater than zero.
"""
struct ListOfPresentConstraints <: AbstractSolverInstanceAttribute end

"""
    ObjectiveFunction()

An `AbstractFunction` instance which represents the objective function.
"""
struct ObjectiveFunction <: AbstractSolverInstanceAttribute end
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

"""
    ConstraintFunction()

Return the `AbstractFunction` object used to define the constraint.
"""
struct ConstraintFunction <: AbstractConstraintAttribute end

"""
    ConstraintSet()

Return the `AbstractSet` object used to define the constraint.
"""
struct ConstraintSet <: AbstractConstraintAttribute end

## Termination status
"""
    TerminationStatus()

A `TerminationStatusCode` explaining why the solver stopped.
"""
struct TerminationStatus <: AbstractSolverInstanceAttribute end

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

To be documented: `NumericalError`, `InvalidSolverInstance`, `InvalidOption`, `Interrupted`, `OtherError`.

"""
@enum TerminationStatusCode Success AlmostSuccess InfeasibleNoResult UnboundedNoResult InfeasibleOrUnbounded IterationLimit TimeLimit NodeLimit SolutionLimit MemoryLimit ObjectiveLimit NormLimit OtherLimit SlowProgress NumericalError InvalidSolverInstance InvalidOption Interrupted OtherError

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
"""
struct PrimalStatus <: AbstractSolverInstanceAttribute
    N::Int
end
PrimalStatus() = PrimalStatus(1)

"""
    DualStatus(N)
    DualStatus()

The `ResultStatusCode` of the dual result `N`.
If `N` is omitted, it defaults to 1.
"""
struct DualStatus <: AbstractSolverInstanceAttribute
    N::Int
end
DualStatus() = DualStatus(1)
