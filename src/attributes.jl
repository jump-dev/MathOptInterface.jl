# Attributes

"""
    AbstractOptimizerAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the optimizer.

### Note

The difference between `AbstractOptimizerAttribute` and `AbstractModelAttribute` lies in the behavior of `isempty`, `empty!` and `copy!`.
Typically optimizer attributes only affect how the model is solved.
"""
abstract type AbstractOptimizerAttribute end

"""
    AbstractModelAttribute

Abstract supertype for attribute objects that can be used to set or get attributes (properties) of the model.
"""
abstract type AbstractModelAttribute end

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

const AnyAttribute = Union{AbstractOptimizerAttribute, AbstractModelAttribute, AbstractVariableAttribute, AbstractConstraintAttribute}

"""
    supports(model::ModelLike, attr::AbstractOptimizerAttribute)::Bool

Return a `Bool` indicating whether `model` supports the optimizer attribute `attr`.

    supports(model::ModelLike, attr::AbstractModelAttribute)::Bool

Return a `Bool` indicating whether `model` supports the model attribute `attr`.

    supports(model::ModelLike, attr::AbstractVariableAttribute, ::Type{VariableIndex})::Bool

Return a `Bool` indicating whether `model` supports the variable attribute `attr`.

    supports(model::ModelLike, attr::AbstractConstraintAttribute, ::Type{ConstraintIndex{F,S}})::Bool where {F,S}

Return a `Bool` indicating whether `model` supports the constraint attribute `attr` applied to an `F`-in-`S` constraint.

In other words, it should return `true` if `copy!(model, src)` does not return `CopyUnsupportedAttribute` when the attribute `attr` is set to `src`.
If the attribute is only not supported in specific circumstances, it should still return `true`.
"""
function supports end
supports(::ModelLike, ::Union{AbstractModelAttribute, AbstractOptimizerAttribute}) = false
supports(::ModelLike, ::Union{AbstractVariableAttribute, AbstractConstraintAttribute}, ::Type{<:Index}) = false

"""
    get(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)

Return an attribute `attr` of the optimizer `optimizer`.

    get(model::ModelLike, attr::AbstractModelAttribute)

Return an attribute `attr` of the model `model`.

    get(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex)

Return an attribute `attr` of the variable `v` in model `model`.

    get(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex})

Return a vector of attributes corresponding to each variable in the collection `v` in the model `model`.

    get(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex)

Return an attribute `attr` of the constraint `c` in model `model`.

    get(model::ModelLike, attr::AbstractConstraintAttribute, c::Vector{ConstraintIndex{F,S}})

Return a vector of attributes corresponding to each constraint in the collection `c` in the model `model`.

    get(model::ModelLike, ::Type{VariableIndex}, name::String)

If a variable with name `name` exists in the model `model`, return the corresponding index, otherwise throw a `KeyError`.

    get(model::ModelLike, ::Type{ConstraintIndex{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}

If an `F`-in-`S` constraint with name `name` exists in the model `model`, return the corresponding index, otherwise throw a `KeyError`.

    get(model::ModelLike, ::Type{ConstraintIndex}, name::String)

If *any* constraint with name `name` exists in the model `model`, return the corresponding index, otherwise throw a `KeyError`. This version is available for convenience but may incur a performance penalty because it is not type stable.

### Note

It is the user's responsibility to check that `canget` returns `true` prior to calling `get`. 
Calling `get` when `canget` returns `false` will throw an error.

### Examples

```julia
get(model, ObjectiveValue())
get(model, VariablePrimal(), ref)
get(model, VariablePrimal(5), [ref1, ref2])
get(model, OtherAttribute("something specific to cplex"))
get(model, VariableIndex, "var1")
get(model, ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}, "con1")
get(model, ConstraintIndex, "con1")
```
"""
function get end
# We want to avoid being too specific in the type arguments to avoid method ambiguity.
# For model, get(::ModelLike, ::AbstractVariableAttribute, ::Vector{VariableIndex}) would not allow
# to define get(::SomeModel, ::AnyAttribute, ::Vector)
get(model::ModelLike, attr::AnyAttribute, idxs::Vector) = get.(model, attr, idxs)

function get(model::ModelLike, attr::AnyAttribute, args...)
    throw(ArgumentError("ModelLike of type $(typeof(model)) does not support accessing the attribute $attr"))
end

"""
    get!(output, model::ModelLike, args...)

An in-place version of `get`.
The signature matches that of `get` except that the the result is placed in the vector `output`.
"""
function get! end
function get!(output, model::ModelLike, attr::AnyAttribute, args...)
    throw(ArgumentError("ModelLike of type $(typeof(model)) does not support accessing the attribute $attr"))
end

"""
    canget(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)::Bool

Return a `Bool` indicating whether `optimizer` currently has a value for the attribute specified by attr type `attr`.

    canget(model::ModelLike, attr::AbstractModelAttribute)::Bool

Return a `Bool` indicating whether `model` currently has a value for the attribute specified by attribute type `attr`.

    canget(model::ModelLike, attr::AbstractVariableAttribute, ::Type{VariableIndex})::Bool

Return a `Bool` indicating whether `model` currently has a value for the attribute specified by attribute type `attr` applied to *every* variable of the model.

    canget(model::ModelLike, attr::AbstractConstraintAttribute, ::Type{ConstraintIndex{F,S}})::Bool where {F<:AbstractFunction,S<:AbstractSet}

Return a `Bool` indicating whether `model` currently has a value for the attribute specified by attribute type `attr` applied to *every* `F`-in-`S` constraint.

    canget(model::ModelLike, ::Type{VariableIndex}, name::String)::Bool

Return a `Bool` indicating if a variable with the name `name` exists in `model`.

    canget(model::ModelLike, ::Type{ConstraintIndex{F,S}}, name::String)::Bool where {F<:AbstractFunction,S<:AbstractSet}

Return a `Bool` indicating if an `F`-in-`S` constraint with the name `name` exists in `model`.

    canget(model::ModelLike, ::Type{ConstraintIndex}, name::String)::Bool

Return a `Bool` indicating if a constraint of any kind with the name `name` exists in `model`.


### Examples

```julia
canget(model, ObjectiveValue())
canget(model, VariablePrimalStart(), VariableIndex)
canget(model, VariablePrimal(), VariableIndex)
canget(model, ConstraintPrimal(), ConstraintIndex{SingleVariable,EqualTo{Float64}})
canget(model, VariableIndex, "var1")
canget(model, ConstraintIndex{ScalarAffineFunction{Float64},LessThan{Float64}}, "con1")
canget(model, ConstraintIndex, "con1")
```
"""
function canget end
canget(model::ModelLike, attr::Union{AbstractModelAttribute, AbstractOptimizerAttribute}) = false
canget(model::ModelLike, attr::Union{AbstractVariableAttribute, AbstractConstraintAttribute}, ::Type{<:Index}) = false

"""
    canset(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute)::Bool

Return a `Bool` indicating whether it is possible to set the attribute `attr` to the optimizer `optimizer`.

    canset(model::ModelLike, attr::AbstractModelAttribute)::Bool

Return a `Bool` indicating whether it is possible to set the attribute `attr` to the model `model`.

    canset(model::ModelLike, attr::AbstractVariableAttribute, R::Type{VariableIndex})::Bool
    canset(model::ModelLike, attr::AbstractConstraintAttribute, R::Type{ConstraintIndex{F,S})::Bool

Return a `Bool` indicating whether it is possible to set attribute `attr` applied to the index type `R` in the model `model`.

### Examples

```julia
canset(model, ObjectiveValue())
canset(model, VariablePrimalStart(), VariableIndex)
canset(model, ConstraintPrimal(), ConstraintIndex{VectorAffineFunction{Float64},Nonnegatives})
```
"""
function canset end
canset(model::ModelLike, attr::Union{AbstractModelAttribute, AbstractOptimizerAttribute}) = false
canset(model::ModelLike, attr::Union{AbstractVariableAttribute, AbstractConstraintAttribute}, ref::Type{<:Index}) = false

"""
    set!(optimizer::AbstractOptimizer, attr::AbstractOptimizerAttribute, value)

Assign `value` to the attribute `attr` of the optimizer `optimizer`.

    set!(model::ModelLike, attr::AbstractModelAttribute, value)

Assign `value` to the attribute `attr` of the model `model`.

    set!(model::ModelLike, attr::AbstractVariableAttribute, v::VariableIndex, value)

Assign `value` to the attribute `attr` of variable `v` in model `model`.

    set!(model::ModelLike, attr::AbstractVariableAttribute, v::Vector{VariableIndex}, vector_of_values)

Assign a value respectively to the attribute `attr` of each variable in the collection `v` in model `model`.

    set!(model::ModelLike, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

Assign a value to the attribute `attr` of constraint `c` in model `model`.

    set!(model::ModelLike, attr::AbstractConstraintAttribute, c::Vector{ConstraintIndex{F,S}}, vector_of_values)

Assign a value respectively to the attribute `attr` of each constraint in the collection `c` in model `model`.

### Note

It is the user's responsibility to check that `canset` returns `true` prior to calling `set!`. 
Calling `set!` when `canset` returns `false` will throw an error.
"""
function set! end
# See note with get
set!(model::ModelLike, attr::Union{AbstractVariableAttribute, AbstractConstraintAttribute}, idxs::Vector, vector_of_values::Vector) = set!.(model, attr, idxs, vector_of_values)

function set!(model::ModelLike, attr::AnyAttribute, args...)
    throw(ArgumentError("ModelLike of type $(typeof(model)) does not support setting the attribute $attr"))
end

## Optimizer attributes

"""
    ListOfOptimizerAttributesSet()

A `Vector{AbstractOptimizerAttribute}` of all optimizer attributes that were set.
"""
struct ListOfOptimizerAttributesSet <: AbstractOptimizerAttribute end

## Model attributes

"""
    ListOfModelAttributesSet()

A `Vector{AbstractModelAttribute}` of all model attributes that were set to the model.
"""
struct ListOfModelAttributesSet <: AbstractModelAttribute end

"""
    Name()

A string identifying the model.
"""
struct Name <: AbstractModelAttribute end

"""
    ObjectiveSense()

The sense of the objective function, an `OptimizationSense` with value `MinSense`, `MaxSense`, or `FeasiblitySense`.
"""
struct ObjectiveSense <: AbstractModelAttribute end

@enum OptimizationSense MinSense MaxSense FeasibilitySense

"""
    NumberOfVariables()

The number of variables in the model.
"""
struct NumberOfVariables <: AbstractModelAttribute end

"""
    ListOfVariableIndices()

A `Vector{VariableIndex}` indexing all variables present
in the model (i.e., of length equal to the value of `NumberOfVariables()`).
"""
struct ListOfVariableIndices <: AbstractModelAttribute end

"""
    ListOfConstraintIndices{F,S}()

A `Vector{ConstraintIndex{F,S}}` indexing all constraints of
type `F`-in`S` in the model (i.e., of length equal to the value of `NumberOfConstraints{F,S}()`).
"""
struct ListOfConstraintIndices{F,S} <: AbstractModelAttribute end

"""
    NumberOfConstraints{F,S}()

The number of constraints of the type `F`-in-`S` present in the model.
"""
struct NumberOfConstraints{F,S} <: AbstractModelAttribute end

"""
    ListOfConstraints()

A list of tuples of the form `(F,S)`, where `F` is a function type
and `S` is a set type indicating that the attribute `NumberOfConstraints{F,S}()`
has value greater than zero.
"""
struct ListOfConstraints <: AbstractModelAttribute end

"""
    ObjectiveFunction{F<:AbstractScalarFunction}()

An `F` model which represents the objective function.
It is guaranteed to be equivalent but not necessarily identical to the function provided by the user.
Throws an `InexactError` if the objective function cannot be converted to `F`,
e.g. the objective function is quadratic and `F` is `ScalarAffineFunction{Float64}` or
it has non-integer coefficient and `F` is `ScalarAffineFunction{Int}`.
"""
struct ObjectiveFunction{F<:AbstractScalarFunction} <: AbstractModelAttribute end

## Optimizer attributes

"""
    ObjectiveValue(resultidx::Int=1)

The objective value of the `resultindex`th primal result.
"""
struct ObjectiveValue <: AbstractModelAttribute
    resultindex::Int
    (::Type{ObjectiveValue})(resultindex=1) = new(resultindex)
end

"""
    ObjectiveBound()

The best known bound on the optimal objective value.
"""
struct ObjectiveBound <: AbstractModelAttribute end

"""
    RelativeGap()

The final relative optimality gap, defined as ``\\frac{|b-f|}{|f|}``, where ``b`` is the best bound and ``f`` is the best feasible objective value.
"""
struct RelativeGap <: AbstractModelAttribute  end

"""
    SolveTime()

The total elapsed solution time (in seconds) as reported by the optimizer.
"""
struct SolveTime <: AbstractModelAttribute end

"""
    SimplexIterations()

The cumulative number of simplex iterations during the optimization process.
In particular, for a mixed-integer program (MIP), the total simplex iterations for all nodes.
"""
struct SimplexIterations <: AbstractModelAttribute end

"""
    BarrierIterations()

The cumulative number of barrier iterations while solving a problem.
"""
struct BarrierIterations <: AbstractModelAttribute end

"""
    NodeCount()

The total number of branch-and-bound nodes explored while solving a mixed-integer program (MIP).
"""
struct NodeCount <: AbstractModelAttribute end

"""
    RawSolver()

An object that may be used to access a solver-specific API for this optimizer.
"""
struct RawSolver <: AbstractModelAttribute end

"""
    ResultCount()

The number of results available.
"""
struct ResultCount <: AbstractModelAttribute end

## Variable attributes

"""
    ListOfVariableAttributesSet()

A `Vector{AbstractVariableAttribute}` of all variable attributes that were set to the model.
"""
struct ListOfVariableAttributesSet <: AbstractModelAttribute end

"""
    VariableName()

A string identifying the variable. It is invalid for two variables to have the same name.
"""
struct VariableName <: AbstractVariableAttribute end

"""
    VariablePrimalStart()

An initial assignment of the variables that the optimizer may use to warm-start the solve.
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
    ListOfConstraintAttributesSet{F, S}()

A `Vector{AbstractConstraintAttribute}` of all constraint attributes that were set to `F`-in-`S` constraints.
"""
struct ListOfConstraintAttributesSet{F,S} <: AbstractModelAttribute end

"""
    ConstraintName()

A string identifying the constraint. It is invalid for two constraints of any kind to have the same name.
"""
struct ConstraintName <: AbstractConstraintAttribute end

"""
    ConstraintPrimalStart()

An initial assignment of the constraint primal values that the optimizer may use to warm-start the solve.
"""
struct ConstraintPrimalStart <: AbstractConstraintAttribute end

"""
    ConstraintDualStart()

An initial assignment of the constraint duals that the optimizer may use to warm-start the solve.
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
It is guaranteed to be equivalent but not necessarily identical to the function provided by the user.
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

A `TerminationStatusCode` explaining why the optimizer stopped.
"""
struct TerminationStatus <: AbstractModelAttribute end

"""
    TerminationStatusCode

An Enum of possible values for the `TerminationStatus` attribute.
This attribute is meant to explain the reason why the optimizer stopped executing.

## OK

These are generally OK statuses.

* `Success`: the algorithm ran successfully and has a result; this includes cases where the algorithm converges to an infeasible point (NLP) or converges to a solution of a homogeneous self-dual problem and has a certificate of primal/dual infeasibility
* `InfeasibleNoResult`: the algorithm stopped because it decided that the problem is infeasible but does not have a result to return
* `UnboundedNoResult`: the algorithm stopped because it decided that the problem is unbounded but does not have a result to return
* `InfeasibleOrUnbounded`: the algorithm stopped because it decided that the problem is infeasible or unbounded (no result is available); this occasionally happens during MIP presolve

## Limits

The optimizer stopped because of some user-defined limit.
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
* `UnknownResultStatus`
* `OtherResultStatus`
"""
@enum ResultStatusCode FeasiblePoint NearlyFeasiblePoint InfeasiblePoint InfeasibilityCertificate NearlyInfeasibilityCertificate ReductionCertificate NearlyReductionCertificate UnknownResultStatus OtherResultStatus

"""
    PrimalStatus(N)
    PrimalStatus()

The `ResultStatusCode` of the primal result `N`.
If `N` is omitted, it defaults to 1.
"""
struct PrimalStatus <: AbstractModelAttribute
    N::Int
end
PrimalStatus() = PrimalStatus(1)

"""
    DualStatus(N)
    DualStatus()

The `ResultStatusCode` of the dual result `N`.
If `N` is omitted, it defaults to 1.
"""
struct DualStatus <: AbstractModelAttribute
    N::Int
end
DualStatus() = DualStatus(1)
