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
    struct UnsupportedAttribute{AttrType} <: UnsupportedError
        attr::AttrType
        message::String
    end

An error indicating that the attribute `attr` is not supported by the model,
i.e. that [`supports`](@ref) returns `false`.
"""
struct UnsupportedAttribute{AttrType<:AnyAttribute} <: UnsupportedError
    attr::AttrType
    message::String
end
UnsupportedAttribute(attr::AnyAttribute) = UnsupportedAttribute(attr, "")
element_name(err::UnsupportedAttribute) = "Attribute $(err.attr)"

"""
    struct SetAttributeNotAllowed{AttrType} <: NotAllowedError
        attr::AttrType
        message::String # Human-friendly explanation why the attribute cannot be set
    end

An error indicating that the attribute `attr` is supported (see
[`supports`](@ref)) but cannot be set for some reason (see the error string).
"""
struct SetAttributeNotAllowed{AttrType<:AnyAttribute} <: NotAllowedError
    attr::AttrType
	message::String # Human-friendly explanation why the attribute cannot be set
end
SetAttributeNotAllowed(attr::AnyAttribute) = SetAttributeNotAllowed(attr, "")

operation_name(err::SetAttributeNotAllowed) = "Setting attribute $(err.attr)"
message(err::SetAttributeNotAllowed) = err.message

"""
    supports(model::ModelLike, attr::AbstractOptimizerAttribute)::Bool

Return a `Bool` indicating whether `model` supports the optimizer attribute
`attr`. That is, it returns `false` if `copy!(model, src)` shows a warning in
case `attr` is in the [`ListOfOptimizerAttributesSet`](@ref) of `src`; see
[`copy!`](@ref) for more details on how unsupported optimizer attributes are
handled in copy.

    supports(model::ModelLike, attr::AbstractModelAttribute)::Bool

Return a `Bool` indicating whether `model` supports the model attribute `attr`.
That is, it returns `false` if `copy!(model, src)` cannot be performed in case
`attr` is in the [`ListOfModelAttributesSet`](@ref) of `src`.

    supports(model::ModelLike, attr::AbstractVariableAttribute, ::Type{VariableIndex})::Bool

Return a `Bool` indicating whether `model` supports the variable attribute
`attr`. That is, it returns `false` if `copy!(model, src)` cannot be performed
in case `attr` is in the [`ListOfVariableAttributesSet`](@ref) of `src`.

    supports(model::ModelLike, attr::AbstractConstraintAttribute, ::Type{ConstraintIndex{F,S}})::Bool where {F,S}

Return a `Bool` indicating whether `model` supports the constraint attribute
`attr` applied to an `F`-in-`S` constraint. That is, it returns `false` if
`copy!(model, src)` cannot be performed in case `attr` is in the
[`ListOfConstraintAttributesSet`](@ref) of `src`.

For all four methods, if the attribute is only not supported in specific
circumstances, it should still return `true`.
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
get(model::ModelLike, attr::AnyAttribute, idxs::Vector) = get.(Ref(model), Ref(attr), idxs)

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
canget(::ModelLike, ::Union{AbstractModelAttribute, AbstractOptimizerAttribute}) = false
canget(::ModelLike, ::Union{AbstractVariableAttribute, AbstractConstraintAttribute}, ::Type{<:Index}) = false

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

An [`UnsupportedAttribute`](@ref) error is thrown if `model` does not support
the attribute `attr` (see [`supports`](@ref)) and a [`SetAttributeNotAllowed`](@ref)
error is thrown if it supports the attribute `attr` but it cannot be set.

### Replace set in a constraint

    set!(model::ModelLike, ::ConstraintSet, c::ConstraintIndex{F,S}, set::S)

Change the set of constraint `c` to the new set `set` which should be of the
same type as the original set.

#### Examples

If `c` is a `ConstraintIndex{F,Interval}`

```julia
set!(model, ConstraintSet(), c, Interval(0, 5))
set!(model, ConstraintSet(), c, GreaterThan(0.0))  # Error
```

### Replace function in a constraint

    set!(model::ModelLike, ::ConstraintFunction, c::ConstraintIndex{F,S}, func::F)

Replace the function in constraint `c` with `func`. `F` must match the original
function type used to define the constraint.

#### Examples

If `c` is a `ConstraintIndex{ScalarAffineFunction,S}` and `v1` and `v2` are
`VariableIndex` objects,

```julia
set!(model, ConstraintFunction(), c, ScalarAffineFunction([v1,v2],[1.0,2.0],5.0))
set!(model, ConstraintFunction(), c, SingleVariable(v1)) # Error
```
"""
function set! end
# See note with get
set!(model::ModelLike, attr::Union{AbstractVariableAttribute, AbstractConstraintAttribute}, idxs::Vector, vector_of_values::Vector) = set!.(Ref(model), Ref(attr), idxs, vector_of_values)

function set!(model::ModelLike, attr::AnyAttribute, args...)
    set!_fallback_error(model, attr, args...)
end
# set!_fallback_error is included so that we can return type-specific error
# messages without needing to overload set! and cause ambiguity errors. For
# examples, see ConstraintSet and ConstraintFunction. set!_fallback_error should
# not be overloaded by users of MOI.
function set!_fallback_error(model::ModelLike,
                             attr::Union{AbstractModelAttribute,
                                         AbstractOptimizerAttribute},
                             value)
    if supports(model, attr)
        throw(SetAttributeNotAllowed(attr))
    else
        throw(UnsupportedAttribute(attr))
    end
end
function set!_fallback_error(model::ModelLike,
                             attr::Union{AbstractVariableAttribute,
                                         AbstractConstraintAttribute},
                             index::Index, value)
    if supports(model, attr, typeof(index))
        throw(SetAttributeNotAllowed(attr))
    else
        throw(UnsupportedAttribute(attr))
    end
end

## Optimizer attributes

"""
    ListOfOptimizerAttributesSet()

An optimizer attribute for the `Vector{AbstractOptimizerAttribute}` of all optimizer attributes that were set.
"""
struct ListOfOptimizerAttributesSet <: AbstractOptimizerAttribute end

"""
    SolverName()

An optimizer attribute for the string identifying the solver/optimizer.
"""
struct SolverName <: AbstractOptimizerAttribute end

## Model attributes

"""
    ListOfModelAttributesSet()

A model attribute for the `Vector{AbstractModelAttribute}` of all model attributes that were set to the model.
"""
struct ListOfModelAttributesSet <: AbstractModelAttribute end

"""
    Name()

A model attribute for the string identifying the model.
"""
struct Name <: AbstractModelAttribute end

"""
    ObjectiveSense()

A model attribute for the `OptimizationSense` of the objective function, which can be `MinSense`, `MaxSense`, or `FeasiblitySense`.
"""
struct ObjectiveSense <: AbstractModelAttribute end

@enum OptimizationSense MinSense MaxSense FeasibilitySense

"""
    NumberOfVariables()

A model attribute for the number of variables in the model.
"""
struct NumberOfVariables <: AbstractModelAttribute end

"""
    ListOfVariableIndices()

A model attribute for the `Vector{VariableIndex}` of all variable indices present in the model
(i.e., of length equal to the value of `NumberOfVariables()`) in the order in
which they were added.
"""
struct ListOfVariableIndices <: AbstractModelAttribute end

"""
    ListOfConstraintIndices{F,S}()

A model attribute for the `Vector{ConstraintIndex{F,S}}` of all constraint indices of type
`F`-in-`S` in the model (i.e., of length equal to the value of
`NumberOfConstraints{F,S}()`) in the order in which they were added.
"""
struct ListOfConstraintIndices{F,S} <: AbstractModelAttribute end

"""
    NumberOfConstraints{F,S}()

A model attribute for the number of constraints of the type `F`-in-`S` present in the model.
"""
struct NumberOfConstraints{F,S} <: AbstractModelAttribute end

"""
    ListOfConstraints()

A model attribute for the list of tuples of the form `(F,S)`, where `F` is a function type
and `S` is a set type indicating that the attribute `NumberOfConstraints{F,S}()`
has value greater than zero.
"""
struct ListOfConstraints <: AbstractModelAttribute end

"""
    ObjectiveFunction{F<:AbstractScalarFunction}()

A model attribute for the objective function which has a type `F<:AbstractScalarFunction`.
`F` should be guaranteed to be equivalent but not necessarily identical to the function type provided by the user.
Throws an `InexactError` if the objective function cannot be converted to `F`,
e.g. the objective function is quadratic and `F` is `ScalarAffineFunction{Float64}` or
it has non-integer coefficient and `F` is `ScalarAffineFunction{Int}`.
"""
struct ObjectiveFunction{F<:AbstractScalarFunction} <: AbstractModelAttribute end

"""
    ObjectiveFunctionType()

A model attribute for the type `F` of the objective function set using the
`ObjectiveFunction{F}` attribute.

## Examples

In the following code, `attr` should be equal to `MOI.SingleVariable`:
```julia
x = MOI.addvariable!(model)
MOI.set!(model, MOI.ObjectiveFunction{MOI.SingleVariable}(),
         MOI.SingleVariable(x))
attr = MOI.get(model, MOI.ObjectiveFunctionType())
```
"""
struct ObjectiveFunctionType <: AbstractModelAttribute end

## Optimizer attributes

"""
    ObjectiveValue(resultidx::Int=1)

A model attribute for the objective value of the `resultindex`th primal result.
"""
struct ObjectiveValue <: AbstractModelAttribute
    resultindex::Int
    (::Type{ObjectiveValue})(resultindex=1) = new(resultindex)
end

"""
    ObjectiveBound()

A model attribute for the best known bound on the optimal objective value.
"""
struct ObjectiveBound <: AbstractModelAttribute end

"""
    RelativeGap()

A model attribute for the final relative optimality gap, defined as ``\\frac{|b-f|}{|f|}``, where ``b`` is the best bound and ``f`` is the best feasible objective value.
"""
struct RelativeGap <: AbstractModelAttribute  end

"""
    SolveTime()

A model attribute for the total elapsed solution time (in seconds) as reported by the optimizer.
"""
struct SolveTime <: AbstractModelAttribute end

"""
    SimplexIterations()

A model attribute for the cumulative number of simplex iterations during the optimization process.
In particular, for a mixed-integer program (MIP), the total simplex iterations for all nodes.
"""
struct SimplexIterations <: AbstractModelAttribute end

"""
    BarrierIterations()

A model attribute for the cumulative number of barrier iterations while solving a problem.
"""
struct BarrierIterations <: AbstractModelAttribute end

"""
    NodeCount()

A model attribute for the total number of branch-and-bound nodes explored while solving a mixed-integer program (MIP).
"""
struct NodeCount <: AbstractModelAttribute end

"""
    RawSolver()

A model attribute for the object that may be used to access a solver-specific API for this optimizer.
"""
struct RawSolver <: AbstractModelAttribute end

"""
    ResultCount()

A model attribute for the number of results available.
"""
struct ResultCount <: AbstractModelAttribute end

## Variable attributes

"""
    ListOfVariableAttributesSet()

A model attribute for the `Vector{AbstractVariableAttribute}` of all variable attributes that were set to the model.
"""
struct ListOfVariableAttributesSet <: AbstractModelAttribute end

"""
    VariableName()

A variable attribute for the string identifying the variable. It is invalid for two variables to have the same name.
"""
struct VariableName <: AbstractVariableAttribute end

"""
    VariablePrimalStart()

A variable attribute for the initial assignment to some primal variable's value that the optimizer may use to warm-start the solve.
"""
struct VariablePrimalStart <: AbstractVariableAttribute end

"""
    VariablePrimal(N)
    VariablePrimal()

A variable attribute for the assignment to some primal variable's value in result `N`.
If `N` is omitted, it is 1 by default.
"""
struct VariablePrimal <: AbstractVariableAttribute
    N::Int
end
VariablePrimal() = VariablePrimal(1)

"""
    VariableBasisStatus()

A variable attribute for the `BasisStatusCode` of some variable, with respect to an available optimal solution basis.
"""
struct VariableBasisStatus <: AbstractVariableAttribute end

"""
    BasisStatusCode

An Enum of possible values for the `VariableBasisStatus` and `ConstraintBasisStatus` attributes.
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

A model attribute for the `Vector{AbstractConstraintAttribute}` of all constraint attributes that were set to `F`-in-`S` constraints.

## Note

The attributes [`ConstraintFunction`](@ref) and [`ConstraintSet`](@ref) should
not be included in the list even if then have been set with [`set!`](@ref).
"""
struct ListOfConstraintAttributesSet{F,S} <: AbstractModelAttribute end

"""
    ConstraintName()

A constraint attribute for the string identifying the constraint. It is invalid for two constraints of any kind to have the same name.
"""
struct ConstraintName <: AbstractConstraintAttribute end

"""
    ConstraintPrimalStart()

A constraint attribute for the initial assignment to some constraint's primal value(s) that the optimizer may use to warm-start the solve.
"""
struct ConstraintPrimalStart <: AbstractConstraintAttribute end

"""
    ConstraintDualStart()

A constraint attribute for the initial assignment to some constraint's dual value(s) that the optimizer may use to warm-start the solve.
"""
struct ConstraintDualStart <: AbstractConstraintAttribute end

"""
    ConstraintPrimal(N)
    ConstraintPrimal()

A constraint attribute for the assignment to some constraint's primal value(s) in result `N`.
If `N` is omitted, it is 1 by default.

Given a constraint `function-in-set`, the `ConstraintPrimal` is the value of the
function evaluated at the primal solution of the variables. For example, given
the constraint `ScalarAffineFunction([x,y], [1, 2], 3)`-in-`Interval(0, 20)` and
a primal solution of `(x,y) = (4,5)`, the `ConstraintPrimal` solution of the
constraint is `1 * 4 + 2 * 5 + 3 = 17`.
"""
struct ConstraintPrimal <: AbstractConstraintAttribute
    N::Int
end
ConstraintPrimal() = ConstraintPrimal(1)

"""
    ConstraintDual(N)
    ConstraintDual()

A constraint attribute for the assignment to some constraint's dual value(s) in result `N`.
If `N` is omitted, it is 1 by default.
"""
struct ConstraintDual <: AbstractConstraintAttribute
    N::Int
end
ConstraintDual() = ConstraintDual(1)

"""
    ConstraintBasisStatus()

A constraint attribute for the `BasisStatusCode` of some constraint, with respect to an available optimal solution basis.
"""
struct ConstraintBasisStatus <: AbstractConstraintAttribute end

"""
    ConstraintFunction()

A constraint attribute for the `AbstractFunction` object used to define the constraint.
It is guaranteed to be equivalent but not necessarily identical to the function provided by the user.
"""
struct ConstraintFunction <: AbstractConstraintAttribute end

function set!_fallback_error(::ModelLike, attr::ConstraintFunction,
                     ::ConstraintIndex{F, S}, ::F) where {F <: AbstractFunction,
                                                          S}
    throw(SetAttributeNotAllowed(attr))
end
func_type(c::ConstraintIndex{F, S}) where {F, S} = F
function set!_fallback_error(::ModelLike, ::ConstraintFunction,
                     constraint_index::ConstraintIndex, func::AbstractFunction)
    throw(ArgumentError("""Cannot modify functions of different types.
    Constraint type is $(func_type(constraint_index)) while the replacement
    function is of type $(typeof(func))."""))
end

"""
    ConstraintSet()

A constraint attribute for the `AbstractSet` object used to define the constraint.
"""
struct ConstraintSet <: AbstractConstraintAttribute end

function set!_fallback_error(::ModelLike, attr::ConstraintSet, ::ConstraintIndex{F, S},
                     ::S) where {F, S <: AbstractSet}
    throw(SetAttributeNotAllowed(attr))
end
set_type(::ConstraintIndex{F, S}) where {F, S} = S
function set!_fallback_error(::ModelLike, ::ConstraintSet,
                     constraint_index::ConstraintIndex, set::AbstractSet)
    throw(ArgumentError("""Cannot modify sets of different types. Constraint
    type is $(set_type(constraint_index)) while the replacement set is of
    type $(typeof(set)). Use `transform!` instead."""))
end

## Termination status
"""
    TerminationStatus()

A model attribute for the `TerminationStatusCode` explaining why the optimizer stopped.
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

A model attribute for the `ResultStatusCode` of the primal result `N`.
If `N` is omitted, it defaults to 1.
"""
struct PrimalStatus <: AbstractModelAttribute
    N::Int
end
PrimalStatus() = PrimalStatus(1)

"""
    DualStatus(N)
    DualStatus()

A model attribute for the `ResultStatusCode` of the dual result `N`.
If `N` is omitted, it defaults to 1.
"""
struct DualStatus <: AbstractModelAttribute
    N::Int
end
DualStatus() = DualStatus(1)
