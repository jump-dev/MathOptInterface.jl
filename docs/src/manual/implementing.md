```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Implementing a solver interface

This guide outlines the basic steps to implement an interface to
MathOptInterface for a new solver.

!!! warning
    Implementing an interface to MathOptInterface for a new solver is a lot of
    work. Before starting, we recommend that you join the
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) and explain a
    little bit about the solver you are wrapping. If you have questions that are
    not answered by this guide, please ask them in the [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev)
    so we can improve this guide!

## Deciding if MathOptInterface is right for you

The first step in writing a wrapper is to decide whether implementing an
interface is the right thing to do.

MathOptInterface is an abstraction layer for unifying _constrained_ mathematical
optimization solvers. If your solver doesn't fit in the category, i.e., it
implements a derivative-free algorithm for unconstrained objective functions,
MathOptInterface may not be the right tool for the job.

!!! tip
    If you're not sure whether you should write an interface, ask in the
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev).

## Find a similar solver already wrapped

The next step is to find (if possible) a similar solver that is already wrapped.
Although not strictly necessary, this will be a good place to look for
inspiration when implementing your wrapper.

The [JuMP documentation](https://jump.dev/JuMP.jl/stable/installation/#Supported-solvers)
has a good list of solvers, along with the problem classes they support.

!!! tip
    If you're not sure which solver is most similar, ask in the
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev).

## Create a low-level interface

Before writing a MathOptInterface, you first been to be able to call the solver
from Julia.

### Wrapping solvers written in Julia

If your solver is written in Julia, there's nothing to do here! Go to the next
section.

### Solvers written in C

Julia is well suited to wrapping solvers written in C. (This is _not_ true for
C++.)

Before writing a MathOptInterface wrapper, there are a few extra steps.

#### Create a JLL

If the C code is publicly available under an open-source license, create a
JLL package via [Yggdrasil](https://github.com/JuliaPackaging/Yggdrasil). The
easiest way to do this is to copy an existing solver. Good examples to follow
are the [COIN-OR solvers](https://github.com/JuliaPackaging/Yggdrasil/tree/master/C/Coin-OR).

!!! warning
    Building the solver via Yggdrasil is non-trivial. please ask the
    [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) for help.

If the code is commercial or not publicly available, the user will need to
manually install the solver. See [Gurobi.jl](https://github.com/jump-dev/Gurobi.jl)
or [CPLEX.jl](https://github.com/jump-dev/CPLEX.jl) for examples of how to
structure this.

#### Use Clang.jl to wrap the C API

The next step is to use [Clang.jl](https://github.com/JuliaInterop/Clang.jl) to
automatically wrap the C API. The easiest way to do this is to follow an
example. Good examples to follow are
[Cbc.jl](https://github.com/jump-dev/Cbc.jl/blob/master/scripts/clang.jl) and
[HiGHS.jl](https://github.com/jump-dev/HiGHS.jl/blob/master/gen/gen.jl).

Sometimes, you will need to make manual modifications to the resulting files.

### Solvers written in other languages

Ask the [Developer chatroom](https://gitter.im/JuliaOpt/JuMP-dev) for advice.
You may be able to use on of the JuliaInterop packages to call out to the
solver.

For example, [SeDuMi.jl](https://github.com/jump-dev/SeDuMi.jl) uses
[MATLAB.jl](https://github.com/JuliaInterop/MATLAB.jl) to call the SeDuMi solver
written in MATLAB.

If your solver is written in C++, you will first need to write a C interface.

## Structuring the package

Structure your wrapper as a Julia package.

MOI solver interfaces may be in the same package as the solver itself (either
the C wrapper if the solver is accessible through C, or the Julia code if the
solver is written in Julia, for example), or in a separate package which depends
on the solver package.

!!! note
    The JuMP [core contributors](https://jump.dev/pages/governance/#core-contributors)
    request that you do not use "JuMP" in the name of your package without prior
    consent.

Create a file named `src/MOI_wrapper.jl` for the MOI wrapper, and a file named
`test/MOI_wrapper.jl` for the tests.

If the MOI wrapper implementation is spread in several files, they should be
stored in a `src/MOI_wrapper` folder and included by a
`src/MOI_wrapper/MOI_wrapper.jl` file.

For example:
```
/gen
    gen.jl  # Code to wrap the C API
/src
    NewSolver.jl
    /gen
        libnewsolver_api.jl
        libnewsolver_common.jl
    /MOI_wrapper
        MOI_wrapper.jl
        other_files.jl
/test
    runtests.jl
    /MOI_wrapper
        MOI_wrapper.jl
```

## Setup tests

The best way to implement an interface to MathOptInterface is via
[test-driven development](https://en.wikipedia.org/wiki/Test-driven_development).

The [`MOI.Test` submodule](@ref test_module) contains a large test suite to help
check that you have implemented things correctly.

Follow the guide [How to test a solver](@ref) to set up the
`test/MOI_wrapper.jl` file for your package.

!!! tip
    Run the tests frequently when developing. However, at the start there is
    going to be a lot of errors! Comment out all the `test_` functions but one,
    run the tests, implement any missing methods until the test passes, then
    uncomment another test and repeat.

## The `Optimizer` object

The first object to create is a subtype of `AbstractOptimizer`. By convention,
these optimizers should not be exported and should be named
`PackageName.Optimizer`.

```julia
import MathOptInterface
const MOI = MathOptInterface

struct Optimizer <: MOI.AbstractOptimizer
    # Fields go here
end
```

### Optimizer objects for C solvers

!!! warning
    This section is important if you wrap a solver written in C.

Wrapping a solver written in C will require the use of pointers. **Never pass
the pointer directly to a low-level function.** Instead, store the pointer as a
field in your `Optimizer`, and implement `Base.cconvert` and
`Base.unsafe_convert as follows:

```julia
struct Optimizer <: MOI.AbstractOptimizer
    ptr::Ptr{Cvoid}
end

Base.cconvert(::Type{Ptr{Cvoid}}, model::Optimizer) = model
Base.unsafe_convert(::Type{Ptr{Cvoid}}, model::Optimizer) = model.ptr
```

Then, pass `model` instead of `model.ptr` to low-level functions that expect the
model pointer.

### Implement methods for `Optimizer`

Now that we have an `Optimizer`, we need to implement a few basic methods.

* [`empty!`](@ref) and [`is_empty`](@ref)

!!! tip
    For this and all future methods, read the docstrings to understand what each
    method does, what it expects as input, and what it produces as output. If it
    isn't clear, let us know and we will improve the docstrings!

You should also implement `Base.show(::IO, ::Optimizer)` to print a nice string
when some prints your model. For example
```julia
function Base.show(io::IO, model::Optimizer)
    return print(io, "NewSolver with the pointer $(model.ptr)")
end
```

### Implement attributes

You also need to implement the model and optimizer attributes in the following
table.

For each attribute
 * [`get`](@ref) gets the current value of the attribute
 * [`set`](@ref) sets a new value of the attribute. Not all attributes can be
   set. For example, the user can't modify the [`SolverName`](@ref).
 * [`supports`](@ref) returns a `Bool` indicating whether the solver supports the
   attribute. Only implement this if you implement the getter and setter!

| Attribute              | [`get`](@ref) | [`set`](@ref) | [`supports`](@ref) |
| ---------------------- | --------------| ------------- | ------------------ |
| [`SolverName`](@ref)   | Yes           | No            | No                 |
| [`RawSolver`](@ref)    | Yes           | No            | No                 |
| [`Name`](@ref)         | Yes           | Yes           | Yes                |
| [`Silent`](@ref)       | Yes           | Yes           | Yes                |
| [`TimeLimitSec`](@ref) | Yes           | Yes           | Yes                |
| [`RawParameter`](@ref) | Yes           | Yes           | Yes                |
| [`NumberOfThreads`](@ref) | Yes        | Yes           | Yes                |

For example:
```julia
function MOI.get(model::Optimizer, ::MOI.Silent)
    return # true if MOI.Silent is set
end

function MOI.set(model::Optimizer, ::MOI.Silent, v::Bool)
    if v
        # Set a parameter to turn off printing
    else
        # Restore the default printing
    end
    return
end

MOI.supports(::Optimizer, ::MOI.Silent) = true
```

## Define `supports`

The next step is to define which constraints and objective functions you plan to
support.

For each function-set constraint pair, define [`supports_constraint`](@ref):
```julia
function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.SingleVariable},
    ::Type{MOI.ZeroOne},
)
    return true
end
```
To make this easier, you may want to use `Union`s:
```julia
function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.SingleVariable},
    ::Type{<:Union{MOI.LessThan,MOI.GreaterThan,MOI.EqualTo}},
)
    return true
end
```

You also need to define which objective functions you support via
[`supports`](@ref):
```julia
function MOI.supports(
    ::Model,
    ::MOI.ObjectiveFunction{MOI.SingleVariable},
)
    return true
end
```

!!! tip
    Only support a constraint or objective function if your solver has native
    support for it.

## The first big decision: incremental modifications?

The first big decision you face is whether to support incremental modification.

Incremental modification means that you can add variables and constraints
one-by-one without needing to rebuild the entire problem, and you can modify
the problem data after an [`optimize!`](@ref) call. Supporting incremental
modification means implementing functions like [`add_variable`](@ref) and
[`add_constraint`](@ref).

The alternative is to accept the problem data in a single [`copy_to`](@ref)
function call, afterwhich it cannot be modified. Because [`copy_to`](@ref) sees
all of the data at once, it can typically call a more efficient function to load
data into the underlying solver.

Good examples of solvers supporting incremental modification are MILP solvers
like [GLPK.jl](https://github.com/jump-dev/GLPK.jl) and
[Gurobi.jl](https://github.com/jump-dev/Gurobi.jl). Examples of [`copy_to`](@ref)
solvers are [AmplNLWriter.jl](https://github.com/jump-dev/AmplNLWriter.jl) and
[SCS.jl](https://github.com/jump-dev/SCS.jl)

It is possible to implement both approaches, but you should probably start with
one for simplicity.

!!! tip
    Only support incremental modification if your solver has native support for
    it.

In general, supporting incremental modification is more work, and it usually
requires some extra book-keeping. However, it provides a more efficient
interface to the solver if the problem is going to be resolved multiple times
with small modifications. Moreover, once you've implemented incremental
modification, it's usually not much extra work to add a [`copy_to`](@ref)
interface. The converse is not true.

!!! tip
    If this is your first time writing an interface, start with
    [`copy_to`](@ref).

## The `copy_to` interface

To implement the [`copy_to`](@ref) interface, implement the following function:

* [`copy_to`](@ref)

## The incremental interface

To implement the incremental interface, implement the following functions:

* [`add_variable`](@ref)
* [`add_variables`](@ref)
* [`add_constraint`](@ref)
* [`add_constraints`](@ref)
* [`is_valid`](@ref)
* [`delete`](@ref)

In addition, you should implement the following attributes:

* [`ObjectiveFunction`](@ref)
* [`ObjectiveSense`](@ref)
* [`ConstraintFunction`](@ref)
* [`ConstraintSet`](@ref)
* [`NumberOfConstraints`](@ref)
* [`NumberOfVariables`](@ref)

### Modifications

If your solver supports modifying data in-place, implement:

* [`modify`](@ref)
* [`ScalarConstantChange`](@ref)
* [`ScalarCoefficientChange`](@ref)
* [`VectorConstantChange`](@ref)
* [`MultirowChange`](@ref)

### Incremental and `copy_to`

If you implement the incremental interface, you have the option of also
implementing [`copy_to`](@ref). If you don't want to implement
[`copy_to`](@ref), e.g., because the solver has no API for building the problem
in a single function call, define the following fallback:
```julia
function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike; kwargs...)
    return MOI.Utilities.automatic_copy_to(dest, src; kwargs...)
end

function MOI.Utilities.supports_default_copy_to(
    model::Optimizer,
    copy_names::Bool,
)
    # If you support names...
    return true
    # Otherwise...
    return !copy_names
end
```
See [`Utilities.supports_default_copy_to`](@ref) for more details.

## [Names](@id implement_names)

Regardless of which interface you implement, you have the option of implementing
the `Name` attribute for variables and constraints:

* [`VariableName`](@ref)
* [`ConstraintName`](@ref)

If you implement names, you should also implement the following three methods:
```julia
function MOI.get(model::Optimizer, ::Type{MOI.VariableIndex}, name::String)
    return # The variable named `name`.
end

function MOI.get(model::Optimizer, ::Type{MOI.ConstraintIndex}, name::String)
    return # The constraint any type named `name`.
end

function MOI.get(
    model::Optimizer,
    ::Type{MOI.ConstraintIndex{F,S}},
    name::String,
) where {F,S}
    return # The constraint of type F-in-S named `name`.
end
```

These methods have the following rules:

* If there is no variable or constraint with the name, return `nothing`
* If there is a single variable or constraint with that name, return the
  variable or constraint
* If there are multiple variables or constraints with the name, throw an error.

## Solutions

Implement [`optimize!`](@ref) to solve the model:

* [`optimize!`](@ref)

At a minimum, implement the following attributes to allow the user to access
solution information.

* [`TerminationStatus`](@ref)
* [`PrimalStatus`](@ref)
* [`DualStatus`](@ref)
* [`RawStatusString`](@ref)
* [`ResultCount`](@ref)
* [`ObjectiveValue`](@ref)
* [`VariablePrimal`](@ref)
* [`SolveTime`](@ref)

!!! note
    Solver wrappers should document how the low-level statuses map to the MOI
    statuses. Statuses like `NEARLY_FEASIBLE_POINT` and `INFEASIBLE_POINT`, are
    designed to be used when the solver explicitly indicates that relaxed
    tolerances are satisfied or the returned point is infeasible, respectively.

!!! tip
    Attributes like [`VariablePrimal`](@ref) and [`ObjectiveValue`](@ref) are
    indexed by the result count. Use
    `MOI.check_result_index_bounds(model, attr)` to throw an error if the
    attribute is not available.

If your solver returns dual solutions, implement:

* [`ConstraintDual`](@ref)
* [`DualObjectiveValue`](@ref)

For integer solvers, implement:

* [`ObjectiveBound`](@ref)
* [`RelativeGap`](@ref)

If applicable, implement:

* [`SimplexIterations`](@ref)
* [`BarrierIterations`](@ref)
* [`NodeCount`](@ref)

If your solver uses the Simplex method, implement:

* [`ConstraintBasisStatus`](@ref)

## Warm-starts

If your solver accepts primal or dual warm-starts, implement:

* [`VariablePrimalStart`](@ref)
* [`ConstraintDualStart`](@ref)

## Extra: solver-specific attributes

You don't need to restrict yourself to the attributes defined in the
MathOptInterface.jl package.

Solver-specific attributes should be specified by creating an appropriate
subtype of [`AbstractModelAttribute`](@ref), [`AbstractOptimizerAttribute`](@ref),
[`AbstractVariableAttribute`](@ref), or [`AbstractConstraintAttribute`](@ref).

For example, Gurobi.jl adds attributes for multiobjective optimization by
[defining](https://github.com/jump-dev/Gurobi.jl/blob/d9cebe4ec05a102df8917ff2602e6c38abdac090/src/MOI_multi_objective.jl#L1-L15):
```julia
struct NumberOfObjectives <: MOI.AbstractModelAttribute end

function MOI.set(model::Optimizer, ::NumberOfObjectives, n::Integer)
    # Code to set NumberOfOBjectives
    return
end

function MOI.get(model::Optimizer, ::NumberOfObjectives)
    n = # Code to get NumberOfobjectives
    return n
end
```

Then, the user can write:
```julia
model = Gurobi.Optimizer()
MOI.set(model, Gurobi.NumberofObjectives(), 3)
```

## Extra: variables constrained on creation

The solver interface should only implement support for variables
constrained on creation (see
[`add_constrained_variable`](@ref)/[`add_constrained_variables`](@ref)) or
constraints that directly map to a structure exploited by the solver algorithm.
There is no need to add support for additional types, this is handled by
[The Bridges submodule](@ref). Furthermore, this allows
[`supports_constraint`](@ref) to indicate which types are exploited by the
solver and hence allows layers such as [`Bridges.LazyBridgeOptimizer`](@ref)
to accurately select the most appropriate transformations.

As [`add_constrained_variable`](@ref) (resp. [`add_constrained_variables`](@ref))
falls back to [`add_variable`](@ref) (resp. [`add_variables`](@ref)) followed by
[`add_constraint`](@ref), there is no need to implement this function
if `model` does not require that variables be constrained when they are created.
However, if `model` requires that variables be constrained when they're created,
then it should only implement [`add_constrained_variable`](@ref) and not
[`add_variable`](@ref) nor [`add_constraint`](@ref) for
[`SingleVariable`](@ref)-in-`typeof(set)`. In addition, it should implement
`supports_add_constrained_variables(::Optimizer, ::Type{Reals})` and return
`false` so that these variables are bridged, see
[`supports_add_constrained_variables`](@ref).

## Other tips

### Unsupported constraints at runtime

In some cases, your solver may support a particular type of constraint (e.g.,
quadratic constraints), but only if the data meets some condition (e.g., it is
convex).

In this case, declare that you `support` the constraint, and throw
[`AddConstraintNotAllowed`](@ref).

### Dealing with multiple variable bounds

MathOptInterface uses [`SingleVariable`](@ref) constraints to represent variable
bounds. Defining multiple variable bounds on a single variable is not allowed.

Throw [`LowerBoundAlreadySet`](@ref) or [`UpperBoundAlreadySet`](@ref) if the
user adds a constraint that results in multiple bounds.

Only throw if the constraints conflict. It is okay to add
[`SingleVariable`](@ref)-in-[`GreaterThan`](@ref) and then
[`SingleVariable`](@ref)-in-[`LessThan`](@ref), but not
[`SingleVariable`](@ref)-in-[`Interval`](@ref) and then
[`SingleVariable`](@ref)-in-[`LessThan`](@ref),

### Expect duplicate coefficients

Solvers should expect that functions such as [`ScalarAffineFunction`](@ref) and
[`VectorQuadraticFunction`](@ref) may contain duplicate coefficents.

For example,
`ScalarAffineFunction([ScalarAffineTerm(x, 1), ScalarAffineTerm(x, 1)], 0.0)`.

Use [`Utilities.canonical`](@ref) to return a new function with the duplicate
coefficients aggregated together.

### Don't modify user-data

All data passed to the solver should be copied immediately to internal data
structures. Solvers may not modify any input vectors and should assume that
input vectors may be modified by users in the future.

This applies, for example, to the `terms` vector in
[`ScalarAffineFunction`](@ref). Vectors returned to the user, e.g., via
[`ObjectiveFunction`](@ref) or
[`ConstraintFunction`](@ref) attributes, should not be modified by the solver
afterwards. The in-place version of [`get!`](@ref) can be used by users to avoid
extra copies in this case.

## Column Generation

There is no special interface for column generation. If the solver has a special
API for setting coefficients in existing constraints when adding a new variable,
it is possible to queue modifications and new variables and then call the
solver's API once all of the new coefficients are known.

## JuMP mapping

MOI defines a very general interface, with multiple possible ways to describe
the same constraint.

This is considered a feature, not a bug.

MOI is designed to make it possible to experiment with alternative
representations of an optimization problem at both the solving and modeling
level.

When implementing an interface, it is important to keep in mind that the way the
user can express problems in JuMP is not directly limited by the constraints
which a solver supports via MOI as JuMP performs automatic reformulation
via [The Bridges submodule](@ref).

Therefore, we recommend to only support the constraint types that directly map
to a structure exploited by the solver algorithm.

The following bullet points show examples of how JuMP constraints are translated
into MOI function-set pairs:

 - `@constraint(m, 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`LessThan`
 - `@constraint(m, 2x + y >= 10)` becomes `ScalarAffineFunction`-in-`GreaterThan`
 - `@constraint(m, 2x + y == 10)` becomes `ScalarAffineFunction`-in-`EqualTo`
 - `@constraint(m, 0 <= 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`Interval`
 - `@constraint(m, 2x + y in ArbitrarySet())` becomes
   `ScalarAffineFunction`-in-`ArbitrarySet`.

Variable bounds are handled in a similar fashion:

 - `@variable(m, x <= 1)` becomes `SingleVariable`-in-`LessThan`
 - `@variable(m, x >= 1)` becomes `SingleVariable`-in-`GreaterThan`

One notable difference is that a variable with an upper and lower bound is
translated into two constraints, rather than an interval. i.e.:

 - `@variable(m, 0 <= x <= 1)` becomes `SingleVariable`-in-`LessThan` *and*
    `SingleVariable`-in-`GreaterThan`.

Solvers are not expected to support `AbstractScalarFunction` in `GreaterThan`,
`LessThan`, `EqualTo`, or `Interval` with a nonzero constant in the function.
Constants in the affine function should instead be moved into the parameters of
the corresponding sets. The [`ScalarFunctionConstantNotZero`](@ref) exception
may be thrown in this case.
