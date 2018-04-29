
# Manual

## Purpose

Each mathematical optimization solver API has its own concepts and data structures for representing optimization models and obtaining results.
However, it is often desirable to represent an instance of an optimization problem at a higher level so that it is easy to try using different solvers.
MathOptInterface (MOI) is an abstraction layer designed to provide a unified interface to mathematical optimization solvers so that users do not need to understand multiple solver-specific APIs.
MOI can be used directly, or through a higher-level modeling interface like [JuMP](https://github.com/JuliaOpt/JuMP.jl).

MOI has been designed to replace [MathProgBase](https://github.com/JuliaOpt/MathProgBase.jl), which has been used by modeling packages such as [JuMP](https://github.com/JuliaOpt/JuMP.jl) and [Convex.jl](https://github.com/JuliaOpt/Convex.jl).
This second-generation abstraction layer addresses a number of limitations of MathProgBase.
MOI is designed to:
- Be simple and extensible, unifying linear, quadratic, and conic optimization, and seamlessly facilitate extensions to essentially arbitrary constraints and functions (e.g., indicator constraints, complementarity constraints, and piecewise linear functions)
- Be fast by allowing access to a solver's in-memory representation of a problem without writing intermediate files (when possible) and by using multiple dispatch and avoiding requiring containers of nonconcrete types
- Allow a solver to return multiple results (e.g., a pool of solutions)
- Allow a solver to return extra arbitrary information via attributes (e.g., variable- and constraint-wise membership in an irreducible inconsistent subset for infeasibility analysis)
- Provide a greatly expanded set of status codes explaining what happened during the optimization procedure
- Enable a solver to more precisely specify which problem classes it supports
- Enable both primal and dual warm starts
- Enable adding and removing both variables and constraints by indices that are not required to be consecutive
- Enable any modification that the solver supports to an existing model
- Avoid requiring the solver wrapper to store an additional copy of the problem data

This manual introduces the concepts needed to understand MOI and give a high-level picture of how all of the pieces fit together. The primary focus is on MOI from the perspective of a user of the interface. At the end of the manual we have a section on [Implementing a solver interface](@ref).
The [API Reference](@ref) page lists the complete API.

MOI does not export functions, but for brevity we often omit qualifying names with the MOI module. Best practice is to have
```julia
using MathOptInterface
const MOI = MathOptInterface
```
and prefix all MOI methods with `MOI.` in user code. If a name is also available in base Julia, we always explicitly use the module prefix, for example, with `MOI.get`.

## Standard form problem

The standard form problem is:

```math
\begin{align}
    & \min_{x \in \mathbb{R}^n} & f_0(x)
    \\
    & \;\;\text{s.t.} & f_i(x) & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```

where:
* the functions ``f_0, f_1, \ldots, f_m`` are specified by [`AbstractFunction`](@ref MathOptInterface.AbstractFunction) objects
* the sets ``\mathcal{S}_1, \ldots, \mathcal{S}_m`` are specified by [`AbstractSet`](@ref MathOptInterface.AbstractSet) objects

The current function types are:
* **[`SingleVariable`](@ref MathOptInterface.SingleVariable)**: ``x_j``, i.e., projection onto a single coordinate defined by a variable index ``j``
* **[`VectorOfVariables`](@ref MathOptInterface.VectorOfVariables)**: projection onto multiple coordinates (i.e., extracting a subvector)
* **[`ScalarAffineFunction`](@ref MathOptInterface.ScalarAffineFunction)**: ``a^T x + b``, where ``a`` is a vector and ``b`` scalar
* **[`VectorAffineFunction`](@ref MathOptInterface.VectorAffineFunction)**: ``A x + b``, where ``A`` is a matrix and ``b`` is a vector
* **[`ScalarQuadraticFunction`](@ref MathOptInterface.ScalarQuadraticFunction)**: ``\frac{1}{2} x^T Q x + a^T x + b``, where ``Q`` is a symmetric matrix, ``a`` is a vector, and ``b`` is a constant
* **[`VectorQuadraticFunction`](@ref MathOptInterface.VectorQuadraticFunction)**: a vector of scalar-valued quadratic functions

Extensions for nonlinear programming are present but not yet well documented.

MOI defines some commonly used sets, but the interface is extensible to other sets recognized by the solver.

* **[`LessThan(upper)`](@ref MathOptInterface.LessThan)**: ``\{ x \in \mathbb{R} : x \le \mbox{upper} \}``
* **[`GreaterThan(lower)`](@ref MathOptInterface.GreaterThan)**: ``\{ x \in \mathbb{R} : x \ge \mbox{lower} \}``
* **[`EqualTo(value)`](@ref MathOptInterface.GreaterThan)**: ``\{ x \in \mathbb{R} : x = \mbox{value} \}``
* **[`Interval(lower, upper)`](@ref MathOptInterface.Interval)**: ``\{ x \in \mathbb{R} : x \in [\mbox{lower},\mbox{upper}] \}``
* **[`Reals(dimension)`](@ref MathOptInterface.Reals)**: ``\mathbb{R}^\mbox{dimension}``
* **[`Zeros(dimension)`](@ref MathOptInterface.Zeros)**: ``0^\mbox{dimension}``
* **[`Nonnegatives(dimension)`](@ref MathOptInterface.Nonnegatives)**: ``\{ x \in \mathbb{R}^\mbox{dimension} : x \ge 0 \}``
* **[`Nonpositives(dimension)`](@ref MathOptInterface.Nonpositives)**: ``\{ x \in \mathbb{R}^\mbox{dimension} : x \le 0 \}``
* **[`SecondOrderCone(dimension)`](@ref MathOptInterface.SecondOrderCone)**: ``\{ (t,x) \in \mathbb{R}^\mbox{dimension} : t \ge ||x||_2 \}``
* **[`RotatedSecondOrderCone(dimension)`](@ref MathOptInterface.RotatedSecondOrderCone)**: ``\{ (t,u,x) \in \mathbb{R}^\mbox{dimension} : 2tu \ge ||x||_2^2, t,u \ge 0 \}``
* **[`GeometricMeanCone(dimension)`](@ref MathOptInterface.GeometricMeanCone)**: ``\{ (t,x) \in \mathbb{R}^{n+1} : x \ge 0, t \le \sqrt[n]{x_1 x_2 \cdots x_n} \}`` where ``n`` is ``dimension - 1``
* **[`ExponentialCone()`](@ref MathOptInterface.ExponentialCone)**: ``\{ (x,y,z) \in \mathbb{R}^3 : y \exp (x/y) \le z, y > 0 \}``
* **[`DualExponentialCone()`](@ref MathOptInterface.DualExponentialCone)**: ``\{ (u,v,w) \in \mathbb{R}^3 : -u \exp (v/u) \le exp(1) w, u < 0 \}``
* **[`PowerCone(exponent)`](@ref MathOptInterface.PowerCone)**: ``\{ (x,y,z) \in \mathbb{R}^3 : x^\mbox{exponent} y^{1-\mbox{exponent}} \ge |z|, x,y \ge 0 \}``
* **[`DualPowerCone(exponent)`](@ref MathOptInterface.DualPowerCone)**: ``\{ (u,v,w) \in \mathbb{R}^3 : \frac{u}{\mbox{exponent}}^\mbox{exponent} \frac{v}{1-\mbox{exponent}}^{1-\mbox{exponent}} \ge |w|, u,v \ge 0 \}``
* **[`PositiveSemidefiniteConeTriangle(dimension)`](@ref MathOptInterface.PositiveSemidefiniteConeTriangle)**: ``\{ X \in \mathbb{R}^{\mbox{dimension}(\mbox{dimension}+1)/2} : X \mbox{is the upper triangle of a PSD matrix}\}``
* **[`PositiveSemidefiniteConeSquare(dimension)`](@ref MathOptInterface.PositiveSemidefiniteConeSquare)**: ``\{ X \in \mathbb{R}^{\mbox{dimension}^2} : X \mbox{is a PSD matrix}\}``
* **[`LogDetConeTriangle(dimension)`](@ref MathOptInterface.LogDetConeTriangle)**: ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}(1+\mbox{dimension})/2} : t \le \log(\det(X)), X \mbox{is the upper triangle of a PSD matrix}\}``
* **[`LogDetConeSquare(dimension)`](@ref MathOptInterface.LogDetConeSquare)**: ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}^2} : t \le \log(\det(X)), X \mbox{is a PSD matrix}\}``
* **[`RootDetConeTriangle(dimension)`](@ref MathOptInterface.RootDetConeTriangle)**: ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}(1+\mbox{dimension})/2} : t \le det(X)^{1/\mbox{dimension}}, X \mbox{is the upper triangle of a PSD matrix}\}``
* **[`RootDetConeSquare(dimension)`](@ref MathOptInterface.RootDetConeSquare)**: ``\{ (t,X) \in \mathbb{R}^{1+\mbox{dimension}^2} : t \le \det(X)^{1/\mbox{dimension}}, X \mbox{is a PSD matrix}\}``
* **[`Integer()`](@ref MathOptInterface.Integer)**: ``\mathbb{Z}``
* **[`ZeroOne()`](@ref MathOptInterface.ZeroOne)**: ``\{ 0, 1 \}``
* **[`Semicontinuous(lower,upper)`](@ref MathOptInterface.Semicontinuous)**: ``\{ 0\} \cup [lower,upper]``
* **[`Semiinteger(lower,upper)`](@ref MathOptInterface.Semiinteger)**: ``\{ 0\} \cup \{lower,lower+1,\ldots,upper-1,upper\}``


## The `ModelLike` and `AbstractOptimizer` APIs

The most significant part of MOI is the definition of the **model API** that is
used to specify an instance of an optimization problem (e.g., by adding
variables and constraints). Objects that implement the model API should inherit
from the [`ModelLike`](@ref MathOptInterface.ModelLike) abstract type.

Notably missing from the model API is the method to solve an optimization problem.
`ModelLike` objects may store an instance (e.g., in memory or backed by a file format)
without being linked to a particular solver. In addition to the model API, MOI
defines [`AbstractOptimizer`](@ref MathOptInterface.AbstractOptimizer). *Optimizers*
(or solvers) implement the model API (inheriting from `ModelLike`) and additionally
provide methods to solve the model.

Through the rest of the manual, `model` is used as a generic `ModelLike`, and
`optimizer` is used as a generic `AbstractOptimizer`.

[Discuss how models are constructed, optimizer attributes.]

## Variables

All variables in MOI are scalar variables.
New scalar variables are created with [`addvariable!`](@ref MathOptInterface.addvariable!) or [`addvariables!`](@ref MathOptInterface.addvariables!), which return a [`VariableIndex`](@ref MathOptInterface.VariableIndex) or `Vector{VariableIndex}` respectively. `VariableIndex` objects are type-safe wrappers around integers that refer to a variable in a particular model.

One uses `VariableIndex` objects to set and get variable attributes. For example, the [`VariablePrimalStart`](@ref MathOptInterface.VariablePrimalStart) attribute is used to provide an initial starting point for a variable or collection of variables:
```julia
v = addvariable!(model)
set!(model, VariablePrimalStart(), v, 10.5)
v2 = addvariables!(model, 3)
set!(model, VariablePrimalStart(), v2, [1.3,6.8,-4.6])
```

A variable can be deleted from a model with [`delete!(::ModelLike, ::VariableIndex)`](@ref MathOptInterface.delete!(::MathOptInterface.ModelLike, ::MathOptInterface.Index)). Not all models support deleting variables; the [`candelete`](@ref MathOptInterface.candelete) method is used to check if this is supported.

## Functions

MOI defines six functions as listed in the definition of the [Standard form problem](@ref). The simplest function is [`SingleVariable`](@ref MathOptInterface.SingleVariable) defined as:
```julia
struct SingleVariable <: AbstractFunction
    variable::VariableIndex
end
```

If `v` is a `VariableIndex` object, then `SingleVariable(v)` is simply the scalar-valued function from the complete set of variables in a model that returns the value of variable `v`. One may also call this function a coordinate projection, which is more useful for defining constraints than as an objective function.


A more interesting function is [`ScalarAffineFunction`](@ref MathOptInterface.ScalarAffineFunction), defined as
```julia
struct ScalarAffineFunction{T} <: AbstractFunction
    variables::Vector{VariableIndex}
    coefficients::Vector{T}
    constant::T
end
```

If `x` is a vector of `VariableIndex` objects, then `ScalarAffineFunction([x[1],x[2]],[5.0,-2.3],1.0)` represents the function ``5x_1 - 2.3x_2 + 1``.

Objective functions are assigned to a model by setting the [`ObjectiveFunction`](@ref MathOptInterface.ObjectiveFunction) attribute.
The [`ObjectiveSense`](@ref MathOptInterface.ObjectiveSense) attribute is used for setting the optimization sense.
For example,
```julia
x = addvariables!(model, 2)
set!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarAffineFunction([x[1],x[2]],[5.0,-2.3],1.0))
set!(model, ObjectiveSense(), MinSense)
```
sets the objective to the function just discussed in the minimization sense.

See [Functions and function modifications](@ref) for the complete list of functions.

## Sets and Constraints

All constraints are specified with [`addconstraint!`](@ref MathOptInterface.addconstraint!)
by restricting the output of some function to a set.
The interface allows an arbitrary combination of functions and sets, but of course
solvers may decide to support only a small number of combinations.

For example, linear programming solvers should support, at least, combinations of affine functions with
the [`LessThan`](@ref MathOptInterface.LessThan) and [`GreaterThan`](@ref MathOptInterface.GreaterThan)
sets. These are simply linear constraints.
`SingleVariable` functions combined with these same sets are used to specify upper and lower bounds on variables.

The code example below encodes the linear optimization problem:
```math
\begin{align}
& \max_{x \in \mathbb{R}^2} & 3x_1 + 2x_2 &
\\
& \;\;\text{s.t.} & x_1 + x_2 &\le 5
\\
&& x_1 & \ge 0
\\
&&x_2 & \ge -1
\end{align}
```

```julia
x = addvariables!(model, 2)
set!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarAffineFunction(x, [3.0,2.0], 0.0))
set!(model, ObjectiveSense(), MaxSense)
addconstraint!(model, ScalarAffineFunction(x, [1.0,1.0], 0.0), LessThan(5.0))
addconstraint!(model, SingleVariable(x[1]), GreaterThan(0.0))
addconstraint!(model, SingleVariable(x[2]), GreaterThan(-1.0))
```

Besides scalar-valued functions in scalar-valued sets it possible to use vector-valued functions and sets.

The code example below encodes the convex optimization problem:
```math
\begin{align}
& \max_{x,y,z \in \mathbb{R}} & y + z &
\\
& \;\;\text{s.t.} & 3x &= 2
\\
&& x & \ge ||(y,z)||_2
\end{align}
```

```julia
x,y,z = addvariables!(model, 3)
set!(model, ObjectiveFunction{ScalarAffineFunction{Float64}}(), ScalarAffineFunction([y,z], [1.0,1.0], 0.0))
set!(model, ObjectiveSense(), MaxSense)
addconstraint!(model, VectorAffineFunction([1],[x],[3.0],[-2.0]), Zeros(1))
addconstraint!(model, VectorOfVariables([x,y,z]), SecondOrderCone(3))
```

[Describe `ConstraintIndex` objects.]

### Constraints by function-set pairs

Below is a list of common constraint types and how they are represented
as function-set pairs in MOI. In the notation below, ``x`` is a vector of decision variables,
``x_i`` is a scalar decision variable, and all other terms are fixed constants.

[Define notation more precisely. ``a`` vector; ``A`` matrix; don't reuse ``u,l,b`` as scalar and vector]

#### Linear constraints

| Mathematical Constraint       | MOI Function                 | MOI Set        |
|-------------------------------|------------------------------|----------------|
| ``a^Tx \le u``                | `ScalarAffineFunction`       | `LessThan`     |
| ``a^Tx \ge l``                | `ScalarAffineFunction`       | `GreaterThan`  |
| ``a^Tx = b``                  | `ScalarAffineFunction`       | `EqualTo`      |
| ``l \le a^Tx \le u``          | `ScalarAffineFunction`       | `Interval`     |
| ``x_i \le u``                 | `SingleVariable`             | `LessThan`     |
| ``x_i \ge l``                 | `SingleVariable`             | `GreaterThan`  |
| ``x_i = b``                   | `SingleVariable`             | `EqualTo`      |
| ``l \le x_i \le u``           | `SingleVariable`             | `Interval`     |
| ``Ax + b \in \mathbb{R}_+^n`` | `VectorAffineFunction`       | `Nonnegatives` |
| ``Ax + b \in \mathbb{R}_-^n`` | `VectorAffineFunction`       | `Nonpositives` |
| ``Ax + b = 0``                | `VectorAffineFunction`       | `Zeros`        |

By convention, solvers are not expected to support nonzero constant terms in the `ScalarAffineFunction`s the first four rows above, because they are redundant with the parameters of the sets. For example, ``2x + 1 \le 2`` should be encoded as ``2x \le 1``.

Constraints with `SingleVariable` in `LessThan`, `GreaterThan`, `EqualTo`, or `Interval` sets have a natural interpretation as variable bounds. As such, it is typically not natural to impose multiple lower or upper bounds on the same variable, and by convention we do not ask solver interfaces to support this. It is natural, however, to impose upper and lower bounds separately as two different constraints on a single variable. The difference between imposing bounds by using a single `Interval` constraint and by using separate `LessThan` and `GreaterThan` constraints is that the latter will allow the solver to return separate dual multipliers for the two bounds, while the former will allow the solver to return only a single dual for the interval constraint.

[Define ``\mathbb{R}_+, \mathbb{R}_-``]

#### Conic constraints


| Mathematical Constraint                                       | MOI Function                 | MOI Set                            |
|---------------------------------------------------------------|------------------------------|------------------------------------|
| ``\lVert Ax + b\rVert_2 \le c^Tx + d``                        | `VectorAffineFunction`       | `SecondOrderCone`                  |
| ``y \ge \lVert x \rVert_2``                                   | `VectorOfVariables`          | `SecondOrderCone`                  |
| ``2yz \ge \lVert x \rVert_2^2, y,z \ge 0``                    | `VectorOfVariables`          | `RotatedSecondOrderCone`           |
| ``(a_1^Tx + b_1,a_2^Tx + b_2,a_3^Tx + b_3) \in \mathcal{E}``  | `VectorAffineFunction`       | `ExponentialCone`                  |
| ``A(x) \in \mathcal{S}_+``                                    | `VectorAffineFunction`       | `PositiveSemidefiniteConeTriangle` |
| ``A(x) \in \mathcal{S}'_+``                                   | `VectorAffineFunction`       | `PositiveSemidefiniteConeSquare`   |
| ``x \in \mathcal{S}_+``                                       | `VectorOfVariables`          | `PositiveSemidefiniteConeTriangle` |
| ``x \in \mathcal{S}'_+``                                      | `VectorOfVariables`          | `PositiveSemidefiniteConeSquare`   |


[Define ``\mathcal{E}`` (exponential cone), ``\mathcal{S}_+`` (smat), ``\mathcal{S}'_+`` (svec). ``A(x)`` is an affine function of ``x`` that outputs a matrix.]

#### Quadratic constraints


| Mathematical Constraint       | MOI Function                 | MOI Set                       |
|-------------------------------|------------------------------|-------------------------------|
| ``x^TQx + a^Tx + b \ge 0``    | `ScalarQuadraticFunction`    | `GreaterThan`                 |
| ``x^TQx + a^Tx + b \le 0``    | `ScalarQuadraticFunction`    | `LessThan`                    |
| ``x^TQx + a^Tx + b = 0``      | `ScalarQuadraticFunction`    | `EqualTo`                     |
| Bilinear matrix inequality    | `VectorQuadraticFunction`    | `PositiveSemidefiniteCone...` |


#### Discrete and logical constraints


| Mathematical Constraint                                                                    | MOI Function        | MOI Set                            |
|--------------------------------------------------------------------------------------------|---------------------|------------------------------------|
| ``x_i \in \mathbb{Z}``                                                                     | `SingleVariable`    | `Integer`                          |
| ``x_i \in \{0,1\}``                                                                        | `SingleVariable`    | `ZeroOne`                          |
| ``x_i \in \{0\} \cup [l,u]``                                                               | `SingleVariable`    | `Semicontinuous`                   |
| ``x_i \in \{0\} \cup \{l,l+1,\ldots,u-1,u\}``                                              | `SingleVariable`    | `Semiinteger`                      |
| At most one component of ``x`` can be nonzero                                              | `VectorOfVariables` | `SOS1`                             |
| At most two components of ``x`` can be nonzero, and if so they must be adjacent components | `VectorOfVariables` | `SOS2`                             |

## Solving and retrieving the results

Once an optimizer is loaded with the objective function and all of the constraints, we can ask the solver to solve the model by calling [`optimize!`](@ref MathOptInterface.optimize!).
```julia
optimize!(optimizer)
```

The optimization procedure may terminate for a number of reasons. The [`TerminationStatus`](@ref MathOptInterface.TerminationStatus) attribute of the optimizer returns a [`TerminationStatusCode`](@ref MathOptInterface.TerminationStatusCode) object which explains why the solver stopped. Some statuses indicate generally successful termination, some termination because of limit, and some termination because of something unexpected like invalid problem data or failure to converge. A typical usage of the `TerminationStatus` attribute is as follows:
```julia
status = MOI.get(optimizer, TerminationStatus())
if status == Success
    # Ok, the solver has a result to return
else
    # Handle other cases
    # The solver may or may not have a result
end
```

The `Success` status code specifically implies that the solver has a "result" to return. In the case that the solver converged to an optimal solution, this result will just be the optimal solution vector. The [`PrimalStatus`](@ref MathOptInterface.PrimalStatus) attribute returns a [`ResultStatusCode`](@ref MathOptInterface.ResultStatusCode) that explains how to interpret the result. In the case that the solver is known to return globally optimal solutions (up to numerical tolerances), the combination of `Success` termination status and `FeasiblePoint` primal result status implies that the primal result vector should be interpreted as a globally optimal solution. A result may be available even if the status is not `Success`, for example, if the solver stopped because of a time limit and has a feasible but nonoptimal solution. Use the [`ResultCount`](@ref MathOptInterface.ResultCount) attribute to check if one or more results are available.

In addition to the primal status, the [`DualStatus`](@ref MathOptInterface.DualStatus) provides important information for primal-dual solvers.

If a result is available, it may be retrieved with the [`VariablePrimal`](@ref MathOptInterface.VariablePrimal) attribute:
```julia
MOI.get(optimizer, VariablePrimal(), x)
```
If `x` is a `VariableIndex` then the function call returns a scalar, and if `x` is a `Vector{VariableIndex}` then the call returns a vector of scalars. `VariablePrimal()` is equivalent to `VariablePrimal(1)`, i.e., the variable primal vector of the first result. Use `VariablePrimal(N)` to access the `N`th result.

See also the attributes [`ConstraintPrimal`](@ref MathOptInterface.ConstraintPrimal), and [`ConstraintDual`](@ref MathOptInterface.ConstraintDual).
See [Duals](@ref) for a discussion of the MOI conventions for primal-dual pairs and certificates.



### Common status situations

The sections below describe how to interpret different status cases for three common classes of solvers. Most importantly, it is essential to know if a solver is expected to provide a global or only locally optimal solution when interpreting the result statuses. Solver wrappers may provide additional information on how the solver's statuses map to MOI statuses.

#### Primal-dual convex solver

A typical primal-dual solver is capable of certifying optimality of a solution to a convex optimization problem by providing a primal-dual feasible solution with matching objective values. It may also certify that either the primal or dual problem is infeasible by providing a certain ray of the dual or primal, respectively. Typically two solves are required to certify unboundedness, one to find a ray and a second to find a feasible point. A solver may also provide a [facial reduction certificate](http://www.optimization-online.org/DB_FILE/2015/09/5104.pdf). When a primal-dual solver terminates with `Success` status, it is reasonable to assume that a primal and dual statuses of `FeasiblePoint` imply that the corresponding primal-dual results are a (numerically) optimal primal-dual pair. The `AlmostSuccess` status implies that the solve has completed to relaxed tolerances, so in this case `FeasiblePoint` or `NearlyFeasiblePoint` statuses would imply a near-optimal primal-dual pair. For all other termination statuses, there are no specific guarantees on the results returned.

#### Global mixed-integer or nonconvex solver

When a global solver returns `Success` and the primal result is a `FeasiblePoint`, then it is implied that the primal result is indeed a globally optimal solution up to the specified tolerances. Typically, no dual certificate is available to certify optimality. The [`ObjectiveBound`](@ref MathOptInterface.ObjectiveBound) should provide extra information on the optimality gap.

#### Local solver

For solvers which perform a search based only on local criteria (for example, gradient descent), without additional knowledge of the structure of the problem, we can say only that `Success` and `FeasiblePoint` imply that the primal result belongs to the class of points which the chosen algorithm is known to converge to. Gradient descent algorithms may converge to saddle points, for example. It is also possible for such algorithms to converge to infeasible points, in which case the termination status would be `Success` and the primal result status would be `InfeasiblePoint`. This does not imply that the problem is infeasible and so cannot be called a certificate of infeasibility.


## A complete example: solving a knapsack problem


[ needs formatting help, doc tests ]

```julia
using MathOptInterface
const MOI = MathOptInterface
using MathOptInterfaceGLPK

# Solve the binary-constrained knapsack problem: max c'x: w'x <= C, x binary using GLPK.

c = [1.0, 2.0, 3.0]
w = [0.3, 0.5, 1.0]
C = 3.2

numvariables = length(c)

optimizer = MathOptInterfaceGLPK.GLPKOptimizerMIP()

# create the variables in the problem
x = MOI.addvariables!(optimizer, numvariables)

# set the objective function
MOI.set!(optimizer, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), MOI.ScalarAffineFunction(x, c, 0.0))
MOI.set!(optimizer, MOI.ObjectiveSense(), MOI.MaxSense)

# add the knapsack constraint
MOI.addconstraint!(optimizer, MOI.ScalarAffineFunction(x, w, 0.0), MOI.LessThan(C))

# add integrality constraints
for i in 1:numvariables
    MOI.addconstraint!(optimizer, MOI.SingleVariable(x[i]), MOI.ZeroOne())
end

# all set
MOI.optimize!(optimizer)

termination_status = MOI.get(optimizer, TerminationStatus())
objvalue = MOI.canget(optimizer, MOI.ObjectiveValue()) ? MOI.get(optimizer, MOI.ObjectiveValue()) : NaN
if termination_status != MOI.Success
    error("Solver terminated with status $termination_status")
end

@assert MOI.get(optimizer, MOI.ResultCount()) > 0

result_status = MOI.get(optimizer, MOI.PrimalStatus())
if result_status != MOI.FeasiblePoint
    error("Solver ran successfully did not return a feasible point. The problem may be infeasible.")
end
primal_variable_result = MOI.get(optimizer, MOI.VariablePrimal(), x)

@show objvalue
@show primal_variable_result
```

## Advanced

### Duals


Conic duality is the starting point for MOI's duality conventions. When all functions are affine (or coordinate projections), and all constraint sets are closed convex cones, the model may be called a conic optimization problem.
For conic-form minimization problems, the primal is:

```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align}
```

and the dual is:

```math
\begin{align}
& \max_{y_1, \ldots, y_m} & -\sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 - \sum_{i=1}^m A_i^T y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```

where each ``\mathcal{C}_i`` is a closed convex cone and ``\mathcal{C}_i^*`` is its dual cone.

For conic-form maximization problems, the primal is:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^T x + b_0
\\
& \;\;\text{s.t.} & A_i x + b_i & \in \mathcal{C}_i & i = 1 \ldots m
\end{align}
```

and the dual is:

```math
\begin{align}
& \min_{y_1, \ldots, y_m} & \sum_{i=1}^m b_i^T y_i + b_0
\\
& \;\;\text{s.t.} & a_0 + \sum_{i=1}^m A_i^T y_i & = 0
\\
& & y_i & \in \mathcal{C}_i^* & i = 1 \ldots m
\end{align}
```



A linear inequality constraint ``a^T x + b \ge c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_+``, and similarly ``a^T x + b \le c`` should be interpreted as ``a^T x + b - c \in \mathbb{R}_-``.
Variable-wise constraints should be interpreted as affine constraints with the appropriate identity mapping in place of ``A_i``.

For the special case of minimization LPs, the MOI primal form can be stated as
```math
\begin{align}
& \min_{x \in \mathbb{R}^n} & a_0^T x &+ b_0
\\
& \;\;\text{s.t.}
&A_1 x & \ge b_1\\
&& A_2 x & \le b_2\\
&& A_3 x & = b_3
\end{align}
```

By applying the stated transformations to conic form, taking the dual, and transforming back into linear inequality form, one obtains the following dual:

```math
\begin{align}
& \max_{y_1,y_2,y_3} & b_1^Ty_1 + b_2^Ty_2 + b_3^Ty_3 &+ b_0
\\
& \;\;\text{s.t.}
&A_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3 & = a_0\\
&& y_1 &\ge 0\\
&& y_2 &\le 0
\end{align}
```

For maximization LPs, the MOI primal form can be stated as:
```math
\begin{align}
& \max_{x \in \mathbb{R}^n} & a_0^T x &+ b_0
\\
& \;\;\text{s.t.}
&A_1 x & \ge b_1\\
&& A_2 x & \le b_2\\
&& A_3 x & = b_3
\end{align}
```

and similarly, the dual is:
```math
\begin{align}
& \min_{y_1,y_2,y_3} & -b_1^Ty_1 - b_2^Ty_2 - b_3^Ty_3 &+ b_0
\\
& \;\;\text{s.t.}
&A_1^Ty_1 + A_2^Ty_2 + A_3^Ty_3 & = -a_0\\
&& y_1 &\ge 0\\
&& y_2 &\le 0
\end{align}
```

An important note for the LP case is that the signs of the feasible duals depend only on the sense of the inequality and not on the objective sense.


Currently, a convention for duals is not defined for problems with non-conic sets ``\mathcal{S}_i`` or quadratic functions ``f_0, f_i``.

#### Duality and scalar product

The scalar product is different from the canonical one for the sets [`PositiveSemidefiniteConeTriangle`](@ref MathOptInterface.PositiveSemidefiniteConeTriangle), [`LogDetConeTriangle`](@ref MathOptInterface.LogDetConeTriangle), [`RootDetConeTriangle`](@ref MathOptInterface.RootDetConeTriangle).
If the set ``C_i`` of the section [Duals](@ref) is one of these three cones,
then the rows of the matrix ``A_i`` corresponding to off-diagonal entries are twice the value of the `coefficients` field in the `VectorAffineFunction` for the corresponding rows.
See [`PositiveSemidefiniteConeTriangle`](@ref MathOptInterface.PositiveSemidefiniteConeTriangle) for details.

### Modifying a model

[Explain `modifyconstraint!` and `modifyobjective!`.]

### Constraint bridges

A constraint often possess different equivalent formulations, but a solver may only support one of them.
It would be duplicate work to implement rewritting rules in every solver wrapper for every different formulation of the constraint to express it in the form supported by the solver.
Constraint bridges provide a way to define a rewritting rule on top of the MOI interface which can be used by any optimizer.
Some rules also implement constraint modifications and constraint primal and duals translations.

For example, the `SplitIntervalBridge` defines the reformulation of a `ScalarAffineFunction`-in-`Interval` constraint into a `ScalarAffineFunction`-in-`GreaterThan` and a `ScalarAffineFunction`-in-`LessThan` constraint.
The `SplitInterval` is the bridge optimizer that applies the `SplitIntervalBridge` rewritting rule.
Given an optimizer `optimizer` implementing `ScalarAffineFunction`-in-`GreaterThan` and `ScalarAffineFunction`-in-`LessThan`, the optimizer
```
bridgedoptimizer = SplitInterval(optimizer)
```
will additionally support `ScalarAffineFunction`-in-`Interval`.

## Implementing a solver interface

[The interface is designed for multiple dispatch, e.g., attributes, combinations of sets and functions.]

[Avoid storing extra copies of the problem when possible.]

[`copy!`]

### JuMP mapping

MOI defines a very general interface, with multiple possible ways to describe the same constraint. This is considered a feature, not a bug. MOI is designed to make it possible to experiment with alternative representations of an optimization problem at both the solving and modeling level. When implementing an interface, it is important to keep in mind that the constraints which a solver supports via MOI will have a near 1-to-1 correspondence with how users can express problems in JuMP, because JuMP does not perform automatic transformations. (Alternative systems like Convex.jl do.) The following bullet points show examples of how JuMP constraints are translated into MOI function-set pairs:
 - `@constraint(m, 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`LessThan`;
 - `@constraint(m, 2x + y >= 10)` becomes `ScalarAffineFunction`-in-`GreaterThan`;
 - `@constraint(m, 2x + y == 10)` becomes `ScalarAffineFunction`-in-`EqualTo`;
 - `@constraint(m, 0 <= 2x + y <= 10)` becomes `ScalarAffineFunction`-in-`Interval`;
 - `@constraint(m, 2x + y in ArbitrarySet())` becomes `ScalarAffineFunction`-in-`ArbitrarySet`.

Variable bounds are handled in a similar fashion:
 - `@variable(m, x <= 1)` becomes `SingleVariable`-in-`LessThan`;
 - `@variable(m, x >= 1)` becomes `SingleVariable`-in-`GreaterThan`.

One notable difference is that a variable with an upper and lower bound is translated into two constraints, rather than an interval. i.e.:
 - `@variable(m, 0 <= x <= 1)` becomes `SingleVariable`-in-`LessThan` *and* `SingleVariable`-in-`GreaterThan`.

Therefore, if a solver wrapper does not support `ScalarAffineFunction`-in-`LessThan` constraints, users will not be able to write: `@constraint(m, 2x + y <= 10)` in JuMP. With this in mind, developers should support all the constraint types that they want to be usable from JuMP. That said, from the perspective of JuMP, solvers can safely choose to not support the following constraints:

- `ScalarAffineFunction` in `GreaterThan`, `LessThan`, or `EqualTo` with a nonzero constant in the function. Constants in the affine function should instead be moved into the parameters of the corresponding sets.

- `ScalarAffineFunction` in `Nonnegative`, `Nonpositive` or `Zeros`. Alternative constraints are available by using a `VectorAffineFunction` with one output row or `ScalarAffineFunction` with `GreaterThan`, `LessThan`, or `EqualTo`.

- Two `SingleVariable`-in-`LessThan` constraints applied to the same variable (similarly with `GreaterThan`). These should be interpreted as variable bounds, and each variable naturally has at most one upper or lower bound.

### Column Generation

There is no special interface for column generation. If the solver has a special API for setting
coefficients in existing constraints when adding a new variable, it is possible
to queue modifications and new variables and then call the solver's API once all of the
new coefficients are known.

### Problem data

All data passed to the solver should be copied immediately to internal data structures. Solvers may not modify any input vectors and should not assume that input vectors will not be modified by users in the future. This applies, for example, to the coefficient vector in `ScalarAffineFunction`. Vectors returned to the user, e.g., via `ObjectiveFunction` or `ConstraintFunction` attributes should not be modified by the solver afterwards. The in-place version of `get!` can be used by users to avoid extra copies in this case.

### Statuses

Solver wrappers should document how the low-level solver statuses map to the MOI statuses. In particular, the characterization of a result with status `FeasiblePoint` and termination status `Success` is entirely solver defined. It may or may not be a globally optimal solution. Solver wrappers are not responsible for verifying the feasibility of results. Statuses like `NearlyFeasiblePoint`, `InfeasiblePoint`, `NearlyInfeasiblePoint`, and `NearlyReductionCertificate` are designed to be used when the solver explicitly indicates as much.

### canXXX

For most operations, MOI provides a function `canXXX` that can be used to check if the operation `XXX` is allowed.
For example, `addconstraint!(model::ModelLike, func::F, set::S)` has the corresponding function `canaddconstraint(model, ::Type{F}, ::Type{S})::Bool`.
If `canaddconstraint` returns `false`, then calling `addconstraint!` must throw an error (likewise with all `XXX` and `canXXX` pairs). Note that even if `canaddconstraint` returns `true`, `addconstraint!` may still throw an error if, for example, the constraint function does not meet some sparsity conditions, one of the coefficients is `NaN`, or an invalid variable index is provided. This is because (in most cases) the `canXXX` method receives type information instead of the actual arguments.

### Package Naming

MOI solver interfaces may be in the same package as the solver itself (either the C wrapper if the solver is accessible through C, or the Julia code if the solver is written in Julia, for example).
The guideline for naming the file containing the MOI wrapper is `src/MOIWrapper.jl` and `test/MOIWrapper.jl` for the tests.
If the MOI wrapper implementation is spread in several files, they should be stored in a `src/MOIWrapper` folder and included by a `src/MOIWrapper/MOIWrapper.jl` file.
In some cases it may be more appropriate to host the MOI wrapper in its own package; in this case it is recommended that the MOI wrapper package be named `MathOptInterfaceXXX` where `XXX` is the solver name.

### Testing guideline

The skeleton below can be used for the wrapper test file of a solver name `FooBar`. A few bridges are used to give examples, you can find more bridges in the [Bridges](@ref) section.
```julia
using MathOptInterface
const MOI = MathOptInterface
const MOIT = MOI.Test
const MOIB = MOI.Bridges

const optimizer = FooBarOptimizer()
const config = MOIT.TestConfig(atol=1e-6, rtol=1e-6)

@testset "MOI Continuous Linear" begin
    # If `optimizer` does not support the `Interval` set,
    # the `SplitInterval` bridge can be used to split each `f`-in-`Interval(lb, ub)` constraint into
    # a constraint `f`-in-`GreaterThan(lb)` and a constraint `f`-in-`LessThan(ub)`
    MOIT.contlineartest(MOIB.SplitInterval{Float64}(optimizer), config)
end

@testset "MOI Continuous Conic" begin
    # If the solver supports rotated second order cone, the `GeoMean` bridge can be used to make it support geometric mean cone constraints.
    # If it additionally support positive semidefinite cone constraints, the `RootDet` bridge can be used to make it support root-det cone constraints.
    MOIT.contlineartest(MOIB.RootDet{Float64}(MOIB.GeoMean{Float64}(optimizer)), config)
end

@testset "MOI Integer Conic" begin
    MOIT.intconictest(optimizer, config)
end
```

If the wrapper does not support building the model incrementally (i.e. with `addvariable!` and `addconstraint!`), the line `const optimizer = FooBarOptimizer()` can be replaced with
```julia
const MOIU = MOI.Utilities
# Include here the functions/sets supported by the solver wrapper (not those that are supported through bridges)
MOIU.@model FooBarModelData () (EqualTo, GreaterThan, LessThan) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)
const optimizer = MOIU.CachingOptimizer(FooBarModelData{Float64}(), FooBarOptimizer())
```
