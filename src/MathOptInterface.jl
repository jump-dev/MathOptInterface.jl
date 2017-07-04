module MathOptInterface

"""
    AbstractSolver

Abstract supertype for "solver" objects.
A solver is a lightweight object used for selecting solvers and parameters.
It does not store any solver instance data.
"""
abstract type AbstractSolver end

"""
    AbstractSolverInstance

Abstract supertype which represents a solver's in-memory representation of an optimization problem.
"""
abstract type AbstractSolverInstance end

"""
    SolverInstance(solver::AbstractSolver)

Create a solver instance from the given solver.
"""
function SolverInstance end

"""
    optimize!(m::AbstractSolverInstance)

Start the solution procedure.
"""
function optimize! end

"""
    free!(m::AbstractSolverInstance)

Release any resources and memory used by the solver instance.
Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced.
This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs.
Users must discard the solver instance object after this method is invoked.
"""
function free! end

"""
    writeproblem(m::AbstractSolverInstance, filename::String)

Writes the current problem data to the given file.
Supported file types are solver-dependent.
"""
function writeproblem end

"""
    supportsproblem(s::AbstractSolver, objective_types::Vector, constriant_types::Vector)::Bool

Return `true` if the solver supports optimizing a problem with objective types listed in `objective_types` (which should have one element except for the case of multiobjective optimization) and constraints of the types specified by `constraint_types` which is a list of tuples `(F,S)` for `F`-in-`S` constraints. Return false if the solver does not support this problem class.

    supportsproblem(s::AbstractSolver, objective_type::F, constriant_types::Vector)::Bool

Return `true` if the solver supports optimizing a problem with objective type `F` and constraints of the types specified by `constraint_types` which is a list of tuples `(F,S)` for `F`-in-`S` constraints. Return false if the solver does not support this problem class.
### Examples

```julia
supportsproblem(s, ScalarAffineFunction,
    [(ScalarAffineFunction,Zeros),
    (ScalarAffineFunction,LessThan),
    (ScalarAffineFunction,GreaterThan)])
```
should be `true` for a linear programming solver.

```julia
supportsproblem(s, ScalarQuadraticFunction,
    [(ScalarAffineFunction,Zeros),
    (ScalarAffineFunction,LessThan),
    (ScalarAffineFunction,GreaterThan)])
```
should be `true` for a quadratic programming solver.

```julia
supportsproblem(s, ScalarAffineFunction,
    [(ScalarAffineFunction,Zeros),
    (ScalarAffineFunction,LessThan),
    (ScalarAffineFunction,GreaterThan),
    (ScalarVariablewiseFunction,ZeroOne)])
```
should be `true` for a mixed-integer linear programming solver.

```julia
supportsproblem(s, ScalarAffineFunction,
    [(ScalarAffineFunction,Zeros),
    (ScalarAffineFunction,LessThan),
    (ScalarAffineFunction,GreaterThan),
    (VectorAffineFunction,SecondOrderCone)])
```
should be `true` for a second-order cone solver.

"""
function supportsproblem end

include("attributes.jl")
include("constraints.jl")
include("objectives.jl")
include("references.jl")
include("functions.jl")
include("sets.jl")
include("variables.jl")

end
