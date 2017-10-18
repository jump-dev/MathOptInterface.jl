__precompile__()
module MathOptInterface

"""
    AbstractSolver

Abstract supertype for "solver" objects.
A solver is a lightweight object used for selecting solvers and parameters.
It does not store any solver instance data.
"""
abstract type AbstractSolver end

"""
    AbstractInstance

Abstract supertype for objects representing an instance of an optimization problem.
"""
abstract type AbstractInstance end

"""
    AbstractStandaloneInstance

Abstract supertype for objects representing an instance of an optimization problem
unattached to any particular solver. Does not have methods for solving
or querying results.
"""
abstract type AbstractStandaloneInstance <: AbstractInstance end


"""
    AbstractSolverInstance

Abstract supertype for objects representing an instance of an optimization problem
tied to a particular solver. This is typically a solver's in-memory representation.
In addition to `AbstractInstance`, `AbstractSolverInstance` objects let you
solve the instance and query the solution.
"""
abstract type AbstractSolverInstance <: AbstractInstance end

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
    writeproblem(m::AbstractInstance, filename::String)

Writes the current problem data to the given file.
Supported file types depend on the solver or standalone instance type.
"""
function writeproblem end

"""
    supportsproblem(s::AbstractSolver, objective_type::F, constraint_types::Vector)::Bool

Return `true` if the solver supports optimizing a problem with objective type `F` and constraints of the types specified by `constraint_types` which is a list of tuples `(F,S)` for `F`-in-`S` constraints. Return false if the solver does not support this problem class.

### Examples

```julia
supportsproblem(s, ScalarAffineFunction{Float64},
    [(ScalarAffineFunction{Float64},Zeros),
    (ScalarAffineFunction{Float64},LessThan{Float64}),
    (ScalarAffineFunction{Float64},GreaterThan{Float64})])
```
should be `true` for a linear programming solver `s`.

```julia
supportsproblem(s, ScalarQuadraticFunction{Float64},
    [(ScalarAffineFunction{Float64},Zeros),
    (ScalarAffineFunction{Float64},LessThan{Float64}),
    (ScalarAffineFunction{Float64},GreaterThan{Float64})])
```
should be `true` for a quadratic programming solver `s`.

```julia
supportsproblem(s, ScalarAffineFunction{Float64},
    [(ScalarAffineFunction{Float64},Zeros),
    (ScalarAffineFunction{Float64},LessThan{Float64}),
    (ScalarAffineFunction{Float64},GreaterThan{Float64}),
    (SingleVariable,ZeroOne)])
```
should be `true` for a mixed-integer linear programming solver `s`.

```julia
supportsproblem(s, ScalarAffineFunction{Float64},
    [(ScalarAffineFunction{Float64},Zeros),
    (ScalarAffineFunction{Float64},LessThan{Float64}),
    (ScalarAffineFunction{Float64},GreaterThan{Float64}),
    (VectorAffineFunction{Float64},SecondOrderCone)])
```
should be `true` for a second-order cone solver `s`.

"""
function supportsproblem end

"""
    coefficienttype(m::AbstractSolver)

Returns the coefficient type used by the solver. Defaults to Float64.
"""
function coefficienttype(::AbstractSolver); Float64; end

"""
    coefficienttype(m::AbstractInstance)

Returns the coefficient type used by the instance. Defaults to Float64.
"""
function coefficienttype(::AbstractInstance); Float64; end


include("references.jl")
include("attributes.jl")
include("functions.jl")
include("sets.jl")
include("constraints.jl")
include("objectives.jl")
include("variables.jl")

end
