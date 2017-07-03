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

Create a solver instance of `AbstractSolverInstance` using the given solver.
"""
function SolverInstance end

"""
    optimize!(m::AbstractSolverInstance)

Start the solution procedure.
"""
function optimize! end

"""
    freesolverinstance!(m::AbstractSolverInstance)

Release any resources and memory used by the solver instance.
Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced.
This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs.
Users must discard the solver instance object after this method is invoked.
"""
function freesolverinstance! end

"""
    writeproblem(m::AbstractSolverInstance, filename::String)

Writes the current problem data to the given file.
Supported file types are solver-dependent.
"""
function writeproblem end


include("attributes.jl")
include("constraints.jl")
include("objectives.jl")
include("references.jl")
include("sets.jl")
include("variables.jl")

end
