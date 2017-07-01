module MathOptInterface

"""
    AbstractSolver

Abstract supertype for "solver" objects.
A solver is a lightweight object used for selecting solvers and parameters.
It does not store any instance data.
"""
abstract type AbstractSolver end

"""
    AbstractInstance

Abstract supertype which represents a solver's in-memory representation of an optimization problem.
"""
abstract type AbstractInstance end

"""
    Instance(solver::AbstractSolver)

Create an instance of `AbstractInstance` using the given solver.
"""
function Instance end

"""
    optimize!(m::AbstractInstance)

Start the solution procedure.
"""
function optimize! end

"""
    freeinstance!(m::AbstractInstance)

Release any resources and memory used by the instance.
Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced.
This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs.
Users must discard the instance object after this method is invoked.
"""
function freeinstance! end

"""
    writeproblem(m::AbstractInstance, filename::String)

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
