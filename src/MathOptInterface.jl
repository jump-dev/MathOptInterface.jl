module MathOptInterface

"""
    AbstractSolver

Abstract supertype for "solver" objects.
A solver is a lightweight object used for selecting solvers and parameters.
It does not store any instance data.
"""
abstract type AbstractSolver end

"""
    AbstractModel

Abstract supertype which represents a solver's in-memory representation of an optimization problem.
"""
abstract type AbstractModel end

"""
    Model(solver::AbstractSolver)

Create an instance of ``AbstractModel`` using the given solver.
"""
function Model end

"""
    optimize!(m::AbstractModel)

Start the solution procedure.
"""
function optimize! end

"""
    freemodel!(m::AbstractModel)

Release any resources and memory used by the model.
Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced.
This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs.
Users must discard the model object after this method is invoked.
"""
function freemodel! end

"""
    writeproblem(m::AbstractModel, filename::String)

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
