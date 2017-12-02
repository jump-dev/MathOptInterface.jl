__precompile__()
module MathOptInterface

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
    optimize!(instance::AbstractSolverInstance)

Start the solution procedure.
"""
function optimize! end

"""
    free!(instance::AbstractSolverInstance)

Release any resources and memory used by the solver instance.
Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced.
This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs.
Users must discard the solver instance object after this method is invoked.
"""
function free! end

"""
    write(instance::AbstractInstance, filename::String)

Writes the current instance data to the given file.
Supported file types depend on the solver or standalone instance type.
"""
function write end

"""
    read!(instance::AbstractInstance, filename::String)

Read the file `filename` into the instance `instance`. If `m` is non-empty, this may
throw an error.

Supported file types depend on the instance type.
"""
function read! end

"""
    copy!(dest::AbstractInstance, src::AbstractInstance)

Copy the model from the instance `src` into the instance `dest`. The target instance `dest` must be empty. Returns a dictionary-like object that translates variable and constraint references from the `src` instance to the corresponding references in the `dest` instance.

### Example

```julia
# Given `AbstractInstance`s `src` and `dest`.
# `dest` must be empty.

x = addvariable!(src)
isvalid(src, x)   # true
isvalid(dest, x)  # false (`dest` has no variables)

reference_map = copy!(dest, src)

isvalid(dest, x) # false (unless x == reference_map[x])
isvalid(dest, reference_map[x]) # true
```
"""
function copy! end

include("references.jl")
include("attributes.jl")
include("functions.jl")
include("sets.jl")
include("constraints.jl")
include("objectives.jl")
include("variables.jl")

end
