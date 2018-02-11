__precompile__()
module MathOptInterface

"""
    ModelLike

Abstract supertype for objects that implement the "Model" interface for defining
an optimization problem.
"""
abstract type ModelLike end

"""
    AbstractOptimizer

Abstract supertype for objects representing an instance of an optimization problem
tied to a particular solver. This is typically a solver's in-memory representation.
In addition to `ModelLike`, `AbstractOptimizer` objects let you solve the
instance and query the solution.
"""
abstract type AbstractOptimizer <: ModelLike end

"""
    optimize!(optimizer::AbstractOptimizer)

Start the solution procedure.
"""
function optimize! end

"""
    free!(optimizer::AbstractOptimizer)

Release any resources and memory used by the optimizer.
Note that the Julia garbage collector takes care of this automatically, but automatic collection cannot always be forced.
This method is useful for more precise control of resources, especially in the case of commercial solvers with licensing restrictions on the number of concurrent runs.
Users must discard the optimizer object after this method is invoked.
"""
function free! end

"""
    write(model::ModelLike, filename::String)

Writes the current model data to the given file.
Supported file types depend on the solver or standalone instance type.
"""
function write end

"""
    read!(model::ModelLike, filename::String)

Read the file `filename` into the instance `instance`. If `m` is non-empty, this may
throw an error.

Supported file types depend on the instance type.
"""
function read! end

"""
    isempty(model::ModelLike)

Returns `false` if the `model` has any instance attribute set or has any variables or constraints.
Note that an empty model can have optimizer parameters set.
"""
function isempty end

"""
    empty!(model::ModelLike)

Empty the model, that is, remove all variables, constraints and model attributes but not optimizer parameters.
"""
function empty! end

"""
    CopyStatusCode

An Enum of possible statuses returned by a `copy!` operation through the `CopyResult` struct.

* `CopySuccess`: The copy was successful.
* `CopyUnsupportedAttribute`: The copy failed because the destination does not support an attribute present in the source.
* `CopyUnsupportedConstraint`: The copy failed because the destination does not support a constraint present in the source.
* `CopyOtherError`: The copy failed for a different reason.

In the failure cases:

- See the corresponding `message` field of the `CopyResult` for an explanation of the failure.
- The state of the destination instance is undefined.
"""
@enum CopyStatusCode CopySuccess CopyUnsupportedAttribute CopyUnsupportedConstraint CopyOtherError

"""
    struct CopyResult{T}
        status::CopyStatusCode
        message::String # Human-friendly explanation why the copy failed
        indexmap::T     # Only valid if status is CopySuccess
    end

A struct returned by `copy!` to indicate success or failure. If success, also exposes a map between the variable and constraint indices of the two instances.
"""
struct CopyResult{T}
    status::CopyStatusCode
    message::String # Human-friendly explanation why the copy failed
    indexmap::T     # Only valid if status is CopySuccess
end

"""
    copy!(dest::ModelLike, src::ModelLike, warnattributes=true)::CopyResult

Copy the model from `src` into `dest`. The target `dest` is emptied, and all
previous indices to variables or constraints in `dest` are invalidated. Returns
a `CopyResult` object. If the copy is successful, the `CopyResult` contains a
dictionary-like object that translates variable and constraint indices from the
`src` model to the corresponding indices in the `dest` model.

If an attribute `attr` cannot be copied from `src` to `dest` then an error is thrown. If an optimizer parameter cannot be copied then:

* If `warnattributes` is `true`, a warning is displayed, otherwise,
* The attribute is silently ignored.

### Example

```julia
# Given empty `ModelLike` objects `src` and `dest`.

x = addvariable!(src)

isvalid(src, x)   # true
isvalid(dest, x)  # false (`dest` has no variables)

copy_result = copy!(dest, src)
if copy_result.status == CopySuccess
    index_map = copy_result.indexmap
    isvalid(dest, x) # false (unless index_map[x] == x)
    isvalid(dest, index_map[x]) # true
else
    println("Copy failed with status ", copy_result.status)
    println("Failure message: ", copy_result.message)
end
```
"""
function copy! end

include("indextypes.jl")
include("functions.jl")
include("sets.jl")
include("constraints.jl")
include("attributes.jl")
include("objectives.jl")
include("variables.jl")
include("nlp.jl")

end
