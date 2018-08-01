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
model and query the solution.
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
Supported file types depend on the model type.
"""
function write end

"""
    read!(model::ModelLike, filename::String)

Read the file `filename` into the model `model`. If `model` is non-empty, this may
throw an error.

Supported file types depend on the model type.

### Note

Once the contents of the file are loaded into the model, users can query the variables via
`get(model, ListOfVariableIndices())`. However, some filetypes, such as LP files, do not
maintain an explicit ordering of the variables. Therefore, the returned list may be in an
arbitrary order. To avoid depending on the order of the indices, users should look up each
variable index by name: `get(model, VariableIndex, "name")`.
"""
function read! end

"""
    isempty(model::ModelLike)

Returns `false` if the `model` has any model attribute set or has any variables or constraints.
Note that an empty model can have optimizer attributes set.
"""
function isempty end

"""
    empty!(model::ModelLike)

Empty the model, that is, remove all variables, constraints and model attributes but not optimizer attributes.
"""
function empty! end

"""
    copy!(dest::ModelLike, src::ModelLike; copynames=true, warnattributes=true)

Copy the model from `src` into `dest`. The target `dest` is emptied, and all
previous indices to variables or constraints in `dest` are invalidated. Returns
a dictionary-like object that translates variable and constraint indices from
the `src` model to the corresponding indices in the `dest` model.

If `copynames` is `false`, the `Name`, `VariableName` and `ConstraintName`
attributes are not copied even if they are set in `src`. If a constraint that
is copied from `src` is not supported by `dest` then an
[`UnsupportedConstraint`](@ref) error is thrown. Similarly, if a model, variable
or constraint attribute that is copied from `src` is not supported by `dest`
then an [`UnsupportedAttribute`](@ref) error is thrown. Unsupported *optimizer*
attributes are treated differently:

* If `warnattributes` is `true`, a warning is displayed, otherwise,
* the attribute is silently ignored.

### Example

```julia
# Given empty `ModelLike` objects `src` and `dest`.

x = addvariable!(src)

isvalid(src, x)   # true
isvalid(dest, x)  # false (`dest` has no variables)

index_map = copy!(dest, src)
isvalid(dest, x) # false (unless index_map[x] == x)
isvalid(dest, index_map[x]) # true
```
"""
function copy! end

include("error.jl")
include("indextypes.jl")
include("functions.jl")
include("sets.jl")
include("attributes.jl")
include("constraints.jl")
include("modifications.jl")
include("variables.jl")
include("nlp.jl")

# submodules
include("Utilities/Utilities.jl") # MOI.Utilities
include("Test/Test.jl")           # MOI.Test
include("Bridges/Bridges.jl")     # MOI.Bridges

end
