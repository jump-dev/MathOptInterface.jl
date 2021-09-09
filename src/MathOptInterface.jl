module MathOptInterface

"""
    ModelLike

Abstract supertype for objects that implement the "Model" interface for defining
an optimization problem.
"""
abstract type ModelLike end

# This allows to use `ModelLike`s in broadcast calls without the need to
# embed it in a `Ref`
Base.broadcastable(model::ModelLike) = Ref(model)

function Base.show(io::IO, model::ModelLike)
    return Utilities.print_with_acronym(io, summary(model))
end

"""
    AbstractOptimizer <: ModelLike

Abstract supertype for objects representing an instance of an optimization
problem tied to a particular solver. This is typically a solver's in-memory
representation. In addition to `ModelLike`, `AbstractOptimizer` objects let you
solve the model and query the solution.
"""
abstract type AbstractOptimizer <: ModelLike end

"""
    optimize!(optimizer::AbstractOptimizer)

Start the solution procedure.
"""
function optimize! end

"""
    compute_conflict!(optimizer::AbstractOptimizer)

Computes a minimal subset of constraints such that the model with the other
constraint removed is still infeasible.

Some solvers call a set of conflicting constraints an Irreducible Inconsistent
Subsystem (IIS).

See also [`ConflictStatus`](@ref) and [`ConstraintConflictStatus`](@ref).

### Note

If the model is modified after a call to `compute_conflict!`, the implementor
is not obliged to purge the conflict. Any calls to the above attributes may
return values for the original conflict without a warning. Similarly, when
modifying the model, the conflict can be discarded.
"""
function compute_conflict! end

"""
    write_to_file(model::ModelLike, filename::String)

Writes the current model data to the given file.
Supported file types depend on the model type.
"""
function write_to_file end

"""
    read_from_file(model::ModelLike, filename::String)

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
function read_from_file end

"""
    is_empty(model::ModelLike)

Returns `false` if the `model` has any model attribute set or has any variables
or constraints.

Note that an empty model can have optimizer attributes set.
"""
function is_empty end

"""
    empty!(model::ModelLike)

Empty the model, that is, remove all variables, constraints and model attributes
but not optimizer attributes.
"""
function empty! end

"""
    supports_incremental_interface(model::ModelLike)

Return a `Bool` indicating whether `model` supports building incrementally via
[`add_variable`](@ref) and [`add_constraint`](@ref).

The main purpose of this function is to determine whether a model can be loaded
into `model` incrementally or whether it should be cached and copied at once
instead.
"""
supports_incremental_interface(::ModelLike) = false

"""
    copy_to(dest::ModelLike, src::ModelLike)::IndexMap

Copy the model from `src` into `dest`.

The target `dest` is emptied, and all previous indices to variables and
constraints in `dest` are invalidated.

Returns an [`IndexMap`](@ref) object that translates variable and constraint
indices from the `src` model to the corresponding indices in the `dest` model.

## Notes

 * If a constraint that in `src` is not supported by `dest`, then an
   [`UnsupportedConstraint`](@ref) error is thrown.
 * If an [`AbstractModelAttribute`](@ref), [`AbstractVariableAttribute`](@ref),
   or [`AbstractConstraintAttribute`](@ref) is set in `src` but not supported by
   `dest`, then an [`UnsupportedAttribute`](@ref) error is thrown.

[`AbstractOptimizerAttribute`](@ref)s are _not_ copied  to the `dest` model.

## IndexMap

Implementations of `copy_to` must return an [`IndexMap`](@ref). For technical
reasons, this type is defined in the Utilities submodule as
`MOI.Utilities.IndexMap`. However, since it is an integral part of the MOI API,
we provide `MOI.IndexMap` as an alias.

### Example

```julia
# Given empty `ModelLike` objects `src` and `dest`.

x = add_variable(src)

is_valid(src, x)   # true
is_valid(dest, x)  # false (`dest` has no variables)

index_map = copy_to(dest, src)
is_valid(dest, x) # false (unless index_map[x] == x)
is_valid(dest, index_map[x]) # true
```
"""
function copy_to(dest, src; kwargs...)
    if length(kwargs) > 0
        @warn(
            "copy_to with keyword arguments is deprecated. Now names are " *
            "copied by default",
            maxlog = 1,
        )
    end
    return copy_to(dest, src)
end

"""
    optimize_model!(
        dest::AbstractOptimizer,
        src::ModelLike,
    )::IndexMap

A single call similar to calling [`copy_to`](@ref) followed by
[`optimize!`](@ref). Return an [`IndexMap`](@ref) and a `Bool` `copied`.
If `copied` is `true`, `src` was copied to `dest` so [`optimize!](@ref)
can be called again on `dest`, e.g., after changing the starting values
or optimizer attributes. On the other hand, if `copied` is `false`, the
model was not copied to `dest` so calling [`optimize!`](@ref) is not allowed.
However, attributes for which [`is_set_by_optimize`](@ref) is true can be
queried from `dest`.

An optimizer can decide to implement this function instead of implementing
[`copy_to`](@ref) and [`optimize!`](@ref) individually.

!!! warning
    This is an experimental new feature of MOI v0.10.1 that may break in MOI v1.0.
"""
function optimize_model!(dest, src)
    # The arguments above are untyped to avoid ambiguities.
    return copy_to_and_optimize!(dest, src), true
end

"""
    copy_to_and_optimize!(
        dest::AbstractOptimizer,
        src::ModelLike,
    )::IndexMap

A single call equivalent to calling [`copy_to`](@ref) followed by
[`optimize!`](@ref).  Like [`copy_to`](@ref), it returns an [`IndexMap`].

An optimizer can decide to implement this function instead of implementing
[`copy_to`](@ref) and [`optimize!`](@ref) individually.

!!! warning
    This function will be removed in MOI v1.0, use `optimize_model!` instead.
"""
function copy_to_and_optimize!(dest, src) # TODO remove in favor of `optimize_model!`
    # The arguments above are untyped to avoid ambiguities.
    index_map = copy_to(dest, src)
    optimize!(dest)
    return index_map
end

include("error.jl")
include("indextypes.jl")
include("functions.jl")
include("sets.jl")
include("attributes.jl")
include("constraints.jl")
include("modifications.jl")
include("variables.jl")
include("nlp.jl")

if VERSION > v"1.4.2"
    include("precompile.jl")
end

# submodules
include("Utilities/Utilities.jl") # MOI.Utilities
include("Test/Test.jl")
include("Bridges/Bridges.jl")     # MOI.Bridges
include("Benchmarks/Benchmarks.jl")
include("FileFormats/FileFormats.jl")

include("instantiate.jl")
include("deprecate.jl")
include("DeprecatedTest/DeprecatedTest.jl")

"""
    IndexMap()

The dictionary-like object returned by [`copy_to`](@ref).

## IndexMap

Implementations of [`copy_to`](@ref) must return an [`IndexMap`](@ref). For
technical reasons, the `IndexMap` type is defined in the Utilties submodule as
`MOI.Utilities.IndexMap`. However, since it is an integral part of the MOI API,
we provide this `MOI.IndexMap` as an alias.
"""
const IndexMap = Utilities.IndexMap

end
