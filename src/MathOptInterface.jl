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

Optimize the problem contained in `optimizer`.

Before calling `optimize!`, the problem should first be constructed using the
incremental interface (see [`supports_incremental_interface`](@ref)) or [`copy_to`](@ref).
"""
function optimize! end

"""
    optimize!(dest::AbstractOptimizer, src::ModelLike)::Tuple{IndexMap,Bool}

A "one-shot" call that copies the problem from `src` into `dest` and then uses
`dest` to optimize the problem.

Returns a tuple of an [`IndexMap`](@ref) and a `Bool` `copied`.

 * The [`IndexMap`](@ref) object translates variable and constraint indices from
   the `src` model to the corresponding indices in the `dest` optimizer. See
   [`copy_to`](@ref) for details.
 * If `copied == true`, `src` was copied to `dest` and then cached, allowing
   incremental modification if supported by the solver.
 * If `copied == false`, a cache of the model was _not_ kept in `dest`.
   Therefore, only the solution information (attributes for which
   [`is_set_by_optimize`](@ref) is true) is available to query.

!!! note
    The main purpose of `optimize!` method with two arguments is for use in
    [`Utilities.CachingOptimizer`](@ref).

!!! warning
    The new `optimize!` method with two arguments is an experimental new feature
    of MOI v0.10.2 that may break in MOI v1.0.

## Relationship to the single-argument `optimize!`

The default fallback of `optimize!(dest::AbstractOptimizer, src::ModelLike)` is
```julia
function optimize!(dest::AbstractOptimizer, src::ModelLike)
    index_map = copy_to(dest, src)
    optimize!(dest)
    return index_map, true
end
```
Therefore, subtypes of [`AbstractOptimizer`](@ref) should either implement this
two-argument method, or implement both [`copy_to(::Optimizer, ::ModelLike)`](@ref copy_to)
and `optimize!(::Optimizer)`.
"""
function optimize!(dest, src)
    # The arguments above are untyped to avoid ambiguities.
    index_map = copy_to(dest, src)
    optimize!(dest)
    return index_map, true
end

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
function compute_conflict!(optimizer::AbstractOptimizer)
    return throw(
        ArgumentError(
            "The optimizer $(typeof(optimizer)) does not support " *
            "`compute_conflict!`",
        ),
    )
end

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
    if length(kwargs) == 0
        error(
            "`copy_to` is not supported by the solver `$(typeof(dest))`. Did " *
            "you mean to call " *
            "`optimize!(dest::AbstractOptimizer, src::ModelLike)` instead?",
        )
    end
    @warn(
        "copy_to with keyword arguments is deprecated. Now names are " *
        "copied by default",
        maxlog = 1,
    )
    return copy_to(dest, src)
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

if VERSION > v"1.4.2"
    _precompile_()
end

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
