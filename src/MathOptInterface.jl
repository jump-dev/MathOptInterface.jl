# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MathOptInterface

import MutableArithmetics as MA

"""
    ModelLike

Abstract supertype for objects that implement the "Model" interface for defining
an optimization problem.
"""
abstract type ModelLike end

# This allows to use `ModelLike`s in broadcast calls without the need to
# embed it in a `Ref`
Base.broadcastable(model::ModelLike) = Ref(model)

function _try_get(model::ModelLike, attr, default)
    try
        return get(model, attr)
    catch
        return default
    end
end

function Base.show(io::IO, model::ModelLike)
    offset = Base.get(io, :offset, "")
    Utilities.print_with_acronym(io, summary(model))
    println(io)
    # ObjectiveSense
    sense = _try_get(model, ObjectiveSense(), "unknown")
    println(io, offset, "├ ObjectiveSense: $sense")
    # ObjectiveFunctionType
    F = _try_get(model, ObjectiveFunctionType(), "unknown")
    Utilities.print_with_acronym(io, "$(offset)├ ObjectiveFunctionType: $F\n")
    # NumberOfVariables
    n = _try_get(model, NumberOfVariables(), "unknown")
    println(io, offset, "├ NumberOfVariables: $n")
    # NumberOfConstraints
    constraint_types = _try_get(model, ListOfConstraintTypesPresent(), nothing)
    if constraint_types === nothing
        print(io, offset, "└ NumberOfConstraints: unknown")
    else
        constraint_lines, n_total = String[], 0
        for (i, (F, S)) in enumerate(constraint_types)
            m = _try_get(model, NumberOfConstraints{F,S}(), 0)
            n_total += m
            tree = ifelse(i == length(constraint_types), '└', '├')
            push!(constraint_lines, "\n$offset  $tree $F in $S: $m")
        end
        print(io, offset, "└ NumberOfConstraints: $n_total")
        for line in constraint_lines
            Utilities.print_with_acronym(io, line)
        end
    end
    return
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
    write_to_file(model::ModelLike, filename::String; kwargs...)

Write the current model to the file at `filename`.

Supported file types depend on the model type.

Additional keyword arguments are passed to the `Model` constructor of the
relevant file format. See, for example, [`FileFormats.LP.Model`](@ref) and
[`FileFormats.MPS.Model`](@ref).

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Int}();

julia> x, _ = MOI.add_constrained_variable(model, MOI.Interval(2, 3));

julia> MOI.set(model, MOI.VariableName(), x, "x");

julia> filename = joinpath(tempdir(), "model.lp");

julia> MOI.write_to_file(model, filename; coefficient_type = Int);

julia> print(read(filename, String))
minimize
obj:
subject to
Bounds
2 <= x <= 3
End
```
"""
function write_to_file(model::ModelLike, filename::String; kwargs...)
    dest = FileFormats.Model(; filename, kwargs...)
    copy_to(dest, model)
    write_to_file(dest, filename)
    return
end

"""
    read_from_file(model::ModelLike, filename::String; kwargs...)

Read the file `filename` into the model `model`. If `model` is non-empty, this
may throw an error.

Supported file types depend on the model type.

Additional keyword arguments are passed to the `Model` constructor of the
relevant file format. See, for example, [`FileFormats.LP.Model`](@ref) and
[`FileFormats.MPS.Model`](@ref).

### Note

Once the contents of the file are loaded into the model, you can query the
variables via `MOI.get(model, MOI.ListOfVariableIndices())`. However, some
filetypes, such as LP files, do not maintain an explicit ordering of the
variables. Therefore, the returned list may be in an arbitrary order.

To avoid depending on the order of the indices, look up each variable index by
name using `MOI.get(model, MOI.VariableIndex, "name")`.

## Example

```jldoctest
julia> model = MOI.Utilities.Model{Int}();

julia> x, _ = MOI.add_constrained_variable(model, MOI.GreaterThan(2));

julia> MOI.set(model, MOI.VariableName(), x, "x");

julia> filename = joinpath(tempdir(), "model.lp");

julia> MOI.write_to_file(model, filename; coefficient_type = Int);

julia> new_model = MOI.Utilities.Model{Int}();

julia> MOI.read_from_file(new_model, filename; coefficient_type = Int);

julia> print(new_model)
Minimize ScalarAffineFunction{Int64}:
 (0)

Subject to:

VariableIndex-in-GreaterThan{Int64}
 x >= (2)

julia> MOI.get(new_model, MOI.VariableIndex, "x")
MOI.VariableIndex(1)
```
"""
function read_from_file(model::ModelLike, filename::String; kwargs...)
    src = FileFormats.Model(; filename, kwargs...)
    read_from_file(src, filename)
    copy_to(model, src)
    return
end

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
function copy_to(dest, src)
    return error(
        "`copy_to` is not supported by the solver `$(typeof(dest))`. Did " *
        "you mean to call " *
        "`optimize!(dest::AbstractOptimizer, src::ModelLike)` instead?",
    )
end

"""
    @_documented_enum(args...)

A utility macro for writing `@enum`s with inline documentation strings.

The even arguments are forwarded to the `@enum` macro, and the odd arguments are
used as the docstring for the corresponding even argument.

## Example

```julia
julia> MOI.@_documented_enum(
           \"\"\"
               MyDocumentedEnum

           This is an example for the documentation.
           \"\"\",
           MyDocumentedEnum,
           \"A country down-under\",
           AUSTRALIA,
           \"Another country down-under\",
           NEW_ZEALAND,
       )
MyDocumentedEnum

help?> MyDocumentedEnum
search: MyDocumentedEnum

  MyDocumentedEnum

  This is an example for the documentation.

  Values
  ========

  Possible values are:

    •  AUSTRALIA: A country down-under

    •  NEW_ZEALAND: Another country down-under

help?> AUSTRALIA
search: AUSTRALIA

  AUSTRALIA::MyDocumentedEnum

  An instance of the MyDocumentedEnum enum.

  AUSTRALIA: A country down-under
```
"""
macro _documented_enum(args...)
    @assert iseven(length(args))
    code = quote
        $(Expr(:macrocall, Symbol("@enum"), __source__, args[2:2:end]...))
    end
    main_doc, enum = args[1], args[2]
    main_doc *= "\n## Values\n\n"
    for i in 3:2:length(args)
        docstr, value = args[i], args[i+1]
        doc = """
            $value::$enum

        An instance of the [`$enum`](@ref) enum.

        ## About

        $docstr
        """
        push!(
            code.args,
            Expr(:macrocall, Symbol("@doc"), __source__, doc, Meta.quot(value)),
        )
        new_docstr = join(split(docstr, "\n"), "\n   ")
        main_doc *= "### [`$value`](@ref)\n\n$new_docstr\n"
    end
    push!(
        code.args,
        Expr(
            :macrocall,
            Symbol("@doc"),
            __source__,
            main_doc,
            Meta.quot(args[2]),
        ),
    )
    return esc(code)
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

include("precompile.jl")

# submodules
include("Utilities/Utilities.jl") # MOI.Utilities
include("Test/Test.jl")
include("Bridges/Bridges.jl")     # MOI.Bridges
include("Benchmarks/Benchmarks.jl")
include("Nonlinear/Nonlinear.jl")
include("FileFormats/FileFormats.jl")

include("instantiate.jl")

"""
    IndexMap()

The dictionary-like object returned by [`copy_to`](@ref).

## IndexMap

Implementations of [`copy_to`](@ref) must return an [`IndexMap`](@ref). For
technical reasons, the `IndexMap` type is defined in the Utilities submodule as
`MOI.Utilities.IndexMap`. However, since it is an integral part of the MOI API,
we provide this `MOI.IndexMap` as an alias.
"""
const IndexMap = Utilities.IndexMap

import PrecompileTools

PrecompileTools.@setup_workload begin
    PrecompileTools.@compile_workload begin
        model = Utilities.CachingOptimizer(
            Utilities.UniversalFallback(Utilities.Model{Float64}()),
            instantiate(Utilities.MockOptimizer; with_bridge_type = Float64),
        )
        set(model, Silent(), true)
        x = add_variables(model, 3)
        add_constraint(model, x[1], ZeroOne())
        add_constraint(model, x[2], Integer())
        add_constraint(model, x[1], GreaterThan(0.0))
        add_constraint(model, x[2], LessThan(0.0))
        add_constraint(model, x[3], EqualTo(0.0))
        f = 1.0 * x[1] + x[2] + x[3]
        add_constraint(model, f, GreaterThan(0.0))
        add_constraint(model, f, LessThan(0.0))
        add_constraint(model, f, EqualTo(0.0))
        y, _ = add_constrained_variables(model, Nonnegatives(2))
        set(model, ObjectiveSense(), MAX_SENSE)
        set(model, ObjectiveFunction{typeof(f)}(), f)
        optimize!(model)
    end
end

end
