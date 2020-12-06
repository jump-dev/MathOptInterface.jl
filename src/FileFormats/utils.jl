###
### Re-naming utilities
###

"""
    create_unique_names(
        model::MOI.ModelLike;
        warn::Bool = false,
        replacements::Vector{Function} = Function[]
    )

Rename variables in `model` to ensure that all variables and constraints have
a unique name. In addition, loop through `replacements` and replace names with
`f(name)`.

If `warn`, print a warning if a variable or constraint is renamed.
"""
function create_unique_names(
    model::MOI.ModelLike;
    warn::Bool = false,
    replacements::Vector{Function} = Function[],
)
    create_unique_variable_names(model, warn, replacements)
    create_unique_constraint_names(model, warn, replacements)
    return
end

function _replace(s::String, replacements::Vector{Function})
    for f in replacements
        s = f(s)
    end
    return s
end

function create_unique_constraint_names(
    model::MOI.ModelLike,
    warn::Bool,
    replacements::Vector{Function},
)
    original_names = Set{String}()
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            name = MOI.get(model, MOI.ConstraintName(), index)
            push!(original_names, _replace(name, replacements))
        end
    end
    added_names = Set{String}()
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            original_name = MOI.get(model, MOI.ConstraintName(), index)
            new_name = _replace(
                original_name != "" ? original_name : "c$(index.value)",
                replacements,
            )
            if new_name in added_names
                # We found a duplicate name! We could just append a string like
                # "_", but we're going to be clever and loop through the
                # integers to name them appropriately. Thus, if we have three
                # constraints named c, we'll end up with variables named c, c_1,
                # and c_2.
                i = 1
                tmp_name = string(new_name, "_", i)
                while tmp_name in added_names || tmp_name in original_names
                    i += 1
                    tmp_name = string(new_name, "_", i)
                end
                new_name = tmp_name
            end
            push!(added_names, new_name)
            if new_name != original_name
                if warn
                    if original_name == ""
                        @warn(
                            "Blank name detected for constraint $(index). " *
                            "Renamed to $(new_name)."
                        )
                    else
                        @warn(
                            "Duplicate name $(original_name) detected for " *
                            "constraint $(index). Renamed to $(new_name)."
                        )
                    end
                end
                MOI.set(model, MOI.ConstraintName(), index, new_name)
            end
        end
    end
end

function create_unique_variable_names(
    model::MOI.ModelLike,
    warn::Bool,
    replacements::Vector{Function},
)
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    # This is a list of all of the names currently in the model. We're going to
    # use this to make sure we don't rename a variable to a name that already
    # exists.
    original_names = Set{String}([
        _replace(MOI.get(model, MOI.VariableName(), index), replacements)
        for index in variables
    ])
    # This set of going to store all of the names in the model so that we don't
    # add duplicates.
    added_names = Set{String}()
    for index in variables
        original_name = MOI.get(model, MOI.VariableName(), index)
        new_name = _replace(
            original_name != "" ? original_name : "x$(index.value)",
            replacements,
        )
        if new_name in added_names
            # We found a duplicate name! We could just append a string like "_",
            # but we're going to be clever and loop through the integers to name
            # them appropriately. Thus, if we have three variables named x,
            # we'll end up with variables named x, x_1, and x_2.
            i = 1
            tmp_name = string(new_name, "_", i)
            while tmp_name in added_names || tmp_name in original_names
                i += 1
                tmp_name = string(new_name, "_", i)
            end
            new_name = tmp_name
        end
        push!(added_names, new_name)
        if new_name != original_name
            if warn
                if original_name == ""
                    @warn(
                        "Blank name detected for variable $(index). Renamed to " *
                        "$(new_name)."
                    )
                else
                    @warn(
                        "Duplicate name $(original_name) detected for variable " *
                        "$(index). Renamed to $(new_name)."
                    )
                end
            end
            MOI.set(model, MOI.VariableName(), index, new_name)
        end
    end
end

###
### Compression utilities
###

function error_mode(mode::String)
    return throw(ArgumentError("Compressed mode must be \"r\" or \"w\". Got: $mode."))
end

"""
    abstract type AbstractCompressionScheme end

Base type to implement a new compression scheme for MathOptFormat.

To do so, create a concrete subtype (e.g., named after the compression scheme)
and implement:

    extension(::Val{:your_scheme_extension}) = YourScheme()
    compressed_open(f::Function, filename::String, mode::String, ::YourScheme)
"""
abstract type AbstractCompressionScheme end

struct NoCompression <: AbstractCompressionScheme end

extension(::Val) = NoCompression()

function compressed_open(
    f::Function,
    filename::String,
    mode::String,
    ::NoCompression,
)
    return Base.open(f, filename, mode)
end

struct Gzip <: AbstractCompressionScheme end

extension(::Val{:gz}) = Gzip()

function compressed_open(f::Function, filename::String, mode::String, ::Gzip)
    if mode == "w"
        return Base.open(f, CodecZlib.GzipCompressorStream, filename, mode)
    elseif mode == "r"
        return Base.open(f, CodecZlib.GzipDecompressorStream, filename, mode)
    end
    return error_mode(mode)
end

struct Bzip2 <: AbstractCompressionScheme end

extension(::Val{:bz2}) = Bzip2()

function compressed_open(f::Function, filename::String, mode::String, ::Bzip2)
    if mode == "w"
        return Base.open(f, CodecBzip2.Bzip2CompressorStream, filename, mode)
    elseif mode == "r"
        return Base.open(f, CodecBzip2.Bzip2DecompressorStream, filename, mode)
    end
    return error_mode(mode)
end

struct AutomaticCompression <: AbstractCompressionScheme end

function compressed_open(
    f::Function,
    filename::String,
    mode::String,
    ::AutomaticCompression,
)
    ext = Symbol(split(filename, ".")[end])
    return compressed_open(f, filename, mode, extension(Val{ext}()))
end
