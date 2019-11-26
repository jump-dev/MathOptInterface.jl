module MathOptFormat

import MathOptInterface
const MOI = MathOptInterface

import CodecBzip2
import CodecZlib

include("compression.jl")
include("CBF/CBF.jl")
include("LP/LP.jl")
include("MOF/MOF.jl")
include("MPS/MPS.jl")

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
    replacements::Vector{Function} = Function[]
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
    model::MOI.ModelLike, warn::Bool, replacements::Vector{Function}
)
    original_names = Set{String}()
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
            name = MOI.get(model, MOI.ConstraintName(), index)
            push!(original_names, _replace(name, replacements))
        end
    end
    added_names = Set{String}()
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
            original_name = MOI.get(model, MOI.ConstraintName(), index)
            new_name = _replace(
                original_name != "" ? original_name : "c$(index.value)",
                replacements
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
                        @warn("Blank name detected for constraint $(index). " *
                              "Renamed to $(new_name).")
                    else
                        @warn("Duplicate name $(original_name) detected for " *
                              "constraint $(index). Renamed to $(new_name).")
                    end
                end
                MOI.set(model, MOI.ConstraintName(), index, new_name)
            end
        end
    end
end

function create_unique_variable_names(
    model::MOI.ModelLike, warn::Bool, replacements::Vector{Function}
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
            replacements
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
                    @warn("Blank name detected for variable $(index). Renamed to " *
                          "$(new_name).")
                else
                    @warn("Duplicate name $(original_name) detected for variable " *
                          "$(index). Renamed to $(new_name).")
                end
            end
            MOI.set(model, MOI.VariableName(), index, new_name)
        end
    end
end

"""
    FileFormat

List of accepted export formats.

`AUTOMATIC_FILE_FORMAT` corresponds to a detection from the file name, only
based on the extension (regardless of compression format).
"""
@enum(
    FileFormat,
    FORMAT_CBF,
    FORMAT_LP,
    FORMAT_MOF,
    FORMAT_MPS,
    AUTOMATIC_FILE_FORMAT,
)

const _FILE_FORMATS = Dict{FileFormat, Tuple{String, Any}}(
    # ENUMERATED VALUE => extension, model type
    FORMAT_CBF => (".cbf", CBF.Model),
    FORMAT_LP => (".lp", LP.Model),
    FORMAT_MOF => (".mof.json", MOF.Model),
    FORMAT_MPS => (".mps", MPS.Model)
)

const MATH_OPT_FORMATS = Union{
    CBF.InnerModel, LP.InnerModel, MOF.Model, MPS.InnerModel
}

function MOI.write_to_file(
    model::MATH_OPT_FORMATS,
    filename::String;
    compression::AbstractCompressionScheme = AutomaticCompression()
)
    _compressed_open(filename, "w", compression) do io
        MOI.write_to_file(model, io)
    end
end

function MOI.read_from_file(
    model::MATH_OPT_FORMATS,
    filename::String;
    compression::AbstractCompressionScheme = AutomaticCompression()
)
    _compressed_open(filename, "r", compression) do io
        MOI.read_from_file(model, io)
    end
end

function _detect_file_format(filename::String)
    for (format, (ext, _)) in _FILE_FORMATS
        if endswith(filename, ext) || occursin("$(ext).", filename)
            return format
        end
    end
    error(
        "Unable to detect automatically format of $(filename). Use the " *
        "`file_format` keyword to specify the file format."
    )
end

"""
    read_from_file(
        filename::String;
        compression::AbstractCompressionScheme = AutomaticCompression(),
        file_format::FileFormat = AUTOMATIC_FILE_FORMAT,
    )

Return a new MOI model by reading `filename`.

Default arguments for `file_format` and `compression` will attempt to detect
model type from `filename`.
"""
function read_from_file(
    filename::String;
    compression::AbstractCompressionScheme = AutomaticCompression(),
    file_format::FileFormat = AUTOMATIC_FILE_FORMAT,
)
    if file_format == AUTOMATIC_FILE_FORMAT
        file_format = _detect_file_format(filename)
    end
    model = _FILE_FORMATS[file_format][2]()
    MOI.read_from_file(model, filename; compression = compression)
    return model
end

end
