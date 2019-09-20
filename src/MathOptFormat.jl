module MathOptFormat

import MathOptInterface
const MOI = MathOptInterface

import GZip

include("CBF/CBF.jl")
include("LP/LP.jl")
include("MOF/MOF.jl")
include("MPS/MPS.jl")

"""
    create_unique_names(
        model::MOI.ModelLike; warn::Bool = false,
        replacements::Vector{Pair{Char, Char}} = Pair{Char, Char}[]
    )

Rename variables in `model` to ensure that all variables and constraints have
a unique name. In addition, loop through `replacements` pairs `old => new` and
replace all `old` `Char`s with `new`.

If `warn`, print a warning if a variable or constraint is renamed.
"""
function create_unique_names(
    model::MOI.ModelLike; warn::Bool = false,
    replacements::Vector{Pair{Char, Char}} = Pair{Char, Char}[]
)
    create_unique_variable_names(model, warn, replacements)
    create_unique_constraint_names(model, warn, replacements)
    return
end

function _replace(s::String, replacements::Vector{Pair{Char, Char}})
    for replacement in replacements
        s = replace(s, replacement)
    end
    return s
end

function create_unique_constraint_names(
    model::MOI.ModelLike, warn::Bool, replacements::Vector{Pair{Char, Char}}
)
    original_names = Set{String}()
    for (F, S) in MOI.get(model, MOI.ListOfConstraints())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F, S}())
            name = MOI.get(model, MOI.ConstraintName(), index)
            push!(original_names, name)
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
                # We found a duplicate name! We could just append a string like "_",
                # but we're going to be clever and loop through the integers to name
                # them appropriately. Thus, if we have three constraints named c,
                # we'll end up with variables named c, c_1, and c_2.
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
    model::MOI.ModelLike, warn::Bool, replacements::Vector{Pair{Char, Char}}
)
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    # This is a list of all of the names currently in the model. We're going to
    # use this to make sure we don't rename a variable to a name that already
    # exists.
    original_names = Set{String}([
        MOI.get(model, MOI.VariableName(), index) for index in variables])
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

function gzip_open(f::Function, filename::String, mode::String)
    if endswith(filename, ".gz")
        GZip.open(f, filename, mode)
    else
        return open(f, filename, mode)
    end
end

const MATH_OPT_FORMATS = Union{
    CBF.InnerModel, LP.InnerModel, MOF.Model, MPS.InnerModel
}

function MOI.write_to_file(model::MATH_OPT_FORMATS, filename::String)
    gzip_open(filename, "w") do io
        MOI.write_to_file(model, io)
    end
end

function MOI.read_from_file(model::MATH_OPT_FORMATS, filename::String)
    gzip_open(filename, "r") do io
        MOI.read_from_file(model, io)
    end
end

"""
    read_from_file(filename::String)

Create a MOI model by reading `filename`. Type of the returned model depends on
the extension of `filename`.
"""
function read_from_file(filename::String)
    model = if endswith(filename, ".mof.json.gz") || endswith(filename, ".mof.json")
        MOF.Model()
    elseif endswith(filename, ".cbf.gz") || endswith(filename, ".cbf")
        CBF.Model()
    elseif endswith(filename, ".mps.gz") || endswith(filename, ".mps")
        MPS.Model()
    elseif endswith(filename, ".lp.gz") || endswith(filename, ".lp")
        LP.Model()
    else
        error("File-type of $(filename) not supported by MathOptFormat.jl.")
    end
    MOI.read_from_file(model, filename)
    return model
end

end
