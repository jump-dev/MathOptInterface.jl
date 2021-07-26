"""
    Base.write(io::IO, model::FileFormats.MOF.Model)

Write `model` to `io` in the MathOptFormat file format.
"""
function Base.write(io::IO, model::Model)
    options = get_options(model)
    object = OrderedObject(
        "name" => "MathOptFormat Model",
        "version" => OrderedObject(
            "major" => Int(VERSION.major),
            "minor" => Int(VERSION.minor),
        ),
        "variables" => Object[],
        "objective" => OrderedObject("sense" => "feasibility"),
        "constraints" => Object[],
    )
    FileFormats.create_unique_names(model, warn = options.warn)
    name_map = write_variables(object, model)
    write_nlpblock(object, model, name_map)
    write_objective(object, model, name_map)
    write_constraints(object, model, name_map)
    indent = options.print_compact ? nothing : 2
    Base.write(io, JSON.json(object, indent))
    return
end

function write_variables(object, model::Model)
    name_map = Dict{MOI.VariableIndex,String}()
    for index in MOI.get(model, MOI.ListOfVariableIndices())
        variable = moi_to_object(index, model)
        name_map[index] = variable["name"]
        push!(object["variables"], variable)
    end
    return name_map
end

function write_objective(
    object::T,
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
) where {T<:Object}
    if object["objective"]["sense"] != "feasibility"
        return  # Objective must have been written from NLPBlock.
    end
    sense = MOI.get(model, MOI.ObjectiveSense())
    object["objective"] = T("sense" => moi_to_object(sense))
    if sense != MOI.FEASIBILITY_SENSE
        F = MOI.get(model, MOI.ObjectiveFunctionType())
        objective_function = MOI.get(model, MOI.ObjectiveFunction{F}())
        object["objective"]["function"] =
            moi_to_object(objective_function, name_map)
    end
    return
end

function write_constraints(
    object,
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
)
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            push!(object["constraints"], moi_to_object(index, model, name_map))
        end
    end
end

"""
    moi_to_object(x, model::Model)

Convert `x` into an OrderedDict representation.
"""
function moi_to_object end

function moi_to_object(index::MOI.VariableIndex, model::Model)
    name = MOI.get(model, MOI.VariableName(), index)
    if name == ""
        error("Variable name for $(index) cannot be blank in an MOF file.")
    end
    return OrderedObject("name" => name)
end

function moi_to_object(
    index::MOI.ConstraintIndex{F,S},
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
) where {F,S}
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    object = OrderedObject()
    if F != MOI.SingleVariable
        name = MOI.get(model, MOI.ConstraintName(), index)
        if name != ""
            object["name"] = name
        end
    end
    object["function"] = moi_to_object(func, name_map)
    object["set"] = moi_to_object(set, name_map)
    return object
end

function moi_to_object(sense::MOI.OptimizationSense)
    if sense == MOI.MIN_SENSE
        return "min"
    elseif sense == MOI.MAX_SENSE
        return "max"
    else
        @assert sense == MOI.FEASIBILITY_SENSE
        return "feasibility"
    end
end

# ========== Non-typed scalar functions ==========

function moi_to_object(
    foo::MOI.SingleVariable,
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "type" => "SingleVariable",
        "variable" => name_map[foo.variable],
    )
end

# ========== Typed scalar functions ==========

function moi_to_object(
    foo::MOI.ScalarAffineTerm{Float64},
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "coefficient" => foo.coefficient,
        "variable" => name_map[foo.variable],
    )
end

function moi_to_object(
    foo::MOI.ScalarAffineFunction{Float64},
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "type" => "ScalarAffineFunction",
        "terms" => moi_to_object.(foo.terms, Ref(name_map)),
        "constant" => foo.constant,
    )
end

function moi_to_object(
    foo::MOI.ScalarQuadraticTerm{Float64},
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "coefficient" => foo.coefficient,
        "variable_1" => name_map[foo.variable_1],
        "variable_2" => name_map[foo.variable_2],
    )
end

function moi_to_object(
    foo::MOI.ScalarQuadraticFunction{Float64},
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "type" => "ScalarQuadraticFunction",
        "affine_terms" => moi_to_object.(foo.affine_terms, Ref(name_map)),
        "quadratic_terms" => moi_to_object.(foo.quadratic_terms, Ref(name_map)),
        "constant" => foo.constant,
    )
end

# ========== Non-typed vector functions ==========

function moi_to_object(
    foo::MOI.VectorOfVariables,
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "type" => "VectorOfVariables",
        "variables" => [name_map[variable] for variable in foo.variables],
    )
end

# ========== Typed vector functions ==========

function moi_to_object(
    foo::MOI.VectorAffineTerm,
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "output_index" => foo.output_index,
        "scalar_term" => moi_to_object(foo.scalar_term, name_map),
    )
end

function moi_to_object(
    foo::MOI.VectorAffineFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "type" => "VectorAffineFunction",
        "terms" => moi_to_object.(foo.terms, Ref(name_map)),
        "constants" => foo.constants,
    )
end

function moi_to_object(
    foo::MOI.VectorQuadraticTerm,
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "output_index" => foo.output_index,
        "scalar_term" => moi_to_object(foo.scalar_term, name_map),
    )
end

function moi_to_object(
    foo::MOI.VectorQuadraticFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    return OrderedObject(
        "type" => "VectorQuadraticFunction",
        "affine_terms" => moi_to_object.(foo.affine_terms, Ref(name_map)),
        "quadratic_terms" => moi_to_object.(foo.quadratic_terms, Ref(name_map)),
        "constants" => foo.constants,
    )
end

# ========== Default ==========
"""
    head_name(::Type{SetType}) where SetType <: MOI.AbstractSet

Return the string that is stored in the `"type"` field of the MOF object for a
set of type `SetType`.
"""
function head_name end
# We don't need a fallback that throws an error because it is impossible for
# this to be called for a set that is not defined in the MOIU Model constructor.

# Add every field as the field is named in MathOptInterface.
function moi_to_object(
    set::SetType,
    ::Dict{MOI.VariableIndex,String},
) where {SetType}
    object = OrderedObject("type" => head_name(SetType))
    for key in fieldnames(SetType)
        object[string(key)] = getfield(set, key)
    end
    return object
end

# ========== Non-typed scalar sets ==========
head_name(::Type{MOI.ZeroOne}) = "ZeroOne"
head_name(::Type{MOI.Integer}) = "Integer"

# ========== Typed scalar sets ==========
head_name(::Type{<:MOI.LessThan}) = "LessThan"
head_name(::Type{<:MOI.GreaterThan}) = "GreaterThan"
head_name(::Type{<:MOI.EqualTo}) = "EqualTo"
head_name(::Type{<:MOI.Interval}) = "Interval"
head_name(::Type{<:MOI.Semiinteger}) = "Semiinteger"
head_name(::Type{<:MOI.Semicontinuous}) = "Semicontinuous"

# ========== Non-typed vector sets ==========
head_name(::Type{MOI.Zeros}) = "Zeros"
head_name(::Type{MOI.Reals}) = "Reals"
head_name(::Type{MOI.Nonnegatives}) = "Nonnegatives"
head_name(::Type{MOI.Nonpositives}) = "Nonpositives"
head_name(::Type{MOI.SecondOrderCone}) = "SecondOrderCone"
head_name(::Type{MOI.RotatedSecondOrderCone}) = "RotatedSecondOrderCone"
head_name(::Type{MOI.GeometricMeanCone}) = "GeometricMeanCone"
head_name(::Type{MOI.ExponentialCone}) = "ExponentialCone"
head_name(::Type{MOI.DualExponentialCone}) = "DualExponentialCone"
head_name(::Type{MOI.NormOneCone}) = "NormOneCone"
head_name(::Type{MOI.NormInfinityCone}) = "NormInfinityCone"
head_name(::Type{MOI.RelativeEntropyCone}) = "RelativeEntropyCone"
head_name(::Type{MOI.NormSpectralCone}) = "NormSpectralCone"
head_name(::Type{MOI.NormNuclearCone}) = "NormNuclearCone"
head_name(::Type{MOI.RootDetConeTriangle}) = "RootDetConeTriangle"
head_name(::Type{MOI.RootDetConeSquare}) = "RootDetConeSquare"
head_name(::Type{MOI.LogDetConeTriangle}) = "LogDetConeTriangle"
head_name(::Type{MOI.LogDetConeSquare}) = "LogDetConeSquare"
function head_name(::Type{MOI.PositiveSemidefiniteConeTriangle})
    return "PositiveSemidefiniteConeTriangle"
end
function head_name(::Type{MOI.PositiveSemidefiniteConeSquare})
    return "PositiveSemidefiniteConeSquare"
end
head_name(::Type{MOI.Complements}) = "Complements"

# ========== Typed vector sets ==========
head_name(::Type{<:MOI.PowerCone}) = "PowerCone"
head_name(::Type{<:MOI.DualPowerCone}) = "DualPowerCone"
head_name(::Type{<:MOI.SOS1}) = "SOS1"
head_name(::Type{<:MOI.SOS2}) = "SOS2"

function moi_to_object(
    set::MOI.Indicator{I,S},
    name_map::Dict{MOI.VariableIndex,String},
) where {I,S}
    @assert I == MOI.ACTIVATE_ON_ONE || I == MOI.ACTIVATE_ON_ZERO
    return OrderedObject(
        "type" => "IndicatorSet",
        "set" => moi_to_object(set.set, name_map),
        "activate_on" => (I == MOI.ACTIVATE_ON_ONE) ? "one" : "zero",
    )
end
