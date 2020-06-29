"""
    Base.read!(io::IO, model::FileFormats.MOF.Model)

Read `io` in the MathOptFormat file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read model from file as destination model is not empty.")
    end
    options = get_options(model)
    if options.validate
        validate(io)
    end
    object = JSON.parse(io; dicttype=Object)
    file_version = _parse_mof_version(object["version"])
    if file_version > VERSION
        error(
            "Sorry, the file can't be read because this library supports " *
            "v$(VERSION) of MathOptFormat, but the file you are trying to " *
            "read is v$(file_version)."
        )
    end
    name_map = read_variables(model, object)
    read_objective(model, object, name_map)
    read_constraints(model, object, name_map)
    return
end

function read_variables(model::Model, object::Object)
    indices = MOI.add_variables(model, length(object["variables"]))
    name_map = Dict{String, MOI.VariableIndex}()
    for (index, variable) in zip(indices, object["variables"])
        if !haskey(variable, "name")
            error("Variable is missing a `name` field.")
        end
        name = variable["name"]
        if name == ""
            error("Variable name is `\"\"`. MathOptFormat variable names must" *
                  " be unique and non-empty.")
        end
        MOI.set(model, MOI.VariableName(), index, name)
        name_map[name] = index
    end
    return name_map
end

function read_objective(
    model::Model, object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    obj = object["objective"]
    sense = read_objective_sense(obj["sense"])
    MOI.set(model, MOI.ObjectiveSense(), sense)
    if sense == MOI.FEASIBILITY_SENSE
        return
    end
    func = function_to_moi(obj["function"], name_map)
    MOI.set(model, MOI.ObjectiveFunction{typeof(func)}(), func)
    return
end

function _add_constraint(
    model::Model, constraint::Object, name_map::Dict{String, MOI.VariableIndex}
)
    index = MOI.add_constraint(
        model,
        function_to_moi(constraint["function"], name_map),
        set_to_moi(constraint["set"]),
    )
    if haskey(constraint, "name")
        MOI.set(model, MOI.ConstraintName(), index, constraint["name"])
    end
    return
end

function read_constraints(
    model::Model, object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    for constraint in object["constraints"]
        _add_constraint(model, constraint, name_map)
    end
end

function read_objective_sense(sense::String)
    if sense == "min"
        return MOI.MIN_SENSE
    elseif sense == "max"
        return MOI.MAX_SENSE
    else
        @assert sense == "feasibility"
        return MOI.FEASIBILITY_SENSE
    end
end


"""
    function_to_moi(x::Object, name_map::Dict{String, MOI.VariableIndex})

Convert `x` from an OrderedDict representation into a MOI representation.
"""
function function_to_moi(x::Object, name_map)
    return function_to_moi(Val{Symbol(x["head"])}(), x, name_map)
end

function function_to_moi(
    ::Val{FunctionSymbol},
    ::Object,
    ::Dict{String, MOI.VariableIndex},
) where {FunctionSymbol}
    error(
        "Version $(VERSION) of MathOptFormat does not support the function: " *
        "$(FunctionSymbol)."
    )
end

# ========== Non-typed scalar functions ==========

function function_to_moi(
    ::Val{:SingleVariable},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.SingleVariable(name_map[object["variable"]])
end

# ========== Typed scalar functions ==========

# Here, we deal with a special case: ScalarAffineTerm, ScalarQuadraticTerm,
# VectorAffineTerm, and VectorQuadraticTerm do not contain a "head" field
# (because it is unnecessary at the JSON level).

function parse_scalar_affine_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.ScalarAffineTerm(
        object["coefficient"], name_map[object["variable"]]
    )
end

function function_to_moi(
    ::Val{:ScalarAffineFunction},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.ScalarAffineFunction{Float64}(
        parse_scalar_affine_term.(object["terms"], Ref(name_map)),
        object["constant"],
    )
end

function parse_scalar_quadratic_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.ScalarQuadraticTerm(
        object["coefficient"],
        name_map[object["variable_1"]],
        name_map[object["variable_2"]],
    )
end

function function_to_moi(
    ::Val{:ScalarQuadraticFunction},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.ScalarQuadraticFunction{Float64}(
        parse_scalar_affine_term.(object["affine_terms"], Ref(name_map)),
        parse_scalar_quadratic_term.(object["quadratic_terms"], Ref(name_map)),
        object["constant"],
    )
end

# ========== Non-typed vector functions ==========

function function_to_moi(
    ::Val{:VectorOfVariables},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.VectorOfVariables(
        [name_map[variable] for variable in object["variables"]]
    )
end

# ========== Typed vector functions ==========

function parse_vector_affine_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.VectorAffineTerm(
        object["output_index"],
        parse_scalar_affine_term(object["scalar_term"], name_map),
    )
end

function function_to_moi(
    ::Val{:VectorAffineFunction},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.VectorAffineFunction{Float64}(
        parse_vector_affine_term.(object["terms"], Ref(name_map)),
        Float64.(object["constants"]),
    )
end

function parse_vector_quadratic_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.VectorQuadraticTerm(
        object["output_index"],
        parse_scalar_quadratic_term(object["scalar_term"], name_map),
    )
end

function function_to_moi(
    ::Val{:VectorQuadraticFunction},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.VectorQuadraticFunction{Float64}(
        parse_vector_affine_term.(object["affine_terms"], Ref(name_map)),
        parse_vector_quadratic_term.(object["quadratic_terms"], Ref(name_map)),
        Float64.(object["constants"]),
    )
end

# ========== Default fallback ==========
"""
    set_to_moi(x::OrderedDict)

Convert `x` from an OrderedDict representation into a MOI representation.
"""
set_to_moi(x::Object) = set_to_moi(Val{Symbol(x["head"])}(), x)

"""
    set_info(::Type{Val{HeadName}}) where HeadName

Return a tuple of the corresponding MOI set and an ordered list of fieldnames.

`HeadName` is a symbol of the string returned by `head_name(set)`.

    HeadName = Symbol(head_name(set))
    typeof(set_info(Val{HeadName})[1]) == typeof(set)
"""
function set_info(::Type{Val{SetSymbol}}) where {SetSymbol}
    error(
        "Version $(VERSION) of MathOptFormat does not support the set: " *
        "$(SetSymbol)."
    )
end

function set_to_moi(::Val{SetSymbol}, object::Object) where {SetSymbol}
    args = set_info(Val{SetSymbol})
    SetType = args[1]
    if length(args) > 1
        return SetType([object[key] for key in args[2:end]]...)
    else
        return SetType()
    end
end

function set_to_moi(::Val{:SOS1}, object::Object)
    return MOI.SOS1(Float64.(object["weights"]))
end

function set_to_moi(::Val{:SOS2}, object::Object)
    return MOI.SOS2(Float64.(object["weights"]))
end

function set_to_moi(::Val{:IndicatorSet}, object::Object)
    set = set_to_moi(object["set"])
    indicator = if object["activate_on"] == "one"
        MOI.ACTIVATE_ON_ONE
    else
        @assert object["activate_on"] == "zero"
        MOI.ACTIVATE_ON_ZERO
    end
    return MOI.IndicatorSet{indicator}(set)
end

# ========== Non-typed scalar sets ==========
set_info(::Type{Val{:ZeroOne}}) = (MOI.ZeroOne,)
set_info(::Type{Val{:Integer}}) = (MOI.Integer,)

# ========== Typed scalar sets ==========
set_info(::Type{Val{:LessThan}}) = (MOI.LessThan, "upper")
set_info(::Type{Val{:GreaterThan}}) = (MOI.GreaterThan, "lower")
set_info(::Type{Val{:EqualTo}}) = (MOI.EqualTo, "value")
set_info(::Type{Val{:Interval}}) = (MOI.Interval, "lower", "upper")
set_info(::Type{Val{:Semiinteger}}) = (MOI.Semiinteger, "lower", "upper")
set_info(::Type{Val{:Semicontinuous}}) = (MOI.Semicontinuous, "lower", "upper")

# ========== Non-typed vector sets ==========
set_info(::Type{Val{:Zeros}}) = (MOI.Zeros, "dimension")
set_info(::Type{Val{:Reals}}) = (MOI.Reals, "dimension")
set_info(::Type{Val{:Nonnegatives}}) = (MOI.Nonnegatives, "dimension")
set_info(::Type{Val{:Nonpositives}}) = (MOI.Nonpositives, "dimension")
set_info(::Type{Val{:SecondOrderCone}}) = (MOI.SecondOrderCone, "dimension")
function set_info(::Type{Val{:RotatedSecondOrderCone}})
    return MOI.RotatedSecondOrderCone, "dimension"
end
set_info(::Type{Val{:GeometricMeanCone}}) = (MOI.GeometricMeanCone, "dimension")
set_info(::Type{Val{:NormOneCone}}) = (MOI.NormOneCone, "dimension")
set_info(::Type{Val{:NormInfinityCone}}) = (MOI.NormInfinityCone, "dimension")
set_info(::Type{Val{:RelativeEntropyCone}}) = (MOI.RelativeEntropyCone, "dimension")
set_info(::Type{Val{:NormSpectralCone}}) = (MOI.NormSpectralCone, "row_dim", "column_dim")
set_info(::Type{Val{:NormNuclearCone}}) = (MOI.NormNuclearCone, "row_dim", "column_dim")
function set_info(::Type{Val{:RootDetConeTriangle}})
    return MOI.RootDetConeTriangle, "side_dimension"
end
function set_info(::Type{Val{:RootDetConeSquare}})
    return MOI.RootDetConeSquare, "side_dimension"
end
function set_info(::Type{Val{:LogDetConeTriangle}})
    return MOI.LogDetConeTriangle, "side_dimension"
end
function set_info(::Type{Val{:LogDetConeSquare}})
    return MOI.LogDetConeSquare, "side_dimension"
end
function set_info(::Type{Val{:PositiveSemidefiniteConeTriangle}})
    return MOI.PositiveSemidefiniteConeTriangle, "side_dimension"
end
function set_info(::Type{Val{:PositiveSemidefiniteConeSquare}})
    return MOI.PositiveSemidefiniteConeSquare, "side_dimension"
end
set_info(::Type{Val{:ExponentialCone}}) = (MOI.ExponentialCone, )
set_info(::Type{Val{:DualExponentialCone}}) = (MOI.DualExponentialCone, )

# ========== Typed vector sets ==========
set_info(::Type{Val{:PowerCone}}) = (MOI.PowerCone, "exponent")
set_info(::Type{Val{:DualPowerCone}}) = (MOI.DualPowerCone, "exponent")
