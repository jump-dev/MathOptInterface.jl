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
    object = JSON.parse(io; dicttype = UnorderedObject)
    file_version = _parse_mof_version(object["version"]::UnorderedObject)
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
    name_map = Dict{String, MOI.VariableIndex}()
    for variable::typeof(object) in object["variables"]
        name = get(variable, "name", "")::String
        if isempty(name)
            error(
                "Variable name is `\"\"`. MathOptFormat variable names must " *
                "be unique and non-empty."
            )
        end
        name_map[name] = index = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), index, name)
    end
    return name_map
end

function read_objective(
    model::Model,
    object::Object,
    name_map::Dict{String, MOI.VariableIndex}
)
    obj = object["objective"]::typeof(object)
    sense = read_objective_sense(obj["sense"]::String)
    MOI.set(model, MOI.ObjectiveSense(), sense)
    if sense == MOI.FEASIBILITY_SENSE
        return
    end
    func = function_to_moi(obj["function"]::typeof(object), name_map)
    MOI.set(model, MOI.ObjectiveFunction{typeof(func)}(), func)
    return
end

function _add_constraint(
    model::Model,
    object::Object,
    name_map::Dict{String, MOI.VariableIndex}
)
    index = MOI.add_constraint(
        model,
        function_to_moi(object["function"]::typeof(object), name_map),
        set_to_moi(object["set"]::typeof(object)),
    )
    if haskey(object, "name")
        MOI.set(model, MOI.ConstraintName(), index, object["name"]::String)
    end
    return
end

function read_constraints(
    model::Model,
    object::Object,
    name_map::Dict{String, MOI.VariableIndex}
)
    for constraint::typeof(object) in object["constraints"]::Vector
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
    function_to_moi(
        x::Object, name_map::Dict{String, MOI.VariableIndex}
    )

Convert `x` from an MOF representation into a MOI representation.
"""
function function_to_moi(
    x::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return function_to_moi(Val(Symbol(x["head"]::String)), x, name_map)
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
    return MOI.SingleVariable(name_map[object["variable"]::String])
end

# ========== Typed scalar functions ==========

# Here, we deal with a special case: ScalarAffineTerm, ScalarQuadraticTerm,
# VectorAffineTerm, and VectorQuadraticTerm do not contain a "head" field
# (because it is unnecessary at the JSON level).

function parse_scalar_affine_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.ScalarAffineTerm(
        object["coefficient"]::Float64, name_map[object["variable"]::String]
    )
end

function function_to_moi(
    ::Val{:ScalarAffineFunction},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.ScalarAffineFunction{Float64}(
        parse_scalar_affine_term.(object["terms"], Ref(name_map)),
        object["constant"]::Float64,
    )
end

function parse_scalar_quadratic_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.ScalarQuadraticTerm(
        object["coefficient"]::Float64,
        name_map[object["variable_1"]::String],
        name_map[object["variable_2"]::String],
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
        object["constant"]::Float64,
    )
end

# ========== Non-typed vector functions ==========

function function_to_moi(
    ::Val{:VectorOfVariables},
    object::Object,
    name_map::Dict{String, MOI.VariableIndex},
)
    return MOI.VectorOfVariables(
        [name_map[variable] for variable::String in object["variables"]]
    )
end

# ========== Typed vector functions ==========

function parse_vector_affine_term(
    object::Object, name_map::Dict{String, MOI.VariableIndex}
)
    return MOI.VectorAffineTerm(
        object["output_index"]::Int,
        parse_scalar_affine_term(object["scalar_term"]::typeof(object), name_map),
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
        object["output_index"]::Int,
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
    set_to_moi(x::Object)

Convert `x` from an OrderedDict representation into a MOI representation.
"""
set_to_moi(x::Object) = set_to_moi(Val(Symbol(x["head"]::String)), x)

"""
    set_info(::Val{HeadName}) where HeadName

Return a tuple of the corresponding MOI set and an ordered list of fieldnames.

`HeadName` is a symbol of the string returned by `head_name(set)`.

    HeadName = Symbol(head_name(set))
    typeof(set_info(Val{HeadName}())[1]) == typeof(set)
"""
function set_info(::Val{S}) where {S}
    error("Version $(VERSION) of MathOptFormat does not support the set: $S.")
end

function set_to_moi(val::Val{S}, object::Object) where {S}
    args = set_info(val)
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
    set = set_to_moi(object["set"]::typeof(object))
    indicator = if object["activate_on"]::String == "one"
        MOI.ACTIVATE_ON_ONE
    else
        @assert object["activate_on"]::String == "zero"
        MOI.ACTIVATE_ON_ZERO
    end
    return MOI.IndicatorSet{indicator}(set)
end

# ========== Non-typed scalar sets ==========
set_info(::Val{:ZeroOne}) = (MOI.ZeroOne,)
set_info(::Val{:Integer}) = (MOI.Integer,)

# ========== Typed scalar sets ==========
set_info(::Val{:LessThan}) = (MOI.LessThan, "upper")
set_info(::Val{:GreaterThan}) = (MOI.GreaterThan, "lower")
set_info(::Val{:EqualTo}) = (MOI.EqualTo, "value")
set_info(::Val{:Interval}) = (MOI.Interval, "lower", "upper")
set_info(::Val{:Semiinteger}) = (MOI.Semiinteger, "lower", "upper")
set_info(::Val{:Semicontinuous}) = (MOI.Semicontinuous, "lower", "upper")

# ========== Non-typed vector sets ==========
set_info(::Val{:Zeros}) = (MOI.Zeros, "dimension")
set_info(::Val{:Reals}) = (MOI.Reals, "dimension")
set_info(::Val{:Nonnegatives}) = (MOI.Nonnegatives, "dimension")
set_info(::Val{:Nonpositives}) = (MOI.Nonpositives, "dimension")
set_info(::Val{:SecondOrderCone}) = (MOI.SecondOrderCone, "dimension")
function set_info(::Val{:RotatedSecondOrderCone})
    return MOI.RotatedSecondOrderCone, "dimension"
end
set_info(::Val{:GeometricMeanCone}) = (MOI.GeometricMeanCone, "dimension")
set_info(::Val{:NormOneCone}) = (MOI.NormOneCone, "dimension")
set_info(::Val{:NormInfinityCone}) = (MOI.NormInfinityCone, "dimension")
set_info(::Val{:RelativeEntropyCone}) = (MOI.RelativeEntropyCone, "dimension")
set_info(::Val{:NormSpectralCone}) = (MOI.NormSpectralCone, "row_dim", "column_dim")
set_info(::Val{:NormNuclearCone}) = (MOI.NormNuclearCone, "row_dim", "column_dim")
function set_info(::Val{:RootDetConeTriangle})
    return MOI.RootDetConeTriangle, "side_dimension"
end
function set_info(::Val{:RootDetConeSquare})
    return MOI.RootDetConeSquare, "side_dimension"
end
function set_info(::Val{:LogDetConeTriangle})
    return MOI.LogDetConeTriangle, "side_dimension"
end
function set_info(::Val{:LogDetConeSquare})
    return MOI.LogDetConeSquare, "side_dimension"
end
function set_info(::Val{:PositiveSemidefiniteConeTriangle})
    return MOI.PositiveSemidefiniteConeTriangle, "side_dimension"
end
function set_info(::Val{:PositiveSemidefiniteConeSquare})
    return MOI.PositiveSemidefiniteConeSquare, "side_dimension"
end
set_info(::Val{:ExponentialCone}) = (MOI.ExponentialCone, )
set_info(::Val{:DualExponentialCone}) = (MOI.DualExponentialCone, )

# ========== Typed vector sets ==========
set_info(::Val{:PowerCone}) = (MOI.PowerCone, "exponent")
set_info(::Val{:DualPowerCone}) = (MOI.DualPowerCone, "exponent")
