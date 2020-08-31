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
    if file_version.major != VERSION.major || file_version.minor > VERSION.minor
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

macro head_to_val(f, arg1, args...)
    head = gensym()
    body = Expr(
        :if,
        Expr(:call, :(==), head, string(arg1)),
        Expr(:return, Val(arg1))
    )
    leaf = body
    for arg in args
        new_expr = Expr(
            :elseif,
            Expr(:call, :(==), head, string(arg)),
            Expr(:return, Val(arg))
        )
        push!(leaf.args, new_expr)
        leaf = new_expr
    end
    push!(leaf.args, Expr(:return, Val(head)))
    quote
        function $(esc(f))($head::String)
            $body
        end
    end
end

@head_to_val(
    head_to_function,
    SingleVariable,
    VectorOfVariables,
    ScalarAffineFunction,
    ScalarQuadraticFunction,
    VectorAffineFunction,
    VectorQuadraticFunction,
    ScalarNonlinearFunction,
)

"""
    function_to_moi(
        x::Object, name_map::Dict{String, MOI.VariableIndex}
    )

Convert `x` from an MOF representation into a MOI representation.
"""
function function_to_moi(
    x::Object, name_map::Dict{String, MOI.VariableIndex}
)
    val = if haskey(x, "type")
        head_to_function(x["type"]::String)
    else
        # TODO(odow): remove when v0.4 no longer supported.
        head_to_function(x["head"]::String)
    end
    return function_to_moi(val, x, name_map)
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
# VectorAffineTerm, and VectorQuadraticTerm do not contain a "type" field
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

@head_to_val(
    head_to_set,
    ZeroOne,
    Integer,
    LessThan,
    GreaterThan,
    EqualTo,
    Interval,
    Semiinteger,
    Semicontinuous,
    Zeros,
    Reals,
    Nonnegatives,
    Nonpositives,
    SecondOrderCone,
    RotatedSecondOrderCone,
    GeometricMeanCone,
    NormOneCone,
    NormInfinityCone,
    RelativeEntropyCone,
    NormSpectralCone,
    NormNuclearCone,
    RootDetConeTriangle,
    RootDetConeSquare,
    LogDetConeTriangle,
    LogDetConeSquare,
    PositiveSemidefiniteConeTriangle,
    PositiveSemidefiniteConeSquare,
    ExponentialCone,
    DualExponentialCone,
    PowerCone,
    DualPowerCone,
    SOS1,
    SOS2,
    IndicatorSet,
)

"""
    set_to_moi(x::Object)

Convert `x` from an OrderedDict representation into a MOI representation.
"""
function set_to_moi(x::Object)
    if haskey(x, "type")
        return set_to_moi(head_to_set(x["type"]::String), x)
    else
        # TODO(odow): remove when v0.4 no longer supported.
        return set_to_moi(head_to_set(x["head"]::String), x)
    end
end

# ========== Non-typed scalar sets ==========

function set_to_moi(::Val{:ZeroOne}, ::Object)
    return MOI.ZeroOne()
end

function set_to_moi(::Val{:Integer}, ::Object)
    return MOI.Integer()
end

# ========== Typed scalar sets ==========

function set_to_moi(::Val{:LessThan}, object::Object)
    return MOI.LessThan(object["upper"])
end

function set_to_moi(::Val{:GreaterThan}, object::Object)
    return MOI.GreaterThan(object["lower"])
end

function set_to_moi(::Val{:EqualTo}, object::Object)
    return MOI.EqualTo(object["value"])
end

function set_to_moi(::Val{:Interval}, object::Object)
    return MOI.Interval(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Semiinteger}, object::Object)
    return MOI.Semiinteger(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Semicontinuous}, object::Object)
    return MOI.Semicontinuous(object["lower"], object["upper"])
end

# ========== Non-typed vector sets ==========

function set_to_moi(::Val{:Zeros}, object::Object)
    return MOI.Zeros(object["dimension"]::Int)
end

function set_to_moi(::Val{:Reals}, object::Object)
    return MOI.Reals(object["dimension"]::Int)
end

function set_to_moi(::Val{:Nonnegatives}, object::Object)
    return MOI.Nonnegatives(object["dimension"]::Int)
end

function set_to_moi(::Val{:Nonpositives}, object::Object)
    return MOI.Nonpositives(object["dimension"]::Int)
end

function set_to_moi(::Val{:SecondOrderCone}, object::Object)
    return MOI.SecondOrderCone(object["dimension"]::Int)
end

function set_to_moi(::Val{:RotatedSecondOrderCone}, object::Object)
    return MOI.RotatedSecondOrderCone(object["dimension"]::Int)
end

function set_to_moi(::Val{:GeometricMeanCone}, object::Object)
    return MOI.GeometricMeanCone(object["dimension"]::Int)
end

function set_to_moi(::Val{:NormOneCone}, object::Object)
    return MOI.NormOneCone(object["dimension"]::Int)
end

function set_to_moi(::Val{:NormInfinityCone}, object::Object)
    return MOI.NormInfinityCone(object["dimension"]::Int)
end

function set_to_moi(::Val{:RelativeEntropyCone}, object::Object)
    return MOI.RelativeEntropyCone(object["dimension"]::Int)
end

function set_to_moi(::Val{:NormSpectralCone}, object::Object)
    return MOI.NormSpectralCone(
        object["row_dim"]::Int, object["column_dim"]::Int
    )
end

function set_to_moi(::Val{:NormNuclearCone}, object::Object)
    return MOI.NormNuclearCone(
        object["row_dim"]::Int, object["column_dim"]::Int
    )
end

function set_to_moi(::Val{:RootDetConeTriangle}, object::Object)
    return MOI.RootDetConeTriangle(object["side_dimension"]::Int)
end

function set_to_moi(::Val{:RootDetConeSquare}, object::Object)
    return MOI.RootDetConeSquare(object["side_dimension"]::Int)
end

function set_to_moi(::Val{:LogDetConeTriangle}, object::Object)
    return MOI.LogDetConeTriangle(object["side_dimension"]::Int)
end

function set_to_moi(::Val{:LogDetConeSquare}, object::Object)
    return MOI.LogDetConeSquare(object["side_dimension"]::Int)
end

function set_to_moi(::Val{:PositiveSemidefiniteConeTriangle}, object::Object)
    return MOI.PositiveSemidefiniteConeTriangle(object["side_dimension"]::Int)
end

function set_to_moi(::Val{:PositiveSemidefiniteConeSquare}, object::Object)
    return MOI.PositiveSemidefiniteConeSquare(object["side_dimension"]::Int)
end

function set_to_moi(::Val{:ExponentialCone}, ::Object)
    return MOI.ExponentialCone()
end

function set_to_moi(::Val{:DualExponentialCone}, ::Object)
    return MOI.DualExponentialCone()
end

# ========== Typed vector sets ==========

function set_to_moi(::Val{:PowerCone}, object::Object)
    return MOI.PowerCone(object["exponent"]::Float64)
end

function set_to_moi(::Val{:DualPowerCone}, object::Object)
    return MOI.DualPowerCone(object["exponent"]::Float64)
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
