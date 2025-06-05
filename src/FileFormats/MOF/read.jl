# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    Base.read!(io::IO, model::FileFormats.MOF.Model)

Read `io` in the MathOptFormat file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read model from file as destination model is not empty.")
    end
    object = JSON3.read(io, Dict{String,Any})
    file_version = _parse_mof_version(object["version"]::Dict{String,Any})
    if !(file_version in _SUPPORTED_VERSIONS)
        version = _SUPPORTED_VERSIONS[1]
        error(
            "Sorry, the file can't be read because this library supports " *
            "v$version of MathOptFormat, but the file you are trying to " *
            "read is v$(file_version).",
        )
    end
    name_map = read_variables(model, object)
    read_objective(model, object, name_map)
    read_constraints(model, object, name_map)
    options = get_options(model)
    # We should convert to NLPBlock if...
    #                                        |  options.use_nlp_block
    #                                        | true    false   nothing
    # object["has_scalar_nonlinear"] = false |   1       0        1
    #                                = true  |   1       0        0
    if something(
        options.use_nlp_block,
        !get(object, "has_scalar_nonlinear", false),
    )
        _convert_to_nlpblock(model)
    end
    return
end

function _convert_to_nlpblock(model::Model{T}) where {T}
    needs_nlp_block = false
    nlp_model = MOI.Nonlinear.Model()
    F = MOI.ScalarNonlinearFunction
    for S in
        (MOI.LessThan{T}, MOI.GreaterThan{T}, MOI.EqualTo{T}, MOI.Interval{T})
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            f = MOI.get(model, MOI.ConstraintFunction(), ci)
            set = MOI.get(model, MOI.ConstraintSet(), ci)
            MOI.Nonlinear.add_constraint(nlp_model, f, set)
            # We don't need this in `model` any more.
            MOI.delete(model, ci)
            needs_nlp_block = true
        end
    end
    if MOI.get(model, MOI.ObjectiveFunctionType()) == F
        obj = MOI.get(model, MOI.ObjectiveFunction{F}())
        MOI.Nonlinear.set_objective(nlp_model, obj)
        MOI.set(
            model,
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}(),
            zero(MOI.ScalarAffineFunction{T}),
        )
        needs_nlp_block = true
    end
    if needs_nlp_block
        options = get_options(model)
        evaluator = MOI.Nonlinear.Evaluator(
            nlp_model,
            options.differentiation_backend,
            MOI.get(model, MOI.ListOfVariableIndices()),
        )
        MOI.set(model, MOI.NLPBlock(), MOI.NLPBlockData(evaluator))
    end
    return
end

function read_variables(model::Model{T}, object::Dict) where {T}
    name_map = Dict{String,MOI.VariableIndex}()
    for variable::typeof(object) in object["variables"]
        name = get(variable, "name", "")::String
        if isempty(name)
            error(
                "Variable name is `\"\"`. MathOptFormat variable names must " *
                "be unique and non-empty.",
            )
        end
        name_map[name] = index = MOI.add_variable(model)
        MOI.set(model, MOI.VariableName(), index, name)
        if haskey(variable, "primal_start")
            MOI.set(
                model,
                MOI.VariablePrimalStart(),
                index,
                convert(T, variable["primal_start"]),
            )
        end
    end
    return name_map
end

function read_objective(
    model::Model{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    obj = object["objective"]::typeof(object)
    sense = read_objective_sense(obj["sense"]::String)
    MOI.set(model, MOI.ObjectiveSense(), sense)
    if sense == MOI.FEASIBILITY_SENSE
        return
    end
    func = function_to_moi(T, obj["function"]::typeof(object), name_map)
    MOI.set(model, MOI.ObjectiveFunction{typeof(func)}(), func)
    return
end

_to_T(::Type{T}, x::Number) where {T} = convert(T, x)
_to_T(::Type{T}, x::AbstractVector) where {T} = convert(Vector{T}, x)

function _add_constraint(
    model::Model{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    f = function_to_moi(T, object["function"]::typeof(object), name_map)
    s = set_to_moi(T, object["set"]::typeof(object))
    index = MOI.add_constraint(model, f, s)
    if haskey(object, "name") && typeof(f) != MOI.VariableIndex
        MOI.set(model, MOI.ConstraintName(), index, object["name"]::String)
    end
    if haskey(object, "dual_start")
        MOI.set(
            model,
            MOI.ConstraintDualStart(),
            index,
            _to_T(T, object["dual_start"]),
        )
    end
    if haskey(object, "primal_start")
        MOI.set(
            model,
            MOI.ConstraintPrimalStart(),
            index,
            _to_T(T, object["primal_start"]),
        )
    end
    return
end

function read_constraints(
    model::Model,
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
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
        Expr(:return, Val(arg1)),
    )
    leaf = body
    for arg in args
        new_expr = Expr(
            :elseif,
            Expr(:call, :(==), head, string(arg)),
            Expr(:return, Val(arg)),
        )
        push!(leaf.args, new_expr)
        leaf = new_expr
    end
    push!(leaf.args, Expr(:return, Val(head)))
    quote
        function $(esc(f))($head::String)
            return $body
        end
    end
end

@head_to_val(
    head_to_function,
    SingleVariable,     # Required for v0.6
    Variable,           # Required for v1.0
    VectorOfVariables,
    ScalarAffineFunction,
    ScalarQuadraticFunction,
    VectorAffineFunction,
    VectorQuadraticFunction,
    ScalarNonlinearFunction,
    VectorNonlinearFunction,
)

"""
    function_to_moi(
        ::Type{T},
        x::Dict,
        name_map::Dict{String,MOI.VariableIndex},
    ) where {T}

Convert `x` from an MOF representation into a MOI representation.
"""
function function_to_moi(
    ::Type{T},
    x::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    head = if haskey(x, "type")
        x["type"]::String
    else
        x["head"]::String  # Required for v0.4
    end
    return function_to_moi(head_to_function(head), T, x, name_map)
end

function function_to_moi(
    ::Val{FunctionSymbol},
    ::Type{T},
    ::Dict,
    ::Dict{String,MOI.VariableIndex},
) where {FunctionSymbol,T}
    return error(
        "Version $(_SUPPORTED_VERSIONS[1]) of MathOptFormat does not support " *
        "the function: $(FunctionSymbol).",
    )
end

# ========== Non-typed scalar functions ==========

# Required for v0.6
function function_to_moi(
    ::Val{:SingleVariable},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return name_map[object["variable"]::String]
end

# Required for v1.0
function function_to_moi(
    ::Val{:Variable},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return name_map[object["name"]::String]
end

function function_to_moi(
    ::Val{:ScalarNonlinearFunction},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return _parse_scalar_nonlinear_function(
        T,
        object["root"],
        object["node_list"],
        name_map,
    )::MOI.ScalarNonlinearFunction
end

function _parse_scalar_nonlinear_function(
    ::Type{T},
    node::Real,
    ::Vector,
    ::Dict{String,MOI.VariableIndex},
) where {T}
    return node
end

function _parse_scalar_nonlinear_function(
    ::Type{T},
    node::String,
    ::Vector,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return name_map[node]
end

function _parse_scalar_nonlinear_function(
    ::Type{T},
    node::Dict,
    node_list::Vector,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    head = node["type"]
    if head == "real"
        # Required for v1.6 and earlier
        return node["value"]
    elseif head == "complex"
        return Complex{T}(node["real"], node["imag"])
    elseif head == "variable"
        # Required for v1.6 and earlier
        return name_map[node["name"]]
    elseif head == "node"
        return _parse_scalar_nonlinear_function(
            T,
            node_list[node["index"]],
            node_list,
            name_map,
        )
    end
    f = MOI.ScalarNonlinearFunction(Symbol(head), Any[])
    for arg in node["args"]
        push!(
            f.args,
            _parse_scalar_nonlinear_function(T, arg, node_list, name_map),
        )
    end
    return f
end

# ========== Typed scalar functions ==========

# Here, we deal with a special case: ScalarAffineTerm, ScalarQuadraticTerm,
# VectorAffineTerm, and VectorQuadraticTerm do not contain a "type" field
# (because it is unnecessary at the JSON level).

function parse_scalar_affine_term(
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.ScalarAffineTerm{T}(
        convert(T, object["coefficient"]),
        name_map[object["variable"]::String],
    )
end

function function_to_moi(
    ::Val{:ScalarAffineFunction},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.ScalarAffineFunction{T}(
        parse_scalar_affine_term.(T, object["terms"], Ref(name_map)),
        convert(T, object["constant"]),
    )
end

function parse_scalar_quadratic_term(
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.ScalarQuadraticTerm{T}(
        convert(T, object["coefficient"]),
        name_map[object["variable_1"]::String],
        name_map[object["variable_2"]::String],
    )
end

function function_to_moi(
    ::Val{:ScalarQuadraticFunction},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.ScalarQuadraticFunction{T}(
        parse_scalar_quadratic_term.(
            T,
            object["quadratic_terms"],
            Ref(name_map),
        ),
        parse_scalar_affine_term.(T, object["affine_terms"], Ref(name_map)),
        convert(T, object["constant"]),
    )
end

# ========== Non-typed vector functions ==========

function function_to_moi(
    ::Val{:VectorOfVariables},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.VectorOfVariables(
        MOI.VariableIndex[
            name_map[variable] for variable::String in object["variables"]
        ],
    )
end

function function_to_moi(
    ::Val{:VectorNonlinearFunction},
    ::Type{T},
    object::Dict{String,Any},
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    node_list = Dict{String,Any}.(object["node_list"])
    rows = map(object["rows"]) do r
        return _parse_scalar_nonlinear_function(T, r, node_list, name_map)
    end
    return MOI.VectorNonlinearFunction(rows)
end

# ========== Typed vector functions ==========

function parse_vector_affine_term(
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.VectorAffineTerm{T}(
        object["output_index"],
        parse_scalar_affine_term(
            T,
            object["scalar_term"]::typeof(object),
            name_map,
        ),
    )
end

function function_to_moi(
    ::Val{:VectorAffineFunction},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.VectorAffineFunction{T}(
        parse_vector_affine_term.(T, object["terms"], Ref(name_map)),
        convert(Vector{T}, object["constants"]),
    )
end

function parse_vector_quadratic_term(
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.VectorQuadraticTerm{T}(
        object["output_index"],
        parse_scalar_quadratic_term(T, object["scalar_term"], name_map),
    )
end

function function_to_moi(
    ::Val{:VectorQuadraticFunction},
    ::Type{T},
    object::Dict,
    name_map::Dict{String,MOI.VariableIndex},
) where {T}
    return MOI.VectorQuadraticFunction{T}(
        parse_vector_quadratic_term.(
            T,
            object["quadratic_terms"],
            Ref(name_map),
        ),
        parse_vector_affine_term.(T, object["affine_terms"], Ref(name_map)),
        convert(Vector{T}, object["constants"]),
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
    Parameter,
    Zeros,
    Reals,
    Nonnegatives,
    Nonpositives,
    HyperRectangle,
    SecondOrderCone,
    RotatedSecondOrderCone,
    GeometricMeanCone,
    NormOneCone,
    NormInfinityCone,
    NormCone,
    RelativeEntropyCone,
    NormSpectralCone,
    NormNuclearCone,
    RootDetConeTriangle,
    RootDetConeSquare,
    LogDetConeTriangle,
    LogDetConeSquare,
    PositiveSemidefiniteConeTriangle,
    ScaledPositiveSemidefiniteConeTriangle, # Required for v1.4
    Scaled,                                 # Required for v1.5
    PositiveSemidefiniteConeSquare,
    HermitianPositiveSemidefiniteConeTriangle,
    ExponentialCone,
    DualExponentialCone,
    PowerCone,
    DualPowerCone,
    SOS1,
    SOS2,
    IndicatorSet,   # Required for v0.6
    Indicator,      # Required for v1.0
    Reified,
    Complements,
    AllDifferent,
    Circuit,
    CountAtLeast,
    CountBelongs,
    CountDistinct,
    CountGreaterThan,
    Cumulative,
    Path,
    BinPacking,
    Table,
)

"""
    set_to_moi(::Type{T}, x::Dict) where {T}

Convert `x` from a Dict representation into a MOI representation.
"""
function set_to_moi(::Type{T}, x::Dict) where {T}
    head = if haskey(x, "type")
        x["type"]::String
    else
        x["head"]::String  # Required for <v0.4
    end
    return set_to_moi(head_to_set(head), T, x)
end

set_to_moi(v::Val, ::Type{T}, object::Dict) where {T} = set_to_moi(v, object)

# ========== Non-typed scalar sets ==========

set_to_moi(::Val{:ZeroOne}, ::Dict) = MOI.ZeroOne()

set_to_moi(::Val{:Integer}, ::Dict) = MOI.Integer()

# ========== Typed scalar sets ==========

function set_to_moi(::Val{:LessThan}, ::Type{T}, object::Dict) where {T}
    return MOI.LessThan{T}(object["upper"])
end

function set_to_moi(::Val{:GreaterThan}, ::Type{T}, object::Dict) where {T}
    return MOI.GreaterThan{T}(object["lower"])
end

function set_to_moi(::Val{:EqualTo}, ::Type{T}, object::Dict) where {T}
    return MOI.EqualTo{T}(object["value"])
end

function set_to_moi(::Val{:Interval}, ::Type{T}, object::Dict) where {T}
    return MOI.Interval{T}(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Semiinteger}, ::Type{T}, object::Dict) where {T}
    return MOI.Semiinteger{T}(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Semicontinuous}, ::Type{T}, object::Dict) where {T}
    return MOI.Semicontinuous{T}(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Parameter}, ::Type{T}, object::Dict) where {T}
    return MOI.Parameter{T}(object["value"])
end

# ========== Non-typed vector sets ==========

function set_to_moi(::Val{:Zeros}, object::Dict)
    return MOI.Zeros(object["dimension"])
end

function set_to_moi(::Val{:Reals}, object::Dict)
    return MOI.Reals(object["dimension"])
end

function set_to_moi(::Val{:Nonnegatives}, object::Dict)
    return MOI.Nonnegatives(object["dimension"])
end

function set_to_moi(::Val{:Nonpositives}, object::Dict)
    return MOI.Nonpositives(object["dimension"])
end

function set_to_moi(::Val{:SecondOrderCone}, object::Dict)
    return MOI.SecondOrderCone(object["dimension"])
end

function set_to_moi(::Val{:RotatedSecondOrderCone}, object::Dict)
    return MOI.RotatedSecondOrderCone(object["dimension"])
end

function set_to_moi(::Val{:GeometricMeanCone}, object::Dict)
    return MOI.GeometricMeanCone(object["dimension"])
end

function set_to_moi(::Val{:NormOneCone}, object::Dict)
    return MOI.NormOneCone(object["dimension"])
end

function set_to_moi(::Val{:NormInfinityCone}, object::Dict)
    return MOI.NormInfinityCone(object["dimension"])
end

function set_to_moi(::Val{:NormCone}, object::Dict)
    return MOI.NormCone(object["p"], object["dimension"])
end

function set_to_moi(::Val{:RelativeEntropyCone}, object::Dict)
    return MOI.RelativeEntropyCone(object["dimension"])
end

function set_to_moi(::Val{:NormSpectralCone}, object::Dict)
    return MOI.NormSpectralCone(object["row_dim"], object["column_dim"])
end

function set_to_moi(::Val{:NormNuclearCone}, object::Dict)
    return MOI.NormNuclearCone(object["row_dim"], object["column_dim"])
end

function set_to_moi(::Val{:RootDetConeTriangle}, object::Dict)
    return MOI.RootDetConeTriangle(object["side_dimension"])
end

function set_to_moi(::Val{:RootDetConeSquare}, object::Dict)
    return MOI.RootDetConeSquare(object["side_dimension"])
end

function set_to_moi(::Val{:LogDetConeTriangle}, object::Dict)
    return MOI.LogDetConeTriangle(object["side_dimension"])
end

function set_to_moi(::Val{:LogDetConeSquare}, object::Dict)
    return MOI.LogDetConeSquare(object["side_dimension"])
end

function set_to_moi(::Val{:PositiveSemidefiniteConeTriangle}, object::Dict)
    return MOI.PositiveSemidefiniteConeTriangle(object["side_dimension"])
end

function set_to_moi(
    ::Val{:ScaledPositiveSemidefiniteConeTriangle},
    object::Dict,
)
    d = object["side_dimension"]
    return MOI.Scaled(MOI.PositiveSemidefiniteConeTriangle(d))
end

function set_to_moi(::Val{:Scaled}, ::Type{T}, object::Dict) where {T}
    return MOI.Scaled(set_to_moi(T, object["set"]))
end

function set_to_moi(::Val{:PositiveSemidefiniteConeSquare}, object::Dict)
    return MOI.PositiveSemidefiniteConeSquare(object["side_dimension"])
end

function set_to_moi(
    ::Val{:HermitianPositiveSemidefiniteConeTriangle},
    object::Dict,
)
    side_dimension = object["side_dimension"]
    return MOI.HermitianPositiveSemidefiniteConeTriangle(side_dimension)
end

function set_to_moi(::Val{:ExponentialCone}, object::Dict)
    return MOI.ExponentialCone()
end

function set_to_moi(::Val{:DualExponentialCone}, object::Dict)
    return MOI.DualExponentialCone()
end

function set_to_moi(::Val{:Complements}, object::Dict)
    return MOI.Complements(object["dimension"])
end

function set_to_moi(::Val{:AllDifferent}, object::Dict)
    return MOI.AllDifferent(object["dimension"])
end

function set_to_moi(::Val{:Circuit}, object::Dict)
    return MOI.Circuit(object["dimension"])
end

function set_to_moi(::Val{:CountAtLeast}, object::Dict)
    return MOI.CountAtLeast(
        object["n"],
        convert(Vector{Int}, object["partitions"]),
        Set{Int}(object["set"]),
    )
end

function set_to_moi(::Val{:CountBelongs}, object::Dict)
    return MOI.CountBelongs(object["dimension"], Set{Int}(object["set"]))
end

function set_to_moi(::Val{:CountDistinct}, object::Dict)
    return MOI.CountDistinct(object["dimension"])
end

function set_to_moi(::Val{:CountGreaterThan}, object::Dict)
    return MOI.CountGreaterThan(object["dimension"])
end

function set_to_moi(::Val{:Cumulative}, object::Dict)
    return MOI.Cumulative(object["dimension"])
end

function set_to_moi(::Val{:Path}, object::Dict)
    return MOI.Path(Int.(object["from"]), Int.(object["to"]))
end

# ========== Typed vector sets ==========

function set_to_moi(::Val{:PowerCone}, ::Type{T}, object::Dict) where {T}
    return MOI.PowerCone{T}(object["exponent"])
end

function set_to_moi(::Val{:DualPowerCone}, ::Type{T}, object::Dict) where {T}
    return MOI.DualPowerCone{T}(object["exponent"])
end

function set_to_moi(::Val{:SOS1}, ::Type{T}, object::Dict) where {T}
    return MOI.SOS1(convert(Vector{T}, object["weights"]))
end

function set_to_moi(::Val{:SOS2}, ::Type{T}, object::Dict) where {T}
    return MOI.SOS2(convert(Vector{T}, object["weights"]))
end

function set_to_moi(::Val{:HyperRectangle}, ::Type{T}, object::Dict) where {T}
    return MOI.HyperRectangle(
        convert(Vector{T}, object["lower"]),
        convert(Vector{T}, object["upper"]),
    )
end

# :IndicatorSet is required for v0.6
# :Indicator is required for v1.0
function set_to_moi(
    ::Union{Val{:Indicator},Val{:IndicatorSet}},
    ::Type{T},
    object::Dict,
) where {T}
    set = set_to_moi(T, object["set"]::typeof(object))
    if object["activate_on"]::String == "one"
        return MOI.Indicator{MOI.ACTIVATE_ON_ONE}(set)
    else
        @assert object["activate_on"]::String == "zero"
        return MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(set)
    end
end

function set_to_moi(::Val{:Reified}, ::Type{T}, object::Dict) where {T}
    return MOI.Reified(set_to_moi(T, object["set"]::typeof(object)))
end

function set_to_moi(::Val{:BinPacking}, ::Type{T}, object::Dict) where {T}
    return MOI.BinPacking(
        convert(T, object["capacity"]),
        convert(Vector{T}, object["weights"]),
    )
end

function set_to_moi(::Val{:Table}, ::Type{T}, object::Dict) where {T}
    table = convert(Matrix{T}, vcat([t' for t in object["table"]]...))
    return MOI.Table(table)
end
