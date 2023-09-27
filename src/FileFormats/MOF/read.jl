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
    object = JSON.parse(io; dicttype = UnorderedObject)
    file_version = _parse_mof_version(object["version"]::UnorderedObject)
    if !(file_version in SUPPORTED_VERSIONS)
        error(
            "Sorry, the file can't be read because this library supports " *
            "v$(VERSION) of MathOptFormat, but the file you are trying to " *
            "read is v$(file_version).",
        )
    end
    name_map = read_variables(model, object)
    read_objective(model, object, name_map)
    read_constraints(model, object, name_map)
    options = get_options(model)
    if options.parse_as_nlpblock
        _convert_to_nlpblock(model)
    end
    return
end

function _convert_to_nlpblock(model::Model)
    needs_nlp_block = false
    nlp_model = MOI.Nonlinear.Model()
    F = MOI.ScalarNonlinearFunction
    for S in (
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    )
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
            MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
            zero(MOI.ScalarAffineFunction{Float64}),
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

function read_variables(model::Model, object::Object)
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
                variable["primal_start"],
            )
        end
    end
    return name_map
end

function read_objective(
    model::Model,
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
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
    name_map::Dict{String,MOI.VariableIndex},
)
    f = function_to_moi(object["function"]::typeof(object), name_map)
    s = set_to_moi(object["set"]::typeof(object))
    index = MOI.add_constraint(model, f, s)
    if haskey(object, "name") && typeof(f) != MOI.VariableIndex
        MOI.set(model, MOI.ConstraintName(), index, object["name"]::String)
    end
    if haskey(object, "dual_start")
        MOI.set(model, MOI.ConstraintDualStart(), index, object["dual_start"])
    end
    if haskey(object, "primal_start")
        MOI.set(
            model,
            MOI.ConstraintPrimalStart(),
            index,
            object["primal_start"],
        )
    end
    return
end

function read_constraints(
    model::Model,
    object::Object,
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
    function_to_moi(x::Object, name_map::Dict{String,MOI.VariableIndex})

Convert `x` from an MOF representation into a MOI representation.
"""
function function_to_moi(x::Object, name_map::Dict{String,MOI.VariableIndex})
    if haskey(x, "type")
        return function_to_moi(head_to_function(x["type"]::String), x, name_map)
    else
        # Required for v0.4
        return function_to_moi(head_to_function(x["head"]::String), x, name_map)
    end
end

function function_to_moi(
    ::Val{FunctionSymbol},
    ::Object,
    ::Dict{String,MOI.VariableIndex},
) where {FunctionSymbol}
    return error(
        "Version $(VERSION) of MathOptFormat does not support the function: " *
        "$(FunctionSymbol).",
    )
end

# ========== Non-typed scalar functions ==========

# Required for v0.6
function function_to_moi(
    ::Val{:SingleVariable},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return name_map[object["variable"]::String]
end

# Required for v1.0
function function_to_moi(
    ::Val{:Variable},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return name_map[object["name"]::String]
end

function function_to_moi(
    ::Val{:ScalarNonlinearFunction},
    object::T,
    name_map::Dict{String,MOI.VariableIndex},
) where {T<:Object}
    node_list = T.(object["node_list"])
    f = _parse_scalar_nonlinear_function(object["root"], node_list, name_map)
    return f::MOI.ScalarNonlinearFunction
end

function _parse_scalar_nonlinear_function(
    node::T,
    node_list::Vector{T},
    name_map::Dict{String,MOI.VariableIndex},
) where {T<:Object}
    head = node["type"]
    if head == "real"
        return node["value"]
    elseif head == "complex"
        return Complex(node["real"], node["imag"])
    elseif head == "variable"
        return name_map[node["name"]]
    elseif head == "node"
        return _parse_scalar_nonlinear_function(
            node_list[node["index"]],
            node_list,
            name_map,
        )
    elseif !haskey(STRING_TO_FUNCTION, head)
        throw(MOI.UnsupportedNonlinearOperator(Symbol(head)))
    end
    julia_symbol, arity = STRING_TO_FUNCTION[head]
    validate_arguments(head, arity, length(node["args"]))
    f = MOI.ScalarNonlinearFunction(julia_symbol, Any[])
    for arg in node["args"]
        push!(
            f.args,
            _parse_scalar_nonlinear_function(arg, node_list, name_map),
        )
    end
    return f
end

# ========== Typed scalar functions ==========

# Here, we deal with a special case: ScalarAffineTerm, ScalarQuadraticTerm,
# VectorAffineTerm, and VectorQuadraticTerm do not contain a "type" field
# (because it is unnecessary at the JSON level).

function parse_scalar_affine_term(
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.ScalarAffineTerm(
        convert(Float64, object["coefficient"]),
        name_map[object["variable"]::String],
    )
end

function function_to_moi(
    ::Val{:ScalarAffineFunction},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.ScalarAffineFunction{Float64}(
        parse_scalar_affine_term.(object["terms"], Ref(name_map)),
        convert(Float64, object["constant"]),
    )
end

function parse_scalar_quadratic_term(
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.ScalarQuadraticTerm(
        convert(Float64, object["coefficient"]),
        name_map[object["variable_1"]::String],
        name_map[object["variable_2"]::String],
    )
end

function function_to_moi(
    ::Val{:ScalarQuadraticFunction},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.ScalarQuadraticFunction{Float64}(
        parse_scalar_quadratic_term.(object["quadratic_terms"], Ref(name_map)),
        parse_scalar_affine_term.(object["affine_terms"], Ref(name_map)),
        convert(Float64, object["constant"]),
    )
end

# ========== Non-typed vector functions ==========

function function_to_moi(
    ::Val{:VectorOfVariables},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.VectorOfVariables(
        MOI.VariableIndex[
            name_map[variable] for variable::String in object["variables"]
        ],
    )
end

function function_to_moi(
    ::Val{:VectorNonlinearFunction},
    object::T,
    name_map::Dict{String,MOI.VariableIndex},
) where {T<:Object}
    node_list = T.(object["node_list"])
    rows = map(object["rows"]) do r
        return _parse_scalar_nonlinear_function(r, node_list, name_map)
    end
    return MOI.VectorNonlinearFunction(rows)
end

# ========== Typed vector functions ==========

function parse_vector_affine_term(
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.VectorAffineTerm(
        object["output_index"],
        parse_scalar_affine_term(
            object["scalar_term"]::typeof(object),
            name_map,
        ),
    )
end

function function_to_moi(
    ::Val{:VectorAffineFunction},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.VectorAffineFunction{Float64}(
        parse_vector_affine_term.(object["terms"], Ref(name_map)),
        Float64.(object["constants"]),
    )
end

function parse_vector_quadratic_term(
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.VectorQuadraticTerm(
        object["output_index"],
        parse_scalar_quadratic_term(object["scalar_term"], name_map),
    )
end

function function_to_moi(
    ::Val{:VectorQuadraticFunction},
    object::Object,
    name_map::Dict{String,MOI.VariableIndex},
)
    return MOI.VectorQuadraticFunction{Float64}(
        parse_vector_quadratic_term.(object["quadratic_terms"], Ref(name_map)),
        parse_vector_affine_term.(object["affine_terms"], Ref(name_map)),
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
    set_to_moi(x::Object)

Convert `x` from an OrderedDict representation into a MOI representation.
"""
function set_to_moi(x::Object)
    if haskey(x, "type")
        return set_to_moi(head_to_set(x["type"]::String), x)
    else
        # Required for <v0.4
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
    return MOI.LessThan{Float64}(object["upper"])
end

function set_to_moi(::Val{:GreaterThan}, object::Object)
    return MOI.GreaterThan{Float64}(object["lower"])
end

function set_to_moi(::Val{:EqualTo}, object::Object)
    return MOI.EqualTo{Float64}(object["value"])
end

function set_to_moi(::Val{:Interval}, object::Object)
    return MOI.Interval{Float64}(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Semiinteger}, object::Object)
    return MOI.Semiinteger{Float64}(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Semicontinuous}, object::Object)
    return MOI.Semicontinuous{Float64}(object["lower"], object["upper"])
end

function set_to_moi(::Val{:Parameter}, object::Object)
    return MOI.Parameter{Float64}(object["value"])
end

# ========== Non-typed vector sets ==========

function set_to_moi(::Val{:Zeros}, object::Object)
    return MOI.Zeros(object["dimension"])
end

function set_to_moi(::Val{:Reals}, object::Object)
    return MOI.Reals(object["dimension"])
end

function set_to_moi(::Val{:Nonnegatives}, object::Object)
    return MOI.Nonnegatives(object["dimension"])
end

function set_to_moi(::Val{:Nonpositives}, object::Object)
    return MOI.Nonpositives(object["dimension"])
end

function set_to_moi(::Val{:SecondOrderCone}, object::Object)
    return MOI.SecondOrderCone(object["dimension"])
end

function set_to_moi(::Val{:RotatedSecondOrderCone}, object::Object)
    return MOI.RotatedSecondOrderCone(object["dimension"])
end

function set_to_moi(::Val{:GeometricMeanCone}, object::Object)
    return MOI.GeometricMeanCone(object["dimension"])
end

function set_to_moi(::Val{:NormOneCone}, object::Object)
    return MOI.NormOneCone(object["dimension"])
end

function set_to_moi(::Val{:NormInfinityCone}, object::Object)
    return MOI.NormInfinityCone(object["dimension"])
end

function set_to_moi(::Val{:NormCone}, object::Object)
    return MOI.NormCone(object["p"], object["dimension"])
end

function set_to_moi(::Val{:RelativeEntropyCone}, object::Object)
    return MOI.RelativeEntropyCone(object["dimension"])
end

function set_to_moi(::Val{:NormSpectralCone}, object::Object)
    return MOI.NormSpectralCone(object["row_dim"], object["column_dim"])
end

function set_to_moi(::Val{:NormNuclearCone}, object::Object)
    return MOI.NormNuclearCone(object["row_dim"], object["column_dim"])
end

function set_to_moi(::Val{:RootDetConeTriangle}, object::Object)
    return MOI.RootDetConeTriangle(object["side_dimension"])
end

function set_to_moi(::Val{:RootDetConeSquare}, object::Object)
    return MOI.RootDetConeSquare(object["side_dimension"])
end

function set_to_moi(::Val{:LogDetConeTriangle}, object::Object)
    return MOI.LogDetConeTriangle(object["side_dimension"])
end

function set_to_moi(::Val{:LogDetConeSquare}, object::Object)
    return MOI.LogDetConeSquare(object["side_dimension"])
end

function set_to_moi(::Val{:PositiveSemidefiniteConeTriangle}, object::Object)
    return MOI.PositiveSemidefiniteConeTriangle(object["side_dimension"])
end

function set_to_moi(
    ::Val{:ScaledPositiveSemidefiniteConeTriangle},
    object::Object,
)
    d = object["side_dimension"]
    return MOI.Scaled(MOI.PositiveSemidefiniteConeTriangle(d))
end

function set_to_moi(::Val{:Scaled}, object::Object)
    return MOI.Scaled(set_to_moi(object["set"]))
end

function set_to_moi(::Val{:PositiveSemidefiniteConeSquare}, object::Object)
    return MOI.PositiveSemidefiniteConeSquare(object["side_dimension"])
end

function set_to_moi(
    ::Val{:HermitianPositiveSemidefiniteConeTriangle},
    object::Object,
)
    side_dimension = object["side_dimension"]
    return MOI.HermitianPositiveSemidefiniteConeTriangle(side_dimension)
end

function set_to_moi(::Val{:ExponentialCone}, ::Object)
    return MOI.ExponentialCone()
end

function set_to_moi(::Val{:DualExponentialCone}, ::Object)
    return MOI.DualExponentialCone()
end

function set_to_moi(::Val{:Complements}, object::Object)
    return MOI.Complements(object["dimension"])
end

function set_to_moi(::Val{:AllDifferent}, object::Object)
    return MOI.AllDifferent(object["dimension"])
end

function set_to_moi(::Val{:Circuit}, object::Object)
    return MOI.Circuit(object["dimension"])
end

function set_to_moi(::Val{:CountAtLeast}, object::Object)
    return MOI.CountAtLeast(
        object["n"],
        convert(Vector{Int}, object["partitions"]),
        Set{Int}(object["set"]),
    )
end

function set_to_moi(::Val{:CountBelongs}, object::Object)
    return MOI.CountBelongs(object["dimension"], Set{Int}(object["set"]))
end

function set_to_moi(::Val{:CountDistinct}, object::Object)
    return MOI.CountDistinct(object["dimension"])
end

function set_to_moi(::Val{:CountGreaterThan}, object::Object)
    return MOI.CountGreaterThan(object["dimension"])
end

function set_to_moi(::Val{:Cumulative}, object::Object)
    return MOI.Cumulative(object["dimension"])
end

function set_to_moi(::Val{:Path}, object::Object)
    return MOI.Path(Int.(object["from"]), Int.(object["to"]))
end

# ========== Typed vector sets ==========

function set_to_moi(::Val{:PowerCone}, object::Object)
    return MOI.PowerCone{Float64}(object["exponent"])
end

function set_to_moi(::Val{:DualPowerCone}, object::Object)
    return MOI.DualPowerCone{Float64}(object["exponent"])
end

function set_to_moi(::Val{:SOS1}, object::Object)
    return MOI.SOS1(convert(Vector{Float64}, object["weights"]))
end

function set_to_moi(::Val{:SOS2}, object::Object)
    return MOI.SOS2(convert(Vector{Float64}, object["weights"]))
end

function set_to_moi(::Val{:HyperRectangle}, object::Object)
    return MOI.HyperRectangle(
        convert(Vector{Float64}, object["lower"]),
        convert(Vector{Float64}, object["upper"]),
    )
end

# :IndicatorSet is required for v0.6
# :Indicator is required for v1.0
function set_to_moi(::Union{Val{:Indicator},Val{:IndicatorSet}}, object::Object)
    set = set_to_moi(object["set"]::typeof(object))
    if object["activate_on"]::String == "one"
        return MOI.Indicator{MOI.ACTIVATE_ON_ONE}(set)
    else
        @assert object["activate_on"]::String == "zero"
        return MOI.Indicator{MOI.ACTIVATE_ON_ZERO}(set)
    end
end

function set_to_moi(::Val{:Reified}, object::Object)
    return MOI.Reified(set_to_moi(object["set"]::typeof(object)))
end

function set_to_moi(::Val{:BinPacking}, object::Object)
    return MOI.BinPacking(
        convert(Float64, object["capacity"]),
        convert(Vector{Float64}, object["weights"]),
    )
end

function set_to_moi(::Val{:Table}, object::Object)
    table = convert(Matrix{Float64}, vcat([t' for t in object["table"]]...))
    return MOI.Table(table)
end
