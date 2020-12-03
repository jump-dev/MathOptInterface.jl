module MPS

import ..FileFormats

import MathOptInterface
const MOI = MathOptInterface

# Julia 1.6 removes Grisu from Base. Previously, we went
#   print_shortest(io, x) = Base.Grisu.print_shortest(io, x)
# To avoid adding Grisu as a dependency, use the following printing heuristic.
# TODO(odow): consider printing 1.0 as 1.0 instead of 1, i.e., without the
# rounding branch.
function print_shortest(io::IO, x::Real)
    x_int = round(Int, x)
    if isapprox(x, x_int)
        print(io, x_int)
    else
        print(io, x)
    end
    return
end

MOI.Utilities.@model(Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (MOI.SOS1, MOI.SOS2),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}
) where {T}
    return false
end

struct Options
    warn::Bool
end

get_options(m::Model) = get(m.ext, :MPS_OPTIONS, Options(false))

"""
    Model(; kwargs...)

Create an empty instance of FileFormats.MPS.Model.

Keyword arguments are:

 - `warn::Bool=false`: print a warning when variables or constraints are renamed.
"""
function Model(;
    warn::Bool = false,
)
    model = Model{Float64}()
    model.ext[:MPS_OPTIONS] = Options(warn)
    return model
end

function Base.show(io::IO, ::Model)
    print(io, "A Mathematical Programming System (MPS) model")
    return
end

@enum(VType, VTYPE_CONTINUOUS, VTYPE_INTEGER, VTYPE_BINARY)

# The card logic is as follows: where possible, try to fit within the strict MPS
# field limits. That means fields start at columns 2, 5, 15, 25, 40, and 50.
# However, since most readers default to loose MPS, make sure each field is
# separated by at least one space.

function pad_field(field, n)
    return length(field) < n ? field : field * " "
end

struct Card
    f1::String
    f2::String
    f3::String
    f4::String
    f5::String
    f6::String
    num_fields::Int

    function Card(;
        f1::String = "",
        f2::String = "",
        f3::String = "",
        f4::String = "",
        f5::String = "",
        f6::String = "",
    )
        num_fields = isempty(f1) ? 0          : 1
        num_fields = isempty(f2) ? num_fields : 2
        num_fields = isempty(f3) ? num_fields : 3
        num_fields = isempty(f4) ? num_fields : 4
        num_fields = isempty(f5) ? num_fields : 5
        num_fields = isempty(f6) ? num_fields : 6
        return new(
            pad_field(f1, 3),
            pad_field(f2, 10),
            pad_field(f3, 10),
            pad_field(f4, 15),
            pad_field(f5, 10),
            pad_field(f6, Inf),
            num_fields,
        )
    end
end

function Base.println(io::IO, card::Card)
    if card.num_fields == 0
        println(io)
        return
    elseif card.num_fields == 1
        println(io, " ", card.f1)
        return
    end
    print(io, " ", rpad(card.f1, 3))
    if card.num_fields == 2
        println(io, card.f2)
        return
    end
    print(io, rpad(card.f2, 10))
    if card.num_fields == 3
        println(io, card.f3)
        return
    end
    print(io, rpad(card.f3, 10))
    if card.num_fields == 4
        println(io, card.f4)
        return
    end
    print(io, rpad(card.f4, 15))
    if card.num_fields == 5
        println(io, card.f5)
        return
    end
    print(io, rpad(card.f5, 10))
    if card.num_fields == 6
        print(io, card.f6)
    end
    return
end

# ==============================================================================
#
#   Base.write
#
# ==============================================================================

"""
    Base.write(io::IO, model::FileFormats.MPS.Model)

Write `model` to `io` in the MPS file format.
"""
function Base.write(io::IO, model::Model)
    options = get_options(model)
    FileFormats.create_unique_names(
        model;
        warn = options.warn,
        replacements = Function[s -> replace(s, ' ' => '_')]
    )
    ordered_names = String[]
    names = Dict{MOI.VariableIndex, String}()
    for x in MOI.get(model, MOI.ListOfVariableIndices())
        n = MOI.get(model, MOI.VariableName(), x)
        push!(ordered_names, n)
        names[x] = n
    end
    write_model_name(io, model)
    write_rows(io, model)
    discovered_columns = write_columns(io, model, ordered_names, names)
    write_rhs(io, model)
    write_ranges(io, model)
    write_bounds(io, model, discovered_columns, ordered_names, names)
    write_sos(io, model, names)
    println(io, "ENDATA")
    return
end

# ==============================================================================
#   Model name
# ==============================================================================

function write_model_name(io::IO, model::Model)
    model_name = MOI.get(model, MOI.Name())
    println(io, rpad("NAME", 14), model_name)
    return
end

# ==============================================================================
#   ROWS
# ==============================================================================

const SET_TYPES = (
    (MOI.LessThan{Float64}, "L"),
    (MOI.GreaterThan{Float64}, "G"),
    (MOI.EqualTo{Float64}, "E"),
    (MOI.Interval{Float64}, "L"),  # See the note in the RANGES section.
)

function _write_rows(io, model, S, sense_char)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, S}()
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        if row_name == ""
            error("Row name is empty: $(index).")
        end
        println(io, Card(f1 = sense_char, f2 = row_name))
    end
    return
end

function write_rows(io::IO, model::Model)
    println(io, "ROWS")
    println(io, Card(f1 = "N", f2 = "OBJ"))
    for (set_type, sense_char) in SET_TYPES
        _write_rows(io, model, set_type, sense_char)
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function _list_of_integer_variables(model, names, integer_variables, S)
    for index in MOI.get(
        model, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}()
    )
        v_index = MOI.get(model, MOI.ConstraintFunction(), index)
        push!(integer_variables, names[v_index.variable])
    end
    return
end

function list_of_integer_variables(model::Model, names)
    integer_variables = Set{String}()
    for S in (MOI.ZeroOne, MOI.Integer)
        _list_of_integer_variables(model, names, integer_variables, S)
    end
    return integer_variables
end

function extract_terms(
    v_names::Dict{MOI.VariableIndex, String},
    coefficients::Dict{String, Vector{Tuple{String, Float64}}},
    row_name::String,
    func::MOI.ScalarAffineFunction,
    discovered_columns::Set{String},
    multiplier::Float64 = 1.0,
)
    for term in func.terms
        variable_name = v_names[term.variable_index]
        push!(
            coefficients[variable_name],
            (row_name, multiplier * term.coefficient)
        )
        push!(discovered_columns, variable_name)
    end
    return
end

function _collect_coefficients(
    model,
    S,
    v_names::Dict{MOI.VariableIndex, String},
    coefficients::Dict{String, Vector{Tuple{String, Float64}}},
    discovered_columns::Set{String},
)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, S}(),
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        extract_terms(
            v_names, coefficients, row_name, func, discovered_columns
        )
    end
    return
end

function write_columns(io::IO, model::Model, ordered_names, names)
    coefficients = Dict{String, Vector{Tuple{String, Float64}}}(
        n => Tuple{String, Float64}[] for n in ordered_names
    )
    # Many MPS readers (e.g., CPLEX and GAMS) will error if a variable (column)
    # appears in the BOUNDS section but did not appear in the COLUMNS section.
    # This is likely because such variables are meaningless - they don't appear
    # in the objective or constraints and so can be trivially removed.
    # To avoid generating MPS files that crash existing readers, we cache the
    # names of all variables seen in COLUMNS into `discovered_columns`, and then
    # pass this set to `write_bounds` so that it can act appropriately.
    discovered_columns = Set{String}()
    # Build constraint coefficients
    for (S, _) in SET_TYPES
        _collect_coefficients(model, S, names, coefficients, discovered_columns)
    end
    # Build objective
    # MPS doesn't support maximization so we flip the sign on the objective
    # coefficients.
    s = MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE ? -1.0 : 1.0
    obj_func = MOI.get(
        model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}()
    )
    extract_terms(names, coefficients, "OBJ", obj_func, discovered_columns, s)
    integer_variables = list_of_integer_variables(model, names)
    println(io, "COLUMNS")
    int_open = false
    for variable in ordered_names
        is_int = variable in integer_variables
        if is_int && !int_open
            println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTORG'"))
            int_open = true
        elseif !is_int && int_open
            println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTEND'"))
            int_open = false
        end
        for (constraint, coefficient) in coefficients[variable]
            println(
                io,
                Card(
                    f2 = variable,
                    f3 = constraint,
                    f4 = sprint(print_shortest, coefficient),
                )
            )
        end
    end
    return discovered_columns
end

# ==============================================================================
#   RHS
# ==============================================================================

value(set::MOI.LessThan) = set.upper
value(set::MOI.GreaterThan) = set.lower
value(set::MOI.EqualTo) = set.value
value(set::MOI.Interval) = set.upper  # See the note in the RANGES section.

function _write_rhs(io, model, S, sense_char)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, S}()
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        println(
            io,
            Card(
                f2 = "rhs",
                f3 = row_name,
                f4 = sprint(print_shortest, value(set)),
            )
        )
    end
end

function write_rhs(io::IO, model::Model)
    println(io, "RHS")
    for (set_type, sense_char) in SET_TYPES
        _write_rhs(io, model, set_type, sense_char)
    end
    return
end

# ==============================================================================
#  RANGES
#
# Here is how RANGE information is encoded.
#
#     Row type | Range value |  lower bound  |  upper bound
#     ------------------------------------------------------
#         G    |     +/-     |     rhs       | rhs + |range|
#         L    |     +/-     | rhs - |range| |     rhs
#         E    |      +      |     rhs       | rhs + range
#         E    |      -      | rhs + range   |     rhs
#
# We elect to write out ScalarAffineFunction-in-Interval constraints in terms of
# LessThan (L) constraints with a range shift. The RHS term is set to the upper
# bound, and the RANGE term to upper - lower.
# ==============================================================================

function write_ranges(io::IO, model::Model)
    println(io, "RANGES")
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}
        }()
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)::MOI.Interval{Float64}
        println(
            io,
            Card(
                f2 = "rhs",
                f3 = row_name,
                f4 = sprint(print_shortest, set.upper - set.lower),
            )
        )
    end
    return
end

# ==============================================================================
#   BOUNDS
# Variables default to [0, ∞).
#     FR    free variable      -∞ < x < ∞
#     FX    fixed variable     x == b
#     MI    lower bound -inf   -∞ < x
#     UP    upper bound             x <= b
#     PL    upper bound +inf        x < ∞
#     LO    lower bound        b <= x
#     BV    binary variable    x = 0 or 1
#
#  Not yet implemented:
#     LI    integer variable   b <= x
#     UI    integer variable        x <= b
#     SC    semi-cont variable x = 0 or l <= x <= b
#           l is the lower bound on the variable. If none set then defaults to 1
# ==============================================================================

function write_single_bound(io::IO, var_name::String, lower, upper)
    if lower == upper
        println(
            io,
            Card(
                f1 = "FX",
                f2 = "bounds",
                f3 = var_name,
                f4 = sprint(print_shortest, lower),
            )
        )
    elseif lower == -Inf && upper == Inf
        println(
            io,
            Card(
                f1 = "FR",
                f2 = "bounds",
                f3 = var_name,
            )
        )
    else
        if lower == -Inf
            println(
                io,
                Card(
                    f1 = "MI",
                    f2 = "bounds",
                    f3 = var_name,
                )
            )
        else
            println(
                io,
                Card(
                    f1 = "LO",
                    f2 = "bounds",
                    f3 = var_name,
                    f4 = sprint(print_shortest, lower),
                )
            )
        end
        if upper == Inf
            println(
                io,
                Card(
                    f1 = "PL",
                    f2 = "bounds",
                    f3 = var_name,
                )
            )
        else
            println(
                io,
                Card(
                    f1 = "UP",
                    f2 = "bounds",
                    f3 = var_name,
                    f4 = sprint(print_shortest, upper),
                )
            )
        end
    end
    return
end

function update_bounds(x::Tuple{Float64, Float64, VType}, set::MOI.GreaterThan)
    return (max(x[1], set.lower), x[2], x[3])
end

function update_bounds(x::Tuple{Float64, Float64, VType}, set::MOI.LessThan)
    return (x[1], min(x[2], set.upper), x[3])
end

function update_bounds(x::Tuple{Float64, Float64, VType}, set::MOI.Interval)
    return (set.lower, set.upper, x[3])
end

function update_bounds(x::Tuple{Float64, Float64, VType}, set::MOI.EqualTo)
    return (set.value, set.value, x[3])
end

function update_bounds(x::Tuple{Float64, Float64, VType}, set::MOI.ZeroOne)
    return (x[1], x[2], VTYPE_BINARY)
end

function _collect_bounds(bounds, model, S, names)
    for index in MOI.get(
        model, MOI.ListOfConstraintIndices{MOI.SingleVariable, S}()
    )
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)::S
        name = names[func.variable]
        bounds[name] = update_bounds(bounds[name], set)
    end
    return
end

function write_bounds(
    io::IO, model::Model, discovered_columns::Set{String}, ordered_names, names
)
    println(io, "BOUNDS")
    bounds = Dict{String, Tuple{Float64, Float64, VType}}(
        n => (-Inf, Inf, VTYPE_CONTINUOUS) for n in ordered_names
    )
    for S in (
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
        MOI.ZeroOne,
    )
        _collect_bounds(bounds, model, S, names)
    end
    for var_name in ordered_names
        lower, upper, vtype = bounds[var_name]
        if !(var_name in discovered_columns)
            @warn(
                "Variable $var_name is mentioned in BOUNDS, but is not " *
                "mentioned in the COLUMNS section. We are ignoring it."
            )
            continue
        elseif vtype == VTYPE_BINARY
            println(
                io,
                Card(
                    f1 = "BV",
                    f2 = "bounds",
                    f3 = var_name,
                )
            )
            # Only add bounds if they are tighter than the implicit bounds of a
            # binary variable.
            if lower > 0 || upper < 1
                write_single_bound(io, var_name, lower, upper)
            end
        else
            write_single_bound(io, var_name, lower, upper)
        end
    end
    return
end

# ==============================================================================
#   SOS
# ==============================================================================

function write_sos_constraint(io::IO, model::Model, index, names)
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    for (variable, weight) in zip(func.variables, set.weights)
        println(
            io,
            Card(
                f2 = names[variable],
                f3 = sprint(print_shortest, weight),
            )
        )
    end
end

function write_sos(io::IO, model::Model, names)
    sos1_indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables, MOI.SOS1{Float64}}(),
    )
    sos2_indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables, MOI.SOS2{Float64}}(),
    )
    if length(sos1_indices) + length(sos2_indices) > 0
        println(io, "SOS")
        idx = 1
        for (sos_type, indices) in enumerate([sos1_indices, sos2_indices])
            for index in indices
                println(
                    io,
                    Card(
                        f1 = "S$(sos_type)",
                        f2 = "SOS$(idx)",
                    )
                )
                write_sos_constraint(io, model, index, names)
                idx += 1
            end
        end
    end
    return
end

# ==============================================================================
#
#   Base.read!
#
# Here is a template for an MPS file, reproduced from
# http://lpsolve.sourceforge.net/5.5/mps-format.htm.
#
# Field:    1           2          3         4         5         6
# Columns:  2-3        5-12      15-22     25-36     40-47     50-61
#           NAME   problem name
#           ROWS
#            type     name
#           COLUMNS
#                     column      row       value     row      value
#                     name        name                name
#           RHS
#                     rhs         row       value     row      value
#                     name        name                name
#           RANGES
#                     range       row       value     row      value
#                     name        name                name
#           BOUNDS
#            type     bound       column    value
#                     name        name
#           SOS
#            type     CaseName    SOSName   SOSpriority
#                     CaseName    VarName1  VarWeight1
#                     CaseName    VarName2  VarWeight2
#                     CaseName    VarNameN  VarWeightN
#           ENDATA
# ==============================================================================

@enum(Sense, SENSE_N, SENSE_G, SENSE_L, SENSE_E, SENSE_UNKNOWN)
function Sense(s::String)
    if s == "G"
        return SENSE_G
    elseif s == "L"
        return SENSE_L
    elseif s == "E"
        return SENSE_E
    elseif s == "N"
        return SENSE_N
    else
        return SENSE_UNKNOWN
    end
end
mutable struct TempMPSModel
    name::String
    obj_name::String
    c::Vector{Float64}
    col_lower::Vector{Float64}
    col_upper::Vector{Float64}
    row_lower::Vector{Float64}
    row_upper::Vector{Float64}
    sense::Vector{Sense}
    A::Vector{Vector{Tuple{Int, Float64}}}
    vtype::Vector{VType}
    name_to_col::Dict{String, Int}
    col_to_name::Vector{String}
    name_to_row::Dict{String, Int}
    row_to_name::Vector{String}
    intorg_flag::Bool  # A flag used to parse COLUMNS section.
end

function TempMPSModel()
    return TempMPSModel(
        "",
        "",
        Float64[],  # c
        Float64[],  # col_lower
        Float64[],  # col_upper
        Float64[],  # row_lower
        Float64[],  # row_upper
        Sense[],   # sense
        Vector{Vector{Tuple{Int, Float64}}}[],  # A
        Bool[],
        Dict{String, Int}(),
        String[],
        Dict{String, Int}(),
        String[],
        false,
    )
end

@enum(
    Headers,
    HEADER_NAME,
    HEADER_ROWS,
    HEADER_COLUMNS,
    HEADER_RHS,
    HEADER_RANGES,
    HEADER_BOUNDS,
    HEADER_SOS,
    HEADER_ENDATA,
    HEADER_UNKNOWN,
)

# Headers(s) gets called _alot_ (on every line), so we try very hard to be
# efficient.]
function Headers(s::AbstractString)
    N = length(s)
    if N > 7 || N < 3
        return HEADER_UNKNOWN
    elseif N == 3
        x = first(s)
        if (x == 'R' || x == 'r') && uppercase(s) == "RHS"
            return HEADER_RHS
        elseif (x == 'S' || x == 's') && uppercase(s) == "SOS"
            return HEADER_SOS
        end
    elseif N == 4
        x = first(s)
        if (x == 'R' || x == 'r') && uppercase(s) == "ROWS"
            return HEADER_ROWS
        end
    elseif N == 5
        return HEADER_UNKNOWN
    elseif N == 6
        x = first(s)
        if (x == 'R' || x == 'r') && uppercase(s) == "RANGES"
            return HEADER_RANGES
        elseif (x == 'B' || x == 'b') && uppercase(s) == "BOUNDS"
            return HEADER_BOUNDS
        elseif (x == 'E' || x == 'e') && uppercase(s) == "ENDATA"
            return HEADER_ENDATA
        end
    elseif N == 7
        x = first(s)
        if (x == 'C' || x == 'c') && (uppercase(s) == "COLUMNS")
            return HEADER_COLUMNS
        end
    end
    return HEADER_UNKNOWN
end

function line_to_items(line)
    items = split(line, " "; keepempty = false)
    return String.(items)
end

"""
    Base.read!(io::IO, model::FileFormats.MPS.Model)

Read `io` in the MPS file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    data = TempMPSModel()
    header = HEADER_NAME
    multi_objectives = String[]
    while !eof(io)
        line = string(strip(readline(io)))
        if isempty(line) || first(line) == '*'
            continue  # Skip blank lines and comments.
        end
        h = Headers(line)
        if h != HEADER_UNKNOWN
            header = h
            continue
        else
            # Carry on with the previous header
        end
        # TODO: split into hard fields based on column indices.
        items = line_to_items(line)
        if header == HEADER_NAME
            parse_name_line(data, items)
        elseif header == HEADER_ROWS
            multi_obj = parse_rows_line(data, items)
            if multi_obj !== nothing
                push!(multi_objectives, multi_obj)
            end
        elseif header == HEADER_COLUMNS
            parse_columns_line(data, items, multi_objectives)
        elseif header == HEADER_RHS
            parse_rhs_line(data, items)
        elseif header == HEADER_RANGES
            parse_ranges_line(data, items)
        elseif header == HEADER_BOUNDS
            parse_bounds_line(data, items)
        elseif header == HEADER_SOS
            error("TODO(odow): implement parsing of SOS constraints.")
        else
            @assert header == HEADER_ENDATA
            break
        end
    end
    copy_to(model, data)
    return
end

function bounds_to_set(lower, upper)
    if -Inf < lower < upper < Inf
        return MOI.Interval(lower, upper)
    elseif -Inf < lower && upper == Inf
        return MOI.GreaterThan(lower)
    elseif -Inf == lower && upper < Inf
        return MOI.LessThan(upper)
    elseif lower == upper
        return MOI.EqualTo(upper)
    end
    return  # free variable
end

function copy_to(model::Model, data::TempMPSModel)
    MOI.set(model, MOI.Name(), data.name)
    variable_map = Dict{String, MOI.VariableIndex}()
    for (i, name) in enumerate(data.col_to_name)
        _add_variable(model, data, variable_map, i, name)
    end
    _add_objective(model, data, variable_map)
    for (j, c_name) in enumerate(data.row_to_name)
        set = bounds_to_set(data.row_lower[j], data.row_upper[j])
        if set !== nothing
            _add_linear_constraint(model, data, variable_map, j, c_name, set)
        end
        # `else` is a free constraint. Don't add it.
    end
    return
end

function _add_variable(model, data, variable_map, i, name)
    x = MOI.add_variable(model)
    variable_map[name] = x
    MOI.set(model, MOI.VariableName(), x, name)
    set = bounds_to_set(data.col_lower[i], data.col_upper[i])
    if set !== nothing
        MOI.add_constraint(model, MOI.SingleVariable(x), set)
    end
    if data.vtype[i] == VTYPE_INTEGER
        MOI.add_constraint(model, MOI.SingleVariable(x), MOI.Integer())
    elseif data.vtype[i] == VTYPE_BINARY
        MOI.add_constraint(model, MOI.SingleVariable(x), MOI.ZeroOne())
    end
    return
end

function _add_objective(model, data, variable_map)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(
            [
                MOI.ScalarAffineTerm(data.c[i], variable_map[v])
                for (i, v) in enumerate(data.col_to_name) if !iszero(data.c[i])
            ],
            0.0,
        )
    )
    return
end

function _add_linear_constraint(model, data, variable_map, j, c_name, set)
    terms = MOI.ScalarAffineTerm{Float64}[
        MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]])
        for (i, coef) in data.A[j]
    ]
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(terms, 0.0), set)
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

# ==============================================================================
#   NAME
# ==============================================================================

function parse_name_line(data::TempMPSModel, items::Vector{String})
    if !(1 <= length(items) <= 2) || uppercase(items[1]) != "NAME"
        error("Malformed NAME line: $(join(items, " "))")
    end
    if length(items) == 2
        data.name = items[2]
    elseif length(items) == 1
        data.name = ""
    end
    return
end

# ==============================================================================
#   ROWS
# ==============================================================================

function parse_rows_line(data::TempMPSModel, items::Vector{String})
    if length(items) != 2
        error("Malformed ROWS line: $(join(items, " "))")
    end
    sense, name = Sense(items[1]), items[2]
    if haskey(data.name_to_row, name)
        error("Duplicate row encountered: $(join(items, " ")).")
    elseif sense == SENSE_UNKNOWN
        error("Invalid row sense: $(join(items, " "))")
    end
    if sense == SENSE_N
        if data.obj_name != ""
            return name  # Detected a duplicate objective. Skip it.
        end
        data.obj_name = name
        return
    end
    if name == data.obj_name
        error("Found row with same name as objective: $(join(items, " ")).")
    end
    # Add some default bounds for the constraints.
    push!(data.row_to_name, name)
    row = length(data.row_to_name)
    data.name_to_row[name] = row
    push!(data.sense, sense)
    push!(data.A, Tuple{Int, Float64}[])
    if sense == SENSE_G
        push!(data.row_lower, 0.0)
        push!(data.row_upper, Inf)
        data.row_upper[row] = Inf
    elseif sense == SENSE_L
        push!(data.row_lower, -Inf)
        push!(data.row_upper, 0.0)
    else
        @assert sense == SENSE_E
        push!(data.row_lower, 0.0)
        push!(data.row_upper, 0.0)
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function parse_single_coefficient(
    data, row_name::String, column::Int, value::Float64
)
    if row_name == data.obj_name
        data.c[column] += value
        return
    end
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    push!(data.A[row], (column, value))
    return
end

function _add_new_column(data, column_name)
    if haskey(data.name_to_col, column_name)
        return
    end
    push!(data.col_to_name, column_name)
    data.name_to_col[column_name] = length(data.col_to_name)
    push!(data.c, 0.0)
    push!(data.col_lower, 0.0)
    push!(data.col_upper, Inf)
    push!(data.vtype, VTYPE_CONTINUOUS)
    return
end

function _set_intorg(data, column, column_name)
     if data.intorg_flag
        data.vtype[column] = VTYPE_INTEGER
    elseif data.vtype[column] != VTYPE_CONTINUOUS
        error(
            "Variable $(column_name) appeared in COLUMNS outside an " *
            "`INT` marker after already being declared as integer."
        )
    end
    return
end

function parse_columns_line(
    data::TempMPSModel, items::Vector{String}, multi_objectives::Vector{String}
)
    if length(items) == 3
        # [column name] [row name] [value]
        column_name, row_name, value = items
        if row_name == "'MARKER'"
            if value == "'INTORG'"
                data.intorg_flag = true
                return
            elseif value == "'INTEND'"
                data.intorg_flag = false
                return
            end
        elseif row_name in multi_objectives
            return
        end
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(data, row_name, column, parse(Float64, value))
        _set_intorg(data, column, column_name)
    elseif length(items) == 5
        # [column name] [row name] [value] [row name 2] [value 2]
        column_name, row_name_1, value_1, row_name_2, value_2 = items
        if row_name_1 in multi_objectives || row_name_2 in multi_objectives
            return
        end
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(
            data, row_name_1, column, parse(Float64, value_1)
        )
        parse_single_coefficient(
            data, row_name_2, column, parse(Float64, value_2)
        )
        _set_intorg(data, column, column_name)
    else
        error("Malformed COLUMNS line: $(join(items, " "))")
    end
    return
end

# ==============================================================================
#   RHS
# ==============================================================================

function parse_single_rhs(
    data, row_name::String, value::Float64, items::Vector{String}
)
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    if data.sense[row] == SENSE_E
        data.row_upper[row] = value
        data.row_lower[row] = value
    elseif data.sense[row] == SENSE_G
        data.row_lower[row] = value
    elseif data.sense[row] == SENSE_L
        data.row_upper[row] = value
    else
        @assert data.sense[row] == SENSE_N
        error("Cannot have RHS for objective: $(join(items, " "))")
    end
    return
end

# TODO: handle multiple RHS vectors.
function parse_rhs_line(data::TempMPSModel, items::Vector{String})
    if length(items) == 3
        # [rhs name] [row name] [value]
        rhs_name, row_name, value = items
        parse_single_rhs(data, row_name, parse(Float64, value), items)
    elseif length(items) == 5
        # [rhs name] [row name 1] [value 1] [row name 2] [value 2]
        rhs_name, row_name_1, value_1, row_name_2, value_2 = items
        parse_single_rhs(data, row_name_1, parse(Float64, value_1), items)
        parse_single_rhs(data, row_name_2, parse(Float64, value_2), items)
    else
        error("Malformed RHS line: $(join(items, " "))")
    end
    return
end

# ==============================================================================
#  RANGES
#
# Here is how RANGE information is encoded. (We repeat this comment because it
# is so non-trivial.)
#
#     Row type | Range value |  lower bound  |  upper bound
#     ------------------------------------------------------
#         G    |     +/-     |     rhs       | rhs + |range|
#         L    |     +/-     | rhs - |range| |     rhs
#         E    |      +      |     rhs       | rhs + range
#         E    |      -      | rhs + range   |     rhs
# ==============================================================================

function parse_single_range(data, row_name::String, value::Float64)
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    if data.sense[row] == SENSE_G
        data.row_upper[row] = data.row_lower[row] + abs(value)
    elseif data.sense[row] == SENSE_L
        data.row_lower[row] = data.row_upper[row] - abs(value)
    elseif data.sense[row] == SENSE_E
        if value > 0.0
            data.row_upper[row] = data.row_lower[row] + value
        else
            data.row_lower[row] = data.row_upper[row] + value
        end
    end
    return
end

# TODO: handle multiple RANGES vectors.
function parse_ranges_line(data::TempMPSModel, items::Vector{String})
    if length(items) == 3
        # [rhs name] [row name] [value]
        _, row_name, value = items
        parse_single_range(data, row_name, parse(Float64, value))
    elseif length(items) == 5
        # [rhs name] [row name] [value] [row name 2] [value 2]
        _, row_name_1, value_1, row_name_2, value_2 = items
        parse_single_range(data, row_name_1, parse(Float64, value_1))
        parse_single_range(data, row_name_2, parse(Float64, value_2))
    else
        error("Malformed RANGES line: $(join(items, " "))")
    end
    return
end

# ==============================================================================
#   BOUNDS
# ==============================================================================

function _parse_single_bound(data, column_name::String, bound_type::String)
    col = get(data.name_to_col, column_name, nothing)
    if col === nothing
        error("Column name $(column_name) not found.")
    end
    if bound_type == "PL"
        data.col_upper[col] = Inf
    elseif bound_type == "MI"
        data.col_lower[col] = -Inf
    elseif bound_type == "FR"
        data.col_lower[col] = -Inf
        data.col_upper[col] = Inf
    elseif bound_type == "BV"
        data.col_lower[col] = -Inf
        data.col_upper[col] = Inf
        data.vtype[col] = VTYPE_BINARY
    else
        error("Invalid bound type $(bound_type): $(column_name)")
    end
end

function _parse_single_bound(
    data, column_name::String, bound_type::String, value::Float64
)
    col = get(data.name_to_col, column_name, nothing)
    if col === nothing
        error("Column name $(column_name) not found.")
    end
    if bound_type == "FX"
        data.col_lower[col] = value
        data.col_upper[col] = value
    elseif bound_type == "UP"
        data.col_upper[col] = value
    elseif bound_type == "LO"
        data.col_lower[col] = value
    elseif bound_type == "LI"
        data.col_lower[col] = value
        data.vtype[col] = VTYPE_INTEGER
    elseif bound_type == "UI"
        data.col_upper[col] = value
        data.vtype[col] = VTYPE_INTEGER
    elseif bound_type == "FR"
        # So even though FR bounds should be of the form:
        #  FR BOUND1    VARNAME
        # there are cases in MIPLIB2017 (e.g., leo1 and leo2) like so:
        #  FR BOUND1    C0000001       .000000
        # In these situations, just ignore the value.
        data.col_lower[col] = -Inf
        data.col_upper[col] = Inf
    elseif bound_type == "BV"
        # A similar situation happens with BV bounds in leo1 and leo2.
        data.col_lower[col] = -Inf
        data.col_upper[col] = Inf
        data.vtype[col] = VTYPE_BINARY
    else
        error("Invalid bound type $(bound_type): $(column_name)")
    end
end

function parse_bounds_line(data::TempMPSModel, items::Vector{String})
    if length(items) == 3
        bound_type, _, column_name = items
        _parse_single_bound(data, column_name, bound_type)
    elseif length(items) == 4
        bound_type, _, column_name, value = items
        _parse_single_bound(data, column_name, bound_type, parse(Float64, value))
    else
        error("Malformed BOUNDS line: $(join(items, " "))")
    end
    return
end

end
