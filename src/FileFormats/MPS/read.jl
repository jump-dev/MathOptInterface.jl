# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# ==============================================================================
#
#   `Base.read!`
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
#           RHS
#                     rhs         row       value     row      value
#           RANGES
#                     range       row       value     row      value
#           BOUNDS
#            type     bound       column    value
#           SOS
#            type     CaseName    SOSName   SOSpriority
#                     CaseName    VarName1  VarWeight1
#                     CaseName    VarName2  VarWeight2
#                     CaseName    VarNameN  VarWeightN
#           ENDATA
# ==============================================================================

@enum(Sense, SENSE_N, SENSE_G, SENSE_L, SENSE_E, SENSE_UNKNOWN)

function Sense(s)
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

struct _SOSConstraint{T}
    type::Int
    weights::Vector{T}
    columns::Vector{String}
end

mutable struct TempMPSModel{T}
    lines::Int
    contents::String
    name::String
    is_minimization::Bool
    obj_name::String
    c::Vector{T}
    obj_constant::T
    col_lower::Vector{T}
    col_upper::Vector{T}
    col_bounds_default::Vector{Bool}
    row_lower::Vector{T}
    row_upper::Vector{T}
    sense::Vector{Sense}
    A::Vector{Vector{Tuple{Int,T}}}
    vtype::Vector{VType}
    name_to_col::Dict{String,Int}
    col_to_name::Vector{String}
    name_to_row::Dict{String,Int}
    row_to_name::Vector{String}
    intorg_flag::Bool  # A flag used to parse COLUMNS section.
    sos_constraints::Vector{_SOSConstraint{T}}
    quad_obj::Vector{Tuple{String,String,T}}
    qc_matrix::Dict{String,Vector{Tuple{String,String,T}}}
    current_qc_matrix::String
    indicators::Dict{String,Tuple{String,MOI.ActivationCondition}}

    function TempMPSModel{T}() where {T}
        return new{T}(
            0,        # line
            "",       # contents
            "",       # name
            true,     # is_minimization
            "",       # obj_name
            T[],      # c
            zero(T),  # obj_constant
            T[],      # col_lower
            T[],      # col_upper
            Bool[],   # col_bounds_default
            T[],      # row_lower
            T[],      # row_upper
            Sense[],  # sense
            Vector{Tuple{Int,T}}[],  # A
            VType[],
            Dict{String,Int}(),
            String[],
            Dict{String,Int}(),
            String[],
            false,
            _SOSConstraint{T}[],
            Tuple{String,String,T}[],
            Dict{String,Vector{Tuple{String,String,T}}}(),
            "",
            Dict{String,Tuple{String,MOI.ActivationCondition}}(),
        )
    end
end

"""
    struct ParseError <: Exception
        line::Int
        msg::String
    end

This error is thrown when we encounter an error parsing the MPS file.
"""
struct ParseError <: Exception
    line::Int
    msg::String
end

function _throw_parse_error(data, msg)
    counters = "1234567890"
    counter_line = counters ^ ceil(Int, length(data.contents) / 10)
    counter_line = counter_line[1:length(data.contents)]
    msg = string(data.contents, "\n", counter_line, "\n\n", msg)
    return throw(ParseError(data.lines, msg))
end

function Base.showerror(io::IO, err::ParseError)
    return print(io, "Error parsing MPS file on line $(err.line):\n\n", err.msg)
end

@enum(
    Headers,
    HEADER_NAME,
    HEADER_OBJSENSE,
    HEADER_ROWS,
    HEADER_COLUMNS,
    HEADER_RHS,
    HEADER_RANGES,
    HEADER_BOUNDS,
    HEADER_SOS,
    HEADER_ENDATA,
    HEADER_UNKNOWN,
    HEADER_QUADOBJ,
    HEADER_QMATRIX,
    HEADER_QCMATRIX,
    HEADER_QSECTION,
    HEADER_INDICATORS,
)

# `Headers` gets called _alot_ (on every line), so we try very hard to be
# efficient.
function Headers(s)
    N = length(s)
    x = first(s)
    if N == 3
        if (x == 'R' || x == 'r') && uppercase(s) == "RHS"
            return HEADER_RHS
        elseif (x == 'S' || x == 's') && uppercase(s) == "SOS"
            return HEADER_SOS
        end
    elseif N == 4
        if (x == 'R' || x == 'r') && uppercase(s) == "ROWS"
            return HEADER_ROWS
        end
    elseif N == 6
        if (x == 'R' || x == 'r') && uppercase(s) == "RANGES"
            return HEADER_RANGES
        elseif (x == 'B' || x == 'b') && uppercase(s) == "BOUNDS"
            return HEADER_BOUNDS
        elseif (x == 'E' || x == 'e') && uppercase(s) == "ENDATA"
            return HEADER_ENDATA
        end
    elseif N == 7
        if (x == 'C' || x == 'c') && (uppercase(s) == "COLUMNS")
            return HEADER_COLUMNS
        elseif (x == 'Q' || x == 'q')
            header = uppercase(s)
            if header == "QUADOBJ"
                return HEADER_QUADOBJ
            elseif header == "QMATRIX"
                return HEADER_QMATRIX
            end
        end
    elseif N >= 8
        if (x == 'O' || x == 'o') && startswith(uppercase(s), "OBJSENSE")
            return HEADER_OBJSENSE
        elseif (x == 'Q' || x == 'q')
            header = uppercase(s)
            if startswith(header, "QCMATRIX")
                return HEADER_QCMATRIX
            elseif startswith(header, "QSECTION")
                return HEADER_QSECTION
            end
        elseif N == 10
            if (x == 'I' || x == 'i') && uppercase(s) == "INDICATORS"
                return HEADER_INDICATORS
            end
        end
    end
    return HEADER_UNKNOWN
end

function line_to_items(line)
    # Split on any whitespace characters. We can't split only on `' '` because
    # at least one models in MIPLIB has `\t` as a separator.
    #
    # This decision assumes that we are parsing a free MPS file, where
    # whitespace is disallowed in names. If this ever becomes a problem, we
    # could change to the fixed MPS format, where the files are split at the
    # usual offsets.
    return split(line, r"\s"; keepempty = false)
end

"""
    Base.read!(io::IO, model::FileFormats.MPS.Model)

Read `io` in the MPS file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model{T}) where {T}
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    data = TempMPSModel{T}()
    header = HEADER_NAME
    while !eof(io) && header != HEADER_ENDATA
        data.contents = readline(io)
        data.lines += 1
        if startswith(data.contents, '*')
            continue  # Lines starting with `*` are comments
        end
        line = string(strip(data.contents))
        if isempty(line)
            continue  # Skip blank lines
        end
        h = Headers(line)
        if h == HEADER_OBJSENSE
            items = line_to_items(line)
            if length(items) == 2
                sense = uppercase(items[2])
                if !(sense in ("MIN", "MAX"))
                    _throw_parse_error(
                        data,
                        "The objective sense must be MIN or MAX.",
                    )
                end
                data.is_minimization = sense == "MIN"
            else
                header = HEADER_OBJSENSE
            end
            continue
        elseif h == HEADER_QCMATRIX || h == HEADER_QSECTION
            items = line_to_items(line)
            if length(items) != 2
                _throw_parse_error(
                    data,
                    "The header for a quadratic matrix must have two fields, where the second field is the name of the Q matrix.",
                )
            end
            data.current_qc_matrix = String(items[2])
            header = h
            data.qc_matrix[data.current_qc_matrix] = Tuple{String,String,T}[]
            continue
        elseif h != HEADER_UNKNOWN
            header = h
            continue
        end
        # Otherwise, carry on with the previous header
        # TODO: split into hard fields based on column indices.
        items = line_to_items(line)
        if header == HEADER_NAME
            parse_name_line(data, line)
        elseif header == HEADER_OBJSENSE
            sense = uppercase(only(items))
            if !(sense in ("MIN", "MAX"))
                _throw_parse_error(
                    data,
                    "The objective sense must be MIN or MAX.",
                )
            end
            data.is_minimization = sense == "MIN"
        elseif header == HEADER_ROWS
            parse_rows_line(data, items)
        elseif header == HEADER_COLUMNS
            parse_columns_line(data, items)
        elseif header == HEADER_RHS
            parse_rhs_line(data, items)
        elseif header == HEADER_RANGES
            parse_ranges_line(data, items)
        elseif header == HEADER_BOUNDS
            parse_bounds_line(data, items)
        elseif header == HEADER_SOS
            parse_sos_line(data, items)
        elseif header == HEADER_QUADOBJ
            parse_quadobj_line(data, items)
        elseif header == HEADER_QMATRIX
            parse_qmatrix_line(data, items)
        elseif header == HEADER_QCMATRIX
            parse_qcmatrix_line(data, items)
        elseif header == HEADER_QSECTION
            parse_qsection_line(data, items)
        elseif header == HEADER_INDICATORS
            parse_indicators_line(data, items)
        else
            # This really is an assert, not an opportunity for a ParseError.
            @assert header == HEADER_ENDATA
        end
    end
    copy_to(model, data)
    return
end

function bounds_to_set(lower::T, upper::T) where {T}
    if typemin(T) < lower < upper < typemax(T)
        return MOI.Interval(lower, upper)
    elseif typemin(T) < lower && upper == typemax(T)
        return MOI.GreaterThan(lower)
    elseif typemin(T) == lower && upper < typemax(T)
        return MOI.LessThan(upper)
    elseif lower == upper
        return MOI.EqualTo(upper)
    end
    return  # free variable
end

function copy_to(model::Model, data::TempMPSModel{T}) where {T}
    MOI.set(model, MOI.Name(), data.name)
    variable_map = Dict{String,MOI.VariableIndex}()
    for (i, name) in enumerate(data.col_to_name)
        _add_variable(model, data, variable_map, i, name)
    end
    _add_objective(model, data, variable_map)
    for (j, c_name) in enumerate(data.row_to_name)
        set = bounds_to_set(data.row_lower[j], data.row_upper[j])
        if set === nothing
            free_set = MOI.Interval(typemin(T), typemax(T))
            _add_constraint(model, data, variable_map, j, c_name, free_set)
        else
            _add_constraint(model, data, variable_map, j, c_name, set)
        end
    end
    for sos in data.sos_constraints
        MOI.add_constraint(
            model,
            MOI.VectorOfVariables([variable_map[x] for x in sos.columns]),
            sos.type == 1 ? MOI.SOS1(sos.weights) : MOI.SOS2(sos.weights),
        )
    end
    return
end

function _add_variable(model::Model{T}, data, variable_map, i, name) where {T}
    x = MOI.add_variable(model)
    variable_map[name] = x
    MOI.set(model, MOI.VariableName(), x, name)
    set = bounds_to_set(data.col_lower[i], data.col_upper[i])
    if set isa MOI.Interval
        # Do not add MOI.Interval constraints because we want to follow JuMP's
        # convention of adding separate lower and upper bounds.
        MOI.add_constraint(model, x, MOI.GreaterThan(set.lower::T))
        MOI.add_constraint(model, x, MOI.LessThan(set.upper::T))
    elseif set !== nothing
        MOI.add_constraint(model, x, set)
    end
    if data.vtype[i] == VTYPE_INTEGER
        MOI.add_constraint(model, x, MOI.Integer())
    elseif data.vtype[i] == VTYPE_BINARY
        MOI.add_constraint(model, x, MOI.ZeroOne())
    end
    return
end

function _add_objective(model::Model{T}, data, variable_map) where {T}
    if data.is_minimization
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    else
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    end
    affine_terms = MOI.ScalarAffineTerm{T}[
        MOI.ScalarAffineTerm(data.c[i], variable_map[v]) for
        (i, v) in enumerate(data.col_to_name) if !iszero(data.c[i])
    ]
    q_terms = MOI.ScalarQuadraticTerm{T}[]
    for (i, j, q) in data.quad_obj
        x = variable_map[i]
        y = variable_map[j]
        push!(q_terms, MOI.ScalarQuadraticTerm(q, x, y))
    end
    obj = if length(q_terms) == 0
        MOI.ScalarAffineFunction(affine_terms, -data.obj_constant)
    else
        MOI.ScalarQuadraticFunction(q_terms, affine_terms, -data.obj_constant)
    end

    MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
    return
end

function _add_constraint(model, data, variable_map, j, c_name, set)
    if haskey(data.qc_matrix, c_name)
        _add_quad_constraint(model, data, variable_map, j, c_name, set)
    elseif haskey(data.indicators, c_name)
        _add_indicator_constraint(model, data, variable_map, j, c_name, set)
    else
        _add_linear_constraint(model, data, variable_map, j, c_name, set)
    end
    return
end

function _add_indicator_constraint(
    model::Model{T},
    data,
    variable_map,
    j,
    c_name,
    set,
) where {T}
    z, activate = data.indicators[c_name]
    terms = MOI.VectorAffineTerm{T}[MOI.VectorAffineTerm(
        1,
        MOI.ScalarAffineTerm(one(T), variable_map[z]),
    ),]
    for (i, coef) in data.A[j]
        scalar = MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]])
        push!(terms, MOI.VectorAffineTerm(2, scalar))
    end
    f = MOI.VectorAffineFunction(terms, zeros(T, 2))
    c = MOI.add_constraint(model, f, MOI.Indicator{activate}(set))
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

function _add_linear_constraint(
    model::Model{T},
    data,
    variable_map,
    j,
    c_name,
    set,
) where {T}
    terms = MOI.ScalarAffineTerm{T}[
        MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]]) for
        (i, coef) in data.A[j]
    ]
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(terms, zero(T)), set)
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

function _add_quad_constraint(
    model::Model{T},
    data,
    variable_map,
    j,
    c_name,
    set,
) where {T}
    aff_terms = MOI.ScalarAffineTerm{T}[
        MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]]) for
        (i, coef) in data.A[j]
    ]
    quad_terms = MOI.ScalarQuadraticTerm{T}[]
    options = get_options(model)
    scale = 1
    if options.quadratic_format == kQuadraticFormatGurobi
        # Gurobi does NOT have a /2 as part of the quadratic matrix. Why oh why
        # would you break precedent with all other formats.
        scale = 2
    end
    for (x_name, y_name, q) in data.qc_matrix[c_name]
        x, y = variable_map[x_name], variable_map[y_name]
        push!(quad_terms, MOI.ScalarQuadraticTerm(scale * q, x, y))
    end
    f = MOI.ScalarQuadraticFunction(quad_terms, aff_terms, zero(T))
    c = MOI.add_constraint(model, f, set)
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

# ==============================================================================
#   NAME
# ==============================================================================

function parse_name_line(data::TempMPSModel, line)
    m = match(r"^\s*NAME(.*)"i, line)
    if m === nothing
        _throw_parse_error(
            data,
            "This line must be  of the form `NAME <problem name>`.",
        )
    end
    data.name = strip(m[1])
    return
end

# ==============================================================================
#   ROWS
# ==============================================================================

function parse_rows_line(data::TempMPSModel{T}, items::Vector) where {T}
    if length(items) < 2
        _throw_parse_error(
            data,
            "A `ROWS` line must have two fields, where the first is the sense (`N`, `G`, `L`, or `E`) and the second is the name of the row.",
        )
    end
    # if length(items) > 2
    #     We could throw an error here, but it seems like other solvers just
    #     happily ignore the extra fields.
    #
    #     See https://github.com/jump-dev/MathOptInterface.jl/issues/2792
    #
    #     Oscar dislikes the poorly standardized nature of MPS.
    # end
    sense, name = Sense(items[1]), items[2]
    if haskey(data.name_to_row, name)
        _throw_parse_error(
            data,
            "There are two ROWS with a duplicate name: $name.",
        )
    elseif sense == SENSE_UNKNOWN
        _throw_parse_error(
            data,
            "This row sense is invalid: $(items[1]). It must be `N`, `G`, `L`, or `E`.",
        )
    end
    if sense == SENSE_N
        if data.obj_name == ""
            # The first N row is the objective
            data.obj_name = name
            return
        end
    end
    if name == data.obj_name
        _throw_parse_error(
            data,
            "Encountered a row with same name as the objective: $name.",
        )
    end
    # Add some default bounds for the constraints.
    push!(data.row_to_name, name)
    row = length(data.row_to_name)
    data.name_to_row[name] = row
    push!(data.sense, sense)
    push!(data.A, Tuple{Int,T}[])
    if sense == SENSE_G
        push!(data.row_lower, zero(T))
        push!(data.row_upper, typemax(T))
        data.row_upper[row] = typemax(T)
    elseif sense == SENSE_L
        push!(data.row_lower, typemin(T))
        push!(data.row_upper, zero(T))
    elseif sense == SENSE_E
        push!(data.row_lower, zero(T))
        push!(data.row_upper, zero(T))
    else
        # This really is an assert, not an opportunity for a ParseError
        @assert sense == SENSE_N
        push!(data.row_lower, typemin(T))
        push!(data.row_upper, typemax(T))
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function parse_single_coefficient(data, row_name, column::Int, value)
    if row_name == data.obj_name
        data.c[column] += value
        return
    end
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        _throw_parse_error(
            data,
            "The ROW name $(row_name) not recognised. If a row appears in the COLUMNS section, it must first be declared in the ROWS section.",
        )
    end
    push!(data.A[row], (column, value))
    return
end

function _add_new_column(data::TempMPSModel{T}, column_name) where {T}
    if haskey(data.name_to_col, column_name)
        return
    end
    push!(data.col_to_name, column_name)
    data.name_to_col[column_name] = length(data.col_to_name)
    push!(data.c, zero(T))
    push!(data.col_lower, zero(T))
    push!(data.col_upper, typemax(T))
    push!(data.col_bounds_default, true)
    push!(data.vtype, VTYPE_CONTINUOUS)
    return
end

function _set_intorg(data::TempMPSModel{T}, column, column_name) where {T}
    if data.intorg_flag
        data.vtype[column] = VTYPE_INTEGER
        # The default upper bound for variables in INTORG is `1`, not `Inf`...
        data.col_upper[column] = one(T)
    elseif data.vtype[column] != VTYPE_CONTINUOUS
        _throw_parse_error(
            data,
            "The variable $(column_name) appeared in COLUMNS outside an `INTORG`-`INTEND` marker after already being declared as integer.",
        )
    end
    return
end

function parse_columns_line(data::TempMPSModel{T}, items::Vector) where {T}
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
        end
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(data, row_name, column, parse(T, value))
        _set_intorg(data, column, column_name)
    elseif length(items) == 5
        # [column name] [row name] [value] [row name 2] [value 2]
        column_name, row_name_1, value_1, row_name_2, value_2 = items
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(data, row_name_1, column, parse(T, value_1))
        parse_single_coefficient(data, row_name_2, column, parse(T, value_2))
        _set_intorg(data, column, column_name)
    else
        _throw_parse_error(
            data,
            "Malformed COLUMNS line. This line must have 3 or 5 fields.",
        )
    end
    return
end

# ==============================================================================
#   RHS
# ==============================================================================

function parse_single_rhs(data, row_name, value, items::Vector)
    if row_name == data.obj_name
        data.obj_constant = value
        return
    end
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        _throw_parse_error(
            data,
            "The ROW name $(row_name) not recognised. If a row appears in the RHS section, it must previously have been declared in the ROWS section.",
        )
    end
    if data.sense[row] == SENSE_E
        data.row_upper[row] = value
        data.row_lower[row] = value
    elseif data.sense[row] == SENSE_G
        data.row_lower[row] = value
    elseif data.sense[row] == SENSE_L
        data.row_upper[row] = value
    else
        # This really is an assert, not an opportunity for a ParseError.
        @assert data.sense[row] == SENSE_N
        _throw_parse_error(
            data,
            "A row sense `N` cannot have a right-hand side value.",
        )
    end
    return
end

# TODO: handle multiple RHS vectors.
function parse_rhs_line(data::TempMPSModel{T}, items::Vector) where {T}
    if length(items) == 3
        # [rhs name] [row name] [value]
        rhs_name, row_name, value = items
        parse_single_rhs(data, row_name, parse(T, value), items)
    elseif length(items) == 5
        # [rhs name] [row name 1] [value 1] [row name 2] [value 2]
        rhs_name, row_name_1, value_1, row_name_2, value_2 = items
        parse_single_rhs(data, row_name_1, parse(T, value_1), items)
        parse_single_rhs(data, row_name_2, parse(T, value_2), items)
    else
        _throw_parse_error(
            data,
            "Malformed RHS line: expected three or five fields.",
        )
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

function parse_single_range(data, row_name, value)
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        _throw_parse_error(
            data,
            "The ROW name $(row_name) not recognised. If a row appears in the RHS section, it must previously have been declared in the ROWS section.",
        )
    end
    if data.sense[row] == SENSE_G
        data.row_upper[row] = data.row_lower[row] + abs(value)
    elseif data.sense[row] == SENSE_L
        data.row_lower[row] = data.row_upper[row] - abs(value)
    elseif data.sense[row] == SENSE_E
        if value > 0
            data.row_upper[row] = data.row_lower[row] + value
        else
            data.row_lower[row] = data.row_upper[row] + value
        end
    end
    return
end

# TODO: handle multiple RANGES vectors.
function parse_ranges_line(data::TempMPSModel{T}, items::Vector) where {T}
    if length(items) == 3
        # [rhs name] [row name] [value]
        _, row_name, value = items
        parse_single_range(data, row_name, parse(T, value))
    elseif length(items) == 5
        # [rhs name] [row name] [value] [row name 2] [value 2]
        _, row_name_1, value_1, row_name_2, value_2 = items
        parse_single_range(data, row_name_1, parse(T, value_1))
        parse_single_range(data, row_name_2, parse(T, value_2))
    else
        _throw_parse_error(
            data,
            "Malformed RANGES line: expected three or five fields.",
        )
    end
    return
end

# ==============================================================================
#   BOUNDS
# ==============================================================================

function _parse_single_bound(
    data::TempMPSModel{T},
    column_name,
    bound_type,
) where {T}
    col = get(data.name_to_col, column_name, nothing)
    if col === nothing
        _throw_parse_error(
            data,
            "The column name $(column_name) was not recognized. If a column appears in the BOUNDS section, it must have previously appeared in the COLUMNS section.",
        )
    end
    if data.col_bounds_default[col] && data.vtype[col] == VTYPE_INTEGER
        # This column was part of an INTORG...INTEND block, so it gets a default
        # bound of [0, 1]. However, since it now has a bound, it reverts to a
        # default of [0, inf).
        data.col_upper[col] = typemax(T)
    end
    data.col_bounds_default[col] = false
    if bound_type == "PL"
        data.col_upper[col] = typemax(T)
    elseif bound_type == "MI"
        data.col_lower[col] = typemin(T)
    elseif bound_type == "FR"
        data.col_lower[col] = typemin(T)
        data.col_upper[col] = typemax(T)
    elseif bound_type == "BV"
        data.col_lower[col] = typemin(T)
        data.col_upper[col] = typemax(T)
        data.vtype[col] = VTYPE_BINARY
    else
        _throw_parse_error(
            data,
            "The bound type $bound_type is invalid when there are three fields.",
        )
    end
end

function _parse_single_bound(
    data::TempMPSModel{T},
    column_name,
    bound_type,
    value::T,
) where {T}
    col = get(data.name_to_col, column_name, nothing)
    if col === nothing
        _throw_parse_error(
            data,
            "The column name $(column_name) was not recognized. If a column appears in the BOUNDS section, it must have previously appeared in the COLUMNS section.",
        )
    end
    if data.col_bounds_default[col] && data.vtype[col] == VTYPE_INTEGER
        # This column was part of an INTORG...INTEND block, so it gets a default
        # bound of [0, 1]. However, since it now has a bound, it reverts to a
        # default of [0, inf).
        data.col_upper[col] = typemax(T)
    end
    data.col_bounds_default[col] = false
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
        # there are cases in MIPLIB2017 (for example, leo1 and leo2) like so:
        #  FR BOUND1    C0000001       .000000
        # In these situations, just ignore the value.
        data.col_lower[col] = typemin(T)
        data.col_upper[col] = typemax(T)
    elseif bound_type == "BV"
        # A similar situation happens with BV bounds in leo1 and leo2.
        data.col_lower[col] = typemin(T)
        data.col_upper[col] = typemax(T)
        data.vtype[col] = VTYPE_BINARY
    else
        _throw_parse_error(
            data,
            "The bound type $bound_type is invalid when there are four fields.",
        )
    end
end

function parse_bounds_line(data::TempMPSModel{T}, items::Vector) where {T}
    if length(items) == 3
        bound_type, _, column_name = items
        _parse_single_bound(data, column_name, bound_type)
    elseif length(items) == 4
        bound_type, _, column_name, value = items
        _parse_single_bound(data, column_name, bound_type, parse(T, value))
    else
        _throw_parse_error(
            data,
            "Malformed BOUNDS line: expected three or four fields.",
        )
    end
    return
end

# ==============================================================================
#   SOS
# ==============================================================================

function parse_sos_line(data::TempMPSModel{T}, items) where {T}
    if length(items) != 2
        _throw_parse_error(data, "Malformed SOS line: expected two fields.")
    elseif items[1] == "S1"
        push!(data.sos_constraints, _SOSConstraint(1, T[], String[]))
    elseif items[1] == "S2"
        push!(data.sos_constraints, _SOSConstraint(2, T[], String[]))
    else
        sos = data.sos_constraints[end]
        push!(sos.columns, items[1])
        push!(sos.weights, parse(T, items[2]))
    end
    return
end

# ==============================================================================
#   QUADOBJ
# ==============================================================================

function parse_quadobj_line(data::TempMPSModel{T}, items) where {T}
    if length(items) != 3
        _throw_parse_error(
            data,
            "Malformed QUADOBJ line: expected three fields.",
        )
    end
    push!(data.quad_obj, (items[1], items[2], parse(T, items[3])))
    return
end

# ==============================================================================
#   QMATRIX
# ==============================================================================

function parse_qmatrix_line(data::TempMPSModel{T}, items) where {T}
    if length(items) != 3
        _throw_parse_error(
            data,
            "Malformed QMATRIX line: expected three fields.",
        )
    end
    if data.name_to_col[items[1]] <= data.name_to_col[items[2]]
        # Off-diagonals have duplicate entries. We don't need to store both
        # triangles.
        push!(data.quad_obj, (items[1], items[2], parse(T, items[3])))
    end
    return
end

# ==============================================================================
#   QMATRIX
# ==============================================================================

function parse_qcmatrix_line(data::TempMPSModel{T}, items) where {T}
    if length(items) != 3
        _throw_parse_error(
            data,
            "Malformed QCMATRIX line: expected three fields.",
        )
    end
    if data.name_to_col[items[1]] <= data.name_to_col[items[2]]
        # Off-diagonals have duplicate entries. We don't need to store both
        # triangles.
        push!(
            data.qc_matrix[data.current_qc_matrix],
            (items[1], items[2], parse(T, items[3])),
        )
    end
    return
end

# ==============================================================================
#   QSECTION
# ==============================================================================

function parse_qsection_line(data::TempMPSModel{T}, items) where {T}
    if length(items) != 3
        _throw_parse_error(
            data,
            "Malformed QSECTION line: expected three fields.",
        )
    end
    if data.current_qc_matrix == "OBJ"
        push!(data.quad_obj, (items[1], items[2], parse(T, items[3])))
    else
        push!(
            data.qc_matrix[data.current_qc_matrix],
            (items[1], items[2], parse(T, items[3])),
        )
    end
    return
end

# ==============================================================================
#   INDICATORS
# ==============================================================================

function parse_indicators_line(data, items)
    if length(items) != 4
        _throw_parse_error(
            data,
            "Malformed INDICATORS line: expected four fields.",
        )
    end
    condition = if items[4] == "0"
        MOI.ACTIVATE_ON_ZERO
    elseif items[4] == "1"
        MOI.ACTIVATE_ON_ONE
    else
        _throw_parse_error(
            data,
            "The value in field four of an indicator constraint must be either `0` or `1`.",
        )
    end
    data.indicators[items[2]] = (items[3], condition)
    return
end
