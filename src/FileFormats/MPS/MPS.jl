module MPS

import ..FileFormats

import MathOptInterface
const MOI = MathOptInterface

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
    warn::Bool = false
)
    model = Model{Float64}()
    model.ext[:MPS_OPTIONS] = Options(warn)
    return model
end

function Base.show(io::IO, ::Model)
    print(io, "A Mathematical Programming System (MPS) model")
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
    write_model_name(io, model)
    write_rows(io, model)
    discovered_columns = write_columns(io, model)
    write_rhs(io, model)
    write_ranges(io, model)
    write_bounds(io, model, discovered_columns)
    write_sos(io, model)
    println(io, "ENDATA")
    return
end

# ==============================================================================
#   Model name
# ==============================================================================

function write_model_name(io::IO, model::Model)
    model_name = MOI.get(model, MOI.Name())
    println(io, "NAME          ", model_name)
    return
end

# ==============================================================================
#   ROWS
# ==============================================================================

const LINEAR_CONSTRAINTS = (
    (MOI.LessThan{Float64}, 'L'),
    (MOI.GreaterThan{Float64}, 'G'),
    (MOI.EqualTo{Float64}, 'E'),
    (MOI.Interval{Float64}, 'L')  # See the note in the RANGES section.
)

function _write_rows(io, model, set_type, sense_char)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64}, set_type
        }()
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        if row_name == ""
            error("Row name is empty: $(index).")
        end
        println(io, " ", sense_char, "  ", row_name)
    end
    return
end

function write_rows(io::IO, model::Model)
    println(io, "ROWS\n N  OBJ")
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
        _write_rows(io, model, set_type, sense_char)
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function _list_of_integer_variables(model, integer_variables, set_type)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.SingleVariable, set_type}()
    )
        v_index = MOI.get(model, MOI.ConstraintFunction(), index)
        v_name = MOI.get(model, MOI.VariableName(), v_index.variable)
        push!(integer_variables, v_name)
    end
    return
end

function list_of_integer_variables(model::Model)
    integer_variables = Set{String}()
    for set_type in (MOI.ZeroOne, MOI.Integer)
        _list_of_integer_variables(model, integer_variables, set_type)
    end
    return integer_variables
end

function add_coefficient(coefficients, variable_name, row_name, coefficient)
    if haskey(coefficients, variable_name)
        push!(coefficients[variable_name], (row_name, coefficient))
    else
        coefficients[variable_name] = [(row_name, coefficient)]
    end
    return
end

function extract_terms(
    model::Model,
    coefficients,
    row_name::String,
    func::MOI.ScalarAffineFunction,
    discovered_columns::Set{String}
)
    for term in func.terms
        variable_name = MOI.get(model, MOI.VariableName(), term.variable_index)
        add_coefficient(coefficients, variable_name, row_name, term.coefficient)
        push!(discovered_columns, variable_name)
    end
    return
end

function extract_terms(
    model::Model,
    coefficients,
    row_name::String,
    func::MOI.SingleVariable,
    discovered_columns::Set{String},
)
    variable_name = MOI.get(model, MOI.VariableName(), func.variable)
    add_coefficient(coefficients, variable_name, row_name, 1.0)
    push!(discovered_columns, variable_name)
    return
end

function _write_columns(
    io, model, set_type, sense_char, coefficients, discovered_columns
)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64}, set_type
        }()
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        extract_terms(
            model, coefficients, row_name, func, discovered_columns
        )
    end
end
function write_columns(io::IO, model::Model)
    # Many MPS readers (e.g., CPLEX and GAMS) will error if a variable (column)
    # appears in the BOUNDS section but did not appear in the COLUMNS section.
    # This is likely because such variables are meaningless - they don't appear
    # in the objective or constraints and so can be trivially removed.
    # To avoid generating MPS files that crash existing readers, we cache the
    # names of all variables seen in COLUMNS into `discovered_columns`, and then
    # pass this set to `write_bounds` so that it can act appropriately.
    discovered_columns = Set{String}()
    println(io, "COLUMNS")
    coefficients = Dict{String, Vector{Tuple{String, Float64}}}()
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
        _write_columns(
            io, model, set_type, sense_char, coefficients, discovered_columns
        )
    end
    obj_func_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_func = MOI.get(model, MOI.ObjectiveFunction{obj_func_type}())
    extract_terms(model, coefficients, "OBJ", obj_func, discovered_columns)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        # MPS doesn't support maximization so we flip the sign on the objective
        # coefficients.
        for (v_name, terms) in coefficients
            for (idx, (row_name, coef)) in enumerate(terms)
                if row_name == "OBJ"
                    terms[idx] = (row_name, -coef)
                end
            end
        end
    end
    integer_variables = list_of_integer_variables(model)
    for (variable, terms) in coefficients
        if variable in integer_variables
            println(io, "    MARKER    'MARKER'                 'INTORG'")
        end
        for (constraint, coefficient) in terms
            print(io, "     ", rpad(variable, 8), " ", rpad(constraint, 8), " ")
            Base.Grisu.print_shortest(io, coefficient)
            println(io)
        end
        if variable in integer_variables
            println(io, "    MARKER    'MARKER'                 'INTEND'")
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

function _write_rhs(io, model, set_type, sense_char)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{
            MOI.ScalarAffineFunction{Float64}, set_type
        }()
    )
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        print(io, "    rhs       ", rpad(row_name, 8), "  ")
        set = MOI.get(model, MOI.ConstraintSet(), index)
        Base.Grisu.print_shortest(io, value(set))
        println(io)
    end
end

function write_rhs(io::IO, model::Model)
    println(io, "RHS")
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
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
        print(io, "    rhs       ", rpad(row_name, 8), "  ")
        set = MOI.get(model, MOI.ConstraintSet(), index)::MOI.Interval{Float64}
        Base.Grisu.print_shortest(io, set.upper - set.lower)
        println(io)
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
    name = rpad(var_name, 8)
    if lower == upper
        print(io, " FX bounds    ", name, " ")
        Base.Grisu.print_shortest(io, lower)
        println(io)
    elseif lower == -Inf && upper == Inf
        # Skip this for now, we deal with it at the end of write_bounds.
    else
        if lower == -Inf
            println(io, " MI bounds    ", name)
        else
            print(io, " LO bounds    ", name, " ")
            Base.Grisu.print_shortest(io, lower)
            println(io)
        end
        if upper == Inf
            println(io, " PL bounds    ", name)
        else
            print(io, " UP bounds    ", name, " ")
            Base.Grisu.print_shortest(io, upper)
            println(io)
        end
    end
    return
end

function update_bounds(current::Tuple{Float64, Float64}, set::MOI.GreaterThan)
    return (max(current[1], set.lower), current[2])
end

function update_bounds(current::Tuple{Float64, Float64}, set::MOI.LessThan)
    return (current[1], min(current[2], set.upper))
end

function update_bounds(::Tuple{Float64, Float64}, set::MOI.Interval)
    return (set.lower, set.upper)
end

function update_bounds(::Tuple{Float64, Float64}, set::MOI.EqualTo)
    return (set.value, set.value)
end

function _collect_bounds(bounds, model, set_type)
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.SingleVariable, set_type}()
    )
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        variable_index = func.variable::MOI.VariableIndex
        if !haskey(bounds, variable_index)
            bounds[variable_index] = (-Inf, Inf)
        end
        set = MOI.get(model, MOI.ConstraintSet(), index)::set_type
        bounds[variable_index] = update_bounds(bounds[variable_index], set)
    end
    return
end

function write_bounds(io::IO, model::Model, discovered_columns::Set{String})
    println(io, "BOUNDS")
    free_variables = Set(MOI.get(model, MOI.ListOfVariableIndices()))
    bounds = Dict{MOI.VariableIndex, Tuple{Float64, Float64}}()
    for (set_type, _) in LINEAR_CONSTRAINTS
        _collect_bounds(bounds, model, set_type)
    end
    for (index, (lower, upper)) in bounds
        var_name = MOI.get(model, MOI.VariableName(), index)
        if var_name in discovered_columns
            write_single_bound(io, var_name, lower, upper)
        else
            @warn("Variable $var_name is mentioned in BOUNDS, but is not " *
                  "mentioned in the COLUMNS section. We are ignoring it.")
        end
        pop!(free_variables, index)
    end
    for index in MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.SingleVariable, MOI.ZeroOne}()
    )
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        variable_index = func.variable::MOI.VariableIndex
        var_name = MOI.get(model, MOI.VariableName(), variable_index)
        if var_name in discovered_columns
            println(io, " BV bounds    ", var_name)
        else
            @warn("Variable $var_name is mentioned in BOUNDS, but is not " *
                  "mentioned in the COLUMNS section. We are ignoring it.")
        end
        if variable_index in free_variables
            # We can remove the variable because it has a bound, but first check
            # that it is still there because some variables might have two
            # bounds and so might have already been removed.
            pop!(free_variables, variable_index)
        end
    end
    for variable_index in free_variables
        var_name = MOI.get(model, MOI.VariableName(), variable_index)
        if var_name in discovered_columns
            println(io, " FR bounds    ", var_name)
        else
            @warn("Variable $var_name is mentioned in BOUNDS, but is not " *
                  "mentioned in the COLUMNS section. We are ignoring it.")
        end
    end
    return
end

# ==============================================================================
#   SOS
# ==============================================================================

function write_sos_constraint(io::IO, model::Model, index)
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    for (variable, weight) in zip(func.variables, set.weights)
        var_name = MOI.get(model, MOI.VariableName(), variable)
        print(io, "    ", rpad(var_name, 8), "  ")
        Base.Grisu.print_shortest(io, weight)
        println(io)
    end
end

function write_sos(io::IO, model::Model)
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
                println(io, " S", sos_type, " SOS", idx)
                write_sos_constraint(io, model, index)
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

mutable struct TempMPSModel
    name::String
    obj_name::String
    c::Vector{Float64}
    col_lower::Vector{Float64}
    col_upper::Vector{Float64}
    row_lower::Vector{Float64}
    row_upper::Vector{Float64}
    sense::Vector{String}
    A::Vector{Vector{Tuple{Int, Float64}}}
    is_int::Vector{Bool}
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
        String[],   # sense
        Vector{Vector{Tuple{Int, Float64}}}[],  # A
        Bool[],
        Dict{String, Int}(),
        String[],
        Dict{String, Int}(),
        String[],
        false,
    )
end

const HEADERS = ("ROWS", "COLUMNS", "RHS", "RANGES", "BOUNDS", "SOS", "ENDATA")

"""
    Base.read!(io::IO, model::FileFormats.MPS.Model)

Read `io` in the MPS file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    data = TempMPSModel()
    header = "NAME"
    multi_objectives = String[]
    while !eof(io) && header != "ENDATA"
        line = strip(readline(io))
        if line == "" || startswith(line, "*")
            # Skip blank lines and comments.
            continue
        end
        if uppercase(string(line)) in HEADERS
            header = uppercase(string(line))
            continue
        end
        # TODO: split into hard fields based on column indices.
        items = String.(split(line, " ", keepempty = false))
        if header == "NAME"
            # A special case. This only happens at the start.
            parse_name_line(data, items)
        elseif header == "ROWS"
            multi_obj = parse_rows_line(data, items)
            multi_obj !== nothing && push!(multi_objectives, multi_obj)
        elseif header == "COLUMNS"
            parse_columns_line(data, items, multi_objectives)
        elseif header == "RHS"
            parse_rhs_line(data, items)
        elseif header == "RANGES"
            parse_ranges_line(data, items)
        elseif header == "BOUNDS"
            parse_bounds_line(data, items)
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
    return
end

function copy_to(model::Model, data::TempMPSModel)
    MOI.set(model, MOI.Name(), data.name)
    variable_map = Dict{String, MOI.VariableIndex}()
    # Add variables.
    for (i, name) in enumerate(data.col_to_name)
        x = MOI.add_variable(model)
        variable_map[name] = x
        MOI.set(model, MOI.VariableName(), x, name)
        set = bounds_to_set(data.col_lower[i], data.col_upper[i])
        if set === nothing && data.is_int[i]
            # TODO: some solvers may interpret this as binary.
            MOI.add_constraint(model, MOI.SingleVariable(x), MOI.Integer())
        elseif set !== nothing && data.is_int[i]
            if set == MOI.Interval(0.0, 1.0)
                MOI.add_constraint(model, MOI.SingleVariable(x), MOI.ZeroOne())
            else
                MOI.add_constraint(model, MOI.SingleVariable(x), MOI.Integer())
                MOI.add_constraint(model, MOI.SingleVariable(x), set)
            end
        elseif set !== nothing
            MOI.add_constraint(model, MOI.SingleVariable(x), set)
        end
    end
    # Set objective.
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
    # Add linear constraints.
    for (j, c_name) in enumerate(data.row_to_name)
        set = bounds_to_set(data.row_lower[j], data.row_upper[j])
        if set === nothing
            error("Expected a non-empty set for $(c_name). Got row=$(row)")
        end
        c = MOI.add_constraint(
            model,
            MOI.ScalarAffineFunction(
                [
                    MOI.ScalarAffineTerm(
                        coef, variable_map[data.col_to_name[i]]
                    ) for (i, coef) in data.A[j]
                ],
                0.0
            ),
            set,
        )
        MOI.set(model, MOI.ConstraintName(), c, c_name)
    end
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
    sense, name = items
    if haskey(data.name_to_row, name)
        error("Duplicate row encountered: $(line).")
    elseif sense != "N" && sense != "L" && sense != "G" && sense != "E"
        error("Invalid row sense: $(join(items, " "))")
    end
    if sense == "N"
        if data.obj_name != ""
            return name  # Detected a duplicate objective. Skip it.
        end
        data.obj_name = name
        return
    end
    if name == data.obj_name
        error("Found row with same name as objective: $(line).")
    end
    # Add some default bounds for the constraints.
    push!(data.row_to_name, name)
    row = length(data.row_to_name)
    data.name_to_row[name] = row
    push!(data.sense, sense)
    push!(data.A, Tuple{Int, Float64}[])
    if sense == "G"
        push!(data.row_lower, 0.0)
        push!(data.row_upper, Inf)
        data.row_upper[row] = Inf
    elseif sense == "L"
        push!(data.row_lower, -Inf)
        push!(data.row_upper, 0.0)
    elseif sense == "E"
        push!(data.row_lower, 0.0)
        push!(data.row_upper, 0.0)
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function parse_single_coefficient(data, row_name::String, column::Int, value)
    if row_name == data.obj_name
        data.c[column] += parse(Float64, value)
        return
    end
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    push!(data.A[row], (column, parse(Float64, value)))
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
    push!(data.is_int, false)
    return
end

function _set_intorg(data, column, column_name)
    if data.is_int[column] && !data.intorg_flag
        error(
            "Variable $(column_name) appeared in COLUMNS outside an " *
            "`INT` marker after already being declared as integer."
        )
    end
    data.is_int[column] = data.intorg_flag
end

function parse_columns_line(
    data::TempMPSModel, items::Vector{String}, multi_objectives::Vector{String}
)
    if length(items) == 3
        # [column name] [row name] [value]
        column_name, row_name, value = items
        if uppercase(row_name) == "'MARKER'" && uppercase(value) == "'INTORG'"
            data.intorg_flag = true
            return
        elseif uppercase(row_name) == "'MARKER'" && uppercase(value) == "'INTEND'"
            data.intorg_flag = false
            return
        elseif row_name in multi_objectives
            return
        end
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(data, row_name, column, value)
        _set_intorg(data, column, column_name)
    elseif length(items) == 5
        # [column name] [row name] [value] [row name 2] [value 2]
        column_name, row_name_1, value_1, row_name_2, value_2 = items
        if row_name_1 in multi_objectives || row_name_2 in multi_objectives
            return
        end
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(data, row_name_1, column, value_1)
        parse_single_coefficient(data, row_name_2, column, value_2)
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
    if data.sense[row] == "E"
        data.row_upper[row] = value
        data.row_lower[row] = value
    elseif data.sense[row] == "G"
        data.row_lower[row] = value
    elseif data.sense[row] == "L"
        data.row_upper[row] = value
    else
        @assert data.sense[row] == "N"
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

function parse_single_range(data, row_name, value)
    row = get(data.name_to_row, row_name, nothing)
    if row === nothing
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    value = parse(Float64, value)
    if data.sense[row] == "G"
        data.row_upper[row] = data.row_lower[row] + abs(value)
    elseif data.sense[row] == "L"
        data.row_lower[row] = data.row_upper[row] - abs(value)
    elseif data.sense[row] == "E"
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
        rhs_name, row_name, value = items
        parse_single_range(data, row_name, value)
    elseif length(items) == 5
        # [rhs name] [row name] [value] [row name 2] [value 2]
        rhs_name, row_name, value, row_name_2, value_2 = items
        parse_single_range(data, row_name, value)
        parse_single_range(data, row_name_2, value_2)
    else
        error("Malformed RANGES line: $(join(items, " "))")
    end
    return
end

# ==============================================================================
#   BOUNDS
# ==============================================================================

function _parse_single_bound(data, column_name, bound_type)
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
        data.col_lower[col] = 0.0
        data.col_upper[col] = 1.0
        data.is_int[col] = true
    else
        error("Invalid bound type $(bound_type): $(join(items, " "))")
    end
end

function _parse_single_bound(data, column_name, bound_type, value::Float64)
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
        data.is_int[col] = true
    elseif bound_type == "UI"
        data.col_upper[col] = value
        data.is_int[col] = true
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
        data.col_lower[col] = 0.0
        data.col_upper[col] = 1.0
        data.is_int[col] = true
    else
        error("Invalid bound type $(bound_type): $(join(items, " "))")
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
