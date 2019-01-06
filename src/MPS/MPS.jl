module MPS

using MathOptInterface

const MOI = MathOptInterface
const MOIU = MOI.Utilities

MOIU.@model(InnerMPSModel,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (MOI.SOS1, MOI.SOS2),
    (MOI.SingleVariable,),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

const Model = InnerMPSModel{Float64}

function Base.show(io::IO, ::Model)
    print(io, "A Mathematical Programming System (MPS) model")
    return
end

# ==============================================================================
#
#   MOI.write_to_file
#
# ==============================================================================

function MOI.write_to_file(model::Model, filename::String)
    open(filename, "w") do io
        write_model_name(io, model)
        write_rows(io, model)
        write_columns(io, model)
        write_rhs(io, model)
        write_ranges(io, model)
        write_bounds(io, model)
        write_sos(io, model)
        println(io, "ENDATA")
    end
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

function write_rows(io::IO, model::Model)
    println(io, "ROWS\n N  OBJ")
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
        for index in MOI.get(model, MOI.ListOfConstraintIndices{
                                        MOI.ScalarAffineFunction{Float64},
                                        set_type}())
            row_name = MOI.get(model, MOI.ConstraintName(), index)
            if row_name == ""
                error("Row name is empty: $(index).")
            end
            println(io, " ", sense_char, "  ", row_name)
        end
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function list_of_integer_variables(model::Model)
    integer_variables = Set{String}()
    for set_type in (MOI.ZeroOne, MOI.Integer)
        for index in MOI.get(model, MOI.ListOfConstraintIndices{
                MOI.SingleVariable, set_type}())
            v_index = MOI.get(model, MOI.ConstraintFunction(), index)
            v_name = MOI.get(model, MOI.VariableName(), v_index.variable)
            push!(integer_variables, v_name)
        end
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

function extract_terms(model::Model, coefficients, row_name::String,
                       func::MOI.ScalarAffineFunction)
    for term in func.terms
        variable_name = MOI.get(model, MOI.VariableName(), term.variable_index)
        add_coefficient(coefficients, variable_name, row_name, term.coefficient)
    end
    return
end

function extract_terms(model::Model, coefficients, row_name::String,
                       func::MOI.SingleVariable)
    variable_name = MOI.get(model, MOI.VariableName(), func.variable)
    add_coefficient(coefficients, variable_name, row_name, 1.0)
    return
end

function write_columns(io::IO, model::Model)
    println(io, "COLUMNS")
    coefficients = Dict{String, Vector{Tuple{String, Float64}}}()
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
        for index in MOI.get(model, MOI.ListOfConstraintIndices{
                                        MOI.ScalarAffineFunction{Float64},
                                        set_type}())
            row_name = MOI.get(model, MOI.ConstraintName(), index)
            func = MOI.get(model, MOI.ConstraintFunction(), index)
            extract_terms(model, coefficients, row_name, func)
        end
    end
    obj_func_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_func = MOI.get(model, MOI.ObjectiveFunction{obj_func_type}())
    extract_terms(model, coefficients, "OBJ", obj_func)
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
    return
end

# ==============================================================================
#   RHS
# ==============================================================================

value(set::MOI.LessThan) = set.upper
value(set::MOI.GreaterThan) = set.lower
value(set::MOI.EqualTo) = set.value
value(set::MOI.Interval) = set.upper  # See the note in the RANGES section.

function write_rhs(io::IO, model::Model)
    println(io, "RHS")
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
        for index in MOI.get(model, MOI.ListOfConstraintIndices{
                                        MOI.ScalarAffineFunction{Float64},
                                        set_type}())
            row_name = MOI.get(model, MOI.ConstraintName(), index)
            print(io, "    rhs       ", rpad(row_name, 8), "  ")
            set = MOI.get(model, MOI.ConstraintSet(), index)
            Base.Grisu.print_shortest(io, value(set))
            println(io)
        end
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
    for index in MOI.get(model, MOI.ListOfConstraintIndices{
                                    MOI.ScalarAffineFunction{Float64},
                                    MOI.Interval{Float64}}())
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

function write_single_bound(io::IO, set::MOI.LessThan, var_name::String)
    print(io, " UP bounds    ", rpad(var_name, 8))
    Base.Grisu.print_shortest(io, set.upper)
    println(io)
    println(io, " MI bounds    ", var_name)
    return
end

function write_single_bound(io::IO, set::MOI.GreaterThan, var_name::String)
    print(io, " LO bounds    ", rpad(var_name, 8))
    Base.Grisu.print_shortest(io, set.lower)
    println(io)
    # For clarity, we also print the Inf upper bound.
    println(io, " PL bounds    ", var_name)
    return
end

function write_single_bound(io::IO, set::MOI.EqualTo, var_name::String)
    print(io, " FX bounds    ", rpad(var_name, 8))
    Base.Grisu.print_shortest(io, set.value)
    println(io)
    return
end

function write_single_bound(io::IO, set::MOI.Interval, var_name::String)
    print(io, " LO bounds    ", rpad(var_name, 8))
    Base.Grisu.print_shortest(io, set.lower)
    println(io)
    print(io, " UP bounds    ", rpad(var_name, 8))
    Base.Grisu.print_shortest(io, set.upper)
    println(io)
    return
end

function write_bounds(io::IO, model::Model)
    println(io, "BOUNDS")
    free_variables = Set(MOI.get(model, MOI.ListOfVariableIndices()))
    for (set_type, sense_char) in LINEAR_CONSTRAINTS
        for index in MOI.get(model, MOI.ListOfConstraintIndices{
                MOI.SingleVariable, set_type}())
            func = MOI.get(model, MOI.ConstraintFunction(), index)
            variable_index = func.variable::MOI.VariableIndex
            set = MOI.get(model, MOI.ConstraintSet(), index)::set_type
            var_name = MOI.get(model, MOI.VariableName(), variable_index)
            write_single_bound(io, set, var_name)
            pop!(free_variables, variable_index)
        end
    end
    for index in MOI.get(model, MOI.ListOfConstraintIndices{
            MOI.SingleVariable, MOI.ZeroOne}())
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        variable_index = func.variable::MOI.VariableIndex
        var_name = MOI.get(model, MOI.VariableName(), variable_index)
        println(io, " BV bounds    ", var_name)
        pop!(free_variables, variable_index)
    end
    for variable_index in free_variables
        var_name = MOI.get(model, MOI.VariableName(), variable_index)
        println(io, " FR bounds    ", var_name)
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
    println(io, "SOS")
    idx = 1
    for index in MOI.get(model, MOI.ListOfConstraintIndices{
            MOI.VectorOfVariables, MOI.SOS1{Float64}}())
        println(io, " S1 SOS", idx)
        write_sos_constraint(io, model, index)
        idx += 1
    end
    for index in MOI.get(model, MOI.ListOfConstraintIndices{
            MOI.VectorOfVariables, MOI.SOS2{Float64}}())
        println(io, " S2 SOS", idx)
        write_sos_constraint(io, model, index)
        idx += 1
    end
    return
end

# ==============================================================================
#
#   MOI.read_from_file
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

mutable struct TempColumn
    lower::Float64
    upper::Float64
    is_int::Bool
    TempColumn() = new(0.0, Inf, false)
end

mutable struct TempRow
    lower::Float64
    upper::Float64
    sense::String
    terms::Dict{String, Float64}
    TempRow() = new(-Inf, Inf, "E", Dict{String, Float64}())
end

mutable struct TempMPSModel
    name::String
    obj_name::String
    columns::Dict{String, TempColumn}
    rows::Dict{String, TempRow}
    intorg_flag::Bool  # A flag used to parse COLUMNS section.
    TempMPSModel() = new("", "", Dict{String, TempColumn}(),
        Dict{String, TempRow}(), false)
end

const HEADERS = ("ROWS", "COLUMNS", "RHS", "RANGES", "BOUNDS", "SOS", "ENDATA")

function MOI.read_from_file(model::Model, filename::String)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    data = TempMPSModel()
    open(filename, "r") do io
        header = "NAME"
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
            items = if VERSION >= v"0.7"
                String.(split(line, " ", keepempty = false))
            else
                String.(split(line, " ", keep = false))
            end
            if header == "NAME"
                # A special case. This only happens at the start.
                parse_name_line(data, items)
            elseif header == "ROWS"
                parse_rows_line(data, items)
            elseif header == "COLUMNS"
                parse_columns_line(data, items)
            elseif header == "RHS"
                parse_rhs_line(data, items)
            elseif header == "RANGES"
                parse_ranges_line(data, items)
            elseif header == "BOUNDS"
                parse_bounds_line(data, items)
            end
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
    return nothing
end

function copy_to(model::Model, temp::TempMPSModel)
    MOI.set(model, MOI.Name(), temp.name)
    variable_map = Dict{String, MOI.VariableIndex}()
    # Add variables.
    for (name, column) in temp.columns
        x = MOI.add_variable(model)
        variable_map[name] = x
        MOI.set(model, MOI.VariableName(), x, name)
        set = bounds_to_set(column.lower, column.upper)
        if set === nothing && column.is_int
            # TODO: some solvers may interpret this as binary.
            MOI.add_constraint(model, MOI.SingleVariable(x), MOI.Integer())
        elseif set !== nothing && column.is_int
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
    # Add linear constraints.
    for (c_name, row) in temp.rows
        if c_name == temp.obj_name
            # Set objective.
            MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
            obj_func = if length(row.terms) == 1 &&
                    first(row.terms).second == 1.0
                MOI.SingleVariable(variable_map[first(row.terms).first])
            else
                MOI.ScalarAffineFunction([
                    MOI.ScalarAffineTerm(coef, variable_map[v_name])
                        for (v_name, coef) in row.terms],
                0.0)
            end
            MOI.set(model, MOI.ObjectiveFunction{typeof(obj_func)}(), obj_func)
        else
            constraint_function = MOI.ScalarAffineFunction([
                MOI.ScalarAffineTerm(coef, variable_map[v_name])
                    for (v_name, coef) in row.terms],
                0.0)
            set = bounds_to_set(row.lower, row.upper)
            if set !== nothing
                c = MOI.add_constraint(model, constraint_function, set)
                MOI.set(model, MOI.ConstraintName(), c, c_name)
            end
        end
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
    if haskey(data.rows, name)
        error("Duplicate row encountered: $(line).")
    elseif sense != "N" && sense != "L" && sense != "G" && sense != "E"
        error("Invalid row sense: $(join(items, " "))")
    end
    row = TempRow()
    row.sense = sense
    data.rows[name] = row
    if sense == "N"
        if data.obj_name != ""
            error("Multiple obectives encountered: $(join(items, " "))")
        end
        data.obj_name = name
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function parse_single_coefficient(data, row_name, column_name, value)
    terms = data.rows[row_name].terms
    value = parse(Float64, value)
    if haskey(terms, column_name)
        terms[column_name] += value
    else
        terms[column_name] = value
    end
    return
end

function parse_columns_line(data::TempMPSModel, items::Vector{String})
    if length(items) == 3
        # [column name] [row name] [value]
        column_name, row_name, value = items
        if uppercase(row_name) == "'MARKER'" && uppercase(value) == "'INTORG'"
            data.intorg_flag = true
            return
        elseif uppercase(row_name) == "'MARKER'" &&
                uppercase(value) == "'INTEND'"
            data.intorg_flag = false
            return
        end
        if !haskey(data.columns, column_name)
            data.columns[column_name] = TempColumn()
        end
        parse_single_coefficient(data, row_name, column_name, value)
        if data.columns[column_name].is_int && !data.intorg_flag
            error("Variable $(column_name) appeared in COLUMNS outside an" *
                  " `INT` marker after already being declared as integer.")
        end
        data.columns[column_name].is_int = data.intorg_flag
    elseif length(items) == 5
        # [column name] [row name] [value] [row name 2] [value 2]
        column_name, row_name_1, value_1, row_name_2, value_2 = items
        if !haskey(data.columns, column_name)
            data.columns[column_name] = TempColumn()
        end
        parse_single_coefficient(data, row_name_1, column_name, value_1)
        parse_single_coefficient(data, row_name_2, column_name, value_2)
        if data.columns[column_name].is_int && !data.intorg_flag
            error("Variable $(column_name) appeared in COLUMNS outside an" *
                  " `INT` marker after already being declared as integer.")
        end
        data.columns[column_name].is_int = data.intorg_flag
    else
        error("Malformed COLUMNS line: $(join(items, " "))")
    end
    return
end

# ==============================================================================
#   RHS
# ==============================================================================

function parse_single_rhs(data, row_name, value)
    if !haskey(data.rows, row_name)
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    value = parse(Float64, value)
    sense = data.rows[row_name].sense
    if sense == "E"
        data.rows[row_name].upper = value
        data.rows[row_name].lower = value
    elseif sense == "G"
        data.rows[row_name].lower = value
    elseif sense == "L"
        data.rows[row_name].upper = value
    elseif sense == "N"
        error("Cannot have RHS for objective: $(join(items, " "))")
    end
    return
end

# TODO: handle multiple RHS vectors.
function parse_rhs_line(data::TempMPSModel, items::Vector{String})
    if length(items) == 3
        # [rhs name] [row name] [value]
        rhs_name, row_name, value = items
        parse_single_rhs(data, row_name, value)
    elseif length(items) == 5
        # [rhs name] [row name 1] [value 1] [row name 2] [value 2]
        rhs_name, row_name_1, value_1, row_name_2, value_2 = items
        parse_single_rhs(data, row_name_1, value_1)
        parse_single_rhs(data, row_name_2, value_2)
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
    if !haskey(data.rows, row_name)
        error("ROW name $(row_name) not recognised. Is it in the ROWS field?")
    end
    value = parse(Float64, value)
    row = data.rows[row_name]
    if row.sense == "G"
        row.upper = row.lower + abs(value)
    elseif row.sense == "L"
        row.lower = row.upper - abs(value)
    elseif row.sense == "E"
        if value > 0.0
            row.upper = row.upper + value
        else
            row.lower = row.lower + value
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

function parse_bounds_line(data::TempMPSModel, items::Vector{String})
    if length(items) == 3
        bound_type, bound_name, column_name = items
        if !haskey(data.columns, column_name)
            error("Column name $(column_name) not found.")
        end
        column = data.columns[column_name]
        if bound_type == "PL"
            column.upper = Inf
        elseif bound_type == "MI"
            column.lower = -Inf
        elseif bound_type == "FR"
            column.lower = -Inf
            column.upper = Inf
        elseif bound_type == "BV"
            column.lower = 0.0
            column.upper = 1.0
            column.is_int = true
        else
            error("Invalid bound type $(bound_type): $(join(items, " "))")
        end
    elseif length(items) == 4
        bound_type, bound_name, column_name, value = items
        if !haskey(data.columns, column_name)
            error("Column name $(column_name) not found.")
        end
        column = data.columns[column_name]
        value = parse(Float64, value)
        if bound_type == "FX"
            column.lower = column.upper = value
        elseif bound_type == "UP"
            column.upper = value
        elseif bound_type == "LO"
            column.lower = value
        elseif bound_type == "LI"
            column.lower = value
            column.is_int = true
        elseif bound_type == "UI"
            column.upper = value
            column.is_int = true
        else
            error("Invalid bound type $(bound_type): $(join(items, " "))")
        end
    else
        error("Malformed BOUNDS line: $(join(items, " "))")
    end
    return
end

end
