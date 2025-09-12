# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

const _NUM_TO_STRING = [string(i) for i in -10:10]

function _to_string(x::Real)
    if isinteger(x)
        if -10 <= x <= 10
            # Optimize some very common cases. It seems annoying to do this, but
            # the lookup is faster than `string(::Int)`, and many models contain
            # small integer constants like -1, 0, or 1.
            return _NUM_TO_STRING[Int(x)+11]
        elseif typemin(Int) <= x <= typemax(Int)
            return string(round(Int, x))
        end
    end
    return string(x)
end

# The card logic is as follows: where possible, try to fit within the strict MPS
# field limits. That means fields start at columns 2, 5, 15, 25, 40, and 50.
# However, since most readers default to loose MPS, make sure each field is
# separated by at least one space.

struct Card
    f1::String
    f2::String
    f3::String
    f4::String
    f5::String

    function Card(;
        f1::String = "",
        f2::String = "",
        f3::String = "",
        f4::String = "",
        f5::String = "",
    )
        return new(f1, f2, f3, f4, f5)
    end
end

function print_offset(io, offset, field, min_start)
    n = max(1, min_start - offset - 1)
    for _ in 1:n
        print(io, ' ')
    end
    print(io, field)
    return offset + n + length(field)
end

function Base.show(io::IO, card::Card)
    offset = print_offset(io, 0, card.f1, 2)
    offset = print_offset(io, offset, card.f2, 5)
    if !isempty(card.f3)
        offset = print_offset(io, offset, card.f3, 15)
    end
    if !isempty(card.f4)
        offset = print_offset(io, offset, card.f4, 25)
    end
    if !isempty(card.f5)
        offset = print_offset(io, offset, card.f5, 40)
    end
    return
end

"""
    Base.write(io::IO, model::FileFormats.MPS.Model)

Write `model` to `io` in the MPS file format.
"""
function Base.write(io::IO, model::Model)
    options = get_options(model)
    if options.generic_names
        # Generic variable names handled in this writer.
        FileFormats.create_generic_constraint_names(model)
    else
        FileFormats.create_unique_names(
            model;
            warn = options.warn,
            replacements = Function[s->replace(s, ' '=>'_')],
        )
    end
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    var_to_column = OrderedDict{MOI.VariableIndex,Int}()
    for (i, x) in enumerate(variables)
        var_to_column[x] = i
    end
    write_model_name(io, model)
    flip_obj = false
    if options.objsense
        if MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
            println(io, "OBJSENSE\n    MAX")
        else
            println(io, "OBJSENSE\n    MIN")
        end
    else
        flip_obj = MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
    end
    write_rows(io, model)
    obj_const, indicators = write_columns(io, model, flip_obj, var_to_column)
    write_rhs(io, model, obj_const)
    write_ranges(io, model)
    write_bounds(io, model, var_to_column)
    write_quadobj(io, model, flip_obj, var_to_column)
    if options.quadratic_format != kQuadraticFormatCPLEX
        # Gurobi needs qcons _after_ quadobj and _before_ SOS.
        write_quadcons(io, model, var_to_column)
    end
    write_sos(io, model, var_to_column)
    if options.quadratic_format == kQuadraticFormatCPLEX
        # CPLEX needs qcons _after_ SOS.
        write_quadcons(io, model, var_to_column)
    end
    write_indicators(io, indicators)
    println(io, "ENDATA")
    return
end

# ==============================================================================
#   Model name
# ==============================================================================

function write_model_name(io::IO, model::Model)
    model_name = MOI.get(model, MOI.Name())
    if isempty(model_name)
        println(io, "NAME")
    else
        println(io, rpad("NAME", 14), model_name)
    end
    return
end

# ==============================================================================
#   ROWS
# ==============================================================================

function _write_rows(io, model, ::Type{F}, ::Type{S}, sense_char) where {F,S}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        if row_name == ""
            error("Row name is empty: $(index).")
        end
        println(io, Card(f1 = sense_char, f2 = row_name))
    end
    return
end

function _write_rows(
    io,
    model::Model{T},
    ::Type{F},
    ::Type{S},
    ::Any,
) where {T,F,S<:MOI.Interval}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        if set.lower == typemin(T) && set.upper == typemax(T)
            println(io, Card(f1 = "N", f2 = row_name))
        elseif set.upper == typemax(T)
            println(io, Card(f1 = "G", f2 = row_name))
        elseif set.lower == typemin(T)
            println(io, Card(f1 = "L", f2 = row_name))
        else
            println(io, Card(f1 = "L", f2 = row_name))
        end
    end
    return
end

function write_rows(io::IO, model::Model{T}) where {T}
    println(io, "ROWS")
    println(io, Card(f1 = "N", f2 = "OBJ"))
    SAF = MOI.ScalarAffineFunction{T}
    SQF = MOI.ScalarQuadraticFunction{T}
    _write_rows(io, model, SAF, MOI.LessThan{T}, "L")
    _write_rows(io, model, SQF, MOI.LessThan{T}, "L")
    _write_rows(io, model, SAF, MOI.GreaterThan{T}, "G")
    _write_rows(io, model, SQF, MOI.GreaterThan{T}, "G")
    _write_rows(io, model, SAF, MOI.EqualTo{T}, "E")
    _write_rows(io, model, SQF, MOI.EqualTo{T}, "E")
    _write_rows(io, model, SAF, MOI.Interval{T}, "L")
    _write_rows(io, model, SQF, MOI.Interval{T}, "L")
    VAF = MOI.VectorAffineFunction{T}
    _write_rows(io, model, VAF, IndicatorLessThanTrue{T}, "L")
    _write_rows(io, model, VAF, IndicatorLessThanFalse{T}, "L")
    _write_rows(io, model, VAF, IndicatorGreaterThanTrue{T}, "G")
    _write_rows(io, model, VAF, IndicatorGreaterThanFalse{T}, "G")
    _write_rows(io, model, VAF, IndicatorEqualToTrue{T}, "E")
    _write_rows(io, model, VAF, IndicatorEqualToFalse{T}, "E")
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function _list_of_integer_variables(
    model,
    var_to_column,
    integer_variables,
    ::Type{S},
) where {S}
    for index in
        MOI.get(model, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
        v_index = MOI.get(model, MOI.ConstraintFunction(), index)
        push!(integer_variables, var_to_column[v_index])
    end
    return
end

function list_of_integer_variables(model::Model, var_to_column)
    set = Set{Int}()
    _list_of_integer_variables(model, var_to_column, set, MOI.ZeroOne)
    _list_of_integer_variables(model, var_to_column, set, MOI.Integer)
    return set
end

function _extract_terms(
    var_to_column::OrderedDict{MOI.VariableIndex,Int},
    coefficients::Vector{Vector{Tuple{String,T}}},
    row_name::String,
    func::MOI.ScalarAffineFunction,
    flip_sign::Bool = false,
) where {T}
    for term in func.terms
        column = var_to_column[term.variable]
        coef = flip_sign ? -term.coefficient : term.coefficient
        push!(coefficients[column], (row_name, coef))
    end
    return
end

function _extract_terms(
    var_to_column::OrderedDict{MOI.VariableIndex,Int},
    coefficients::Vector{Vector{Tuple{String,T}}},
    row_name::String,
    func::MOI.ScalarQuadraticFunction,
    flip_sign::Bool = false,
) where {T}
    for term in func.affine_terms
        column = var_to_column[term.variable]
        coef = flip_sign ? -term.coefficient : term.coefficient
        push!(coefficients[column], (row_name, coef))
    end
    return
end

function _collect_coefficients(
    model,
    ::Type{F},
    ::Type{S},
    var_to_column::OrderedDict{MOI.VariableIndex,Int},
    coefficients::Vector{Vector{Tuple{String,T}}},
) where {T,F,S}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        _extract_terms(var_to_column, coefficients, row_name, func)
    end
    return
end

_activation_condition(::Type{<:MOI.Indicator{A}}) where {A} = A

function _collect_indicator(
    model::Model{T},
    ::Type{S},
    var_to_column,
    coefficients,
    indicators,
) where {T,S}
    options = get_options(model)
    F = MOI.VectorAffineFunction{T}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        funcs = MOI.Utilities.eachscalar(func)
        z = convert(MOI.VariableIndex, funcs[1])
        _extract_terms(var_to_column, coefficients, row_name, funcs[2])
        condition = _activation_condition(S)
        var_name = _var_name(model, z, var_to_column[z], options.generic_names)
        push!(indicators, (row_name, var_name, condition))
    end
    return
end

function _get_objective(model::Model{T}) where {T}
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    return convert(MOI.ScalarQuadraticFunction{T}, f)
end

function _extract_terms_objective(model, var_to_column, coefficients, flip_obj)
    obj_func = _get_objective(model)
    _extract_terms(var_to_column, coefficients, "OBJ", obj_func, flip_obj)
    return flip_obj ? -obj_func.constant : obj_func.constant
end

function _var_name(
    model::Model,
    variable::MOI.VariableIndex,
    column::Int,
    generic_name::Bool,
)::String
    if generic_name
        return "C$column"
    else
        return MOI.get(model, MOI.VariableName(), variable)
    end
end

function write_columns(
    io::IO,
    model::Model{T},
    flip_obj,
    var_to_column,
) where {T}
    options = get_options(model)
    indicators = Tuple{String,String,MOI.ActivationCondition}[]
    coefficients = Vector{Tuple{String,T}}[
        Tuple{String,T}[] for _ in 1:length(var_to_column)
    ]
    # Build constraint coefficients
    # The functions and sets are given explicitly so that this function is
    # type-stable.
    SAF = MOI.ScalarAffineFunction{T}
    SQF = MOI.ScalarQuadraticFunction{T}
    LT, GT = MOI.LessThan{T}, MOI.GreaterThan{T}
    ET, IT = MOI.EqualTo{T}, MOI.Interval{T}
    _collect_coefficients(model, SAF, LT, var_to_column, coefficients)
    _collect_coefficients(model, SQF, LT, var_to_column, coefficients)
    _collect_coefficients(model, SAF, GT, var_to_column, coefficients)
    _collect_coefficients(model, SQF, GT, var_to_column, coefficients)
    _collect_coefficients(model, SAF, ET, var_to_column, coefficients)
    _collect_coefficients(model, SQF, ET, var_to_column, coefficients)
    _collect_coefficients(model, SAF, IT, var_to_column, coefficients)
    _collect_coefficients(model, SQF, IT, var_to_column, coefficients)
    _collect_indicator(
        model,
        IndicatorLessThanTrue{T},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorLessThanFalse{T},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorGreaterThanTrue{T},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorGreaterThanFalse{T},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorEqualToTrue{T},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorEqualToFalse{T},
        var_to_column,
        coefficients,
        indicators,
    )
    # Build objective
    constant =
        _extract_terms_objective(model, var_to_column, coefficients, flip_obj)
    integer_variables = list_of_integer_variables(model, var_to_column)
    println(io, "COLUMNS")
    int_open = false
    for (variable, column) in var_to_column
        var_name = _var_name(model, variable, column, options.generic_names)
        is_int = column in integer_variables
        if is_int && !int_open
            println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTORG'"))
            int_open = true
        elseif !is_int && int_open
            println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTEND'"))
            int_open = false
        end
        if length(coefficients[column]) == 0
            # Every variable must appear in the COLUMNS section. Add a 0
            # objective coefficient instead.
            println(io, Card(f2 = var_name, f3 = "OBJ", f4 = "0"))
        end
        for (constraint, coefficient) in coefficients[column]
            println(
                io,
                Card(
                    f2 = var_name,
                    f3 = constraint,
                    f4 = _to_string(coefficient),
                ),
            )
        end
    end
    if int_open
        println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTEND'"))
    end
    return constant, indicators
end

# ==============================================================================
#   RHS
# ==============================================================================

_value(set::MOI.LessThan) = set.upper
_value(set::MOI.GreaterThan) = set.lower
_value(set::MOI.EqualTo) = set.value
_value(set::MOI.Indicator) = _value(set.set)

function _write_rhs(io, model, ::Type{F}, ::Type{S}) where {F,S}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        println(
            io,
            Card(f2 = "rhs", f3 = row_name, f4 = _to_string(_value(set))),
        )
    end
    return
end

function _write_rhs(
    io,
    model::Model{T},
    ::Type{F},
    ::Type{S},
) where {T,F,S<:MOI.Interval}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        if set.lower == typemin(T) && set.upper == typemax(T)
            # No RHS. Free row
        elseif set.upper == typemax(T)
            value = _to_string(set.lower)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = value))
        elseif set.lower == typemin(T)
            value = _to_string(set.upper)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = value))
        else
            value = _to_string(set.upper)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = value))
        end
    end
    return
end

function write_rhs(io::IO, model::Model{T}, obj_const) where {T}
    println(io, "RHS")
    SAF = MOI.ScalarAffineFunction{T}
    SQF = MOI.ScalarQuadraticFunction{T}
    _write_rhs(io, model, SAF, MOI.LessThan{T})
    _write_rhs(io, model, SQF, MOI.LessThan{T})
    _write_rhs(io, model, SAF, MOI.GreaterThan{T})
    _write_rhs(io, model, SQF, MOI.GreaterThan{T})
    _write_rhs(io, model, SAF, MOI.EqualTo{T})
    _write_rhs(io, model, SQF, MOI.EqualTo{T})
    _write_rhs(io, model, SAF, MOI.Interval{T})
    _write_rhs(io, model, SQF, MOI.Interval{T})
    VAF = MOI.VectorAffineFunction{T}
    _write_rhs(io, model, VAF, IndicatorLessThanTrue{T})
    _write_rhs(io, model, VAF, IndicatorLessThanFalse{T})
    _write_rhs(io, model, VAF, IndicatorGreaterThanTrue{T})
    _write_rhs(io, model, VAF, IndicatorGreaterThanFalse{T})
    _write_rhs(io, model, VAF, IndicatorEqualToTrue{T})
    _write_rhs(io, model, VAF, IndicatorEqualToFalse{T})
    # Objective constants are added to the RHS as a negative offset.
    # https://www.ibm.com/docs/en/icos/20.1.0?topic=standard-records-in-mps-format
    if !iszero(obj_const)
        println(io, Card(f2 = "rhs", f3 = "OBJ", f4 = _to_string(-obj_const)))
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
# We elect to write out F-in-Interval constraints in terms of LessThan (L)
# constraints with a range shift. The RHS term is set to the upper bound, and
# the RANGE term to upper - lower.
# ==============================================================================

function _write_ranges(io::IO, model::Model{T}, ::Type{F}) where {T,F}
    cis = MOI.get(model, MOI.ListOfConstraintIndices{F,MOI.Interval{T}}())
    for index in cis
        set = MOI.get(model, MOI.ConstraintSet(), index)::MOI.Interval{T}
        if isfinite(set.upper - set.lower)
            # We only need to write the range if the bounds are both finite
            row_name = MOI.get(model, MOI.ConstraintName(), index)
            range = _to_string(set.upper - set.lower)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = range))
        end
    end
    return
end

function write_ranges(io::IO, model::Model{T}) where {T}
    println(io, "RANGES")
    _write_ranges(io, model, MOI.ScalarAffineFunction{T})
    _write_ranges(io, model, MOI.ScalarQuadraticFunction{T})
    return
end

# ==============================================================================
#   BOUNDS
#
# Variables default to [0, ∞), or [0, 1] if the variable appears in INTORG and
# does not appear in BOUNDS.
#
#     FX    fixed variable     x == b
#     FR    free variable      -∞ < x < ∞
#     MI    lower bound -inf   -∞ < x
#     LO    lower bound        b <= x
#     LI    integer variable   b <= x
#     PL    upper bound +inf        x < ∞
#     UP    upper bound             x <= b
#     UI    integer variable        x <= b
#     BV    binary variable    x = 0 or 1
#
#  Not yet implemented:
#
#     SC    semi-cont variable x = 0 or l <= x <= b
#           l is the lower bound on the variable. If none set then defaults to 1
# ==============================================================================

function write_single_bound(
    io::IO,
    var_name::String,
    lower::T,
    upper::T,
    vtype,
) where {T}
    if lower == upper
        println(
            io,
            Card(
                f1 = "FX",
                f2 = "bounds",
                f3 = var_name,
                f4 = _to_string(lower),
            ),
        )
    elseif lower == typemin(T) && upper == typemax(T)
        println(io, Card(f1 = "FR", f2 = "bounds", f3 = var_name))
    else
        if lower == typemin(T)
            println(io, Card(f1 = "MI", f2 = "bounds", f3 = var_name))
        else
            println(
                io,
                Card(
                    f1 = vtype == VTYPE_CONTINUOUS ? "LO" : "LI",
                    f2 = "bounds",
                    f3 = var_name,
                    f4 = _to_string(lower),
                ),
            )
        end
        if upper == typemax(T)
            println(io, Card(f1 = "PL", f2 = "bounds", f3 = var_name))
        else
            println(
                io,
                Card(
                    f1 = vtype == VTYPE_CONTINUOUS ? "UP" : "UI",
                    f2 = "bounds",
                    f3 = var_name,
                    f4 = _to_string(upper),
                ),
            )
        end
    end
    return
end

update_bounds(x, set::MOI.GreaterThan) = (max(x[1], set.lower), x[2], x[3])

update_bounds(x, set::MOI.LessThan) = (x[1], min(x[2], set.upper), x[3])

update_bounds(x, set::MOI.Interval) = (set.lower, set.upper, x[3])

update_bounds(x, set::MOI.EqualTo) = (set.value, set.value, x[3])

update_bounds(x, set::MOI.ZeroOne) = (x[1], x[2], VTYPE_BINARY)

function _collect_bounds(bounds, model, ::Type{S}, var_to_column) where {S}
    for index in
        MOI.get(model, MOI.ListOfConstraintIndices{MOI.VariableIndex,S}())
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)::S
        column = var_to_column[func]
        bounds[column] = update_bounds(bounds[column], set)
    end
    return
end

function write_bounds(io::IO, model::Model{T}, var_to_column) where {T}
    options = get_options(model)
    println(io, "BOUNDS")
    bounds = [
        (typemin(T), typemax(T), VTYPE_CONTINUOUS) for
        _ in 1:length(var_to_column)
    ]
    _collect_bounds(bounds, model, MOI.LessThan{T}, var_to_column)
    _collect_bounds(bounds, model, MOI.GreaterThan{T}, var_to_column)
    _collect_bounds(bounds, model, MOI.EqualTo{T}, var_to_column)
    _collect_bounds(bounds, model, MOI.Interval{T}, var_to_column)
    _collect_bounds(bounds, model, MOI.ZeroOne, var_to_column)
    for (variable, column) in var_to_column
        var_name = _var_name(model, variable, column, options.generic_names)
        lower, upper, vtype = bounds[column]
        if vtype == VTYPE_BINARY
            if lower <= 0 && upper >= 1
                println(io, Card(f1 = "BV", f2 = "bounds", f3 = var_name))
            else
                lower = max(0, lower)
                if lower > 0
                    lower = one(T)
                end
                upper = min(1, upper)
                if upper < 1
                    upper = zero(T)
                end
                write_single_bound(io, var_name, lower, upper, vtype)
            end
        else
            write_single_bound(io, var_name, lower, upper, vtype)
        end
    end
    return
end

# ==============================================================================
#   QUADRATIC OBJECTIVE
# ==============================================================================

function write_quadobj(io::IO, model::Model, flip_obj::Bool, var_to_column)
    f = _get_objective(model)
    if isempty(f.quadratic_terms)
        return
    end
    options = get_options(model)
    # Here we always write out QUADOBJ sections for the quadratic objective. All
    # solvers can read these, even if CPLEX writes QMATRIX by default and Mosek
    # writes QSECTION OBJ.
    println(io, "QUADOBJ")
    _write_q_matrix(
        io,
        model,
        f,
        var_to_column;
        flip_coef = flip_obj,
        generic_names = options.generic_names,
        # In QUADOBJ, we need only to specific the ij term:
        include_ij_and_ji = false,
        # And all solvers interpret QUADOBJ to include /2:
        include_div_2 = true,
    )
    return
end

function _write_q_matrix(
    io::IO,
    model::Model{T},
    f::MOI.ScalarQuadraticFunction,
    var_to_column;
    flip_coef::Bool,
    generic_names::Bool,
    include_ij_and_ji::Bool,
    include_div_2::Bool,
) where {T}
    terms = Dict{Tuple{MOI.VariableIndex,MOI.VariableIndex},T}()
    scale = flip_coef ? -one(T) : one(T)
    if !include_div_2
        scale /= 2
    end
    for term in f.quadratic_terms
        x = term.variable_1
        y = term.variable_2
        if var_to_column[x] > var_to_column[y]
            x, y = y, x
        end
        if haskey(terms, (x, y))
            terms[(x, y)] += term.coefficient
        else
            terms[(x, y)] = term.coefficient
        end
    end
    # Use sort for reproducibility, and so the Q matrix is given in order.
    for (x, y) in sort!(
        collect(keys(terms)),
        by = ((x, y),) -> (var_to_column[x], var_to_column[y]),
    )
        x_name = _var_name(model, x, var_to_column[x], generic_names)
        y_name = _var_name(model, y, var_to_column[y], generic_names)
        coef = scale * terms[(x, y)]
        println(io, Card(f2 = x_name, f3 = y_name, f4 = _to_string(coef)))
        if x != y && include_ij_and_ji
            println(io, Card(f2 = y_name, f3 = x_name, f4 = _to_string(coef)))
        end
    end
    return
end

# ==============================================================================
#   QUADRATIC CONSTRAINTS
# ==============================================================================

function write_quadcons(io::IO, model::Model{T}, var_to_column) where {T}
    options = get_options(model)
    F = MOI.ScalarQuadraticFunction{T}
    for S in
        (MOI.LessThan{T}, MOI.GreaterThan{T}, MOI.EqualTo{T}, MOI.Interval{T})
        for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            name = MOI.get(model, MOI.ConstraintName(), ci)
            println(io, "QCMATRIX   $name")
            f = MOI.get(model, MOI.ConstraintFunction(), ci)
            _write_q_matrix(
                io,
                model,
                f,
                var_to_column;
                generic_names = options.generic_names,
                # flip_coef is needed only for maximization objectives
                flip_coef = false,
                # All solvers interpret QCMATRIX to require both (i,j) and (j,i)
                # terms.
                include_ij_and_ji = true,
                # In Gurobi's QCMATRIX there is no factor of /2. This is
                # different to both CPLEX and Mosek.
                include_div_2 = options.quadratic_format !=
                                kQuadraticFormatGurobi,
            )
        end
    end
    return
end

# ==============================================================================
#   SOS
# ==============================================================================

function write_sos_constraint(io::IO, model::Model, index, var_to_column)
    options = get_options(model)
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    for (variable, weight) in zip(func.variables, set.weights)
        var_name = _var_name(
            model,
            variable,
            var_to_column[variable],
            options.generic_names,
        )
        println(io, Card(f2 = var_name, f3 = _to_string(weight)))
    end
end

function write_sos(io::IO, model::Model{T}, var_to_column) where {T}
    sos1_indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SOS1{T}}(),
    )
    sos2_indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SOS2{T}}(),
    )
    if length(sos1_indices) + length(sos2_indices) > 0
        println(io, "SOS")
        idx = 1
        for (sos_type, indices) in enumerate([sos1_indices, sos2_indices])
            for index in indices
                println(io, Card(f1 = "S$(sos_type)", f2 = "SOS$(idx)"))
                write_sos_constraint(io, model, index, var_to_column)
                idx += 1
            end
        end
    end
    return
end

# ==============================================================================
#   INDICATORS
# ==============================================================================

function write_indicators(io::IO, indicators)
    if isempty(indicators)
        return
    end
    println(io, "INDICATORS")
    for (row, var, condition) in indicators
        if condition == MOI.ACTIVATE_ON_ONE
            println(io, Card(f1 = "IF", f2 = row, f3 = var, f4 = "1"))
        else
            println(io, Card(f1 = "IF", f2 = row, f3 = var, f4 = "0"))
        end
    end
    return
end
