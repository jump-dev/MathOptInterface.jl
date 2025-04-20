# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MPS

import ..FileFormats

import MathOptInterface as MOI
import DataStructures: OrderedDict

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

const IndicatorLessThanTrue{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}

const IndicatorGreaterThanTrue{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}}

const IndicatorEqualToTrue{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{T}}

const IndicatorLessThanFalse{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{T}}

const IndicatorGreaterThanFalse{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.GreaterThan{T}}

const IndicatorEqualToFalse{T} =
    MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.EqualTo{T}}

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (
        MOI.SOS1,
        MOI.SOS2,
        IndicatorLessThanTrue,
        IndicatorLessThanFalse,
        IndicatorGreaterThanTrue,
        IndicatorGreaterThanFalse,
        IndicatorEqualToTrue,
        IndicatorEqualToFalse,
    ),
    (),
    (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{<:Union{MOI.SOS1{T},MOI.SOS2{T}}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{<:Union{MOI.Parameter,MOI.Semicontinuous,MOI.Semiinteger}},
)
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorOfVariables},
    ::Type{
        <:Union{
            IndicatorLessThanTrue{T},
            IndicatorLessThanFalse{T},
            IndicatorGreaterThanTrue{T},
            IndicatorGreaterThanFalse{T},
            IndicatorEqualToTrue{T},
            IndicatorEqualToFalse{T},
        },
    },
) where {T}
    return false
end

@enum(
    QuadraticFormat,
    kQuadraticFormatCPLEX,
    kQuadraticFormatGurobi,
    kQuadraticFormatMosek,
)

struct Options
    warn::Bool
    objsense::Bool
    generic_names::Bool
    quadratic_format::QuadraticFormat
end

function get_options(m::Model)::Options
    return get(
        m.ext,
        :MPS_OPTIONS,
        Options(false, false, false, kQuadraticFormatGurobi),
    )
end

"""
    Model(; kwargs...)

Create an empty instance of FileFormats.MPS.Model.

Keyword arguments are:

 - `warn::Bool=false`: print a warning when variables or constraints are renamed.
 - `print_objsense::Bool=false`: print the OBJSENSE section when writing
 - `generic_names::Bool=false`: strip all names in the model and replace them
   with the generic names `C\$i` and `R\$i` for the i'th column and row
   respectively.
 - `quadratic_format::QuadraticFormat = kQuadraticFormatGurobi`: specify the
   solver-specific extension used when writing the quadratic components of the
   model. Options are `kQuadraticFormatGurobi`, `kQuadraticFormatCPLEX`, and
   `kQuadraticFormatMosek`.
"""
function Model(;
    warn::Bool = false,
    print_objsense::Bool = false,
    generic_names::Bool = false,
    quadratic_format::QuadraticFormat = kQuadraticFormatGurobi,
)
    model = Model{Float64}()
    model.ext[:MPS_OPTIONS] =
        Options(warn, print_objsense, generic_names, quadratic_format)
    return model
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.MPS.Model")

@enum(VType, VTYPE_CONTINUOUS, VTYPE_INTEGER, VTYPE_BINARY)

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
    model,
    ::Type{F},
    ::Type{S},
    ::Any,
) where {F,S<:MOI.Interval{Float64}}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        if set.lower == -Inf && set.upper == Inf
            println(io, Card(f1 = "N", f2 = row_name))
        elseif set.upper == Inf
            println(io, Card(f1 = "G", f2 = row_name))
        elseif set.lower == -Inf
            println(io, Card(f1 = "L", f2 = row_name))
        else
            println(io, Card(f1 = "L", f2 = row_name))
        end
    end
    return
end

function write_rows(io::IO, model::Model)
    println(io, "ROWS")
    println(io, Card(f1 = "N", f2 = "OBJ"))
    SAF = MOI.ScalarAffineFunction{Float64}
    SQF = MOI.ScalarQuadraticFunction{Float64}
    _write_rows(io, model, SAF, MOI.LessThan{Float64}, "L")
    _write_rows(io, model, SQF, MOI.LessThan{Float64}, "L")
    _write_rows(io, model, SAF, MOI.GreaterThan{Float64}, "G")
    _write_rows(io, model, SQF, MOI.GreaterThan{Float64}, "G")
    _write_rows(io, model, SAF, MOI.EqualTo{Float64}, "E")
    _write_rows(io, model, SQF, MOI.EqualTo{Float64}, "E")
    _write_rows(io, model, SAF, MOI.Interval{Float64}, "L")
    _write_rows(io, model, SQF, MOI.Interval{Float64}, "L")
    VAF = MOI.VectorAffineFunction{Float64}
    _write_rows(io, model, VAF, IndicatorLessThanTrue{Float64}, "L")
    _write_rows(io, model, VAF, IndicatorLessThanFalse{Float64}, "L")
    _write_rows(io, model, VAF, IndicatorGreaterThanTrue{Float64}, "G")
    _write_rows(io, model, VAF, IndicatorGreaterThanFalse{Float64}, "G")
    _write_rows(io, model, VAF, IndicatorEqualToTrue{Float64}, "E")
    _write_rows(io, model, VAF, IndicatorEqualToFalse{Float64}, "E")
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
    coefficients::Vector{Vector{Tuple{String,Float64}}},
    row_name::String,
    func::MOI.ScalarAffineFunction,
    flip_sign::Bool = false,
)
    for term in func.terms
        column = var_to_column[term.variable]
        coef = flip_sign ? -term.coefficient : term.coefficient
        push!(coefficients[column], (row_name, coef))
    end
    return
end

function _extract_terms(
    var_to_column::OrderedDict{MOI.VariableIndex,Int},
    coefficients::Vector{Vector{Tuple{String,Float64}}},
    row_name::String,
    func::MOI.ScalarQuadraticFunction,
    flip_sign::Bool = false,
)
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
    coefficients::Vector{Vector{Tuple{String,Float64}}},
) where {F,S}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        func = MOI.get(model, MOI.ConstraintFunction(), index)
        _extract_terms(var_to_column, coefficients, row_name, func)
    end
    return
end

_activation_condition(::Type{<:MOI.Indicator{A}}) where {A} = A

function _collect_indicator(
    model,
    ::Type{S},
    var_to_column,
    coefficients,
    indicators,
) where {S}
    options = get_options(model)
    F = MOI.VectorAffineFunction{Float64}
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

function _get_objective(model)::MOI.ScalarQuadraticFunction{Float64}
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    return MOI.get(model, MOI.ObjectiveFunction{F}())
end

function _extract_terms_objective(model, var_to_column, coefficients, flip_obj)
    obj_func = _get_objective(model)
    _extract_terms(var_to_column, coefficients, "OBJ", obj_func, flip_obj)
    return obj_func.constant
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

function write_columns(io::IO, model::Model, flip_obj, var_to_column)
    options = get_options(model)
    indicators = Tuple{String,String,MOI.ActivationCondition}[]
    coefficients = Vector{Tuple{String,Float64}}[
        Tuple{String,Float64}[] for _ in 1:length(var_to_column)
    ]
    # Build constraint coefficients
    # The functions and sets are given explicitly so that this function is
    # type-stable.
    SAF = MOI.ScalarAffineFunction{Float64}
    SQF = MOI.ScalarQuadraticFunction{Float64}
    LT, GT = MOI.LessThan{Float64}, MOI.GreaterThan{Float64}
    ET, IT = MOI.EqualTo{Float64}, MOI.Interval{Float64}
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
        IndicatorLessThanTrue{Float64},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorLessThanFalse{Float64},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorGreaterThanTrue{Float64},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorGreaterThanFalse{Float64},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorEqualToTrue{Float64},
        var_to_column,
        coefficients,
        indicators,
    )
    _collect_indicator(
        model,
        IndicatorEqualToFalse{Float64},
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
    model,
    ::Type{F},
    ::Type{S},
) where {F,S<:MOI.Interval{Float64}}
    for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        row_name = MOI.get(model, MOI.ConstraintName(), index)
        set = MOI.get(model, MOI.ConstraintSet(), index)
        if set.lower == -Inf && set.upper == Inf
            # No RHS. Free row
        elseif set.upper == Inf
            value = _to_string(set.lower)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = value))
        elseif set.lower == -Inf
            value = _to_string(set.upper)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = value))
        else
            value = _to_string(set.upper)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = value))
        end
    end
    return
end

function write_rhs(io::IO, model::Model, obj_const)
    println(io, "RHS")
    SAF = MOI.ScalarAffineFunction{Float64}
    SQF = MOI.ScalarQuadraticFunction{Float64}
    _write_rhs(io, model, SAF, MOI.LessThan{Float64})
    _write_rhs(io, model, SQF, MOI.LessThan{Float64})
    _write_rhs(io, model, SAF, MOI.GreaterThan{Float64})
    _write_rhs(io, model, SQF, MOI.GreaterThan{Float64})
    _write_rhs(io, model, SAF, MOI.EqualTo{Float64})
    _write_rhs(io, model, SQF, MOI.EqualTo{Float64})
    _write_rhs(io, model, SAF, MOI.Interval{Float64})
    _write_rhs(io, model, SQF, MOI.Interval{Float64})
    VAF = MOI.VectorAffineFunction{Float64}
    _write_rhs(io, model, VAF, IndicatorLessThanTrue{Float64})
    _write_rhs(io, model, VAF, IndicatorLessThanFalse{Float64})
    _write_rhs(io, model, VAF, IndicatorGreaterThanTrue{Float64})
    _write_rhs(io, model, VAF, IndicatorGreaterThanFalse{Float64})
    _write_rhs(io, model, VAF, IndicatorEqualToTrue{Float64})
    _write_rhs(io, model, VAF, IndicatorEqualToFalse{Float64})
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

function _write_ranges(io::IO, model::Model, ::Type{F}) where {F}
    cis = MOI.get(model, MOI.ListOfConstraintIndices{F,MOI.Interval{Float64}}())
    for index in cis
        set = MOI.get(model, MOI.ConstraintSet(), index)::MOI.Interval{Float64}
        if isfinite(set.upper - set.lower)
            # We only need to write the range if the bounds are both finite
            row_name = MOI.get(model, MOI.ConstraintName(), index)
            range = _to_string(set.upper - set.lower)
            println(io, Card(f2 = "rhs", f3 = row_name, f4 = range))
        end
    end
    return
end

function write_ranges(io::IO, model::Model)
    println(io, "RANGES")
    _write_ranges(io, model, MOI.ScalarAffineFunction{Float64})
    _write_ranges(io, model, MOI.ScalarQuadraticFunction{Float64})
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

function write_single_bound(io::IO, var_name::String, lower, upper, vtype)
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
    elseif lower == -Inf && upper == Inf
        println(io, Card(f1 = "FR", f2 = "bounds", f3 = var_name))
    else
        if lower == -Inf
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
        if upper == Inf
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

function update_bounds(x::Tuple{Float64,Float64,VType}, set::MOI.GreaterThan)
    return (max(x[1], set.lower), x[2], x[3])
end

function update_bounds(x::Tuple{Float64,Float64,VType}, set::MOI.LessThan)
    return (x[1], min(x[2], set.upper), x[3])
end

function update_bounds(x::Tuple{Float64,Float64,VType}, set::MOI.Interval)
    return (set.lower, set.upper, x[3])
end

function update_bounds(x::Tuple{Float64,Float64,VType}, set::MOI.EqualTo)
    return (set.value, set.value, x[3])
end

function update_bounds(x::Tuple{Float64,Float64,VType}, set::MOI.ZeroOne)
    return (x[1], x[2], VTYPE_BINARY)
end

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

function write_bounds(io::IO, model::Model, var_to_column)
    options = get_options(model)
    println(io, "BOUNDS")
    bounds = [(-Inf, Inf, VTYPE_CONTINUOUS) for _ in 1:length(var_to_column)]
    _collect_bounds(bounds, model, MOI.LessThan{Float64}, var_to_column)
    _collect_bounds(bounds, model, MOI.GreaterThan{Float64}, var_to_column)
    _collect_bounds(bounds, model, MOI.EqualTo{Float64}, var_to_column)
    _collect_bounds(bounds, model, MOI.Interval{Float64}, var_to_column)
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
                    lower = 1
                end
                upper = min(1, upper)
                if upper < 1
                    upper = 0
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
    model::Model,
    f::MOI.ScalarQuadraticFunction,
    var_to_column;
    flip_coef::Bool,
    generic_names::Bool,
    include_ij_and_ji::Bool,
    include_div_2::Bool,
)
    terms = Dict{Tuple{MOI.VariableIndex,MOI.VariableIndex},Float64}()
    scale = flip_coef ? -1.0 : 1.0
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

function write_quadcons(io::IO, model::Model, var_to_column)
    options = get_options(model)
    F = MOI.ScalarQuadraticFunction{Float64}
    for S in (
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    )
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

function write_sos(io::IO, model::Model, var_to_column)
    sos1_indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SOS1{Float64}}(),
    )
    sos2_indices = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VectorOfVariables,MOI.SOS2{Float64}}(),
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

struct _SOSConstraint
    type::Int
    weights::Vector{Float64}
    columns::Vector{String}
end

mutable struct TempMPSModel
    name::String
    is_minimization::Bool
    obj_name::String
    c::Vector{Float64}
    obj_constant::Float64
    col_lower::Vector{Float64}
    col_upper::Vector{Float64}
    col_bounds_default::Vector{Bool}
    row_lower::Vector{Float64}
    row_upper::Vector{Float64}
    sense::Vector{Sense}
    A::Vector{Vector{Tuple{Int,Float64}}}
    vtype::Vector{VType}
    name_to_col::Dict{String,Int}
    col_to_name::Vector{String}
    name_to_row::Dict{String,Int}
    row_to_name::Vector{String}
    intorg_flag::Bool  # A flag used to parse COLUMNS section.
    sos_constraints::Vector{_SOSConstraint}
    quad_obj::Vector{Tuple{String,String,Float64}}
    qc_matrix::Dict{String,Vector{Tuple{String,String,Float64}}}
    current_qc_matrix::String
    indicators::Dict{String,Tuple{String,MOI.ActivationCondition}}
end

function TempMPSModel()
    return TempMPSModel(
        "",
        true,
        "",
        Float64[],  # c
        0.0,        # obj_constant
        Float64[],  # col_lower
        Float64[],  # col_upper
        Bool[],     # col_bounds_default
        Float64[],  # row_lower
        Float64[],  # row_upper
        Sense[],   # sense
        Vector{Tuple{Int,Float64}}[],  # A
        VType[],
        Dict{String,Int}(),
        String[],
        Dict{String,Int}(),
        String[],
        false,
        _SOSConstraint[],
        Tuple{String,String,Float64}[],
        Dict{String,Vector{Tuple{String,String,Float64}}}(),
        "",
        Dict{String,Tuple{String,MOI.ActivationCondition}}(),
    )
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
function Headers(s::AbstractString)
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
    elseif N == 8
        if (x == 'O' || x == 'o') && startswith(uppercase(s), "OBJSENSE")
            return HEADER_OBJSENSE
        end
    elseif N == 10
        if (x == 'I' || x == 'i') && uppercase(s) == "INDICATORS"
            return HEADER_INDICATORS
        end
    elseif N >= 12
        if (x == 'O' || x == 'o') && startswith(uppercase(s), "OBJSENSE")
            return HEADER_OBJSENSE
        elseif (x == 'Q' || x == 'q')
            header = uppercase(s)
            if startswith(header, "QCMATRIX")
                return HEADER_QCMATRIX
            elseif startswith(header, "QSECTION")
                return HEADER_QSECTION
            end
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
    while !eof(io) && header != HEADER_ENDATA
        raw_line = readline(io)
        if startswith(raw_line, '*')
            continue  # Lines starting with `*` are comments
        end
        line = string(strip(raw_line))
        if isempty(line)
            continue  # Skip blank lines
        end
        h = Headers(line)
        if h == HEADER_OBJSENSE
            items = line_to_items(line)
            if length(items) == 2
                sense = uppercase(items[2])
                @assert sense == "MAX" || sense == "MIN"
                data.is_minimization = sense == "MIN"
            else
                header = HEADER_OBJSENSE
            end
            continue
        elseif h == HEADER_QCMATRIX || h == HEADER_QSECTION
            items = line_to_items(line)
            @assert length(items) == 2
            data.current_qc_matrix = String(items[2])
            header = h
            data.qc_matrix[data.current_qc_matrix] =
                Tuple{String,String,Float64}[]
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
            @assert length(items) == 1
            sense = uppercase(items[1])
            @assert sense == "MAX" || sense == "MIN"
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
            @assert header == HEADER_ENDATA
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
    variable_map = Dict{String,MOI.VariableIndex}()
    for (i, name) in enumerate(data.col_to_name)
        _add_variable(model, data, variable_map, i, name)
    end
    _add_objective(model, data, variable_map)
    for (j, c_name) in enumerate(data.row_to_name)
        set = bounds_to_set(data.row_lower[j], data.row_upper[j])
        if set === nothing
            free_set = MOI.Interval(-Inf, Inf)
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

function _add_variable(model, data, variable_map, i, name)
    x = MOI.add_variable(model)
    variable_map[name] = x
    MOI.set(model, MOI.VariableName(), x, name)
    set = bounds_to_set(data.col_lower[i], data.col_upper[i])
    if set isa MOI.Interval
        # Do not add MOI.Interval constraints because we want to follow JuMP's
        # convention of adding separate lower and upper bounds.
        MOI.add_constraint(model, x, MOI.GreaterThan(set.lower::Float64))
        MOI.add_constraint(model, x, MOI.LessThan(set.upper::Float64))
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

function _add_objective(model, data, variable_map)
    if data.is_minimization
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    else
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    end
    affine_terms = MOI.ScalarAffineTerm{Float64}[
        MOI.ScalarAffineTerm(data.c[i], variable_map[v]) for
        (i, v) in enumerate(data.col_to_name) if !iszero(data.c[i])
    ]
    q_terms = MOI.ScalarQuadraticTerm{Float64}[]
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

function _add_indicator_constraint(model, data, variable_map, j, c_name, set)
    z, activate = data.indicators[c_name]
    terms = MOI.VectorAffineTerm{Float64}[MOI.VectorAffineTerm(
        1,
        MOI.ScalarAffineTerm(1.0, variable_map[z]),
    ),]
    for (i, coef) in data.A[j]
        scalar = MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]])
        push!(terms, MOI.VectorAffineTerm(2, scalar))
    end
    f = MOI.VectorAffineFunction(terms, [0.0, 0.0])
    c = MOI.add_constraint(model, f, MOI.Indicator{activate}(set))
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

function _add_linear_constraint(model, data, variable_map, j, c_name, set)
    terms = MOI.ScalarAffineTerm{Float64}[
        MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]]) for
        (i, coef) in data.A[j]
    ]
    c = MOI.add_constraint(model, MOI.ScalarAffineFunction(terms, 0.0), set)
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

function _add_quad_constraint(model, data, variable_map, j, c_name, set)
    aff_terms = MOI.ScalarAffineTerm{Float64}[
        MOI.ScalarAffineTerm(coef, variable_map[data.col_to_name[i]]) for
        (i, coef) in data.A[j]
    ]
    quad_terms = MOI.ScalarQuadraticTerm{Float64}[]
    options = get_options(model)
    scale = if options.quadratic_format == kQuadraticFormatGurobi
        # Gurobi does NOT have a /2 as part of the quadratic matrix. Why oh why
        # would you break precedent with all other formats.
        2.0
    else
        @assert in(
            options.quadratic_format,
            (kQuadraticFormatCPLEX, kQuadraticFormatMosek),
        )
        1.0
    end
    for (x_name, y_name, q) in data.qc_matrix[c_name]
        x, y = variable_map[x_name], variable_map[y_name]
        push!(quad_terms, MOI.ScalarQuadraticTerm(scale * q, x, y))
    end
    f = MOI.ScalarQuadraticFunction(quad_terms, aff_terms, 0.0)
    c = MOI.add_constraint(model, f, set)
    MOI.set(model, MOI.ConstraintName(), c, c_name)
    return
end

# ==============================================================================
#   NAME
# ==============================================================================

function parse_name_line(data::TempMPSModel, line::String)
    m = match(r"^\s*NAME(.*)"i, line)
    if m === nothing
        error("Malformed NAME line: ", line)
    end
    data.name = strip(m[1]::AbstractString)
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
        if data.obj_name == ""
            # The first N row is the objective
            data.obj_name = name
            return
        end
    end
    if name == data.obj_name
        error("Found row with same name as objective: $(join(items, " ")).")
    end
    # Add some default bounds for the constraints.
    push!(data.row_to_name, name)
    row = length(data.row_to_name)
    data.name_to_row[name] = row
    push!(data.sense, sense)
    push!(data.A, Tuple{Int,Float64}[])
    if sense == SENSE_G
        push!(data.row_lower, 0.0)
        push!(data.row_upper, Inf)
        data.row_upper[row] = Inf
    elseif sense == SENSE_L
        push!(data.row_lower, -Inf)
        push!(data.row_upper, 0.0)
    elseif sense == SENSE_E
        push!(data.row_lower, 0.0)
        push!(data.row_upper, 0.0)
    else
        @assert sense == SENSE_N
        push!(data.row_lower, -Inf)
        push!(data.row_upper, Inf)
    end
    return
end

# ==============================================================================
#   COLUMNS
# ==============================================================================

function parse_single_coefficient(
    data,
    row_name::String,
    column::Int,
    value::Float64,
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
    push!(data.col_bounds_default, true)
    push!(data.vtype, VTYPE_CONTINUOUS)
    return
end

function _set_intorg(data, column, column_name)
    if data.intorg_flag
        data.vtype[column] = VTYPE_INTEGER
        # The default upper bound for variables in INTORG is `1`, not `Inf`...
        data.col_upper[column] = 1.0
    elseif data.vtype[column] != VTYPE_CONTINUOUS
        error(
            "Variable $(column_name) appeared in COLUMNS outside an " *
            "`INT` marker after already being declared as integer.",
        )
    end
    return
end

function parse_columns_line(data::TempMPSModel, items::Vector{String})
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
        parse_single_coefficient(data, row_name, column, parse(Float64, value))
        _set_intorg(data, column, column_name)
    elseif length(items) == 5
        # [column name] [row name] [value] [row name 2] [value 2]
        column_name, row_name_1, value_1, row_name_2, value_2 = items
        _add_new_column(data, column_name)
        column = data.name_to_col[column_name]
        parse_single_coefficient(
            data,
            row_name_1,
            column,
            parse(Float64, value_1),
        )
        parse_single_coefficient(
            data,
            row_name_2,
            column,
            parse(Float64, value_2),
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
    data,
    row_name::String,
    value::Float64,
    items::Vector{String},
)
    if row_name == data.obj_name
        data.obj_constant = value
        return
    end
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
        error("Cannot have RHS for free row: $(join(items, " "))")
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
    if data.col_bounds_default[col] && data.vtype[col] == VTYPE_INTEGER
        # This column was part of an INTORG...INTEND block, so it gets a default
        # bound of [0, 1]. However, since it now has a bound, it reverts to a
        # default of [0, inf).
        data.col_upper[col] = Inf
    end
    data.col_bounds_default[col] = false
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
    data,
    column_name::String,
    bound_type::String,
    value::Float64,
)
    col = get(data.name_to_col, column_name, nothing)
    if col === nothing
        error("Column name $(column_name) not found.")
    end
    if data.col_bounds_default[col] && data.vtype[col] == VTYPE_INTEGER
        # This column was part of an INTORG...INTEND block, so it gets a default
        # bound of [0, 1]. However, since it now has a bound, it reverts to a
        # default of [0, inf).
        data.col_upper[col] = Inf
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
        _parse_single_bound(
            data,
            column_name,
            bound_type,
            parse(Float64, value),
        )
    else
        error("Malformed BOUNDS line: $(join(items, " "))")
    end
    return
end

# ==============================================================================
#   SOS
# ==============================================================================

function parse_sos_line(data, items)
    if length(items) != 2
        error("Malformed SOS line: $(join(items, " "))")
    elseif items[1] == "S1"
        push!(data.sos_constraints, _SOSConstraint(1, Float64[], String[]))
    elseif items[1] == "S2"
        push!(data.sos_constraints, _SOSConstraint(2, Float64[], String[]))
    else
        sos = data.sos_constraints[end]
        push!(sos.columns, items[1])
        push!(sos.weights, parse(Float64, items[2]))
    end
    return
end

# ==============================================================================
#   QUADOBJ
# ==============================================================================

function parse_quadobj_line(data, items)
    if length(items) != 3
        error("Malformed QUADOBJ line: $(join(items, " "))")
    end
    push!(data.quad_obj, (items[1], items[2], parse(Float64, items[3])))
    return
end

# ==============================================================================
#   QMATRIX
# ==============================================================================

function parse_qmatrix_line(data, items)
    if length(items) != 3
        error("Malformed QMATRIX line: $(join(items, " "))")
    end
    if data.name_to_col[items[1]] <= data.name_to_col[items[2]]
        # Off-diagonals have duplicate entries. We don't need to store both
        # triangles.
        push!(data.quad_obj, (items[1], items[2], parse(Float64, items[3])))
    end
    return
end

# ==============================================================================
#   QMATRIX
# ==============================================================================

function parse_qcmatrix_line(data, items)
    if length(items) != 3
        error("Malformed QCMATRIX line: $(join(items, " "))")
    end
    if data.name_to_col[items[1]] <= data.name_to_col[items[2]]
        # Off-diagonals have duplicate entries. We don't need to store both
        # triangles.
        push!(
            data.qc_matrix[data.current_qc_matrix],
            (items[1], items[2], parse(Float64, items[3])),
        )
    end
    return
end

# ==============================================================================
#   QSECTION
# ==============================================================================

function parse_qsection_line(data, items)
    if length(items) != 3
        error("Malformed QSECTION line: $(join(items, " "))")
    end
    if data.current_qc_matrix == "OBJ"
        push!(data.quad_obj, (items[1], items[2], parse(Float64, items[3])))
    else
        push!(
            data.qc_matrix[data.current_qc_matrix],
            (items[1], items[2], parse(Float64, items[3])),
        )
    end
    return
end

# ==============================================================================
#   INDICATORS
# ==============================================================================

function parse_indicators_line(data, items)
    if length(items) != 4
        error("Malformed INDICATORS line: $(join(items, " "))")
    end
    condition = if items[4] == "0"
        MOI.ACTIVATE_ON_ZERO
    else
        @assert items[4] == "1"
        MOI.ACTIVATE_ON_ONE
    end
    data.indicators[items[2]] = (items[3], condition)
    return
end

import PrecompileTools

PrecompileTools.@setup_workload begin
    PrecompileTools.@compile_workload begin
        let
            model = Model()
            x = MOI.add_variables(model, 4)
            for i in 1:4
                MOI.set(model, MOI.VariableName(), x[i], "x[$i]")
            end
            MOI.add_constraint(model, x[1], MOI.LessThan(1.0))
            MOI.add_constraint(model, x[2], MOI.GreaterThan(1.0))
            MOI.add_constraint(model, x[3], MOI.EqualTo(1.2))
            MOI.add_constraint(model, x[4], MOI.Interval(-1.0, 100.0))
            MOI.add_constraint(model, x[1], MOI.ZeroOne())
            MOI.add_constraint(model, x[2], MOI.Integer())
            MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
            f = 1.0 * x[1] + 2.0 * x[2] + 3.0
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
            MOI.add_constraint(model, 1.5 * x[1], MOI.LessThan(1.0))
            MOI.add_constraint(model, 1.5 * x[2], MOI.GreaterThan(1.0))
            MOI.add_constraint(model, 1.5 * x[3], MOI.EqualTo(1.2))
            MOI.add_constraint(model, 1.5 * x[4], MOI.Interval(-1.0, 100.0))
            io = IOBuffer()
            write(io, model)
        end
    end
end

# Originally removed by #2421, but this constant was used by extensions like
# BilevelJuMP so I'm re-adding to maintain backwards compatibility.
const SET_TYPES = (
    (MOI.LessThan{Float64}, "L"),
    (MOI.GreaterThan{Float64}, "G"),
    (MOI.EqualTo{Float64}, "E"),
    (MOI.Interval{Float64}, "L"),
)

end
