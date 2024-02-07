# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MPS2

import ..FileFormats

import MathOptInterface as MOI

@enum(
    RType,
    RTYPE_FREE,
    RTYPE_LESS_THAN,
    RTYPE_GREATER_THAN,
    RTYPE_EQUAL_TO,
    RTYPE_INTERVAL,
    RTYPE_LESS_AND_GREATER_THAN,
)

_data(s::MOI.LessThan) = RTYPE_LESS_THAN, s.upper, 0.0
_data(s::MOI.GreaterThan) = RTYPE_GREATER_THAN, s.lower, 0.0
_data(s::MOI.EqualTo) = RTYPE_EQUAL_TO, s.value, 0.0
_data(s::MOI.Interval) = RTYPE_INTERVAL, s.lower, s.upper - s.lower

function _sense(r::RType)
    if r == RTYPE_EQUAL_TO
        return "E"
    elseif r == RTYPE_GREATER_THAN
        return "G"
    elseif r == RTYPE_LESS_THAN
        return "L"
    elseif r == RTYPE_INTERVAL
        return "G"
    else
        error("TODO")
    end
end

function _set(r::RType)
    if r == RTYPE_EQUAL_TO
        return MOI.EqualTo{Float64}
    elseif r == RTYPE_GREATER_THAN
        return MOI.GreaterThan{Float64}
    elseif r == RTYPE_LESS_THAN
        return MOI.LessThan{Float64}
    elseif r == RTYPE_INTERVAL
        return MOI.Interval{Float64}
    else
        error("TODO")
    end
end

_rtype(::Type{<:MOI.LessThan}) = (RTYPE_LESS_THAN, RTYPE_LESS_AND_GREATER_THAN)
function _rtype(::Type{<:MOI.GreaterThan})
    return (RTYPE_GREATER_THAN, RTYPE_LESS_AND_GREATER_THAN)
end
_rtype(::Type{<:MOI.EqualTo}) = (RTYPE_EQUAL_TO,)
_rtype(::Type{<:MOI.Interval}) = (RTYPE_INTERVAL,)

@enum(CType, CTYPE_CONTINUOUS, CTYPE_INTEGER, CTYPE_BINARY)

mutable struct Model <: MOI.ModelLike
    sense::MOI.OptimizationSense
    obj_offset::Float64
    # Columns
    c_name::Vector{String}
    c_objective::Vector{Float64}
    c_lower::Vector{Float64}
    c_upper::Vector{Float64}
    c_bound_type::Vector{RType}
    c_type::Vector{CType}
    c_data::Vector{Vector{Tuple{Int,Float64}}}
    # Rows
    r_name::Vector{String}
    r_b::Vector{Float64}
    r_range::Vector{Float64}
    r_type::Vector{RType}

    function Model()
        return new(
            MOI.FEASIBILITY_SENSE,
            0.0,
            String[],
            Float64[],
            Float64[],
            Float64[],
            RType[],
            CType[],
            Vector{Tuple{Int,Float64}}[],
            String[],
            Float64[],
            Float64[],
            RType[],
        )
    end
end

function MOI.is_empty(model::Model)
    return isempty(model.c_name) && isempty(model.r_name)
end

function MOI.empty!(model::Model)
    model.sense = MOI.FEASIBILITY_SENSE
    model.obj_offset = 0.0
    empty!(model.c_name)
    empty!(model.c_objective)
    empty!(model.c_lower)
    empty!(model.c_upper)
    empty!(model.c_bound_type)
    empty!(model.c_type)
    empty!(model.c_data)
    empty!(model.r_name)
    empty!(model.r_b)
    empty!(model.r_range)
    return
end

function MOI.get(::Model, ::MOI.ListOfModelAttributesSet)
    return [
        MOI.ObjectiveSense(),
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    ]
end

function MOI.get(::Model, ::MOI.ListOfVariableAttributesSet)
    return [MOI.VariableName()]
end

function MOI.get(::Model, ::MOI.ListOfConstraintAttributesSet{F}) where {F}
    return [MOI.ConstraintName()]
end

MOI.get(::Model, ::MOI.ListOfConstraintAttributesSet{MOI.VariableIndex}) = []

function MOI.get(model::Model, ::MOI.ListOfVariableIndices)
    return [MOI.VariableIndex(i) for i in 1:length(model.c_name)]
end

function MOI.get(model::Model, ::MOI.ListOfConstraintTypesPresent)
    s = Set{Any}()
    F = MOI.ScalarAffineFunction{Float64}
    for r in model.r_type
        if r == RTYPE_EQUAL_TO
            push!(s, (F, MOI.EqualTo{Float64}))
        elseif r == RTYPE_LESS_THAN
            push!(s, (F, MOI.LessThan{Float64}))
        elseif r == RTYPE_GREATER_THAN
            push!(s, (F, MOI.GreaterThan{Float64}))
        elseif r == RTYPE_INTERVAL
            push!(s, (F, MOI.Interval{Float64}))
        else
            error("TODO")
        end
    end
    for r in model.c_type
        if r == RTYPE_EQUAL_TO
            push!(s, (MOI.VariableIndex, MOI.EqualTo{Float64}))
        elseif r == RTYPE_LESS_THAN
            push!(s, (MOI.VariableIndex, MOI.LessThan{Float64}))
        elseif r == RTYPE_GREATER_THAN
            push!(s, (MOI.VariableIndex, MOI.GreaterThan{Float64}))
        elseif r == RTYPE_INTERVAL
            push!(s, (MOI.VariableIndex, MOI.Interval{Float64}))
        elseif r == RTYPE_LESS_AND_GREATER_THAN
            push!(s, (MOI.VariableIndex, MOI.GreaterThan{Float64}))
            push!(s, (MOI.VariableIndex, MOI.LessThan{Float64}))
        end
    end
    return collect(s)
end

function MOI.get(
    model::Model,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F<:MOI.ScalarAffineFunction,S}
    ret = MOI.ConstraintIndex{F,S}[]
    for (i, r_type) in enumerate(model.r_type)
        if r_type in _rtype(S)
            push!(ret, MOI.ConstraintIndex{F,S}(i))
        end
    end
    return ret
end

function MOI.get(
    model::Model,
    ::MOI.ListOfConstraintIndices{F,S},
) where {F<:MOI.VariableIndex,S}
    ret = MOI.ConstraintIndex{F,S}[]
    for (i, r_type) in enumerate(model.c_bound_type)
        if r_type in _rtype(S)
            push!(ret, MOI.ConstraintIndex{F,S}(i))
        end
    end
    return ret
end

# add_variable

function MOI.add_variable(model::Model)
    push!(model.c_name, "")
    push!(model.c_objective, 0.0)
    push!(model.c_lower, -Inf)
    push!(model.c_upper, Inf)
    push!(model.c_bound_type, RTYPE_FREE)
    push!(model.c_type, CTYPE_CONTINUOUS)
    push!(model.c_data, Tuple{Int,Float64}[])
    return MOI.VariableIndex(length(model.c_name))
end

# VariableName

MOI.supports(::Model, ::MOI.VariableName, ::Type{MOI.VariableIndex}) = true

function MOI.get(model::Model, ::MOI.VariableName, x::MOI.VariableIndex)
    return model.c_name[x.value]
end

function MOI.set(
    model::Model,
    ::MOI.VariableName,
    x::MOI.VariableIndex,
    name::String,
)
    model.c_name[x.value] = name
    return
end

# TODO(odow): this is slow
function MOI.get(model::Model, ::Type{MOI.VariableIndex}, name::String)
    index = findfirst(==(name), model.c_name)
    if index === nothing
        return nothing
    end
    return MOI.VariableIndex(index)
end

# Bound constraints

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{S},
) where {
    S<:Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
        MOI.ZeroOne,
        MOI.Integer,
    },
}
    return true
end

function MOI.add_constraint(
    model::Model,
    x::MOI.VariableIndex,
    set::MOI.LessThan{Float64},
)
    model.c_upper[x.value] = set.upper
    if model.c_bound_type[x.value] == RTYPE_FREE
        model.c_bound_type[x.value] = RTYPE_LESS_THAN
    elseif model.c_bound_type[x.value] == RTYPE_GREATER_THAN
        model.c_bound_type[x.value] = RTYPE_LESS_AND_GREATER_THAN
    else
        error("TODO")
    end
    F, S = MOI.VariableIndex, MOI.LessThan{Float64}
    return MOI.ConstraintIndex{F,S}(x.value)
end

function MOI.add_constraint(
    model::Model,
    x::MOI.VariableIndex,
    set::MOI.GreaterThan{Float64},
)
    if model.c_bound_type[x.value] == RTYPE_FREE
        model.c_bound_type[x.value] = RTYPE_GREATER_THAN
    elseif model.c_bound_type[x.value] == RTYPE_LESS_THAN
        model.c_bound_type[x.value] = RTYPE_LESS_AND_GREATER_THAN
    else
        error("TODO")
    end
    model.c_lower[x.value] = set.lower
    F, S = MOI.VariableIndex, MOI.GreaterThan{Float64}
    return MOI.ConstraintIndex{F,S}(x.value)
end

function MOI.add_constraint(
    model::Model,
    x::MOI.VariableIndex,
    set::MOI.EqualTo{Float64},
)
    if model.c_bound_type[x.value] == RTYPE_FREE
        model.c_bound_type[x.value] = RTYPE_EQUAL_TO
    else
        error("TODO")
    end
    model.c_lower[x.value] = set.value
    model.c_upper[x.value] = set.value
    F, S = MOI.VariableIndex, MOI.EqualTo{Float64}
    return MOI.ConstraintIndex{F,S}(x.value)
end

function MOI.add_constraint(
    model::Model,
    x::MOI.VariableIndex,
    set::MOI.Interval{Float64},
)
    if model.c_bound_type[x.value] == RTYPE_FREE
        model.c_bound_type[x.value] = RTYPE_INTERVAL
    else
        error("TODO")
    end
    model.c_lower[x.value] = set.lower
    model.c_upper[x.value] = set.upper
    F, S = MOI.VariableIndex, MOI.Interval{Float64}
    return MOI.ConstraintIndex{F,S}(x.value)
end

function MOI.add_constraint(
    model::Model,
    x::MOI.VariableIndex,
    ::MOI.ZeroOne,
)
    model.c_type[x.value] = CTYPE_BINARY
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value)
end

function MOI.add_constraint(
    model::Model,
    x::MOI.VariableIndex,
    ::MOI.Integer,
)
    model.c_type[x.value] = CTYPE_INTEGER
    return MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(x.value)
end

# ObjectiveSense

MOI.supports(::Model, ::MOI.ObjectiveSense) = true

MOI.get(model::Model, ::MOI.ObjectiveSense) = model.sense

function MOI.set(
    model::Model,
    ::MOI.ObjectiveSense,
    sense::MOI.OptimizationSense,
)
    model.sense = sense
    return
end

# ObjectiveFunction

function MOI.get(::Model, ::MOI.ObjectiveFunctionType)
    return MOI.ScalarAffineFunction{Float64}
end

function MOI.supports(
    ::Model,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
)
    return true
end

function MOI.get(
    model::Model,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
)
    terms = MOI.ScalarAffineTerm[]
    for (i, coef) in enumerate(model.c_objective)
        if !iszero(coef)
            push!(terms, MOI.ScalarAffineTerm(coef, MOI.VariableIndex(i)))
        end
    end
    return MOI.ScalarAffineFunction{Float64}(terms, model.obj_offset)
end

function MOI.set(
    model::Model,
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}},
    f::MOI.ScalarAffineFunction{Float64},
)
    model.obj_offset = f.constant
    for term in f.terms
        model.c_objective[term.variable.value] += term.coefficient
    end
    return
end

# Constraints

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.ScalarAffineFunction{Float64}},
    ::Type{S},
) where {
    S<:Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    },
}
    return true
end

function MOI.add_constraint(
    model::Model,
    f::MOI.ScalarAffineFunction{Float64},
    s::S,
) where {
    S<:Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    },
}
    @assert iszero(f.constant)
    r_type, r_b, r_range = _data(s)
    push!(model.r_name, "")
    push!(model.r_b, r_b)
    push!(model.r_range, r_range)
    push!(model.r_type, r_type)
    row = length(model.r_name)
    for term in f.terms
        push!(model.c_data[term.variable.value], (row, term.coefficient))
    end
    return MOI.ConstraintIndex{typeof(f),typeof(s)}(row)
end

function MOI.supports(
    ::Model,
    ::MOI.ConstraintName,
    ::Type{MOI.ConstraintIndex{F,S}},
) where {
    F<:MOI.ScalarAffineFunction{Float64},
    S<:Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    },
}
    return true
end

function MOI.get(
    model::Model,
    ::MOI.ConstraintName,
    ci::MOI.ConstraintIndex{F,S},
) where {
    F<:MOI.ScalarAffineFunction{Float64},
    S<:Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    },
}
    return model.r_name[ci.value]
end

function MOI.set(
    model::Model,
    ::MOI.ConstraintName,
    ci::MOI.ConstraintIndex{F,S},
    name::String,
) where {
    F<:MOI.ScalarAffineFunction{Float64},
    S<:Union{
        MOI.LessThan{Float64},
        MOI.GreaterThan{Float64},
        MOI.EqualTo{Float64},
        MOI.Interval{Float64},
    },
}
    model.r_name[ci.value] = name
    return
end

function MOI.get(
    model::Model,
    ::Type{<:MOI.ConstraintIndex},
    name::String,
)
    index = findfirst(==(name), model.r_name)
    if index === nothing
        return
    end
    S = _set(model.r_type[index])
    F = MOI.ScalarAffineFunction{Float64}
    return MOI.ConstraintIndex{F,S}(index)
end

function MOI.get(
    model::Model,
    ::MOI.ConstraintFunction,
    ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction,S}
) where {S}
    # TODO(odow): this is very slow
    f = MOI.ScalarAffineFunction{Float64}(
        MOI.ScalarAffineTerm{Float64}[],
        0.0
    )
    for (i, c_data) in enumerate(model.c_data)
        for (row, coef) in c_data
            if row == ci.value
                push!(f.terms, MOI.ScalarAffineTerm(coef, MOI.VariableIndex(i)))
            end
        end
    end
    return f
end

function MOI.get(
    model::Model,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction,S}
) where {S}
    return S(model.r_b[ci.value])
end

function MOI.get(
    model::Model,
    ::MOI.ConstraintSet,
    ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction,<:MOI.Interval}
)
    l = model.r_b[ci.value]
    u = l + model.r_range[ci.value]
    return MOI.Interval(l, u)
end

# ==============================================================================
#   Base.write(io, ::Model)
# ==============================================================================

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

const EMPTY_STRING = ""

struct Card
    f1::String
    f2::String
    f3::String
    f4::String
    f5::String

    function Card(;
        f1::String = EMPTY_STRING,
        f2::String = EMPTY_STRING,
        f3::String = EMPTY_STRING,
        f4::String = EMPTY_STRING,
        f5::String = EMPTY_STRING,
    )
        return new(f1, f2, f3, f4, f5)
    end
end

const _INTORG = "    MARKER    'MARKER'                 'INTORG'"
const _INTEND = "    MARKER    'MARKER'                 'INTEND'"

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

function Base.write(io::IO, model::Model)
    # options = get_options(model)
    if false # options.generic_names
        FileFormats.create_generic_names(model)
    else
        FileFormats.create_unique_names(
            model;
            warn = false, # options.warn,
            replacements = Function[s->replace(s, ' ' => '_')],
        )
    end
    println(io, "NAME")
    if model.sense == MOI.MAX_SENSE
        println(io, "OBJSENSE MAX")
    else
        println(io, "OBJSENSE MIN")
    end
    println(io, "ROWS\n N  OBJ")
    for (r_name, r_type) in zip(model.r_name, model.r_type)
        println(io, Card(f1 = _sense(r_type), f2 = r_name))
    end
    println(io, "COLUMNS")
    int_open = false
    for (i, c_name) in enumerate(model.c_name)
        is_int = model.c_type[i] != CTYPE_CONTINUOUS
        if is_int && !int_open
            # println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTORG'"))
            println(io, _INTORG)
            int_open = true
        elseif !is_int && int_open
            println(io, _INTEND)
            # println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTEND'"))
            int_open = false
        end
        if !iszero(model.c_objective[i])
            println(
                io,
                Card(
                    f2 = c_name,
                    f3 = "OBJ",
                    f4 = _to_string(model.c_objective[i]),
                ),
            )
        end
        for (row, coef) in model.c_data[i]
            println(
                io,
                Card(
                    f2 = c_name,
                    f3 = model.r_name[row],
                    f4 = _to_string(coef),
                ),
            )
        end
    end
    if int_open
        println(io, Card(f2 = "MARKER", f3 = "'MARKER'", f5 = "'INTEND'"))
    end
    println(io, "RHS")
    # Objective constants are added to the RHS as a negative offset.
    # https://www.ibm.com/docs/en/icos/20.1.0?topic=standard-records-in-mps-format
    if !iszero(model.obj_offset)
        println(io, Card(f2 = "rhs", f3 = "OBJ", f4 = _to_string(-model.obj_offset)))
    end
    for (r_name, r_b) in zip(model.r_name, model.r_b)
        println(io, Card(f2 = "rhs", f3 = r_name, f4 = _to_string(r_b)))
    end
    range_printed = false
    for (r_name, r_range) in zip(model.r_name, model.r_range)
        if !range_printed
            println(io, "RANGES")
            range_printed = true
        end
        if !iszero(r_range)
            println(io, Card(f2 = "rhs", f3 = r_name, f4 = _to_string(r_range)))
        end
    end
    println(io, "BOUNDS")
    for (i, c_name) in enumerate(model.c_name)
        if model.c_bound_type[i] == RTYPE_FREE
            println(io, Card(f1 = "FR", f2 = "bounds", f3 = c_name))
        elseif model.c_bound_type[i] == RTYPE_EQUAL_TO
            println(
                io,
                Card(
                    f1 = "FX",
                    f2 = "bounds",
                    f3 = c_name,
                    f4 = _to_string(model.c_lower[i]),
                ),
            )
        elseif model.c_bound_type[i] == RTYPE_GREATER_THAN
            println(
                io,
                Card(
                    f1 = "LO bounds    ",
                    # f2 = "bounds",
                    f3 = c_name,
                    f4 = _to_string(model.c_lower[i]),
                ),
            )
        elseif model.c_bound_type[i] == RTYPE_LESS_THAN
            println(
                io,
                Card(
                    f1 = "UP",
                    f2 = "bounds",
                    f3 = c_name,
                    f4 = _to_string(model.c_upper[i]),
                ),
            )
        else
            println(
                io,
                Card(
                    f1 = "LO bounds",
                    # f2 = "bounds",
                    f3 = c_name,
                    f4 = _to_string(model.c_lower[i]),
                ),
            )
            println(
                io,
                Card(
                    f1 = "UP bounds",
                    # f2 = "bounds",
                    f3 = c_name,
                    f4 = _to_string(model.c_upper[i]),
                ),
            )
        end
    end
    return println(io, "ENDATA")
end

end  # MPS2
