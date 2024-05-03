# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    _CBFDataStructure

A helpful datastructure for converting between MOI and CBF.
"""
mutable struct _CBFDataStructure
    num_rows::Int
    power_cones::Int
    dual_power_cones::Int
    psd_side_dims::Vector{Int}
    cones::Vector{Tuple{String,Int}}
    acoord::Vector{Tuple{Int,Int,Float64}}
    bcoord::Vector{Tuple{Int,Float64}}
    hcoord::Vector{Tuple{Int,Int,Int,Int,Float64}}
    dcoord::Vector{Tuple{Int,Int,Int,Float64}}
    variables_with_domain::Set{MOI.VariableIndex}
    variable_cones::Vector{Tuple{Vector{MOI.VariableIndex},String}}
    # This is the identity mapping, except for EXP and EXP* cones,
    # in which (u, v, w) in MOI are swapped to (w, v, u) in CBF.
    scalar_variables::Vector{Int}

    function _CBFDataStructure()
        return new(
            0,
            0,
            0,
            Int[],
            Tuple{String,Int}[],
            Tuple{Int,Int,Float64}[],
            Tuple{Int,Float64}[],
            Tuple{Int,Int,Int,Int,Float64}[],
            Tuple{Int,Int,Int,Float64}[],
            Set{MOI.VariableIndex}(),
            Tuple{Vector{MOI.VariableIndex},String}[],
            Int[],
        )
    end
end

_cone_string(::Any, ::Type{MOI.Zeros}) = "L="
_cone_string(::Any, ::Type{MOI.Reals}) = "F"
_cone_string(::Any, ::Type{MOI.Nonnegatives}) = "L+"
_cone_string(::Any, ::Type{MOI.Nonpositives}) = "L-"
_cone_string(::Any, ::Type{MOI.SecondOrderCone}) = "Q"
_cone_string(::Any, ::Type{MOI.RotatedSecondOrderCone}) = "QR"
_cone_string(::Any, ::Type{MOI.ExponentialCone}) = "EXP"
_cone_string(::Any, ::Type{MOI.DualExponentialCone}) = "EXP*"

function _cone_string(data::_CBFDataStructure, ::Type{MOI.PowerCone{Float64}})
    s = "@$(data.power_cones):POW"
    data.power_cones += 1
    return s
end

function _cone_string(
    data::_CBFDataStructure,
    ::Type{MOI.DualPowerCone{Float64}},
)
    s = "@$(data.dual_power_cones):POW*"
    data.dual_power_cones += 1
    return s
end

function _add_function(data, f::MOI.VectorOfVariables, ::Any)
    for v in f.variables
        data.num_rows += 1
        push!(data.acoord, (data.num_rows, v.value, 1.0))
    end
    return
end

function _add_function(data, f::MOI.VectorAffineFunction, ::Any)
    for term in f.terms
        t = term.scalar_term
        row = data.num_rows + term.output_index
        push!(data.acoord, (row, t.variable.value, t.coefficient))
    end
    for c in f.constants
        data.num_rows += 1
        push!(data.bcoord, (data.num_rows, c))
    end
    return
end

function _add_function(
    data,
    f::MOI.VectorOfVariables,
    ::Type{<:Union{MOI.ExponentialCone,MOI.DualExponentialCone}},
)
    # The Exponential cone in MOI and CBF are reversed
    for v in reverse(f.variables)
        data.num_rows += 1
        push!(data.acoord, (data.num_rows, v.value, 1.0))
    end
    return
end

function _add_function(
    data,
    f::MOI.VectorAffineFunction,
    ::Type{<:Union{MOI.ExponentialCone,MOI.DualExponentialCone}},
)
    # The Exponential cone in MOI and CBF are reversed
    for term in f.terms
        t = term.scalar_term
        row = data.num_rows + 4 - term.output_index
        push!(data.acoord, (row, t.variable.value, t.coefficient))
    end
    for c in reverse(f.constants)
        data.num_rows += 1
        push!(data.bcoord, (data.num_rows, c))
    end
    return
end

function _add_cones(
    data::_CBFDataStructure,
    model::Model,
    ::Type{F},
    ::Type{S},
) where {F,S}
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        _add_function(data, f, S)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        push!(data.cones, (_cone_string(data, S), MOI.dimension(set)))
    end
    return
end

function _add_cones(
    data::_CBFDataStructure,
    model::Model,
    ::Type{F},
    ::Type{S},
) where {F<:MOI.VectorOfVariables,S}
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        is_variable_cone = true
        for (i, xi) in enumerate(f.variables)
            if xi in data.variables_with_domain
                is_variable_cone = false
                break
            elseif xi.value != f.variables[1].value + i - 1
                is_variable_cone = false
                break
            end
        end
        str = _cone_string(data, S)
        if !is_variable_cone
            _add_function(data, f, S)
            set = MOI.get(model, MOI.ConstraintSet(), ci)
            push!(data.cones, (str, MOI.dimension(set)))
        else
            for xi in f.variables
                push!(data.variables_with_domain, xi)
            end
            push!(data.variable_cones, (f.variables, str))
        end
    end
    return
end

function _add_cones(
    data::_CBFDataStructure,
    model::Model,
    F::Type{MOI.VectorOfVariables},
    S::Type{MOI.PositiveSemidefiniteConeTriangle},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        push!(data.psd_side_dims, set.side_dimension)
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        k = 0
        for i in 1:set.side_dimension, j in 1:i
            k += 1
            push!(
                data.hcoord,
                (length(data.psd_side_dims), f.variables[k].value, i, j, 1.0),
            )
        end
    end
    return
end

function _add_cones(
    data::_CBFDataStructure,
    model::Model,
    F::Type{MOI.VectorAffineFunction{Float64}},
    S::Type{MOI.PositiveSemidefiniteConeTriangle},
)
    for ci in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        push!(data.psd_side_dims, set.side_dimension)
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        for term in f.terms
            # Get (i,j) index in symmetric matrix from lower triangle index.
            i = div(1 + isqrt(8 * term.output_index - 7), 2)
            push!(
                data.hcoord,
                (
                    length(data.psd_side_dims),
                    term.scalar_term.variable.value,
                    i,
                    term.output_index - div(i * (i - 1), 2),
                    term.scalar_term.coefficient,
                ),
            )
        end
        k = 0
        for i in 1:set.side_dimension, j in 1:i
            k += 1
            c = f.constants[k]
            if !iszero(c)
                push!(data.dcoord, (length(data.psd_side_dims), i, j, c))
            end
        end
    end
    return
end

function _CBFDataStructure(model::Model)
    data = _CBFDataStructure()
    for S in (
        MOI.Zeros,
        MOI.Reals,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
        MOI.PowerCone{Float64},
        MOI.DualPowerCone{Float64},
    )
        _add_cones(data, model, MOI.VectorOfVariables, S)
        _add_cones(data, model, MOI.VectorAffineFunction{Float64}, S)
    end
    _add_cones(
        data,
        model,
        MOI.VectorOfVariables,
        MOI.PositiveSemidefiniteConeTriangle,
    )
    _add_cones(
        data,
        model,
        MOI.VectorAffineFunction{Float64},
        MOI.PositiveSemidefiniteConeTriangle,
    )
    return data
end

function _write_VER(io)
    println(io, "VER\n3\n")
    return
end

function _write_OBJSENSE(io::IO, model::Model)
    if MOI.get(model, MOI.ObjectiveSense()) == MOI.MAX_SENSE
        println(io, "OBJSENSE\nMAX\n")
    else
        println(io, "OBJSENSE\nMIN\n")
    end
    return
end

function _write_POWCONES(io::IO, model::Model, S, keyword)
    cons = vcat(
        MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorOfVariables,S}()),
        MOI.get(
            model,
            MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},S}(),
        ),
    )
    if isempty(cons)
        return
    end
    println(io, keyword)
    println(io, length(cons), " ", 2 * length(cons))
    for ci in cons
        println(io, 2)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        println(io, set.exponent)
        println(io, 1.0 - set.exponent)
    end
    println(io)
    return
end

function _write_VAR(io::IO, model::Model, data)
    num_var = MOI.get(model, MOI.NumberOfVariables())
    append!(data.scalar_variables, collect(1:num_var))
    cones = Tuple{String,Int}[]
    current_variable = 0
    for (f, str) in sort!(data.variable_cones; by = x -> first(x[1]).value)
        offset = first(f).value - current_variable - 1
        if offset > 0
            push!(cones, ("F", offset))
        end
        push!(cones, (str, length(f)))
        current_variable = last(f).value
    end
    if current_variable < num_var
        push!(cones, ("F", num_var - current_variable))
    end
    println(io, "VAR")
    println(io, num_var, " ", length(cones))
    offset = 1
    for (K, n) in cones
        println(io, K, " ", n)
        if K == "EXP" || K == "EXP*"
            # The ordering for EXP and EXP* is reversed between MOI and CBF
            tmp = data.scalar_variables[offset]
            data.scalar_variables[offset] = data.scalar_variables[offset+2]
            data.scalar_variables[offset+2] = tmp
        end
        offset += n
    end
    println(io)
    return
end

function _write_INT(io::IO, model::Model, data)
    cons = MOI.get(
        model,
        MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.Integer}(),
    )
    if isempty(cons)
        return
    end
    println(io, "INT")
    println(io, length(cons))
    for ci in cons
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        println(io, data.scalar_variables[f.value] - 1)
    end
    println(io)
    return
end

function _write_PSDCON(io::IO, data::_CBFDataStructure)
    if isempty(data.psd_side_dims)
        return
    end
    println(io, "PSDCON\n", length(data.psd_side_dims))
    for side_dim in data.psd_side_dims
        println(io, side_dim)
    end
    println(io)
    return
end

function _write_OBJACOORD(io::IO, model::Model, data)
    f = MOI.get(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    )
    if !isempty(f.terms)
        println(io, "OBJACOORD\n", length(f.terms))
        for t in f.terms
            column = data.scalar_variables[t.variable.value] - 1
            println(io, column, " ", t.coefficient)
        end
        println(io)
    end
    return f.constant
end

function _write_OBJBCOORD(io::IO, constant::Float64)
    if !iszero(constant)
        println(io, "OBJBCOORD\n", constant, "\n")
    end
    return
end

function _write_CON(io::IO, data::_CBFDataStructure)
    if isempty(data.cones)
        return
    end
    println(io, "CON\n", data.num_rows, " ", length(data.cones))
    for (s, dim) in data.cones
        println(io, s, " ", dim)
    end
    println(io)
    return
end

function _write_ACOORD(io::IO, data::_CBFDataStructure)
    if isempty(data.acoord)
        return
    end
    println(io, "ACOORD\n", length(data.acoord))
    for (row, var, coef) in data.acoord
        column = data.scalar_variables[var] - 1
        println(io, row - 1, " ", column, " ", coef)
    end
    println(io)
    return
end

function _write_BCOORD(io::IO, data::_CBFDataStructure)
    if isempty(data.bcoord)
        return
    end
    println(io, "BCOORD\n", length(data.bcoord))
    for (row, constant) in data.bcoord
        println(io, row - 1, " ", constant)
    end
    println(io)
    return
end

function _write_HCOORD(io::IO, data::_CBFDataStructure)
    if isempty(data.hcoord)
        return
    end
    println(io, "HCOORD\n", length(data.hcoord))
    for (psd_idx, var, i, j, coef) in data.hcoord
        println(
            io,
            psd_idx - 1,
            " ",
            data.scalar_variables[var] - 1,
            " ",
            i - 1,
            " ",
            j - 1,
            " ",
            coef,
        )
    end
    println(io)
    return
end

function _write_DCOORD(io::IO, data::_CBFDataStructure)
    if isempty(data.dcoord)
        return
    end
    println(io, "DCOORD\n", length(data.dcoord))
    for (psd_idx, i, j, constant) in data.dcoord
        println(io, psd_idx - 1, " ", i - 1, " ", j - 1, " ", constant)
    end
    println(io)
    return
end

"""
    Base.write(io::IO, model::FileFormats.CBF.Model)

Write `model` to `io` in the Conic Benchmark Format.
"""
function Base.write(io::IO, model::Model)
    # The CBF file format requires an explicit ordering of the sections.
    # Below, each section is wrapped in a _write_SECTION function.
    # Sections that are currently not supported by the writer are added, but
    # commented out. They should be implemented in the future.

    # The CBF file format splits problem structure and problem data. Use this
    # datastructure to cache results between the different sections.
    #
    # If a section doesn't need cached data, just use `model` directly.
    data = _CBFDataStructure(model)

    ###
    ### File format
    ###
    _write_VER(io)

    ###
    ### Parametric cone specification
    ###
    _write_POWCONES(io, model, MOI.PowerCone{Float64}, "POWCONES")
    _write_POWCONES(io, model, MOI.DualPowerCone{Float64}, "POW*CONES")

    ###
    ### Problem structure
    ###
    _write_OBJSENSE(io, model)
    # _write_PSDVAR
    _write_VAR(io, model, data)
    _write_INT(io, model, data)
    _write_PSDCON(io, data)
    _write_CON(io, data)

    ###
    ### Problem data
    ###
    # _write_OBJFCOORD
    constant = _write_OBJACOORD(io, model, data)
    _write_OBJBCOORD(io, constant)
    # _write_FCOORD
    _write_ACOORD(io, data)
    _write_BCOORD(io, data)
    _write_HCOORD(io, data)
    _write_DCOORD(io, data)
    # _write_CHANGE
    return
end
