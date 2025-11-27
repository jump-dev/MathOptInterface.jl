# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module MPS

import ..FileFormats
import MathOptInterface as MOI
import OrderedCollections: OrderedDict

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

MOI.supports(::Model, ::MOI.ObjectiveFunction) = false

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{
        <:Union{
            MOI.VariableIndex,
            MOI.ScalarAffineFunction{T},
            MOI.ScalarQuadraticFunction{T},
        },
    },
) where {T}
    return true
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
    Model(;
        warn::Bool = false,
        print_objsense::Bool = false,
        generic_names::Bool = false,
        quadratic_format::QuadraticFormat = kQuadraticFormatGurobi,
        coefficient_type::Type{T} = Float64,
    ) where {T}

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

 - `coefficient_type::Type{T}=Float64`: the supported type to use when reading
   and writing files.
"""
function Model(;
    warn::Bool = false,
    print_objsense::Bool = false,
    generic_names::Bool = false,
    quadratic_format::QuadraticFormat = kQuadraticFormatGurobi,
    coefficient_type::Type{T} = Float64,
) where {T}
    model = Model{T}()
    model.ext[:MPS_OPTIONS] =
        Options(warn, print_objsense, generic_names, quadratic_format)
    return model
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.MPS.Model")

@enum(VType, VTYPE_CONTINUOUS, VTYPE_INTEGER, VTYPE_BINARY)

include("read.jl")
include("write.jl")

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

end  # module MPS
