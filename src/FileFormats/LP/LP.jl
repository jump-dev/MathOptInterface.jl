# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module LP

import ..FileFormats
import MathOptInterface as MOI

const _ILT1{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.LessThan{T}}
const _IGT1{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{T}}
const _IET1{T} = MOI.Indicator{MOI.ACTIVATE_ON_ONE,MOI.EqualTo{T}}
const _ILT0{T} = MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.LessThan{T}}
const _IGT0{T} = MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.GreaterThan{T}}
const _IET0{T} = MOI.Indicator{MOI.ACTIVATE_ON_ZERO,MOI.EqualTo{T}}

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (),
    (MOI.SOS1, MOI.SOS2, _ILT1, _IET1, _IGT1, _ILT0, _IGT0, _IET0),
    (),
    (MOI.ScalarQuadraticFunction, MOI.ScalarAffineFunction),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{<:Union{MOI.Parameter,MOI.Semicontinuous,MOI.Semiinteger}},
)
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.SOS1{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VectorAffineFunction{T}},
    ::Type{MOI.SOS2{T}},
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

struct Options
    maximum_length::Int
    warn::Bool
end

function get_options(m::Model)
    default_options = Options(255, false)
    return get(m.ext, :LP_OPTIONS, default_options)
end

"""
    Model(;
        maximum_length::Int = 255,
        warn::Bool = false,
        coefficient_type::Type{T} = Float64,
    ) where {T}

Create an empty instance of FileFormats.LP.Model.

Keyword arguments are:

 - `maximum_length::Int=255`: the maximum length for the name of a variable.
   lp_solve 5.0 allows only 16 characters, while CPLEX 12.5+ allow 255.

 - `warn::Bool=false`: print a warning when variables or constraints are renamed.

 - `coefficient_type::Type{T} = Float64`: the supported type to use when reading
   and writing files.
"""
function Model(;
    maximum_length::Int = 255,
    warn::Bool = false,
    coefficient_type::Type{T} = Float64,
) where {T}
    model = Model{T}()
    options = Options(maximum_length, warn)
    model.ext[:LP_OPTIONS] = options
    return model
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.LP.Model")

include("read.jl")
include("write.jl")

end  # module LP
