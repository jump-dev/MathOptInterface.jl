# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module CBF

import ..FileFormats
import MathOptInterface as MOI

MOI.Utilities.@model(
    Model,
    (MOI.Integer,),
    (),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
    ),
    (MOI.PowerCone, MOI.DualPowerCone),
    (),
    (),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{MOI.Integer},
)
    return true
end

MOI.supports(::Model, ::MOI.ObjectiveFunction) = false

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{
        <:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}},
    },
) where {T}
    return true
end

"""
    Model()

Create an empty instance of `FileFormats.CBF.Model`.
"""
Model(; kwargs...) = Model{Float64}()

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.CBF.Model")

include("read.jl")
include("write.jl")

end
