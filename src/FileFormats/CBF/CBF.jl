module CBF

import ..FileFormats
import MathOptInterface

const MOI = MathOptInterface

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
    ::Type{MOI.SingleVariable},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.SingleVariable},
    ::Type{MOI.Integer},
)
    return true
end

function MOI.supports(
    ::Model,
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{Float64}},
)
    return false
end

"""
    Model()

Create an empty instance of `FileFormats.CBF.Model`.
"""
Model() = Model{Float64}()

Base.show(io::IO, ::Model) = print(io, "A Conic Benchmark Format (CBF) model")

include("read.jl")
include("write.jl")

end
