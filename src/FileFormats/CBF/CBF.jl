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
    (MOI.ScalarAffineFunction,),
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

struct Options end

get_options(m::Model) = get(m.ext, :CBF_OPTIONS, Options())

"""
    Model()

Create an empty instance of `FileFormats.CBF.Model`.
"""
function Model()
    model = Model{Float64}()
    model.ext[:CBF_OPTIONS] = Options()
    return model
end

Base.show(io::IO, ::Model) = print(io, "A Conic Benchmark Format (CBF) model")

include("read.jl")
include("write.jl")

end
