#=
    Sets defined by MathOptFormat. These are map closely to those defined in
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/sets.jl
    In most cases, the doc-strings are copied verbatim
=#

function checkinf(x)
    if x == -Inf
        return "-inf"
    elseif x == Inf
        return "+inf"
    else
        return x
    end
end

"""
    object(set::MOI.AbstractSet)

Convert a MOI set to the MathOptFormat JSON representation. All MathOptFormat
set objects have a key `head` with the corresponding value that matches the name
of the MathOptFormat set (i.e. `GreaterThan`, `ZeroOne` etc.).

Any fields inside the set `set` are also copied. For example:

    object(set::LessThan) = Object("head"=>"LessThan", "upper"=>set.upper)
"""
function object end

object(set::MOI.EqualTo) = Object("head" => "EqualTo", "value"=> set.value)

object(set::MOI.LessThan) = Object("head" => "LessThan", "upper"=> set.upper)

object(set::MOI.GreaterThan) = Object("head" => "GreaterThan", "lower"=> set.lower)

object(set::MOI.Interval) = Object("head" => "Interval", "lower" => checkinf(set.lower), "upper" => checkinf(set.upper))

object(set::MOI.Integer) = Object("head" => "Integer")

object(set::MOI.ZeroOne) = Object("head" => "ZeroOne")

object(set::MOI.Reals) = Object("head" => "Reals", "dimension" => set.dimension)

object(set::MOI.Zeros) = Object("head" => "Zeros", "dimension" => set.dimension)

object(set::MOI.Nonnegatives) = Object("head" => "Nonnegatives", "dimension" => set.dimension)

object(set::MOI.Nonpositives) = Object("head" => "Nonpositives", "dimension" => set.dimension)

object(set::MOI.Semicontinuous) = Object("head" => "Semicontinuous", "lower" => checkinf(set.lower), "upper" => checkinf(set.upper))

object(set::MOI.Semiinteger) = Object("head" => "Semiinteger", "lower" => checkinf(set.lower), "upper" => checkinf(set.upper))

object(set::MOI.SOS1) = Object("head" => "SOS1", "weights" => set.weights)

object(set::MOI.SOS2) = Object("head" => "SOS2", "weights" => set.weights)

object(set::MOI.SecondOrderCone) = Object("head" => "SecondOrderCone", "dimension" => set.dimension)

object(set::MOI.RotatedSecondOrderCone) = Object("head" => "RotatedSecondOrderCone", "dimension" => set.dimension)

object(set::MOI.ExponentialCone) = Object("head" => "ExponentialCone")

object(set::MOI.DualExponentialCone) = Object("head" => "DualExponentialCone")

object(set::MOI.PowerCone) = Object("head" => "PowerCone", "exponent" => set.exponent)

object(set::MOI.DualPowerCone) = Object("head" => "DualPowerCone", "exponent" => set.exponent)

object(set::MOI.PositiveSemidefiniteConeTriangle) = Object("head" => "PositiveSemidefiniteConeTriangle", "dimension" => set.dimension)

object(set::MOI.PositiveSemidefiniteConeScaled)  = Object("head" => "PositiveSemidefiniteConeScaled", "dimension" => set.dimension)
