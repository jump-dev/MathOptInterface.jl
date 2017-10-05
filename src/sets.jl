#=
    Sets defined by MathOptFormat. These are map closely to those defined in
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/sets.jl
    In most cases, the doc-strings are copied verbatim
=#

"""
    Object(set::MOI.AbstractSet)

Convert a MOI set to the MathOptFormat JSON representation. All MathOptFormat
set objects have a key `head` with the corresponding value that matches the name
of the MathOptFormat set (i.e. `GreaterThan`, `ZeroOne` etc.).

Any fields inside the set `set` are also copied. For example:

    Object(set::LessThan) = Object("head"=>"LessThan", "upper"=>set.upper)
"""
Object(set::MOI.AbstractSet) = error("Set $set not defined in MathOptFormat")

Object(set::MOI.EqualTo) = Object("head" => "EqualTo", "value"=> set.value)

Object(set::MOI.LessThan) = Object("head" => "LessThan", "upper"=> set.upper)

Object(set::MOI.GreaterThan) = Object("head" => "GreaterThan", "lower"=> set.lower)

Object(set::MOI.Interval) = Object("head" => "Interval", "lower" => set.lower, "upper" => set.upper)

Object(::MOI.Integer) = Object("head" => "Integer")

Object(::MOI.ZeroOne) = Object("head" => "ZeroOne")

Object(set::MOI.Reals) = Object("head" => "Reals", "dim" => set.dim)

Object(set::MOI.Zeros) = Object("head" => "Zeros", "dim" => set.dim)

Object(set::MOI.Nonnegatives) = Object("head" => "Nonnegatives", "dim" => set.dim)

Object(set::MOI.Nonpositives) = Object("head" => "Nonpositives", "dim" => set.dim)

Object(set::MOI.Semicontinuous) = Object("head" => "Semicontinuous", "l" => set.l, "u" => set.u)

Object(set::MOI.Semiinteger) = Object("head" => "Semiinteger", "l" => set.l, "u" => set.u)

Object(set::MOI.SOS1) = Object("head" => "SOS1", "weights" => set.weights)

Object(set::MOI.SOS2) = Object("head" => "SOS2", "weights" => set.weights)

Object(set::MOI.SecondOrderCone) = Object("head" => "SecondOrderCone", "dim" => set.dim)

Object(set::MOI.RotatedSecondOrderCone) = Object("head" => "RotatedSecondOrderCone", "dim" => set.dim)

Object(set::MOI.ExponentialCone) = Object("head" => "ExponentialCone")

Object(set::MOI.DualExponentialCone) = Object("head" => "DualExponentialCone")

Object(set::MOI.PowerCone) = Object("head" => "PowerCone", "a" => set.a)

Object(set::MOI.DualPowerCone) = Object("head" => "DualPowerCone", "a" => set.a)

Object(set::MOI.PositiveSemidefiniteConeTriangle) = Object("head" => "PositiveSemidefiniteConeTriangle", "dim" => set.dim)

Object(set::MOI.PositiveSemidefiniteConeScaled)  = Object("head" => "PositiveSemidefiniteConeScaled", "dim" => set.dim)
