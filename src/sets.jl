#=
    Sets defined by MathOptFormat. These are largely inspired by
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/sets.jl
=#

"""
    equalto(value)

The set containing the single point `value ∈ R`.

### Examples

    {
        "head": "EqualTo",
        "value": 3.0
    }
"""
equalto(value) = Object("head" => "EqualTo", "value"=> value)

"""
    lessthan(value)

The set `(-∞, value] ⊆ R`.

### Examples

    {
        "head": "LessThan",
        "value": 3.0
    }
"""
lessthan(value) = Object("head" => "LessThan", "value"=> value)

"""
    greaterthan(value)

The set `[value, ∞) ⊆ R`.

### Examples

    {
        "head": "GreaterThan",
        "value": 3.0
    }
"""
greaterthan(value) = Object("head" => "GreaterThan", "value"=> value)

"""
    integer()

The set of integers `Z`.

### Examples

    {
        "head": "Integer"
    }
"""
integer() = Object("head" => "Integer")

"""
    zeroone()

The set `{0, 1}`.

### Examples

    {
        "head": "ZeroOne"
    }
"""
zeroone() = Object("head" => "ZeroOne")
