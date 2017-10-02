#=
    This file contains the logic to convert MathOptInterface sets to
    MathOptFormat objects.
=#
"""
    equalto

### Examples

    {
        "head": "EqualTo",
        "value": 3.0
    }
"""
equalto(value) = Object("head" => "EqualTo", "value"=> value)

"""
    {
        "head": "LessThan",
        "value": 3.0
    }
"""
lessthan(value) = Object("head" => "LessThan", "value"=> value)

"""
    {
        "head": "GreaterThan",
        "value": 3.0
    }
"""
greaterthan(value) = Object("head" => "GreaterThan", "value"=> value)
