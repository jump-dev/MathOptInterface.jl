#=
    Sets defined by MathOptFormat. These are map closely to those defined in
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/sets.jl
    In most cases, the doc-strings are copied verbatim
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
    interval(lower, upper)

The set `[lower, upper] ⊆ R`.

### Examples

    {
        "head": "Interval",
        "lower": 1.0,
        "upper": 2.0
    }
"""
interval(lower, upper) = Object("head" => "Interval", "lower" => lower, "upper" => upper)

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

"""
    reals(dim)

The set `R^{dim}`.

### Examples

    {
        "head": "Reals",
        "dimension": 1
    }
"""
reals(dim::Int) = Object("head" => "Reals", "dimension" => dim)

"""
    zeros(dim)

The set `{0}^{dim}`.

### Examples

    {
        "head": "Zeros",
        "dimension": 1
    }
"""
zeros(dim::Int) = Object("head" => "Zeros", "dimension" => dim)

"""
    nonnegatives(dim)

The set `{x ∈ R^{dim}: x ≤ 0}`.

### Examples

    {
        "head": "Nonnegatives",
        "dimension": 1
    }
"""
nonnegatives(dim::Int) = Object("head" => "Nonnegatives", "dimension" => dim)

"""
    nonpositives(dim)

The set `{x ∈ R^{dim}: x ≥ 0}`.

### Examples

    {
        "head": "Nonpositives",
        "dimension": 1
    }
"""
nonpositives(dim::Int) = Object("head" => "Nonpositives", "dimension" => dim)

"""
    semicontinuous(lower, upper)

The set `{0} ∪ [lower, upper]`.

### Examples

    {
        "head": "Semicontinuous",
        "lower": 1,
        "upper": 2
    }
"""
semicontinuous(lower, upper) = Object("head" => "Semicontinuous", "lower" => lower, "upper" => upper)

"""
    semiinteger(lower, upper)

The set `{0} ∪ [lower, lower+1, ..., upper-1, upper]`.

### Examples

    {
        "head": "Semiinteger",
        "lower": 1,
        "upper": 2
    }
"""
semiinteger(lower, upper) = Object("head" => "Semiinteger", "lower" => lower, "upper" => upper)

"""
    sos1(weights)

The set corresponding to the special ordered set (SOS) constraint of type 1. Of
the variables in the set, at most one can be nonzero. The weights induce an
ordering of the variables; as such, they should be unique values. The kth
element in the set corresponds to the kth weight in weights

### Examples

    {
        "head": "SOSI",
        "lower": [1.0, 2.0, 3.0]
    }
"""
sos1(weights::Vector) = Object("head" => "SOSI", "weights" => weights)

"""
    sos2(weights)

The set corresponding to the special ordered set (SOS) constraint of type 2. Of
the variables in the set, at most two can be nonzero, and if two are nonzero,
they must be adjacent in the ordering of the set. The weights induce an ordering
of the variables; as such, they should be unique values. The kth element in the
set corresponds to the kth weight in weights

### Examples

    {
        "head": "SOSII",
        "lower": [1.0, 2.0, 3.0]
    }
"""
sos2(weights::Vector) = Object("head" => "SOSII", "weights" => weights)

#=
TODO: the following sets have not yet been copied over

    secondordercone
    rotatedsecondordercone
    exponentialcone
    exponentialcone
    dualexponentialcone
    powercone
    dualpowercone
    positivesemidefiniteconetriangle
    positivesemidefiniteconescaled
=#
