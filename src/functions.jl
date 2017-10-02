#=
    Functions defined by MathOptFormat. These are largely inspired by
    https://github.com/JuliaOpt/MathOptInterface.jl/blob/master/src/functions.jl
=#

"""
    variable

Refer to a variable in the model. Other fields:

 - `name`: Must be a string that refers the a unique name of the variable in the
 model.

### Example

    {
        "head": "variable",
        "name": "x1"
    }
"""
variable(name::String) = Object("head"=>"variable", "name"=> name)


"""
    variableset

Refer to an ordered set of variables in the model.

 - `names`: Must be an array of strings. Each element must refer to a unique
 name of the variable in the model. Duplicate names are not allowed.

### Example

    {
        "head": "variableset",
        "names": ["x1", "x2"]
    }
"""
variableset(names::Vector{String}) = Object("head"=>"variableset", "names"=>names)


"""
    linear

A special case to express an affine expression <variables, coefficients> +
constant. Other fields are:

 - `variables`: an array of variable names. Every element must be a string that
 corresponds to the name of a variable. Objects and numeric values are illegal.
 - `coefficients`: an array of numeric values. Every element must be a numeric
 value. Objects and strings are illegal.
 - `constant`: This must be a numeric value. Objects and strings are illegal.

### Example

    {
      head: "linear",
      variables: ["x", "y"],
      coefficients: [1.0, 2.0],
      constant: 3.0
    }
"""
linear(variables, coefficients, constant) = Object(
    "head"         => "linear",
    "variables"    => variables,
    "coefficients" => coefficients,
    "constant"     => constant
)
