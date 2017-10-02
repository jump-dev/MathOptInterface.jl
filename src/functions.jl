"""
    variable

### Example
    {
        "head": "variable",
        "value": "x1"
    }
"""
variable(name::String) = Object("head"=>"variable", "name"=> name)


"""
    variableset

### Example

    {
        "head": "variableset",
        "value": ["x1", "x2"]
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

### Example: x + y + 3

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
