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
    affine

A special case to express an affine expression <variables, coefficients> +
constant. Other fields are:

 - `variables`: an array of variable names. Every element must be a string that
 corresponds to the name of a variable. Objects and numeric values are illegal.
 - `coefficients`: an array of numeric values. Every element must be a numeric
 value. Objects and strings are illegal.
 - `constant`: This must be a numeric value. Objects and strings are illegal.

### Example

    {
      head: "affine",
      variables: ["x", "y"],
      coefficients: [1.0, 2.0],
      constant: 3.0
    }
"""
affine(variables, coefficients, constant) = Object(
    "head"         => "affine",
    "variables"    => variables,
    "coefficients" => coefficients,
    "constant"     => constant
)

"""
    vectoraffine(outputindex, variables, coefficients, constant)

The vector-valued affine function Ax+b, where:
 - A is a sparse matrix specified in triplet form by `outputindex`,
 `variables`, `coefficients`
 - b is a vector specified by `constant`

### Example
    |1 2|   |x|   |3|
    |4 3| * |y| + |4|
    {
      head: "vectoraffine",
      outputindex: [1, 1, 2, 2]
      variables: ["x", "y", "x", "y"],
      coefficients: [1.0, 2.0, 4.0, 3.0],
      constant: [3.0, 4.0]
    }
"""
vectoraffine(outputindex, variables, coefficients, constant) = Object(
    "head"         => "vectoraffine",
    "outputindex"  => outputindex,
    "variables"    => variables,
    "coefficients" => coefficients,
    "constant"     => constant
)

"""
    quadratic(affine_variables, affine_coefficients, quadratic_rowvariables,
        quadratic_colvariables, quadratic_coefficients, constant)

The scalar-valued quadratic function 0.5xᵀQx + aᵀx + b, where:
 - a is a sparse vector specified in tuple form by `affine_variables`,
 `affine_coefficients`
 - b is a scalar specified by `constant`
 - Q is a symmetric matrix is specified in triplet form by
 `quadratic_rowvariables`, `quadratic_colvariables`, `quadratic_coefficients`

### Example

    {
      head: "quadratic",
      affine_variables: ["x", "y"],
      affine_coefficients: [1.0, 2.0],
      quadratic_rowvariables: ["x", "y"],
      quadratic_colvariables: ["x", "y"],
      quadratic_coefficients: [1.0, 2.0],
      constant: 3.0
    }
"""
quadratic(affine_variables, affine_coefficients, quadratic_rowvariables,
    quadratic_colvariables, quadratic_coefficients, constant) = Object(
    "head"                   => "quadratic",
    "affine_variables"       => affine_variables,
    "affine_coefficients"    => affine_coefficients,
    "quadratic_rowvariables" => quadratic_rowvariables,
    "quadratic_colvariables" => quadratic_colvariables,
    "quadratic_coefficients" => quadratic_coefficients,
    "constant"               => constant
)

"""
    vectorquadratic(affine_outputindex, affine_variables, affine_coefficients,
        quadratic_outputindex, quadratic_rowvariables, quadratic_colvariables,
        quadratic_coefficients, constant)

The vector-valued quadratic function with i`th` component ("output index")
defined as ``\\frac{1}{2}x^TQ_ix + a_i^T x + b_i``, where:
* ``a_i`` is a sparse vector specified in tuple form by the subset of
`affine_variables, affine_coefficients` for the indices `k` where
`affine_outputindex[k] == i`.
* ``b_i`` is a scalar specified by `constant[i]`
* ``Q_i`` is a symmetric matrix is specified in triplet form by the subset
of `quadratic_rowvariables, quadratic_colvariables, quadratic_coefficients`
for the indices `k` where `quadratic_outputindex[k] == i`

### Example

    1x + 2y + x^2 +     + 2y^2 + 0
    3x + 4y +       x*y        + 3

    {
      head: "vectorquadratic",
      affine_outputindex: [1, 1, 2, 2]
      affine_variables: ["x", "y", "x", "y"],
      affine_coefficients: [1.0, 2.0, 3.0, 4.0],
      quadratic_outputindex: [1, 1, 2],
      quadratic_rowvariables: ["x", "y", "x"],
      quadratic_colvariables: ["x", "y", "y"],
      quadratic_coefficients: [2.0, 4.0, 1.0],
      constant: [0.0, 3.0]
    }
"""
vectorquadratic(affine_outputindex, affine_variables, affine_coefficients,
    quadratic_outputindex, quadratic_rowvariables, quadratic_colvariables,
    quadratic_coefficients, constant) = Object(
    "head"         => "vectorquadratic",
    "affine_outputindex" => affine_outputindex,
    "affine_variables"    => affine_variables,
    "affine_coefficients" => affine_coefficients,
    "quadratic_outputindex" => quadratic_outputindex,
    "quadratic_rowvariables" => quadratic_rowvariables,
    "quadratic_colvariables" => quadratic_colvariables,
    "quadratic_coefficients" => quadratic_coefficients,
    "constant"     => constant
)
