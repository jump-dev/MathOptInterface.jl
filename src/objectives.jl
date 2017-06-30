"""
    setobjective!(m::AbstractModel, b, a_varref::Vector{VariableReference}, a_coef, Q_vari::Vector{VariableReference}, Q_varj::Vector{VariableReference}, Q_coef, N::Int=1)

Set the `N`'th objective in the model `m` to be
```math
a^Tx + b + \\frac{1}{2}x^TQx
```
where ``a`` is a sparse vector specified in tuple form by `a_varref`, and
`a_coef`; ``b`` is a scalar; and the symmetric matrix ``Q`` is defined by the
triplets in `Q_vari`, `Q_varj`, `Q_coef`.

Duplicate indices in either the ``a`` vector or the ``Q`` matrix are accepted and will be
summed together. Off-diagonal entries of ``Q`` will be mirrored, so either the
upper triangular or lower triangular entries of ``Q`` should be provided. If
entries for both ``(i,j)`` and ``(j,i)`` are provided, these are considered
duplicate terms. `a_varref`, `Q_vari`, `Q_varj` should be collections of
`VariableReference` objects.

    setobjective!(m::AbstractModel, b, a_varref::Vector{VariableReference}, a_coef, N::Int=1)

Set the `N`'th objective in the model `m` to be
```math
a^Tx + b
```
where ``a`` is a sparse vector specified in tuple form by `a_varref` and
`a_coef` and ``b`` is a scalar.

Duplicate indices in either the ``a`` vector are accepted and will be
summed together.
"""
function setobjective! end

"""
    modifyobjective!(m::AbstractModel, i::Int, args...)

Modify elements of the `i`'th objective depending on the
arguments `args`. The `i`'th objective will have the form:
```math
    a_i^Tx + b_i + \\frac{1}{2}x^TQ_ix
```
There are three cases.

# Modify Constant term

    modifyobjective!(m::AbstractModel, i::Int, b)

Set the constant term of the `i`'th row objective to `b`.

### Examples

```julia
modifyobjective!(m, 1, 1.0)
```

# Modify Linear term

    modifyobjective!(m::AbstractModel, i::Int, a_varidx, a_coef)

Set elements given by `a_varidx` in the linear term of the `i`'th objective to
`a_coef`. Either `a_varidx` and `a_coef` are both singletons, or they should be
collections with equal length.

The behaviour of duplicate entries in `a_varidx` is undefined.

### Examples

```julia
modifyobjective!(m, 1, v, 1.0)
modifyobjective!(m, 1, [v1, v2], [1.0, 2.0])
```

# Modify Quadratic term

    modifyobjective!(m::AbstractModel, i::Int, Q_vari, Q_varj, Q_coef)

Set the elements in the quadratic term of the `i`'th objective specified by the
triplets `Q_vari`, `Q_varj`, and `Q_coef`. Off-diagonal entries will be mirrored.
`Q_vari`, `Q_varj` should be collections of `VariableReference` objects.

The behaviour of duplicate entries is undefined. If entries for both ``(i,j)``
and ``(j,i)`` are provided, these are considered duplicate terms.

### Examples

```julia
modifyobjective!(m, 1, v1, v2, 1.0)
modifyobjective!(m, 1, [v1, v2], [v1, v1], [1.0, 2.0])
```
"""
function modifyobjective! end

"""
    getobjectiveconstant(m, i::Int=1)

Return the constant term in the `i`'th objective.
"""
function getobjectiveconstant end

"""
    getobjectiveaffine(m, i::Int=1)

Return the affine part of the `i`'th objective in tuple form `(varref,coef)` where `varref` is a `VariableReference`, and `coef` is a coefficient. Output is a tuple of two vectors.

    getobjectiveaffine(m, v::VariableReference, i::Int=1)

Return the coefficient for the variable `v` in the affine part of the `i`'th objective.
"""
function getobjectiveaffine end

## TODO: getobjectivequadratic
