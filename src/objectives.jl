# Objectives

"""
    setobjective!(m::AbstractModel, b, a_varref::Vector{VariableReference}, a_coef, Q_vari::Vector{VariableReference}, Q_varj::Vector{VariableReference}, Q_coef, N::Int=1)

Set the `N`th objective in the model `m` to be ``\\frac{1}{2} x^T Q_0 x + a_0^T x + b_0``, where:
* ``a_0`` is a sparse vector specified in tuple form by `a_varref, a_coef`
* ``b_0`` is a scalar
* the symmetric matrix ``Q_0`` is defined by the triplets in `Q_vari, Q_varj, Q_coef`

Duplicate indices (sparse) in either the ``a_0`` vector or the ``Q_0`` matrix are accepted and will be summed together.
Off-diagonal entries of ``Q_0`` will be mirrored, so either the upper triangular or lower triangular entries of ``Q_0`` should be provided.
If entries for both ``(i,j)`` and ``(j,i)`` are provided, these are considered duplicate terms.
`a_varref`, `Q_vari`, `Q_varj` should be collections of `VariableReference` objects.

    setobjective!(m::AbstractModel, b, a_varref::Vector{VariableReference}, a_coef, N::Int=1)

Set the `N`th objective in the model `m` to be ``a_0^T x + b_0``, where:
* ``a_0`` is a sparse vector specified in tuple form by `a_varref, a_coef`
* ``b_0`` is a scalar

Duplicate indices (sparse) in either the ``a_0`` vector or the ``Q_0`` matrix are accepted and will be summed together.
"""
function setobjective! end

"""
    getobjectiveconstant(m, N::Int=1)

Return the constant term in the `N`th objective.
"""
function getobjectiveconstant end

"""
    getobjectiveaffine(m, N::Int=1)

Return the affine part of the `N`th objective in tuple form `(varref, coef)` where `varref` is a `VariableReference` and `coef` is a coefficient.
Output is a tuple of two vectors.

    getobjectiveaffine(m, v::VariableReference, N::Int=1)

Return the coefficient for the variable `v` in the affine part of the `N`th objective.
"""
function getobjectiveaffine end

## TODO: getobjectivequadratic

"""
    modifyobjective!(m::AbstractModel, N::Int, args...)

Modify elements of the `N`th objective depending on the arguments `args`.
The `N`th objective has the form ``\\frac{1}{2} x^T Q_0 x + a_0^T x + b_0``.

There are three cases, below.

## Modify Constant term

    modifyobjective!(m::AbstractModel, N::Int, b)

Set the constant term ``b_0`` of the `N`th objective to `b`.

### Examples

```julia
modifyobjective!(m, 1, 1.0)
```

## Modify Linear term

    modifyobjective!(m::AbstractModel, N::Int, a_varidx, a_coef)

Set elements given by `a_varidx` in the linear term of the `N`th objective to `a_coef`.
Either `a_varidx` and `a_coef` are both singletons, or they should be collections with equal length.
The behavior of duplicate entries in `a_varidx` is undefined.

### Examples

```julia
modifyobjective!(m, 1, v, 1.0)
modifyobjective!(m, 1, [v1, v2], [1.0, 2.0])
```

## Modify Quadratic term

    modifyobjective!(m::AbstractModel, N::Int, Q_vari, Q_varj, Q_coef)

Set the elements in the quadratic term of the `N`th objective specified by the triplets `Q_vari, Q_varj, Q_coef`.
Off-diagonal entries will be mirrored.
`Q_vari, Q_varj` should be collections of `VariableReference` objects.
The behavior of duplicate entries is undefined.
If entries for both ``(i,j)`` and ``(j,i)`` are provided, these are considered duplicate terms.

### Examples

```julia
modifyobjective!(m, 1, v1, v2, 1.0)
modifyobjective!(m, 1, [v1, v2], [v1, v1], [1.0, 2.0])
```
"""
function modifyobjective! end
