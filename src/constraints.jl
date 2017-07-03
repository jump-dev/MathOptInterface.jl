# Constraints

"""
    addconstraint!(m::AbstractSolverInstance, func::F, set::S)::ConstraintReference{F,S}

Add the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`,
and ``\\mathcal{S}`` is defined by `set`.

"""
function addconstraint! end

# TODO: method to query if solver supports this type of modification

"""
    getconstraintconstant(m::AbstractSolverInstance, c::ConstraintReference)

Return the ``b`` vector of the constraint `c`.

    getconstraintconstant(m::AbstractSolverInstance, c::ConstraintReference, k::Int)

Return the constant term of the `k`th row of the constraint `c`.
"""
function getconstraintconstant end

"""
    getconstraintaffine(m::AbstractSolverInstance, c::ConstraintReference)

Return the ``A_i`` matrix of the constraint corresponding to `c` in triplet form `(i, v, coef)`, where `v` is a `VariableReference`, and `coef` is a coefficient value.
Output is a tuple of three vectors.

    getconstraintaffine(m::AbstractSolverInstance, c::ConstraintReference, k::Int)

Return the `k`th row of the ``A_k`` matrix of the constraint corresponding to `c` in tuple form `(v, coef)`, where `v` is a `VariableReference`, and `coef` is a coefficient value.
Output is a tuple of two vectors.

    getconstraintaffine(m::AbstractSolverInstance, c::ConstraintReference, k::Int, v::VariableReference)

Return the element of the ``A_k`` matrix of the constraint corresponding to `c` in row `k` and variable `v`.
"""
function getconstraintaffine end

"""
    getconstraintquadratic(m::AbstractSolverInstance, c::ConstraintReference, k::Int)

Return the ``Q_{i,k}`` matrix of the `k`th row of the constraint corresponding to `c` in triplet form `(v_1, v_2, coef)`, where `v_1, v_2` are `VariableReference`s, and `coef` is a coefficient value.
Output is a tuple of three vectors.
The ``Q_{i,k}`` matrix must be symmetric, and only one element is returned.

    getconstraintquadratic(m::AbstractSolverInstance, c::ConstraintReference, k::Int, v_1::VariableReference, v_2::VariableReference)

Return the element corresponding to `(v_1, v_2)` of the ``Q_{i,k}`` matrix of the `k`th row of the constraint corresponding to `c`.
"""
function getconstraintquadratic end

"""
    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, k::Int, args...)

Modify elements of the `k`th row of the constraint `c` depending on the arguments `args`.
The `k`th row will have the form ``q_{i,k}(x) + A_{i,k}^T x + b_{i,k}``.
There are four cases.

## Modify Constant term

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, k::Int, b)

Set the constant term of the `k`th row in the constraint `c` to `b`.

### Examples

```julia
modifyconstraint!(m, c, 1, 1.0)
```

## Modify Linear term

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, k::Int, a_v::Vector{VariableReference}, a_coef)

Set elements given by `a_v` in the linear term of the `k`th row in the constraint `c` to `a_coef`.
Either `a_v` and `a_coef` are both singletons, or they should be collections with equal length.
The behavior of duplicate entries in `a_v` is undefined.

### Examples

```julia
modifyconstraint!(m, c, v, 1.0)
modifyconstraint!(m, c, [v_1, v_2], [1.0, 2.0])
```

## Modify Quadratic term

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference, k::Int, Q_vari, Q_varj, Q_coef)

Set the elements in the quadratic term of the `k`th row of the constraint `c` specified by the triplets `Q_vari, Q_varj, Q_coef`.
Off-diagonal entries will be mirrored.
`Q_vari, Q_varj` should be collections of `VariableReference` objects.
The behavior of duplicate entries is undefined.
If entries for both ``(i,j)`` and ``(j,i)`` are provided, these are considered duplicate terms.

### Examples

```julia
modifyconstraint!(m, c, v_1, v_2, 1.0)
modifyconstraint!(m, c, [v_1, v_2], [v_1, v_1], [1.0, 2.0])
```

## Modify Set

    modifyconstraint!(m::AbstractSolverInstance, c::ConstraintReference{Set}, S::Set)

Change the set of constraint `c` to the new set `S` which should be of the same type as the original set.

### Examples

If `c` is a `ConstraintReference{Interval}`

```julia
modifyconstraint!(m, c, Interval(0, 5))
modifyconstraint!(m, c, NonPositives) # errors
```
"""
function modifyconstraint! end
