# Constraints

"""
    addconstraint!(m::AbstractModel, b, a_constridx, a_v::Vector{VariableReference}, a_coef, Q_constridx, Q_vari::Vector{VariableReference}, Q_varj::Vector{VariableReference}, Q_coef, S::AbstractSet)::QuadraticConstraintReference{typeof(S)}

Add the vector quadratic-in-set constraint ``q_i(x) + A_i^T x + b_i \\in \\mathcal{S}_i``, where:
* ``A_i`` is a sparse matrix specified in triplet form by `a_constridx, a_v, a_coef`
* ``b_i`` is a vector specified by `b`
* ``q_i(x)`` is a vector with component ``(q_i(x))_k`` defined as ``\\frac{1}{2} x^T Q_{i,k} x``, where each symmetric matrix ``Q_{i,k}`` has `Q_constridx` equal to `k` and is specified in triplet form by `Q_vari, Q_varj, Q_coef`
* ``\\mathcal{S}_i`` is a pre-defined set specified as `S`

Duplicate indices in either the ``A_i`` matrix or any ``Q_{i,k}`` matrix are accepted and will be summed together.
Off-diagonal entries of ``Q_{i,k}`` will be mirrored, so either the upper triangular or lower triangular entries of ``Q_{i,k}`` should be provided.
If entries for both ``(i,j)`` and ``(j,i)`` are provided, these are considered duplicate terms.
`a_v`, `Q_vari`, `Q_varj` should be collections of `VariableReference` objects.

    addconstraint!(m::AbstractModel, b, a_v::Vector{VariableReference}, a_coef, Q_vari::Vector{VariableReference}, Q_varj::Vector{VariableReference}, Q_coef, S::AbstractSet)::QuadraticConstraintReference{typeof(S)}

Add the scalar quadratic-in-set constraint ``q_i(x) + a_i^T x + b_i \\in \\mathcal{S}_i``, where:
* ``a_i`` is a sparse vector specified in tuple form by `a_v, a_coef`
* ``b_i`` is a scalar specified by `b`
* ``q_i(x)`` is defined as ``\\frac{1}{2} x^T Q_{i} x``, where the symmetric matrix ``Q_{i}`` is specified in triplet form by `Q_vari, Q_varj, Q_coef`
* ``\\mathcal{S}_i`` is a pre-defined *scalar* set specified as `S`

Duplicate indices in the ``a_i`` or the ``Q_{i,k}`` are accepted and will be summed together.

    addconstraint!(m::AbstractModel, b, a_constridx, a_v::Vector{VariableReference}, a_coef, S::AbstractSet)::AffineConstraintReference{typeof(S)}

Add the vector affine-in-set constraint ``A_i^T x + b_i \\in \\mathcal{S}_i``, where:
* ``A_i`` is a sparse matrix specified in triplet form by `a_constridx, a_v, a_coef`
* ``b_i`` is a vector specified by `b`
* ``\\mathcal{S}_i`` is a pre-defined set specified as `S`

Duplicate indices in the ``A_i`` are accepted and will be summed together.

    addconstraint!(m::AbstractModel, b, a_v::Vector{VariableReference}, a_coef, S::AbstractSet)::AffineConstraintReference{typeof(S)}

Add the scalar affine-in-set constraint ``a_i^T x + b_i \\in \\mathcal{S}_i``, where:
* ``a_i`` is a sparse vector specified in tuple form by `a_v, a_coef`
* ``b_i`` is a scalar specified by `b`
* ``\\mathcal{S}_i`` is a pre-defined *scalar* set specified as `S`

Duplicate indices in the ``a_i`` are accepted and will be summed together.

    addconstraint!(m::AbstractModel, vs::Vector{VariableReference}, S::AbstractSet)::VariablewiseConstraintReference{typeof(S)}

Add the vector variable-wise constraint ``(x_j)_{j \\in v_i} \\in \\mathcal{S}_i``, where:
* ``v_i`` is a list of variable indices specified as a vector of variable references `vs`
* ``\\mathcal{S}_i`` is a pre-defined set specified as `S`

Behavior is not defined for duplicate indices in the ``v_i``.

    addconstraint!(m::AbstractModel, v::VariableReference, S::AbstractSet)::VariablewiseConstraintReference{typeof(S)}

Add the scalar variable-wise constraint ``x_j \\in \\mathcal{S}_i``, where:
* ``x_j`` is variable specified as a variable reference `v`
* ``\\mathcal{S}_i`` is a pre-defined *scalar* set specified as `S`
"""
function addconstraint! end

# TODO: method to query if solver supports this type of modification

"""
    getconstraintconstant(m::AbstractModel, c::ConstraintReference)

Return the ``b`` vector of the constraint `c`.

    getconstraintconstant(m::AbstractModel, c::ConstraintReference, k::Int)

Return the constant term of the `k`th row of the constraint `c`.
"""
function getconstraintconstant end

"""
    getconstraintaffine(m::AbstractModel, c::ConstraintReference)

Return the ``A_i`` matrix of the constraint corresponding to `c` in triplet form `(i, v, coef)`, where `v` is a `VariableReference`, and `coef` is a coefficient value.
Output is a tuple of three vectors.

    getconstraintaffine(m::AbstractModel, c::ConstraintReference, k::Int)

Return the `k`th row of the ``A_k`` matrix of the constraint corresponding to `c` in tuple form `(v, coef)`, where `v` is a `VariableReference`, and `coef` is a coefficient value.
Output is a tuple of two vectors.

    getconstraintaffine(m::AbstractModel, c::ConstraintReference, k::Int, v::VariableReference)

Return the element of the ``A_k`` matrix of the constraint corresponding to `c` in row `k` and variable `v`.
"""
function getconstraintaffine end

"""
    getconstraintquadratic(m::AbstractModel, c::ConstraintReference, k::Int)

Return the ``Q_{i,k}`` matrix of the `k`th row of the constraint corresponding to `c` in triplet form `(v_1, v_2, coef)`, where `v_1, v_2` are `VariableReference`s, and `coef` is a coefficient value.
Output is a tuple of three vectors.
The ``Q_{i,k}`` matrix must be symmetric, and only one element is returned.

    getconstraintquadratic(m::AbstractModel, c::ConstraintReference, k::Int, v_1::VariableReference, v_2::VariableReference)

Return the element corresponding to `(v_1, v_2)` of the ``Q_{i,k}`` matrix of the `k`th row of the constraint corresponding to `c`.
"""
function getconstraintquadratic end

"""
    modifyconstraint!(m::AbstractModel, c::ConstraintReference, k::Int, args...)

Modify elements of the `k`th row of the constraint `c` depending on the arguments `args`.
The `k`th row will have the form ``q_{i,k}(x) + A_{i,k}^T x + b_{i,k}``.
There are four cases.

## Modify Constant term

    modifyconstraint!(m::AbstractModel, c::ConstraintReference, k::Int, b)

Set the constant term of the `k`th row in the constraint `c` to `b`.

### Examples

```julia
modifyconstraint!(m, c, 1, 1.0)
```

## Modify Linear term

    modifyconstraint!(m::AbstractModel, c::ConstraintReference, k::Int, a_v::Vector{VariableReference}, a_coef)

Set elements given by `a_v` in the linear term of the `k`th row in the constraint `c` to `a_coef`.
Either `a_v` and `a_coef` are both singletons, or they should be collections with equal length.
The behavior of duplicate entries in `a_v` is undefined.

### Examples

```julia
modifyconstraint!(m, c, v, 1.0)
modifyconstraint!(m, c, [v_1, v_2], [1.0, 2.0])
```

## Modify Quadratic term

    modifyconstraint!(m::AbstractModel, c::ConstraintReference, k::Int, Q_vari, Q_varj, Q_coef)

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

    modifyconstraint!(m::AbstractModel, c::ConstraintReference{Set}, S::Set)

Change the set of constraint `c` to the new set `S` which should be of the same type as the original set.

### Examples

If `c` is a `ConstraintReference{Interval}`

```julia
modifyconstraint!(m, c, Interval(0, 5))
modifyconstraint!(m, c, NonPositives) # errors
```
"""
function modifyconstraint! end
