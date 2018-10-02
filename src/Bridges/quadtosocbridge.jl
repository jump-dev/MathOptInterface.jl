using Compat.LinearAlgebra

"""
    QuadtoSOCBridge{T}

The set of points `x` satisfying the constraint
```math
\\frac{1}{2}x^T Q x + a^T x + b \\le 0
```
is a convex set if `Q` is positive semidefinite and is the union of two convex
cones if `a` and `b` are zero (i.e. *homogeneous* case) and `Q` has only one
negative eigenvalue.

## Non-homogeneous case

If `Q` is positive semidefinite, there exists `U` such that ``Q = U^T U``, the
inequality can then be rewritten as
```math
\\|U x\\|_2 \\le 2 (a^T x + b)
```
which is equivalent to the membership of `(1, a^T x + b, Ux)` to the rotated
second-order cone.

## Homogeneous case

If `Q` has only one negative eigenvalue, the set of `x` such that ``x^T Q x \\le
0`` is the union of a convex cone and its opposite. We can choose which one to
model by checking the existence of bounds on variables as shown below.

### Second-order cone

If `Q` is diagonal and has eigenvalues `(1, 1, -1)`, the inequality
``x^2 + x^2 \\le z^2`` combined with ``z \\ge 0`` defines the Lorenz cone (i.e.
the second-order cone) but when combined with ``z \\le 0``, it gives the
opposite of the second order cone. Therefore, we need to check if the variable
`z` has a lower bound 0 or an upper bound 0 in order to determine which cone is

### Rotated second-order cone

The matrix `Q` corresponding to the inequality ``x^2 \\le 2yz`` has one
eigenvalue 1 with eigenvectors `(1, 0, 0)` and `(0, 1, -1)` and one eigenvalue
`-1` corresponding to the eigenvector `(0, 1, 1)`. Hence if we intersect this
union of two convex cone with the halfspace ``x + y \\ge 0``, we get the rotated
second-order cone and if we intersect it with the halfspace ``x + y \\le 0`` we
get the opposite of the rotated second-order cone. Note that `y` and `z` have
the same sign since `yz` is nonnegative hence ``x + y \\ge 0`` is equivalent to
``x \\ge 0`` and ``y \\ge 0``.

### Note

The check for existence of bound can be implemented (but inefficiently) with the
current interface but if bound is removed or transformed (e.g. `≤ 0` transformed
into `≥ 0`) then the bridge is no longer valid. For this reason the homogeneous
version of the bridge is not implemented yet.
"""
struct QuadtoSOCBridge{T} <: AbstractBridge
    soc::CI{MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone}
    dimension::Int  # dimension of the SOC constraint
    less_than::Bool # whether the constraint was ≤ or ≥
    set_constant::T # the constant that was on the set
end
function QuadtoSOCBridge{T}(model, func::MOI.ScalarQuadraticFunction{T},
                            set::Union{MOI.LessThan{T},
                                       MOI.GreaterThan{T}}) where T
    set_constant = MOIU.getconstant(set)
    less_than = set isa MOI.LessThan
    if !less_than
        set_constant = -set_constant
    end
    Q, index_to_variable_map = matrix_from_quadratic_terms(func.quadratic_terms)
    if !less_than
        rmul!(Q, -1)
    end
    U = cholesky(Symmetric(Q)).U
    Ux_terms = matrix_to_vector_affine_terms(U, index_to_variable_map)
    Ux = MOI.VectorAffineFunction(Ux_terms, zeros(T, size(U, 1)))
    t = MOI.ScalarAffineFunction(less_than ? MOIU.operate_terms(-, func.affine_terms) : func.affine_terms,
                                 less_than ? set_constant - func.constant : func.constant - set_constant)
    f = MOIU.operate(vcat, T, one(T), t, Ux)
    dimension = MOI.output_dimension(f)
    soc = MOI.addconstraint!(model, f,
                             MOI.RotatedSecondOrderCone(dimension))
    return QuadtoSOCBridge(soc, dimension, less_than, set_constant)
end

function matrix_from_quadratic_terms(terms::Vector{MOI.ScalarQuadraticTerm{T}}) where T
    variable_to_index_map = Dict{VI, Int}()
    index_to_variable_map = Dict{Int, VI}()
    n = 0
    for term in terms
        for variable in (term.variable_index_1, term.variable_index_2)
            if !(variable in keys(variable_to_index_map))
                n += 1
                variable_to_index_map[variable] = n
                index_to_variable_map[n] = variable
            end
        end
    end
    Q = zeros(T, n, n)
    for term in terms
        i = variable_to_index_map[term.variable_index_1]
        j = variable_to_index_map[term.variable_index_2]
        Q[i, j] += term.coefficient
        if i != j
            Q[j, i] += term.coefficient
        end
    end
    return Q, index_to_variable_map
end

function matrix_to_vector_affine_terms(matrix::AbstractMatrix{T},
                                       index_to_variable_map::Dict{Int, VI}) where T
    terms = MOI.VectorAffineTerm{T}[]
    for row in 1:size(matrix, 1)
        for col in 1:size(matrix, 2)
            if !iszero(matrix[row, col])
                push!(terms, MOI.VectorAffineTerm(row,
                                                  MOI.ScalarAffineTerm(matrix[row, col],
                                                                       index_to_variable_map[col])))
            end
        end
    end
    return terms
end

function MOI.supports_constraint(::Type{QuadtoSOCBridge{T}},
                                 ::Type{MOI.ScalarQuadraticFunction{T}},
                                 ::Type{<:Union{MOI.LessThan{T},
                                                MOI.GreaterThan{T}}}) where T
    return true
end
function added_constraint_types(::Type{QuadtoSOCBridge{T}}) where T
    return [(MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone)]
end
function concrete_bridge_type(::Type{<:QuadtoSOCBridge{T}},
                              ::Type{MOI.ScalarQuadraticFunction{T}},
                              ::Type{<:Union{MOI.LessThan{T},
                                               MOI.GreaterThan{T}}}) where T
    return QuadtoSOCBridge{T}
end

# Attributes, Bridge acting as an model
function MOI.get(::QuadtoSOCBridge{T},
                 ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T},
                                           MOI.RotatedSecondOrderCone}) where T
    return 1
end
function MOI.get(bridge::QuadtoSOCBridge{T},
                 ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T},
                                               MOI.RotatedSecondOrderCone}) where T
    return [bridge.soc]
end

# References
function MOI.delete(model::MOI.ModelLike, bridge::QuadtoSOCBridge)
    MOI.delete(model, bridge.soc)
end

# Attributes, Bridge acting as a constraint
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal,
                 bridge::QuadtoSOCBridge)
    soc = MOI.get(model, attr, bridge.soc)
    output = sum(soc[i]^2 for i in 3:bridge.dimension)
    output /= 2
    output -= soc[1] * soc[2]
    if !bridge.less_than
        output = -output
    end
    output += bridge.set_constant
    return output
end
