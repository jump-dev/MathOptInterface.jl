# Copyright (c) 2013: Iain Dunning, Miles Lubin, and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# This file is adapted from `Ipopt.jl/ext/IpoptMathOptInterfaceExt/utils.jl`.

@enum(
    _FunctionType,
    _kFunctionTypeVariableIndex,
    _kFunctionTypeScalarAffine,
    _kFunctionTypeScalarQuadratic,
)

function _function_type_to_func(::Type{T}, k::_FunctionType) where {T}
    if k == _kFunctionTypeVariableIndex
        return MOI.VariableIndex
    elseif k == _kFunctionTypeScalarAffine
        return MOI.ScalarAffineFunction{T}
    else
        @assert k == _kFunctionTypeScalarQuadratic
        return MOI.ScalarQuadraticFunction{T}
    end
end

_function_info(::MOI.VariableIndex) = _kFunctionTypeVariableIndex
_function_info(::MOI.ScalarAffineFunction) = _kFunctionTypeScalarAffine
_function_info(::MOI.ScalarQuadraticFunction) = _kFunctionTypeScalarQuadratic

@enum(
    _BoundType,
    _kBoundTypeLessThan,
    _kBoundTypeGreaterThan,
    _kBoundTypeEqualTo,
    _kBoundTypeInterval,
)

_set_info(s::MOI.LessThan) = _kBoundTypeLessThan, -Inf, s.upper
_set_info(s::MOI.GreaterThan) = _kBoundTypeGreaterThan, s.lower, Inf
_set_info(s::MOI.EqualTo) = _kBoundTypeEqualTo, s.value, s.value
_set_info(s::MOI.Interval) = _kBoundTypeInterval, s.lower, s.upper

function _bound_type_to_set(::Type{T}, k::_BoundType) where {T}
    if k == _kBoundTypeEqualTo
        return MOI.EqualTo{T}
    elseif k == _kBoundTypeLessThan
        return MOI.LessThan{T}
    elseif k == _kBoundTypeGreaterThan
        return MOI.GreaterThan{T}
    else
        @assert k == _kBoundTypeInterval
        return MOI.Interval{T}
    end
end

"""
    QPBlockData{T}()

A data structure holding an affine or quadratic objective and a block of
affine and quadratic constraints, together with methods to evaluate them
following the [`MOI.AbstractNLPEvaluator`](@ref) callback conventions.

This is a helper for solvers that pass affine and quadratic constraints to
the solver through the same callbacks as an [`MOI.AbstractNLPEvaluator`](@ref)
(for example, Ipopt and MadNLP).

## Parameters

A variable is treated as a parameter if and only if its index is offset by
[`_PARAMETER_OFFSET`](@ref); see [`_is_parameter`](@ref). The value of the
parameter `x` is `parameters[x.value - _PARAMETER_OFFSET]`, following the
indexing of [`ParameterIndex`](@ref), so that `parameters` can alias the
parameter storage of a [`Model`](@ref). The values may be updated freely
between function evaluations.
"""
mutable struct QPBlockData{T}
    objective::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}
    objective_function_type::_FunctionType
    constraints::Vector{
        Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    }
    g_L::Vector{T}
    g_U::Vector{T}
    mult_g::Vector{Union{Nothing,T}}
    function_type::Vector{_FunctionType}
    bound_type::Vector{_BoundType}
    parameters::Vector{T}

    function QPBlockData{T}() where {T}
        return new(
            zero(MOI.ScalarQuadraticFunction{T}),
            _kFunctionTypeScalarAffine,
            Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}}[],
            T[],
            T[],
            Union{Nothing,T}[],
            _FunctionType[],
            _BoundType[],
            T[],
        )
    end
end

"""
    _PARAMETER_OFFSET

The offset of the `MOI.VariableIndex` value of a parameter: the variable
`x` is a parameter if and only if `x.value >= _PARAMETER_OFFSET`, and
`x.value - _PARAMETER_OFFSET` is the value of the corresponding
[`ParameterIndex`](@ref).
"""
const _PARAMETER_OFFSET = 0x00f0000000000000

"""
    _is_parameter(x::MOI.VariableIndex)

Return whether `x` is a parameter, following the [`_PARAMETER_OFFSET`](@ref)
convention.
"""
_is_parameter(x::MOI.VariableIndex) = x.value >= _PARAMETER_OFFSET

_is_parameter(term::MOI.ScalarAffineTerm) = _is_parameter(term.variable)

function _is_parameter(term::MOI.ScalarQuadraticTerm)
    return _is_parameter(term.variable_1) || _is_parameter(term.variable_2)
end

function _value(v::MOI.VariableIndex, x, p::Vector)
    if _is_parameter(v)
        return p[v.value-_PARAMETER_OFFSET]
    end
    return x[v.value]
end

function _eval_function(
    f::MOI.ScalarQuadraticFunction{T},
    x::AbstractVector{T},
    p::Vector{T},
)::T where {T}
    y = f.constant
    for term in f.affine_terms
        y += term.coefficient * _value(term.variable, x, p)
    end
    for term in f.quadratic_terms
        v1 = _value(term.variable_1, x, p)
        v2 = _value(term.variable_2, x, p)
        if term.variable_1 == term.variable_2
            y += term.coefficient * v1 * v2 / 2
        else
            y += term.coefficient * v1 * v2
        end
    end
    return y
end

function _eval_function(
    f::MOI.ScalarAffineFunction{T},
    x::AbstractVector{T},
    p::Vector{T},
)::T where {T}
    y = f.constant
    for term in f.terms
        y += term.coefficient * _value(term.variable, x, p)
    end
    return y
end

function _eval_dense_gradient(
    ∇f::AbstractVector{T},
    f::MOI.ScalarQuadraticFunction{T},
    x::AbstractVector{T},
    p::Vector{T},
)::Nothing where {T}
    for term in f.affine_terms
        if !_is_parameter(term.variable)
            ∇f[term.variable.value] += term.coefficient
        end
    end
    for term in f.quadratic_terms
        if !_is_parameter(term.variable_1)
            v = _value(term.variable_2, x, p)
            ∇f[term.variable_1.value] += term.coefficient * v
        end
        if term.variable_1 != term.variable_2 && !_is_parameter(term.variable_2)
            v = _value(term.variable_1, x, p)
            ∇f[term.variable_2.value] += term.coefficient * v
        end
    end
    return
end

function _eval_dense_gradient(
    ∇f::AbstractVector{T},
    f::MOI.ScalarAffineFunction{T},
    x::AbstractVector{T},
    p::Vector{T},
)::Nothing where {T}
    for term in f.terms
        if !_is_parameter(term.variable)
            ∇f[term.variable.value] += term.coefficient
        end
    end
    return
end

function _append_sparse_gradient_structure!(
    f::MOI.ScalarQuadraticFunction,
    J,
    row,
    p::Vector,
)
    for term in f.affine_terms
        if !_is_parameter(term.variable)
            push!(J, (row, term.variable.value))
        end
    end
    for term in f.quadratic_terms
        if !_is_parameter(term.variable_1)
            push!(J, (row, term.variable_1.value))
        end
        if term.variable_1 != term.variable_2 && !_is_parameter(term.variable_2)
            push!(J, (row, term.variable_2.value))
        end
    end
    return
end

function _append_sparse_gradient_structure!(
    f::MOI.ScalarAffineFunction,
    J,
    row,
    p::Vector,
)
    for term in f.terms
        if !_is_parameter(term.variable)
            push!(J, (row, term.variable.value))
        end
    end
    return
end

function _eval_sparse_gradient(
    ∇f::AbstractVector{T},
    f::MOI.ScalarQuadraticFunction{T},
    x::AbstractVector{T},
    p::Vector{T},
)::Int where {T}
    i = 0
    for term in f.affine_terms
        if !_is_parameter(term.variable)
            i += 1
            ∇f[i] = term.coefficient
        end
    end
    for term in f.quadratic_terms
        if !_is_parameter(term.variable_1)
            v = _value(term.variable_2, x, p)
            i += 1
            ∇f[i] = term.coefficient * v
        end
        if term.variable_1 != term.variable_2 && !_is_parameter(term.variable_2)
            v = _value(term.variable_1, x, p)
            i += 1
            ∇f[i] = term.coefficient * v
        end
    end
    return i
end

function _eval_sparse_gradient(
    ∇f::AbstractVector{T},
    f::MOI.ScalarAffineFunction{T},
    x::AbstractVector{T},
    p::Vector{T},
)::Int where {T}
    i = 0
    for term in f.terms
        if !_is_parameter(term.variable)
            i += 1
            ∇f[i] = term.coefficient
        end
    end
    return i
end

function _append_sparse_hessian_structure!(
    f::MOI.ScalarQuadraticFunction,
    H,
    p::Vector,
)
    for term in f.quadratic_terms
        if _is_parameter(term.variable_1) || _is_parameter(term.variable_2)
            continue
        end
        push!(H, (term.variable_1.value, term.variable_2.value))
    end
    return
end

function _append_sparse_hessian_structure!(
    ::MOI.ScalarAffineFunction,
    H,
    ::Vector,
)
    return nothing
end

function _eval_sparse_hessian(
    ∇²f::AbstractVector{T},
    f::MOI.ScalarQuadraticFunction{T},
    σ::T,
    p::Vector{T},
)::Int where {T}
    i = 0
    for term in f.quadratic_terms
        if _is_parameter(term.variable_1) || _is_parameter(term.variable_2)
            continue
        end
        i += 1
        ∇²f[i] = term.coefficient * σ
    end
    return i
end

function _eval_sparse_hessian(
    ∇²f::AbstractVector{T},
    f::MOI.ScalarAffineFunction{T},
    σ::T,
    p::Vector{T},
)::Int where {T}
    return 0
end

Base.length(block::QPBlockData) = length(block.bound_type)

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ObjectiveFunction{F},
    f::F,
) where {T,F<:Union{MOI.VariableIndex,MOI.ScalarAffineFunction{T}}}
    block.objective = convert(MOI.ScalarAffineFunction{T}, f)
    block.objective_function_type = _function_info(f)
    return
end

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}},
    f::MOI.ScalarQuadraticFunction{T},
) where {T}
    block.objective = f
    block.objective_function_type = _function_info(f)
    return
end

function MOI.get(block::QPBlockData{T}, ::MOI.ObjectiveFunctionType) where {T}
    return _function_type_to_func(T, block.objective_function_type)
end

function MOI.get(block::QPBlockData{T}, ::MOI.ObjectiveFunction{F}) where {T,F}
    return convert(F, block.objective)
end

function MOI.get(
    block::QPBlockData{T},
    ::MOI.ListOfConstraintTypesPresent,
) where {T}
    constraints = Set{Tuple{Type,Type}}()
    for i in 1:length(block)
        F = _function_type_to_func(T, block.function_type[i])
        S = _bound_type_to_set(T, block.bound_type[i])
        push!(constraints, (F, S))
    end
    return collect(constraints)
end

function MOI.is_valid(
    block::QPBlockData{T},
    ci::MOI.ConstraintIndex{F,S},
) where {
    T,
    F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T},MOI.Interval{T}},
}
    return 1 <= ci.value <= length(block)
end

function MOI.get(
    block::QPBlockData{T},
    ::MOI.ListOfConstraintIndices{F,S},
) where {
    T,
    F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T},MOI.Interval{T}},
}
    ret = MOI.ConstraintIndex{F,S}[]
    for i in 1:length(block)
        if _bound_type_to_set(T, block.bound_type[i]) != S
            continue
        elseif _function_type_to_func(T, block.function_type[i]) != F
            continue
        end
        push!(ret, MOI.ConstraintIndex{F,S}(i))
    end
    return ret
end

function MOI.get(
    block::QPBlockData{T},
    ::MOI.NumberOfConstraints{F,S},
) where {
    T,
    F<:Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    S<:Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T},MOI.Interval{T}},
}
    return length(MOI.get(block, MOI.ListOfConstraintIndices{F,S}()))
end

function MOI.add_constraint(
    block::QPBlockData{T},
    f::Union{MOI.ScalarAffineFunction{T},MOI.ScalarQuadraticFunction{T}},
    s::Union{MOI.LessThan{T},MOI.GreaterThan{T},MOI.EqualTo{T},MOI.Interval{T}},
) where {T}
    push!(block.constraints, f)
    bound_type, l, u = _set_info(s)
    push!(block.g_L, l)
    push!(block.g_U, u)
    push!(block.mult_g, nothing)
    push!(block.bound_type, bound_type)
    push!(block.function_type, _function_info(f))
    return MOI.ConstraintIndex{typeof(f),typeof(s)}(length(block.bound_type))
end

function MOI.get(
    block::QPBlockData{T},
    ::MOI.ConstraintFunction,
    c::MOI.ConstraintIndex{F,S},
) where {T,F,S}
    return convert(F, block.constraints[c.value])
end

function MOI.get(
    block::QPBlockData{T},
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F,S},
) where {T,F,S}
    row = c.value
    if block.bound_type[row] == _kBoundTypeEqualTo
        return MOI.EqualTo(block.g_L[row])::S
    elseif block.bound_type[row] == _kBoundTypeLessThan
        return MOI.LessThan(block.g_U[row])::S
    elseif block.bound_type[row] == _kBoundTypeGreaterThan
        return MOI.GreaterThan(block.g_L[row])::S
    else
        @assert block.bound_type[row] == _kBoundTypeInterval
        return MOI.Interval(block.g_L[row], block.g_U[row])::S
    end
end

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F,MOI.LessThan{T}},
    set::MOI.LessThan{T},
) where {T,F}
    block.g_U[c.value] = set.upper
    return
end

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F,MOI.GreaterThan{T}},
    set::MOI.GreaterThan{T},
) where {T,F}
    block.g_L[c.value] = set.lower
    return
end

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F,MOI.EqualTo{T}},
    set::MOI.EqualTo{T},
) where {T,F}
    block.g_L[c.value] = set.value
    block.g_U[c.value] = set.value
    return
end

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ConstraintSet,
    c::MOI.ConstraintIndex{F,MOI.Interval{T}},
    set::MOI.Interval{T},
) where {T,F}
    block.g_L[c.value] = set.lower
    block.g_U[c.value] = set.upper
    return
end

function MOI.get(
    block::QPBlockData{T},
    ::MOI.ConstraintDualStart,
    c::MOI.ConstraintIndex{F,S},
) where {T,F,S}
    return block.mult_g[c.value]
end

function MOI.set(
    block::QPBlockData{T},
    ::MOI.ConstraintDualStart,
    c::MOI.ConstraintIndex{F,S},
    value,
) where {T,F,S}
    block.mult_g[c.value] = value
    return
end

function MOI.eval_objective(
    block::QPBlockData{T},
    x::AbstractVector{T},
) where {T}
    return _eval_function(block.objective, x, block.parameters)
end

function MOI.eval_objective_gradient(
    block::QPBlockData{T},
    ∇f::AbstractVector{T},
    x::AbstractVector{T},
) where {T}
    ∇f .= zero(T)
    _eval_dense_gradient(∇f, block.objective, x, block.parameters)
    return
end

function MOI.eval_constraint(
    block::QPBlockData{T},
    g::AbstractVector{T},
    x::AbstractVector{T},
) where {T}
    for (i, constraint) in enumerate(block.constraints)
        g[i] = _eval_function(constraint, x, block.parameters)
    end
    return
end

function MOI.jacobian_structure(block::QPBlockData)
    J = Tuple{Int,Int}[]
    for (row, constraint) in enumerate(block.constraints)
        _append_sparse_gradient_structure!(constraint, J, row, block.parameters)
    end
    return J
end

function MOI.eval_constraint_jacobian(
    block::QPBlockData{T},
    J::AbstractVector{T},
    x::AbstractVector{T},
) where {T}
    i = 0
    for constraint in block.constraints
        ∇f = view(J, (i+1):length(J))
        i += _eval_sparse_gradient(∇f, constraint, x, block.parameters)
    end
    return
end

function MOI.hessian_lagrangian_structure(block::QPBlockData)
    H = Tuple{Int,Int}[]
    _append_sparse_hessian_structure!(block.objective, H, block.parameters)
    for constraint in block.constraints
        _append_sparse_hessian_structure!(constraint, H, block.parameters)
    end
    return H
end

function MOI.eval_hessian_lagrangian(
    block::QPBlockData{T},
    H::AbstractVector{T},
    x::AbstractVector{T},
    σ::T,
    μ::AbstractVector{T},
) where {T}
    i = _eval_sparse_hessian(H, block.objective, σ, block.parameters)
    for (row, constraint) in enumerate(block.constraints)
        ∇²f = view(H, (i+1):length(H))
        i += _eval_sparse_hessian(∇²f, constraint, μ[row], block.parameters)
    end
    return
end

# The product functions below ACCUMULATE into their output vector, so that
# the contributions of several blocks (for example, the QP block, the
# vector-nonlinear-oracle constraints, and an `MOI.AbstractNLPEvaluator`) can
# be composed into the same output. This is why they are not methods of the
# corresponding `MOI.eval_...` functions, whose contract is to store the
# result: `QPBlockData` is not an `MOI.AbstractNLPEvaluator`, so it does not
# have to define the same interface as evaluators.

function _add_Jv_product(
    f::MOI.ScalarAffineFunction{T},
    y::AbstractVector{T},
    x::AbstractVector{T},
    w::AbstractVector{T},
    p::Vector{T},
    i::Int,
)::Nothing where {T}
    for term in f.terms
        if !_is_parameter(term.variable)
            y[i] += term.coefficient * w[term.variable.value]
        end
    end
    return
end

function _add_Jv_product(
    f::MOI.ScalarQuadraticFunction{T},
    y::AbstractVector{T},
    x::AbstractVector{T},
    w::AbstractVector{T},
    p::Vector{T},
    i::Int,
)::Nothing where {T}
    for term in f.affine_terms
        if !_is_parameter(term.variable)
            y[i] += term.coefficient * w[term.variable.value]
        end
    end
    for term in f.quadratic_terms
        if !_is_parameter(term.variable_1)
            v = _value(term.variable_2, x, p)
            y[i] += term.coefficient * v * w[term.variable_1.value]
        end
        if term.variable_1 != term.variable_2 && !_is_parameter(term.variable_2)
            v = _value(term.variable_1, x, p)
            y[i] += term.coefficient * v * w[term.variable_2.value]
        end
    end
    return
end

function _add_Jtv_product(
    f::MOI.ScalarAffineFunction{T},
    y::AbstractVector{T},
    x::AbstractVector{T},
    w::AbstractVector{T},
    p::Vector{T},
    i::Int,
)::Nothing where {T}
    for term in f.terms
        if !_is_parameter(term.variable)
            y[term.variable.value] += term.coefficient * w[i]
        end
    end
    return
end

function _add_Jtv_product(
    f::MOI.ScalarQuadraticFunction{T},
    y::AbstractVector{T},
    x::AbstractVector{T},
    w::AbstractVector{T},
    p::Vector{T},
    i::Int,
)::Nothing where {T}
    for term in f.affine_terms
        if !_is_parameter(term.variable)
            y[term.variable.value] += term.coefficient * w[i]
        end
    end
    for term in f.quadratic_terms
        if !_is_parameter(term.variable_1)
            v = _value(term.variable_2, x, p)
            y[term.variable_1.value] += term.coefficient * v * w[i]
        end
        if term.variable_1 != term.variable_2 && !_is_parameter(term.variable_2)
            v = _value(term.variable_1, x, p)
            y[term.variable_2.value] += term.coefficient * v * w[i]
        end
    end
    return
end

function _add_Hv_product(
    f::MOI.ScalarQuadraticFunction{T},
    H::AbstractVector{T},
    x::AbstractVector{T},
    v::AbstractVector{T},
    λ::T,
    p::Vector{T},
)::Nothing where {T}
    for term in f.quadratic_terms
        if _is_parameter(term.variable_1) || _is_parameter(term.variable_2)
            continue
        end
        i, j = term.variable_1.value, term.variable_2.value
        H[i] += λ * term.coefficient * v[j]
        if i != j
            H[j] += λ * term.coefficient * v[i]
        end
    end
    return
end

function _add_Hv_product(
    ::MOI.ScalarAffineFunction{T},
    H::AbstractVector{T},
    x::AbstractVector{T},
    v::AbstractVector{T},
    λ::T,
    p::Vector{T},
) where {T}
    return nothing
end

# These are used to add the QP contribution on top of the NL contribution.

"""
    _add_constraint_jacobian_product(
        block::QPBlockData{T},
        y::AbstractVector{T},
        x::AbstractVector{T},
        w::AbstractVector{T},
    )::Nothing where {T}

Add to `y` the product of the Jacobian of the constraints of `block` at `x`
with `w`.

Unlike [`MOI.eval_constraint_jacobian_product`](@ref), this function
accumulates into `y` instead of storing the result, so that the contributions
of several blocks can be composed: the caller is responsible for zeroing `y`
before the first contribution.
"""
function _add_constraint_jacobian_product(
    block::QPBlockData{T},
    y::AbstractVector{T},
    x::AbstractVector{T},
    w::AbstractVector{T},
) where {T}
    for (i, constraint) in enumerate(block.constraints)
        _add_Jv_product(constraint, y, x, w, block.parameters, i)
    end
    return
end

"""
    _add_constraint_jacobian_transpose_product(
        block::QPBlockData{T},
        y::AbstractVector{T},
        x::AbstractVector{T},
        w::AbstractVector{T},
    )::Nothing where {T}

Add to `y` the product of the transpose of the Jacobian of the constraints of
`block` at `x` with `w`.

Unlike [`MOI.eval_constraint_jacobian_transpose_product`](@ref), this
function accumulates into `y` instead of storing the result, so that the
contributions of several blocks can be composed: the caller is responsible
for zeroing `y` before the first contribution.
"""
function _add_constraint_jacobian_transpose_product(
    block::QPBlockData{T},
    y::AbstractVector{T},
    x::AbstractVector{T},
    w::AbstractVector{T},
) where {T}
    for (i, constraint) in enumerate(block.constraints)
        _add_Jtv_product(constraint, y, x, w, block.parameters, i)
    end
    return
end

"""
    _add_hessian_lagrangian_product(
        block::QPBlockData{T},
        H::AbstractVector{T},
        x::AbstractVector{T},
        v::AbstractVector{T},
        σ::T,
        μ::AbstractVector{T},
    )::Nothing where {T}

Add to `H` the product of the Hessian of the Lagrangian of `block` at `x`,
with objective weight `σ` and constraint weights `μ`, with `v`.

Unlike [`MOI.eval_hessian_lagrangian_product`](@ref), this function
accumulates into `H` instead of storing the result, so that the contributions
of several blocks can be composed: the caller is responsible for zeroing `H`
before the first contribution.
"""
function _add_hessian_lagrangian_product(
    block::QPBlockData{T},
    H::AbstractVector{T},
    x::AbstractVector{T},
    v::AbstractVector{T},
    σ::T,
    μ::AbstractVector{T},
) where {T}
    _add_Hv_product(block.objective, H, x, v, σ, block.parameters)
    for (i, constraint) in enumerate(block.constraints)
        _add_Hv_product(constraint, H, x, v, μ[i], block.parameters)
    end
    return
end
