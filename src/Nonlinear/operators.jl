# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

function _create_binary_switch(ids, exprs)
    if length(exprs) <= 3
        out = Expr(:if, Expr(:call, :(==), :id, ids[1]), exprs[1])
        if length(exprs) > 1
            push!(out.args, _create_binary_switch(ids[2:end], exprs[2:end]))
        end
        return out
    end
    mid = length(exprs) >>> 1
    return Expr(
        :if,
        Expr(:call, :(<=), :id, ids[mid]),
        _create_binary_switch(ids[1:mid], exprs[1:mid]),
        _create_binary_switch(ids[(mid+1):end], exprs[(mid+1):end]),
    )
end

function _generate_eval_univariate()
    exprs = map(SYMBOLIC_UNIVARIATE_EXPRESSIONS) do arg
        return :(return $(arg[1])(x), $(arg[2]))
    end
    return _create_binary_switch(1:length(exprs), exprs)
end

@eval @inline function _eval_univariate(id, x::T) where {T}
    $(_generate_eval_univariate())
    return error("Invalid id for univariate operator: $id")
end

function _generate_eval_univariate_2nd_deriv()
    exprs = map(arg -> :(return $(arg[3])), SYMBOLIC_UNIVARIATE_EXPRESSIONS)
    return _create_binary_switch(1:length(exprs), exprs)
end

@eval @inline function _eval_univariate_2nd_deriv(id, x::T) where {T}
    $(_generate_eval_univariate_2nd_deriv())
    return error("Invalid id for univariate operator: $id")
end

struct _UnivariateOperator{F,F′,F′′}
    f::F
    f′::F′
    f′′::F′′
    function _UnivariateOperator(
        f::Function,
        f′::Function,
        f′′::Union{Nothing,Function} = nothing,
    )
        return new{typeof(f),typeof(f′),typeof(f′′)}(f, f′, f′′)
    end
end

function eval_univariate_function(operator::_UnivariateOperator, x::T) where {T}
    ret = operator.f(x)
    check_return_type(T, ret)
    return ret::T
end

function eval_univariate_gradient(operator::_UnivariateOperator, x::T) where {T}
    ret = operator.f′(x)
    check_return_type(T, ret)
    return ret::T
end

function eval_univariate_hessian(operator::_UnivariateOperator, x::T) where {T}
    ret = operator.f′′(x)
    check_return_type(T, ret)
    return ret::T
end

function eval_univariate_function_and_gradient(
    operator::_UnivariateOperator,
    x::T,
) where {T}
    ret_f = eval_univariate_function(operator, x)
    ret_f′ = eval_univariate_gradient(operator, x)
    return ret_f, ret_f′
end

struct _MultivariateOperator{F,F′,F′′}
    N::Int
    f::F
    ∇f::F′
    ∇²f::F′′
    function _MultivariateOperator{N}(
        f::Function,
        ∇f::Function,
        ∇²f::Union{Nothing,Function} = nothing,
    ) where {N}
        return new{typeof(f),typeof(∇f),typeof(∇²f)}(N, f, ∇f, ∇²f)
    end
end

"""
    DEFAULT_UNIVARIATE_OPERATORS

The list of univariate operators that are supported by default.

## Example

```jldoctest
julia> MOI.Nonlinear.DEFAULT_UNIVARIATE_OPERATORS
73-element Vector{Symbol}:
 :+
 :-
 :abs
 :sign
 :sqrt
 :cbrt
 :abs2
 :inv
 :log
 :log10
 ⋮
 :airybi
 :airyaiprime
 :airybiprime
 :besselj0
 :besselj1
 :bessely0
 :bessely1
 :erfcx
 :dawson
```
"""
const DEFAULT_UNIVARIATE_OPERATORS = first.(SYMBOLIC_UNIVARIATE_EXPRESSIONS)

"""
    DEFAULT_MULTIVARIATE_OPERATORS

The list of multivariate operators that are supported by default.

## Example

```jldoctest
julia> MOI.Nonlinear.DEFAULT_MULTIVARIATE_OPERATORS
14-element Vector{Symbol}:
 :+
 :-
 :*
 :^
 :/
 :ifelse
 :atan
 :min
 :max
 :vect
 :dot
 :hcat
 :vcat
 :norm
```
"""
const DEFAULT_MULTIVARIATE_OPERATORS =
    [:+, :-, :*, :^, :/, :ifelse, :atan, :min, :max, :vect, :dot, :hcat, :vcat, :norm]

"""
    OperatorRegistry()

Create a new `OperatorRegistry` to store and evaluate univariate and
multivariate operators.
"""
struct OperatorRegistry
    # NODE_CALL_UNIVARIATE
    univariate_operators::Vector{Symbol}
    univariate_operator_to_id::Dict{Symbol,Int}
    univariate_user_operator_start::Int
    registered_univariate_operators::Vector{_UnivariateOperator}
    # NODE_CALL_MULTIVARIATE
    multivariate_operators::Vector{Symbol}
    multivariate_operator_to_id::Dict{Symbol,Int}
    multivariate_user_operator_start::Int
    registered_multivariate_operators::Vector{_MultivariateOperator}
    # NODE_LOGIC
    logic_operators::Vector{Symbol}
    logic_operator_to_id::Dict{Symbol,Int}
    # NODE_COMPARISON
    comparison_operators::Vector{Symbol}
    comparison_operator_to_id::Dict{Symbol,Int}
    function OperatorRegistry()
        univariate_operators = copy(DEFAULT_UNIVARIATE_OPERATORS)
        multivariate_operators = copy(DEFAULT_MULTIVARIATE_OPERATORS)
        logic_operators = [:&&, :||]
        comparison_operators = [:<=, :(==), :>=, :<, :>]
        return new(
            # NODE_CALL_UNIVARIATE
            univariate_operators,
            Dict{Symbol,Int}(
                op => i for (i, op) in enumerate(univariate_operators)
            ),
            length(univariate_operators),
            _UnivariateOperator[],
            # NODE_CALL
            multivariate_operators,
            Dict{Symbol,Int}(
                op => i for (i, op) in enumerate(multivariate_operators)
            ),
            length(multivariate_operators),
            _MultivariateOperator[],
            # NODE_LOGIC
            logic_operators,
            Dict{Symbol,Int}(op => i for (i, op) in enumerate(logic_operators)),
            # NODE_COMPARISON
            comparison_operators,
            Dict{Symbol,Int}(
                op => i for (i, op) in enumerate(comparison_operators)
            ),
        )
    end
end

function MOI.get(
    registry::OperatorRegistry,
    ::MOI.ListOfSupportedNonlinearOperators,
)
    ops = vcat(
        registry.univariate_operators,
        registry.multivariate_operators,
        registry.logic_operators,
        registry.comparison_operators,
    )
    return unique(ops)
end

const _FORWARD_DIFF_METHOD_ERROR_HELPER = raw"""
Common reasons for this include:

 * The function takes `f(x::Vector)` as input, instead of the splatted
   `f(x...)`.
 * The function assumes `Float64` will be passed as input, it must work for any
   generic `Real` type.
 * The function allocates temporary storage using `zeros(3)` or similar. This
   defaults to `Float64`, so use `zeros(T, 3)` instead.

As examples, instead of:
```julia
my_function(x::Vector) = sum(x.^2)
```
use:
```julia
my_function(x::T...) where {T<:Real} = sum(x[i]^2 for i in 1:length(x))
```

Instead of:
```julia
function my_function(x::Float64...)
    y = zeros(length(x))
    for i in 1:length(x)
        y[i] = x[i]^2
    end
    return sum(y)
end
```
use:
```julia
function my_function(x::T...) where {T<:Real}
    y = zeros(T, length(x))
    for i in 1:length(x)
        y[i] = x[i]^2
    end
    return sum(y)
end
```

Review the stacktrace below for more information, but it can often be hard to
understand why and where your function is failing. You can also debug this
outside of JuMP as follows:
```julia
import ForwardDiff

# If the input dimension is 1
x = 1.0
my_function(a) = a^2
ForwardDiff.derivative(my_function, x)

# If the input dimension is more than 1
x = [1.0, 2.0]
my_function(a, b) = a^2 + b^2
ForwardDiff.gradient(x -> my_function(x...), x)
```
"""

_intercept_ForwardDiff_MethodError(err, ::Symbol) = rethrow(err)

function _intercept_ForwardDiff_MethodError(::MethodError, op::Symbol)
    return error(
        "JuMP's autodiff of the user-defined function $(op) failed with a " *
        "MethodError.\n\n$(_FORWARD_DIFF_METHOD_ERROR_HELPER)",
    )
end

function _checked_derivative(f::F, op::Symbol) where {F}
    return function (x)
        try
            return ForwardDiff.derivative(f, x)
        catch err
            _intercept_ForwardDiff_MethodError(err, op)
        end
    end
end

"""
    _validate_register_assumptions(
        f::Function,
        name::Symbol,
        dimension::Integer,
    )

A function that attempts to check if `f` is suitable for registration via
[`register`](@ref) and throws an informative error if it is not.

Because we don't know the domain of `f`, this function may encounter false
negatives. But it should catch the majority of cases in which users supply
non-differentiable functions that rely on `::Float64` assumptions.
"""
function _validate_register_assumptions(
    f::Function,
    name::Symbol,
    dimension::Integer,
)
    # Assumption 1: check that `f` can be called with `Float64` arguments.
    y = 0.0
    try
        if dimension == 1
            y = f(0.0)
        else
            y = f(zeros(dimension)...)
        end
    catch
        # We hit some other error, perhaps we called a function like log(-1).
        # Ignore for now, and hope that a useful error is shown to the user
        # during the solve.
    end
    if !(y isa Real)
        error(
            "Expected return type of `Float64` from the user-defined " *
            "function :$(name), but got `$(typeof(y))`.",
        )
    end
    # Assumption 2: check that `f` can be differentiated using `ForwardDiff`.
    try
        if dimension == 1
            ForwardDiff.derivative(f, 0.0)
        else
            ForwardDiff.gradient(x -> f(x...), zeros(dimension))
        end
    catch err
        if err isa MethodError
            error(
                "Unable to register the function :$name.\n\n" *
                _FORWARD_DIFF_METHOD_ERROR_HELPER,
            )
        end
        # We hit some other error, perhaps we called a function like log(-1).
        # Ignore for now, and hope that a useful error is shown to the user
        # during the solve.
    end
    return
end

function _UnivariateOperator(op::Symbol, f::Function)
    _validate_register_assumptions(f, op, 1)
    f′ = _checked_derivative(f, op)
    return _UnivariateOperator(op, f, f′)
end

function _UnivariateOperator(op::Symbol, f::Function, f′::Function)
    try
        _validate_register_assumptions(f′, op, 1)
        f′′ = _checked_derivative(f′, op)
        return _UnivariateOperator(f, f′, f′′)
    catch
        return _UnivariateOperator(f, f′, nothing)
    end
end

function _UnivariateOperator(::Symbol, f::Function, f′::Function, f′′::Function)
    return _UnivariateOperator(f, f′, f′′)
end

function _MultivariateOperator{N}(op::Symbol, f::Function) where {N}
    _validate_register_assumptions(f, op, N)
    g = x -> f(x...)
    ∇f = function (ret, x)
        try
            ForwardDiff.gradient!(ret, g, x)
        catch err
            _intercept_ForwardDiff_MethodError(err, op)
        end
        return
    end
    return _MultivariateOperator{N}(g, ∇f)
end

function _MultivariateOperator{N}(::Symbol, f::Function, ∇f::Function) where {N}
    return _MultivariateOperator{N}(x -> f(x...), (g, x) -> ∇f(g, x...))
end

function _MultivariateOperator{N}(
    ::Symbol,
    f::Function,
    ∇f::Function,
    ∇²f::Function,
) where {N}
    return _MultivariateOperator{N}(
        x -> f(x...),
        (g, x) -> ∇f(g, x...),
        (H, x) -> ∇²f(H, x...),
    )
end

function register_operator(
    registry::OperatorRegistry,
    op::Symbol,
    nargs::Int,
    f::Function...,
)
    if nargs == 1
        if haskey(registry.univariate_operator_to_id, op)
            error("Operator $op is already registered.")
        elseif haskey(registry.multivariate_operator_to_id, op)
            error("Operator $op is already registered.")
        end
        operator = _UnivariateOperator(op, f...)
        push!(registry.univariate_operators, op)
        push!(registry.registered_univariate_operators, operator)
        registry.univariate_operator_to_id[op] =
            length(registry.univariate_operators)
    else
        if haskey(registry.multivariate_operator_to_id, op)
            error("Operator $op is already registered.")
        elseif haskey(registry.univariate_operator_to_id, op)
            error("Operator $op is already registered.")
        end
        operator = _MultivariateOperator{nargs}(op, f...)
        push!(registry.multivariate_operators, op)
        push!(registry.registered_multivariate_operators, operator)
        registry.multivariate_operator_to_id[op] =
            length(registry.multivariate_operators)
    end
    return
end

function _is_registered(registry::OperatorRegistry, op::Symbol, nargs::Int)
    if op in (:<=, :>=, :(==), :<, :>, :&&, :||)
        return true
    end
    if nargs == 1 && haskey(registry.univariate_operator_to_id, op)
        return true
    end
    return haskey(registry.multivariate_operator_to_id, op)
end

function _warn_auto_register(op::Symbol, nargs::Int)
    @warn("""Function $op automatically registered with $nargs arguments.

    Calling the function with a different number of arguments will result in an
    error.

    While you can safely ignore this warning, we recommend that you manually
    register the function as follows:
    ```Julia
    model = Model()
    register(model, :$op, $nargs, $op; autodiff = true)
    ```""")
    return
end

"""
    register_operator_if_needed(
        registry::OperatorRegistry,
        op::Symbol,
        nargs::Int,
        f::Function;
    )

Similar to [`register_operator`](@ref), but this function warns if the function
is not registered, and skips silently if it already is.
"""
function register_operator_if_needed(
    registry::OperatorRegistry,
    op::Symbol,
    nargs::Int,
    f::Function,
)
    if !_is_registered(registry, op, nargs)
        register_operator(registry, op, nargs, f)
        _warn_auto_register(op, nargs)
    end
    return
end

"""
    assert_registered(registry::OperatorRegistry, op::Symbol, nargs::Int)

Throw an error if `op` is not registered in `registry` with `nargs` arguments.
"""
function assert_registered(registry::OperatorRegistry, op::Symbol, nargs::Int)
    if !_is_registered(registry, op, nargs)
        msg = """
        Unrecognized function \"$(op)\" used in nonlinear expression.

        You must register it as a user-defined function before building
        the model. For example, replacing `N` with the appropriate number
        of arguments, do:
        ```julia
        model = Model()
        register(model, :$(op), N, $(op), autodiff=true)
        # ... variables and constraints ...
        ```
        """
        error(msg)
    end
    return
end

"""
    check_return_type(::Type{T}, ret::S) where {T,S}

Overload this method for new types `S` to throw an informative error if a
user-defined function returns the type `S` instead of `T`.
"""
check_return_type(::Type{T}, ret::T) where {T} = nothing

function check_return_type(::Type{T}, ret) where {T}
    return error(
        "Expected return type of $T from a user-defined function, but got " *
        "$(typeof(ret)).",
    )
end

"""
    eval_univariate_function(
        registry::OperatorRegistry,
        op::Union{Symbol,Integer},
        x::T,
    ) where {T}

Evaluate the operator `op(x)::T`, where `op` is a univariate function in
`registry`.

If `op isa Integer`, then `op` is the index in
`registry.univariate_operators[op]`.

## Example

```jldoctest
julia> r = MOI.Nonlinear.OperatorRegistry();

julia> MOI.Nonlinear.eval_univariate_function(r, :abs, -1.2)
1.2

julia> r.univariate_operators[3]
:abs

julia> MOI.Nonlinear.eval_univariate_function(r, 3, -1.2)
1.2
```
"""
function eval_univariate_function(
    registry::OperatorRegistry,
    op::Symbol,
    x::T,
) where {T}
    id = registry.univariate_operator_to_id[op]
    return eval_univariate_function(registry, id, x)
end

function eval_univariate_function(
    registry::OperatorRegistry,
    id::Integer,
    x::T,
) where {T}
    if id <= registry.univariate_user_operator_start
        f, _ = _eval_univariate(id, x)
        return f::T
    end
    offset = id - registry.univariate_user_operator_start
    operator = registry.registered_univariate_operators[offset]
    return eval_univariate_function(operator, x)
end

"""
    eval_univariate_gradient(
        registry::OperatorRegistry,
        op::Union{Symbol,Integer},
        x::T,
    ) where {T}

Evaluate the first-derivative of the operator `op(x)::T`, where `op` is a
univariate function in `registry`.

If `op isa Integer`, then `op` is the index in
`registry.univariate_operators[op]`.

## Example

```jldoctest
julia> r = MOI.Nonlinear.OperatorRegistry();

julia> MOI.Nonlinear.eval_univariate_gradient(r, :abs, -1.2)
-1.0

julia> r.univariate_operators[3]
:abs

julia> MOI.Nonlinear.eval_univariate_gradient(r, 3, -1.2)
-1.0
```
"""
function eval_univariate_gradient(
    registry::OperatorRegistry,
    op::Symbol,
    x::T,
) where {T}
    id = registry.univariate_operator_to_id[op]
    return eval_univariate_gradient(registry, id, x)
end

function eval_univariate_gradient(
    registry::OperatorRegistry,
    id::Integer,
    x::T,
) where {T}
    if id <= registry.univariate_user_operator_start
        _, f′ = _eval_univariate(id, x)
        return f′::T
    end
    offset = id - registry.univariate_user_operator_start
    operator = registry.registered_univariate_operators[offset]
    return eval_univariate_gradient(operator, x)
end

"""
    eval_univariate_function_and_gradient(
        registry::OperatorRegistry,
        op::Union{Symbol,Integer},
        x::T,
    )::Tuple{T,T} where {T}

Evaluate the function and first-derivative of the operator `op(x)::T`, where
`op` is a univariate function in `registry`.

If `op isa Integer`, then `op` is the index in
`registry.univariate_operators[op]`.

## Example

```jldoctest
julia> r = MOI.Nonlinear.OperatorRegistry();

julia> MOI.Nonlinear.eval_univariate_function_and_gradient(r, :abs, -1.2)
(1.2, -1.0)

julia> r.univariate_operators[3]
:abs

julia> MOI.Nonlinear.eval_univariate_function_and_gradient(r, 3, -1.2)
(1.2, -1.0)
```
"""
function eval_univariate_function_and_gradient(
    registry::OperatorRegistry,
    op::Symbol,
    x::T,
) where {T}
    id = registry.univariate_operator_to_id[op]
    return eval_univariate_function_and_gradient(registry, id, x)
end

function eval_univariate_function_and_gradient(
    registry::OperatorRegistry,
    id::Integer,
    x::T,
) where {T}
    if id <= registry.univariate_user_operator_start
        return _eval_univariate(id, x)::Tuple{T,T}
    end
    offset = id - registry.univariate_user_operator_start
    operator = registry.registered_univariate_operators[offset]
    return eval_univariate_function_and_gradient(operator, x)
end

"""
    eval_univariate_hessian(
        registry::OperatorRegistry,
        op::Union{Symbol,Integer},
        x::T,
    ) where {T}

Evaluate the second-derivative of the operator `op(x)::T`, where `op` is a
univariate function in `registry`.

If `op isa Integer`, then `op` is the index in
`registry.univariate_operators[op]`.

## Example

```jldoctest
julia> r = MOI.Nonlinear.OperatorRegistry();

julia> MOI.Nonlinear.eval_univariate_hessian(r, :sin, 1.0)
-0.8414709848078965

julia> r.univariate_operators[16]
:sin

julia> MOI.Nonlinear.eval_univariate_hessian(r, 16, 1.0)
-0.8414709848078965

julia> -sin(1.0)
-0.8414709848078965
```
"""
function eval_univariate_hessian(
    registry::OperatorRegistry,
    op::Symbol,
    x::T,
) where {T}
    id = registry.univariate_operator_to_id[op]
    return eval_univariate_hessian(registry, id, x)
end

function eval_univariate_hessian(
    registry::OperatorRegistry,
    id::Integer,
    x::T,
) where {T}
    if id <= registry.univariate_user_operator_start
        ret = _eval_univariate_2nd_deriv(id, x)
        if ret === nothing
            op = registry.univariate_operators[id]
            error("Hessian is not defined for operator $op")
        end
        return ret::T
    end
    offset = id - registry.univariate_user_operator_start
    operator = registry.registered_univariate_operators[offset]
    return eval_univariate_hessian(operator, x)
end

"""
    _nan_pow(x, y)

An alternative for `x^y` that avoids throwing an error in common situations like
`(-1.0)^1.5`. This optimization applies only to Float32 and Float64 inputs; if
a different type is provided as input we fallback to `x^y`.
"""
_nan_pow(x::T, y::T) where {T<:Union{Float32,Float64}} = pow(x, y)
_nan_pow(x, y) = x^y

"""
    eval_multivariate_function(
        registry::OperatorRegistry,
        op::Symbol,
        x::AbstractVector{T},
    ) where {T}

Evaluate the operator `op(x)::T`, where `op` is a multivariate function in
`registry`.
"""
function eval_multivariate_function(
    registry::OperatorRegistry,
    op::Symbol,
    x::AbstractVector{T},
) where {T}
    if op == :+
        return sum(x; init = zero(T))
    elseif op == :-
        @assert length(x) == 2
        return x[1] - x[2]
    elseif op == :*
        return prod(x; init = one(T))
    elseif op == :^
        @assert length(x) == 2
        # Use _nan_pow here to avoid throwing an error in common situations like
        # (-1.0)^1.5.
        return _nan_pow(x[1], x[2])
    elseif op == :/
        @assert length(x) == 2
        return x[1] / x[2]
    elseif op == :ifelse
        @assert length(x) == 3
        return ifelse(Bool(x[1]), x[2], x[3])
    elseif op == :atan
        @assert length(x) == 2
        return atan(x[1], x[2])
    elseif op == :min
        return minimum(x)
    elseif op == :max
        return maximum(x)
    elseif op == :vect
        return x
    end
    id = registry.multivariate_operator_to_id[op]
    offset = id - registry.multivariate_user_operator_start
    operator = registry.registered_multivariate_operators[offset]
    @assert length(x) == operator.N
    ret = operator.f(x)
    check_return_type(T, ret)
    return ret::T
end

"""
    eval_multivariate_gradient(
        registry::OperatorRegistry,
        op::Symbol,
        g::AbstractVector{T},
        x::AbstractVector{T},
    ) where {T}

Evaluate the gradient of operator `g .= ∇op(x)`, where `op` is a multivariate
function in `registry`.
"""
function eval_multivariate_gradient(
    registry::OperatorRegistry,
    op::Symbol,
    g::AbstractVector{T},
    x::AbstractVector{T},
) where {T}
    @assert length(g) == length(x)
    if op == :+
        fill!(g, one(T))
    elseif op == :-
        g[1] = one(T)
        g[2] = -one(T)
    elseif op == :*
        # Special case performance optimizations for common cases.
        if length(x) == 1
            g[1] = one(T)
        elseif length(x) == 2
            g[1] = x[2]
            g[2] = x[1]
        else
            total = prod(x)
            if iszero(total)
                for i in eachindex(x)
                    g[i] = prod(x[j] for j in eachindex(x) if i != j)
                end
            else
                for i in eachindex(x)
                    g[i] = total / x[i]
                end
            end
        end
    elseif op == :^
        @assert length(x) == 2
        if x[2] == one(T)
            g[1] = one(T)
        elseif x[2] == T(2)
            g[1] = T(2) * x[1]
        else
            g[1] = x[2] * _nan_pow(x[1], x[2] - one(T))
        end
        if x[1] > zero(T)
            g[2] = _nan_pow(x[1], x[2]) * log(x[1])
        else
            g[2] = T(NaN)
        end
    elseif op == :/
        @assert length(x) == 2
        g[1] = one(T) / x[2]
        g[2] = -x[1] / x[2]^2
    elseif op == :ifelse
        @assert length(x) == 3
        g[1] = zero(T)  # It doesn't matter what this is.
        g[2] = x[1] == one(T)
        g[3] = x[1] == zero(T)
    elseif op == :atan
        @assert length(x) == 2
        base = x[1]^2 + x[2]^2
        g[1] = x[2] / base
        g[2] = -x[1] / base
    elseif op == :min
        fill!(g, zero(T))
        _, i = findmin(x)
        g[i] = one(T)
    elseif op == :max
        fill!(g, zero(T))
        _, i = findmax(x)
        g[i] = one(T)
    else
        id = registry.multivariate_operator_to_id[op]
        offset = id - registry.multivariate_user_operator_start
        operator = registry.registered_multivariate_operators[offset]
        @assert length(x) == operator.N
        operator.∇f(g, x)
    end
    return
end

_nan_to_zero(x) = isnan(x) ? 0.0 : x

"""
    eval_multivariate_hessian(
        registry::OperatorRegistry,
        op::Symbol,
        H::AbstractMatrix,
        x::AbstractVector{T},
    )::Bool where {T}

Evaluate the Hessian of operator `∇²op(x)`, where `op` is a multivariate
function in `registry`.

The Hessian is stored in the lower-triangular part of the matrix `H`.

Returns a `Bool` indicating whether non-zeros were stored in the matrix.

!!! note
    Implementations of the Hessian operators will not fill structural zeros.
    Therefore, before calling this function you should pre-populate the matrix
    `H` with `0`.
"""
function eval_multivariate_hessian(
    registry::OperatorRegistry,
    op::Symbol,
    H,
    x::AbstractVector{T},
) where {T}
    if op in (:+, :-, :ifelse)
        return false
    end
    if op == :*
        # f(x)    = *(x[i] for i in 1:N)
        #
        # ∇fᵢ(x)  = *(x[j] for j in 1:N if i != j)
        #
        # ∇fᵢⱼ(x) = *(x[k] for k in 1:N if i != k & j != k)
        N = length(x)
        if N == 1
            # Hessian is zero
        elseif N == 2
            H[2, 1] = one(T)
        else
            for i in 1:N, j in (i+1):N
                H[j, i] =
                    prod(x[k] for k in 1:N if k != i && k != j; init = one(T))
            end
        end
    elseif op == :^
        # f(x)   = x[1]^x[2]
        #
        # ∇f(x)  = x[2]*x[1]^(x[2]-1)
        #          x[1]^x[2]*log(x[1])
        #
        # ∇²f(x) = x[2]*(x[2]-1)*x[1]^(x[2]-2)
        #          x[1]^(x[2]-1)*(x[2]*log(x[1])+1) x[1]^x[2]*log(x[1])^2
        ln = x[1] > 0 ? log(x[1]) : NaN
        if x[2] == one(T)
            H[2, 1] = _nan_to_zero(ln + one(T))
            H[2, 2] = _nan_to_zero(x[1] * ln^2)
        elseif x[2] == T(2)
            H[1, 1] = T(2)
            H[2, 1] = _nan_to_zero(x[1] * (T(2) * ln + one(T)))
            H[2, 2] = _nan_to_zero(ln^2 * x[1]^2)
        else
            H[1, 1] = _nan_to_zero(x[2] * (x[2] - 1) * _nan_pow(x[1], x[2] - 2))
            H[2, 1] = _nan_to_zero(_nan_pow(x[1], x[2] - 1) * (x[2] * ln + 1))
            H[2, 2] = _nan_to_zero(ln^2 * _nan_pow(x[1], x[2]))
        end
    elseif op == :/
        # f(x)  = x[1]/x[2]
        #
        # ∇f(x) = 1/x[2]
        #         -x[1]/x[2]^2
        #
        # ∇²(x) = 0.0
        #         -1/x[2]^2 2x[1]/x[2]^3
        d = 1 / x[2]^2
        H[2, 1] = -d
        H[2, 2] = 2 * x[1] * d / x[2]
    elseif op == :atan
        # f(x)  = atan(y, x)
        #
        # ∇f(x) = +x/(x^2+y^2)
        #         -y/(x^2+y^2)
        #
        # ∇²(x) = -(2xy)/(x^2+y^2)^2
        #         (y^2-x^2)/(x^2+y^2)^2 (2xy)/(x^2+y^2)^2
        base = (x[1]^2 + x[2]^2)^2
        H[1, 1] = -2 * x[2] * x[1] / base
        H[2, 1] = (x[1]^2 - x[2]^2) / base
        H[2, 2] = 2 * x[2] * x[1] / base
    elseif op == :min
        _, i = findmin(x)
        H[i, i] = one(T)
    elseif op == :max
        _, i = findmax(x)
        H[i, i] = one(T)
    else
        id = registry.multivariate_operator_to_id[op]
        offset = id - registry.multivariate_user_operator_start
        operator = registry.registered_multivariate_operators[offset]
        if operator.∇²f === nothing
            error("Hessian is not defined for operator $op")
        end
        @assert length(x) == operator.N
        operator.∇²f(H, x)
    end
    return true
end

"""
    eval_logic_function(
        registry::OperatorRegistry,
        op::Symbol,
        lhs::T,
        rhs::T,
    )::Bool where {T}

Evaluate `(lhs op rhs)::Bool`, where `op` is a logic operator in `registry`.
"""
function eval_logic_function(
    ::OperatorRegistry,
    op::Symbol,
    lhs::T,
    rhs::T,
)::Bool where {T}
    if op == :&&
        return lhs && rhs
    else
        @assert op == :||
        return lhs || rhs
    end
end

"""
    eval_comparison_function(
        registry::OperatorRegistry,
        op::Symbol,
        lhs::T,
        rhs::T,
    )::Bool where {T}

Evaluate `(lhs op rhs)::Bool`, where `op` is a comparison operator in
`registry`.
"""
function eval_comparison_function(
    ::OperatorRegistry,
    op::Symbol,
    lhs::T,
    rhs::T,
)::Bool where {T}
    if op == :<=
        return lhs <= rhs
    elseif op == :>=
        return lhs >= rhs
    elseif op == :(==)
        return lhs == rhs
    elseif op == :<
        return lhs < rhs
    else
        @assert op == :>
        return lhs > rhs
    end
end

# This method is implemented here because it needs the OperatorRegistry type from
# the Nonlinear, which doesn't exist when the Utilities submodule is defined.

function MOI.Utilities.eval_variables(
    value_fn::F,
    model::MOI.ModelLike,
    f::MOI.ScalarNonlinearFunction,
) where {F}
    registry = OperatorRegistry()
    return _evaluate_expr(registry, value_fn, model, f)
end

function _evaluate_expr(
    ::OperatorRegistry,
    value_fn::Function,
    model::MOI.ModelLike,
    f::MOI.AbstractFunction,
)
    return MOI.Utilities.eval_variables(value_fn, model, f)
end

function _evaluate_expr(
    ::OperatorRegistry,
    ::Function,
    ::MOI.ModelLike,
    f::Number,
)
    return f
end

function _evaluate_expr(
    registry::OperatorRegistry,
    value_fn::Function,
    model::MOI.ModelLike,
    expr::MOI.ScalarNonlinearFunction,
)
    op = expr.head
    if !_is_registered(registry, op, length(expr.args))
        udf = MOI.get(model, MOI.UserDefinedFunction(op, length(expr.args)))
        if udf === nothing
            throw(MOI.UnsupportedNonlinearOperator(op))
        end
        args = map(expr.args) do arg
            return _evaluate_expr(registry, value_fn, model, arg)
        end
        return first(udf)(args...)
    end
    if length(expr.args) == 1 && haskey(registry.univariate_operator_to_id, op)
        arg = _evaluate_expr(registry, value_fn, model, expr.args[1])
        return eval_univariate_function(registry, op, arg)
    elseif haskey(registry.multivariate_operator_to_id, op)
        args = map(expr.args) do arg
            return _evaluate_expr(registry, value_fn, model, arg)
        end
        return eval_multivariate_function(registry, op, args)
    elseif haskey(registry.logic_operator_to_id, op)
        @assert length(expr.args) == 2
        x = _evaluate_expr(registry, value_fn, model, expr.args[1])
        y = _evaluate_expr(registry, value_fn, model, expr.args[2])
        return eval_logic_function(registry, op, x, y)
    else
        @assert haskey(registry.comparison_operator_to_id, op)
        @assert length(expr.args) == 2
        x = _evaluate_expr(registry, value_fn, model, expr.args[1])
        y = _evaluate_expr(registry, value_fn, model, expr.args[2])
        return eval_comparison_function(registry, op, x, y)
    end
end
