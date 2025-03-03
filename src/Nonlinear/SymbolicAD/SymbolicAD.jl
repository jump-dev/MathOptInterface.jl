# Copyright (c) 2017: Miles Lubin and contributors
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module SymbolicAD

import MathOptInterface as MOI

###
### simplify
###

"""
    simplify(f)

Return a simplified copy of the function `f`.

!!! warning
    This function is not type stable by design.

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(:^, Any[x, 1])
^(MOI.VariableIndex(1), (1))

julia> MOI.Nonlinear.SymbolicAD.simplify(f)
MOI.VariableIndex(1)
```
"""
simplify(f) = simplify!(copy(f))

###
### `simplify!`
###

"""
    simplify!(f)

Simplify the function `f` in-place and return either the function `f` or a
new object if `f` can be represented in a simpler type.

!!! warning
    This function is not type stable by design.

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(
           :+,
           Any[MOI.ScalarNonlinearFunction(:+, Any[1.0, x]), 2.0 * x + 3.0],
       )
+(+(1.0, MOI.VariableIndex(1)), 3.0 + 2.0 MOI.VariableIndex(1))

julia> MOI.Nonlinear.SymbolicAD.simplify!(f)
4.0 + 3.0 MOI.VariableIndex(1)

julia> f
+(1.0, MOI.VariableIndex(1), 3.0 + 2.0 MOI.VariableIndex(1))
```
"""
simplify!(f) = f

function simplify!(f::MOI.ScalarAffineFunction{T}) where {T}
    f = MOI.Utilities.canonicalize!(f)
    if isempty(f.terms)
        return f.constant
    end
    return f
end

function simplify!(f::MOI.ScalarQuadraticFunction{T}) where {T}
    f = MOI.Utilities.canonicalize!(f)
    if isempty(f.quadratic_terms)
        if isempty(f.affine_terms)
            return f.constant
        end
        return MOI.ScalarAffineFunction(f.affine_terms, f.constant)
    end
    return f
end

function simplify!(f::MOI.ScalarNonlinearFunction)
    stack, result_stack = Any[f], Any[]
    while !isempty(stack)
        arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            # We need some sort of hint so that the next time we see this on the
            # stack we evaluate it using the args in `result_stack`. One option
            # would be a custom type. Or we can just wrap in (,) and then check
            # for a Tuple, which isn't (curretly) a valid argument.
            push!(stack, (arg,))
            for child in arg.args
                push!(stack, child)
            end
        elseif arg isa Tuple{<:MOI.ScalarNonlinearFunction}
            result = only(arg)
            for i in eachindex(result.args)
                result.args[i] = pop!(result_stack)
            end
            # simplify!(::Val, ::Any) does not use recursion so this is safe.
            result = simplify!(Val(result.head), result)
            result = _eval_if_constant(result)
            push!(result_stack, result)
        else
            push!(result_stack, arg)
        end
    end
    return _simplify_if_affine!(only(result_stack))
end

function simplify!(f::MOI.VectorAffineFunction{T}) where {T}
    f = MOI.Utilities.canonicalize!(f)
    if isempty(f.terms)
        return f.constants
    end
    return f
end

function simplify!(f::MOI.VectorQuadraticFunction{T}) where {T}
    f = MOI.Utilities.canonicalize!(f)
    if isempty(f.quadratic_terms)
        if isempty(f.affine_terms)
            return f.constants
        end
        return MOI.VectorAffineFunction(f.affine_terms, f.constants)
    end
    return f
end

function simplify!(f::MOI.VectorNonlinearFunction)
    for (i, row) in enumerate(f.rows)
        f.rows[i] = simplify!(row)
    end
    return f
end

# If a ScalarNonlinearFunction has only constant arguments, we should return
# the value.

_isnum(::Any) = false

_isnum(::Union{Bool,Integer,Float64}) = true

function _eval_if_constant(f::MOI.ScalarNonlinearFunction)
    if all(_isnum, f.args) && hasproperty(Base, f.head)
        return getproperty(Base, f.head)(f.args...)
    end
    return f
end

_eval_if_constant(f) = f

_iszero(x::Any)::Bool = _isnum(x) && iszero(x)

_isone(x::Any)::Bool = _isnum(x) && isone(x)

"""
    _isexpr(f::Any, head::Symbol[, n::Int])

Return `true` if `f` is a `ScalarNonlinearFunction` with head `head` and, if
specified, `n` arguments.
"""
_isexpr(::Any, ::Symbol, n::Int = 0) = false

_isexpr(f::MOI.ScalarNonlinearFunction, head::Symbol) = f.head == head

function _isexpr(f::MOI.ScalarNonlinearFunction, head::Symbol, n::Int)
    return _isexpr(f, head) && length(f.args) == n
end

"""
    simplify!(::Val{head}, f::MOI.ScalarNonlinearFunction)

Simplify the function `f` in-place and return either the function `f` or a
new object if `f` can be represented in a simpler type.

## Val

The `head` in `Val{head}` is taken from `f.head`. This function should be called
as:
```julia
f = simplify!(Val(f.head), f)
```

Implementing a method that dispatches on `head` enables custom simplification
rules for different operators without needing a giant switch statement.

## Note

It is important that this function does not recursively call `simplify!`. Deal
only with the immediate operator. The children arguments will already be
simplified.
"""
simplify!(::Val, f::MOI.ScalarNonlinearFunction) = f

function simplify!(::Val{:*}, f::MOI.ScalarNonlinearFunction)
    new_args = Any[]
    first_constant = 0
    for arg in f.args
        if _isexpr(arg, :*)
            # If the child is a :*, lift its arguments to the parent
            append!(new_args, arg.args)
        elseif _iszero(arg)
            # If any argument is zero, the entire expression must be false
            return false
        elseif _isone(arg)
            # Skip any arguments that are one
        elseif arg isa Real
            # Collect all constant arguments into a single value
            if first_constant == 0
                push!(new_args, arg)
                first_constant = length(new_args)
            else
                new_args[first_constant] *= arg
            end
        else
            push!(new_args, arg)
        end
    end
    if length(new_args) == 0
        # *() -> true
        return true
    elseif length(new_args) == 1
        # *(x) -> x
        return only(new_args)
    end
    resize!(f.args, length(new_args))
    copyto!(f.args, new_args)
    return f
end

function simplify!(::Val{:+}, f::MOI.ScalarNonlinearFunction)
    new_args = Any[]
    first_affine_term = 0
    for arg in f.args
        if _isexpr(arg, :+)
            # If a child is a :+, lift its arguments to the parent
            append!(new_args, arg.args)
        elseif _iszero(arg)
            # Skip any zero arguments
        elseif arg isa Real
            # Collect all affine arguments into a single value
            if first_affine_term == 0
                push!(new_args, arg)
                first_affine_term = length(new_args)
            else
                new_args[first_affine_term] += arg
            end
        else
            push!(new_args, arg)
        end
    end
    if length(new_args) == 0
        # +() -> false
        return false
    elseif length(new_args) == 1
        # +(x) -> x
        return only(new_args)
    elseif length(f.args) == 2 && _isexpr(f.args[2], :-, 1)
        # +(x, -y) -> -(x, y)
        return MOI.ScalarNonlinearFunction(
            :-,
            Any[f.args[1], f.args[2].args[1]],
        )
    end
    resize!(f.args, length(new_args))
    copyto!(f.args, new_args)
    return f
end

function simplify!(::Val{:-}, f::MOI.ScalarNonlinearFunction)
    if length(f.args) == 1
        if _isexpr(f.args[1], :-, 1)
            # -(-(x)) => x
            return f.args[1].args[1]
        end
    elseif length(f.args) == 2
        if f.args[1] == f.args[2]
            # x - x => 0
            return false
        elseif _iszero(f.args[1])
            # 0 - x => -x
            popfirst!(f.args)
            return f
        elseif _iszero(f.args[2])
            # x - 0 => x
            return f.args[1]
        elseif _isexpr(f.args[2], :-, 1)
            # x - -(y) => x + y
            return MOI.ScalarNonlinearFunction(
                :+,
                Any[f.args[1], f.args[2].args[1]],
            )
        end
    end
    return f
end

function simplify!(::Val{:^}, f::MOI.ScalarNonlinearFunction)
    if _iszero(f.args[2])
        # x^0 => 1
        return true
    elseif _isone(f.args[2])
        # x^1 => x
        return f.args[1]
    elseif _iszero(f.args[1])
        # 0^x => 0
        return false
    elseif _isone(f.args[1])
        # 1^x => 1
        return true
    end
    return f
end

function simplify!(::Val{:ifelse}, f::MOI.ScalarNonlinearFunction)
    if f.args[1] == true
        # ifelse(true, x, y) => x
        return f.args[2]
    elseif f.args[1] == false
        # ifelse(false, x, y) => y
        return f.args[3]
    elseif f.args[2] == f.args[3]
        # ifelse(y, x, x) => x
        return f.args[2]
    end
    return f
end

###
### variables
###

"""
    variables(f::Union{Real,MOI.AbstractScalarFunction})

Return a sorted list of the `MOI.VariableIndex` present in the function `f`.

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> x = MOI.VariableIndex.(1:3)
3-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(2)
 MOI.VariableIndex(3)

julia> f = MOI.ScalarNonlinearFunction(:atan, Any[x[3], 2.0 * x[1]])
atan(MOI.VariableIndex(3), 0.0 + 2.0 MOI.VariableIndex(1))

julia> MOI.Nonlinear.SymbolicAD.variables(f)
2-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)
 MOI.VariableIndex(3)
```
"""
function variables(f::MOI.AbstractScalarFunction)
    ret = MOI.VariableIndex[]
    _variables(ret, f)
    return sort!(ret; by = x -> x.value)
end

variables(::Real) = MOI.VariableIndex[]

_variables(::Vector{MOI.VariableIndex}, ::Real) = nothing

function _variables(ret::Vector{MOI.VariableIndex}, f::MOI.VariableIndex)
    if !(f in ret)
        push!(ret, f)
    end
    return
end

function _variables(ret::Vector{MOI.VariableIndex}, f::MOI.ScalarAffineTerm)
    _variables(ret, f.variable)
    return
end

function _variables(ret::Vector{MOI.VariableIndex}, f::MOI.ScalarAffineFunction)
    for term in f.terms
        _variables(ret, term)
    end
    return
end

function _variables(ret::Vector{MOI.VariableIndex}, f::MOI.ScalarQuadraticTerm)
    _variables(ret, f.variable_1)
    _variables(ret, f.variable_2)
    return
end

function _variables(
    ret::Vector{MOI.VariableIndex},
    f::MOI.ScalarQuadraticFunction,
)
    for term in f.affine_terms
        _variables(ret, term)
    end
    for q_term in f.quadratic_terms
        _variables(ret, q_term)
    end
    return
end

function _variables(
    ret::Vector{MOI.VariableIndex},
    f::MOI.ScalarNonlinearFunction,
)
    stack = Any[f]
    while !isempty(stack)
        arg = pop!(stack)
        if arg isa MOI.ScalarNonlinearFunction
            # We need to push the args on in reverse order so that we iterate
            # across the tree from left to right.
            for i in reverse(1:length(arg.args))
                push!(stack, arg.args[i])
            end
        else
            _variables(ret, arg)
        end
    end
    return
end

###
### derivative
###

"""
    derivative(f::Union{Real,MOI.AbstractScalarFunction}, x::MOI.VariableIndex)

Return an expression representing the partial derivative of `f` with respect to
`x`.

## Expression swelling

With few exceptions, the algorithm used to compute the derivative does not
perform simplications. As a result, the returned expression may contain terms
like `*(false, g)` that can be trivially simplified to `false`.

In most cases, you should call `simplify!(derivative(f, x))` to return a
simplified expression of the derivative.

## Example

```jldoctest
julia> import MathOptInterface as MOI

julia> x = MOI.VariableIndex(1)
MOI.VariableIndex(1)

julia> f = MOI.ScalarNonlinearFunction(:sin, Any[x])
sin(MOI.VariableIndex(1))

julia> df_dx = MOI.Nonlinear.SymbolicAD.derivative(f, x)
cos(MOI.VariableIndex(1))
```
"""
derivative(::Real, ::MOI.VariableIndex) = false

function derivative(f::MOI.VariableIndex, x::MOI.VariableIndex)
    return ifelse(f == x, true, false)
end

function derivative(
    f::MOI.ScalarAffineFunction{T},
    x::MOI.VariableIndex,
) where {T}
    ret = zero(T)
    for term in f.terms
        if term.variable == x
            ret += term.coefficient
        end
    end
    return ret
end

function derivative(
    f::MOI.ScalarQuadraticFunction{T},
    x::MOI.VariableIndex,
) where {T}
    constant = zero(T)
    for term in f.affine_terms
        if term.variable == x
            constant += term.coefficient
        end
    end
    aff_terms = MOI.ScalarAffineTerm{T}[]
    for q_term in f.quadratic_terms
        if q_term.variable_1 == q_term.variable_2 == x
            push!(aff_terms, MOI.ScalarAffineTerm(q_term.coefficient, x))
        elseif q_term.variable_1 == x
            push!(
                aff_terms,
                MOI.ScalarAffineTerm(q_term.coefficient, q_term.variable_2),
            )
        elseif q_term.variable_2 == x
            push!(
                aff_terms,
                MOI.ScalarAffineTerm(q_term.coefficient, q_term.variable_1),
            )
        end
    end
    return MOI.ScalarAffineFunction(aff_terms, constant)
end

function _replace_expression(node::Expr, u)
    for (i, arg) in enumerate(node.args)
        node.args[i] = _replace_expression(arg, u)
    end
    @assert Meta.isexpr(node, :call)
    op, args = node.args[1], node.args[2:end]
    return MOI.ScalarNonlinearFunction(op, args)
end

_replace_expression(node::Any, u) = node

function _replace_expression(node::Symbol, u)
    if node == :x
        return u
    end
    return node
end

"""
    __DERIVATIVE__ = "__DERIVATIVE__"

This constant is prefixed to the name of univariate operators to indicate
that we should compute their derivative.

If `f.head` is itself a __DERIVATIVE__, then this will create a second
derivative like `__DERIVATIVE____DERIVATIVE__\$(f.head)`.

Munging the `.head` field like this seems a bit hacky, but it is simpler than
the alternatives like `SNF(:derivative, Any[SNF(:f, Any[x]])])`, because this
would require changes to the already complicated expression walker for
evaluating the expression.

The String value of this constant is arbitrary. It just needs to be something
that the user would never write themselves.
"""
const __DERIVATIVE__ = "__DERIVATIVE__"

# This function helps simplify df_du * du_dx in the commonn case that `du_dx`
# is `true` (when u = x), or `false` (when x ∉ u).
function _univariate_chain_rule(df_du, du_dx)
    return MOI.ScalarNonlinearFunction(:*, Any[df_du, du_dx])
end

_univariate_chain_rule(df_du, du_dx::Bool) = ifelse(du_dx, df_du, du_dx)

function derivative(f::MOI.ScalarNonlinearFunction, x::MOI.VariableIndex)
    if length(f.args) == 1
        u = only(f.args)
        du_dx = derivative(u, x)
        if f.head == :+
            return du_dx
        elseif f.head == :-
            return MOI.ScalarNonlinearFunction(:-, Any[du_dx])
        elseif f.head == :abs
            df_du = MOI.ScalarNonlinearFunction(
                :ifelse,
                Any[MOI.ScalarNonlinearFunction(:>=, Any[u, 0]), 1, -1],
            )
            return _univariate_chain_rule(df_du, du_dx)
        elseif f.head == :sign
            return false
        elseif f.head == :deg2rad
            df_du = deg2rad(1)
            return _univariate_chain_rule(df_du, du_dx)
        elseif f.head == :rad2deg
            df_du = rad2deg(1)
            return _univariate_chain_rule(df_du, du_dx)
        end
        for (key, df, _) in MOI.Nonlinear.SYMBOLIC_UNIVARIATE_EXPRESSIONS
            if key == f.head
                # The chain rule: d(f(g(x))) / dx = f'(g(x)) * g'(x)
                df_du = _replace_expression(copy(df), u)
                return _univariate_chain_rule(df_du, du_dx)
            end
        end
        # Delay derivative until evaluation. This may result in a later
        # UnsupportedNonlinearOperator error, but we can't tell just yet.
        d_op = Symbol(__DERIVATIVE__ * "$(f.head)")
        df_du = MOI.ScalarNonlinearFunction(d_op, Any[u])
        return _univariate_chain_rule(df_du, du_dx)
    end
    if f.head == :+
        # d/dx(+(args...)) = +(d/dx args)
        args = Any[]
        for arg in f.args
            d_arg_dx = derivative(arg, x)
            if _iszero(d_arg_dx)
                # Special case: common situation where the arugment doesn't
                # depend on x
                continue
            end
            push!(args, d_arg_dx)
        end
        # A few special case handlers. We don't call the full `simplify!`
        # because that will allocate more, and we don't really care about
        # disaggregated constants.
        if isempty(args)
            return false
        elseif length(args) == 1
            return only(args)
        end
        return MOI.ScalarNonlinearFunction(:+, args)
    elseif f.head == :-
        # d/dx(-(args...)) = -(d/dx args)
        # Note that - is not unary here because that wouuld be caught above.
        args = Any[derivative(arg, x) for arg in f.args]
        return MOI.ScalarNonlinearFunction(:-, args)
    elseif f.head == :*
        # Product rule: d/dx(*(args...)) = sum(d{i}/dx * args\{i})
        sum_terms = Any[]
        for i in eachindex(f.args)
            g = MOI.ScalarNonlinearFunction(:*, copy(f.args))
            g.args[i] = derivative(f.args[i], x)
            if any(_iszero, g.args)
                # Special case: common situation where an argument doesn't
                # depend on x
                continue
            end
            # We call `simplify!` here to clean up any issues like *(y, true)
            # or *(y).
            push!(sum_terms, simplify!(Val(:*), g))
        end
        return simplify!(Val(:+), MOI.ScalarNonlinearFunction(:+, sum_terms))
    elseif f.head == :^
        # d/dx(u^p) = p*u^(p-1)*(du/dx) + u^p*log(u)*(dp/dx))
        @assert length(f.args) == 2
        u, p = f.args
        du_dx = derivative(u, x)
        dp_dx = derivative(p, x)
        term_1 = MOI.ScalarNonlinearFunction(
            :*,
            Any[p, MOI.ScalarNonlinearFunction(:^, Any[u, p-1]), du_dx],
        )
        if _iszero(dp_dx)  # p is constant and does not depend on x
            return term_1
        end
        term_2 = MOI.ScalarNonlinearFunction(
            :*,
            Any[
                MOI.ScalarNonlinearFunction(:^, Any[u, p]),
                MOI.ScalarNonlinearFunction(:log, Any[u]),
                dp_dx,
            ],
        )
        return MOI.ScalarNonlinearFunction(:+, Any[term_1, term_2])
    elseif f.head == :/
        # Quotient rule: d/dx(u / v) = (du/dx)*v - u*(dv/dx)) / v^2
        @assert length(f.args) == 2
        u, v = f.args
        du_dx, dv_dx = derivative(u, x), derivative(v, x)
        return MOI.ScalarNonlinearFunction(
            :/,
            Any[
                MOI.ScalarNonlinearFunction(
                    :-,
                    Any[
                        MOI.ScalarNonlinearFunction(:*, Any[du_dx, v]),
                        MOI.ScalarNonlinearFunction(:*, Any[u, dv_dx]),
                    ],
                ),
                MOI.ScalarNonlinearFunction(:^, Any[v, 2]),
            ],
        )
    elseif f.head == :ifelse
        @assert length(f.args) == 3
        # Pick the derivative of the active branch
        return MOI.ScalarNonlinearFunction(
            :ifelse,
            Any[f.args[1], derivative(f.args[2], x), derivative(f.args[3], x)],
        )
    elseif f.head == :atan
        @assert length(f.args) == 2
        u, v = f.args
        du_dx, dv_dx = derivative(u, x), derivative(v, x)
        u_2 = MOI.ScalarNonlinearFunction(:^, Any[u, 2])
        v_2 = MOI.ScalarNonlinearFunction(:^, Any[v, 2])
        u_dv_dx = MOI.ScalarNonlinearFunction(:*, Any[u, dv_dx])
        v_du_dx = MOI.ScalarNonlinearFunction(:*, Any[v, du_dx])
        return MOI.ScalarNonlinearFunction(
            :/,
            Any[
                MOI.ScalarNonlinearFunction(:+, Any[u_dv_dx, v_du_dx]),
                MOI.ScalarNonlinearFunction(:+, Any[u_2, v_2]),
            ],
        )
    elseif f.head == :min
        g = derivative(f.args[end], x)
        for i in length(f.args)-1:-1:1
            g = MOI.ScalarNonlinearFunction(
                :ifelse,
                Any[
                    MOI.ScalarNonlinearFunction(:(<=), Any[f.args[i], f]),
                    derivative(f.args[i], x),
                    g,
                ],
            )
        end
        return g
    elseif f.head == :max
        g = derivative(f.args[end], x)
        for i in length(f.args)-1:-1:1
            g = MOI.ScalarNonlinearFunction(
                :ifelse,
                Any[
                    MOI.ScalarNonlinearFunction(:(>=), Any[f.args[i], f]),
                    derivative(f.args[i], x),
                    g,
                ],
            )
        end
        return g
    elseif f.head in (:(>=), :(<=), :(<), :(>), :(==))
        return false
    end
    err = MOI.UnsupportedNonlinearOperator(
        f.head,
        "the operator does not support symbolic differentiation",
    )
    return throw(err)
end

###
### gradient_and_hessian
###

"""
    gradient_and_hessian(
        [filter_fn::Function = x -> true,]
        f::MOI.AbstractScalarFunction,
    )

Compute the symbolic gradient and Hessian of `f`, and return the result as a
tuple of four elements:

 1. `x::Vector{MOI.VariableIndex}`: the list of variables that appear in `f`
 2. `∇f::Vector{Any}`: a vector for the first partial derivative of `f` with
    respect to each element in `x`
 3. `H::Vector{Tuple{Int,Int}}`: a vector of `(row, column)` tuples that list
    the non-zero entries in the Hessian of `f`
 4. `∇²f::Vector{Any}`: a vector of expressions, in the same order as `H`, for
    the non-zero entries in the Hessian of `f`

## `filter_fn`

This argument is a function, `filter_fn(::MOI.VariableIndex)::Bool` that returns
`true` if the gradient and Hessian of `f` should be computed with respect to it.

Use this argument to filter out constant parameters from decision variables.
"""
function gradient_and_hessian(
    filter_fn::F,
    f::MOI.AbstractScalarFunction,
) where {F<:Function}
    x = filter!(filter_fn, variables(f))
    n = length(x)
    ∇f = Vector{Any}(undef, n)
    H_by_x, ∇²f_by_x = [Tuple{Int,Int}[] for _ in 1:n], [Any[] for _ in 1:n]
    for i in eachindex(x)
        xi = x[i]
        ∇fi = simplify!(derivative(f, xi))
        ∇f[i] = ∇fi
        for xj in filter!(filter_fn, variables(∇fi))
            j = findfirst(==(xj), x)
            if i > j
                continue  # Don't need lower triangle
            end
            dfij = simplify!(derivative(∇fi, xj))
            if dfij != false
                push!(∇²f_by_x[i], dfij)
                push!(H_by_x[i], (i, j))
            end
        end
    end
    return x, ∇f, reduce(vcat, H_by_x), reduce(vcat, ∇²f_by_x)
end

function gradient_and_hessian(f::MOI.AbstractScalarFunction)
    return gradient_and_hessian(x -> true, f)
end

###
### DAG
###

# operator is a (mask ⊻ id, nargs)::Tuple{Int32,Int32}, packed into an Int64
# This is based on the assumptions that:
#  1. we don't have more than typemax(Int32) >> 4 operators
#  2. we don't have more than typemax(Int32) input arguments
# both of these seem quite sensible.
function _mask_id_nargs_to_operator(mask::UInt32, id::Int, nargs::Int)
    @assert nargs <= typemax(Int32)
    @assert id <= (typemax(Int32) >> 4)
    return xor(Int64(mask ⊻ Int32(id)) << 32, Int64(nargs))
end

function _op_nargs_to_operator(
    reg::MOI.Nonlinear.OperatorRegistry,
    op::Symbol,
    nargs::Int,
)
    if nargs == 1
        op_to_id = reg.univariate_operator_to_id
        if (ret = get(op_to_id, op, nothing)) !== nothing
            return _mask_id_nargs_to_operator(:0x00000000, ret, nargs)
        end
        str_op = string(op)
        prefix_hessian = __DERIVATIVE__ * __DERIVATIVE__
        if startswith(str_op, prefix_hessian)
            f_op = Symbol(replace(str_op, prefix_hessian => ""))
            if (ret = get(op_to_id, f_op, nothing)) !== nothing
                return _mask_id_nargs_to_operator(:0x20000000, ret, nargs)
            end
        elseif startswith(str_op, __DERIVATIVE__)
            f_op = Symbol(replace(str_op, __DERIVATIVE__ => ""))
            if (ret = get(op_to_id, f_op, nothing)) !== nothing
                return _mask_id_nargs_to_operator(:0x10000000, ret, nargs)
            end
        end
    end
    if (ret = get(reg.multivariate_operator_to_id, op, nothing)) !== nothing
        return _mask_id_nargs_to_operator(:0x30000000, ret, nargs)
    end
    if (ret = get(reg.logic_operator_to_id, op, nothing)) !== nothing
        return _mask_id_nargs_to_operator(0x40000000, ret, nargs)
    end
    ret = reg.comparison_operator_to_id[op]
    return _mask_id_nargs_to_operator(0x50000000, ret, nargs)
end

function _operator_to_type_id_nargs(operator::Int64)::Tuple{Symbol,UInt32,Int64}
    @assert operator > 0
    type_id = Int32(operator >> 32)
    type = type_id & 0x70000000
    id = type_id - type
    nargs = operator - (Int64(type_id) << 32)
    if type == 0x00000000
        return :univariate, id, nargs
    elseif type == 0x10000000
        return :univariate_derivative, id, nargs
    elseif type == 0x20000000
        return :univariate_second_derivative, id, nargs
    elseif type == 0x30000000
        return :multivariate, id, nargs
    elseif type == 0x40000000
        return :logic, id, nargs
    else
        @assert type == 0x50000000
        return :comparison, id, nargs
    end
end

const _kNODE_PARAMETER = -1
const _kNODE_VARIABLE = -2
const _kNODE_VALUE = -3

"""
See the docstring of [`_DAG`](@ref).
"""
struct _Node
    operator::Int64
    data::Int64
end

function Base.show(io::IO, n::_Node)
    print(io, "_Node(", n.operator, ", ", n.data, ")\t")
    if n.operator == _kNODE_PARAMETER
        print(io, "\t\t# p[", n.data, "]")
    elseif n.operator == _kNODE_VARIABLE
        print(io, "\t\t# x[", n.data, "]")
    elseif n.operator == _kNODE_VALUE
        print(io, "# ", reinterpret(Float64, n.data))
    else
        type, op, nargs = _operator_to_type_id_nargs(n.operator)
        children = n.data .+ (0:nargs-1)
        print(io, "# $type(op = $op, children = $children)")
    end
end

"""
    _DAG(registry::MOI.Nonlinear.OperatorRegistry)

The [`_DAG`](@ref) represents a vector-valued function `f(x, p)`.

## Fields

### `tape::Vector{_Node}`

The list of nodes in the DAG. A `_Node` is the struct:
```julia
struct _Node
    operator::Int64
    data::Int64
end
```
with the following interpretation:

 * If `operator == _kNODE_PARAMETER == -1`, then the `data` is the index in the
   input `p` vector
 * If `operator == _kNODE_VARIABLE == -2`, then the `data` is the index in the
   input `x` vector`
 * If `operator == _kNODE_VALUE == -3`, then the `data` is a `Float64`, encoded
   as an `Int64`. Access it with `reinterpret(Float64, node.data)`.
 * If `operator > 0`, then the operator is an operator from the
   `MOI.Nonlinear.OperatorRegistry` in the DAG. Use
   ```julia
   type, op, nargs = _operator_to_type_id_nargs(node.operator)
   ```
   to retrieve the type, op-code, and the number of arguments. The `data`
   field is the index of the first child in the `.children` array of the DAG.

### `nodes::Dict{UInt64,Int}`

A mapping of hash values to the index in `.tape`. This data structure is used to
de-duplicate nodes as the DAG is being built.

### `children::Vector{Int}`

An ordered list of the children for operator nodes in `tape`.

We could have stored these as a separate vector of children for each node in the
tape, where each element in the children vector is the index of the child in
`tape`. To minimize the number of unique vectors, we instead store here the
concatenation of all children vectors. The `node.data` value of an operator node
is the index in `.children` of the first child. More concretely, given an
operator node, the child nodes can be computed as:
```julia
type, op, nargs = _operator_to_type_id_nargs(node.operator)
children_indices = dag.children[node.data:node.data+nargs-1]
children_nodes = dag.tape[children_indices]
```

### `registry::MOI.Nonlinear.OperatorRegistry`

The registry containing operator information. The registry is used to evaluate
each operator via the corresponding methods in `MOI.Nonlinear`.

### `indices::Vector{Int}`

The indices of `.tape` that represent the output of the function `f(x, p)`.
These indices are mostly used when querying the result of an evaluated DAG via
`dag.result[dag.indices]`.

### `input::Vector{Float64}`

A cache for storing the input x when evaluating the DAG. This cache is useful
because the DAG's input variables are typically a subset of the full decision
vector. We could use a `view`, but instead we use this cache. The main reason is
to keep the input types as simple as possible (`Vector{Float64}` where
possible). At the cost of copying the values, we also avoid a set of
double-lookups when indexing into the full `x` via a `view`.

### `result::Vector{Float64}`

A cache for storing the result of evaluating the DAG. There is one element for
each element in `.tape`.

See also the `indices` field.

### `cache::Vector{Float64}`

A cache for evaluating multivariate operators. This will be sized such that its
length is the largest multivariate operator that may be called. Other operators
take views of the first nargs elements.

The main reason for this vector is to avoid a complicated `view` into the
`result` vector when evaluating multivariate operators.
"""
struct _DAG
    tape::Vector{_Node}
    nodes::Dict{UInt64,Int}
    children::Vector{Int}
    registry::MOI.Nonlinear.OperatorRegistry
    indices::Vector{Int}
    input::Vector{Float64}
    result::Vector{Float64}
    cache::Vector{Float64}

    function _DAG(registry)
        return new(
            _Node[],
            Dict{UInt64,Int}(),
            Int[],
            registry,
            Int[],
            Float64[],
            Float64[],
            Float64[],
        )
    end
end

function Base.show(io::IO, dag::_DAG)
    println(io, "DAG")
    println(io, "  tape")
    for (i, node) in enumerate(dag.tape)
        println(io, "    [$i]: ", node)
    end
    println(io, "  children")
    for (i, child) in enumerate(dag.children)
        println(io, "    [$i]: ", child)
    end
    return
end

function _DAG(registry::MOI.Nonlinear.OperatorRegistry, f::Vector)
    dag = _DAG(registry)
    stack, result_stack = Any[], Int[]
    for fi in f
        push!(stack, fi)
        while !isempty(stack)
            arg = pop!(stack)
            if arg isa MOI.ScalarAffineFunction
                push!(stack, convert(MOI.ScalarNonlinearFunction, arg))
            elseif arg isa MOI.ScalarQuadraticFunction
                push!(stack, convert(MOI.ScalarNonlinearFunction, arg))
            elseif arg isa MOI.ScalarNonlinearFunction
                push!(stack, (arg,))
                for child in arg.args
                    push!(stack, child)
                end
            else
                _add_node_to_dag(result_stack, dag, arg)
            end
        end
        @assert length(result_stack) == 1
        push!(dag.indices, pop!(result_stack))
    end
    resize!(dag.result, length(dag.tape))
    return dag
end

function _add_node_to_dag(result_stack, dag::_DAG, x::Real)
    _add_node_to_dag(result_stack, dag, convert(Float64, x))
    return
end

function _add_node_to_dag(result_stack, dag::_DAG, x::Float64)
    h = hash(x)
    result = get!(dag.nodes, h) do
        data = reinterpret(Int64, convert(Float64, x))
        node = _Node(_kNODE_VALUE, data)
        push!(dag.tape, node)
        dag.nodes[h] = length(dag.tape)
        return length(dag.tape)
    end
    push!(result_stack, result)
    return
end

function _add_node_to_dag(result_stack, dag::_DAG, x::MOI.VariableIndex)
    h = hash(x, hash(MOI.VariableIndex))
    result = get!(dag.nodes, h) do
        node = if x.value > 0
            _Node(_kNODE_VARIABLE, x.value)
        else
            @assert x.value < 0
            _Node(_kNODE_PARAMETER, -x.value)
        end
        push!(dag.tape, node)
        dag.nodes[h] = length(dag.tape)
        return length(dag.tape)
    end
    push!(result_stack, result)
    return
end

function _add_node_to_dag(
    result_stack,
    dag::_DAG,
    f::MOI.ScalarNonlinearFunction,
)
    op, nargs = f.head, length(f.args)
    children = Int[pop!(result_stack) for _ in 1:nargs]
    h = hash(children, hash(op))
    result = get!(dag.nodes, h) do
        data = length(dag.children) + 1
        append!(dag.children, children)
        operator = _op_nargs_to_operator(dag.registry, op, nargs)
        node = _Node(operator, data)
        if length(dag.cache) < nargs
            resize!(dag.cache, nargs)
        end
        push!(dag.tape, node)
        dag.nodes[h] = length(dag.tape)
        return length(dag.tape)
    end
    push!(result_stack, result)
    return
end

function _add_node_to_dag(
    result_stack,
    dag::_DAG,
    f::Tuple{MOI.ScalarNonlinearFunction},
)
    return _add_node_to_dag(result_stack, dag, only(f))
end

function _evaluate!(dag::_DAG, x::AbstractVector{Float64}, p::Vector{Float64})
    reg = dag.registry
    for (i, node) in enumerate(dag.tape)
        @inbounds dag.result[i] = if node.operator == _kNODE_PARAMETER
            p[node.data]
        elseif node.operator == _kNODE_VARIABLE
            x[node.data]
        elseif node.operator == _kNODE_VALUE
            reinterpret(Float64, node.data)
        else
            @assert node.operator > 0
            type, op, nargs = _operator_to_type_id_nargs(node.operator)
            if type == :univariate
                @assert nargs == 1
                MOI.Nonlinear.eval_univariate_function(
                    reg,
                    op,
                    dag.result[dag.children[node.data]],
                )
            elseif type == :univariate_derivative
                @assert nargs == 1
                MOI.Nonlinear.eval_univariate_gradient(
                    reg,
                    op,
                    dag.result[dag.children[node.data]],
                )
            elseif type == :univariate_second_derivative
                @assert nargs == 1
                MOI.Nonlinear.eval_univariate_hessian(
                    reg,
                    op,
                    dag.result[dag.children[node.data]],
                )
            elseif type == :multivariate
                for j in 1:nargs
                    dag.cache[j] = dag.result[dag.children[node.data+j-1]]
                end
                MOI.Nonlinear.eval_multivariate_function(
                    reg,
                    reg.multivariate_operators[op],
                    view(dag.cache, 1:nargs),
                )
            elseif type == :logic
                @assert nargs == 2
                MOI.Nonlinear.eval_logic_function(
                    reg,
                    reg.logic_operators[op],
                    dag.result[dag.children[node.data]] ≈ 1.0,
                    dag.result[dag.children[node.data+1]] ≈ 1.0,
                )
            else
                @assert type == :comparison
                @assert nargs == 2
                MOI.Nonlinear.eval_comparison_function(
                    reg,
                    reg.comparison_operators[op],
                    dag.result[dag.children[node.data]],
                    dag.result[dag.children[node.data+1]],
                )
            end
        end
    end
    return
end

struct _SymbolicInstance
    hash::UInt64
    x::Vector{Int}
    p::Vector{Float64}
    result::Vector{Float64}
    jac_offset::Int
    hess_offset::Int
end

function _evaluate!(dag::_DAG, g::_SymbolicInstance, x)
    if length(dag.input) != length(g.x)
        resize!(dag.input, length(g.x))
    end
    for (i, j) in enumerate(g.x)
        dag.input[i] = x[j]
    end
    _evaluate!(dag, dag.input, g.p)
    for (i, j) in enumerate(dag.indices)
        g.result[i] = dag.result[j]
    end
    return
end

###
### MOI.AbstractNLPEvaluator
###

struct Evaluator <: MOI.AbstractNLPEvaluator
    dag::Dict{UInt64,_DAG}
    H::Dict{UInt64,Vector{Tuple{Int64,Int64}}}
    objective::Union{Nothing,_SymbolicInstance}
    constraints::Vector{_SymbolicInstance}
    constraint_index_by_hash::Dict{UInt64,Vector{Int}}
    x::Vector{Float64}
end

"""
    _to_symbolic_form(
        nlp::MOI.Nonlinear.Model,
        expr::MOI.Nonlinear.Expression,
        variable_to_column::Dict{Int64,Int},
    )

Take an `expr` from `nlp` and convert it into a symbolic template.

`variable_to_column` is a dictionary that maps the `value` of
`MOI.VariableIndex(value)` to the 1-based index of the variable as it appears in
the MOI evaluators.

## Returns

Returns a `NamedTuple` with the following fields:

### `f::MOI.ScalarNonlinearFunction`

A `MOI.ScalarNonlinearFunction` that represents `expr`, with two subtleties:

 1. Each decision variable `MOI.VariableIndex(value)` in `expr` is translated to
    the column index via `variable_to_column[value]` and the column index is
    appended to `ordered_variables` (if it isn't already). Then, when
    `MOI.VariableIndex(i)` (where `i > 0`) appears in `f`, it means that the
    variable in `expr` corresponded to the column `ordered_variables[i]`.

 2. When `MOI.VariableIndex(-i)` (where `i < 0`) appears in `f`, it means that
    the value in `expr` was `data[i]`.

### `hash::UInt64`

A unique hash of the function `f`, which ignores the particular values of
`ordered_variables` and `data`.

### `ordered_variables::Vector{MOI.VariableIndex}`

A list of the decision variables that appear in `expr`, in the order that they
were traversed.

See `f` for details.

### `data::Vector{Float64}`

A list of the constants that appear in `expr`, in the order that they were
traversed.

See `f` for details.
"""
function _to_symbolic_form(
    nlp::MOI.Nonlinear.Model,
    expr::MOI.Nonlinear.Expression,
    variable_to_column::Dict{Int64,Int},
)
    list_of_variables = Int[]
    tree = Any[]
    h = hash(length(expr.nodes))
    for node in expr.nodes
        node_expr = if node.type == MOI.Nonlinear.NODE_MOI_VARIABLE
            xi = variable_to_column[node.index]
            m = findfirst(==(xi), list_of_variables)
            if m === nothing
                push!(list_of_variables, xi)
                m = length(list_of_variables)
            end
            h = hash((node.type, m), h)
            MOI.VariableIndex(m)
        elseif node.type == MOI.Nonlinear.NODE_VALUE
            h = hash((node.type, node.index), h)
            MOI.VariableIndex(-node.index)
        elseif node.type == MOI.Nonlinear.NODE_CALL_MULTIVARIATE
            op = nlp.operators.multivariate_operators[node.index]
            h = hash(op, h)
            MOI.ScalarNonlinearFunction(op, Any[])
        elseif node.type == MOI.Nonlinear.NODE_CALL_UNIVARIATE
            op = nlp.operators.univariate_operators[node.index]
            h = hash(op, h)
            MOI.ScalarNonlinearFunction(op, Any[])
        elseif node.type == MOI.Nonlinear.NODE_LOGIC
            op = nlp.operators.logic_operators[node.index]
            h = hash(op, h)
            MOI.ScalarNonlinearFunction(op, Any[])
        elseif node.type == MOI.Nonlinear.NODE_COMPARISON
            op = nlp.operators.comparison_operators[node.index]
            h = hash(op, h)
            MOI.ScalarNonlinearFunction(op, Any[])
        elseif node.type == MOI.Nonlinear.NODE_SUBEXPRESSION
            error("Subexpressions not supported")
        else
            @assert node.type == MOI.Nonlinear.NODE_PARAMETER
            error("Parameters not supported")
        end
        if 1 <= node.parent <= length(tree)
            h = hash(node.parent, h)
            push!(tree[node.parent].args, node_expr)
        end
        push!(tree, node_expr)
    end
    return (;
        f = tree[1],
        hash = h,
        ordered_variables = list_of_variables,
        data = copy(expr.values),
    )
end

function Evaluator(
    model::MOI.Nonlinear.Model,
    ordered_variables::Vector{MOI.VariableIndex},
)
    variable_to_column =
        Dict(x.value => i for (i, x) in enumerate(ordered_variables))
    hash_to_dag = Dict{UInt64,_DAG}()
    hash_to_H = Dict{UInt64,Vector{Tuple{Int64,Int64}}}()
    objective = nothing
    constraints = _SymbolicInstance[]
    constraint_index_by_hash = Dict{UInt64,Vector{Int}}()
    jac_offset, hess_offset = 0, 0
    if model.objective !== nothing
        o_sym = _to_symbolic_form(model, model.objective, variable_to_column)
        x, ∇f, H, ∇²f = gradient_and_hessian(x -> x.value > 0, o_sym.f)
        dag = _DAG(model.operators, Any[o_sym.f; ∇f; ∇²f])
        hash_to_dag[o_sym.hash] = dag
        hash_to_H[o_sym.hash] = H
        objective = _SymbolicInstance(
            o_sym.hash,
            o_sym.ordered_variables,
            o_sym.data,
            zeros(Float64, length(dag.indices)),
            jac_offset,
            hess_offset,
        )
        hess_offset += length(H)
    end
    for (i, c) in model.constraints
        c_sym = _to_symbolic_form(model, c.expression, variable_to_column)
        if !haskey(hash_to_dag, c_sym.hash)
            x, ∇f, H, ∇²f = gradient_and_hessian(x -> x.value > 0, c_sym.f)
            hash_to_dag[c_sym.hash] =
                _DAG(model.operators, Any[c_sym.f; ∇f; ∇²f])
            hash_to_H[c_sym.hash] = H
        end
        dag = hash_to_dag[c_sym.hash]
        H = hash_to_H[c_sym.hash]
        g_i = _SymbolicInstance(
            c_sym.hash,
            c_sym.ordered_variables,
            c_sym.data,
            zeros(Float64, length(dag.indices)),
            jac_offset,
            hess_offset,
        )
        jac_offset += length(c_sym.ordered_variables)
        hess_offset += length(H)
        push!(constraints, g_i)
        if !haskey(constraint_index_by_hash, c_sym.hash)
            constraint_index_by_hash[c_sym.hash] = Int[length(constraints)]
        else
            push!(constraint_index_by_hash[c_sym.hash], length(constraints))
        end
    end
    return Evaluator(
        hash_to_dag,
        hash_to_H,
        objective,
        constraints,
        constraint_index_by_hash,
        Float64[],
    )
end

function _evaluate!(model::Evaluator, x)
    if x == model.x
        return
    end
    if length(x) != length(model.x)
        resize!(model.x, length(x))
    end
    copyto!(model.x, x)
    o = model.objective
    if o !== nothing
        _evaluate!(model.dag[o.hash], o, x)
    end
    for h in collect(keys(model.constraint_index_by_hash))
        dag = model.dag[h]
        for i in model.constraint_index_by_hash[h]
            _evaluate!(dag, model.constraints[i], x)
        end
    end
    return
end

MOI.features_available(::Evaluator) = [:Grad, :Jac, :Hess]

function MOI.initialize(model::Evaluator, features::Vector{Symbol})
    @assert all(f in MOI.features_available(model) for f in features)
    return
end

function MOI.eval_objective(model::Evaluator, x)
    _evaluate!(model, x)
    return model.objective.result[1]
end

function MOI.eval_objective_gradient(model::Evaluator, ∇f, x)
    _evaluate!(model, x)
    fill!(∇f, 0.0)
    for i in eachindex(model.objective.x)
        xi = model.objective.x[i]
        ∇f[xi] = model.objective.result[1+i]
    end
    return
end

function MOI.eval_constraint(model::Evaluator, g, x)
    _evaluate!(model, x)
    for i in eachindex(model.constraints)
        g[i] = model.constraints[i].result[1]
    end
    return
end

function MOI.jacobian_structure(model::Evaluator)
    d = last(model.constraints)
    J = Vector{Tuple{Int,Int}}(undef, d.jac_offset + length(d.x))
    for i in eachindex(model.constraints)
        c = model.constraints[i]
        for (j, xj) in enumerate(c.x)
            J[c.jac_offset+j] = (i, xj)
        end
    end
    return J
end

function MOI.eval_constraint_jacobian(model::Evaluator, J, x)
    _evaluate!(model, x)
    for c in model.constraints
        for j in eachindex(c.x)
            J[c.jac_offset+j] = c.result[1+j]
        end
    end
    return
end

function MOI.hessian_lagrangian_structure(model::Evaluator)
    o = model.objective
    n = if !isempty(model.constraints)
        d = last(model.constraints)
        d.hess_offset + length(d.result) - length(d.x) - 1
    elseif o !== nothing
        length(model.H[o.hash])
    else
        0  # No constraints and no objective
    end
    H = Vector{Tuple{Int,Int}}(undef, n)
    if o !== nothing
        for (k, (hi, hj)) in enumerate(model.H[o.hash])
            H[k] = (o.x[hi], o.x[hj])
        end
    end
    for c in model.constraints
        for (k, (hi, hj)) in enumerate(model.H[c.hash])
            H[c.hess_offset+k] = (c.x[hi], c.x[hj])
        end
    end
    return H
end

function MOI.eval_hessian_lagrangian(model::Evaluator, H, x, σ, μ)
    _evaluate!(model, x)
    o = model.objective
    if o !== nothing
        offset = o.hess_offset - length(o.x) - 1
        for j in (2+length(o.x)):length(o.result)
            H[offset+j] = σ * o.result[j]
        end
    end
    for i in eachindex(model.constraints)
        c = model.constraints[i]
        # It is important that `offset_c` does not shadow `offset` from the
        # objective block. If we use the same name, Julia creates a Core.Box
        offset_c = c.hess_offset - length(c.x) - 1
        for j in (2+length(c.x)):length(c.result)
            H[offset_c+j] = μ[i] * c.result[j]
        end
    end
    return
end

# A default fallback for all types
_add_to_affine!(::Any, ::Any, ::T) where {T} = nothing

# The creation of `ret::MOI.ScalarAffineFunction` has been delayed until now!
function _add_to_affine!(
    ::Nothing,
    f::Union{Real,MOI.VariableIndex,MOI.ScalarAffineFunction},
    scale::T,
) where {T}
    return _add_to_affine!(zero(MOI.ScalarAffineFunction{T}), f, scale)
end

function _add_to_affine!(
    ret::MOI.ScalarAffineFunction{T},
    x::S,
    scale::T,
) where {T,S<:Real}
    if promote_type(T, S) != T
        return  # We can't store `S` in `T`.
    end
    ret.constant += scale * convert(T, x)
    return ret
end

function _add_to_affine!(
    ret::MOI.ScalarAffineFunction{T},
    x::MOI.VariableIndex,
    scale::T,
) where {T}
    push!(ret.terms, MOI.ScalarAffineTerm(scale, x))
    return ret
end

function _add_to_affine!(
    ret::MOI.ScalarAffineFunction{T},
    f::MOI.ScalarAffineFunction{S},
    scale::T,
) where {T,S}
    if promote_type(T, S) != T
        return  # We can't store `S` in `T`.
    end
    ret = _add_to_affine!(ret, f.constant, scale)
    for term in f.terms
        ret = _add_to_affine!(ret, term.variable, scale * term.coefficient)
    end
    return ret
end

function _add_to_affine!(
    ret::Union{Nothing,MOI.ScalarAffineFunction{T}},
    f::MOI.ScalarNonlinearFunction,
    scale::T,
) where {T}
    if f.head == :+
        for arg in f.args
            ret = _add_to_affine!(ret, arg, scale)
            if ret === nothing
                return
            end
        end
        return ret
    elseif f.head == :-
        if length(f.args) == 1
            return _add_to_affine!(ret, only(f.args), -scale)
        end
        @assert length(f.args) == 2
        ret = _add_to_affine!(ret, f.args[1], scale)
        if ret === nothing
            return
        end
        return _add_to_affine!(ret, f.args[2], -scale)
    elseif f.head == :*
        y = nothing
        for arg in f.args
            if arg isa Real
                scale *= arg
            elseif y === nothing
                y = arg
            else
                return # We already have a `y`. Can't multiple factors.
            end
        end
        return _add_to_affine!(ret, something(y, one(T)), convert(T, scale))
    end
    return  # An unsupported f.head
end

function _simplify_if_affine!(f::MOI.ScalarNonlinearFunction)
    ret = _add_to_affine!(nothing, f, 1.0)
    if ret === nothing
        return f
    end
    return simplify!(ret::MOI.ScalarAffineFunction{Float64})
end

_simplify_if_affine!(f::Any) = f

end  # module
