# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module SymbolicAD

import MathOptInterface as MOI

"""
    simplify(f)

Return a simplified version of the function `f`.

!!! warning
    This function is not type stable by design.
"""
simplify(f) = f

function simplify(f::MOI.ScalarAffineFunction{T}) where {T}
    f = MOI.Utilities.canonical(f)
    if isempty(f.terms)
        return f.constant
    end
    return f
end

function simplify(f::MOI.ScalarQuadraticFunction{T}) where {T}
    f = MOI.Utilities.canonical(f)
    if isempty(f.quadratic_terms)
        return simplify(MOI.ScalarAffineFunction(f.affine_terms, f.constant))
    end
    return f
end

function simplify(f::MOI.ScalarNonlinearFunction)
    for i in 1:length(f.args)
        f.args[i] = simplify(f.args[i])
    end
    return _eval_if_constant(simplify(Val(f.head), f))
end

function simplify(f::MOI.VectorAffineFunction{T}) where {T}
    f = MOI.Utilities.canonical(f)
    if isempty(f.terms)
        return f.constant
    end
    return f
end

function simplify(f::MOI.VectorQuadraticFunction{T}) where {T}
    f = MOI.Utilities.canonical(f)
    if isempty(f.quadratic_terms)
        return simplify(MOI.VectorAffineFunction(f.affine_terms, f.constants))
    end
    return f
end

function simplify(f::MOI.VectorNonlinearFunction)
    return MOI.VectorNonlinearFunction(simplify.(f.rows))
end

# If a ScalarNonlinearFunction has only constant arguments, we should return
# the vaålue.

_isnum(::Any) = false

_isnum(::Union{Bool,Integer,Float64}) = true

function _eval_if_constant(f::MOI.ScalarNonlinearFunction)
    if all(_isnum, f.args) && hasproperty(Base, f.head)
        return getproperty(Base, f.head)(f.args...)
    end
    return f
end

_eval_if_constant(f) = f

_iszero(x::Any) = _isnum(x) && iszero(x)

_isone(x::Any) = _isnum(x) && isone(x)

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
    simplify(::Val{head}, f::MOI.ScalarNonlinearFunction)

Return a simplified version of `f` where the head of `f` is `head`.

Implementing this method enables custom simplification rules for different
operators without needing a giant switch statement.
"""
simplify(::Val, f::MOI.ScalarNonlinearFunction) = f

function simplify(::Val{:*}, f::MOI.ScalarNonlinearFunction)
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
    if isempty(new_args)
        return true
    elseif length(new_args) == 1
        return only(new_args)
    end
    return MOI.ScalarNonlinearFunction(:*, new_args)
end

function simplify(::Val{:+}, f::MOI.ScalarNonlinearFunction)
    if length(f.args) == 1
        # +(x) -> x
        return only(f.args)
    elseif length(f.args) == 2 && _isexpr(f.args[2], :-, 1)
        # +(x, -y) -> -(x, y)
        return MOI.ScalarNonlinearFunction(
            :-,
            Any[f.args[1], f.args[2].args[1]],
        )
    end
    new_args = Any[]
    first_constant = 0
    for arg in f.args
        if _isexpr(arg, :+)
            # If a child is a :+, lift its arguments to the parent
            append!(new_args, arg.args)
        elseif _iszero(arg)
            # Skip any zero arguments
        elseif arg isa Real
            # Collect all constant arguments into a single value
            if first_constant == 0
                push!(new_args, arg)
                first_constant = length(new_args)
            else
                new_args[first_constant] += arg
            end
        else
            push!(new_args, arg)
        end
    end
    if isempty(new_args)
        # +() -> false
        return false
    elseif length(new_args) == 1
        # +(x) -> x
        return only(new_args)
    end
    return MOI.ScalarNonlinearFunction(:+, new_args)
end

function simplify(::Val{:-}, f::MOI.ScalarNonlinearFunction)
    if length(f.args) == 1
        if _isexpr(f.args[1], :-, 1)
            # -(-(x)) => x
            return f.args[1].args[1]
        end
    elseif length(f.args) == 2
        if _iszero(f.args[1])
            # 0 - x => -x
            return MOI.ScalarNonlinearFunction(:-, Any[f.args[2]])
        elseif _iszero(f.args[2])
            # x - 0 => x
            return f.args[1]
        elseif f.args[1] == f.args[2]
            # x - x => 0
            return false
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

function simplify(::Val{:^}, f::MOI.ScalarNonlinearFunction)
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

end  # module
