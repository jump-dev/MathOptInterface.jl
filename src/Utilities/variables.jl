# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    get_bounds(model::MOI.ModelLike, ::Type{T}, x::MOI.VariableIndex)

Return a tuple `(lb, ub)` of type `Tuple{T, T}`, where `lb` and `ub` are lower
 and upper bounds, respectively, imposed on `x` in `model`.
"""
function get_bounds(
    model::MOI.ModelLike,
    ::Type{T},
    x::F,
) where {T,F<:MOI.VariableIndex}
    # MOI.Interval
    c_interval = MOI.ConstraintIndex{F,MOI.Interval{T}}(x.value)
    if MOI.is_valid(model, c_interval)
        s_interval = MOI.get(model, MOI.ConstraintSet(), c_interval)
        return s_interval.lower, s_interval.upper
    end
    # MOI.EqualTo
    c_equal_to = MOI.ConstraintIndex{F,MOI.EqualTo{T}}(x.value)
    if MOI.is_valid(model, c_equal_to)
        s_equal_to = MOI.get(model, MOI.ConstraintSet(), c_equal_to)
        return s_equal_to.value, s_equal_to.value
    end
    # MOI.Semicontinuous
    c_semicontinuous = MOI.ConstraintIndex{F,MOI.Semicontinuous{T}}(x.value)
    if MOI.is_valid(model, c_semicontinuous)
        s_semicontinuous = MOI.get(model, MOI.ConstraintSet(), c_semicontinuous)
        l = min(zero(T), s_semicontinuous.lower)
        u = max(zero(T), s_semicontinuous.upper)
        return l, u
    end
    # MOI.Semiinteger
    c_si = MOI.ConstraintIndex{F,MOI.Semiinteger{T}}(x.value)
    if MOI.is_valid(model, c_si)
        si::MOI.Semiinteger{T} = MOI.get(model, MOI.ConstraintSet(), c_si)
        return min(zero(T), si.lower), max(zero(T), si.upper)
    end
    l, u = typemin(T), typemax(T)
    # MOI.LessThan
    c_less_than = MOI.ConstraintIndex{F,MOI.LessThan{T}}(x.value)
    if MOI.is_valid(model, c_less_than)
        s_less_than = MOI.get(model, MOI.ConstraintSet(), c_less_than)
        u = min(u, s_less_than.upper)
    end
    # MOI.GreaterThan
    c_greater_than = MOI.ConstraintIndex{F,MOI.GreaterThan{T}}(x.value)
    if MOI.is_valid(model, c_greater_than)
        s_greater_than = MOI.get(model, MOI.ConstraintSet(), c_greater_than)
        l = max(l, s_greater_than.lower)
    end
    return l, u
end

"""
    get_bounds(
        model::MOI.ModelLike,
        bounds_cache::Dict{MOI.VariableIndex,NTuple{2,T}},
        f::MOI.ScalarAffineFunction{T},
    ) where {T} --> Union{Nothing,NTuple{2,T}}

Return the lower and upper bound of `f` as a tuple. If the domain is not bounded,
return `nothing`.
"""
function get_bounds(
    model::MOI.ModelLike,
    bounds_cache::Dict{MOI.VariableIndex,NTuple{2,T}},
    f::MOI.ScalarAffineFunction{T},
) where {T}
    if !is_canonical(f)
        f = canonical(f)
    end
    lb = ub = f.constant
    for term in f.terms
        ret = get_bounds(model, bounds_cache, term.variable)
        if ret === nothing
            return nothing
        end
        if term.coefficient >= 0
            lb += term.coefficient * ret[1]
            ub += term.coefficient * ret[2]
        else
            lb += term.coefficient * ret[2]
            ub += term.coefficient * ret[1]
        end
    end
    return lb, ub
end

"""
    get_bounds(
        model::MOI.ModelLike,
        bounds_cache::Dict{MOI.VariableIndex,NTuple{2,T}},
        x::MOI.VariableIndex,
    ) where {T} --> Union{Nothing,NTuple{2,T}}

Return the lower and upper bound of `x` as a tuple. If the domain is not bounded,
return `nothing`.

Similar to `get_bounds(::MOI.ModelLike, ::Type{T}, ::MOI.VariableIndex)`, except
that the second argument is a cache which maps variables to their bounds and
avoids repeated lookups.
"""
function get_bounds(
    model::MOI.ModelLike,
    bounds_cache::Dict{MOI.VariableIndex,NTuple{2,T}},
    x::MOI.VariableIndex,
) where {T}
    if haskey(bounds_cache, x)
        return bounds_cache[x]
    end
    l, u = get_bounds(model, T, x)
    ci = MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value)
    if MOI.is_valid(model, ci)
        l, u = max(l, zero(T)), min(u, one(T))
    end
    if l == typemin(T) || u == typemax(T)
        return nothing
    end
    bounds_cache[x] = (l, u)
    return (l, u)
end
