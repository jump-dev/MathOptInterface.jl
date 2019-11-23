function _ilog2(n, i)
    if n <= (one(n) << i)
        i
    else
        _ilog2(n, i+1)
    end
end
function ilog2(n::Integer)
    @assert n > zero(n)
    _ilog2(n, zero(n))
end

"""
    GeoMeanBridge{T, F, G, H}

The `GeometricMeanCone` is `SecondOrderCone` representable; see [1, p. 105].
The reformulation is best described in an example.
Consider the cone of dimension 4
```math
t \\le \\sqrt[3]{x_1 x_2 x_3}
```
This can be rewritten as ``\\exists x_{21} \\ge 0`` such that
```math
\\begin{align*}
  t & \\le x_{21},\\\\
  x_{21}^4 & \\le x_1 x_2 x_3 x_{21}.
\\end{align*}
```
Note that we need to create ``x_{21}`` and not use ``t^4`` directly as ``t`` is allowed to be negative.
Now, this is equivalent to
```math
\\begin{align*}
  t & \\le x_{21}/\\sqrt{4},\\\\
  x_{21}^2 & \\le 2x_{11} x_{12},\\\\
  x_{11}^2 & \\le 2x_1 x_2, & x_{12}^2 & \\le 2x_3(x_{21}/\\sqrt{4}).
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex optimization: analysis, algorithms, and engineering applications*. Society for Industrial and Applied Mathematics, 2001.
"""
struct GeoMeanBridge{T, F, G, H} <: AbstractBridge
    # Initially, (t, x) is of dimension d so x is dimension (d-1)
    # We create n new variables so that there is 2^l = d-1+n variables x_i
    # We then need to create 2^l-1 new variables (1+2+...+2^{l-1})
    d::Int
    xij::Vector{MOI.VariableIndex}
    tubc::CI{F, MOI.LessThan{T}}
    socrc::Vector{CI{G, MOI.RotatedSecondOrderCone}}
end
function bridge_constraint(::Type{GeoMeanBridge{T, F, G, H}}, model,
                           f::MOI.AbstractVectorFunction,
                           s::MOI.GeometricMeanCone) where {T, F, G, H}
    d = s.dimension
    n = d - 1
    l = ilog2(n)
    N = 1 << l
    xij = MOI.add_variables(model, N-1)
    f_scalars = MOIU.eachscalar(f)

    xl1 = MOI.SingleVariable(xij[1])
    sN = one(T) / √N
    SG = MOIU.scalar_type(G)
    function _getx(i)::SG
        if i > n
            return sN * xl1
        else
            return f_scalars[1 + i]
        end
    end

    t = f_scalars[1]
    # With sqrt(2)^l*t - xl1, we should scale both the ConstraintPrimal and ConstraintDual
    tubc = MOIU.normalize_and_add_constraint(
        model, MOIU.operate!(+, T, t, -sN * xl1), MOI.LessThan(zero(T)),
        allow_modify_function=true)

    socrc = Vector{CI{G, MOI.RotatedSecondOrderCone}}(undef, N - 1)
    offset = offset_next = 0
    for i in 1:l
        offset_next = offset + i
        for j in 1:(1 << (i - 1))
            if i == l
                a = _getx(2j - 1)
                b = _getx(2j)
            else
                a = convert(SG, MOI.SingleVariable(xij[offset_next + 2j - 1]))
                b = convert(SG, MOI.SingleVariable(xij[offset_next + 2j]))
            end
            c = MOI.SingleVariable(xij[offset+j])
            socrc[offset + j] = MOI.add_constraint(
                model, MOIU.operate(vcat, T, a, b, c),
                MOI.RotatedSecondOrderCone(3))
        end
        offset = offset_next
    end
    return GeoMeanBridge{T, F, G, H}(d, xij, tubc, socrc)
end

function MOI.supports_constraint(::Type{GeoMeanBridge{T}},
                                ::Type{<:MOI.AbstractVectorFunction},
                                ::Type{MOI.GeometricMeanCone}) where T
    return true
end
MOIB.added_constrained_variable_types(::Type{<:GeoMeanBridge}) = Tuple{DataType}[]
function MOIB.added_constraint_types(::Type{<:GeoMeanBridge{T, F, G}}) where {T, F, G}
    return [(F, MOI.LessThan{T}), (G, MOI.RotatedSecondOrderCone)]
end
function concrete_bridge_type(::Type{<:GeoMeanBridge{T}},
                              H::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.GeometricMeanCone}) where T
    S = MOIU.scalar_type(H)
    A = MOIU.promote_operation(*, T, T, MOI.SingleVariable)
    F = MOIU.promote_operation(+, T, S, A)
    G = MOIU.promote_operation(vcat, T, A, S, MOI.SingleVariable)
    return GeoMeanBridge{T, F, G, H}
end

# Attributes, Bridge acting as a model
MOI.get(b::GeoMeanBridge, ::MOI.NumberOfVariables) = length(b.xij)
MOI.get(b::GeoMeanBridge, ::MOI.ListOfVariableIndices) = b.xij
function MOI.get(b::GeoMeanBridge{T, F},
                 ::MOI.NumberOfConstraints{F, MOI.LessThan{T}}) where {T, F}
    return 1 # t ≤ x_{l1}/sqrt(N)
end
function MOI.get(b::GeoMeanBridge{T, F, G},
                 ::MOI.NumberOfConstraints{G, MOI.RotatedSecondOrderCone}) where {T, F, G}
    return length(b.socrc)
end
function MOI.get(b::GeoMeanBridge{T, F},
                 ::MOI.ListOfConstraintIndices{F, MOI.LessThan{T}}) where {T, F}
    return [b.tubc]
end
function MOI.get(b::GeoMeanBridge{T, F, G},
                 ::MOI.ListOfConstraintIndices{G, MOI.RotatedSecondOrderCone}) where {T, F, G}
    return b.socrc
end

# References
function MOI.delete(model::MOI.ModelLike, c::GeoMeanBridge)
    MOI.delete(model, c.xij)
    MOI.delete(model, c.tubc)
    MOI.delete(model, c.socrc)
end

# Attributes, Bridge acting as a constraint
function MOI.get(::MOI.ModelLike, ::MOI.ConstraintSet, bridge::GeoMeanBridge)
    return MOI.GeometricMeanCone(bridge.d)
end
function MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintFunction,
                 bridge::GeoMeanBridge{T, F, G, H}) where {T, F, G, H}
    tub = MOI.get(model, attr, bridge.tubc)
    rhs = MOI.constant(MOI.get(model, MOI.ConstraintSet(), bridge.tubc))
    tub = MOIU.operate(-, T, tub, rhs)
    t = MOIU.remove_variable(tub, bridge.xij[1])
    f_scalars = Vector{MOIU.scalar_type(H)}(undef, bridge.d)
    f_scalars[1] = t
    d = bridge.d
    n = d - 1
    l = ilog2(n)
    offset = offset_next = 0
    for i in 1:l
        offset_next = offset + i
        for j in 1:(1 << (i-1))
            if i == l && 2j <= bridge.d
                func = MOI.get(model, attr, bridge.socrc[offset + j])
                func_scalars = MOIU.eachscalar(func)
                f_scalars[2j] = func_scalars[1]
                f_scalars[2j + 1] = func_scalars[2]
            end
        end
        offset = offset_next
    end
    return MOIU.vectorize(f_scalars)
end
function _getconstrattr(model, a, c::GeoMeanBridge{T}) where T
    output = Vector{T}(undef, c.d)
    output[1] = MOI.get(model, a, c.tubc)
    N = length(c.xij)+1
    offset = div(N, 2) - 1 # 1 + 2 + ... + n/4
    for i in 1:(c.d-1)
        j = ((i-1) >> 1) + 1
        k = i - 2(j - 1)
        output[1+i] = MOI.get(model, a, c.socrc[offset+j])[k]
    end
    output
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintPrimal, c::GeoMeanBridge)
    output = _getconstrattr(model, a, c)
    N = length(c.xij)+1
    # the constraint is t - x_l1/sqrt(2^l) ≤ 0, we need to add the value of x_l1
    output[1] += MOI.get(model, MOI.VariablePrimal(), c.xij[1]) / sqrt(N)
    output
end
#function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::GeoMeanBridge)
#    output = _getconstrattr(model, a, c)
#end
