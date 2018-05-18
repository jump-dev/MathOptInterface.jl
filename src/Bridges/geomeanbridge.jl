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
    GeoMeanBridge{T}

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
  x_{11}^2 & \\le 2x_1 x_2, & x_{21}^2 & \\le 2x_3(x_{21}/\\sqrt{4}).
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex optimization: analysis, algorithms, and engineering applications*. Society for Industrial and Applied Mathematics, 2001.
"""
struct GeoMeanBridge{T} <: AbstractBridge
    # Initially, (t, x) is of dimension d so x is dimension (d-1)
    # We create n new variables so that there is 2^l = d-1+n variables x_i
    # We then need to create 2^l-1 new variables (1+2+...+2^{l-1})
    d::Int
    xij::Vector{VI}
    tubc::CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}
    socrc::Vector{CI{MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone}}
end
function GeoMeanBridge{T}(model, f::MOI.VectorOfVariables, s::MOI.GeometricMeanCone) where T
    GeoMeanBridge{T}(model, MOI.VectorAffineFunction{T}(f), s)
end
function GeoMeanBridge{T}(model, f::MOI.VectorAffineFunction{T}, s::MOI.GeometricMeanCone) where T
    d = s.dimension
    n = d-1
    l = ilog2(n)
    N = 1 << l
    xij = MOI.addvariables!(model, N-1)

    xl1 = xij[1]
    sN = one(T) / sqrt(N)
    function _getx(i)
        if i > n
            MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(sN, xl1)], zero(T))
        else
            MOIU.eachscalar(f)[1+i]
        end
    end

    t = MOIU.eachscalar(f)[1]
    # With sqrt(2)^l*t - xl1, we should scale both the ConstraintPrimal and ConstraintDual
    tubc = MOI.addconstraint!(model, MOI.ScalarAffineFunction([t.terms; MOI.ScalarAffineTerm(-sN, xl1)], t.constant), MOI.LessThan(zero(T)))

    socrc = Vector{CI{MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone}}(N-1)
    offset = offsetnext = 0
    for i in 1:l
        offsetnext = offset + i
        for j in 1:(1 << (i-1))
            if i == l
                a = _getx(2j-1)
                b = _getx(2j)
            else
                a = MOI.ScalarAffineFunction{T}(MOI.SingleVariable(xij[offsetnext+2j-1]))
                b = MOI.ScalarAffineFunction{T}(MOI.SingleVariable(xij[offsetnext+2j]))
            end
            c = MOI.ScalarAffineFunction{T}(MOI.SingleVariable(xij[offset+j]))
            socrc[offset + j] = MOI.addconstraint!(model, MOIU.moivcat(a, b, c), MOI.RotatedSecondOrderCone(3))
        end
        offset = offsetnext
    end
    GeoMeanBridge(d, xij, tubc, socrc)
end

MOI.supportsconstraint(::Type{GeoMeanBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.GeometricMeanCone}) where T = true
addedconstrainttypes(::Type{GeoMeanBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.GeometricMeanCone}) where T = [(MOI.ScalarAffineFunction{T}, MOI.LessThan{T}), (MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone)]

# Attributes, Bridge acting as an model
MOI.get(b::GeoMeanBridge, ::MOI.NumberOfVariables) = length(b.xij)
MOI.get(b::GeoMeanBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where T = 1 # t ≤ x_{l1}/sqrt(N)
MOI.get(b::GeoMeanBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone}) where T = length(b.socrc)
MOI.get(b::GeoMeanBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where T = [b.tubc]
MOI.get(b::GeoMeanBridge{T}, ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone}) where T = b.socrc

# References
function MOI.delete!(model::MOI.ModelLike, c::GeoMeanBridge)
    MOI.delete!(model, c.xij)
    MOI.delete!(model, c.tubc)
    MOI.delete!(model, c.socrc)
end

# Attributes, Bridge acting as a constraint
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintPrimal, ::Type{GeoMeanBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) &&
    MOI.canget(model, a, CI{MOI.VectorAffineFunction{T}, MOI.RotatedSecondOrderCone})
end
function _getconstrattr(model, a, c::GeoMeanBridge{T}) where T
    output = Vector{T}(c.d)
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
MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintDual, ::Type{<:GeoMeanBridge}) = false
#function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::GeoMeanBridge)
#    output = _getconstrattr(model, a, c)
#end

# Constraints
MOI.canmodifyconstraint(model::MOI.ModelLike, c::GeoMeanBridge, change) = false
