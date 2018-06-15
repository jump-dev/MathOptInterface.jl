"""
    RSOCBridge{T}

The `RotatedSecondOrderCone` is `SecondOrderCone` representable; see [1, p. 104].
Indeed, we have ``2tu = (t/√2 + u/√2)^2 - (t/√2 - u/√2)^2`` hence
```math
2tu \\ge || x ||_2^2
```
is equivalent to
```math
(t/√2 + u/√2)^2 \\ge || x ||_2^2 + (t/√2 - u/√2)^2.
```
We can therefore use the transformation ``(t, u, x) \\mapsto (t/√2+u/√2, t/√2-u/√2, x)``.
Note that the linear transformation is a symmetric involution (i.e. it is its own transpose and its own inverse).
That means in particular that the norm is of constraint primal and duals are preserved by the tranformation.

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex optimization: analysis, algorithms, and engineering applications*. Society for Industrial and Applied Mathematics, 2001.
"""
struct RSOCBridge{T} <: AbstractBridge
    soc::CI{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone}
end
function RSOCBridge{T}(model, f::MOI.VectorOfVariables, s::MOI.RotatedSecondOrderCone) where T
    RSOCBridge{T}(model, MOI.VectorAffineFunction{T}(f), s)
end
function RSOCBridge{T}(model, f::MOI.VectorAffineFunction{T}, s::MOI.RotatedSecondOrderCone) where T
    d = s.dimension
    t = MOIU.eachscalar(f)[1]
    u = MOIU.eachscalar(f)[2]
    x = MOIU.eachscalar(f)[3:d]
    s2 = √2
    y = MOI.ScalarAffineFunction([mapcoefficient.(c -> c/s2, t.terms); mapcoefficient.(c -> -c/s2, u.terms)], t.constant/s2 - u.constant/s2)
    z = MOI.ScalarAffineFunction([mapcoefficient.(c -> c/s2, t.terms); mapcoefficient.(c ->  c/s2, u.terms)], t.constant/s2 + u.constant/s2)
    g = MOIU.moivcat(z, y, x)
    soc = MOI.addconstraint!(model, g, MOI.SecondOrderCone(d))
    RSOCBridge{T}(soc)
end

MOI.supportsconstraint(::Type{RSOCBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.RotatedSecondOrderCone}) where T = true
addedconstrainttypes(::Type{RSOCBridge{T}}, ::Type{<:Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}}, ::Type{MOI.RotatedSecondOrderCone}) where T = [(MOI.VectorAffineFunction{T}, MOI.SecondOrderCone)]

# Attributes, Bridge acting as an model
MOI.get(b::RSOCBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone}) where T = 1
MOI.get(b::RSOCBridge{T}, ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone}) where T = [b.soc]

# References
function MOI.delete!(model::MOI.ModelLike, c::RSOCBridge)
    MOI.delete!(model, c.soc)
end

# Attributes, Bridge acting as a constraint
# As the linear transformation is a symmetric involution,
# the constraint primal and dual both need to be processed by reapplying the same transformation
function _get(model, attr::Union{MOI.ConstraintPrimal, MOI.ConstraintDual}, c::RSOCBridge)
    x = MOI.get(model, attr, c.soc)
    s2 = √2
    [x[1]/s2+x[2]/s2; x[1]/s2-x[2]/s2; x[3:end]]
end
# Need to define both `get` methods and redirect to `_get` to avoid ambiguity in dispatch
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintPrimal, ::Type{RSOCBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone})
end
MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintPrimal, c::RSOCBridge) = _get(model, attr, c)
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintDual, ::Type{RSOCBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone})
end
MOI.get(model::MOI.ModelLike, attr::MOI.ConstraintDual, c::RSOCBridge) = _get(model, attr, c)

MOI.canmodify(model::MOI.ModelLike, ::Type{<:RSOCBridge}, ::Type{<:MOI.AbstractFunctionModification}) = false
