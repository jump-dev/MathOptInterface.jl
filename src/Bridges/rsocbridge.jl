"""
    RSOCBridge{T}

The `RotatedSecondOrderCone` is `SecondOrderCone` representable; see [1, p. 104].
Indeed, we have ``2tu = (t/√2 + u/√2)^2 - (t/√2 - u/√2)^2 = (t + u/2)^2 - (t - u/2)^2`` hence
```math
2tu \\ge || x ||_2^2
```
is equivalent to
```math
(t + u/2)^2 \\ge || x ||_2^2 + (t - u/2)^2.
```
We can therefore use the transformation ``(t, u, x) \\mapsto (t+u/2, t-u/2, x)``.
The transformation ``(t, u, x) \\mapsto (t/\\sqrt{2}+u/\\sqrt{2}, t/\\sqrt{2}-u/\\sqrt{2}, x)``
would also work and is its own inverse but it would not preserve the rationality of the input.

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
    y = MOI.ScalarAffineFunction([t.variables; u.variables], [t.coefficients; -u.coefficients/2], t.constant - u.constant/2)
    z = MOI.ScalarAffineFunction([t.variables; u.variables], [t.coefficients;  u.coefficients/2], t.constant + u.constant/2)
    g = MOIU.moivcat(z, y, x)
    soc = MOI.addconstraint!(model, g, MOI.SecondOrderCone(d))
    RSOCBridge{T}(soc)
end
# Attributes, Bridge acting as an model
MOI.get(b::RSOCBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone}) where T = 1
MOI.get(b::RSOCBridge{T}, ::MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone}) where T = [b.soc]

# References
function MOI.delete!(model::MOI.ModelLike, c::RSOCBridge)
    MOI.delete!(model, c.soc)
end

# Attributes, Bridge acting as a constraint
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintPrimal, ::Type{RSOCBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone})
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintPrimal, c::RSOCBridge)
    cp = MOI.get(model, a, c.soc)
    [cp[1]/2+cp[2]/2; cp[1]-cp[2]; cp[3:end]]
end
function MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintDual, ::Type{RSOCBridge{T}}) where T
    MOI.canget(model, a, CI{MOI.VectorAffineFunction{T}, MOI.SecondOrderCone})
end
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::RSOCBridge)
    cd = MOI.get(model, a, c.soc)
    [cd[1]+cd[2]; cd[1]/2-cd[2]/2; cd[3:end]]
end

# Constraints
MOI.canmodifyconstraint(model::MOI.ModelLike, c::RSOCBridge, change) = false
