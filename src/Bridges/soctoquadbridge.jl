"""
    SOCtoQuadBridge{T}

Constraints of the form `VectorOfVariables`-in-`SecondOrderCone` (resp.
`VectorAffineFunction`-in-`SecondOrderCone`) can be transformed into a
`ScalarQuadraticFunction`-in-`GreaterThan` and a
`SingleVariable`-in-`GreaterThan` (resp.
`ScalarAffineFunction`-in-`SecondOrderCone`). Indeed, the definition of the
second-order cone
```math
t \\ge || x ||_2 \\}
```
is equivalent to
```math
\\sum x_i^2 \\le t^2
```
with ``t \\ge 0``.
"""
struct SOCtoQuadBridge{T, F, G} <: AbstractBridge
    quad::CI{F, MOI.GreaterThan{T}}
    t_nonneg::CI{G, MOI.GreaterThan{T}}
end
function SOCtoQuadBridge{T, F, G}(model::MOI.ModelLike,
                                  f::MOI.AbstractVectorFunction,
                                  s::MOI.SecondOrderCone) where {T, F, G}
    d = s.dimension
    f_scalars = MOIU.eachscalar(f)
    t = f_scalars[1]
    t_nonneg = MOIU.add_scalar_constraint(model, t, MOI.GreaterThan(zero(T)))
    quad_f = MOIU.operate(*, T, t, t)
    for i in 2:d
        x = f_scalars[i]
        quad_f = MOIU.operate!(-, T, quad_f, MOIU.operate(*, T, x, x))
    end
    quad = MOIU.add_scalar_constraint(model, quad_f, MOI.GreaterThan(zero(T)))
    return SOCtoQuadBridge{T, F, G}(quad, t_nonneg)
end

function MOI.supportsconstraint(::Type{SOCtoQuadBridge{T}},
                                ::Type{<:MOI.AbstractVectorFunction},
                                ::Type{MOI.SecondOrderCone}) where T
    return true
end
function addedconstrainttypes(::Type{SOCtoQuadBridge{T, F, G}}) where {T, F, G}
    list = [(F, MOI.GreaterThan{T})]
    if F != G
        push!(list, (G, MOI.GreaterThan{T}))
    end
    return list
end
function concrete_bridge_type(::Type{<:SOCtoQuadBridge{T}},
                              H::Type{<:MOI.AbstractVectorFunction},
                              ::Type{MOI.SecondOrderCone}) where T
    G = MOIU.scalar_type(H)
    Q = MOIU.promote_operation(*, T, G, G)
    F = MOIU.promote_operation(-, T, Q, Q)
    return SOCtoQuadBridge{T, F, G}
end

# Attributes, Bridge acting as an model
function MOI.get(b::SOCtoQuadBridge{T, F, G},
                 ::MOI.NumberOfConstraints{H, MOI.GreaterThan{T}}) where {T, F, G, H}
    return (F == H) + (G == H)
end
function MOI.get(b::SOCtoQuadBridge{T, F, G},
                 ::MOI.ListOfConstraintIndices{H, MOI.GreaterThan{T}}) where {T, F, G, H}
    list = CI{H, MOI.GreaterThan{T}}[]
    if F == H
        push!(list, b.quad)
    end
    if G == H
        push!(list, b.t_nonneg)
    end
    return list
end

# References
function MOI.delete!(model::MOI.ModelLike, bridge::SOCtoQuadBridge)
    MOI.delete!(model, bridge.t_nonneg)
    MOI.delete!(model, bridge.quad)
end

need_constraint_primal_fallback(::Type{<:SOCtoQuadBridge}) = true
