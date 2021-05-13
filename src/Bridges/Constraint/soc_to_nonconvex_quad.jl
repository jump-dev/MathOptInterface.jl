abstract type AbstractSOCtoNonConvexQuadBridge{T} <: AbstractBridge end

"""
    SOCtoNonConvexQuadBridge{T}

Constraints of the form `VectorOfVariables`-in-`SecondOrderCone` can be
transformed into a `ScalarQuadraticFunction`-in-`LessThan` and a
`ScalarAffineFunction`-in-`GreaterThan`. Indeed, the definition of the
second-order cone
```math
t \\ge \\lVert x \\rVert_2 \\  (1)
```
is equivalent to
```math
\\sum x_i^2 \\le t^2  (2)
```
with ``t \\ge 0``.  (3)

*WARNING* This transformation starts from a convex constraint (1) and creates a
non-convex constraint (2), because the Q matrix associated with the constraint 2
has one negative eigenvalue. This might be wrongly interpreted by a solver.
Some solvers can look at (2) and understand that it is a second order cone, but
this is not a general rule.
For these reasons this bridge is not automatically added by [`MOI.Bridges.full_bridge_optimizer`](@ref).
Care is recommended when adding this bridge to a optimizer.
"""
struct SOCtoNonConvexQuadBridge{T} <: AbstractSOCtoNonConvexQuadBridge{T}
    quad::CI{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}}
    var_pos::Vector{CI{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}}
    vars::Vector{MOI.VariableIndex}
end

function bridge_constraint(
    ::Type{SOCtoNonConvexQuadBridge{T}},
    model,
    func::MOI.VectorOfVariables,
    set::MOI.SecondOrderCone,
) where {T}
    vis = func.variables

    t = vis[1]
    x = vis[2:end]
    a_terms = MOI.ScalarAffineTerm{T}[]
    q_terms = MOI.ScalarQuadraticTerm{T}[]
    push!(q_terms, MOI.ScalarQuadraticTerm(-T(2), t, t))
    for var in x
        push!(q_terms, MOI.ScalarQuadraticTerm(T(2), var, var))
    end

    fq = MOI.ScalarQuadraticFunction(a_terms, q_terms, zero(T))
    quad = MOI.add_constraint(model, fq, MOI.LessThan(zero(T)))
    # ScalarAffineFunction's are added instead of SingleVariable's
    # because models can only have one SingleVariable per variable.
    # Hence, adding a SingleVariable constraint here could conflict with
    # a user defined SingleVariable
    fp = convert(MOI.ScalarAffineFunction{T}, MOI.SingleVariable(t))
    var_pos = MOI.add_constraint(model, fp, MOI.GreaterThan(zero(T)))

    return SOCtoNonConvexQuadBridge(quad, [var_pos], vis)
end

"""
    RSOCtoNonConvexQuadBridge{T}

Constraints of the form `VectorOfVariables`-in-`SecondOrderCone` can be
transformed into a `ScalarQuadraticFunction`-in-`LessThan` and a
`ScalarAffineFunction`-in-`GreaterThan`. Indeed, the definition of the
second-order cone
```math
2tu \\ge \\lVert x \\rVert_2^2, t,u \\ge 0  (1)
```
is equivalent to
```math
\\sum x_i^2 \\le 2tu  (2)
```
with ``t,u \\ge 0``.  (3)

*WARNING* This transformation starts from a convex constraint (1) and creates a
non-convex constraint (2), because the Q matrix associated with the constraint 2
has two negative eigenvalues. This might be wrongly interpreted by a solver.
Some solvers can look at (2) and understand that it is a rotated second order cone, but
this is not a general rule.
For these reasons, this bridge is not automatically added by [`MOI.Bridges.full_bridge_optimizer`](@ref).
Care is recommended when adding this bridge to an optimizer.
"""
struct RSOCtoNonConvexQuadBridge{T} <: AbstractSOCtoNonConvexQuadBridge{T}
    quad::CI{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}}
    var_pos::Vector{CI{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}}}
    vars::Vector{MOI.VariableIndex}
end

function bridge_constraint(
    ::Type{RSOCtoNonConvexQuadBridge{T}},
    model,
    func::MOI.VectorOfVariables,
    set::MOI.RotatedSecondOrderCone,
) where {T}
    vis = func.variables

    t = vis[1]
    u = vis[2]
    x = vis[3:end]
    a_terms = MOI.ScalarAffineTerm{T}[]
    q_terms = MOI.ScalarQuadraticTerm{T}[]
    push!(q_terms, MOI.ScalarQuadraticTerm(-T(2), t, u))
    for var in x
        push!(q_terms, MOI.ScalarQuadraticTerm(T(2), var, var))
    end

    fq = MOI.ScalarQuadraticFunction(a_terms, q_terms, zero(T))
    quad = MOI.add_constraint(model, fq, MOI.LessThan(zero(T)))
    # ScalarAffineFunction's are added instead of SingleVariable's
    # because models can only have one SingleVariable per variable.
    # Hence, adding a SingleVariable constraint here could conflict with
    # a user defined SingleVariable
    fp1 = convert(MOI.ScalarAffineFunction{T}, MOI.SingleVariable(t))
    var_pos1 = MOI.add_constraint(model, fp1, MOI.GreaterThan(zero(T)))
    fp2 = convert(MOI.ScalarAffineFunction{T}, MOI.SingleVariable(u))
    var_pos2 = MOI.add_constraint(model, fp2, MOI.GreaterThan(zero(T)))

    return RSOCtoNonConvexQuadBridge(quad, [var_pos1, var_pos2], vis)
end

function MOI.supports_constraint(
    ::Type{SOCtoNonConvexQuadBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.SecondOrderCone},
) where {T}
    return true
end

function MOI.supports_constraint(
    ::Type{RSOCtoNonConvexQuadBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.RotatedSecondOrderCone},
) where {T}
    return true
end

function MOIB.added_constrained_variable_types(
    ::Type{<:AbstractSOCtoNonConvexQuadBridge},
)
    return Tuple{DataType}[]
end

function MOIB.added_constraint_types(
    ::Type{<:AbstractSOCtoNonConvexQuadBridge{T}},
) where {T}
    return [
        (MOI.ScalarQuadraticFunction{T}, MOI.LessThan{T}),
        (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}),
    ]
end

function concrete_bridge_type(
    ::Type{SOCtoNonConvexQuadBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.SecondOrderCone},
) where {T}
    return SOCtoNonConvexQuadBridge{T}
end

function concrete_bridge_type(
    ::Type{RSOCtoNonConvexQuadBridge{T}},
    ::Type{MOI.VectorOfVariables},
    ::Type{MOI.RotatedSecondOrderCone},
) where {T}
    return RSOCtoNonConvexQuadBridge{T}
end

# Attributes, Bridge acting as a model
function MOI.get(
    ::AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{T},MOI.LessThan{T}},
) where {T}
    return 1
end

function MOI.get(
    bridge::AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarQuadraticFunction{T},
        MOI.LessThan{T},
    },
) where {T}
    return [bridge.quad]
end

function MOI.get(
    bridge::AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T},MOI.GreaterThan{T}},
) where {T}
    return length(bridge.var_pos)
end

function MOI.get(
    bridge::AbstractSOCtoNonConvexQuadBridge{T},
    ::MOI.ListOfConstraintIndices{
        MOI.ScalarAffineFunction{T},
        MOI.GreaterThan{T},
    },
) where {T}
    return bridge.var_pos
end

# References
function MOI.delete(
    model::MOI.ModelLike,
    bridge::AbstractSOCtoNonConvexQuadBridge,
)
    MOI.delete(model, bridge.quad)
    MOI.delete.(model, bridge.var_pos)
    return
end

# Attributes, Bridge acting as a constraint
function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintPrimal,
    bridge::AbstractSOCtoNonConvexQuadBridge,
)
    vals = MOI.get.(model, MOI.VariablePrimal(attr.N), bridge.vars)
    return vals
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    b::SOCtoNonConvexQuadBridge{T},
) where {T}
    return MOI.SecondOrderCone(length(b.vars))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintSet,
    b::RSOCtoNonConvexQuadBridge{T},
) where {T}
    return MOI.RotatedSecondOrderCone(length(b.vars))
end

function MOI.get(
    model::MOI.ModelLike,
    attr::MOI.ConstraintFunction,
    b::AbstractSOCtoNonConvexQuadBridge{T},
) where {T}
    return MOI.VectorOfVariables(b.vars)
end
