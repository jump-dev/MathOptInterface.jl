struct AutomaticBridgeOptimizer{OT<:MOI.ModelLike, MT<:MOI.ModelLike} <: AbstractBridgeOptimizer
    model::OT   # Internal model
    bridged::MT # Model containing bridged constraints
    bridges::Dict{CI, AbstractBridge} # Constraint Index of bridged constraint in bridged -> Bridge
    bridgetypes::Vector{DataType} # List of types of available bridges
    dist::Dict{Tuple{DataType, DataType}, Int}      # (F, S) -> Number of bridges that need to be used for an `F`-in-`S` constraint
    best::Dict{Tuple{DataType, DataType}, DataType} # (F, S) -> Bridge to be used for an `F`-in-`S` constraint
end
function AutomaticBridgeOptimizer(model::MOI.ModelLike, bridged::MOI.ModelLike)
    AutomaticBridgeOptimizer{typeof(model),
                             typeof(bridged)}(model, bridged,
                                              Dict{CI, AbstractBridge}(),
                                              DataType[],
                                              Dict{Tuple{DataType, DataType}, Int}(),
                                              Dict{Tuple{DataType, DataType}, DataType}())
end

function _dist(b::AutomaticBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if MOI.supportsconstraint(b.model, F, S)
        0
    else
        get(b.dist, (F, S), typemax(Int))
    end
end

"""
    addbridge!(b::AutomaticBridgeOptimizer, BT::Type{<:AbstractBridge})

Enable the use of the bridges of type `BT` by `b`.
"""
function addbridge!(b::AutomaticBridgeOptimizer, BT::Type{<:AbstractBridge})
    push!(b.bridgetypes, BT)
    # Bellman-Ford algorithm
    changed = true # Has b.dist changed in the last iteration ?
    while changed
        changed = false
        i = 0
        for BT in b.bridgetypes
            i += 1
            @show i
            for (F, S) in supportedconstraints(BT)
                if all(MOI.supportsconstraint, addedconstraints(BT, F, S))
                    # Number of bridges needed using BT
                    dist = sum(C -> _dist(b, C...), addedconstraints(BT, F, S))
                    # Is it better that what can currently be done ?
                    if dist < _dist(b, F, S)
                        b.dist[(F, S)] = dist
                        b.best[(F, S)] = BT
                        changed = true
                    end
                end
            end
        end
    end
end

# References
for f in (:candelete, :isvalid, :delete!)
    @eval begin
        function MOI.$f(b::AutomaticBridgeOptimizer, ci::CI{F, S}) where {F, S}
            if MOI.supportsconstraint(b.model, F, S)
                MOI.$f(b.model, ci)
            else
                $f(b, ci)
            end
        end
    end
end
for f in (:canget, :get)
    @eval begin
        function MOI.$f(b::AutomaticBridgeOptimizer, attr::Union{MOI.ListOfConstraintIndices{F, S}, MOI.NumberOfConstraints{F, S}}) where {F, S}
            if MOI.supportsconstraint(b.model, F, S)
                MOI.$f(b.model, attr)
            else
                MOI.$f(b.bridged, attr)
            end
        end
    end
end

for f in (:canget, :canset)
    @eval begin
        function MOI.$f(b::AutomaticBridgeOptimizer, attr::InstanceConstraintAttribute, ci::Type{CI{F, S}}) where {F, S}
            if MOI.supportsconstraint(b.model, F, S)
                MOI.$f(b.model, attr, ci)
            else
                MOI.$f(b.bridged, attr, ci)
            end
        end
        function MOI.$f(b::AutomaticBridgeOptimizer, attr::SolverConstraintAttribute, ci::Type{CI{F, S}}) where {F, S}
            if MOI.supportsconstraint(b.model, F, S)
                MOI.$f(b.model, attr, ci)
            else
                MOI.$f(b.model, attr, bridgetype(b, F, S))
            end
        end
    end
end

function MOI.get(b::AutomaticBridgeOptimizer, attr::InstanceConstraintAttribute, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(b.model, F, S)
        MOI.get(b.model, attr, ci)
    else
        MOI.get(b.bridged, attr, ci)
    end
end
function MOI.get(b::AutomaticBridgeOptimizer, attr::SolverConstraintAttribute, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(b.model, F, S)
        MOI.get(b.model, attr, ci)
    else
        MOI.get(b.model, attr, bridge(b, ci))
    end
end


# Constraints
function MOI.supportsconstraint(b::AutomaticBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    MOI.supportsconstraint(b.model, F, S) || (F, S) in keys(b.dist)
end
function MOI.canaddconstraint(b::AutomaticBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    MOI.canaddconstraint(b.model, F, S) || ((F, S) in b.best && all(C -> MOI.canaddconstraint(b, C...), addedconstraints(b.best[(F, S)], F, S)))
end
function MOI.addconstraint!(b::AutomaticBridgeOptimizer{T}, f::MOI.AbstractFunction, s::MOI.AbstractSet) where T
    if MOI.supportsconstraint(b.model, typeof(f), typeof(s))
        MOI.addconstraint!(b.model, f, s)
    else
        ci = MOI.addconstraint!(b.bridged, f, s)
        @assert !haskey(b.bridges, ci)
        b.bridges[ci] = b.best[(typeof(f), typeof(s))](b.model, f, s)
        ci
    end
end

for f in (:canmodifyconstraint, :modifyconstraint!)
    @eval begin
        function MOI.$f(b::AutomaticBridgeOptimizer, ci::CI{F, S}, change) where {F, S}
            if MOI.supportsconstraint(b.model, F, S)
                MOI.$f(b.model, ci, change)
            else
                $f(b, ci, change)
            end
        end
    end
end
