struct LazyBridgeOptimizer{OT<:MOI.ModelLike, MT<:MOI.ModelLike} <: AbstractBridgeOptimizer
    model::OT   # Internal model
    bridged::MT # Model containing bridged constraints
    bridges::Dict{CI, AbstractBridge} # Constraint Index of bridged constraint in bridged -> Bridge
    bridgetypes::Vector{DataType} # List of types of available bridges
    dist::Dict{Tuple{DataType, DataType}, Int}      # (F, S) -> Number of bridges that need to be used for an `F`-in-`S` constraint
    best::Dict{Tuple{DataType, DataType}, DataType} # (F, S) -> Bridge to be used for an `F`-in-`S` constraint
end
function LazyBridgeOptimizer(model::MOI.ModelLike, bridged::MOI.ModelLike)
    LazyBridgeOptimizer{typeof(model),
                             typeof(bridged)}(model, bridged,
                                              Dict{CI, AbstractBridge}(),
                                              DataType[],
                                              Dict{Tuple{DataType, DataType}, Int}(),
                                              Dict{Tuple{DataType, DataType}, DataType}())
end

function _dist(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if MOI.supportsconstraint(b.model, F, S)
        0
    else
        get(b.dist, (F, S), typemax(Int))
    end
end

"""
    addbridge!(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Enable the use of the bridges of type `BT` by `b`.
"""
function addbridge!(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
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

function MOI.get(b::LazyBridgeOptimizer, attr::InstanceConstraintAttribute, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(b.model, F, S)
        MOI.get(b.model, attr, ci)
    else
        MOI.get(b.bridged, attr, ci)
    end
end
function MOI.get(b::LazyBridgeOptimizer, attr::SolverConstraintAttribute, ci::CI{F, S}) where {F, S}
    if MOI.supportsconstraint(b.model, F, S)
        MOI.get(b.model, attr, ci)
    else
        MOI.get(b.model, attr, bridge(b, ci))
    end
end

# It only bridges when the constraint is not supporting, hence the name "Lazy"
isbridged(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = MOI.supportsconstraint(b.model, F, S)
bridgetype(b::LazyBridgeOptimizer{BT}, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) where BT = b.best[(typeof(f), typeof(s))]

# Constraints
function MOI.supportsconstraint(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
     || (F, S) in keys(b.dist)
end
function MOI.canaddconstraint(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    MOI.canaddconstraint(b.model, F, S) || ((F, S) in b.best && all(C -> MOI.canaddconstraint(b, C...), addedconstraints(b.best[(F, S)], F, S)))
end
function MOI.addconstraint!(b::LazyBridgeOptimizer{T}, f::MOI.AbstractFunction, s::MOI.AbstractSet) where T
    if MOI.supportsconstraint(b.model, typeof(f), typeof(s))
        MOI.addconstraint!(b.model, f, s)
    else
        ci = MOI.addconstraint!(b.bridged, f, s)
        @assert !haskey(b.bridges, ci)
        b.bridges[ci] = (b.model, f, s)
        ci
    end
end

for f in (:canmodifyconstraint, :modifyconstraint!)
    @eval begin
        function MOI.$f(b::LazyBridgeOptimizer, ci::CI{F, S}, change) where {F, S}
            if MOI.supportsconstraint(b.model, F, S)
                MOI.$f(b.model, ci, change)
            else
                $f(b, ci, change)
            end
        end
    end
end
