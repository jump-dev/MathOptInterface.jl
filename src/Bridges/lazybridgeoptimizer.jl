"""
    LazyBridgeOptimizer{OT<:MOI.ModelLike, MT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `LazyBridgeOptimizer` combines several bridges, which are added using the [`addbridge!`](@ref) function.
Whenever a constraint is added, it only attempts to bridge it if it is not supported by the internal model (hence its name `Lazy`).
When bridging a constraint, it selects the minimal number of bridges needed.
For instance, a constraint `F`-in-`S` can be bridged into a constraint `F1`-in-`S1` (supported by the internal model) using bridge 1 or
bridged into a constraint `F2`-in-`S2` (unsupported by the internal model) using bridge 2 which can then be
bridged into a constraint `F3`-in-`S3` (supported by the internal model) using bridge 3,
it will choose bridge 1 as it allows to bridge `F`-in-`S` using only one bridge instead of two if it uses bridge 2 and 3.
"""
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
            for (F, S) in supportedconstrainttypes(BT)
                if all(C -> MOI.supportsconstraint(b, C...), addedconstrainttypes(BT, F, S))
                    # Number of bridges needed using BT
                    dist = sum(C -> _dist(b, C...), addedconstrainttypes(BT, F, S))
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

# It only bridges when the constraint is not supporting, hence the name "Lazy"
function isbridged(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    !MOI.supportsconstraint(b.model, F, S)
end
supportsbridgingconstraint(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = (F, S) in keys(b.best)
bridgetype(b::LazyBridgeOptimizer{BT}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) where BT = b.best[(F, S)]
