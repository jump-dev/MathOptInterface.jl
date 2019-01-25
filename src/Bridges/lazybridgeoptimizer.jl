# /!\ LazyBridgeOptimizer is still experimental and is meant to be used by JuMP. Use the SingleBridgeOptimizer in the solver tests as explained in the MOI documentation.
"""
    LazyBridgeOptimizer{OT<:MOI.ModelLike, MT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `LazyBridgeOptimizer` combines several bridges, which are added using the [`add_bridge`](@ref) function.
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
    bridgetypes::Vector{Any} # List of types of available bridges
    dist::Dict{Tuple{DataType, DataType}, Int}      # (F, S) -> Number of bridges that need to be used for an `F`-in-`S` constraint
    best::Dict{Tuple{DataType, DataType}, DataType} # (F, S) -> Bridge to be used for an `F`-in-`S` constraint
end
function LazyBridgeOptimizer(model::MOI.ModelLike, bridged::MOI.ModelLike)
    LazyBridgeOptimizer{typeof(model),
                             typeof(bridged)}(model, bridged,
                                              Dict{CI, AbstractBridge}(),
                                              Any[],
                                              Dict{Tuple{DataType, DataType}, Int}(),
                                              Dict{Tuple{DataType, DataType}, DataType}())
end

function _dist(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if MOI.supports_constraint(b.model, F, S)
        return 0
    else
        return get(b.dist, (F, S), typemax(Int))
    end
end

# Update `b.dist` and `b.dest` for constraint types in `constraints`
function update_dist!(b::LazyBridgeOptimizer, constraints)
    # Bellman-Ford algorithm
    changed = true # Has b.dist changed in the last iteration ?
    while changed
        changed = false
        for BT in b.bridgetypes
            for (F, S) in constraints
                if MOI.supports_constraint(BT, F, S) && all(C -> MOI.supports_constraint(b, C[1], C[2]), added_constraint_types(BT, F, S))
                    # Number of bridges needed using BT
                    dist = 1 + sum(C -> _dist(b, C[1], C[2]), added_constraint_types(BT, F, S))
                    # Is it better that what can currently be done ?
                    if dist < _dist(b, F, S)
                        b.dist[(F, S)] = dist
                        b.best[(F, S)] = concrete_bridge_type(BT, F, S)
                        changed = true
                    end
                end
            end
        end
    end
end

function fill_required_constraints!(required::Set{Tuple{DataType, DataType}}, b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    if MOI.supports_constraint(b.model, F, S) || (F, S) in keys(b.best)
        return # The constraint is supported
    end
    if (F, S) in required
        return # The requirements for this constraint have already been added or are being added
    end
    # The constraint is not supported yet, add in `required` the required constraint types to bridge it
    push!(required, (F, S))
    for BT in b.bridgetypes
        if MOI.supports_constraint(BT, F, S)
            for C in added_constraint_types(BT, F, S)
                fill_required_constraints!(required, b, C[1], C[2])
            end
        end
    end
end

# Compute dist[(F, S)] and best[(F, S)]
function update_constraint!(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    required = Set{Tuple{DataType, DataType}}()
    fill_required_constraints!(required, b, F, S)
    update_dist!(b, required)
end

"""
    add_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})

Enable the use of the bridges of type `BT` by `b`.
"""
function add_bridge(b::LazyBridgeOptimizer, BT::Type{<:AbstractBridge})
    push!(b.bridgetypes, BT)
    # Some constraints (F, S) in keys(b.best) may now be bridged
    # with a less briges than `b.dist[(F, S)] using `BT`
    update_dist!(b, keys(b.best))
end

# It only bridges when the constraint is not supporting, hence the name "Lazy"
function is_bridged(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    !MOI.supports_constraint(b.model, F, S)
end
function supports_bridging_constraint(b::LazyBridgeOptimizer, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet})
    update_constraint!(b, F, S)
    return (F, S) in keys(b.best)
end
function bridge_type(b::LazyBridgeOptimizer{BT}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) where BT
    update_constraint!(b, F, S)
    result = get(b.best, (F, S), nothing)
    if result === nothing
        throw(MOI.UnsupportedConstraint{F, S}())
    end
    return result
end
