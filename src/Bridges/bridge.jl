export @bridge

"""
    AbstractBridge

A bridge represents a bridged constraint in an `AbstractBridgeOptimizer`. It contains the indices of the constraints that it has been created in the optimizer.
These can be obtained using `MOI.NumberOfConstraints` and `MOI.ListOfConstraintIndices` using the bridge in place of a `ModelLike` in the `MOI.get` function.
Attributes of the bridged optimizer such as `MOI.ConstraintDual` and `MOI.ConstraintPrimal`, can be obtained using the bridge in place of the constraint index in `MOI.get`.
These calls are used by the `AbstractBridgeOptimizer` to communicate with the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables)

The number of variables created by the bridge `b` in the optimizer.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

The number of constraints of the type `F`-in-`S` created by the bridge `b` in the optimizer.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

A `Vector{ConstraintIndex{F,S}}` with indices of all constraints of
type `F`-in`S` created by the bride `b` in the optimizer (i.e., of length equal to the value of `NumberOfConstraints{F,S}()`).
"""
MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = CI{F, S}[]

"""
    MOI.candelete(optimizer::MOI.AbstractOptimizer, b::AbstractBridge)

Return a `Bool` indicating whether the bridge `b` can be removed from the optimizer `optimizer`.
"""
MOI.candelete(optimizer::MOI.AbstractOptimizer, c::AbstractBridge) = true

const InstanceConstraintAttribute = Union{MOI.ConstraintName, MOI.ConstraintFunction, MOI.ConstraintSet}
const SolverConstraintAttribute = Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart, MOI.ConstraintPrimal, MOI.ConstraintDual, MOI.ConstraintBasisStatus}

"""
    AbstractBridgeOptimizer

A bridge optimizer applies a given constraint bridge to a given optimizer.
The attributes of the bridge optimizer are automatically computed to make the bridges transparent, e.g. the variables and constraints created by the bridges are hidden.
"""
abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end
bridge(b::AbstractBridgeOptimizer, ci::CI) = b.bridges[ci.value]
MOI.optimize!(b::AbstractBridgeOptimizer) = MOI.optimize!(b.optimizer)

MOI.isempty(b::AbstractBridgeOptimizer) = isempty(b.bridges) && MOI.isempty(b.optimizer)
function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.optimizer)
    MOI.empty!(b.bridged)
    empty!(b.bridges)
end
MOI.supports(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.supports(b.optimizer, attr)
MOI.copy!(b::AbstractBridgeOptimizer, src::MOI.ModelLike; copynames=false) = MOIU.defaultcopy!(b, src, copynames)

# References
MOI.candelete(b::AbstractBridgeOptimizer, idx::MOI.Index) = MOI.candelete(b.optimizer, idx)
MOI.isvalid(b::AbstractBridgeOptimizer, idx::MOI.Index) = MOI.isvalid(b.optimizer, idx)
MOI.delete!(b::AbstractBridgeOptimizer, idx::MOI.Index) = MOI.delete!(b.optimizer, idx)

# Attributes
function MOI.get(b::AbstractBridgeOptimizer, loc::MOI.ListOfConstraintIndices)
    locr = MOI.get(b.optimizer, loc)
    for bridge in values(b.bridges)
        for c in MOI.get(bridge, loc)
            i = findfirst(locr, c)
            if (VERSION >= v"0.7.0-DEV.3395" && i !== nothing) || (VERSION < v"0.7.0-DEV.3395" && !iszero(i))
                MOI.deleteat!(locr, i)
            end
        end
    end
    locr
end
function MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.NumberOfConstraints, MOI.NumberOfVariables})
    s = MOI.get(b.optimizer, attr)
    for v in values(b.bridges)
        s -= MOI.get(v, attr)
    end
    s
end
MOI.canget(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints) = MOI.canget(b.optimizer, attr) && MOI.canget(b.bridged, attr)
_noc(b, fs) = MOI.get(b, MOI.NumberOfConstraints{fs...}())
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints)
    loc = MOI.get(b.optimizer, attr)
    rm = find(_noc.(b, loc) .== 0)
    deleteat!(loc, rm)
    append!(loc, MOI.get(b.bridged, attr))
end
for f in (:canget, :canset, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.$f(b.optimizer, attr)
    end
end
# Objective function and model name
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}, value) = MOI.set!(b.optimizer, attr, value)
for f in (:canget, :canset)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::Type{<:MOI.Index}) = MOI.$f(b.optimizer, attr, index)
    end
end
MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::MOI.Index) = MOI.get(b.optimizer, attr, index)
MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index}) = MOI.get(b.optimizer, attr, indices)
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::MOI.Index, value) = MOI.set!(b.optimizer, attr, index, value)
MOI.set!(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index}, values::Vector) = MOI.set!(b.optimizer, attr, indices, values)

# Name
MOI.canget(b::AbstractBridgeOptimizer, IdxT::Type{<:MOI.Index}, name::String) = MOI.canget(b.optimizer, IdxT, name)
MOI.get(b::AbstractBridgeOptimizer, IdxT::Type{<:MOI.Index}, name::String) = MOI.get(b.optimizer, IdxT, name)

# Constraints
MOI.supportsconstraint(b::AbstractBridgeOptimizer, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = MOI.supportsconstraint(b.optimizer, F, S)
MOI.canaddconstraint(b::AbstractBridgeOptimizer, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = MOI.canaddconstraint(b.optimizer, F, S)
function MOI.addconstraint!(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet)
    MOI.addconstraint!(b.optimizer, f, s)
end
MOI.canmodifyconstraint(b::AbstractBridgeOptimizer, ci::CI, change) = MOI.canmodifyconstraint(b.optimizer, ci, change)
MOI.modifyconstraint!(b::AbstractBridgeOptimizer, ci::CI, change) = MOI.modifyconstraint!(b.optimizer, ci, change)

# Objective
MOI.canmodifyobjective(b::AbstractBridgeOptimizer, ::Type{M}) where M<:MOI.AbstractFunctionModification = MOI.canmodifyobjective(b.optimizer, M)
MOI.modifyobjective!(b::AbstractBridgeOptimizer, change::MOI.AbstractFunctionModification) = MOI.modifyobjective!(b.optimizer, change)

# Variables
MOI.canaddvariable(b::AbstractBridgeOptimizer) = MOI.canaddvariable(b.optimizer)
MOI.addvariable!(b::AbstractBridgeOptimizer) = MOI.addvariable!(b.optimizer)
MOI.addvariables!(b::AbstractBridgeOptimizer, n) = MOI.addvariables!(b.optimizer, n)

function _mois(t)
    MOIU._moi.(t.args)
end

"""
macro bridge(optimizername, bridge, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, typedscalarfunctions, vectorfunctions, typedvectorfunctions)

Creates a type `optimizername` implementing the MOI optimizer interface and bridging the `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions, `typedscalarfunctions` typed scalar functions, `vectorfunctions` vector functions and `typedvectorfunctions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

### Examples

The optimizer layer bridging the constraints `ScalarAffineFunction`-in-`Interval` is created as follows:
```julia
@bridge SplitInterval MOIB.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()
```
Given an optimizer `optimizer` implementing `ScalarAffineFunction`-in-`GreaterThan` and `ScalarAffineFunction`-in-`LessThan`, the optimizer
```
bridgedoptimizer = SplitInterval(optimizer)
```
will additionally support `ScalarAffineFunction`-in-`Interval`.
"""
macro bridge(optimizername, bridge, ss, sst, vs, vst, sf, sft, vf, vft)
    bridgedoptimizername = Symbol(string(optimizername) * "Instance")
    bridgedfuns = :(Union{$(_mois(sf)...), $(_mois(sft)...), $(_mois(vf)...), $(_mois(vft)...)})
    bridgedsets = :(Union{$(_mois(ss)...), $(_mois(sst)...), $(_mois(vs)...), $(_mois(vst)...)})

    # Attributes
    attributescode = :()

    for f in (:canget, :get)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$optimizername, attr::Union{$MOI.ListOfConstraintIndices{<:$bridgedfuns, <:$bridgedsets}, $MOI.NumberOfConstraints{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.bridged, attr)
            end
        end
    end

    for f in (:canget, :canset)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$optimizername, attr::$MOIB.InstanceConstraintAttribute, ci::Type{$CI{F, S}}) where {F<:$bridgedfuns, S<:$bridgedsets}
                $MOI.$f(b.bridged, attr, ci)
            end
            function $MOI.$f(b::$optimizername{T}, attr::$MOIB.SolverConstraintAttribute, ci::Type{$CI{F, S}}) where {T, F<:$bridgedfuns, S<:$bridgedsets}
                $MOI.$f(b.optimizer, attr, $bridge{T})
            end
        end
    end

    for f in (:set!, :get, :get!)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$optimizername, attr::$MOIB.InstanceConstraintAttribute, ci::$CI{<:$bridgedfuns, <:$bridgedsets})
                $MOI.$f(b.bridged, attr, ci)
            end
            function $MOI.$f(b::$optimizername, attr::$MOIB.SolverConstraintAttribute, ci::$CI{<:$bridgedfuns, <:$bridgedsets})
                $MOI.$f(b.optimizer, attr, $MOIB.bridge(b, ci))
            end
        end
    end

    esc(quote
        $MOIU.@model $bridgedoptimizername $ss $sst $vs $vst $sf $sft $vf $vft

        struct $optimizername{T, IT<:$MOI.AbstractOptimizer} <: $MOIB.AbstractBridgeOptimizer
            optimizer::IT
            bridged::$bridgedoptimizername{T}
            bridges::Dict{Int64, $bridge{T}}
            function $optimizername{T}(optimizer::IT) where {T, IT <: $MOI.AbstractOptimizer}
                new{T, IT}(optimizer, $bridgedoptimizername{T}(), Dict{Int64, $bridge{T}}())
            end
        end

        # References
        $MOI.candelete(b::$optimizername{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T = $MOI.candelete(b.bridged, ci) && $MOI.candelete(b.optimizer, $MOIB.bridge(b, ci))

        $MOI.isvalid(b::$optimizername{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T = $MOI.isvalid(b.bridged, ci)

        function $MOI.delete!(b::$optimizername{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T
            $MOI.delete!(b.optimizer, $MOIB.bridge(b, ci))
            delete!(b.bridges, ci.value)
            $MOI.delete!(b.bridged, ci)
        end

        $attributescode

        # Constraints
        $MOI.supportsconstraint(b::$optimizername, ::Type{F}, ::Type{S}) where {F<:$bridgedfuns, S<:$bridgedsets} = $MOI.supportsconstraint(b.bridged, F, S)
        $MOI.canaddconstraint(b::$optimizername, ::Type{F}, ::Type{S}) where {F<:$bridgedfuns, S<:$bridgedsets} = $MOI.canaddconstraint(b.bridged, F, S)
        function $MOI.addconstraint!(b::$optimizername{T}, f::$bridgedfuns, s::$bridgedsets) where T
            ci = $MOI.addconstraint!(b.bridged, f, s)
            @assert !haskey(b.bridges, ci.value)
            b.bridges[ci.value] = $bridge{T}(b.optimizer, f, s)
            ci
        end
        function $MOI.canmodifyconstraint(b::$optimizername, ci::$CI{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.canmodifyconstraint(b.bridged, ci, change) && $MOI.canmodifyconstraint(b.optimizer, $MOIB.bridge(b, ci), change)
        end
        function $MOI.modifyconstraint!(b::$optimizername, ci::$CI{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.modifyconstraint!(b.optimizer, $MOIB.bridge(b, ci), change)
            $MOI.modifyconstraint!(b.bridged, ci, change)
        end
    end)
end
