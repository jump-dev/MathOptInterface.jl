struct SingleBridgeOptimizer{BT<:AbstractBridge, MT<:MOI.ModelLike, T, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer
    model::OT
    bridged::MT
    bridges::Dict{CI, BT}
end
function SingleBridgeOptimizer{BT, MT, T}(model::OT) where {BT, MT, T, OT <: MOI.ModelLike}
    SingleBridgeOptimizer{BT, MT, T, OT}(model, MT(), Dict{CI, BT}())
end

isbridged(b::SingleBridgeOptimizer, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

bridgetype(b::SingleBridgeOptimizer{BT}, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) where BT = BT

_mois(t) = MOIU._moi.(t.args)

"""
macro bridge(modelname, bridge, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, typedscalarfunctions, vectorfunctions, typedvectorfunctions)

Creates a type `modelname` implementing the MOI model interface and bridging the `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions, `typedscalarfunctions` typed scalar functions, `vectorfunctions` vector functions and `typedvectorfunctions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

### Examples

The optimizer layer bridging the constraints `ScalarAffineFunction`-in-`Interval` is created as follows:
```julia
@bridge SplitInterval MOIB.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()
```
Given an optimizer `optimizer` implementing `ScalarAffineFunction`-in-`GreaterThan` and `ScalarAffineFunction`-in-`LessThan`, the optimizer
```
bridgedmodel = SplitInterval(model)
```
will additionally support `ScalarAffineFunction`-in-`Interval`.
"""
macro bridge(modelname, bridge, ss, sst, vs, vst, sf, sft, vf, vft)
    bridgedmodelname = Symbol(string(modelname) * "Instance")
    bridgedfuns = :(Union{$(_mois(sf)...), $(_mois(sft)...), $(_mois(vf)...), $(_mois(vft)...)})
    bridgedsets = :(Union{$(_mois(ss)...), $(_mois(sst)...), $(_mois(vs)...), $(_mois(vst)...)})

    esc(quote
        $MOIU.@model $bridgedmodelname $ss $sst $vs $vst $sf $sft $vf $vft
        const $modelname{T, OT<:MOI.ModelLike} = $MOIB.SingleBridgeOptimizer{$bridge{T}, $bridgedmodelname{T}, T, OT}
        isbridged(b::$modelname, ::Type{<:$bridgedfuns}, ::Type{<:$bridgedsets}) = true
    end)
end
