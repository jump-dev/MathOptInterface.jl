"""
    SingleBridgeOptimizer{BT<:AbstractBridge, MT<:MOI.ModelLike, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer

The `SingleBridgeOptimizer` bridges any constraint supported by the bridge `BT`.
This is in contrast with the [`LazyBridgeOptimizer`](@ref) which only bridges the constraints that are unsupported by the internal model, even if they are supported by one of its bridges.
"""
struct SingleBridgeOptimizer{BT<:AbstractBridge, MT<:MOI.ModelLike, OT<:MOI.ModelLike} <: AbstractBridgeOptimizer
    model::OT
    bridged::MT
    bridges::Dict{CI, BT}
end
function SingleBridgeOptimizer{BT, MT}(model::OT) where {BT, MT, OT <: MOI.ModelLike}
    SingleBridgeOptimizer{BT, MT, OT}(model, MT(), Dict{CI, BT}())
end

isbridged(b::SingleBridgeOptimizer, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false
bridge_type(b::SingleBridgeOptimizer{BT}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) where BT = BT

# :((Zeros, SecondOrderCone)) -> (:(MOI.Zeros), :(MOI.SecondOrderCone))
_tuple_prefix_moi(t) = MOIU._moi.(t.args)

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
    code = bridge_impl(modelname, bridge, ss, sst, vs, vst, sf, sft, vf, vft)
    esc(code)
end

macro external_bridge(modelname, bridge, ss, sst, vs, vst, sf, sft, vf, vft)
    code = bridge_impl(modelname, bridge, ss, sst, vs, vst, sf, sft, vf, vft,
                      _prefix_func = (x->x.args), use_external_model = true)
    esc(code)
end

function bridge_impl(modelname, bridge, ss, sst, vs, vst, sf, sft, vf, vft;
        _prefix_func = _tuple_prefix_moi, use_external_model = false)            
    bridgedmodelname = Symbol(string(modelname) * "Instance")
    bridgedfuns = :(Union{$(_prefix_func(sf)...), $(_prefix_func(sft)...), $(_prefix_func(vf)...), $(_prefix_func(vft)...)})
    bridgedsets = :(Union{$(_prefix_func(ss)...), $(_prefix_func(sst)...), $(_prefix_func(vs)...), $(_prefix_func(vst)...)})

    if use_external_model
        code = quote
            $MOIU.@external_model $bridgedmodelname $ss $sst $vs $vst $sf $sft $vf $vft
        end
    else
        code = quote
            $MOIU.@model $bridgedmodelname $ss $sst $vs $vst $sf $sft $vf $vft
        end
    end    
    code = quote
        $code
        const $modelname{T, OT<:MOI.ModelLike} = $MOIB.SingleBridgeOptimizer{$bridge{T}, $bridgedmodelname{T}, OT}
        MathOptInterface.Bridges.isbridged(::$modelname, ::Type{<:$bridgedfuns}, ::Type{<:$bridgedsets}) = true
        supportsbridgingconstraint(::$modelname, ::Type{<:$bridgedfuns}, ::Type{<:$bridgedsets}) = true
    end
    return code    
end
