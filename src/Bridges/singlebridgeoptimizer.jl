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

is_bridged(b::SingleBridgeOptimizer, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false
bridge_type(b::SingleBridgeOptimizer{BT}, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) where BT = BT

# :((Zeros, SecondOrderCone)) -> (:(MOI.Zeros), :(MOI.SecondOrderCone))

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
    bridged_model_name = Symbol(string(modelname) * "Instance")
    bridged_funs = :(Union{$((sf.args)...), $((sft.args)...), $((vf.args)...), $((vft.args)...)})
    bridged_sets = :(Union{$((ss.args)...), $((sst.args)...), $((vs.args)...), $((vst.args)...)})

    esc(quote
        $MOIU.@model $bridged_model_name $ss $sst $vs $vst $sf $sft $vf $vft
        const $modelname{T, OT<:MOI.ModelLike} = $MOIB.SingleBridgeOptimizer{$bridge{T}, $bridged_model_name{T}, OT}
        $MOIB.is_bridged(::$modelname, ::Type{<:$bridged_funs}, ::Type{<:$bridged_sets}) = true
        supports_bridging_constraint(::$modelname, ::Type{<:$bridged_funs}, ::Type{<:$bridged_sets}) = true
    end)
end
