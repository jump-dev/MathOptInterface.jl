function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    # TODO: This is a weird one! Base.precompile(Tuple{typeof(bridge_objective),Type,MathOptInterface.Bridges.LazyBridgeOptimizer,MathOptInterface.SingleVariable})   # time: 0.30286795
    Base.precompile(Tuple{typeof(add_key_for_bridge),Map,SlackBridge,MathOptInterface.ScalarAffineFunction{_A} where _A})   # time: 0.044725213
    Base.precompile(Tuple{typeof(empty!),Map})   # time: 0.008765762
    Base.precompile(Tuple{typeof(add_key_for_bridge),Map,SlackBridge{_A, _B, MathOptInterface.ScalarAffineFunction{Float64}} where {_A, _B<:MathOptInterface.AbstractScalarFunction},MathOptInterface.ScalarAffineFunction{Float64}})   # time: 0.007551381
    Base.precompile(Tuple{typeof(root_bridge),Map})   # time: 0.005157664
    Base.precompile(Tuple{typeof(add_key_for_bridge),Map,AbstractBridge,MathOptInterface.AbstractScalarFunction})   # time: 0.00417067
    # TODO: Base.precompile(Tuple{typeof(MathOptInterface.get),MathOptInterface.Bridges.LazyBridgeOptimizer,MathOptInterface.Bridges.ObjectiveFunctionValue,Any})   # time: 0.003866515
    Base.precompile(Tuple{typeof(haskey),Map,MathOptInterface.ObjectiveFunction{MathOptInterface.ScalarAffineFunction{Float64}}})   # time: 0.003800716
    Base.precompile(Tuple{typeof(add_key_for_bridge),Map,FunctionizeBridge{Float64},MathOptInterface.SingleVariable})   # time: 0.002634435
    # TODO: Base.precompile(Tuple{Core.Type{MathOptInterface.Bridges.Objective.SlackBridge{T, F<:MathOptInterface.AbstractScalarFunction, G<:MathOptInterface.AbstractScalarFunction}},MathOptInterface.VariableIndex,Any})   # time: 0.002627885
    # TODO: Base.precompile(Tuple{Core.Type{MathOptInterface.Bridges.Objective.SlackBridge{T, F<:MathOptInterface.AbstractScalarFunction, MathOptInterface.SingleVariable}},MathOptInterface.VariableIndex,Any})   # time: 0.002619257
    Base.precompile(Tuple{typeof(haskey),Map,MathOptInterface.ObjectiveFunction{MathOptInterface.SingleVariable}})   # time: 0.001380728
    Base.precompile(Tuple{typeof(concrete_bridge_type),MathOptInterface.Bridges.AbstractBridgeOptimizer,Type{MathOptInterface.ScalarAffineFunction{Float64}}})   # time: 0.001237356
    # TODO: Base.precompile(Tuple{Core.Type{MathOptInterface.Bridges.Objective.SlackBridge{T, F<:MathOptInterface.AbstractScalarFunction, G<:MathOptInterface.AbstractScalarFunction}},MathOptInterface.VariableIndex,MathOptInterface.ConstraintIndex})   # time: 0.001100836
    # TODO: Base.precompile(Tuple{Core.Type{MathOptInterface.Bridges.Objective.SlackBridge{T, F<:MathOptInterface.AbstractScalarFunction, MathOptInterface.ScalarAffineFunction{Core.Float64}}},MathOptInterface.VariableIndex,MathOptInterface.ConstraintIndex})   # time: 0.001085293
end
