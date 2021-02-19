function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(unbridged_function),LazyBridgeOptimizer,MathOptInterface.AbstractScalarFunction})   # time: 0.6265791
    Base.precompile(Tuple{typeof(bridge_index),Graph,ObjectiveNode})   # time: 0.03183784
    Base.precompile(Tuple{typeof(_functionize_bridge),Vector{Any},Type{MathOptInterface.Bridges.Constraint.VectorFunctionizeBridge}})   # time: 0.007451239
    Base.precompile(Tuple{typeof(_functionize_bridge),Vector{Any},Type{MathOptInterface.Bridges.Constraint.ScalarFunctionizeBridge}})   # time: 0.005963241
    Base.precompile(Tuple{typeof(_functionize_bridge),Vector{Any},Type{MathOptInterface.Bridges.Objective.FunctionizeBridge}})   # time: 0.005938126
    Base.precompile(Tuple{typeof(add_variable_node),Graph})   # time: 0.004315477
    Base.precompile(Tuple{typeof(add_objective_node),Graph})   # time: 0.002944378
    isdefined(MathOptInterface.Bridges, Symbol("#38#39")) && Base.precompile(Tuple{getfield(MathOptInterface.Bridges, Symbol("#38#39")),Nothing})   # time: 0.001879857
    Base.precompile(Tuple{typeof(add_edge),Graph,ObjectiveNode,ObjectiveEdge})   # time: 0.001630966
    Base.precompile(Tuple{typeof(add_edge),Graph,ConstraintNode,Edge})   # time: 0.001597607
    Base.precompile(Tuple{typeof(_first_functionize_bridge),Vector{Any},Type{MathOptInterface.Bridges.Constraint.ScalarFunctionizeBridge}})   # time: 0.001491107
    Base.precompile(Tuple{typeof(_first_functionize_bridge),Vector{Any},Type{MathOptInterface.Bridges.Constraint.VectorFunctionizeBridge}})   # time: 0.001170406
    Base.precompile(Tuple{typeof(bridged_function),LazyBridgeOptimizer,Any})   # time: 0.001030499
end
