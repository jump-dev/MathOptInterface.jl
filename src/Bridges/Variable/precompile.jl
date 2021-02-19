function _precompile_()
    ccall(:jl_generating_output, Cint, ()) == 1 || return nothing
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64}, MathOptInterface.GreaterThan{Float64}}})   # time: 0.04609538
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, _A} where _A})   # time: 0.021449305
    Base.precompile(Tuple{typeof(empty!),Map})   # time: 0.012949045
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Nonnegatives}})   # time: 0.00983498
    Base.precompile(Tuple{typeof(delete!),Map,MathOptInterface.VariableIndex})   # time: 0.009456781
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64}, _A} where _A})   # time: 0.009078153
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable, MathOptInterface.EqualTo{Float64}}})   # time: 0.008405447
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.SecondOrderCone}})   # time: 0.00831034
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.Zeros}})   # time: 0.008290234
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.PositiveSemidefiniteConeTriangle}})   # time: 0.008019037
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable, _A} where _A})   # time: 0.007693966
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable, MathOptInterface.Integer}})   # time: 0.007498911
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable, MathOptInterface.ZeroOne}})   # time: 0.007342877
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{MathOptInterface.ScalarAffineFunction{Float64}}, _A} where _A})   # time: 0.007312434
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable, MathOptInterface.LessThan{Float64}}})   # time: 0.007245695
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.SingleVariable, MathOptInterface.GreaterThan{Float64}}})   # time: 0.007072676
    Base.precompile(Tuple{typeof(MathOptInterface.Bridges.added_constrained_variable_types),Type{VectorizeBridge{Float64, MathOptInterface.Nonnegatives}}})   # time: 0.005922131
    Base.precompile(Tuple{typeof(MathOptInterface.Bridges.added_constrained_variable_types),Type{NonposToNonnegBridge{Float64}}})   # time: 0.005914195
    Base.precompile(Tuple{typeof(MathOptInterface.Bridges.added_constrained_variable_types),Type{VectorizeBridge{Float64, MathOptInterface.Nonpositives}}})   # time: 0.00547066
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, MathOptInterface.PositiveSemidefiniteConeSquare}})   # time: 0.005176492
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64}, MathOptInterface.EqualTo{Float64}}})   # time: 0.004830495
    Base.precompile(Tuple{typeof(register_context),Map,MathOptInterface.ConstraintIndex{MathOptInterface.ScalarAffineFunction{Float64}, MathOptInterface.LessThan{Float64}}})   # time: 0.00478906
    Base.precompile(Tuple{typeof(unbridged_function),Map,MathOptInterface.VariableIndex})   # time: 0.002940502
    Base.precompile(Tuple{Type{VectorizeBridge{Float64, _A}} where _A,Any,Any,Float64})   # time: 0.002500584
    Base.precompile(Tuple{typeof(function_for),Map,MathOptInterface.ConstraintIndex{MathOptInterface.VectorOfVariables, S} where S})   # time: 0.001794881
    # TODO: Base.precompile(Tuple{typeof(unbridged_map),MathOptInterface.Bridges.Variable.VectorizeBridge{T, S} where S,MathOptInterface.VariableIndex})   # time: 0.001506648
end
