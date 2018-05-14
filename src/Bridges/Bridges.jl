module Bridges

using Compat # For lastindex

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

const MOIB = MOI.Bridges # used in macro

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const VI = MOI.VariableIndex
const CI = MOI.ConstraintIndex

# TODO move to MOIU ?
mapcoefficient(coefmap, t::MOI.ScalarAffineTerm) = MOI.ScalarAffineTerm(coefmap(t.coefficient), t.variable_index)
mapcoefficient(coefmap, f::MOI.ScalarAffineFunction) = MOI.ScalarAffineFunction(mapcoefficient.(coefmap, f.terms), coefmap(f.constant))

include("bridge.jl")
include("bridgeoptimizer.jl")
include("singlebridgeoptimizer.jl")
include("lazybridgeoptimizer.jl")

include("intervalbridge.jl")
@bridge SplitInterval SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()
include("rsocbridge.jl")
@bridge RSOC RSOCBridge () () (RotatedSecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("geomeanbridge.jl")
@bridge GeoMean GeoMeanBridge () () (GeometricMeanCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("detbridge.jl")
@bridge LogDet LogDetBridge () () (LogDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)
@bridge RootDet RootDetBridge () () (RootDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)
include("soctopsdbridge.jl")
@bridge SOCtoPSD SOCtoPSDCBridge () () (SecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
@bridge RSOCtoPSD RSOCtoPSDCBridge () () (RotatedSecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)

end # module
