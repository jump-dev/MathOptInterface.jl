struct StrictlyGreaterThan <: MOI.AbstractScalarSet
    lower::Float64
end

MOIU.@model(ExternalModel,
            (),
            (MOI.GreaterThan, MOI.LessThan, MOI.Interval),
            (),
            (),
            (),
            (MOI.ScalarAffineFunction,),
            (),
            ())

const CI = MOI.ConstraintIndex
const GT = MOI.GreaterThan{Float64}
const LT = MOI.LessThan{Float64}
const SAF = MOI.ScalarAffineFunction{Float64}
const SGT = StrictlyGreaterThan

struct GreaterPlusOneBridge{T} <: MOIB.AbstractBridge where T
    lower::CI{SAF, GT}
    dummy::T
end
function GreaterPlusOneBridge{T}(model, f::SAF, s::SGT) where T
    lower = MOI.add_constraint(model, f, GT(s.lower + 1))
    return GreaterPlusOneBridge(lower, 0.0)
end

function MOI.supports_constraint(::Type{GreaterPlusOneBridge},
                                ::Type{SAF}, ::Type{SGT})

    return true
end
function MOIB.added_constraint_types(::Type{GreaterPlusOneBridge},
                                   ::Type{SAF}, ::Type{SGT})

    return [(SAF, GT)]
end

MOIB.@bridge GreaterPlusOne GreaterPlusOneBridge (SGT,) () () () (SAF,) () () ()

@testset "external set model and bridge" begin
    ext_model = ExternalModel{Float64}()

    @test MOI.supports_constraint(ext_model, MOI.ScalarAffineFunction{Float64},
                                  MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(ext_model, MOI.ScalarAffineFunction{Float64},
                                  StrictlyGreaterThan)
    @test MOI.supports_constraint(ext_model, MOI.ScalarAffineFunction{Float64},
                                  MOI.LessThan{Float64})

    ext_bridge = GreaterPlusOne{Float64}(ext_model)

    @test MOI.supports_constraint(ext_bridge, SAF, GT)
    @test MOI.supports_constraint(ext_bridge, SAF, SGT)
    @test MOI.supports_constraint(ext_bridge, SAF, LT)
end
