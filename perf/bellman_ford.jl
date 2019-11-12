using BenchmarkTools
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities
const MOIB = MOI.Bridges

# Model similar to SDPA format, it gives a good example because it does not
# support a lot hence need a lot of bridges
MOIU.@model(SDPAModel,
            (), (MOI.EqualTo,), (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle), (),
            (), (MOI.ScalarAffineFunction,), (MOI.VectorOfVariables,), ())
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.GreaterThan{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.LessThan{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.EqualTo{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel{T}, ::Type{MOI.SingleVariable}, ::Type{MOI.Interval{T}}) where {T} = false
MOI.supports_constraint(::SDPAModel, ::Type{MOI.VectorOfVariables}, ::Type{MOI.Reals}) = false
MOI.supports(::SDPAModel{T}, ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}) where {T} = false
MOI.supports(::SDPAModel, ::MOI.ObjectiveFunction{MOI.SingleVariable}) = false

function interval_constraint()
    model = SDPAModel{Float64}()
    bridged = MOIB.full_bridge_optimizer(model, Float64)
    F = MOI.ScalarAffineFunction{Float64}
    S = MOI.Interval{Float64}
    MOIB.update!(bridged, (F, S))
    @benchmark begin
        MOIB._reset_dist($bridged)
        MOIB.update!($bridged, ($F, $S))
    end
end

display(interval_constraint())

function quadratic_objective()
    model = SDPAModel{Float64}()
    bridged = MOIB.full_bridge_optimizer(model, Float64)
    F = MOI.ScalarQuadraticFunction{Float64}
    MOIB.update!(bridged, (F,))
    @benchmark begin
        MOIB._reset_dist($bridged)
        MOIB.update!($bridged, ($F,))
    end
end

display(quadratic_objective())
