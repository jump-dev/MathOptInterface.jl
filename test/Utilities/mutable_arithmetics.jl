using Test

import MutableArithmetics
const MA = MutableArithmetics

using MathOptInterface
const MOI = MathOptInterface

@testset "promote_operation with $T" for T in [Float64, Float32]
    @test MA.promote_operation(*, MOI.SingleVariable, T) == MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, MOI.ScalarAffineFunction{T}, T) == MOI.ScalarAffineFunction{T}
    @test MA.promote_operation(*, MOI.ScalarQuadraticFunction{T}, T) == MOI.ScalarQuadraticFunction{T}
end
