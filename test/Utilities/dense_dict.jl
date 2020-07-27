using Test
import MathOptInterface
const MOI = MathOptInterface

mul2(x) = x * 2
div2(x) = div(x, 2)
d = MOI.Utilities.DenseDict{Int, Float64}(div2, mul2, 3)

d[4] = 0.25
@test !haskey(d, 2)
@test haskey(d, 4)
@test !haskey(d, 6)
@test length(d) == 1
@test collect(d) == [4 => 0.25]
@test d[4] == 0.25

d[2] = 1.5
@test haskey(d, 2)
@test haskey(d, 4)
@test !haskey(d, 6)
@test length(d) == 2
@test collect(d) == [2 => 1.5, 4 => 0.25]
@test d[2] == 1.5
@test d[4] == 0.25

d[6] = 0.75
@test haskey(d, 2)
@test haskey(d, 4)
@test haskey(d, 6)
@test length(d) == 3
@test collect(d) == [2 => 1.5, 4 => 0.25, 6 => 0.75]
@test d[2] == 1.5
@test d[4] == 0.25
@test d[6] == 0.75
