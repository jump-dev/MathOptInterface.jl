# include("C:/Users/joaquimgarcia/.julia/dev/MathOptInterface/test/Utilities/DoubleDicts.jl")
# include("C:/Users/joaquimgarcia/.julia/dev/MathOptInterface/src/Utilities/DoubleDicts.jl")

using MathOptInterface, Test
const MOI = MathOptInterface
const CI{F,S} = MOI.ConstraintIndex{F,S}

const DoubleDicts = MathOptInterface.Utilities.DoubleDicts

const CI_I = CI{MOI.SingleVariable, MOI.Integer}
const CI_B = CI{MOI.SingleVariable, MOI.ZeroOne}

dict = DoubleDicts.DoubleDict()

@test isempty(dict)

@test length(dict) == 0

empty!(dict)
@test isempty(dict)

@test !haskey(dict, CI_I(1))

dict[CI_I(1)] = CI_I(1)

@test haskey(dict, CI_I(1))

@test values(dict) == [CI_I(1)]
@test keys(dict) == [CI_I(1)]
@test dict[CI_I(1)] == CI_I(1)

for (k,v) in dict
    @test k == v
end

delete!(dict, CI_I(1))

@test !haskey(dict, CI_I(1))
@test isempty(dict)

dict[CI_I(1)] = CI_I(1)
@test length(dict) == 1

empty!(dict)
@test isempty(dict)
@test length(dict) == 0

dict[CI_I(1)] = CI_I(1)

idict = DoubleDicts.WithType{MOI.SingleVariable, MOI.Integer}(dict)
bdict = DoubleDicts.WithType{MOI.SingleVariable, MOI.ZeroOne}(dict)

sizehint!(idict, 2)

@test length(idict) == 1
@test length(bdict) == 0

@test haskey(idict, CI_I(1))
@test !haskey(idict, CI_I(2))
@test !haskey(bdict, CI_B(1))

@test values(idict) == [CI_I(1)]
@test keys(idict) == [CI_I(1)]
@test idict[CI_I(1)] == CI_I(1)

idict[CI_I(2)] = CI_I(2)

@test haskey(idict, CI_I(2))
delete!(idict, CI_I(2))
@test !haskey(idict, CI_I(2))

for (k,v) in idict
    @test k == v
end

@test !isempty(idict)
@test isempty(bdict)
empty!(idict)
empty!(bdict)
@test isempty(idict)
@test isempty(bdict)