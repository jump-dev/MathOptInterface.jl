module DeprecatedTest

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

using Test

include("config.jl")

include("modellike.jl")

include("contlinear.jl")
include("contconic.jl")
include("contquadratic.jl")

include("intlinear.jl")
include("intconic.jl")

include("nlp.jl")

include("UnitTests/unit_tests.jl")

end # module
