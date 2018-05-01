module Test

using MathOptInterface
const MOI = MathOptInterface

using Compat
using Compat.Test

include("config.jl")

include("modellike.jl")

include("contlinear.jl")
include("contconic.jl")
include("contquadratic.jl")

include("intlinear.jl")
include("intconic.jl")

include("nlp.jl")

end # module
