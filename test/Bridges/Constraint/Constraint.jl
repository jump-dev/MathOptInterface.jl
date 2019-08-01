using Test
@testset "Map" begin
    include("map.jl")
end
include("flip_sign.jl")
include("vectorize.jl")
include("scalarize.jl")
include("slack.jl")
include("functionize.jl")
include("interval.jl")
include("rsoc.jl")
include("quad_to_soc.jl")
include("geomean.jl")
include("square.jl")
include("det.jl")
include("soc_to_psd.jl")
include("indicator.jl")
