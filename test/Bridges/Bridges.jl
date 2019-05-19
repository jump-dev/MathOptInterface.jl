using Test

@testset "BridgeOptimizer" begin
    include("bridgeoptimizer.jl")
end
@testset "LazyBridgeOptimizer" begin
    include("lazybridgeoptimizer.jl")
end
@testset "Separate bridges" begin
    include("flip_sign_bridge.jl")
    include("vectorizebridge.jl")
    include("scalarizebridge.jl")
    include("slackbridge.jl")
    include("functionize_bridge.jl")
    include("intervalbridge.jl")
    include("rsocbridge.jl")
    include("quadtosocbridge.jl")
    include("geomeanbridge.jl")
    include("square_bridge.jl")
    include("detbridge.jl")
    include("soctopsdbridge.jl")
end
