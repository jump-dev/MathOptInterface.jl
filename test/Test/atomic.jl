@testset "Atomic Tests" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    MOIT.atomictest(mock, config, [
        "constant_obj",
        "getvariable",
        "getconstraint",
        "blank_obj",
        "lowerbound",
        "upperbound",
        "singlevariable_obj",
        "variablenames"
    ])
end
