@testset "Unit Tests" begin
    mock = MOIU.MockOptimizer(Model{Float64}())
    config = MOIT.TestConfig()
    MOIT.unittest(mock, config, [
        "solve_blank_obj",
        "solve_constant_obj",
        "solve_singlevariable_obj",
        "solve_with_lowerbound",
        "solve_with_upperbound",
        "solve_affine_lessthan",
        "solve_affine_greaterthan",
        "solve_affine_equalto",
        "solve_affine_interval"
        ])
end
