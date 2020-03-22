struct TestConfig{T <: Number}
    atol::Float64 # absolute tolerance for ...
    rtol::Float64 # relative tolerance for ...
    solve::Bool # optimize and test result
    query::Bool # can get objective function, and constraint functions, and constraint sets
    modify_lhs::Bool # can modify function of a constraint
    duals::Bool # test dual solutions
    dual_objective_value::Bool # test `DualObjectiveValue`
    infeas_certificates::Bool # check for primal or dual infeasibility certificates when appropriate
    # The expected "optimal" status returned by the solver. Either
    # MOI.OPTIMAL or MOI.LOCALLY_SOLVED.
    optimal_status::MOI.TerminationStatusCode
    basis::Bool # can get variable and constraint basis status
    function TestConfig{T}(;
        atol::Float64 = 1e-8, rtol::Float64 = 1e-8, solve::Bool = true,
        query::Bool = true, modify_lhs::Bool = true, duals::Bool = true,
        dual_objective_value::Bool = duals, infeas_certificates::Bool = true,
        optimal_status = MOI.OPTIMAL, basis::Bool = false) where {T <: Number}
        new(atol, rtol, solve, query, modify_lhs, duals, dual_objective_value,
            infeas_certificates, optimal_status, basis)
    end
    TestConfig(;kwargs...) = TestConfig{Float64}(; kwargs...)
end

"""
    @moitestset setname subsets

Defines a function `setnametest(model, config, exclude)` that runs the tests defined in the dictionary `setnametests`
with the model `model` and config `config` except the tests whose dictionary key is in `exclude`.
If `subsets` is `true` then each test runs in fact multiple tests hence the `exclude` argument is passed
as it can also contains test to be excluded from these subsets of tests.
"""
macro moitestset(setname, subsets=false)
    testname = Symbol(string(setname) * "test")
    testdict = Symbol(string(testname) * "s")
    if subsets
        runtest = :( f(model, config, exclude) )
    else
        runtest = :( f(model, config) )
    end
    esc(:(
        function $testname(model::$MOI.ModelLike, config::$MOI.Test.TestConfig, exclude::Vector{String} = String[])
            for (name,f) in $testdict
                if name in exclude
                    continue
                end
                @testset "$name" begin
                    $runtest
                end
            end
        end
    ))
end
