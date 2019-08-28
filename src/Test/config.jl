struct TestConfig
    atol::Float64 # absolute tolerance for ...
    rtol::Float64 # relative tolerance for ...
    solve::Bool # optimize and test result
    query::Bool # can get objective function, and constraint functions, and constraint sets
    modify_lhs::Bool # can modify function of a constraint
    duals::Bool # test dual solutions
    infeas_certificates::Bool # check for primal or dual infeasibility certificates when appropriate
    # The expected "optimal" status returned by the solver. Either
    # MOI.OPTIMAL or MOI.LOCALLY_SOLVED.
    optimal_status::MOI.TerminationStatusCode
    basis::Bool # can get variable and constraint basis status
    number_type::Type{<:Number}
    function TestConfig(;
        atol::Float64 = 1e-8, rtol::Float64 = 1e-8, solve::Bool = true,
        query::Bool = true, modify_lhs::Bool = true, duals::Bool = true,
        infeas_certificates::Bool = true, optimal_status = MOI.OPTIMAL,
        basis::Bool = false, number_type::Type{<:Number} = Float64)
        new(atol, rtol, solve, query, modify_lhs, duals, infeas_certificates,
            optimal_status, basis, number_type)
    end
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
