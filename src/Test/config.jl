struct Config{T<:Real}
    atol::T # absolute tolerance for ...
    rtol::T # relative tolerance for ...
    solve::Bool # optimize and test result
    query_number_of_constraints::Bool # can get `MOI.NumberOfConstraints` attribute
    query::Bool # can get objective function, and constraint functions, and constraint sets
    modify_lhs::Bool # can modify function of a constraint
    duals::Bool # test dual solutions
    dual_objective_value::Bool # test `DualObjectiveValue`
    infeas_certificates::Bool # check for primal or dual infeasibility certificates when appropriate
    # The expected "optimal" status returned by the solver. Either
    # MOI.OPTIMAL or MOI.LOCALLY_SOLVED.
    optimal_status::MOI.TerminationStatusCode
    basis::Bool # can get variable and constraint basis status

    """
        Config{T}(;
            atol::Real = Base.rtoldefault(T),
            rtol::Real = Base.rtoldefault(T),
            solve::Bool = true,
            query_number_of_constraints::Bool = true,
            query::Bool = true,
            modify_lhs::Bool = true,
            duals::Bool = true,
            dual_objective_value::Bool = duals,
            infeas_certificates::Bool = true,
            optimal_status = MOI.OPTIMAL,
            basis::Bool = false,
        )

    Return an object that is used to configure various tests.

    ## Keywords

     * `atol::Real = Base.rtoldefault(T)`: Control the absolute tolerance used
        when comparing solutions.
     * `rtol::Real = Base.rtoldefault(T)`: Control the relative tolerance used
        when comparing solutions.
     * `solve::Bool = true`: Set to `false` to skip tests requiring a call to
       [`MOI.optimize!`](@ref)
     * `query_number_of_constraints::Bool = true`: Set to `false` to skip tests
       requiring a call to [`MOI.NumberOfConstraints`](@ref).
     * `query::Bool = true`: Set to `false` to skip tests requiring a call to
       [`MOI.get`](@ref) for [`MOI.ConstraintFunction`](@ref) and
       [`MOI.ConstraintSet`](@ref)
     * `modify_lhs::Bool = true`:
     * `duals::Bool = true`: Set to `false` to skip tests querying
       [`MOI.ConstraintDual`](@ref).
     * `dual_objective_value::Bool = duals`: Set to `false` to skip tests
       querying [`MOI.DualObjectiveValue`](@ref).
     * `infeas_certificates::Bool = true`: Set to `false` to skip tests querying
       primal and dual infeasibility certificates.
     * `optimal_status = MOI.OPTIMAL`: Set to `MOI.LOCALLY_SOLVED` if the solver
       cannot prove global optimality.
     * `basis::Bool = false`: Set to `true` if the solver supports
       [`MOI.ConstraintBasisStatus`](@ref)
    """
    function Config{T}(;
        atol::Real = Base.rtoldefault(T),
        rtol::Real = Base.rtoldefault(T),
        solve::Bool = true,
        query_number_of_constraints::Bool = true,
        query::Bool = true,
        modify_lhs::Bool = true,
        duals::Bool = true,
        dual_objective_value::Bool = duals,
        infeas_certificates::Bool = true,
        optimal_status = MOI.OPTIMAL,
        basis::Bool = false,
    ) where {T<:Real}
        return new(
            atol,
            rtol,
            solve,
            query_number_of_constraints,
            query,
            modify_lhs,
            duals,
            dual_objective_value,
            infeas_certificates,
            optimal_status,
            basis,
        )
    end
    Config(; kwargs...) = Config{Float64}(; kwargs...)
end

@deprecate TestConfig Config

"""
    @moitestset setname subsets

Defines a function `setnametest(model, config, exclude)` that runs the tests defined in the dictionary `setnametests`
with the model `model` and config `config` except the tests whose dictionary key is in `exclude`.
If `subsets` is `true` then each test runs in fact multiple tests hence the `exclude` argument is passed
as it can also contains test to be excluded from these subsets of tests.
"""
macro moitestset(setname, subsets = false)
    testname = Symbol(string(setname) * "test")
    testdict = Symbol(string(testname) * "s")
    if subsets
        runtest = :(f(model, config, exclude))
    else
        runtest = :(f(model, config))
    end
    return esc(
        :(
            function $testname(
                model::$MOI.ModelLike,
                config::$MOI.Test.Config,
                exclude::Vector{String} = String[],
            )
                for (name, f) in $testdict
                    if name in exclude
                        continue
                    end
                    @testset "$name" begin
                        $runtest
                    end
                end
            end
        ),
    )
end
