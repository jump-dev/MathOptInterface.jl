function _test_num_constraints(bridged_mock, F, S, n)
    @test MOI.get(bridged_mock, MOI.NumberOfConstraints{F,S}()) == n
    @test length(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{F,S}())) == n
    ret = (F, S) in MOI.get(bridged_mock, MOI.ListOfConstraintTypesPresent())
    @test ret == !iszero(n)
    return
end

function _warn_incomplete_list_num_constraints(BT, list_num_constraints)
    for (S,) in MOIB.added_constrained_variable_types(BT)
        F = MOIU.variable_function_type(S)
        if !any(c -> c[1] == F && c[2] == S, list_num_constraints)
            error(
                "Bridges of type $BT add constrained variable in $S but " *
                "their number is not tested.",
            )
        end
    end
    for (F, S) in MOIB.added_constraint_types(BT)
        if !any(c -> c[1] == F && c[2] == S, list_num_constraints)
            error(
                "Bridges of type $BT add $F-in-$S constraints but their " *
                "number is not tested.",
            )
        end
    end
    return
end

"""
    _test_delete_bridge(
        m::MOIB.AbstractBridgeOptimizer,
        ci::MOI.ConstraintIndex{F, S},
        nvars::Int,
        list_num_constraints::Tuple;
        used_bridges = 1,
        num_bridged = 1,
    ) where {F, S}

Test deletion of the constraint `ci` in model `m`.

* The number of variables added in `m` is `nvars` (ignoring the variables added
  by the bridges).
* For each `(Fi, Si, n)` in `list_num_constraints`, the number of `Fi`-in-`Si`
  constraints added to `m` is `n` (ignoring the constraints added by the
  bridges). All constraint types added by the bridge bridging `ci` should appear
  in `list_num_constraints`. Otherwise a error is thrown to ensure good test
  coverage.
* The number of bridges that will be deleted when deleted `ci` is
  `used_bridges`.
* The number of `F`-in-`S` constraints is `num_bridged`.
"""
function _test_delete_bridge(
    m::MOIB.AbstractBridgeOptimizer,
    ci::MOI.ConstraintIndex{F,S},
    nvars::Int,
    list_num_constraints::Tuple;
    used_bridges = 1,
    num_bridged = 1,
) where {F,S}
    _warn_incomplete_list_num_constraints(
        typeof(MOIB.bridge(m, ci)),
        list_num_constraints,
    )
    function num_bridges()
        return count(bridge -> true, values(MOIB.Constraint.bridges(m)))
    end
    start_num_bridges = num_bridges()
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars
    _test_num_constraints(m, F, S, num_bridged)
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    @test MOI.is_valid(m, ci)
    MOI.delete(m, ci)
    @test_throws MOI.InvalidIndex(ci) MOI.delete(m, ci)
    @test !MOI.is_valid(m, ci)
    @test num_bridges() == start_num_bridges - used_bridges
    _test_num_constraints(m, F, S, num_bridged - 1)
    # As the bridge has been removed, if the constraints it has created were not
    # removed, it wouldn't be there to decrease this counter anymore.
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    return
end

# Test deletion of variable bridge used for variable `vi`
function _test_delete_bridged_variable(
    m::MOIB.AbstractBridgeOptimizer,
    vi::MOI.VariableIndex,
    S::Type,
    nvars::Int,
    list_num_constraints::Tuple;
    used_bridges = 1,
    num_bridged = 1,
    used_constraints = 1,
)
    _warn_incomplete_list_num_constraints(
        typeof(MOIB.bridge(m, vi)),
        list_num_constraints,
    )
    function num_bridges()
        return count(bridge -> true, values(MOIB.Variable.bridges(m)))
    end
    start_num_bridges = num_bridges()
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars
    if S != MOI.Reals
        F =
            S <: MOI.AbstractScalarSet ? MOI.SingleVariable :
            MOI.VectorOfVariables
        _test_num_constraints(m, F, S, num_bridged)
    end
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    @test MOI.is_valid(m, vi)
    MOI.delete(m, vi)
    @test_throws MOI.InvalidIndex(vi) MOI.delete(m, vi)
    @test !MOI.is_valid(m, vi)
    @test num_bridges() == start_num_bridges - used_bridges
    if S != MOI.Reals
        _test_num_constraints(m, F, S, num_bridged - used_constraints)
    end
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars - 1
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars - 1
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    return
end

# Test deletion of variable bridge used for vector of variables `vis`
function _test_delete_bridged_variables(
    m::MOIB.AbstractBridgeOptimizer,
    vis::Vector{MOI.VariableIndex},
    S::Type,
    nvars::Int,
    list_num_constraints::Tuple;
    used_bridges = 1,
    num_bridged = 1,
)
    _warn_incomplete_list_num_constraints(
        typeof(MOIB.bridge(m, vis[1])),
        list_num_constraints,
    )
    function num_bridges()
        return count(bridge -> true, values(MOIB.Variable.bridges(m)))
    end
    start_num_bridges = num_bridges()
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars
    if S != MOI.Reals
        F =
            S <: MOI.AbstractScalarSet ? MOI.SingleVariable :
            MOI.VectorOfVariables
        _test_num_constraints(m, F, S, num_bridged)
    end
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    @test all(vi -> MOI.is_valid(m, vi), vis)
    MOI.delete(m, vis)
    @test_throws MOI.InvalidIndex(vis[1]) MOI.delete(m, vis)
    @test all(vi -> !MOI.is_valid(m, vi), vis)
    @test num_bridges() == start_num_bridges - used_bridges
    if S != MOI.Reals
        _test_num_constraints(m, F, S, num_bridged - 1)
    end
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars - length(vis)
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars - length(vis)
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    return
end

function _test_delete_objective(
    m::MOIB.AbstractBridgeOptimizer,
    nvars::Int,
    list_num_constraints::Tuple;
    used_bridges = 1,
)
    _warn_incomplete_list_num_constraints(
        typeof(MOIB.Objective.root_bridge(MOIB.Objective.bridges(m))),
        list_num_constraints,
    )
    function num_bridges()
        return length(MOIB.Objective.bridges(m))
    end
    start_num_bridges = num_bridges()
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    MOI.set(m, MOI.ObjectiveSense(), MOI.FEASIBILITY_SENSE)
    @test MOI.get(m, MOI.ObjectiveSense()) == MOI.FEASIBILITY_SENSE
    @test num_bridges() == start_num_bridges - used_bridges
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    @test length(MOI.get(m, MOI.ListOfVariableIndices())) == nvars
    for num_constraints in list_num_constraints
        _test_num_constraints(m, num_constraints...)
    end
    return
end
