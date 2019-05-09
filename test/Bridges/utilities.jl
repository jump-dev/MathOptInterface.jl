function test_noc(bridged_mock, F, S, n)
    @test MOI.get(bridged_mock, MOI.NumberOfConstraints{F, S}()) == n
    @test length(MOI.get(bridged_mock, MOI.ListOfConstraintIndices{F, S}())) == n
    @test ((F, S) in MOI.get(bridged_mock, MOI.ListOfConstraints())) == !iszero(n)
end

# Test deletion of bridge
function test_delete_bridge(
    m::MOIB.AbstractBridgeOptimizer, ci::MOI.ConstraintIndex{F, S}, nvars::Int,
    nocs::Tuple; used_bridges = 1, num_bridged = 1) where {F, S}
    num_bridges = count(bridge -> bridge !== nothing, m.bridges)
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    test_noc(m, F, S, num_bridged)
    for noc in nocs
        test_noc(m, noc...)
    end
    @test MOI.is_valid(m, ci)
    MOI.delete(m, ci)
    @test_throws MOI.InvalidIndex{typeof(ci)} MOI.delete(m, ci)
    try
        MOI.delete(m, ci)
    catch err
        @test err.index == ci
    end
    @test !MOI.is_valid(m, ci)
    @test count(bridge -> bridge !== nothing, m.bridges) == num_bridges - used_bridges
    test_noc(m, F, S, num_bridged - 1)
    # As the bridge has been removed, if the constraints it has created where not removed, it wouldn't be there to decrease this counter anymore
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    for noc in nocs
        test_noc(m, noc...)
    end
end
