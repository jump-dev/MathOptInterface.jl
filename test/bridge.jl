# Model not supporting Interval
MOIU.@model(SimpleModel,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.RotatedSecondOrderCone, MOI.GeometricMeanCone,
             MOI.PositiveSemidefiniteConeTriangle, MOI.ExponentialCone),
            (),
            (MOI.SingleVariable,),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction,))

function test_noc(bridgedmock, F, S, n)
    @test MOI.get(bridgedmock, MOI.NumberOfConstraints{F, S}()) == n
    @test length(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{F, S}())) == n
    @test ((F, S) in MOI.get(bridgedmock, MOI.ListOfConstraints())) == !iszero(n)
end

# Test deletion of bridge
function test_delete_bridge(m::MOIB.AbstractBridgeOptimizer, ci::MOI.ConstraintIndex{F, S}, nvars::Int, nocs::Tuple) where {F, S}
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    test_noc(m, F, S, 1)
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
    @test isempty(m.bridges)
    test_noc(m, F, S, 0)
    # As the bridge has been removed, if the constraints it has created where not removed, it wouldn't be there to decrease this counter anymore
    @test MOI.get(m, MOI.NumberOfVariables()) == nvars
    for noc in nocs
        test_noc(m, noc...)
    end
end

@testset "BridgeOptimizer" begin
    mock = MOIU.MockOptimizer(SimpleModel{Float64}())
    bridgedmock = MOIB.SplitInterval{Float64}(mock)

    @testset "Issue #453" begin
        MOI.empty!(bridgedmock)
        MOIU.loadfromstring!(bridgedmock, """
            variables: x
            maxobjective: 3.0x
            c: 2.0x in Interval(1.0, 4.0)
            d: x in LessThan(1.5)
        """)
        x = MOI.get(bridgedmock, MOI.VariableIndex, "x")
        @test isa(x, MOI.VariableIndex)
        c1 = MOI.get(bridgedmock, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}}, "c")
        @test isa(c1, MOI.ConstraintIndex{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}})
        c2 = MOI.get(bridgedmock, MOI.ConstraintIndex, "c")
        @test c1 == c2
        d1 = MOI.get(bridgedmock, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}}, "d")
        @test isa(d1, MOI.ConstraintIndex{MOI.SingleVariable, MOI.LessThan{Float64}})
        d2 = MOI.get(bridgedmock, MOI.ConstraintIndex, "d")
        @test d1 == d2
    end

    MOI.empty!(bridgedmock)

    @testset "Name test" begin
        MOIT.nametest(bridgedmock)
    end

    @testset "Copy test" begin
        MOIT.failcopytestc(bridgedmock)
        MOIT.failcopytestia(bridgedmock)
        MOIT.failcopytestva(bridgedmock)
        MOIT.failcopytestca(bridgedmock)
        MOIT.copytest(bridgedmock, SimpleModel{Float64}())
    end

    @testset "Custom test" begin
        model = MOIB.SplitInterval{Int}(SimpleModel{Int}())
        @test !MOIB.supports_bridging_constraint(model, MOI.VectorAffineFunction{Float64}, MOI.Interval{Float64})

        x, y = MOI.add_variables(model, 2)
        @test MOI.get(model, MOI.NumberOfVariables()) == 2

        f1 = MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(3, x)], 7)
        c1 = MOI.add_constraint(model, f1, MOI.Interval(-1, 1))

        @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]

        f2 = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([2, -1], [x, y]), 2)
        c2 = MOI.add_constraint(model, f1, MOI.GreaterThan(-2))

        @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 1)
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]

        @test MOI.is_valid(model, c2)
        MOI.delete(model, c2)

        @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
    end

    @testset "Continuous Linear" begin
        MOIT.contlineartest(bridgedmock, MOIT.TestConfig(solve=false))
    end
end

# Model not supporting RotatedSecondOrderCone
MOIU.@model(NoRSOCModel,
            (),
            (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan),
            (MOI.Zeros, MOI.Nonnegatives, MOI.Nonpositives, MOI.SecondOrderCone,
             MOI.ExponentialCone, MOI.PositiveSemidefiniteConeTriangle),
            (),
            (MOI.SingleVariable,),
            (MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction),
            (MOI.VectorOfVariables,),
            (MOI.VectorAffineFunction, MOI.VectorQuadraticFunction))

@testset "LazyBridgeOptimizer" begin
    mock = MOIU.MockOptimizer(NoRSOCModel{Float64}())
    bridgedmock = MOIB.LazyBridgeOptimizer(mock, Model{Float64}())
    MOIB.add_bridge(bridgedmock, MOIB.SplitIntervalBridge{Float64})
    MOIB.add_bridge(bridgedmock, MOIB.RSOCtoPSDBridge{Float64})
    MOIB.add_bridge(bridgedmock, MOIB.SOCtoPSDBridge{Float64})
    MOIB.add_bridge(bridgedmock, MOIB.RSOCBridge{Float64})

    @testset "Name test" begin
        MOIT.nametest(bridgedmock)
    end

    @testset "Copy test" begin
        MOIT.failcopytestc(bridgedmock)
        MOIT.failcopytestia(bridgedmock)
        MOIT.failcopytestva(bridgedmock)
        MOIT.failcopytestca(bridgedmock)
        MOIT.copytest(bridgedmock, NoRSOCModel{Float64}())
    end

    # Test that RSOCtoPSD is used instead of RSOC+SOCtoPSD as it is a shortest path.
    @testset "Bridge selection" begin
        MOI.empty!(bridgedmock)
        @test !(MOI.supports_constraint(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle))
        x = MOI.add_variables(bridgedmock, 3)
        c = MOI.add_constraint(bridgedmock, MOI.VectorOfVariables(x), MOI.RotatedSecondOrderCone(3))
        @test MOIB.bridge(bridgedmock, c) isa MOIB.RSOCtoPSDBridge
        @test bridgedmock.dist[(MathOptInterface.VectorOfVariables, MathOptInterface.RotatedSecondOrderCone)] == 1
    end

    @testset "Supports" begin
        fullbridgedmock = MOIB.fullbridgeoptimizer(mock, Float64)
        for F in [MOI.SingleVariable, MOI.ScalarAffineFunction{Float64},
                  MOI.ScalarQuadraticFunction{Float64}]
            @test MOI.supports_constraint(fullbridgedmock, F,
                                          MOI.Interval{Float64})
        end
        for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64},
                  MOI.VectorQuadraticFunction{Float64}]
            @test MOI.supports_constraint(fullbridgedmock, F,
                                          MOI.PositiveSemidefiniteConeSquare)
            @test MOI.supports_constraint(fullbridgedmock, F,
                                          MOI.GeometricMeanCone)
        end
        for F in [MOI.VectorOfVariables, MOI.VectorAffineFunction{Float64}]
            # The bridges in this for loop do not support yet
            # VectorQuadraticFunction. See TODO's for the reason.
            # TODO: Missing vcat for quadratic for supporting quadratic.
            @test MOI.supports_constraint(fullbridgedmock, F,
                                          MOI.RotatedSecondOrderCone)
            # TODO: Det bridges need to use MOIU.operate to support quadratic.
            @test MOI.supports_constraint(fullbridgedmock, F,
                                          MOI.LogDetConeTriangle)
            @test MOI.supports_constraint(fullbridgedmock, F,
                                          MOI.RootDetConeTriangle)
        end
    end

    @testset "Combining two briges" begin
        fullbridgedmock = MOIB.fullbridgeoptimizer(mock, Float64)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0, 1, 1, 0, 1, √2])
        config = MOIT.TestConfig()
        MOIT.rootdett1vtest(fullbridgedmock, config)
        MOIT.rootdett1ftest(fullbridgedmock, config)
        # Dual is not yet implemented for RootDet and GeoMean bridges
        ci = first(MOI.get(fullbridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle}()))
        test_delete_bridge(fullbridgedmock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                                    (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
                                                    (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)))

    end

    @testset "Continuous Linear" begin
        MOIT.contlineartest(bridgedmock, MOIT.TestConfig(solve=false))
    end

    @testset "Continuous Conic" begin
        MOIT.contconictest(MOIB.fullbridgeoptimizer(mock, Float64), MOIT.TestConfig(solve=false), ["logdets", "rootdets", "psds"])
    end
end

@testset "Bridge tests" begin
    mock = MOIU.MockOptimizer(SimpleModel{Float64}())
    config = MOIT.TestConfig()

    @testset "Vectorize" begin
        bridgedmock = MOIB.Vectorize{Float64}(mock)

        MOIT.scalar_function_constant_not_zero(bridgedmock)

        MOIT.basic_constraint_tests(bridgedmock, config,
                                    include=Iterators.product(
                                        [MOI.SingleVariable,
                                         MOI.ScalarAffineFunction{Float64}],
                                         # TODO add it when operate(vcat, ...)
                                         # is implemented for quadratic
                                         #MOI.ScalarQuadraticFunction{Float64}],
                                        [MOI.EqualTo{Float64},
                                         MOI.GreaterThan{Float64},
                                         MOI.LessThan{Float64}]))

        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 0],
                (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
                (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0], [1]]))
        MOIT.linear2test(bridgedmock, config)

        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
        MOIT.linear4test(bridgedmock, config)

        MOIU.set_mock_optimize!(mock,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4/3, 4/3]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [4, 0]),
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2]))
        MOIT.linear5test(mock, config)

        MOIU.set_mock_optimize!(mock,
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, 0]),
            (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [100, -100]))
        MOIT.linear6test(mock, config)

        MOIU.set_mock_optimize!(mock,
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1/2, 1],
                 (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1], [-2]],
                 (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[2], [0], [0]]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1],
                 (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) => [[-1]],
                 (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) => [[0]]))
        # linear14 has double variable bounds for the z variable
        mock.eval_variable_constraint_dual = false
        MOIT.linear14test(bridgedmock, config)
        mock.eval_variable_constraint_dual = true

        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(3),
                               (MOI.VectorAffineFunction{Float64}, MOI.Zeros) => [[2]])
        MOIT.psdt0vtest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}}()))
        test_delete_bridge(bridgedmock, ci, 3,
                           ((MOI.VectorAffineFunction{Float64},
                             MOI.Zeros, 0),))
   end

    @testset "Interval" begin
        bridgedmock = MOIB.SplitInterval{Float64}(mock)
        MOIT.basic_constraint_tests(bridgedmock, config,
                                    include=[(MOI.SingleVariable,
                                              MOI.Interval{Float64}),
                                             (MOI.ScalarAffineFunction{Float64},
                                              MOI.Interval{Float64}),
                                             (MOI.ScalarQuadraticFunction{Float64},
                                              MOI.Interval{Float64})])
        MOIU.set_mock_optimize!(mock,
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [5.0, 5.0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2.5, 2.5],
                  (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [6.0, 6.0]))
        MOIT.linear10test(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}}()))
        newf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], MOI.get(bridgedmock, MOI.ListOfVariableIndices())), 0.0)
        MOI.set(bridgedmock, MOI.ConstraintFunction(), ci, newf)
        @test MOI.get(bridgedmock, MOI.ConstraintFunction(), ci) ≈ newf
        test_delete_bridge(bridgedmock, ci, 2, ((MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
                                                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},    0)))
   end

    @testset "RSOC" begin
        bridgedmock = MOIB.RSOC{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2, 0.5, 1.0],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64}) => [-√2, -1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)  => [[3/2, 1/2, -1.0, -1.0]])
        MOIT.rotatedsoc1vtest(bridgedmock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone)  => [[3/2, 1/2, -1.0, -1.0]])
        MOIT.rotatedsoc1ftest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
        test_delete_bridge(bridgedmock, ci, 2, ((MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone, 0),))
    end

    @testset "QuadtoSOC" begin
        bridgedmock = MOIB.QuadtoSOC{Float64}(mock)
        @testset "Error for non-convex quadratic constraints" begin
            x = MOI.add_variable(bridgedmock)
            @test_throws ErrorException begin
                MOI.add_constraint(bridgedmock,
                                   MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Float64}[],
                                                               [MOI.ScalarQuadraticTerm(1.0, x, x)],
                                                               0.0),
                                   MOI.GreaterThan(0.0))
            end
            @test_throws ErrorException begin
                MOI.add_constraint(bridgedmock,
                                   MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{Float64}[],
                                                               [MOI.ScalarQuadraticTerm(-1.0, x, x)],
                                                               0.0),
                                   MOI.LessThan(0.0))
            end
        end
        @testset "Quadratic constraints with 2 variables" begin
            MOIU.set_mock_optimize!(mock,
                (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [0.5, 0.5])
                ),
                (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock,
                    MOI.OPTIMAL,
                    (MOI.FEASIBLE_POINT, [0.5, (√13 - 1)/4])
                )
            )
            MOIT.solve_qcp_edge_cases(bridgedmock, config)
            ci = first(MOI.get(mock,
                               MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},
                                                           MOI.RotatedSecondOrderCone}()))
            x, y = MOI.get(mock, MOI.ListOfVariableIndices())
            # The matrix is
            # 2 1
            # 1 2
            # for which the cholesky factorization is U' * U with U =
            # √2 √2/2
            #  . √3/√2
            expected = MOI.VectorAffineFunction{Float64}(MOI.VectorAffineTerm.([3, 3, 4],
                                                                               MOI.ScalarAffineTerm.([√2, √2/2, √3/√2],
                                                                                                     [x, y, y])),
                                                         [1.0, 1.0, 0.0, 0.0])
            @test MOI.get(mock, MOI.ConstraintFunction(), ci) ≈ expected
        end
        @testset "QCP tests" begin
            MOIU.set_mock_optimize!(mock,
                (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/2, 7/4], MOI.FEASIBLE_POINT))
            MOIT.qcp1test(bridgedmock, config)
            MOIU.set_mock_optimize!(mock,
                (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FEASIBLE_POINT))
            MOIT.qcp2test(bridgedmock, config)
            MOIU.set_mock_optimize!(mock,
                (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [√2], MOI.FEASIBLE_POINT))
            MOIT.qcp3test(bridgedmock, config)
            @testset "Bridge deletion" begin
                ci = first(MOI.get(bridgedmock,
                                   MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Float64},
                                                               MOI.LessThan{Float64}}()))
                test_delete_bridge(bridgedmock, ci, 1, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),))
            end
        end
    end

    @testset "SquarePSD" begin
        bridgedmock = MOIB.SquarePSD{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(4),
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [2, 2])
        MOIT.psds0vtest(bridgedmock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, ones(4),
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[1, -1, 1]],
                              (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64})                 => [2, 2])
        MOIT.psds0ftest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock,
                           MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64},
                                                       MOI.PositiveSemidefiniteConeSquare}()))
        test_delete_bridge(bridgedmock, ci, 4,
                           ((MOI.VectorAffineFunction{Float64},
                             MOI.PositiveSemidefiniteConeTriangle, 0),
                            (MOI.ScalarAffineFunction{Float64},
                             MOI.EqualTo{Float64}, 1)))
    end

    @testset "GeoMean" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [ones(4); 2; √2; √2])
        bridgedmock = MOIB.GeoMean{Float64}(mock)
        MOIT.geomean1vtest(bridgedmock, config)
        MOIT.geomean1ftest(bridgedmock, config)
        # Dual is not yet implemented for GeoMean bridge
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
        test_delete_bridge(bridgedmock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},      1)))
    end

    @testset "SOCtoPSD" begin
        bridgedmock = MOIB.SOCtoPSD{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2/2, -1/2, √2/4, -1/2, √2/4, √2/4]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)                            => [[-√2]])
        MOIT.soc1vtest(bridgedmock, config)
        MOIT.soc1ftest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone}()))
        test_delete_bridge(bridgedmock, ci, 3, ((MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0),))
    end

    @testset "RSOCtoPSD" begin
        bridgedmock = MOIB.RSOCtoPSD{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2, 0.5, 1.0],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-√2, -1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
        MOIT.rotatedsoc1vtest(bridgedmock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
        MOIT.rotatedsoc1ftest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
        test_delete_bridge(bridgedmock, ci, 2, ((MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0),))
    end

    @testset "LogDet" begin
        bridgedmock = MOIB.LogDet{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 0, 1, 1, 0, 1, 0, 0, 1])
        MOIT.logdett1vtest(bridgedmock, config)
        MOIT.logdett1ftest(bridgedmock, config)
        # Dual is not yet implemented for LogDet bridge
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle}()))
        test_delete_bridge(bridgedmock, ci, 5, ((MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0), (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)))
    end

    @testset "RootDet" begin
        bridgedmock = MOIB.RootDet{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0, 1, 1, 0, 1])
        MOIT.rootdett1vtest(bridgedmock, config)
        MOIT.rootdett1ftest(bridgedmock, config)
        # Dual is not yet implemented for RootDet bridge
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle}()))
        test_delete_bridge(bridgedmock, ci, 4, ((MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0),
                                                (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone, 0),
                                                (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)))
    end
end

# We need to test this in a module at the top level because it can't be defined
# in a testset. If it runs without error, then we're okay.
module TestExternalBridge
    using MathOptInterface

    struct StrictlyGreaterThan <: MathOptInterface.AbstractScalarSet end
    struct StrictlyGreaterBridge{T} <: MathOptInterface.Bridges.AbstractBridge end

    function StrictlyGreaterBridge(
            model,
            func::MathOptInterface.SingleVariable,
            set::StrictlyGreaterThan)
        return StrictlyGreaterBridge{Float64}()
    end

    function MathOptInterface.supports_constraint(
            ::Type{StrictlyGreaterBridge{T}},
            ::Type{MathOptInterface.SingleVariable},
            ::Type{StrictlyGreaterThan}) where {T}
        return true
    end

    function MathOptInterface.Bridges.added_constraint_types(
            ::Type{StrictlyGreaterBridge{T}},
            ::Type{MathOptInterface.SingleVariable},
            ::Type{StrictlyGreaterThan}) where {T}
        return [(
            MathOptInterface.SingleVariable,
            MathOptInterface.GreaterThan{T}
        )]
    end

    MathOptInterface.Bridges.@bridge(StrictlyGreater, StrictlyGreaterBridge,
        (StrictlyGreaterThan, ),
        (),
        (),
        (),
        (MathOptInterface.SingleVariable, ),
        (),
        (),
        ()
    )
end

@testset "@bridge with external components" begin
    model = SimpleModel{Float64}();
    @test MOI.supports_constraint(model, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test !MOI.supports_constraint(model, MOI.SingleVariable, TestExternalBridge.StrictlyGreaterThan)

    bridge = TestExternalBridge.StrictlyGreater{Float64}(model);
    @test MOI.supports_constraint(bridge, MOI.SingleVariable, MOI.GreaterThan{Float64})
    @test MOI.supports_constraint(bridge, MOI.SingleVariable, TestExternalBridge.StrictlyGreaterThan)
end
