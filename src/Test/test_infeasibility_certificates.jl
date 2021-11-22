for sense in (MOI.MIN_SENSE, MOI.MAX_SENSE), offset in (0.0, 1.2)
    o = iszero(offset) ? "" : "_offset"
    f_unbd_name = Symbol("test_unbounded_$(Symbol(sense))$(o)")
    f_infeas_name = Symbol("test_infeasible_$(Symbol(sense))$(o)")
    f_infeas_affine_name = Symbol("test_infeasible_affine_$(Symbol(sense))$(o)")
    @eval begin
        """
        Test the infeasibility certificates of an unbounded model.
        """
        function $(f_unbd_name)(model::MOI.ModelLike, config::Config)
            @requires _supports(config, MOI.optimize!)
            x = MOI.add_variable(model)
            MOI.set(model, MOI.ObjectiveSense(), $sense)
            f = 2.2 * x + $offset
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
            c = if $sense == MOI.MIN_SENSE
                MOI.add_constraint(model, 1.3 * x, MOI.LessThan(1.1))
            else
                MOI.add_constraint(model, 1.3 * x, MOI.GreaterThan(1.1))
            end
            MOI.optimize!(model)
            @requires(
                MOI.get(model, MOI.TerminationStatus()) == MOI.DUAL_INFEASIBLE,
            )
            @requires(
                MOI.get(model, MOI.PrimalStatus()) ==
                MOI.INFEASIBILITY_CERTIFICATE,
            )
            d = MOI.get(model, MOI.VariablePrimal(), x)
            obj = MOI.get(model, MOI.ObjectiveValue())
            if $sense == MOI.MIN_SENSE
                @test d < -config.atol
                @test obj < -config.atol
            else
                @test d > config.atol
                @test obj > config.atol
            end
            @test isapprox(2.2 * d, obj, config)
            Ad = MOI.get(model, MOI.ConstraintPrimal(), c)
            @test isapprox(1.3 * d, Ad, config)
            return
        end

        function setup_test(
            ::typeof($(f_unbd_name)),
            mock::MOIU.MockOptimizer,
            config::Config,
        )
            MOIU.set_mock_optimize!(
                mock,
                (mock::MOIU.MockOptimizer) -> begin
                    MOIU.mock_optimize!(
                        mock,
                        MOI.DUAL_INFEASIBLE,
                        (
                            MOI.INFEASIBILITY_CERTIFICATE,
                            [$sense == MOI.MIN_SENSE ? -1.0 : 1.0],
                        ),
                        MOI.NO_SOLUTION,
                    )
                end,
            )
            return
        end

        """
        Test the infeasibility certificates of an infeasible model.
        """
        function $(f_infeas_name)(model::MOI.ModelLike, config::Config)
            @requires _supports(config, MOI.optimize!)
            x = MOI.add_variable(model)
            MOI.set(model, MOI.ObjectiveSense(), $sense)
            f = 2.2 * x + $offset
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
            cl = MOI.add_constraint(model, x, MOI.GreaterThan(2.5))
            cu = MOI.add_constraint(model, 1.0 * x, MOI.LessThan(1.4))
            MOI.optimize!(model)
            @requires(
                MOI.get(model, MOI.TerminationStatus()) ==
                config.infeasible_status,
            )
            @requires(
                MOI.get(model, MOI.DualStatus()) ==
                MOI.INFEASIBILITY_CERTIFICATE,
            )
            dl = MOI.get(model, MOI.ConstraintDual(), cl)
            du = MOI.get(model, MOI.ConstraintDual(), cu)
            obj = MOI.get(model, MOI.DualObjectiveValue())
            if $sense == MOI.MIN_SENSE
                @test obj > config.atol
                @test isapprox(-(1.4 * dl + 2.5 * du), obj, config)
            else
                @test obj < -config.atol
                @test isapprox(1.4 * dl + 2.5 * du, obj, config)
            end
            @test dl > config.atol
            @test du < -config.atol
            @test isapprox(dl + du, 0.0, config)
            return
        end

        function setup_test(
            ::typeof($(f_infeas_name)),
            mock::MOIU.MockOptimizer,
            config::Config,
        )
            MOIU.set_mock_optimize!(
                mock,
                (mock::MOIU.MockOptimizer) -> begin
                    MOIU.mock_optimize!(
                        mock,
                        MOI.INFEASIBLE,
                        MOI.NO_SOLUTION,
                        MOI.INFEASIBILITY_CERTIFICATE,
                        (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                            [1.0],
                        (
                            MOI.ScalarAffineFunction{Float64},
                            MOI.LessThan{Float64},
                        ) => [-1.0],
                    )
                end,
            )
            return
        end

        """
        Test the infeasibility certificates of an infeasible model with affine
        constraints.
        """
        function $(f_infeas_affine_name)(model::MOI.ModelLike, config::Config)
            @requires _supports(config, MOI.optimize!)
            x = MOI.add_variable(model)
            MOI.set(model, MOI.ObjectiveSense(), $sense)
            f = 2.2 * x + $offset
            MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
            cl = MOI.add_constraint(model, 1.0 * x, MOI.GreaterThan(2.5))
            cu = MOI.add_constraint(model, x, MOI.LessThan(1.4))
            MOI.optimize!(model)
            @requires(
                MOI.get(model, MOI.TerminationStatus()) ==
                config.infeasible_status,
            )
            @requires(
                MOI.get(model, MOI.DualStatus()) ==
                MOI.INFEASIBILITY_CERTIFICATE,
            )
            dl = MOI.get(model, MOI.ConstraintDual(), cl)
            du = MOI.get(model, MOI.ConstraintDual(), cu)
            obj = MOI.get(model, MOI.DualObjectiveValue())
            if $sense == MOI.MIN_SENSE
                @test obj > config.atol
                @test isapprox(-(1.4 * dl + 2.5 * du), obj, config)
            else
                @test obj < -config.atol
                @test isapprox(1.4 * dl + 2.5 * du, obj, config)
            end
            @test dl > config.atol
            @test du < -config.atol
            @test isapprox(dl + du, 0.0, config)
            return
        end

        function setup_test(
            ::typeof($(f_infeas_affine_name)),
            mock::MOIU.MockOptimizer,
            config::Config,
        )
            MOIU.set_mock_optimize!(
                mock,
                (mock::MOIU.MockOptimizer) -> begin
                    MOIU.mock_optimize!(
                        mock,
                        MOI.INFEASIBLE,
                        MOI.NO_SOLUTION,
                        MOI.INFEASIBILITY_CERTIFICATE,
                        (
                            MOI.ScalarAffineFunction{Float64},
                            MOI.GreaterThan{Float64},
                        ) => [1.0],
                        (MOI.VariableIndex, MOI.LessThan{Float64}) =>
                            [-1.0],
                    )
                end,
            )
            return
        end
    end
end
