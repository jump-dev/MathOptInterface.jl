for sense in (MOI.MIN_SENSE, MOI.MAX_SENSE), offset in (0.0, 1.2)
    o = iszero(offset) ? "" : "_offset"
    f_unbd_name = Symbol("test_unbounded_$(Symbol(sense))$(o)")
    f_infeas_name = Symbol("test_infeasible_$(Symbol(sense))$(o)")
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
            return
        end

        function setup_test(
            ::typeof($(f_unbd_name)),
            mock::MOIU.MockOptimizer,
            config::Config,
        )
            x = [$sense == MOI.MIN_SENSE ? -1.0 : 1.0]
            flag = mock.eval_objective_value
            mock.eval_objective_value = false
            MOIU.set_mock_optimize!(
                mock,
                (mock::MOIU.MockOptimizer) -> begin
                    MOI.set(mock, MOI.ObjectiveValue(), 2.2 * x[1])
                    MOIU.mock_optimize!(
                        mock,
                        MOI.DUAL_INFEASIBLE,
                        (MOI.INFEASIBILITY_CERTIFICATE, x),
                        MOI.NO_SOLUTION,
                    )
                end,
            )
            return () -> mock.eval_objective_value = flag
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
            cu = MOI.add_constraint(model, x, MOI.LessThan(1.4))
            MOI.optimize!(model)
            @requires(
                MOI.get(model, MOI.TerminationStatus()) == MOI.INFEASIBLE,
            )
            @requires(
                MOI.get(model, MOI.DualStatus()) ==
                MOI.INFEASIBILITY_CERTIFICATE,
            )
            dl = MOI.get(model, MOI.ConstraintDual(), cl)
            du = MOI.get(model, MOI.ConstraintDual(), cu)
            obj = MOI.get(model, MOI.DualObjectiveValue())
            @test dl > config.atol
            @test du < -config.atol
            @test obj < -config.atol
            @test isapprox(dl + du, 0.0, config)
            @test isapprox(1.4 * dl + 2.5 * du, obj, config)
            return
        end

        function setup_test(
            ::typeof($(f_infeas_name)),
            mock::MOIU.MockOptimizer,
            config::Config,
        )
            flag_obj = mock.eval_dual_objective_value
            flag_var = mock.eval_variable_constraint_dual
            mock.eval_dual_objective_value = false
            mock.eval_variable_constraint_dual = false
            MOIU.set_mock_optimize!(
                mock,
                (mock::MOIU.MockOptimizer) -> begin
                    MOI.set(mock, MOI.DualObjectiveValue(), -1.1)
                    MOIU.mock_optimize!(
                        mock,
                        MOI.INFEASIBLE,
                        MOI.NO_SOLUTION,
                        MOI.INFEASIBILITY_CERTIFICATE,
                        (MOI.VariableIndex, MOI.GreaterThan{Float64}) =>
                            [1.0],
                        (MOI.VariableIndex, MOI.LessThan{Float64}) =>
                            [-1.0],
                    )
                end,
            )
            return () -> begin
                mock.eval_dual_objective_value = flag_obj
                mock.eval_variable_constraint_dual = flag_var
            end
        end
    end
end
