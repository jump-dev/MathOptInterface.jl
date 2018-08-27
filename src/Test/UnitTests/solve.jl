"""
    solve_objbound_edge_cases(model::MOI.ModelLike, config::TestConfig)

Test a variety of edge cases related to the ObjectiveBound attribute.
"""
function solve_objbound_edge_cases(model::MOI.ModelLike, config::TestConfig)
    @testset "Min IP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            minobjective: 2.0x + -1.0
            c1: x >= 1.5
            c2: x in Integer()
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 3.0,
            variable_primal = [(x, 2.0)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) <= 3.0
        end
    end

    @testset "Max IP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            maxobjective: 2.0x + 1.0
            c1: x <= 1.5
            c2: x in Integer()
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 3.0,
            variable_primal = [(x, 1.0)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) >= 3.0
        end
    end

    @testset "Min LP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            minobjective: 2.0x + -1.0
            c1: x >= 1.5
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 2.0,
            variable_primal = [(x, 1.5)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) <= 2.0
        end
    end

    @testset "Max LP with constant" begin
        MOI.empty!(model)
        @test MOI.is_empty(model)
        MOIU.loadfromstring!(model, """
            variables: x
            maxobjective: 2.0x + 1.0
            c1: x <= 1.5
        """)
        x = MOI.get(model, MOI.VariableIndex, "x")
        test_model_solution(model, config;
            objective_value = 4.0,
            variable_primal = [(x, 1.5)]
        )
        if config.solve
            @test MOI.get(model, MOI.ObjectiveBound()) >= 4.0
        end
    end
end
unittests["solve_objbound_edge_cases"] = solve_objbound_edge_cases
