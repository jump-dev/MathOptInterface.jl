# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    test_cpsat_AllDifferent(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-AllDifferent constraint.
"""
function test_cpsat_AllDifferent(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.AllDifferent,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:3]
    x = first.(y)
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.AllDifferent(3))
    MOI.optimize!(model)
    x_val = MOI.get.(model, MOI.VariablePrimal(), x)
    @test abs(x_val[1] - x_val[2]) > 0.5
    @test abs(x_val[1] - x_val[3]) > 0.5
    @test abs(x_val[2] - x_val[3]) > 0.5
    return
end

function setup_test(
    ::typeof(test_cpsat_AllDifferent),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 1, 2]),
        ),
    )
    return
end

"""
    test_cpsat_CountDistinct(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-CountDistinct constraint.
"""
function test_cpsat_CountDistinct(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.CountDistinct,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:4]
    x = first.(y)
    MOI.add_constraint.(model, x, MOI.Interval(T(0), T(4)))
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.CountDistinct(4))
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), x))
    @test length(unique(x_val[2:end])) == x_val[1]
    return
end

function setup_test(
    ::typeof(test_cpsat_CountDistinct),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2, 0, 1, 0]),
        ),
    )
    return
end

"""
    test_cpsat_Among(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-Among constraint.
"""
function test_cpsat_Among(model::MOI.ModelLike, config::Config{T}) where {T}
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Among)
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    y = [MOI.add_constrained_variable(model, MOI.Integer()) for _ in 1:4]
    x = first.(y)
    set = Set([3, 4])
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Among(4, set))
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), x))
    @test x_val[1] == sum(x_val[i] in set for i in 2:length(x))
    return
end

function setup_test(
    ::typeof(test_cpsat_Among),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2, 3, 4, 0]),
        ),
    )
    return
end

"""
    test_cpsat_CountAtLeast(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-CountAtLeast constraint.
"""
function test_cpsat_CountAtLeast(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.CountAtLeast,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    x, _ = MOI.add_constrained_variable(model, MOI.Integer())
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    z, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint.(model, [x, y, z], MOI.Interval(T(0), T(3)))
    variables = [x, y, y, z]
    partitions = [2, 2]
    set = Set([3])
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(variables),
        MOI.CountAtLeast(1, partitions, set),
    )
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), [x, y, z]))
    @test x_val[1] == 3 || x_val[2] == 3
    @test x_val[2] == 3 || x_val[3] == 3
    return
end

function setup_test(
    ::typeof(test_cpsat_CountAtLeast),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 3, 0]),
        ),
    )
    return
end

"""
    test_cpsat_CountGreaterThan(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-CountGreaterThan constraint.
"""
function test_cpsat_CountGreaterThan(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.CountGreaterThan,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    c, _ = MOI.add_constrained_variable(model, MOI.Integer())
    y, _ = MOI.add_constrained_variable(model, MOI.Integer())
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([c; y; x]),
        MOI.CountGreaterThan(5),
    )
    MOI.optimize!(model)
    c_val = round(Int, MOI.get(model, MOI.VariablePrimal(), c))
    y_val = round(Int, MOI.get(model, MOI.VariablePrimal(), y))
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), x))
    @test c_val > sum(x_val[i] == y_val for i in 1:length(x))
    return
end

function setup_test(
    ::typeof(test_cpsat_CountGreaterThan),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[2, 4, 0, 4, 0]),
        ),
    )
    return
end

"""
    test_cpsat_BinPacking(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-BinPacking constraint.
"""
function test_cpsat_BinPacking(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.BinPacking{T},
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    N = 5
    bins = MOI.add_variables(model, N)
    weights = T[1, 1, 2, 2, 3]
    MOI.add_constraint.(model, bins, MOI.Integer())
    MOI.add_constraint.(model, bins, MOI.Interval(T(1), T(3)))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables(bins),
        MOI.BinPacking(T(3), weights),
    )
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), bins))
    sol = zeros(3)
    for i in 1:N
        sol[x_val[i]] += weights[i]
    end
    @test all(sol .<= 3)
    return
end

function setup_test(
    ::typeof(test_cpsat_BinPacking),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 2, 1, 2, 3]),
        ),
    )
    return
end

"""
    test_cpsat_Cumulative(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-Cumulative constraint.
"""
function test_cpsat_Cumulative(
    model::MOI.ModelLike,
    config::Config{T},
) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Cumulative,
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    s = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint.(model, s, MOI.Interval(T(0), T(3)))
    d = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint.(model, d, MOI.EqualTo(T(2)))
    r = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint.(model, r, MOI.EqualTo.(T[3, 2, 1]))
    b, _ = MOI.add_constrained_variable(model, MOI.Integer())
    MOI.add_constraint.(model, b, MOI.EqualTo(T(5)))
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([s; d; r; b]),
        MOI.Cumulative(10),
    )
    MOI.optimize!(model)
    s_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), s))
    d_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), d))
    r_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), r))
    b_val = round(Int, MOI.get(model, MOI.VariablePrimal(), b))
    times = zeros(1 + maximum(s_val) + maximum(d_val))
    for i in 1:3
        for j in 0:(d_val[i]-1)
            t = s_val[i] + j
            times[t+1] += r_val[i]
        end
    end
    @test all(times .<= b_val)
    return
end

function setup_test(
    ::typeof(test_cpsat_Cumulative),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[0, 1, 2, 2, 2, 2, 3, 2, 1, 5]),
        ),
    )
    return
end

"""
    test_cpsat_Table(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-Table constraint.
"""
function test_cpsat_Table(model::MOI.ModelLike, config::Config{T}) where {T}
    @requires MOI.supports_constraint(
        model,
        MOI.VectorOfVariables,
        MOI.Table{T},
    )
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    table = T[1 1 0; 0 1 1]
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Table(table))
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), x))
    @test x_val == [1, 1, 0] || x_val == [0, 1, 1]
    return
end

function setup_test(
    ::typeof(test_cpsat_Table),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 1, 0]),
        ),
    )
    return
end

"""
    test_cpsat_Circuit(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-Circuit constraint.
"""
function test_cpsat_Circuit(model::MOI.ModelLike, config::Config{T}) where {T}
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Circuit)
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    x = [MOI.add_constrained_variable(model, MOI.Integer())[1] for _ in 1:3]
    MOI.add_constraint(model, MOI.VectorOfVariables(x), MOI.Circuit(3))
    MOI.optimize!(model)
    x_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), x))
    @test x_val == [3, 1, 2] || x_val == [2, 3, 1]
    return
end

function setup_test(
    ::typeof(test_cpsat_Circuit),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[3, 1, 2]),
        ),
    )
    return
end

"""
    test_cpsat_Path(model::MOI.ModelLike, config::Config)

Add a VectorOfVariables-in-Path constraint.
"""
function test_cpsat_Path(model::MOI.ModelLike, config::Config{T}) where {T}
    @requires MOI.supports_constraint(model, MOI.VectorOfVariables, MOI.Path)
    @requires MOI.supports_add_constrained_variable(model, MOI.Integer)
    @requires _supports(config, MOI.optimize!)
    from = [1, 1, 2, 2, 3]
    to = [2, 3, 3, 4, 4]
    N, E = 4, 5
    s, _ = MOI.add_constrained_variable(model, MOI.Integer())
    t, _ = MOI.add_constrained_variable(model, MOI.Integer())
    ns = MOI.add_variables(model, N)
    MOI.add_constraint.(model, ns, MOI.ZeroOne())
    es = MOI.add_variables(model, E)
    MOI.add_constraint.(model, es, MOI.ZeroOne())
    MOI.add_constraint(
        model,
        MOI.VectorOfVariables([s; t; ns; es]),
        MOI.Path(from, to),
    )
    MOI.optimize!(model)
    s_val = round(Int, MOI.get(model, MOI.VariablePrimal(), s))
    @test 1 <= s_val <= 4
    t_val = round(Int, MOI.get(model, MOI.VariablePrimal(), t))
    @test 1 <= t_val <= 4
    ns_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), ns))
    es_val = round.(Int, MOI.get.(model, MOI.VariablePrimal(), es))
    outs = Vector{Int}[[1, 2], [3, 4], [5], Int[]]
    ins = Vector{Int}[[], [1], [2, 3], [4, 5]]
    has_edges = s_val == t_val ? 0 : 1
    # source: must have no incoming and one outgoing (if s != t)
    @test sum(es_val[o] for o in ins[s_val]; init = 0) == 0
    @test sum(es_val[o] for o in outs[s_val]; init = 0) == has_edges
    # dest: must have no outgoing and one incoming (if s != t)
    @test sum(es_val[o] for o in ins[t_val]; init = 0) == has_edges
    @test sum(es_val[o] for o in outs[t_val]; init = 0) == 0
    for i in 1:4
        if i != s_val && i != t_val
            # other nodes: must have one incoming and one outgoing iff node is
            # in subgraph.
            @test sum(es_val[o] for o in outs[i]; init = 0) == ns_val[i]
            @test sum(es_val[o] for o in ins[i]; init = 0) == ns_val[i]
        end
    end
    return
end

function setup_test(
    ::typeof(test_cpsat_Path),
    model::MOIU.MockOptimizer,
    ::Config{T},
) where {T}
    MOIU.set_mock_optimize!(
        model,
        (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(
            mock,
            MOI.OPTIMAL,
            (MOI.FEASIBLE_POINT, T[1, 4, 1, 1, 0, 1, 1, 0, 0, 1, 0]),
        ),
    )
    return
end
