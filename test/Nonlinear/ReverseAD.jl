# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestReverseAD

using Test
import LinearAlgebra
import MathOptInterface
import SparseArrays

const MOI = MathOptInterface
const Nonlinear = MOI.Nonlinear
const ReverseAD = Nonlinear.ReverseAD
const Coloring = ReverseAD.Coloring

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

function test_objective_quadratic_univariate()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :($x^2 + 1))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2]) == 1.2^2 + 1
    g = [NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.2])
    @test g == [2.4]
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [1.2], 1.5, Float64[])
    @test H == 1.5 .* [2.0]
    MOI.eval_hessian_lagrangian_product(
        evaluator,
        H,
        [1.2],
        [1.2],
        1.5,
        Float64[],
    )
    @test H == [1.5 * 2.0 * 1.2]
    return
end

function test_objective_quadratic_multivariate()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :($x^2 + $x * $y + $y^2))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2, 2.3]) == 1.2^2 + 1.2 * 2.3 + 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.2, 2.3])
    @test g == [2 * 1.2 + 2.3, 1.2 + 2 * 2.3]
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [1.2, 2.3], 1.5, Float64[])
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    v = [0.3, 0.4]
    hv = [NaN, NaN]
    MOI.eval_hessian_lagrangian_product(
        evaluator,
        hv,
        [1.2, 2.3],
        v,
        1.5,
        Float64[],
    )
    @test hv ≈ 1.5 .* [2 1; 1 2] * v
    return
end

function test_objective_quadratic_multivariate_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    ex = Nonlinear.add_expression(model, :($x^2))
    ey = Nonlinear.add_expression(model, :($y^2))
    exy = Nonlinear.add_expression(model, :($ex + $x * $y))
    Nonlinear.set_objective(model, :($exy + $ey))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2, 2.3]) == 1.2^2 + 1.2 * 2.3 + 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.2, 2.3])
    @test g == [2 * 1.2 + 2.3, 1.2 + 2 * 2.3]
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [1.2, 2.3], 1.5, Float64[])
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    v = [0.3, 0.4]
    hv = [NaN, NaN]
    MOI.eval_hessian_lagrangian_product(
        evaluator,
        hv,
        [1.2, 2.3],
        v,
        1.5,
        Float64[],
    )
    @test hv ≈ 1.5 .* [2 1; 1 2] * v
    return
end

function test_objective_ifelse_comparison()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(ifelse(1 <= $x <= 2, $x^2, $y^2)))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2, 2.3]) == 1.2^2
    @test MOI.eval_objective(evaluator, [2.2, 2.3]) == 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.2, 2.3])
    @test g == [2 * 1.2, 0.0]
    MOI.eval_objective_gradient(evaluator, g, [2.2, 2.3])
    @test g == [0.0, 2 * 2.3]
    return
end

function test_objective_ifelse_logic()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(ifelse(1 <= $x && $x <= 2, $x^2, $y^2)))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2, 2.3]) == 1.2^2
    @test MOI.eval_objective(evaluator, [2.2, 2.3]) == 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.2, 2.3])
    @test g == [2 * 1.2, 0.0]
    MOI.eval_objective_gradient(evaluator, g, [2.2, 2.3])
    @test g == [0.0, 2 * 2.3]
    return
end

function test_objective_parameter()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    p = Nonlinear.add_parameter(model, 1.2)
    Nonlinear.set_objective(model, :($p * $x))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.3]) == 1.2 * 1.3
    g = [NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.3])
    @test g == [1.2]
    return
end

function test_objective_subexpression()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    input = :($x^2 + 1)
    expr = Nonlinear.add_expression(model, input)
    expr_2 = Nonlinear.add_expression(model, :($expr^2))
    Nonlinear.set_objective(model, :($expr_2^2))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad])
    @test MOI.eval_objective(evaluator, [1.3]) == ((1.3^2 + 1)^2)^2
    g = [NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.3])
    @test g ≈ [2 * (1.3^2 + 1)^2 * (2 * (1.3^2 + 1)) * 2 * 1.3]
    return
end

function test_constraint_quadratic_univariate()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.add_constraint(model, :($x^2), MOI.LessThan(2.0))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    g = [NaN]
    x_val = [1.2]
    MOI.eval_constraint(evaluator, g, x_val)
    @test g == x_val .^ 2
    @test MOI.jacobian_structure(evaluator) == [(1, 1)]
    J = [NaN]
    MOI.eval_constraint_jacobian(evaluator, J, x_val)
    @test J == 2 .* x_val
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, x_val, 0.0, [1.5])
    @test H == 1.5 .* [2.0]
    return
end

function test_constraint_quadratic_multivariate()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.add_constraint(model, :($x^2 + $x * $y + $y^2), MOI.LessThan(2.0))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    g = [NaN]
    x_val = [1.2, 2.3]
    MOI.eval_constraint(evaluator, g, x_val)
    @test g == [x_val[1]^2 + x_val[1] * x_val[2] + x_val[2]^2]
    @test MOI.jacobian_structure(evaluator) == [(1, 1), (1, 2)]
    J = [NaN, NaN]
    MOI.eval_constraint_jacobian(evaluator, J, x_val)
    @test J == [2 * x_val[1] + x_val[2], x_val[1] + 2 * x_val[2]]
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, x_val, 0.0, [1.5])
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    return
end

function test_constraint_quadratic_multivariate_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    ex = Nonlinear.add_expression(model, :($x^2))
    ey = Nonlinear.add_expression(model, :($y^2))
    exy = Nonlinear.add_expression(model, :($ex + $x * $y))
    Nonlinear.add_constraint(model, :($exy + $ey), MOI.LessThan(2.0))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    g = [NaN]
    x_val = [1.2, 2.3]
    MOI.eval_constraint(evaluator, g, x_val)
    @test g ≈ [x_val[1]^2 + x_val[1] * x_val[2] + x_val[2]^2]
    # Jacobian
    @test MOI.jacobian_structure(evaluator) == [(1, 1), (1, 2)]
    J = [NaN, NaN]
    MOI.eval_constraint_jacobian(evaluator, J, x_val)
    @test J ≈ [2 * x_val[1] + x_val[2], x_val[1] + 2 * x_val[2]]
    # Jv
    y, w = [NaN], [1.1, 2.2]
    MOI.eval_constraint_jacobian_product(evaluator, y, x_val, w)
    @test y ≈ [(2 * x_val[1] + x_val[2]) (x_val[1] + 2 * x_val[2])] * w
    # v'J
    y, w = [NaN, NaN], [1.1]
    MOI.eval_constraint_jacobian_transpose_product(evaluator, y, x_val, w)
    wJ = w' * [(2 * x_val[1] + x_val[2]) (x_val[1] + 2 * x_val[2])]
    @test y ≈ wJ[:]
    # Hessian-lagrangian
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, x_val, 0.0, [1.5])
    @test H ≈ 1.5 .* [2.0, 2.0, 1.0]
    return
end

function test_hessian_sparsity_registered_function()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    f(x, y) = x^2 + y^2
    function ∇f(g, x...)
        g[1] = 2x[1]
        g[2] = 2x[2]
        return
    end
    function ∇²f(H, x...)
        H[1, 1] = 2
        H[2, 2] = 2
        return
    end
    model = Nonlinear.Model()
    Nonlinear.register_operator(model, :f, 2, f, ∇f, ∇²f)
    Nonlinear.set_objective(model, :(f($x, $z) + $y^2))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y, z])
    @test :Hess in MOI.features_available(evaluator)
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (3, 3), (3, 1)]
    H = fill(NaN, 4)
    MOI.eval_hessian_lagrangian(evaluator, H, rand(3), 1.5, Float64[])
    @test H == 1.5 .* [2.0, 2.0, 2.0, 0.0]
    return
end

function test_hessian_sparsity_registered_rosenbrock()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    f(x...) = (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
    function ∇f(g, x...)
        g[1] = 400 * x[1]^3 - 400 * x[1] * x[2] + 2 * x[1] - 2
        g[2] = 200 * (x[2] - x[1]^2)
        return
    end
    function ∇²f(H, x...)
        H[1, 1] = 1200 * x[1]^2 - 400 * x[2] + 2
        H[2, 1] = -400 * x[1]
        H[2, 2] = 200.0
        return
    end
    model = Nonlinear.Model()
    Nonlinear.register_operator(model, :rosenbrock, 2, f, ∇f, ∇²f)
    Nonlinear.set_objective(model, :(rosenbrock($x, $y)))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    @test :Hess in MOI.features_available(evaluator)
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1), (2, 2), (2, 1)]
    H = fill(NaN, 3)
    MOI.eval_hessian_lagrangian(evaluator, H, [1.0, 1.0], 1.5, Float64[])
    @test H == 1.5 .* [802, 200, -400]
    return
end

struct _ColoringGraph
    num_vertices::Int
    edges::Vector{Tuple{Int,Int}}
end

function to_adjlist(graph::_ColoringGraph)
    I = [i for (i, _) in graph.edges]
    J = [j for (_, j) in graph.edges]
    return Coloring.UndirectedGraph(I, J, graph.num_vertices)
end

function test_coloring_edge_free_graph()
    graph = _ColoringGraph(10, [])
    _, numcolors = Coloring.acyclic_coloring(to_adjlist(graph))
    @test numcolors == 1
    return
end

function test_coloring_one_edge_graph()
    graph = _ColoringGraph(10, [(2, 4)])
    color, numcolors = Coloring.acyclic_coloring(to_adjlist(graph))
    @test numcolors == 2
    @test color[2] != color[4]
    return
end

function test_coloring_two_edge_graph()
    graph = _ColoringGraph(10, [(2, 4), (2, 3)])
    color, numcolors = Coloring.acyclic_coloring(to_adjlist(graph))
    @test numcolors == 2
    @test color[3] == color[4]
    return
end

function test_coloring_three_edge_graph()
    graph = _ColoringGraph(10, [(2, 4), (2, 3), (3, 4)])
    color, numcolors = Coloring.acyclic_coloring(to_adjlist(graph))
    @test numcolors == 3
    # TODO: What is this testing?
    Coloring.recovery_preprocess(to_adjlist(graph), color, numcolors, Int[])
    return
end

function test_coloring_two_edge_three_vertex_graph()
    graph = _ColoringGraph(3, [(1, 3), (2, 3)])
    _, numcolors = Coloring.acyclic_coloring(to_adjlist(graph))
    @test numcolors == 2
    return
end

function test_coloring_four_edge_four_vertex_graph()
    graph = _ColoringGraph(4, [(1, 2), (2, 3), (3, 4), (4, 1)])
    _, numcolors = Coloring.acyclic_coloring(to_adjlist(graph))
    @test numcolors == 3
    return
end

function test_coloring_topological_sort()
    # graph = _ColoringGraph(6, [(1, 2), (1, 3), (1, 6), (2, 4), (2, 5)])
    vec = [3, 6, 2, 1, 4, 5, 1, 2, 2, 1]
    offset = [1, 4, 7, 8, 9, 10, 11]
    v = Coloring.reverse_topological_sort_by_dfs(vec, offset, 6, zeros(Int, 6))
    @test v[1] == [3, 6, 4, 5, 2, 1]
    @test v[2] == [0, 1, 1, 2, 2, 1]
    return
end

function test_coloring_end_to_end_hessian_coloring_and_recovery()
    I, J, rinfo = Coloring.hessian_color_preprocess(Set([(1, 2)]), 2)
    R = Coloring.seed_matrix(rinfo)
    Coloring.prepare_seed_matrix!(R, rinfo)
    @test I == [1, 2, 2]
    @test J == [1, 2, 1]
    @test R == [1.0 0.0; 0.0 1.0]
    hess = [3.4 2.1; 2.1 1.3]
    matmat = hess * R
    V = zeros(3)
    Coloring.recover_from_matmat!(V, matmat, rinfo, zeros(3))
    @test V == [3.4, 1.3, 2.1]
    return
end

function test_derivatives()
    a = MOI.VariableIndex(1)
    b = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(sin($a^2) + cos($b * 4) / 5 - 2.0))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [a, b])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [2.0, 3.0]
    @test MOI.eval_objective(evaluator, x) ==
          sin(x[1]^2) + cos(x[2] * 4) / 5 - 2.0
    g = [NaN, NaN]
    MOI.eval_objective_gradient(evaluator, g, x)
    @test g ≈ [2 * x[1] * cos(x[1]^2), -4 * sin(x[2] * 4) / 5]
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1), (2, 2)]
    H = [NaN, NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, x, 1.5, Float64[])
    H_exact = [
        -4 * x[1]^2 * sin(x[1]^2) + 2 * cos(x[1]^2),
        -4 / 5 * 4 * cos(x[2] * 4),
    ]
    @test H == 1.5 .* H_exact
    return
end

function test_NLPBlockData()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    Nonlinear.add_constraint(model, :($x - 1), MOI.LessThan(0.0))
    Nonlinear.add_constraint(model, :($x - 2), MOI.GreaterThan(0.0))
    Nonlinear.add_constraint(model, :($x - 3), MOI.EqualTo(0.0))
    Nonlinear.add_constraint(model, :($x), MOI.Interval(4.0, 5.0))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    block = MOI.NLPBlockData(evaluator)
    @test block.constraint_bounds == [
        MOI.NLPBoundsPair(-Inf, 0.0),
        MOI.NLPBoundsPair(0.0, Inf),
        MOI.NLPBoundsPair(0.0, 0.0),
        MOI.NLPBoundsPair(4.0, 5.0),
    ]
    @test block.has_objective == false
    return
end

function test_linearity()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    variables = Dict(x => 1, y => 2, z => 3)
    function _test_linearity(input, test_value, IJ = [], indices = [])
        model = Nonlinear.Model()
        ex = Nonlinear.add_expression(model, input)
        expr = model[ex]
        adj = Nonlinear.adjacency_matrix(expr.nodes)
        nodes = ReverseAD._replace_moi_variables(expr.nodes, variables)
        ret = ReverseAD._classify_linearity(nodes, adj, ReverseAD.Linearity[])
        @test ret[1] == test_value
        indexed_set = Coloring.IndexedSet(100)
        edge_list = ReverseAD._compute_hessian_sparsity(
            nodes,
            adj,
            ret,
            indexed_set,
            Set{Tuple{Int,Int}}[],
            Vector{Int}[],
        )
        if ret[1] != ReverseAD.NONLINEAR
            @test length(edge_list) == 0
        elseif length(IJ) > 0
            @test IJ == edge_list
        end
        if length(indices) > 0
            empty!(indexed_set)
            ReverseAD._compute_gradient_sparsity!(indexed_set, nodes)
            ix = sort(collect(indexed_set))
            @test indices == ix
        end
        return
    end
    _test_linearity(
        :(sin($x^2) + cos($y * 4) - 2.0),
        ReverseAD.NONLINEAR,
        Set([(2, 2), (1, 1)]),
        [1, 2],
    )
    _test_linearity(:(3 * 4 * ($x + $y)), ReverseAD.LINEAR)
    _test_linearity(
        :($z * $y),
        ReverseAD.NONLINEAR,
        Set([(3, 2), (3, 3), (2, 2)]),
        [2, 3],
    )
    _test_linearity(:(3 + 4), ReverseAD.CONSTANT)
    _test_linearity(:(sin(3) + $x), ReverseAD.LINEAR)
    _test_linearity(
        :(cos($z) * sin(3) + $x),
        ReverseAD.NONLINEAR,
        Set([(3, 3)]),
        [1, 3],
    )
    _test_linearity(:($x - 3 * $y), ReverseAD.LINEAR)
    _test_linearity(:(-$x), ReverseAD.LINEAR)
    _test_linearity(:(+$x), ReverseAD.LINEAR)
    _test_linearity(
        :($x^$y),
        ReverseAD.NONLINEAR,
        Set([(2, 2), (1, 1), (2, 1)]),
        [1, 2],
    )
    _test_linearity(:($x / 3 + $y), ReverseAD.LINEAR)
    _test_linearity(
        :(3 / ($x * $y)),
        ReverseAD.NONLINEAR,
        Set([(2, 2), (1, 1), (2, 1)]),
        [1, 2],
    )
    _test_linearity(:(1 / ($x + 3)), ReverseAD.NONLINEAR, Set([(1, 1)]), [1])
    _test_linearity(
        :(ifelse($x <= 1, $x, $y)),
        ReverseAD.PIECEWISE_LINEAR,
        Set([]),
        [],
    )
    _test_linearity(
        :(ifelse($x <= 1, $x^2, $y)),
        ReverseAD.NONLINEAR,
        Set([(1, 1)]),
        [1, 2],
    )
    _test_linearity(:(ifelse(1 <= 1, 2, 3)), ReverseAD.CONSTANT)
    _test_linearity(
        :(1 / ifelse($x < 1, $x, 0)),
        ReverseAD.NONLINEAR,
        Set([(1, 1)]),
        [1],
    )
    return
end

function test_dual_forward()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    function _test_dual_forward(input, x_input, test_value)
        model = Nonlinear.Model()
        Nonlinear.set_objective(model, input)
        evaluator = Nonlinear.Evaluator(
            model,
            Nonlinear.SparseReverseMode(),
            MOI.VariableIndex[x, y],
        )
        MOI.initialize(evaluator, [:Grad])
        ∇f = fill(NaN, 2)
        MOI.eval_objective_gradient(evaluator, ∇f, x_input)
        @test isapprox(∇f, test_value, atol = 1e-10)
        return
    end
    _test_dual_forward(
        :(sin($x^2) + cos($y * 4) / 5 - 2.0),
        [1.0, 2.0],
        [1.0806046117362795, -0.7914865972987055],
    )
    _test_dual_forward(
        :(sin($x^$y) + cos($y * 4) / 5 - 2.0),
        [1.0, 2.0],
        [1.0806046117362795, -0.7914865972987055],
    )
    _test_dual_forward(
        :(sin($x^3) + cos($x * $y * 4) / 5 - 2.0),
        [1.0, 0.0],
        [1.6209069176044193, 0.0],
    )
    _test_dual_forward(
        :($x * $y),
        [3.427139283036299e-206, 1.0],
        [1.0, 3.427139283036299e-206],
    )
    return
end

function test_gradient_registered_function()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    model = Nonlinear.Model()
    f(x, y) = (1 / 3) * y^3 - 2x^2
    function ∇f(g, x, y)
        g[1] = -4x
        g[2] = y^2
        return
    end
    Nonlinear.register_operator(model, :Φ, 2, f, ∇f)
    Nonlinear.register_operator(model, :c, 1, cos, x -> -sin(x), x -> -cos(x))
    Nonlinear.set_objective(model, :(Φ($y, $x - 1) * c($z)))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y, z],
    )
    MOI.initialize(evaluator, [:Grad])
    ∇f = fill(NaN, 3)
    x_input = [2.0, 3.0, 4.0]
    @test MOI.eval_objective(evaluator, x_input) == f(3.0, 2.0 - 1) * cos(4.0)
    MOI.eval_objective_gradient(evaluator, ∇f, [2.0, 3.0, 4.0])
    @test ∇f ≈ [
        cos(x_input[3]) * (x_input[1] - 1)^2,
        -4cos(x_input[3]) * x_input[2],
        -sin(x_input[3]) * f(x_input[2], x_input[1] - 1),
    ]
    return
end

function test_gradient_jump_855()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(
        model,
        :(ifelse($x <= 3.0, ($x - 2.0)^2, 2 * log($x - 2.0) + 1.0)),
    )
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(evaluator, [:Grad])
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(evaluator, ∇f, [-1.0])
    @test ∇f ≈ [-6.0]
    MOI.eval_objective_gradient(evaluator, ∇f, [2.0])
    @test ∇f ≈ [0.0]
    return
end

function test_gradient_abs()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(abs($x)))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(evaluator, [:Grad])
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(evaluator, ∇f, [2.0])
    @test ∇f ≈ [1.0]
    MOI.eval_objective_gradient(evaluator, ∇f, [-2.0])
    @test ∇f ≈ [-1.0]
    return
end

function test_gradient_trig()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(sin($x^2) + cos($y * 4) / 5 - 2.0))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y],
    )
    MOI.initialize(evaluator, [:Grad])
    ∇f = fill(NaN, 2)
    MOI.eval_objective_gradient(evaluator, ∇f, [2.0, 3.0])
    @test ∇f ≈ [2 * 2.0 * cos(2.0^2), -4 * sin(3.0 * 4) / 5]
    return
end

function test_gradient_logical()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :($x > 0.5 && $x < 0.9))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(evaluator, [:Grad])
    @test MOI.eval_objective(evaluator, [1.5]) == 0.0
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(evaluator, ∇f, [1.5])
    @test iszero(∇f)
    return
end

function test_gradient_ifelse()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(ifelse($x >= 0.5 || $x < 0.1, $x, 5)))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(evaluator, [:Grad])
    @test MOI.eval_objective(evaluator, [1.5]) == 1.5
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(evaluator, ∇f, [1.5])
    @test ∇f ≈ [1.0]
    @test MOI.eval_objective(evaluator, [-0.1]) == -0.1
    MOI.eval_objective_gradient(evaluator, ∇f, [-0.1])
    @test ∇f ≈ [1.0]
    @test MOI.eval_objective(evaluator, [0.2]) == 5
    MOI.eval_objective_gradient(evaluator, ∇f, [0.2])
    @test ∇f ≈ [0.0]
    return
end

function test_gradient_sqrt_nan()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(sqrt($x)))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(evaluator, [:Grad])
    @test isnan(MOI.eval_objective(evaluator, [-1.5]))
    ∇f = fill(Inf, 1)
    MOI.eval_objective_gradient(evaluator, ∇f, [-1.5])
    @test isnan(∇f[1])
    return
end

function test_gradient_variable_power()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :((1 / $x)^$y - $z))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y, z],
    )
    MOI.initialize(evaluator, [:Grad])
    x_input = [2.5, 3.5, 1.0]
    @test MOI.eval_objective(evaluator, x_input) == (1 / 2.5)^3.5 - 1.0
    ∇f = fill(Inf, 3)
    MOI.eval_objective_gradient(evaluator, ∇f, x_input)
    @test ∇f ≈ [
        -x_input[2] * x_input[1]^(-x_input[2] - 1),
        -((1 / x_input[1])^x_input[2]) * log(x_input[1]),
        -1,
    ]
    return
end

function test_single_parameter()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    p = Nonlinear.add_parameter(model, 105.2)
    Nonlinear.set_objective(model, :($p))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(evaluator, [:Grad])
    @test MOI.eval_objective(evaluator, [-0.1]) == 105.2
    return
end

function test_gradient_nested_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    ex1 = Nonlinear.add_expression(model, :(sin($x^2) + cos($y * 4) / 5 - 2.0))
    ex2 = Nonlinear.add_expression(model, :($ex1))
    Nonlinear.set_objective(model, ex2)
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y],
    )
    MOI.initialize(evaluator, [:Grad])
    ∇f = fill(NaN, 2)
    MOI.eval_objective_gradient(evaluator, ∇f, [2.0, 3.0])
    @test ∇f ≈ [2 * 2.0 * cos(2.0^2), -4 * sin(3.0 * 4) / 5]
    return
end

function _dense_hessian(hessian_sparsity, V, n)
    I = [i for (i, _) in hessian_sparsity]
    J = [j for (_, j) in hessian_sparsity]
    raw = SparseArrays.sparse(I, J, V, n, n)
    return Matrix(
        raw + raw' -
        SparseArrays.sparse(LinearAlgebra.diagm(0 => LinearAlgebra.diag(raw))),
    )
end

# This covers the code that computes Hessians in odd chunks of Hess-vec
# products.
function test_odd_chunks_Hessian_products()
    for i in 1:18
        _test_odd_chunks_Hessian_products(i)
    end
    return
end

function _test_odd_chunks_Hessian_products(N)
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:N)
    Nonlinear.set_objective(model, Expr(:call, :*, x...))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), x)
    MOI.initialize(evaluator, [:Hess])
    hessian_sparsity = MOI.hessian_lagrangian_structure(evaluator)
    V = zeros(length(hessian_sparsity))
    values = ones(N)
    MOI.eval_hessian_lagrangian(evaluator, V, values, 1.0, Float64[])
    H = _dense_hessian(hessian_sparsity, V, N)
    @test H ≈ (ones(N, N) - LinearAlgebra.diagm(0 => ones(N)))
    values[1] = 0.5
    MOI.eval_hessian_lagrangian(evaluator, V, values, 1.0, Float64[])
    H = _dense_hessian(hessian_sparsity, V, N)
    H_22 = (ones(N - 1, N - 1) - LinearAlgebra.diagm(0 => ones(N - 1))) / 2
    @test H ≈ [0 ones(N - 1)'; ones(N - 1) H_22]
    return
end

end  # module

TestReverseAD.runtests()
