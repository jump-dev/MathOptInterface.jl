module TestReverseAD

using Test
import MathOptInterface

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
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :($x^2 + 1))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(data, [1.2]) == 1.2^2 + 1
    g = [NaN]
    MOI.eval_objective_gradient(data, g, [1.2])
    @test g == [2.4]
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_lagrangian(data, H, [1.2], 1.5, Float64[])
    @test H == 1.5 .* [2.0]
    MOI.eval_hessian_lagrangian_product(data, H, [1.2], [1.2], 1.5, Float64[])
    @test H == [1.5 * 2.0 * 1.2]
    return
end

function test_objective_quadratic_multivariate()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :($x^2 + $x * $y + $y^2))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(data, [1.2, 2.3]) == 1.2^2 + 1.2 * 2.3 + 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(data, g, [1.2, 2.3])
    @test g == [2 * 1.2 + 2.3, 1.2 + 2 * 2.3]
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(data, H, [1.2, 2.3], 1.5, Float64[])
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    v = [0.3, 0.4]
    hv = [NaN, NaN]
    MOI.eval_hessian_lagrangian_product(data, hv, [1.2, 2.3], v, 1.5, Float64[])
    @test hv ≈ 1.5 .* [2 1; 1 2] * v
    return
end

function test_objective_quadratic_multivariate_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    ex = Nonlinear.add_expression(data, :($x^2))
    ey = Nonlinear.add_expression(data, :($y^2))
    exy = Nonlinear.add_expression(data, :($ex + $x * $y))
    Nonlinear.set_objective(data, :($exy + $ey))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(data, [1.2, 2.3]) == 1.2^2 + 1.2 * 2.3 + 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(data, g, [1.2, 2.3])
    @test g == [2 * 1.2 + 2.3, 1.2 + 2 * 2.3]
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(data, H, [1.2, 2.3], 1.5, Float64[])
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    v = [0.3, 0.4]
    hv = [NaN, NaN]
    MOI.eval_hessian_lagrangian_product(data, hv, [1.2, 2.3], v, 1.5, Float64[])
    @test hv ≈ 1.5 .* [2 1; 1 2] * v
    return
end

function test_objective_ifelse_comparison()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(ifelse(1 <= $x <= 2, $x^2, $y^2)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(data, [1.2, 2.3]) == 1.2^2
    @test MOI.eval_objective(data, [2.2, 2.3]) == 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(data, g, [1.2, 2.3])
    @test g == [2 * 1.2, 0.0]
    MOI.eval_objective_gradient(data, g, [2.2, 2.3])
    @test g == [0.0, 2 * 2.3]
    return
end

function test_objective_ifelse_logic()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(ifelse(1 <= $x && $x <= 2, $x^2, $y^2)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(data, [1.2, 2.3]) == 1.2^2
    @test MOI.eval_objective(data, [2.2, 2.3]) == 2.3^2
    g = [NaN, NaN]
    MOI.eval_objective_gradient(data, g, [1.2, 2.3])
    @test g == [2 * 1.2, 0.0]
    MOI.eval_objective_gradient(data, g, [2.2, 2.3])
    @test g == [0.0, 2 * 2.3]
    return
end

function test_objective_parameter()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    p = Nonlinear.add_parameter(data, 1.2)
    Nonlinear.set_objective(data, :($p * $x))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(data, [1.3]) == 1.2 * 1.3
    g = [NaN]
    MOI.eval_objective_gradient(data, g, [1.3])
    @test g == [1.2]
    return
end

function test_objective_subexpression()
    data = Nonlinear.NonlinearData()
    x = MOI.VariableIndex(1)
    input = :($x^2 + 1)
    expr = Nonlinear.add_expression(data, input)
    expr_2 = Nonlinear.add_expression(data, :($expr^2))
    Nonlinear.set_objective(data, :($expr_2^2))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x],
    )
    MOI.initialize(data, [:Grad])
    @test MOI.eval_objective(data, [1.3]) == ((1.3^2 + 1)^2)^2
    g = [NaN]
    MOI.eval_objective_gradient(data, g, [1.3])
    @test g ≈ [2 * (1.3^2 + 1)^2 * (2 * (1.3^2 + 1)) * 2 * 1.3]
    return
end

function test_constraint_quadratic_univariate()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    Nonlinear.add_constraint(data, :($x^2 <= 2.0))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    g = [NaN]
    x_val = [1.2]
    MOI.eval_constraint(data, g, x_val)
    @test g == x_val .^ 2 .- 2
    @test MOI.jacobian_structure(data) == [(1, 1)]
    J = [NaN]
    MOI.eval_constraint_jacobian(data, J, x_val)
    @test J == 2 .* x_val
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_lagrangian(data, H, x_val, 0.0, [1.5])
    @test H == 1.5 .* [2.0]
    return
end

function test_constraint_quadratic_multivariate()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    Nonlinear.add_constraint(data, :($x^2 + $x * $y + $y^2 <= 2.0))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    g = [NaN]
    x_val = [1.2, 2.3]
    MOI.eval_constraint(data, g, x_val)
    @test g == [x_val[1]^2 + x_val[1] * x_val[2] + x_val[2]^2] .- 2
    @test MOI.jacobian_structure(data) == [(1, 1), (1, 2)]
    J = [NaN, NaN]
    MOI.eval_constraint_jacobian(data, J, x_val)
    @test J == [2 * x_val[1] + x_val[2], x_val[1] + 2 * x_val[2]]
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(data, H, x_val, 0.0, [1.5])
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    return
end

function test_constraint_quadratic_multivariate_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    ex = Nonlinear.add_expression(data, :($x^2))
    ey = Nonlinear.add_expression(data, :($y^2))
    exy = Nonlinear.add_expression(data, :($ex + $x * $y))
    Nonlinear.add_constraint(data, :($exy + $ey <= 2.0))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    g = [NaN]
    x_val = [1.2, 2.3]
    MOI.eval_constraint(data, g, x_val)
    @test g ≈ [x_val[1]^2 + x_val[1] * x_val[2] + x_val[2]^2] .- 2
    # Jacobian
    @test MOI.jacobian_structure(data) == [(1, 1), (1, 2)]
    J = [NaN, NaN]
    MOI.eval_constraint_jacobian(data, J, x_val)
    @test J ≈ [2 * x_val[1] + x_val[2], x_val[1] + 2 * x_val[2]]
    # Jv
    y, w = [NaN], [1.1, 2.2]
    MOI.eval_constraint_jacobian_product(data, y, x_val, w)
    @test y ≈ [(2 * x_val[1] + x_val[2]) (x_val[1] + 2 * x_val[2])] * w
    # v'J
    y, w = [NaN, NaN], [1.1]
    MOI.eval_constraint_jacobian_transpose_product(data, y, x_val, w)
    wJ = w' * [(2 * x_val[1] + x_val[2]) (x_val[1] + 2 * x_val[2])]
    @test y ≈ wJ[:]
    # Hessian-lagrangian
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_lagrangian(data, H, x_val, 0.0, [1.5])
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
    data = Nonlinear.NonlinearData()
    Nonlinear.register_operator(data, :f, 2, f, ∇f, ∇²f)
    Nonlinear.set_objective(data, :(f($x, $z) + $y^2))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y, z],
    )
    @test_broken :Hess in MOI.features_available(data)
    # MOI.initialize(data, [:Grad, :Jac, :Hess])
    # @test MOI.hessian_lagrangian_structure(data) ==
    #       [(1, 1), (2, 2), (3, 3), (3, 1)]
    # H = fill(NaN, 4)
    # MOI.eval_hessian_lagrangian(data, H, rand(3), 1.5, Float64[])
    # @test H == 1.5 .* [2.0, 2.0, 2.0, 0.0]
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
    data = Nonlinear.NonlinearData()
    Nonlinear.register_operator(data, :rosenbrock, 2, f, ∇f, ∇²f)
    Nonlinear.set_objective(data, :(rosenbrock($x, $y)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [x, y],
    )
    @test_broken :Hess in MOI.features_available(data)
    # MOI.initialize(data, [:Grad, :Jac, :Hess])
    # @test MOI.hessian_lagrangian_structure(data) == [(1, 1), (2, 2), (2, 1)]
    # H = fill(NaN, 3)
    # MOI.eval_hessian_lagrangian(data, H, [1.0, 1.0], 1.5, Float64[])
    # @test H == 1.5 .* [802, 200, -400]
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
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(sin($a^2) + cos($b * 4) / 5 - 2.0))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        [a, b],
    )
    MOI.initialize(data, [:Grad, :Jac, :Hess])
    x = [2.0, 3.0]
    @test MOI.eval_objective(data, x) == sin(x[1]^2) + cos(x[2] * 4) / 5 - 2.0
    g = [NaN, NaN]
    MOI.eval_objective_gradient(data, g, x)
    @test g ≈ [2 * x[1] * cos(x[1]^2), -4 * sin(x[2] * 4) / 5]
    @test MOI.hessian_lagrangian_structure(data) == [(1, 1), (2, 2)]
    H = [NaN, NaN]
    MOI.eval_hessian_lagrangian(data, H, x, 1.5, Float64[])
    H_exact = [
        -4 * x[1]^2 * sin(x[1]^2) + 2 * cos(x[1]^2),
        -4 / 5 * 4 * cos(x[2] * 4),
    ]
    @test H == 1.5 .* H_exact
    return
end

function test_NLPBlockData()
    data = Nonlinear.NonlinearData()
    x = MOI.VariableIndex(1)
    Nonlinear.add_constraint(data, :($x <= 1))
    Nonlinear.add_constraint(data, :($x >= 2))
    Nonlinear.add_constraint(data, :($x == 3))
    Nonlinear.add_constraint(data, :(4 <= $x <= 5))
    block = MOI.NLPBlockData(data, [x])
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
        data = Nonlinear.NonlinearData()
        ex = Nonlinear.add_expression(data, input)
        expr = data[ex]
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
        data = Nonlinear.NonlinearData()
        Nonlinear.set_objective(data, input)
        Nonlinear.set_differentiation_backend(
            data,
            Nonlinear.SparseReverseMode(),
            MOI.VariableIndex[x, y],
        )
        MOI.initialize(data, [:Grad])
        ∇f = fill(NaN, 2)
        MOI.eval_objective_gradient(data, ∇f, x_input)
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
    data = Nonlinear.NonlinearData()
    f(x, y) = (1 / 3) * y^3 - 2x^2
    function ∇f(g, x, y)
        g[1] = -4x
        g[2] = y^2
        return
    end
    Nonlinear.register_operator(data, :Φ, 2, f, ∇f)
    Nonlinear.register_operator(data, :c, 1, cos, x -> -sin(x), x -> -cos(x))
    Nonlinear.set_objective(data, :(Φ($y, $x - 1) * c($z)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y, z],
    )
    MOI.initialize(data, [:Grad])
    ∇f = fill(NaN, 3)
    x_input = [2.0, 3.0, 4.0]
    @test MOI.eval_objective(data, x_input) == f(3.0, 2.0 - 1) * cos(4.0)
    MOI.eval_objective_gradient(data, ∇f, [2.0, 3.0, 4.0])
    @test ∇f ≈ [
        cos(x_input[3]) * (x_input[1] - 1)^2,
        -4cos(x_input[3]) * x_input[2],
        -sin(x_input[3]) * f(x_input[2], x_input[1] - 1),
    ]
    return
end

function test_gradient_jump_855()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(
        data,
        :(ifelse($x <= 3.0, ($x - 2.0)^2, 2 * log($x - 2.0) + 1.0)),
    )
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(data, [:Grad])
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(data, ∇f, [-1.0])
    @test ∇f ≈ [-6.0]
    MOI.eval_objective_gradient(data, ∇f, [2.0])
    @test ∇f ≈ [0.0]
    return
end

function test_gradient_abs()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(abs($x)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(data, [:Grad])
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(data, ∇f, [2.0])
    @test ∇f ≈ [1.0]
    MOI.eval_objective_gradient(data, ∇f, [-2.0])
    @test ∇f ≈ [-1.0]
    return
end

function test_gradient_trig()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(sin($x^2) + cos($y * 4) / 5 - 2.0))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y],
    )
    MOI.initialize(data, [:Grad])
    ∇f = fill(NaN, 2)
    MOI.eval_objective_gradient(data, ∇f, [2.0, 3.0])
    @test ∇f ≈ [2 * 2.0 * cos(2.0^2), -4 * sin(3.0 * 4) / 5]
    return
end

function test_gradient_logical()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :($x > 0.5 && $x < 0.9))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(data, [:Grad])
    @test MOI.eval_objective(data, [1.5]) == 0.0
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(data, ∇f, [1.5])
    @test iszero(∇f)
    return
end

function test_gradient_ifelse()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(ifelse($x >= 0.5 || $x < 0.1, $x, 5)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(data, [:Grad])
    @test MOI.eval_objective(data, [1.5]) == 1.5
    ∇f = fill(NaN, 1)
    MOI.eval_objective_gradient(data, ∇f, [1.5])
    @test ∇f ≈ [1.0]
    @test MOI.eval_objective(data, [-0.1]) == -0.1
    MOI.eval_objective_gradient(data, ∇f, [-0.1])
    @test ∇f ≈ [1.0]
    @test MOI.eval_objective(data, [0.2]) == 5
    MOI.eval_objective_gradient(data, ∇f, [0.2])
    @test ∇f ≈ [0.0]
    return
end

function test_gradient_sqrt_nan()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :(sqrt($x)))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(data, [:Grad])
    @test isnan(MOI.eval_objective(data, [-1.5]))
    ∇f = fill(Inf, 1)
    MOI.eval_objective_gradient(data, ∇f, [-1.5])
    @test isnan(∇f[1])
    return
end

function test_gradient_variable_power()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    z = MOI.VariableIndex(3)
    data = Nonlinear.NonlinearData()
    Nonlinear.set_objective(data, :((1 / $x)^$y - $z))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y, z],
    )
    MOI.initialize(data, [:Grad])
    x_input = [2.5, 3.5, 1.0]
    @test MOI.eval_objective(data, x_input) == (1 / 2.5)^3.5 - 1.0
    ∇f = fill(Inf, 3)
    MOI.eval_objective_gradient(data, ∇f, x_input)
    @test ∇f ≈ [
        -x_input[2] * x_input[1]^(-x_input[2] - 1),
        -((1 / x_input[1])^x_input[2]) * log(x_input[1]),
        -1,
    ]
    return
end

function test_single_parameter()
    x = MOI.VariableIndex(1)
    data = Nonlinear.NonlinearData()
    p = Nonlinear.add_parameter(data, 105.2)
    Nonlinear.set_objective(data, :($p))
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x],
    )
    MOI.initialize(data, [:Grad])
    @test MOI.eval_objective(data, [-0.1]) == 105.2
    return
end

function test_gradient_nested_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    data = Nonlinear.NonlinearData()
    ex1 = Nonlinear.add_expression(data, :(sin($x^2) + cos($y * 4) / 5 - 2.0))
    ex2 = Nonlinear.add_expression(data, :($ex1))
    Nonlinear.set_objective(data, ex2)
    Nonlinear.set_differentiation_backend(
        data,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y],
    )
    MOI.initialize(data, [:Grad])
    ∇f = fill(NaN, 2)
    MOI.eval_objective_gradient(data, ∇f, [2.0, 3.0])
    @test ∇f ≈ [2 * 2.0 * cos(2.0^2), -4 * sin(3.0 * 4) / 5]
    return
end

end  # module

TestReverseAD.runtests()
