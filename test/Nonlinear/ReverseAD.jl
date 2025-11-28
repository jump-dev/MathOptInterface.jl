# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestReverseAD

using Test
import LinearAlgebra
import MathOptInterface as MOI
import SparseArrays

import MathOptInterface.Nonlinear
import MathOptInterface.Nonlinear.ReverseAD
import MathOptInterface.Nonlinear.ReverseAD.Coloring

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
    @test MOI.hessian_objective_structure(evaluator) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_objective(evaluator, H, [1.2])
    @test H == [2.0]
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

function test_objective_and_constraints_quadratic_univariate()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :($x^2 + 1))
    Nonlinear.add_constraint(model, :($x^2), MOI.LessThan(2.0))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2]) == 1.2^2 + 1
    g = [NaN]
    MOI.eval_objective_gradient(evaluator, g, [1.2])
    @test g == [2.4]
    @test MOI.hessian_objective_structure(evaluator) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_objective(evaluator, H, [1.2])
    @test H == [2.0]
    @test MOI.hessian_constraint_structure(evaluator, 1) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_constraint(evaluator, H, [1.3], 1)
    @test H == [2.0]
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1), (1, 1)]
    H = [NaN, NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [1.2], 1.5, Float64[1.3])
    @test H == [1.5, 1.3] .* [2.0, 2.0]
    Hp = [NaN]
    MOI.eval_hessian_lagrangian_product(
        evaluator,
        Hp,
        [1.2],
        [1.2],
        1.5,
        Float64[1.3],
    )
    @test Hp == [1.5 * 2.0 * 1.2 + 1.3 * 2.0 * 1.2]
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
    @test MOI.hessian_objective_structure(evaluator) == [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_objective(evaluator, H, [1.2, 2.3])
    @test H == [2.0, 2.0, 1.0]
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
    val = [1.2, 2.3]
    @test MOI.eval_objective(evaluator, val) == 1.2^2 + 1.2 * 2.3 + 2.3^2
    @test 0 == @allocated MOI.eval_objective(evaluator, val)
    g = [NaN, NaN]
    MOI.eval_objective_gradient(evaluator, g, val)
    @test g == [2 * 1.2 + 2.3, 1.2 + 2 * 2.3]
    @test 0 == @allocated MOI.eval_objective_gradient(evaluator, g, val)
    @test MOI.hessian_objective_structure(evaluator) == [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    MOI.eval_hessian_objective(evaluator, H, val)
    @test H == [2.0, 2.0, 1.0]
    @test evaluator.backend.max_chunk == 2
    @test 0 == @allocated MOI.eval_hessian_objective(evaluator, H, val)
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (2, 1)]
    H = [NaN, NaN, NaN]
    μ = Float64[]
    MOI.eval_hessian_lagrangian(evaluator, H, val, 1.5, μ)
    @test 0 == @allocated MOI.eval_hessian_lagrangian(evaluator, H, val, 1.5, μ)
    @test H == 1.5 .* [2.0, 2.0, 1.0]
    v = [0.3, 0.4]
    hv = [NaN, NaN]
    MOI.eval_hessian_lagrangian_product(evaluator, hv, val, v, 1.5, μ)
    @test hv ≈ 1.5 .* [2 1; 1 2] * v
    @test 0 == @allocated MOI.eval_hessian_lagrangian_product(
        evaluator,
        hv,
        val,
        v,
        1.5,
        μ,
    )
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
    @test MOI.hessian_constraint_structure(evaluator, 1) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_constraint(evaluator, H, x_val, 1)
    @test H == [2.0]
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
    # Hessian-Lagrangian
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
        @assert size(H) == (2, 2)
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
    @test MOI.hessian_lagrangian_structure(evaluator) ==
          [(1, 1), (2, 2), (2, 1)]
    H = fill(NaN, 3)
    MOI.eval_hessian_lagrangian(evaluator, H, [1.0, 1.0], 1.5, Float64[])
    @test H == 1.5 .* [802.0, 200.0, -400.0]
    return
end

function test_hessian_registered_error()
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
        # Wrong index. Should be [2, 1]
        H[1, 2] = -400 * x[1]
        H[2, 2] = 200.0
        return
    end
    model = Nonlinear.Model()
    Nonlinear.register_operator(model, :rosenbrock, 2, f, ∇f, ∇²f)
    Nonlinear.set_objective(model, :(rosenbrock($x, $y)))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    H = fill(NaN, 3)
    @test_throws(
        ErrorException("Unable to access upper-triangular component: (1, 2)"),
        MOI.eval_hessian_lagrangian(evaluator, H, [1.0, 1.0], 1.5, Float64[]),
    )
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
    _test_linearity(:($z * $y), ReverseAD.NONLINEAR, Set([(3, 2)]), [2, 3])
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
    _test_linearity(
        :(($x + $y)/$z),
        ReverseAD.NONLINEAR,
        Set([(3, 3), (3, 2), (3, 1)]),
        [1, 2, 3],
    )
    return
end

function test_linearity_no_hess()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    ex = Nonlinear.add_expression(model, :($x + 1))
    Nonlinear.set_objective(model, ex)
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac])
    # We initialized without the need for the hessian so
    # the linearity shouldn't be computed.
    @test only(evaluator.backend.subexpressions).linearity ==
          ReverseAD.NONLINEAR
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

function test_gradient_view()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(($x - 1)^2 + 4 * ($y - $x^2)^2))
    evaluator = Nonlinear.Evaluator(
        model,
        Nonlinear.SparseReverseMode(),
        MOI.VariableIndex[x, y],
    )
    MOI.initialize(evaluator, [:Grad])
    X_input = [0.0; 1.0; 2.0; 3.0]
    for idx in [1:2, 3:4, [1, 3], 4:-2:2]
        x_input = view(X_input, idx)
        x_vec = Vector(x_input)
        ∇f = fill(Inf, 2)
        ∇fv = fill(Inf, 2)
        MOI.eval_objective_gradient(evaluator, ∇f, x_input)
        MOI.eval_objective_gradient(evaluator, ∇fv, x_vec)
        @test ∇f == ∇fv
        ∇f = fill(Inf, 2)
        ∇fv = view(fill(Inf, 4), idx)
        MOI.eval_objective_gradient(evaluator, ∇f, x_input)
        MOI.eval_objective_gradient(evaluator, ∇fv, x_input)
        @test ∇f == ∇fv
    end
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

function _dense_jacobian(jacobian_sparsity, V, m, n)
    I = [i for (i, j) in jacobian_sparsity]
    J = [j for (i, j) in jacobian_sparsity]
    raw = SparseArrays.sparse(I, J, V, m, n)
    return Matrix(raw)
end

function test_jacobians_and_jacvec()
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:3)
    a, b, c = x
    Nonlinear.set_objective(model, :($a * $b + $c^2))
    Nonlinear.add_constraint(model, :($c * $b), MOI.LessThan(1.0))
    Nonlinear.add_constraint(model, :($a^2 / 2), MOI.LessThan(1.0))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), x)
    MOI.initialize(evaluator, [:Jac, :JacVec])
    values = [1.0, 2.0, 3.0] # For a, b, c.
    jacobian_sparsity = MOI.jacobian_structure(evaluator)
    V = zeros(length(jacobian_sparsity))
    MOI.eval_constraint_jacobian(evaluator, V, values)
    correct_jacobian = [0.0 3.0 2.0; 1.0 0.0 0.0]
    @test _dense_jacobian(jacobian_sparsity, V, 2, 3) ≈ correct_jacobian
    v = [2.4, 3.5, 1.2]
    product_storage = zeros(2)
    MOI.eval_constraint_jacobian_product(evaluator, product_storage, values, v)
    @test product_storage ≈ correct_jacobian * v
    w = [0.6, 4.3]
    product_storage = zeros(3)
    MOI.eval_constraint_jacobian_transpose_product(
        evaluator,
        product_storage,
        values,
        w,
    )
    @test product_storage ≈ correct_jacobian' * w
    return
end

function test_jacobians_and_jacvec_with_subexpressions()
    model = Nonlinear.Model()
    x = MOI.VariableIndex.(1:3)
    a, b, c = x
    bc = Nonlinear.add_expression(model, :($b * $c))
    Nonlinear.set_objective(model, :($a * $b + $c^2))
    Nonlinear.add_constraint(model, :($bc), MOI.LessThan(1.0))
    Nonlinear.add_constraint(model, :($a^2 / 2), MOI.LessThan(1.0))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), x)
    MOI.initialize(evaluator, [:Jac, :JacVec])
    values = [1.0, 2.0, 3.0] # For a, b, c.
    jacobian_sparsity = MOI.jacobian_structure(evaluator)
    V = zeros(length(jacobian_sparsity))
    MOI.eval_constraint_jacobian(evaluator, V, values)
    correct_jacobian = [0.0 3.0 2.0; 1.0 0.0 0.0]
    @test _dense_jacobian(jacobian_sparsity, V, 2, 3) ≈ correct_jacobian
    v = [2.4, 3.5, 1.2]
    product_storage = zeros(2)
    MOI.eval_constraint_jacobian_product(evaluator, product_storage, values, v)
    @test product_storage ≈ correct_jacobian * v
    w = [0.6, 4.3]
    product_storage = zeros(3)
    MOI.eval_constraint_jacobian_transpose_product(
        evaluator,
        product_storage,
        values,
        w,
    )
    @test product_storage ≈ correct_jacobian' * w
    return
end

function test_pow_complex_result()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(ifelse($x > 0, $x^1.5, -(-$x)^1.5)))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    x = [-2.0]
    @test MOI.eval_objective(evaluator, x) ≈ -(2^1.5)
    g = [NaN]
    MOI.eval_objective_gradient(evaluator, g, x)
    @test g ≈ [1.5 * 2.0^0.5]
    @test MOI.hessian_lagrangian_structure(evaluator) == [(1, 1)]
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, x, 1.5, Float64[])
    @test H ≈ 1.5 .* [-0.5 * 1.5 / sqrt(2.0)]
    return
end

function test_constraint_gradient()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = Nonlinear.Model()
    Nonlinear.add_constraint(model, :($x^2 + $x * $y + $y^2), MOI.LessThan(2.0))
    Nonlinear.add_constraint(model, :(cos($y)), MOI.LessThan(2.0))
    evaluator =
        Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x, y])
    MOI.initialize(evaluator, [:Grad, :Jac])
    @test MOI.constraint_gradient_structure(evaluator, 1) == [1, 2]
    @test MOI.constraint_gradient_structure(evaluator, 2) == [2]
    x_val = [1.2, 2.3]
    ∇g = [NaN, NaN]
    MOI.eval_constraint_gradient(evaluator, ∇g, x_val, 1)
    @test ∇g ≈ [2 * x_val[1] + x_val[2], x_val[1] + 2 * x_val[2]]
    x_val = [1.2, 2.3]
    ∇g = [NaN]
    MOI.eval_constraint_gradient(evaluator, ∇g, x_val, 2)
    @test ∇g ≈ [-sin(x_val[2])]
    return
end

function test_hessian_length()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(log($x)))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Hess])
    H = Float64[]
    got, want = 0, 1
    @test_throws(
        ErrorException(
            "Vector provided for Hessian storage has too few elements. Got " *
            "$got, want $want.",
        ),
        MOI.eval_hessian_lagrangian(evaluator, H, [1.0], 1.0, [1.0]),
    )
    return
end

function test_jacobian_length()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.add_constraint(model, :(sin($x)), MOI.LessThan(0.5))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Jac])
    J = Float64[]
    @test_throws BoundsError MOI.eval_constraint_jacobian(evaluator, J, [1.0])
    return
end

function test_timers()
    x = MOI.VariableIndex(1)
    model = Nonlinear.Model()
    Nonlinear.set_objective(model, :(log($x)))
    Nonlinear.add_constraint(model, :(sin($x)), MOI.LessThan(0.5))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    y = [1.2]
    g = [NaN]
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    MOI.eval_objective(evaluator, y)
    MOI.eval_constraint(evaluator, g, y)
    MOI.eval_objective_gradient(evaluator, g, y)
    MOI.eval_constraint_gradient(evaluator, g, y, 1)
    J = zeros(length(MOI.jacobian_structure(evaluator)))
    MOI.eval_constraint_jacobian(evaluator, J, y)
    H = zeros(length(MOI.hessian_objective_structure(evaluator)))
    MOI.eval_hessian_objective(evaluator, H, y)
    H = zeros(length(MOI.hessian_constraint_structure(evaluator, 1)))
    MOI.eval_hessian_constraint(evaluator, H, y, 1)
    H = zeros(length(MOI.hessian_lagrangian_structure(evaluator)))
    MOI.eval_hessian_lagrangian(evaluator, H, y, 1.0, [1.0])
    timers = [
        evaluator.initialize_timer,
        evaluator.eval_objective_timer,
        evaluator.eval_constraint_timer,
        evaluator.eval_objective_gradient_timer,
        evaluator.eval_constraint_gradient_timer,
        evaluator.eval_constraint_jacobian_timer,
        evaluator.eval_hessian_objective_timer,
        evaluator.eval_hessian_constraint_timer,
        evaluator.eval_hessian_lagrangian_timer,
    ]
    @test all(>=(0.0), timers)
    if !Sys.iswindows()
        # Windows times only in milliseconds, which can be too coarse to
        # accurately measure and test.
        @test sum(timers) > 0.0
    end
    return
end

function test_varying_length_x()
    model = MOI.Nonlinear.Model()
    x = MOI.VariableIndex(1)
    MOI.Nonlinear.set_objective(model, :(sin($x)))
    evaluator =
        MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, Symbol[:Jac])
    ∇f = [NaN]
    MOI.eval_objective_gradient(evaluator, ∇f, [1.0, 2.0])
    @test length(∇f) == 1
    @test ∇f[1] ≈ cos(1.0)
    return
end

function test_univariate_operator_with_no_second_order()
    f(x::Float64) = x^2
    df(x::Float64) = 2 * x
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.register_operator(model, :op_f, 1, f, df)
    x = MOI.VariableIndex(1)
    MOI.Nonlinear.add_constraint(model, :(op_f($x)), MOI.LessThan(2.0))
    evaluator =
        MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), [x])
    @test !(:Hess in MOI.features_available(evaluator))
    MOI.initialize(evaluator, [:Grad, :Jac])
    J = zeros(length(MOI.jacobian_structure(evaluator)))
    MOI.eval_constraint_jacobian(evaluator, J, [2.0])
    @test J == [4.0]
    return
end

function test_no_objective()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad])
    @test_throws(
        ErrorException("No nonlinear objective."),
        MOI.eval_objective(evaluator, [1.0]),
    )
    g = [0.0]
    @test_throws(
        ErrorException("No nonlinear objective."),
        MOI.eval_objective_gradient(evaluator, g, [1.0]),
    )
    return
end

function test_x_power_1()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    MOI.Nonlinear.set_objective(model, :($x^1))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Hess])
    @test MOI.eval_objective(evaluator, [2.0]) ≈ 2.0
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [2.0], 1.5, Float64[])
    @test H == [0.0]
    return
end

function test_variable_first_node_in_tape()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    expr = MOI.Nonlinear.add_expression(model, :($x))
    MOI.Nonlinear.set_objective(model, :(sin($expr)))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [2.0], 1.5, [])
    @test H ≈ [-1.5 * sin(2.0)]
    return
end

function test_subexpression_first_node_in_tape()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    expr = MOI.Nonlinear.add_expression(model, :($x))
    expr2 = MOI.Nonlinear.add_expression(model, :($expr))
    MOI.Nonlinear.set_objective(model, :(sin($expr2)))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [2.0], 1.5, [])
    @test H ≈ [-1.5 * sin(2.0)]
    return
end

function test_parameter_in_hessian()
    model = Nonlinear.Model()
    x = MOI.VariableIndex(1)
    p = MOI.Nonlinear.add_parameter(model, 3.0)
    MOI.Nonlinear.set_objective(model, :(sin($x + $p)))
    evaluator = Nonlinear.Evaluator(model, Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    H = [NaN]
    MOI.eval_hessian_lagrangian(evaluator, H, [2.0], 1.5, [])
    @test H ≈ [-1.5 * sin(2.0 + 3.0)]
    return
end

function test_unsafe_vector_view()
    x = Float64[]
    GC.@preserve x begin
        view = MOI.Nonlinear.ReverseAD._UnsafeVectorView(x, 3)
        @test length(x) == 3
        view[2] = 1.0
        @test x[2] == 1.0
    end
    return
end

function test_classify_linearity_ifelse()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.set_objective(model, :(ifelse($y, $x, 1)))
    evaluator = MOI.Nonlinear.Evaluator(
        model,
        MOI.Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.2, 1.0]) == 1.2
    @test MOI.eval_objective(evaluator, [1.2, 0.0]) == 1.0
    @test isempty(MOI.hessian_lagrangian_structure(evaluator))
    return
end

function test_classify_linearity_logic()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = MOI.Nonlinear.Model()
    MOI.Nonlinear.set_objective(model, :($x && $y))
    evaluator = MOI.Nonlinear.Evaluator(
        model,
        MOI.Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [1.0, 1.0]) == 1.0
    @test MOI.eval_objective(evaluator, [0.0, 1.0]) == 0.0
    @test MOI.eval_objective(evaluator, [1.0, 0.0]) == 0.0
    @test MOI.eval_objective(evaluator, [0.0, 0.0]) == 0.0
    @test isempty(MOI.hessian_lagrangian_structure(evaluator))
    return
end

function test_hessian_sparsity_with_subexpressions()
    x = MOI.VariableIndex(1)
    y = MOI.VariableIndex(2)
    model = MOI.Nonlinear.Model()
    expr = MOI.Nonlinear.add_expression(model, :($x * $y))
    expr2 = MOI.Nonlinear.add_expression(model, :($expr))
    MOI.Nonlinear.set_objective(model, :(sin($expr2)))
    evaluator = MOI.Nonlinear.Evaluator(
        model,
        MOI.Nonlinear.SparseReverseMode(),
        [x, y],
    )
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    MOI.hessian_lagrangian_structure(evaluator)
    return
end

function test_toposort_subexpressions()
    x = MOI.VariableIndex(1)
    model = MOI.Nonlinear.Model()
    a = MOI.Nonlinear.add_expression(model, :($x))
    b = MOI.Nonlinear.add_expression(model, :($x))
    c = MOI.Nonlinear.add_expression(model, :($a + $b))
    d = MOI.Nonlinear.add_expression(model, :($c + $b))
    MOI.Nonlinear.add_constraint(model, :($d), MOI.LessThan(1.0))
    MOI.Nonlinear.add_constraint(model, :($c), MOI.LessThan(1.0))
    evaluator =
        MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), [x])
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    g = [NaN, NaN]
    MOI.eval_constraint(evaluator, g, [2.0])
    @test g == [6.0, 4.0]
    return
end

function test_eval_user_defined_operator_ForwardDiff_gradient!()
    model = MOI.Nonlinear.Model()
    x = MOI.VariableIndex.(1:4)
    p = MOI.Nonlinear.add_parameter(model, 2.0)
    ex = MOI.Nonlinear.add_expression(model, :($p * $(x[1])))
    ψ(x) = sin(x)
    t(x, y) = x + 3y
    MOI.Nonlinear.register_operator(model, :ψ, 1, ψ)
    MOI.Nonlinear.register_operator(model, :t, 2, t)
    MOI.Nonlinear.add_constraint(
        model,
        :($ex^3 + sin($(x[2])) / ψ($(x[2])) + t($(x[3]), $(x[4]))),
        MOI.LessThan(0.0),
    )
    d = MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), x)
    MOI.initialize(d, [:Jac])
    X = [1.1, 1.2, 1.3, 1.4]
    g = [NaN]
    MOI.eval_constraint(d, g, X)
    @test only(g) ≈ 17.148
    @test MOI.jacobian_structure(d) == [(1, 1), (1, 2), (1, 3), (1, 4)]
    J = [NaN, NaN, NaN, NaN]
    MOI.eval_constraint_jacobian(d, J, X)
    @test J ≈ [2.0^3 * 3.0 * 1.1^2, 0.0, 1.0, 3.0]
    return
end

function test_eval_user_defined_operator_type_mismatch()
    model = MOI.Nonlinear.Model()
    x = MOI.VariableIndex.(1:4)
    p = MOI.Nonlinear.add_parameter(model, 2.0)
    ex = MOI.Nonlinear.add_expression(model, :($p * $(x[1])))
    ψ(x) = sin(x)
    t(x, y) = x + 3y
    function ∇t(ret, x, y)
        ret[1] = 1      # These are intentionally the wrong type
        ret[2] = 3 // 1 # These are intentionally the wrong type
        return
    end
    MOI.Nonlinear.register_operator(model, :ψ, 1, ψ, cos)
    MOI.Nonlinear.register_operator(model, :t, 2, t, ∇t)
    MOI.Nonlinear.add_constraint(
        model,
        :($ex^3 + sin($(x[2])) / ψ($(x[2])) + t($(x[3]), $(x[4]))),
        MOI.LessThan(0.0),
    )
    d = MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), x)
    MOI.initialize(d, [:Jac])
    X = [1.1, 1.2, 1.3, 1.4]
    g = [NaN]
    MOI.eval_constraint(d, g, X)
    @test only(g) ≈ 17.148
    @test MOI.jacobian_structure(d) == [(1, 1), (1, 2), (1, 3), (1, 4)]
    J = [NaN, NaN, NaN, NaN]
    MOI.eval_constraint_jacobian(d, J, X)
    @test J ≈ [2.0^3 * 3.0 * 1.1^2, 0.0, 1.0, 3.0]
    return
end

function test_generate_hessian_slice_inner()
    # Test that it evaluates without error. The code contents are tested
    # elsewhere.
    MOI.Nonlinear.ReverseAD._generate_hessian_slice_inner()
    d = ex = nothing  # These arguments are untyped and not needed for this test
    for id in [0, MOI.Nonlinear.ReverseAD.MAX_CHUNK + 1]
        @test_throws(
            ErrorException("Invalid chunk size: $id"),
            MOI.Nonlinear.ReverseAD._hessian_slice_inner(d, ex, id),
        )
    end
    return
end

function test_hessian_reinterpret_unsafe()
    model = MOI.Nonlinear.Model()
    x = MOI.VariableIndex.(1:5)
    MOI.Nonlinear.add_constraint(
        model,
        :(($(x[1]) + $(x[2])) * $(x[3])),
        MOI.EqualTo(0.0),
    )
    MOI.Nonlinear.add_constraint(model, :($(x[4]) * $(x[5])), MOI.EqualTo(1.0))
    evaluator =
        MOI.Nonlinear.Evaluator(model, MOI.Nonlinear.SparseReverseMode(), x)
    MOI.initialize(evaluator, [:Hess])
    H_s = MOI.hessian_lagrangian_structure(evaluator)
    H = zeros(length(H_s))
    x_v = ones(5)
    MOI.eval_hessian_lagrangian(evaluator, H, x_v, 0.0, [1.0, 1.0])
    @test count(isapprox.(H, 1.0; atol = 1e-8)) == 3
    @test count(isapprox.(H, 0.0; atol = 1e-8)) == 5
    @test sort(H_s[round.(Bool, H)]) == [(3, 1), (3, 2), (5, 4)]
    return
end

function test_IntDisjointSet()
    for case in [
        [(1, 2) => [1, 1, 3], (1, 3) => [1, 1, 1]],
        [(1, 2) => [1, 1, 3], (3, 1) => [1, 1, 1]],
        [(2, 1) => [2, 2, 3], (1, 3) => [2, 2, 2]],
        [(2, 1) => [2, 2, 3], (3, 1) => [3, 2, 3]],
        [(1, 3) => [1, 2, 1], (2, 3) => [1, 2, 2]],
        [(1, 3) => [1, 2, 1], (3, 2) => [1, 1, 1]],
        [(3, 1) => [3, 2, 3], (2, 3) => [3, 3, 3]],
        [(3, 1) => [3, 2, 3], (3, 2) => [3, 3, 3]],
        [(2, 3) => [1, 2, 2], (1, 3) => [1, 2, 1]],
        [(2, 3) => [1, 2, 2], (3, 1) => [2, 2, 2]],
        [(3, 2) => [1, 3, 3], (1, 3) => [3, 3, 3]],
        [(3, 2) => [1, 3, 3], (3, 1) => [3, 3, 3]],
    ]
        S = Coloring._IntDisjointSet(3)
        @test Coloring._find_root!.((S,), [1, 2, 3]) == [1, 2, 3]
        @test S.number_of_trees == 3
        for (i, (union, result)) in enumerate(case)
            Coloring._root_union!(S, union[1], union[2])
            @test Coloring._find_root!.((S,), [1, 2, 3]) == result
            @test S.number_of_trees == 3 - i
        end
    end
    return
end

end  # module

TestReverseAD.runtests()
