# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestNonlinearModelWithOracles

using Test
import MathOptInterface as MOI

function _oracle()
    return MOI.VectorNonlinearOracle(;
        dimension = 1,
        l = [0.0],
        u = [4.0],
        eval_f = (y, x) -> (y[1] = x[1]^2),
        jacobian_structure = [(1, 1)],
        eval_jacobian = (J, x) -> (J[1] = 2x[1]),
        hessian_lagrangian_structure = [(1, 1)],
        eval_hessian_lagrangian = (H, x, μ) -> (H[1] = 2μ[1]),
    )
end

function test_moi_model_stack()
    inner = MOI.Nonlinear.Model()
    oracles = MOI.Nonlinear.ModelWithOracles(inner)
    model = MOI.Nonlinear.ModelWithQuad(oracles)
    @test model isa MOI.ModelLike
    @test oracles isa MOI.ModelLike
    @test inner isa MOI.ModelLike
    x = MOI.add_variable(model)
    set = _oracle()
    c = MOI.add_constraint(model, MOI.VectorOfVariables([x]), set)
    @test MOI.supports_constraint(model, MOI.VectorOfVariables, typeof(set))
    @test MOI.get(model, MOI.ConstraintSet(), c) === set
    MOI.set(model, MOI.LagrangeMultiplierStart(), c, [0.5])
    @test MOI.get(model, MOI.LagrangeMultiplierStart(), c) == [0.5]
    f = MOI.ScalarQuadraticFunction(
        [MOI.ScalarQuadraticTerm(2.0, x, x)],
        MOI.ScalarAffineTerm{Float64}[],
        0.0,
    )
    MOI.set(model, MOI.ObjectiveFunction{typeof(f)}(), f)
    MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    evaluator = MOI.Nonlinear.Evaluator(
        model,
        MOI.Nonlinear.SparseReverseMode(),
        [x],
    )
    MOI.initialize(evaluator, [:Grad, :Jac, :Hess])
    @test MOI.eval_objective(evaluator, [2.0]) == -4.0
    g = zeros(1)
    MOI.eval_constraint(evaluator, g, [2.0])
    @test g == [4.0]
end

function test_default_backend_model()
    model = MOI.Nonlinear.model(MOI.Nonlinear.SparseReverseMode())
    @test model isa MOI.Nonlinear.ModelWithQuad
    @test model.inner isa MOI.Nonlinear.ModelWithOracles
    @test model.inner.inner isa MOI.Nonlinear.Model
    return
end

test_moi_model_stack()
test_default_backend_model()

end  # module
