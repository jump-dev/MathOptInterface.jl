import Ipopt
import JuMP
import MathOptInterface as MOI
import PGLib
import PowerModels

model = JuMP.direct_model(Ipopt.Optimizer())
pm = PowerModels.instantiate_model(
    PGLib.pglib("pglib_opf_case10000_goc"),
    PowerModels.ACPPowerModel,
    PowerModels.build_opf;
    jump_model = model,
);

ipopt = JuMP.backend(model)
x = MOI.get(ipopt, MOI.ListOfVariableIndices())
m, n = length(ipopt.nlp_model.constraints), length(x)

evaluator = MOI.Nonlinear.Evaluator(
    ipopt.nlp_model,
    MOI.Nonlinear.SparseReverseMode(),
    x,
)
MOI.initialize(evaluator, [:Grad, :Jac, :Hess])

H_struct = MOI.hessian_lagrangian_structure(evaluator)
H = zeros(length(H_struct))
mu = rand(m)
sigma = 0.0
x_v = rand(n)
@time MOI.eval_hessian_lagrangian(evaluator, H, x_v, sigma, mu)

using BenchmarkTools
@benchmark MOI.eval_hessian_lagrangian($evaluator, $H, $x_v, $sigma, $mu) seconds = 100
