# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

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
μ = rand(m)
σ = 0.0
v = rand(n)
@time MOI.eval_hessian_lagrangian(evaluator, H, v, σ, μ)

using BenchmarkTools
@benchmark MOI.eval_hessian_lagrangian($evaluator, $H, $v, $σ, $μ) seconds = 100
