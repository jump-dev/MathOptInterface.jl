# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# See https://github.com/jump-dev/MathOptInterface.jl/issues/321,
#     https://github.com/jump-dev/MathOptInterface.jl/pull/323 and
#     https://github.com/jump-dev/MathOptInterface.jl/pull/390

using BenchmarkTools
import MathOptInterface as MOI
import MathOptInterface.Utilities as MOIU

MOIU.@model Model () (MOI.Interval,) () () () (MOI.ScalarAffineFunction,) () ()
optimizer = MOIU.MockOptimizer(Model{Float64}())
caching_optimizer = MOIU.CachingOptimizer(Model{Float64}(), optimizer)
MOIU.reset_optimizer(caching_optimizer) # detach optimizer
v = MOI.add_variables(caching_optimizer, 2)
cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0, 0.0], v), 0.0)
c = MOI.add_constraint(caching_optimizer, cf, MOI.Interval(-Inf, 1.0))
@btime MOI.set(
    $caching_optimizer,
    $(MOI.ConstraintSet()),
    $c,
    $(MOI.Interval(0.0, 2.0)),
)
MOIU.attach_optimizer(caching_optimizer)
@btime MOI.set(
    $caching_optimizer,
    $(MOI.ConstraintSet()),
    $c,
    $(MOI.Interval(0.0, 2.0)),
)
@btime MOI.set(
    $(caching_optimizer.model_cache),
    $(MOI.ConstraintSet()),
    $c,
    $(MOI.Interval(0.0, 2.0)),
)
@btime MOI.set(
    $(caching_optimizer.optimizer),
    $(MOI.ConstraintSet()),
    $(caching_optimizer.model_to_optimizer_map[c]),
    $(MOI.Interval(0.0, 2.0)),
)
