# See https://github.com/JuliaOpt/MathOptInterface.jl/issues/321,
#     https://github.com/JuliaOpt/MathOptInterface.jl/pull/323 and
#     https://github.com/JuliaOpt/MathOptInterface.jl/pull/390

using BenchmarkTools
using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

@MOIU.model Model () (Interval,) () () () (ScalarAffineFunction,) () ()
optimizer = MOIU.MockOptimizer(Model{Float64}())
caching_optimizer = MOIU.CachingOptimizer(Model{Float64}(), optimizer)
MOIU.resetoptimizer!(caching_optimizer) # detach optimizer
v = MOI.addvariables!(caching_optimizer, 2)
cf = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([0.0, 0.0], v), 0.0)
c = MOI.add_constraint(caching_optimizer, cf, MOI.Interval(-Inf, 1.0))
@btime MOI.set!($caching_optimizer, $(MOI.ConstraintSet()), $c, $(MOI.Interval(0.0, 2.0)))
MOIU.attachoptimizer!(caching_optimizer)
@btime MOI.set!($caching_optimizer, $(MOI.ConstraintSet()), $c, $(MOI.Interval(0.0, 2.0)))
@btime MOI.set!($(caching_optimizer.model_cache), $(MOI.ConstraintSet()), $c, $(MOI.Interval(0.0, 2.0)))
@btime MOI.set!($(caching_optimizer.optimizer), $(MOI.ConstraintSet()), $(caching_optimizer.model_to_optimizer_map[c]), $(MOI.Interval(0.0, 2.0)))
