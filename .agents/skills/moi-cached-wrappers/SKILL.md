---
name: moi-cached-wrappers
description: Use when writing or reviewing a MathOptInterface (MOI) solver wrapper for JuMP and deciding which methods to implement, especially for names, ConstraintFunction or ConstraintSet getters, modification, and deletion.
---

# Rely on JuMP's model cache

A regular `JuMP.Model(Optimizer)` uses `MOI.Utilities.CachingOptimizer`, as does
a model built with `JuMP.Model()` and then given a solver with `set_optimizer`.
When choosing which MOI methods to implement, ask what the solver can do
natively.

- **Names.** The cache stores variable and constraint names and handles lookup
  by name. Implement `MOI.VariableName` or `MOI.ConstraintName` in the wrapper
  only when the solver has a native use for those names.
- **Model queries.** The cache answers getters such as `MOI.ConstraintFunction`
  and `MOI.ConstraintSet`. A wrapper field that stores names or copies of
  constraint functions solely to answer getters duplicates the cache. Solve
  results such as `MOI.VariablePrimal` still come from the solver.
- **Modification and deletion.** Expose the in-place edits the solver supports.
  For example, if it can change a coefficient and efficiently re-solve the
  existing problem, implement the corresponding `MOI.modify` method. Leave
  unsupported edits and deletions to the cache. In its default automatic mode,
  it records the edit and reloads the revised problem at the next solve if the
  attached optimizer cannot apply the edit.

`JuMP.direct_model(optimizer)` bypasses this cache. Use a regular JuMP model to
test behavior supplied by the cache and the raw optimizer to test native support.

See the [solver-wrapper tutorial](https://jump.dev/MathOptInterface.jl/stable/tutorials/implementing/#The-big-decision:-incremental-modification?)
and the [CachingOptimizer guide](https://jump.dev/MathOptInterface.jl/stable/submodules/Utilities/overview/#Utilities.CachingOptimizer).
