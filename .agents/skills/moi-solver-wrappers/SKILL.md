---
name: moi-solver-wrappers
description: Use when implementing or reviewing a Julia solver wrapper for MathOptInterface (MOI), especially supports_constraint, native cone support, and constraint reformulations.
---

# Constraint support in MOI solver wrappers

Implement `MOI.supports_constraint(optimizer, F, S)` for the function-in-set
pairs the underlying solver handles natively. When support depends on the
selected algorithm, report the capabilities of that configuration. Leave
unsupported pairs to MOI's bridge system.

For example, a solver that accepts positive semidefinite constraints can solve
second-order cone problems through MOI. It should report `false` for
`MOI.SecondOrderCone` if it would have to convert the cone to a PSD matrix.
MOI's default route through a rotated cone turns a three-dimensional SOC into
a 2×2 PSD block. A direct arrow-matrix conversion would use a 3×3 block.

Keeping reformulations in bridges lets users inspect the chosen path with
`JuMP.print_active_bridges(model)`. The bridge also maps results such as
constraint duals back to the original formulation. Improvements to bridge
selection and result mapping then benefit every wrapper that uses them.

When testing support, query the raw optimizer for the function-in-set pair.
Use a bridged model to test the user's formulation and its returned dual.
If a reformulation is missing, implement it as a reusable MOI bridge.

See the [solver-wrapper tutorial](../../../docs/src/tutorials/implementing.md#define-supports_constraint)
and the [constraint bridge tutorial](../../../docs/src/tutorials/bridging_constraint.md).
