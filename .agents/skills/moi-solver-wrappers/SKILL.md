---
name: moi-solver-wrappers
description: Use when implementing or reviewing a Julia solver wrapper for MathOptInterface (MOI), especially supports_constraint, custom cone bridges, and MatrixOfConstraints.
---

# Constraint support in MOI solver wrappers

Implement `MOI.supports_constraint(optimizer, F, S)` for the function-in-set
pairs the underlying solver handles natively. When support depends on the
selected algorithm, report the capabilities of that configuration. Leave
unsupported pairs to MOI's bridge system.

Choose a set that matches the solver's cone convention, including coordinate
order and scaling. If no existing MOI set matches, define a solver-specific
set and a custom bridge from the standard set. Return the bridge type from
`MOI.get(optimizer, MOI.Bridges.ListOfNonstandardBridges{T}())`. SCS.jl uses
this pattern for the triangular ordering of scaled PSD matrices. ECOS.jl
uses it to swap the second and third coordinates of the exponential cone.

With `MOI.Utilities.MatrixOfConstraints`, configure the cache's product of sets
to include the solver-specific set. The bridge transforms each affine function
before the cache assembles its coefficients and constants into cone blocks.
The wrapper can then pass the assembled data to the solver in its native row
order. Deferring the conversion to the solver call would require reordering or
rescaling those rows after assembly.

Keeping reformulations in bridges lets users inspect the chosen path with
`JuMP.print_active_bridges(model)`. The bridge also maps results such as
constraint duals back to the original formulation. Improvements to bridge
selection and result mapping then benefit every wrapper that uses them.

When testing support, query the raw optimizer for the function-in-set pair.
Use a bridged model to test the user's formulation and its returned dual.
If a reformulation is missing, implement it as a reusable MOI bridge.

See the [solver-wrapper tutorial](../../../docs/src/tutorials/implementing.md#define-supports_constraint),
the [constraint bridge tutorial](../../../docs/src/tutorials/bridging_constraint.md),
and the [MatrixOfConstraints guide](../../../docs/src/submodules/Utilities/overview.md#utilitiesmatrixofconstraints).
