```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Nonlinear programming

## Types
```@docs
AbstractNLPEvaluator
NLPBoundsPair
NLPBlockData
```

## Attributes

```@docs
NLPBlock
NLPBlockDual
NLPBlockDualStart
```

## Functions

```@docs
initialize
features_available
eval_objective
eval_constraint
eval_objective_gradient
jacobian_structure
hessian_lagrangian_structure
eval_constraint_jacobian
eval_constraint_jacobian_product
eval_constraint_jacobian_transpose_product
eval_hessian_lagrangian
eval_hessian_lagrangian_product
objective_expr
constraint_expr
```
