```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
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
hessian_objective_structure
eval_hessian_objective
jacobian_structure
hessian_lagrangian_structure
eval_constraint_jacobian
hessian_constraint_structure
eval_hessian_constraint
eval_constraint_jacobian_product
eval_constraint_jacobian_transpose_product
eval_hessian_lagrangian
eval_hessian_lagrangian_product
objective_expr
constraint_expr
```
