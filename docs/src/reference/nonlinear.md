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
jacobian_structure
eval_constraint_gradient
constraint_gradient_structure
eval_constraint_jacobian
eval_constraint_jacobian_product
eval_constraint_jacobian_transpose_product
hessian_lagrangian_structure
hessian_objective_structure
hessian_constraint_structure
eval_hessian_objective
eval_hessian_constraint
eval_hessian_lagrangian
eval_hessian_lagrangian_product
objective_expr
constraint_expr
```
