```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

## [Utilities.Model](@id reference_utilities_model)

```@docs
Utilities.Model
```

## [Utilities.UniversalFallback](@id reference_utilities_universal_fallback)

```@docs
Utilities.UniversalFallback
```

## [Utilities.@model](@id reference_model_macro)

```@docs
Utilities.@model
Utilities.GenericModel
Utilities.GenericOptimizer
```

### `.objective`

```@docs
Utilities.ObjectiveContainer
```

### `.variables`

```@docs
Utilities.VariablesContainer
Utilities.FreeVariables
```

### `.constraints`

```@docs
Utilities.VectorOfConstraints
Utilities.StructOfConstraints
Utilities.@struct_of_constraints_by_function_types
Utilities.@struct_of_constraints_by_set_types
Utilities.struct_of_constraint_code
```

## Caching optimizer

```@docs
Utilities.CachingOptimizer
Utilities.CachingOptimizerState
Utilities.NO_OPTIMIZER
Utilities.EMPTY_OPTIMIZER
Utilities.ATTACHED_OPTIMIZER
Utilities.state
Utilities.CachingOptimizerMode
Utilities.AUTOMATIC
Utilities.MANUAL
Utilities.mode
Utilities.attach_optimizer
Utilities.reset_optimizer
Utilities.drop_optimizer
```

## Mock optimizer

```@docs
Utilities.MockOptimizer
```

## Printing

```@docs
Utilities.latex_formulation
```

## Copy utilities

```@docs
Utilities.default_copy_to
Utilities.IndexMap
Utilities.identity_index_map
Utilities.ModelFilter
Utilities.loadfromstring!
```

## Penalty relaxation

```@docs
Utilities.PenaltyRelaxation
Utilities.ScalarPenaltyRelaxation
```

## MatrixOfConstraints

```@docs
Utilities.MatrixOfConstraints
```

### `.coefficients`

```@docs
Utilities.add_column
Utilities.allocate_terms
Utilities.set_number_of_rows
Utilities.load_terms
Utilities.final_touch
Utilities.extract_function
```

```@docs
Utilities.MutableSparseMatrixCSC
Utilities.AbstractIndexing
Utilities.ZeroBasedIndexing
Utilities.OneBasedIndexing
```

### `.constants`

```@docs
Utilities.load_constants
Utilities.function_constants
Utilities.set_from_constants
Utilities.modify_constants
```

```@docs
Utilities.Hyperrectangle
```

### `.sets`

```@docs
Utilities.set_index
Utilities.set_types
Utilities.add_set
Utilities.rows
Utilities.num_rows
Utilities.set_with_dimension
```

```@docs
Utilities.ProductOfSets
Utilities.MixOfScalarSets
Utilities.@mix_of_scalar_sets
Utilities.OrderedProductOfSets
Utilities.@product_of_sets
```

## Fallbacks

```@docs
Utilities.get_fallback
```

## Function utilities

The following utilities are available for functions:

```@docs
Utilities.eval_variables
Utilities.map_indices
Utilities.substitute_variables
Utilities.filter_variables
Utilities.remove_variable
Utilities.all_coefficients
Utilities.unsafe_add
Utilities.isapprox_zero
Utilities.modify_function
Utilities.zero_with_output_dimension
```

The following functions can be used to canonicalize a function:

```@docs
Utilities.is_canonical
Utilities.canonical
Utilities.canonicalize!
```

The following functions can be used to manipulate functions with basic algebra:

```@docs
Utilities.scalar_type
Utilities.scalarize
Utilities.eachscalar
Utilities.promote_operation
Utilities.operate
Utilities.operate!
Utilities.operate_output_index!
Utilities.vectorize
```

## Constraint utilities

The following utilities are available for moving the function constant to the
set for scalar constraints:

```@docs
Utilities.shift_constant
Utilities.supports_shift_constant
Utilities.normalize_and_add_constraint
Utilities.normalize_constant
```

The following utility identifies those constraints imposing bounds on a given
variable, and returns those bound values:
```@docs
Utilities.get_bounds
```

The following utilities are useful when working with symmetric matrix cones.
```@docs
Utilities.is_diagonal_vectorized_index
Utilities.side_dimension_for_vectorized_dimension
```

## Set utilities

The following utilities are available for sets:

```@docs
Utilities.AbstractDistance
Utilities.ProjectionUpperBoundDistance
Utilities.distance_to_set
Utilities.set_dot
```

## DoubleDicts

```@docs
Utilities.DoubleDicts.DoubleDict
Utilities.DoubleDicts.DoubleDictInner
Utilities.DoubleDicts.IndexDoubleDict
Utilities.DoubleDicts.IndexDoubleDictInner
Utilities.DoubleDicts.outer_keys
Utilities.DoubleDicts.nonempty_outer_keys
```
