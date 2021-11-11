```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface

    # For compatibility with both Julia 1.0.5 and 1.5.2
    # Upon the Julia LTS version becoming 1.6, these imports could be dropped,
    # and all ScalarAffineTerm and VariableIndex instances in doctests below
    # could be replaced with MOI.ScalarAffineTerm and MOI.VariableIndex
    # Check discussion at PR 1184: https://github.com/jump-dev/MathOptInterface.jl/pull/1184#discussion_r515300914
    import MathOptInterface.ScalarAffineTerm
    import MathOptInterface.VariableIndex
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

## Utilities.Model

```@docs
Utilities.Model
```

## Utilities.UniversalFallback

```@docs
Utilities.UniversalFallback
```

## Utilities.@model

```@docs
Utilities.@model
Utilities.GenericModel
Utilities.GenericOptimizer
Utilities.@struct_of_constraints_by_function_types
Utilities.@struct_of_constraints_by_set_types
Utilities.struct_of_constraint_code
```

### Caching optimizer

```@docs
Utilities.CachingOptimizer
Utilities.attach_optimizer
Utilities.reset_optimizer
Utilities.drop_optimizer
Utilities.state
Utilities.mode
```

### Mock optimizer

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

## DoubleDicts

```@docs
Utilities.DoubleDicts.DoubleDict
Utilities.DoubleDicts.DoubleDictInner
Utilities.DoubleDicts.IndexDoubleDict
Utilities.DoubleDicts.IndexDoubleDictInner
```
