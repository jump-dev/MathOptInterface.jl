```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

## Benchmarks

Functions to help benchmark the performance of solver wrappers. See
[The Benchmarks submodule](@ref) for more details.

```@docs
Benchmarks.suite
Benchmarks.create_baseline
Benchmarks.compare_against_baseline
```
