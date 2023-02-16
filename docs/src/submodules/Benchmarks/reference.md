```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI

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

## Benchmarks

Functions to help benchmark the performance of solver wrappers. See
[The Benchmarks submodule](@ref) for more details.

```@docs
Benchmarks.suite
Benchmarks.create_baseline
Benchmarks.compare_against_baseline
```
