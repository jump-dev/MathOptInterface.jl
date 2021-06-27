```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# [The Test submodule](@id test_reference)

Functions to help test implementations of MOI. See
[The Test submodule](@ref test_module) for more details.

```@docs
Test.Config
Test.runtests
Test.setup_test
Test.@requires
Test.RequirementUnmet
```
