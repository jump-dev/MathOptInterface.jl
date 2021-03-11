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

## File Formats

Functions to help read and write MOI models to/from various file formats. See
[The FileFormats submodule](@ref) for more details.

```@docs
FileFormats.Model
FileFormats.FileFormat
```
