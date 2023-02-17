```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

## File Formats

Functions to help read and write MOI models to/from various file formats. See
[The FileFormats submodule](@ref) for more details.

```@docs
FileFormats.Model
FileFormats.FileFormat
FileFormats.CBF.Model
FileFormats.LP.Model
FileFormats.MOF.Model
FileFormats.MPS.Model
FileFormats.NL.Model
FileFormats.SDPA.Model
```

## Other helpers

```@docs
FileFormats.NL.SolFileResults
```
