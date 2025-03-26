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
FileFormats.FORMAT_AUTOMATIC
FileFormats.FORMAT_CBF
FileFormats.CBF.Model
FileFormats.FORMAT_LP
FileFormats.LP.Model
FileFormats.FORMAT_MOF
FileFormats.MOF.Model
FileFormats.FORMAT_MPS
FileFormats.MPS.Model
FileFormats.FORMAT_NL
FileFormats.NL.Model
FileFormats.FORMAT_REW
FileFormats.FORMAT_SDPA
FileFormats.SDPA.Model
```

## Other helpers

```@docs
FileFormats.NL.SolFileResults
```
