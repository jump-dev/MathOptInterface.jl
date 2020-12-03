```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The `FileFormats` submodule

The `FileFormats` module provides functionality for reading and writing MOI
models using [`write_to_file`](@ref) and [`read_from_file`](@ref).

To write a model `src` to a MathOptFormat file, use:
```julia
src = # ...
dest = FileFormats.Model(format = FileFormats.FORMAT_MOF)
MOI.copy_to(dest, src)
MOI.write_to_file(dest, "file.mof.json")
```
The list of supported formats is given by the [`FileFormats.FileFormat`](@ref)
enum.

Instead of the `format` keyword, you can also use the `filename` keyword
argument to [`FileFormats.Model`](@ref). This will attempt to automatically
guess the format from the file extension. For example:
```julia
src = # ...
filename = "my_model.cbf.gz"
dest = FileFormats.Model(filename = filename)
MOI.copy_to(dest, src)
MOI.write_to_file(dest, filename)

src_2 = FileFormats.Model(filename = filename)
MOI.read_from_file(src_2, filename)
```
Note how the compression format (GZip) is also automatically detected from the
filename.

In some cases `src` may contain constraints that are not supported by the file
format (e.g., the CBF format supports integer variables but not binary). If so,
you should copy `src` to a bridged model using [`Bridges.full_bridge_optimizer`](@ref):
```julia
src = # ... conic model ...
dest = FileFormats.Model(format = FileFormats.FORMAT_CBF)
bridged = MOI.Bridges.full_bridge_optimizer(dest, Float64)
MOI.copy_to(bridged, src)
MOI.write_to_file(dest, "my_model.cbf")
```
You should also note that even after bridging, it may still not be possible to
write the model to file because of unsupported constraints (e.g., PSD variables
in the LP file format).

In addition to [`write_to_file`](@ref) and [`read_from_file`](@ref), you can
read and write directly from `IO` streams using `Base.write` and `Base.read!`:
```julia
src = # ...
io = IOBuffer()
dest = FileFormats.Model(format = FileFormats.FORMAT_MPS)
MOI.copy_to(dest, src)
write(io, dest)

seekstart(io)
src_2 = FileFormats.Model(format = FileFormats.FORMAT_MPS)
read!(io, src_2)
```
