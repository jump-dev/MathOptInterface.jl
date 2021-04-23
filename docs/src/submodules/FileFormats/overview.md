```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The FileFormats submodule

The `FileFormats` module provides functionality for reading and writing MOI
models using [`write_to_file`](@ref) and [`read_from_file`](@ref).

## Supported file types

You must read and write files to a [`FileFormats.Model`](@ref) object. Specifc
the file-type by passing a [`FileFormats.FileFormat`](@ref) enum. For example:

**The Conic Benchmark Format**

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_CBF)
A Conic Benchmark Format (CBF) model
```

**The LP file format**

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_LP)
A .LP-file model
```

**The MathOptFormat file format**

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MOF)
A MathOptFormat Model
```

**The MPS file format**

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MPS)
A Mathematical Programming System (MPS) model
```

**The SDPA file format**

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_SDPA)
A SemiDefinite Programming Algorithm Format (SDPA) model
```

## Write to file

To write a model `src` to a [MathOptFormat file](https://jump.dev/MathOptFormat/),
use:
```jldoctest fileformats
julia> src = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}

julia> MOI.add_variable(src)
MathOptInterface.VariableIndex(1)

julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MOF)
A MathOptFormat Model

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap with 1 entry:
  VariableIndex(1) => VariableIndex(1)

julia> MOI.write_to_file(dest, "file.mof.json")

julia> print(read("file.mof.json", String))
{
  "name": "MathOptFormat Model",
  "version": {
    "major": 0,
    "minor": 5
  },
  "variables": [
    {
      "name": "x1"
    }
  ],
  "objective": {
    "sense": "feasibility"
  },
  "constraints": []
}
```

## Read from file

To read a MathOptFormat file, use:
```jldoctest fileformats
julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MOF)
A MathOptFormat Model

julia> MOI.read_from_file(dest, "file.mof.json")

julia> MOI.get(dest, MOI.ListOfVariableIndices())
1-element Array{MathOptInterface.VariableIndex,1}:
 MathOptInterface.VariableIndex(1)

julia> rm("file.mof.json")  # Clean up after ourselves.
```

## Detecing the filetype automatically

Instead of the `format` keyword, you can also use the `filename` keyword
argument to [`FileFormats.Model`](@ref). This will attempt to automatically
guess the format from the file extension. For example:

```jldoctest fileformats
julia> src = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}

julia> dest = MOI.FileFormats.Model(filename = "file.cbf.gz")
A Conic Benchmark Format (CBF) model

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap with 0 entries

julia> MOI.write_to_file(dest, "file.cbf.gz")

julia> src_2 = MOI.FileFormats.Model(filename = "file.cbf.gz")
A Conic Benchmark Format (CBF) model

julia> src = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}

julia> dest = MOI.FileFormats.Model(filename = "file.cbf.gz")
A Conic Benchmark Format (CBF) model

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap with 0 entries

julia> MOI.write_to_file(dest, "file.cbf.gz")

julia> src_2 = MOI.FileFormats.Model(filename = "file.cbf.gz")
A Conic Benchmark Format (CBF) model

julia> MOI.read_from_file(src_2, "file.cbf.gz")

julia> rm("file.cbf.gz")  # Clean up after ourselves.
```
Note how the compression format (GZip) is also automatically detected from the
filename.

## Unsupported constraints

In some cases `src` may contain constraints that are not supported by the file
format (e.g., the CBF format supports integer variables but not binary). If so,
you should copy `src` to a bridged model using [`Bridges.full_bridge_optimizer`](@ref):
```julia
src = MOI.Utilities.Model{Float64}()
x = MOI.add_variable(model)
MOI.add_constraint(model, MOI.SingleVariable(x), MOI.ZeroOne())
dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_CBF)
bridged = MOI.Bridges.full_bridge_optimizer(dest, Float64)
MOI.copy_to(bridged, src)
MOI.write_to_file(dest, "my_model.cbf")
```
You should also note that even after bridging, it may still not be possible to
write the model to file because of unsupported constraints (e.g., PSD variables
in the LP file format).

## Read and write to `io`

In addition to [`write_to_file`](@ref) and [`read_from_file`](@ref), you can
read and write directly from `IO` streams using `Base.write` and `Base.read!`:
```jldoctest
julia> src = MOI.Utilities.Model{Float64}()
MOIU.GenericModel{Float64,MOIU.ModelFunctionConstraints{Float64}}

julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MPS)
A Mathematical Programming System (MPS) model

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap with 0 entries

julia> io = IOBuffer()
IOBuffer(data=UInt8[...], readable=true, writable=true, seekable=true, append=false, size=0, maxsize=Inf, ptr=1, mark=-1)

julia> write(io, dest)

julia> seekstart(io)
IOBuffer(data=UInt8[...], readable=true, writable=true, seekable=true, append=false, size=61, maxsize=Inf, ptr=1, mark=-1)

julia> src_2 = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MPS)
A Mathematical Programming System (MPS) model

julia> read!(io, src_2)
```
