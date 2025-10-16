```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# The FileFormats submodule

The `FileFormats` module provides functions for reading and writing MOI
models using [`write_to_file`](@ref) and [`read_from_file`](@ref).

## Supported file types

You must read and write files to a [`FileFormats.Model`](@ref) object. Specific
the file-type by passing a [`FileFormats.FileFormat`](@ref) enum. For example:

### The Conic Benchmark Format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_CBF)
MOI.FileFormats.CBF.Model
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

### The LP file format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_LP)
MOI.FileFormats.LP.Model
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

### The MathOptFormat file format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MOF)
MOI.FileFormats.MOF.Model
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

### The MPS file format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MPS)
MOI.FileFormats.MPS.Model
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

### The NL file format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_NL)
MOI.FileFormats.NL.Model
├ ObjectiveSense: unknown
├ ObjectiveFunctionType: unknown
├ NumberOfVariables: unknown
└ NumberOfConstraints: unknown
```

### The REW file format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_REW)
MOI.FileFormats.MPS.Model
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

Note that the REW format is identical to the MPS file format, except that all
names are replaced with generic identifiers.

### The SDPA file format

```jldoctest
julia> model = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_SDPA)
MOI.FileFormats.SDPA.Model
├ ObjectiveSense: FEASIBILITY_SENSE
├ ObjectiveFunctionType: MOI.ScalarAffineFunction{Float64}
├ NumberOfVariables: 0
└ NumberOfConstraints: 0
```

## Write to file

To write a model `src` to a [MathOptFormat file](https://jump.dev/MathOptFormat/),
use:
```jldoctest fileformats
julia> src = MOI.Utilities.Model{Float64}();

julia> MOI.add_variable(src);

julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MOF);

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap with 1 entry:
  MOI.VariableIndex(1) => MOI.VariableIndex(1)

julia> MOI.write_to_file(dest, "file.mof.json")

julia> print(read("file.mof.json", String))
{"name":"MathOptFormat Model","version":{"major":1,"minor":7},"variables":[{"name":"x1"}],"objective":{"sense":"feasibility"},"constraints":[]}
```

## Read from file

To read a MathOptFormat file, use:
```jldoctest fileformats
julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MOF);

julia> MOI.read_from_file(dest, "file.mof.json")

julia> MOI.get(dest, MOI.ListOfVariableIndices())
1-element Vector{MathOptInterface.VariableIndex}:
 MOI.VariableIndex(1)

julia> rm("file.mof.json")  # Clean up after ourselves.
```

## Detecting the file-type automatically

Instead of the `format` keyword, you can also use the `filename` keyword
argument to [`FileFormats.Model`](@ref). This will attempt to automatically
guess the format from the file extension. For example:

```jldoctest fileformats
julia> src = MOI.Utilities.Model{Float64}();

julia> dest = MOI.FileFormats.Model(filename = "file.cbf.gz");

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap()

julia> MOI.write_to_file(dest, "file.cbf.gz")

julia> src_2 = MOI.FileFormats.Model(filename = "file.cbf.gz");

julia> MOI.read_from_file(src_2, "file.cbf.gz")

julia> rm("file.cbf.gz")  # Clean up after ourselves.
```
Note how the compression format (GZip) is also automatically detected from the
filename.

## Unsupported constraints

In some cases `src` may contain constraints that are not supported by the file
format (for example, the CBF format supports integer variables but not binary).
If so, copy `src` to a bridged model using [`Bridges.full_bridge_optimizer`](@ref):
```jldoctest
julia> src = MOI.Utilities.Model{Float64}();

julia> x = MOI.add_variable(src);

julia> MOI.add_constraint(src, x, MOI.ZeroOne());

julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_CBF);

julia> bridged = MOI.Bridges.full_bridge_optimizer(dest, Float64);

julia> MOI.copy_to(bridged, src);

julia> MOI.write_to_file(dest, "my_model.cbf")

julia> rm("my_model.cbf")  # Clean up after ourselves.
```
!!! note
    Even after bridging, it may still not be possible to write the model to file
    because of unsupported constraints (for example, PSD variables in the LP
    file format).

## Read and write to `io`

In addition to [`write_to_file`](@ref) and [`read_from_file`](@ref), you can
read and write directly from `IO` streams using `Base.write` and `Base.read!`:
```jldoctest
julia> src = MOI.Utilities.Model{Float64}();

julia> dest = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MPS);

julia> MOI.copy_to(dest, src)
MathOptInterface.Utilities.IndexMap()

julia> io = IOBuffer();

julia> write(io, dest)

julia> seekstart(io);

julia> src_2 = MOI.FileFormats.Model(format = MOI.FileFormats.FORMAT_MPS);

julia> read!(io, src_2);
```

## ScalarNonlinearFunction

By default, reading a `.nl` or `.mof.json` that contains nonlinear expressions
will create an [`NLPBlock`](@ref).

To instead read nonlinear expressions as [`ScalarNonlinearFunction`](@ref),
pass the `use_nlp_block = false` keyword argument to the `Model` constructor:

```jldoctest
julia> model = MOI.FileFormats.Model(;
           format = MOI.FileFormats.FORMAT_MOF,
           use_nlp_block = false,
       );

julia> model = MOI.FileFormats.Model(;
           format = MOI.FileFormats.FORMAT_NL,
           use_nlp_block = false,
       );
```

## Validating MOF files

MathOptFormat files are governed by a schema. Use [JSONSchema.jl](https://github.com/fredo-dedup/JSONSchema.jl)
to check if a `.mof.json` file satisfies the schema.

First, construct the schema object as follows:
```jldoctest schema_mof
julia> import JSON, JSONSchema

julia> schema = JSONSchema.Schema(JSON.parsefile(MOI.FileFormats.MOF.SCHEMA_PATH))
A JSONSchema
```

Then, check if a model file is valid using `isvalid`:
```jldoctest schema_mof
julia> good_model = JSON.parse("""
       {
         "version": {
           "major": 1,
           "minor": 5
         },
         "variables": [{"name": "x"}],
         "objective": {"sense": "feasibility"},
         "constraints": []
       }
       """);

julia> isvalid(schema, good_model)
true
```

If we construct an invalid file, for example by mis-typing `name` as `NaMe`, the
validation fails:
```jldoctest schema_mof
julia> bad_model = JSON.parse("""
       {
         "version": {
           "major": 1,
           "minor": 5
         },
         "variables": [{"NaMe": "x"}],
         "objective": {"sense": "feasibility"},
         "constraints": []
       }
       """);

julia> isvalid(schema, bad_model)
false
```

Use `JSONSchema.validate` to obtain more insight into why the validation failed:
```jldoctest schema_mof
julia> JSONSchema.validate(schema, bad_model)
Validation failed:
path:         [variables][1]
instance:     JSON.Object{String, Any}("NaMe" => "x")
schema key:   required
schema value: Any["name"]
```
