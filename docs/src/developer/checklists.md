# Checklists

The purpose of this page is to collate a series of checklists for commonly
performed changes to the source code of MathOptInterface.

In each case, copy the checklist into the description of the pull request.

## Making a release

Use this checklist when making a release of the MathOptInterface repository.

```
## Basic

 - [ ] `version` field of `Project.toml` has been updated
       - If a breaking change, increment the MAJOR field and reset others to 0
       - If adding new features, increment the MINOR field and reset PATCH to 0
       - If adding bug fixes or documentation changes, increment the PATCH field

## Documentation

 - [ ] Add a new entry to `docs/src/changelog.md`, following existing style

## Tests

 - [ ] The `solver-tests.yml` GitHub action does not have unexpected failures.
       To run the action, go to:
       https://github.com/jump-dev/MathOptInterface.jl/actions/workflows/solver-tests.yml
       and click "Run workflow"
```

## Adding a new set

Use this checklist when adding a new set to the MathOptInterface repository.

```
## Basic

 - [ ] Add a new `AbstractScalarSet` or `AbstractVectorSet` to `src/sets.jl`
 - [ ] If `isbitstype(S) == false`, implement `Base.copy(set::S)`
 - [ ] If `isbitstype(S) == false`, implement `Base.:(==)(x::S, y::S)`
 - [ ] If an `AbstractVectorSet`, implement `dimension(set::S)`, unless the
       dimension is given by `set.dimension`.

## Utilities

 - [ ] If an `AbstractVectorSet`, implement `Utilities.set_dot`,
       unless the dot product between two vectors in the set is equivalent to
       `LinearAlgebra.dot`
 - [ ] If an `AbstractVectorSet`, implement `Utilities.set_with_dimension` in
       `src/Utilities/matrix_of_constraints.jl`
 - [ ] Add the set to the `@model` macro at the bottom of `src/Utilities.model.jl`

## Documentation

 - [ ] Add a docstring, which gives the mathematical definition of the set,
       along with an `## Example` block containing a `jldoctest`
 - [ ] Add the docstring to `docs/src/reference/standard_form.md`
 - [ ] Add the set to the relevant table in `docs/src/manual/standard_form.md`

## Tests

 - [ ] Define a new `_set(::Type{S})` method in `src/Test/test_basic_constraint.jl`
       and add the name of the set to the list at the bottom of that files
 - [ ] If the set has any checks in its constructor, add tests to `test/sets.jl`

## MathOptFormat

 - [ ] Open an issue at `https://github.com/jump-dev/MathOptFormat` to add
       support for the new set {{ replace with link to the issue }}

## Optional

 - [ ] Implement `dual_set(::S)` and `dual_set_type(::Type{S})`
 - [ ] Add new tests to the `Test` submodule exercising your new set
 - [ ] Add new bridges to convert your set into more commonly used sets
```

## Adding a new bridge

Use this checklist when adding a new bridge to the MathOptInterface repository.

The steps are mostly the same, but locations depend on whether the bridge is a
`Constraint`, `Objective`, or `Variable` bridge. In each case below, replace
`XXX` with the appropriate type of bridge.

```
## Basic

 - [ ] Create a new file in `src/Bridges/XXX/bridges`
 - [ ] Define the bridge, following existing examples. The name of the bridge
       struct must end in `Bridge`
 - [ ] Check if your bridge can be a subtype of [`MOI.Bridges.Constraint.SetMapBridge`](@ref)
 - [ ] Define a new `const` that is a `SingleBridgeOptimizer` wrapping the
       new bridge. The name of the const must be the name of the bridge, less
       the `Bridge` suffix
 - [ ] `include` the file in `src/Bridges/XXX/bridges/XXX.jl`
 - [ ] If the bridge should be enabled by default, add the bridge to
       `add_all_bridges` at the bottom of `src/Bridges/XXX/XXX.jl`

## Tests

 - [ ] Create a new file in the appropriate subdirectory of `tests/Bridges/XXX`
 - [ ] Use `MOI.Bridges.runtests` to test various inputs and outputs of the
       bridge
 - [ ] If, after opening the pull request to add the bridge, some lines are not
       covered by the tests, add additional bridge-specific tests to cover the
       untested lines.

## Documentation

 - [ ] Add a docstring which uses the same template as existing bridges.
 - [ ] Add the docstring to `docs/src/submodules/Bridges/list_of_bridges.md`

## Final touch

If the bridge depends on run-time values of other variables and constraints in
the model:

 - [ ] Implement `MOI.Utilities.needs_final_touch(::Bridge)`
 - [ ] Implement `MOI.Utilities.final_touch(::Bridge, ::MOI.ModelLike)`
 - [ ] Ensure that `final_touch` can be called multiple times in a row
```

## Updating MathOptFormat

Use this checklist when updating the version of MathOptFormat.

```
## Basic

 - [ ] The file at `src/FileFormats/MOF/mof.X.Y.schema.json` is updated
 - [ ] The constants `SCHEMA_PATH`, `VERSION`, and `SUPPORTED_VERSIONS` are
       updated in `src/FileFormats/MOF/MOF.jl`

## New sets

 - [ ] New sets are added to the `@model` in `src/FileFormats/MOF/MOF.jl`
 - [ ] New sets are added to the `@enum` in `src/FileFormats/MOF/read.jl`
 - [ ] `set_to_moi` is defined for each set in `src/FileFormats/MOF/read.jl`
 - [ ] `head_name` is defined for each set in `src/FileFormats/MOF/write.jl`
 - [ ] A new unit test calling `_test_model_equality` is aded to
       `test/FileFormats/MOF/MOF.jl`

## Tests

 - [ ] The version field in `test/FileFormats/MOF/nlp.mof.json` is updated

## Documentation

 - [ ] The version fields are updated in `docs/src/submodules/FileFormats/overview.md`
```
