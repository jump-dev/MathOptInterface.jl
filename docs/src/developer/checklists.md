# Checklists

The purpose of this page is to collate a series of checklists for commonly
performed changes to the source code of MathOptInterface.

In each case, copy the checklist into the description of the pull request.

## Adding a new set

Use this checklist when adding a new set to the MathOptInterface repository.

```
## Basic

 - [ ] Add a new `AbstractScalarSet` or `AbstractVectorSet` to `src/sets.jl`
 - [ ] If `isbitstype(S) == false`, implement `Base.copy(set::S)`
 - [ ] If `isbitstype(S) == false`, implement `Base.:(==)(x::S, y::S)`
 - [ ] If an `AbstractVectorSet`, implement `dimension(set::S)`

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
       support for the new set

## Optional

 - [ ] Implement `dual_set(::S)` and `dual_set_type(::Type{S})`
 - [ ] Add new tests to the `Test` submodule exercising your new set
 - [ ] Add new bridges to convert your set into more commonly used sets
```
