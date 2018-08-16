MathOptInterface (MOI) release notes
====================================

v0.6.0 (August 16, 2018)
-----------------------

- Add result fallback for `ConstraintDual` of variable-wise constraint,
  `ConstraintPrimal` and `ObjectiveValue`.
- Add tests for `ObjectiveBound`.
- Add test for empty row in vector linear constraint.
- Rework errors: `CannotError` has been renamed `NotAllowedError` and
  the distinction between `UnsupportedError` and `NotAllowedError` is now
  about whether the element is not supported (i.e. it cannot be copied a
  model containing this element) or the operation is not allowed (either
  because it is not implemented, because it cannot be performemd in the current
  state of the model, because it cannot be performed for a specific index, ...)

v0.5.0 (August 5, 2018)
-----------------------

- Fix names with CachingOptimizer.
- Cleanup thanks to @mohamed82008.
- Added a universal fallback for constraints.
- Fast utilities for function canonicalization thanks to @rdeits.
- Renamed `dimension` field to `side_dimension` in the context of matrix-like
  sets.
- New and improved tests for cases like duplicate terms and `ObjectiveBound`.
- Removed `cantransform`, `canaddconstraint`, `canaddvariable`, `canset`,
  `canmodify`, and `candelete` functions from the API. They are replaced by a
  new set of errors that are thrown: Subtypes of `UnsupportedError` indicate
  unsupported operations, while subtypes of `CannotError` indicate operations
  that cannot be performed in the current state.
 - The API for `copy!` is updated to remove the CopyResult type.
 - Updates for the new JuMP style guide.

v0.4.1 (June 28, 2018)
----------------------

- Fixes vector function modification on 32 bits.
- Fixes Bellman-Ford algorithm for bridges.
- Added an NLP test with `FeasibilitySense`.
- Update modification documentation.

v0.4.0 (June 23, 2018)
----------------------

- Helper constructors for `VectorAffineTerm` and `VectorQuadraticTerm`.
- Added `modify_lhs` to `TestConfig`.
- Additional unit tests for optimizers.
- Added a type parameter to `CachingOptimizer` for the `optimizer` field.
- New API for problem modification (#388)
- Tests pass without deprecation warnings on Julia 0.7.
- Small fixes and documentation updates.

v0.3.0 (May 25, 2018)
---------------------

- Functions have been redefined to use arrays-of-structs instead of
  structs-of-arrays.
- Improvements to `MockOptimizer`.
- Significant changes to `Bridges`.
- New and improved unit tests.
- Fixes for Julia 0.7.


v0.2.0 (April 24, 2018)
-----------------------

- Improvements to and better coverage of `Tests`.
- Documentation fixes.
- `SolverName` attribute.
- Changes to the NLP interface (new definition of variable order and arrays of
  structs for bound pairs and sparsity patterns).
- Addition of NLP tests.
- Introduction of `UniversalFallback`.
- `copynames` keyword argument to `MOI.copy!`.
- Add Bridges submodule.


v0.1.0 (February 28, 2018)
--------------------------

- Initial public release.
- The framework for MOI was developed at the JuMP-dev workshop at MIT in June
  2017 as a sorely needed replacement for MathProgBase.
