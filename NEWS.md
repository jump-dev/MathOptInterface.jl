MathOptInterface (MOI) release notes
====================================

v0.9.0 (May 2?, 2019)
---------------------

- Support for Julia v0.6 and v0.7 are dropped (#714, #717).
- Add support for indicator constraints (#709).
- Add `RawParameter` attribute (#733).
- The field of `ObjectiveValue` was renamed to `result_index` (#729).
- The `_constant` and `Utilities.getconstant` function are renamed to `constant`
  (#723).
- Add `REDUCTION_CERTIFICATE` result status (#734).
- Add abstract matrix sets (#731).
- Testing improvements:
  * The testing guideline was update (#728).
  * Add quadratic tests (#697).
  * Add unit tests for `RawStatusString`, `SolveTime` and `SolverName` (#726).
  * Small tweaks to existing tests (#702, #703).
- Bridges improvements:
  * The bridges no longer store the constraint function and set before it is
    briged, the bridges now have to implement `ConstraintFunction` and
    `ConstraintSet` if the user wants to recover them. As a consequence, the
    `@bridge` macro was removed (#722).
  * Bridge are now instantiated with a `bridge_constraint` function instead of
    using a constructor (#730).
  * Fix constraint attributes for bridges (#699).

v0.8.4 (March 13, 2019)
-----------------------

- Performance improvement in `default_copy_to` and bridge optimizer (#696).
- Add `Silent` and implement setting optimizer attributes in caching and mock
  optimizers (#695).
- Add functionize bridges (SingleVariable and VectorOfVariables) (#659).
- Minor typo fixes (#694).

v0.8.3 (March 6, 2019)
----------------------

- Use zero constant in scalar constraint function of `MOI.Test.copytest` (#691).
- Fix variable deletion with `SingleVariable` objective function (#690).
- Fix `LazyBridgeOptimizer` with bridges that add no constraints (#689).
- Error message improvements (#673, #685, #686, #688).
- Documentation improvements (#682, #683, #687).
- Basis status:
  * Remove `VariableBasisStatus` (#679).
  * Test `ConstraintBasisStatus` and implement it in bridges (#678).
- Fix inference of `NumberOfVariables` and `NumberOfConstraints` (#677).
- Implement division between a quadratic function and a number (#675).

v0.8.2 (February 7, 2019)
-------------------------

- Add `RawStatusString` attribute (#629).
- Do not set names to the optimizer but only to the cache in `CachingOptimizer`
  (#638).
- Make scalar MOI functions act as scalars in broadcast (#646).
- Add function utilities:
  * Implement `Base.zero` (#634), `Base.iszero` (#643), add missing arithmetic
    operations (#644, #645) and fix division (#648).
  * Add a `vectorize` function that turns a vector of `ScalarAffineFunction`
    into a `VectorAffineFunction` (#642).
- Improve support for starting values:
  * Show a warning in copy when starting values are not supported instead of
    throwing an error (#630).
  * Fix `UniversalFallback` for getting an variable or constraint attribute set to
    no indices (#623).
  * Add a test in `contlineartest` with partially set `VariablePrimalStart`.
- Bridges improvements:
  * Fix `StackOverFlow` in `LazyBridgeOptimizer` when there is a cycle in the
    graph of bridges.
  * Add `Slack` bridges (#610, #650).
  * Add `FlipSign` bridges (#658).
- Add tests with duplicate coefficients in `ScalarAffineFunction` and
  `VectorAffineFunction` (#639).
- Use tolerance to compare `VariablePrimal` in `rotatedsoc1` test (#632).
- Use a zero constant in `ScalarAffineFunction` of constraints in `psdt2`
  (#622).

v0.8.1 (January 7, 2019)
-------------------------

- Adding an NLP objective now overrides any objective set using the
  `ObjectiveFunction` attribute (#619).
- Rename `fullbridgeoptimizer` into `full_bridge_optimizer` (#621).
- Allow custom constraint types with `full_bridge_optimizer` (#617).
- Add `Vectorize` bridge which transforms scalar linear constraints into vector
  linear constraints (#615).

v0.8.0 (December 18, 2018)
--------------------------

- Rename all enum values to follow the JuMP naming guidelines for constants,
  e.g., `Optimal` becomes `OPTIMAL`, and `DualInfeasible` becomes
  `DUAL_INFEASIBLE`.
- Rename CachingOptimizer methods for style compliance.
- Add an `MOI.TerminationStatusCode` called `ALMOST_DUAL_INFEASIBLE`.

v0.7.0 (December 13, 2018)
--------------------------

- Test that `MOI.TerminationStatus` is `MOI.OptimizeNotCalled` before
  `MOI.optimize!` is called.
- Check `supports_default_copy_to` in tests (#594).
- Key pieces of information like optimality, infeasibility, etc., are now reported
  through `TerminationStatusCode`. It is typically no longer necessary to check the
  result statuses in addition to the termination status.
- Add perspective dimension to log-det cone (#593).

v0.6.4 (November 27, 2018)
--------------------------

- Add `OptimizeNotCalled` termination status (#577) and improve documentation of
  other statuses (#575).
- Add a solver naming guideline (#578).
- Make `FeasibilitySense` the default `ObjectiveSense` (#579).
- Fix `Utilities.@model` and `Bridges.@bridge` macros for functions and sets
  defined outside MOI (#582).
- Document solver-specific attributes (#580) and implement them in
  `Utilities.CachingOptimizer` (#565).

v0.6.3 (November 16, 2018)
--------------------------

- Variables and constraints are now allowed to have duplicate names. An error is
  thrown only on lookup. This change breaks some existing tests. (#549)
- Attributes may now be partially set (some values could be `nothing`). (#563)
- Performance improvements in Utilities.Model (#549, #567, #568)
- Fix bug in QuadtoSOC (#558).
- New `supports_default_copy_to` method that optimizers should implement to
  control caching behavior.
- Documentation improvements.

v0.6.2 (October 26, 2018)
---------------------------

- Improve hygiene of `@model` macro (#544).
- Fix bug in copy tests (#543).
- Fix bug in UniversalFallback attribute getter (#540).
- Allow all correct solutions for `solve_blank_obj` unit test (#537).
- Add errors for Allocate-Load and bad constraints (#534).
- \[performance\] Add specialized implementation of `hash` for `VariableIndex` (#533).
- \[performance\] Construct the name to object dictionaries lazily in model (#535).
- Add the `QuadtoSOC` bridge which transforms `ScalarQuadraticFunction`
  constraints into `RotatedSecondOrderCone` (#483).

v0.6.1 (September 22, 2018)
---------------------------

- Enable `PositiveSemidefiniteConeSquare` set and quadratic functions
  in `MOIB.fullbridgeoptimizer` (#524).
- Add warning in the bridge between `PositiveSemidefiniteConeSquare` and
  `PositiveSemidefiniteConeTriangle` when the matrix is almost symmetric (#522).
- Modify `MOIT.copytest` to not add multiples constraints on the same variable
  (#521).
- Add missing keyword argument in one of `MOIU.add_scalar_constraint` methods
  (#520).

v0.6.0 (August 30, 2018)
------------------------

- The `MOIU.@model` and `MOIB.@bridge` macros now support functions and sets
  defined in external modules. As a consequence, function and set names in the
  macro arguments need to be prefixed by module name.
- Rename functions according to the [JuMP style guide](http://www.juliaopt.org/JuMP.jl/latest/style.html):
  * `copy!` with keyword arguments `copynames` and `warnattributes` ->
    `copy_to` with keyword arguments `copy_names` and `warn_attributes`;
  * `set!` -> `set`;
  * `addvariable[s]!` -> `add_variable[s]`;
  * `supportsconstraint` -> `supports_constraint`;
  * `addconstraint[s]!` -> `add_constraint[s]`;
  * `isvalid` -> `is_valid`;
  * `isempty` -> `is_empty`;
  * `Base.delete!` -> `delete`;
  * `modify!` -> `modify`;
  * `transform!` -> `transform`;
  * `initialize!` -> `initialize`;
  * `write` -> `write_to_file`; and
  * `read!` -> `read_from_file`.
- Remove `free!` (use `Base.finalize` instead).
- Add the `SquarePSD` bridge which transforms `PositiveSemidefiniteConeTriangle`
  constraints into `PositiveSemidefiniteConeTriangle`.
- Add result fallback for `ConstraintDual` of variable-wise constraint,
  `ConstraintPrimal` and `ObjectiveValue`.
- Add tests for `ObjectiveBound`.
- Add test for empty rows in vector linear constraint.
- Rework errors: `CannotError` has been renamed `NotAllowedError` and
  the distinction between `UnsupportedError` and `NotAllowedError` is now
  about whether the element is not supported (i.e. it cannot be copied a
  model containing this element) or the operation is not allowed (either
  because it is not implemented, because it cannot be performemd in the current
  state of the model, because it cannot be performed for a specific index, ...)
- `canget` is removed. `NoSolution` is added as a result status to indicate
  that the solver does not have either a primal or dual solution available
  (See #479).

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
