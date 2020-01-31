MathOptInterface (MOI) release notes
====================================

v0.9.10 (January 31, 2020)
---------------------

- Added `OptimizerWithAttributes` grouping an optimizer constructor and a list
  of optimizer attributes (#1008).
- Added `RelativeEntropyCone` with corresponding bridge into exponential cone
  constraints (#993).
- Added `NormSpectralCone` and `NormNuclearCone` with corresponding bridges
  into positive semidefinite constraints (#976).
- Added `supports_constrained_variable(s)` (#1004).
- Added `dual_set_type` (#1002).
- Added tests for vector specialized version of `delete` (#989, #1011).
- Added PSD3 test (#1007).
- Clarifed dual solution of `Tests.pow1v` and `Tests.pow1f` (#1013).
- Added support for `EqualTo` and `Zero` in
  `Bridges.Constraint.SplitIntervalBridge` (#1005).
- Fixed `Utilities.vectorize` for empty vector (#1003).
- Fixed free variables in LP writer (#1006).

v0.9.9 (December 29, 2019)
---------------------

- Incorporated MathOptFormat.jl as the FileFormats submodule. FileFormats
  provides readers and writers for a number of standard file formats and MOF, a
  file format specialized for MOI (#969).
- Improved performance of deletion of vector of variables in
  `MOI.Utilities.Model` (#983).
- Updated to MutableArithmetics v0.2 (#981).
- Added `MutableArithmetics.promote_operation` allocation tests (#975).
- Fixed inference issue on Julia v1.1 (#982).

v0.9.8 (December 19, 2019)
---------------------

- Implemented MutableArithmetics API (#924).
- Fixed callbacks with `CachingOptimizer` (#959).
- Fixed `MOI.dimension` for `MOI.Complements` (#948).
- Added fallback for `add_variables` (#972).
- Added `is_diagonal_vectorized_index` utility (#965).
- Improved linear constraints display in manual (#963, #964).
- Bridges improvements:
  * Added `IndicatorSet` to `SOS1` bridge (#877).
  * Added support for starting values for `Variable.VectorizeBridge` (#944).
  * Fixed `MOI.add_constraints` with non-bridged variable constraint on bridged
    variable (#951).
  * Fixed corner cases and docstring of geomean bridge (#961, #962, #966).
  * Fixed choice between variable or constraint bridges for constrained
    variables (#973).
  * Improve performance of bridge shortest path (#945, #946, #956).
  * Added docstring for `test_delete_bridge` (#954).
  * Added Variable bridge tests (#952).

v0.9.7 (October 30, 2019)
---------------------

- Implemented `_result_index_field` for `NLPBlockDual` (#934).
- Fixed copy of model with starting values for vector constraints (#941).
- Bridges improvements:
  * Improved performance of `add_bridge` and added `has_bridge` (#935).
  * Added `AbstractSetMapBridge` for bridges between sets `S1`, `S2` such that
    there is a linear map `A` such that `A*S1 = S2` (#933).
  * Added support for starting values for `FlipSignBridge`, `VectorizeBridge`,
    `ScalarizeBridge`, `SlackBridge`, `SplitIntervalBridge`, `RSOCBridge`,
    `SOCRBridge` `NormInfinityBridge`, `SOCtoPSDBridge` and `RSOCtoPSDBridge`
    (#933, #936, #937, #938, #939).

v0.9.6 (October 25, 2019)
---------------------

- Added complementarity constraints (#913).
- Allowed `ModelLike` objects as value of attributes (#928).
- Testing improvements:
  * Added `dual_objective_value` option to `MOI.Test.TestConfig` (#922).
  * Added `InvalidIndex` tests in `basic_constraint_tests` (#921).
  * Added tests for the constant term in indicator constraint (#929).
- Bridges improvements:
  * Added support for starting values for functionize bridges (#923).
  * Added variable indices context to variable bridges (#920).
  * Fixed a typo in printing o `debug_supports` (#927).

v0.9.5 (October 9, 2019)
---------------------

- Clarified `PrimalStatus`/`DualStatus` to be `NO_SOLUTION` if `result_index` is
  out of bounds (#912).
- Added tolerance for checks  and use `ResultCount` + 1 for the `result_index`
  in `MOI.Test.solve_result_status` (#910, #917).
- Use `0.5` instead of `2.0` for power in PowerCone in `basic_constraint_test`
  (#916).
- Bridges improvements:
  * Added debug utilities for unsupported variable/constraint/objective (#861).
  * Fixed deletion of variables in bridged `VectorOfVariables` constraints (#909).
  * Fixed `result_index` with objective bridges (#911).

v0.9.4 (October 2, 2019)
---------------------

- Added solver-independent MIP callbacks (#782).
- Implements submit for `Utilities.CachingOptimizer` and
  `Bridges.AbstractBridgeOptimizer` (#906).
- Added tests for result count of solution attributes (#901, #904).
- Added `NumberOfThreads` attribute (#892).
- Added `Utilities.get_bounds` to get the bounds on a variable (#890).
- Added a note on duplicate coefficients in documentation (#581).
- Added result index in `ConstraintBasisStatus` (#898).
- Added extension dictionary to `Utilities.Model` (#884, #895).
- Fixed deletion of constrained variables for CachingOptimizer (#905).
- Implemented `Utilities.shift_constraint` for `Test.UnknownScalarSet` (#896).
- Bridges improvements:
  * Added Variable.RSOCtoSOCBridge (#907).
  * Implemented `MOI.get` for `ConstraintFunction`/`ConstraintSet` for
    `Bridges.Constraint.SquareBridge` (#899).

v0.9.3 (September 20, 2019)
---------------------

- Fixed ambiguity detected in Julia v1.3 (#891, #893).
- Fixed missing sets from `ListOfSupportedConstraints` (#880).
- Fixed copy of `VectorOfVariables` constraints with duplicate indices (#886).
- Added extension dictionary to MOIU.Model (#884).
- Implemented `MOI.get` for function and set for `GeoMeanBridge` (#888).
- Updated documentation for SingleVariable indices and bridges (#885).
- Testing improvements:
  * Added more comprehensive tests for names (#882).
  * Added tests for `SingleVariable` duals (#883).
  * Added tests for `DualExponentialCone` and `DualPowerCone` (#873).
- Improvements for arbitary coefficient type:
  * Fixed `==` for sets with mutable fields (#887).
  * Removed some `Float64` assumptions in bridges (#878).
  * Automatic selection of `Constraint.[Scalar|Vector]FunctionizeBridge` (#889).

v0.9.2 (September 5, 2019)
---------------------

- Implemented model printing for `MOI.ModelLike` and specialized it for models
  defined in MOI (864).
- Generalized `contlinear` tests for arbitary coefficient type (#855).
- Fixed `supports_constraint` for `Semiinteger` and `Semicontinuous` and
  `supports` for `ObjectiveFunction` (#859).
- Fixed Allocate-Load copy for single variable constraints (#856).
- Bridges improvements:
  * Add objective bridges (#789).
  * Fixed `Variable.RSOCtoPSDBridge` for dimension 2 (#869).
  * Added `Variable.SOCtoRSOCBridge` (#865).
  * Added `Constraint.SOCRBridge` and disable
    `MOI.Bridges.Constraint.SOCtoPSDBridge` (#751).
  * Fixed `added_constraint_types` for `Contraint.LogDetBridge` and
    `Constraint.RootDetBridge` (#870).

v0.9.1 (August 22, 2019)
---------------------

- Fix support for Julia v1.2 (#834).
- L_1 and L_∞ norm epigraph cones and corresponding bridges to LP were added (#818).
- Added tests to `MOI.Test.nametest` (#833).
- Fix `MOI.Test.soc3test` for solvers not supporting infeasibility certificates (#839).
- Implements `operate` for operators `*` and `/` between vector function and
  constant (#837).
- Implements `show` for `MOI.Utilities.IndexMap` (#847).
- Fix corner cases for mapping of variables in `MOI.Utilities.CachingOptimizer`
  and substitution of variables in `MOI.Bridges.AbstractBridgeOptimizer` (#848).
- Fix transformation of constant terms for `MOI.Bridges.Constraint.SOCtoPSDBridge`
  and `MOI.Bridges.Constraint.RSOCtoPSDBridge` (#840).

v0.9.0 (August 13, 2019)
---------------------

- Support for Julia v0.6 and v0.7 was dropped (#714, #717).
- A `MOI.Utilities.Model` implementation of `ModelLike`, this should replace
  most use cases of `MOI.Utilities.@model` (#781).
- `add_constrained_variable` and `add_constrained_variables` were added (#759).
- Support for indicator constraints was added (#709, #712).
- `DualObjectiveValue` attribute was added (#473).
- `RawParameter` attribute was added (#733).
- A `dual_set` function was added (#804).
- A `Benchmarks` submodule was added to facilitate solver benchmarking (#769).
- A `submit` function was added, this may for intance allow the user to submit
  solutions or cuts to the solver from a callback (#775).
- The field of `ObjectiveValue` was renamed to `result_index` (#729).
- The `_constant` and `Utilities.getconstant` function were renamed to `constant`
- `REDUCTION_CERTIFICATE` result status was added (#734).
- Abstract matrix sets were added (#731).
- Testing improvements:
  * The testing guideline was updated (#728).
  * Quadratic tests were added (#697).
  * Unit tests for `RawStatusString`, `SolveTime`, `Silent` and `SolverName`
    were added (#726, #741).
  * A rotated second-order cone test was added (#759).
  * A power cone test was added (#768).
  * Tests for `ZeroOne` variables with variable bounds were added (#772).
  * An unbounded test was added (#773).
  * Existing tests had a few updates (#702, #703, #763).
- Documentation improvements:
  * Added a section on `CachingOptimizer` (#777).
  * Added a section on `UniversalFallback`, `Model` and `@model` (#762).
  * Transition the knapsack example to a doctest with MockOptimizer (#786).
- Utilities improvements:
  * A `CleverDict` utility was added for a vector that automatically transform
    into a dictionary once a first index is removed (#767).
  * The `Utilities.constant` function was renamed to `Utilities.constant_vector`
    (#740).
  * Implement optimizer attributes for CachingOptimizer (#745).
  * Rename `Utilities.add_scalar_constraint` to
    `Utilities.normalize_and_add_constraint` (#801).
  * `operate` with `vcat`, `SingleVariable` and `VectorOfVariables` now returns
    a `VectorOfVariables` (#616).
  * Fix a type piracy of `operate` (#784).
  * The `load_constraint` fallback signature was fixed (#760).
  * The `set_dot` function was extended to work with sparse arrays (#805).
- Bridges improvements:
  * The bridges no longer store the constraint function and set before it is
    briged, the bridges now have to implement `ConstraintFunction` and
    `ConstraintSet` if the user wants to recover them. As a consequence, the
    `@bridge` macro was removed (#722).
  * Bridge are now instantiated with a `bridge_constraint` function instead of
    using a constructor (#730).
  * Fix constraint attributes for bridges (#699).
  * Constraint bridges were moved to the `Bridges/Constraint` submodule so they
    should now inherit from `MOI.Bridges.Constraint.Abstract` and should
    implement `MOI.Bridges.Constraint.concrete_bridge_type` instead of
    `MOI.Bridges.concrete_bridge_type` (#756).
  * Variable bridges were added in (#759).
  * Various improvements (#746, #747).

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
