```@meta
CurrentModule = MathOptInterface
```

# Release notes

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v1.19.0 (August 15, 2023)

### Added

 - Added [`VectorNonlinearFunction`](@ref) (#2201)
 - Added new bridges
    * [`Bridges.Constraint.IntegerToZeroOneBridge`](@ref) (#2205)
    * [`Bridges.Constraint.ToScalarQuadraticBridge`](@ref) (#2235)
    * [`Bridges.Constraint.ToVectorQuadraticBridge`](@ref) (#2235)
    * [`Bridges.Constraint.ToScalarNonlinearBridge`](@ref) (#2233) (#2235)
    * [`Bridges.Constraint.FunctionConversionBridge`](@ref) (#2235)
 - Added [`Bridges.bridging_cost`](@ref) for setting a bridge-specific cost in
   the bridging graph (#2235)
 - Added [`Utilities.eval_variables`](@ref) support for
   [`ScalarNonlinearFunction`](@ref) (#2218) (#2219)
 - Added support for [`ScalarNonlinearFunction`](@ref) in `FileFormats.NL`
   (#2228) (#2231)
 - Added support for writing non-`Float64` functions in `FileFormats.MOF`
 - Added `Utilities.lazy_map` (#2254)

### Fixed

 - Fixed method ambiguities in `operate` (#2224)
 - Fixed reading LP file with a `-infinity <= x <= +infinity` variable (#2225)
 - Fixed missing `require` in `Test.test_nonlinear_duals` (#2230)
 - Fixed bug in [`ConstraintPrimal`](@ref) of [`Bridges.Constraint.QuadtoSOCBridge`](@ref)
   (#2240)

### Other

 - Added extensions to `solver-tests.yml` (#2229)
 - Refactored `test/Benchmarks` (#2234)
 - Fixed warnings in tests (#2241) (#2243)
 - Small refactoring of bridges for upcoming `VectorNonlinearFunction` (#2244)
   (#2245)
 - Fixed various typos (#2251) (#2255)
 - Partitioned how we run the tests on GitHub actions (#2252) (#2253)

## v1.18.0 (June 23, 2023)

### Added

 - Added `Bridges.Objective.SlackBridgePrimalDualStart` (#2194)
 - Added [`constraint_gradient_structure`](@ref) and
   [`eval_constraint_gradient`](@ref) (#2200)

### Fixed

 - Fixed a missing `@require` in `MOI.Test` (#2195) (#2196)
 - Fixed incorrect usage of `Utilities.operate!` in bridges (#2207) (#2216)
 - Fixed splatting nonlinear expression with univariate operator (#2221)

### Other

 - Removed unused argument names (#2199)
 - Reduced memory requirement for tests (#2204)
 - Refactored `Utilities.promote_operation` (#2206)
 - Improved code style in `Utilities/mutable_arithmetics.jl` (#2209)
 - Refactored various methods in `Utilities/functions.jl` (#2208) (#2212)
   (#2213) (#2214) (#2215)

## v1.17.1 (June 6, 2023)

### Fixed

 - Fixed comparison of [`ScalarFunctionConstantNotZero`](@ref) (#2190)

### Other

 - Added documentation for enum instances (#2186)
 - Updated chatroom links in documentation (#2188)
 - Changed the documentation to build on Julia v1.9 (#2191)

## v1.17.0 (June 1, 2023)

### Added

 - Added [`ScalarNonlinearFunction`](@ref) (#2059)
 - Added a variety of tests for [`NormSpectralCone`](@ref), [`NormNuclearCone`](@ref),
   and [`HermitianPositiveSemidefiniteConeTriangle`](@ref) (#2174)
 - Added `Utilities.value_type` for computing the value type of a function
   (#2176)

### Fixed

 - Fixed support for external sets in `Utilities.loadfromstring!` (#2177)
 - Fixed `promote_operation` for [`ScalarNonlinearFunction`](@ref) (#2179)
 - Fixed two issues in `FileFormats.LP` when reading files with quadratic
   functions (#2182) (#2184)

## v1.16.0 (May 16, 2023)

### Added

 - Added support for MathOptFormat v1.3 and v1.4 (#2158) (#2169)
 - Added new method to the nonlinear API (#2162) (#2164)
    - [`eval_hessian_constraint`](@ref)
    - [`eval_hessian_objective`](@ref)
    - [`hessian_constraint_structure`](@ref)
    - [`hessian_objective_structure`](@ref)
 - Added new sets
    - [`NormCone`](@ref) (#2119)
    - [`ScaledPositiveSemidefiniteConeTriangle`](@ref) (#2154)

### Fixed

 - Fixed support for Julia v1.9 to work around a bug in the upstream Julia
   compiler (#2161) (#2163)
 - Fixed a correctness bug in [`Bridges.Constraint.HermitianToSymmetricPSDBridge`](@ref)
   (#2171)
 - Fixed `convert(::VariableIndex, ::ScalarAffineFunction)` when the function
   has terms with `0`coefficients (#2173)

### Other

 - Fixed `solver-tests.yml` (#2157)
 - Updated documentation links to developer chatroom (#2160)
 - Added various tests for bridges (#2156)
 - Added checklists to the developer documentation (#2167) (#2168)

## v1.15.1 (April 25, 2023)

### Fixed

  - Fixed deleting a variable in a bridged objective (#2150)

## v1.15.0 (April 19, 2023)

### Added

 - Added [`Bridges.Objective.VectorFunctionizeBridge`](@ref) (#2139)

### Fixed

 - Fixed support for `Rational` in [`Bridges.Constraint.SplitIntervalBridge`](@ref)
   (#2137)
 - Fixed printing of LaTeX models (#2141)
 - Fixed [`modify`](@ref) in [`Bridges.Objective.VectorSlackBridge`](@ref) (#2144)
 - Fixed NAME record with spaces in `FileFormats.MPS` (#2146)
 - Fixed deleting a variable in a bridged objective (#2147)

### Other

 - Add a test for variables in one-sided open [`Interval`](@ref) sets (#2133)
 - Minor style fixes in the source code (#2148)

## v1.14.1 (April 6, 2023)

### Fixed

 - Fixed a bug in [`Bridges.print_active_bridges`](@ref) (#2135)

### Other

 - Added a warning when an ambiguous string is passed to `exclude` in
   [`Test.runtests`](@ref) (#2136)

## v1.14.0 (April 4, 2023)

### Added

 - Added support for starting values in [`Bridges.Constraint.QuadtoSOCBridge`](@ref)
   (#2115)
 - Added support for `Regex` in the `include` and `exclude` arguments to
   [`Test.runtests`](@ref) (#2129)
 - Added [`Bridges.print_active_bridges`](@ref) methods for individual
   objectives and constraints (#2128)

### Fixed

 - Fixed [`ResultCount`](@ref) when parsing `.sol` files in `FileFormats.NL`
   (#2130)

## v1.13.2 (March 21, 2023)

### Fixed

 - Fixed splatting of containers in `MOI.Nonlinear` (#2120)
 - Fixed a bug reading LP files with default bounds (#2121)
 - Fixed a bug in which [`Bridges.Constraint.HermitianToSymmetricPSDBridge`](@ref)
   was not enabled by default (#2123)

### Other

 - Fixed typos in the documentation (#2114)
 - Functions now print to the REPL in algebraic form. This is potentially
   breaking if you have tests which rely on a specific `String` form of MOI
   functions. (#2112) (#2126)

## v1.13.1 (March 3, 2023)

### Other

 - Added the Google style guide to the documentation linter Vale, and fixed the
   resulting warnings (#2110)
 - Improved the docstrings in `src/functions.jl` (#2108)

## v1.13.0 (February 28, 2023)

### Added

 - Added [`Bridges.Constraint.NumberConversionBridge`](@ref) (#2091)
 - Added [`Parameter`](@ref) set (#2095) (#2105) (#2106) (#2109)
 - Added `with_cache_type` argument to [`instantiate`](@ref) (#2097)
 - Added support for [`HermitianPositiveSemidefiniteConeTriangle`](@ref) in
   `Utilities.Model` (#2100)

### Fixed

 - Fixed bug when `Utilities.@product_of_sets` is empty (#2101)
 - Fixed [`Bridges.print_active_bridges`](@ref) when variable bridge is an
   [`AbstractScalarSet`](@ref) (#2107)

### Other

 - Added tests for vector-valued objective functions in `FileFormats.MOF` (#2093)
 - Used and documented preference for `import MathOptInterface as MOI` (#2096)
 - Fix and test links in the documentation with `linkcheck = true` (#2098)
 - Improved docstrings of sets in `src/sets.jl` (#2099)
 - Skip checking flakey links in documentation with `linkcheck_ignore`  (#2103)

## v1.12.0 (February 10, 2023)

### Added

 - Added support for vector-valued objective functions (#2070)
 - Added a [`Utilities.distance_to_set`](@ref) method for
   [`SecondOrderCone`](@ref) (#2060)

### Fixed

 - Fixed a number of constraint bridges so that `Bridges.final_touch` can be
   called multiple times without forcing a rebuild of the reformulation (#2089)

### Other

 - Added new tests that a set [`ObjectiveFunction`](@ref) appears in
   [`ListOfModelAttributesSet`](@ref) (#2085)
 - Improved the docstrings of a number of constraint-programming related sets
   (#2087)

## v1.11.5 (January 24, 2023)

### Fixed

 - Fixed a bug writing `.lp` files with an off-diagonal quadratic objective
   (#2082)

### Other

 - Added `SnoopPrecompile` directives for reduced time-to-first-X in Julia v1.9
   (#2080)

## v1.11.4 (January 12, 2023)

### Fixed

 - Fixed a bug reading `.lp` files with an `Integer` section (#2078)

## v1.11.3 (January 12, 2023)

### Fixed

 - Fixed a performance bug when deleting a vector of constraints (#2072)
 - Fixed a bug reading `.lp` files with terms like `x -1 y` (#2076)

### Other

 - Documented the two-argument method of [`optimize!`](@ref) (#2074)

## v1.11.2 (January 2, 2023)

### Fixed

 - Fixed a bug reading `.mof.json` files with `ConstraintName` set for
   `VariableIndex` constraints (#2066)
 - Fixed a bug reading `.mof.json` files with nonlinear objectives and no
   constraints (#2068)

## v1.11.1 (December 22, 2022)

### Fixed

 - Fixed a bug reading `.mof.json` files with integer coefficients for affine
   and quadratic functions (#2063)

## v1.11.0 (December 2, 2022)

### Added

 - Added [`Utilities.PenaltyRelaxation`](@ref) and
   [`Utilities.ScalarPenaltyRelaxation`](@ref) (#1995)
 - Added [`Utilities.distance_to_set`](@ref) (#2048)
 - Added support for [`ConstraintPrimalStart`](@ref) and [`ConstraintDualStart`](@ref)
   in `FileFormats.MOF` (#2056)

### Other

 - Tidied these release notes (#2055)

## v1.10.0 (November 22, 2022)

### Added

 - Added new methods `set(::OptimizerWithAttributes, ::RawOptimizerAttribute, value)`
   and `get(::OptimizerWithAttributes, ::RawOptimizerAttribute)` (#2049)
 - Added new methods [`Utilities.DoubleDicts.outer_keys`](@ref) and
   [`Utilities.DoubleDicts.nonempty_outer_keys`](@ref) (#2052)

### Fixed

 - Fixed [`Bridges.Objective.SlackBridge`](@ref) when the objective function is
   complex-valued (#2036) (#2038)
 - Fixed docstring of [`Test.runtests`](@ref) to clarify the `warn_unsupported`
   argument (#2037)
 - Fixed reading of free variables in `FileFormats.LP` (#2044)
 - Fixed numerous edge cases reading files from QPLIB using `FileFormats.LP`
   (#2042) (#2044)
 - Fixed situations in which `x^y` returns a complex value in `Nonlinear`
   (#2050)

### Other

 - Improved the error message thrown when a user-defined nonlinear function does
   not accept splatted input (#2032)
 - Removed specialized iterators for `keys` and `values` in
   `Utilities.CleverDicts` (#2051)

## v1.9.0 (October 29, 2022)

### Added

 - Added default fallback for getting [`ListOfConstraintIndices`](@ref) and
   [`NumberOfConstraints`](@ref) when the constraint type is unsupported by the
   model (#2021)
 - Added support for `min` and `max` in nonlinear expressions (#2023)
 - Added support for `Indicator{EqualTo{T}}` constraints in `FileFormats.MPS`
   (#2022)
 - Added default fallback for [`write_to_file`](@ref) and [`read_from_file`](@ref)
   (#2029)

### Fixed

 - Fixed `Constraint.ZeroOneBridge` by adding new bounds as affine constraints
   instead of variable bounds (#1879)
 - Fixed reading free rows in `FileFormats.MPS` files (#2009)
 - Fixed parsing of `OBJSENSE` blocks in `FileFormats.MPS` files (#2016) (#2019)
 - Fixed the parsing of deeply nested nonlinear expressions by removing the use
   of recursion (#2020)
 - Fixed the requirements check in `Test.test_constrainnt_get_ConstraintIndex`
   (#2024)

## v1.8.2 (September 20, 2022)

### Documentation

 - Added [vale](https://vale.sh) as a documentation linter (#2002)
 - Improved styling of code blocks in the PDF (#1999) (#2000)
 - Fixed a number of typos in the documentation (#2001) (#2003)

## v1.8.1 (September 12, 2022)

### Fixed

 - Fixed a bug in `supports(::AbstractBridgeOptimizer` for constraint attributes
   (#1991) (#1992)

## v1.8.0 (September 1, 2022)

### Added

 - Added new sets
   - [`HyperRectangle`](@ref) (#1961)
   - [`Reified`](@ref) (#1955)
 - Added new bridges (#1955)
   - [`Bridges.Constraint.ReifiedAllDifferentToCountDistinctBridge`](@ref)
   - [`Bridges.Constraint.ReifiedCountDistinctToMILPBridge`](@ref)
   - [`Bridges.Constraint.SplitHyperRectangleBridge`](@ref)
 - Added support for `atan(y, x)` in `Nonlinear` (#1987)

### Fixed

 - Lazily construct expressions in `Nonlinear` so that expressions are updated
   when `Nonlinear.Parameter` values are updated (#1984)
 - Allow `NORM_LIMIT` as a `TerminationStatus` for unbounded problems in `Test`
   (#1990)

## v1.7.0 (August 16, 2022)

### Added

 - Added new sets
   - [`HermitianPositiveSemidefiniteConeTriangle`](@ref)
 - Added new optimizer-independent options
   - [`RelativeGapTolerance`](@ref)
   - [`AbsoluteGapTolerance`](@ref)
 - Added new bridges
   - [`Bridges.Constraint.GeoMeanToPowerBridge`](@ref)
   - [`Bridges.Constraint.HermitianToSymmetricPSDBridge`](@ref)
   - [`Bridges.Constraint.IndicatorGreaterToLessThanBridge`](@ref)
   - [`Bridges.Constraint.IndicatorLessToGreaterThanBridge`](@ref)
   - [`Bridges.Constraint.SplitComplexZerosBridge`](@ref)
   - [`Bridges.Constraint.SplitComplexEqualToBridge`](@ref)
   - [`Bridges.Objective.QuadratizeBridge`](@ref)
 - Added support for generic number types in `Utilities.loadfromstring!`
 - Updated `FileFormats.MOF` to MathOptFormat v1.1, enabling support for
   constraint programming sets in the `MOF` file format
 - Added support in various `FileFormats` for
   - indicator constraints in `FileFormats.MPS`
   - quadratic constraints and an objective in `FileFormats.LP`
   - quadratic constraints and an objective in `FileFormats.MPS`

### Fixed

 - Fixed some missing promotion rules

### Other

 - Improved the performance of Jacobian products in `Nonlinear`
 - Removed an un-needed `copy` in `Utilities.modify_function`
 - Various clean-ups in `Bridges/bridge_optimizer.jl`

## v1.6.1 (July 23, 2022)

### Fixed

 - Added support for `ExponentialCone` in `MatrixOfConstraints`
 - Fix `PSDSquare_3` test to reflect a previously fixed bug getting the
   `ConstraintDual` of a `PositiveSemidefiniteConeSquare` constraint

## v1.6.0 (July 2, 2022)

### Added

 - Added `Bridges.needs_final_touch` and `Bridges.final_touch`
 - Added new bridges from constraint programming sets to mixed-integer linear
   programs:
   - `AllDifferentToCountDistinctBridge`
   - `CountAtLeastToCountBelongsBridge`
   - `CountBelongsToMILPBridge`
   - `CountDistinctToMILPBridge`
   - `CountGreaterThanToMILPBridge`
   - `CircuitToMILPBridge`

### Fixed

 - Relax an instance of `::Vector` to `::AbstractVector` in `MOI.Nonlinear`
 - Fix `BinPackingToMILPBridge` to respect variable bounds
 - Fix `SemiToBinaryBridge` to throw error if other bounds are set

## v1.5.0 (June 27, 2022)

### Added

 - Added `GetAttributeNotAllowed` for solvers to indicate when getting an
   attribute encounters an error
 - Added `Utilities.get_fallback` support for `ObjectiveValue` and
   `DualObjectiveValue`
 - Added new bridges:
   - `RootDetConeSquare` to `RootDetConeTriangle`
   - `LogDetConeSquare` to `LogDetConeTriangle`
   - `BinPacking` to a mixed-integer linear program
   - `Table` to a mixed-integer linear program
 - Added `Bridges.print_active_bridges` to display the current optimal
   hyper-path in a `Bridges.LazyBridgeOptimizer`

### Fixed

 - Fixed `ZeroOne` tests with lower and upper bounds
 - Fixed error in `FileFormats.LP` when reading a malformed file
 - Fixed reading of nonlinear programs in `FileFormats.MOF`
 - Fixed bug in `ConstraintDual` when using `SquareBridge`

### Other

 - Improved documentation of nonlinear API
 - Documented duality convention for `PositiveSemidefiniteConeSquare` sets
 - Fixed typo in `Bridges.Constraint.QuadToSOCBridge` docstring

## v1.4.0 (June 9, 2022)

### Added

 - Added a number of sets for constraint programming:
   - `AllDifferent`
   - `BinPacking`
   - `Circuit`
   - `CountAtLeast`
   - `CountBelongs`
   - `CountDistinct`
   - `CountGreaterThan`
   - `Cumulative`
   - `Path`
   - `Table`
 - Added support for user-defined hessians in `Nonlinear`
 - Added `Bridges.runtests` to simplify the testing of bridge implementations

### Fixed

 - Fixed a bug in `FileFormats.NL` when writing univariate `*`

### Other

 - Began a large refactoring of the `Bridges` submodule, with greatly improved
   documentation.

## v1.3.0 (May 27, 2022)

### Added

 - Add `MOI.Nonlinear` submodule. This is a large new submodule that has been
   refactored from code that was in JuMP. For now, it should be considered
   experimental.
 - Add `FileFormats.NL.SolFileResults(::IO, ::Model)`
 - Add `FileFormats.NL.read!(::IO, ::Model)`
 - Add `MOI.modify` that accepts a vector of modifications

### Fixed

 - Fixed a bug in `Test` which attempted to include non-`.jl` files
 - Fixed a bug in `FileFormats` for models with open interval constraints

### Other

 - Fixed a performance issue in `Utilities.DoubleDict`
 - Various minor improvements to the documentation

## v1.2.0 (April 25, 2022)

### Added

 - Add support for the `FORMAT_REW`/`.rew` file format in `FileFormats`.

### Fixed

 - Fix bug handling of default variable bounds in `FileFormats.LP`
 - Fix `FileFormats.MPS` to not write `OBJSENSE` by default since this is only
   supported by some readers.

## v1.1.2 (March 31, 2022)

### Fixed

 - Fix a range of bugs in `FileFormats.LP`
 - Fix reading of problem dimensions in `FileFormats.SDPA`

## v1.1.1 (March 23, 2022)

### Fixed

 - Fix bug in `test_model_UpperBoundAlreadySet`
 - Fix bug in `test_infeasible_` tests
 - Fix bug in `test_objective_ObjectiveFunction_blank`
 - Relax restriction of `MOI.AbstractOptimizer` to `MOI.ModelLike` in
   `Utilities.CachingOptimizer` and `instantiate`.

### New tests

 - Add `test_conic_empty_matrix` that checks conic solvers support problems with
   no variables.

## v1.1.0 (March 2, 2022)

### Added

 - Added `MOI.Utilities.throw_unsupported(::UniversalFallback)` for simplifying
   solver wrappers which copy from a `UniversalFallback`.

## v1.0.2 (March 1, 2022)

### Fixed

 - Fixed a bug in the `test_model_ScalarFunctionConstantNotZero` test
 - Fixed the error type when an `AbstractFunctionConversionBridge` cannot get or
   set an attribute
 - Identified a correctness bug in `RSOCtoPSDBridge`. We now thrown an error
   instead of returning an incorrect result.

## v1.0.1 (February 25, 2022)

### Fixed

 - Fixed a bug in which OptimizerAttributes were not copied in CachingOptimizer
 - Fixed a bug in which `shift_constant` did not promote mixed types of coefficients
 - Fixed a bug in which deleting a constraint of a bridged variable threw
   `ErrorException` instead of `MOI.DeleteNotAllowed`
 - Fixed a bug in which `add_constraint` in `MatrixOfConstraints` did not
   canonicalize the function
 - Fixed a bug when modifying scalar constants of a function containing a
   bridged variable
 - Fixed a bug in which `final_touch` was not always called with a
   `CachingOptimizer`

## v1.0.0 (February 17, 2022)

Although tagged as a breaking release, v1.0.0 is v0.10.9 with deprecations
removed, similar to how Julia 1.0 was Julia 0.7 with deprecations removed.

### Breaking

 - Julia 1.6 is now the minimum supported version
 - All deprecations have been removed

### Troubleshooting problems when updating

If you experience problems when updating, you are likely using previously
deprecated features. (By default, Julia does not warn when you use
deprecated features.)

To find the deprecated features you are using, start Julia with `--depwarn=yes`:
```
$ julia --depwarn=yes
```
Then install MathOptInterface v0.10.9:
```julia
julia> using Pkg
julia> pkg"add MathOptInterface@0.10"
```
And then run your code. Apply any suggestions, or search the release notes below
for advice on updating a specific deprecated feature.

## v0.10.9 (February 16, 2022)

### Added

 - Added `MOI.Utilities.FreeVariables` as a new `VariablesConstrainer` for conic
   solvers
 - Added `MOI.default_cache` for specifying the model used in `CachingOptimizer`

### Fixed

 - Fixed LaTeX printing of `MOI.Interval` sets

### Other

 - Added Aqua.jl as a CI check, and fixed suggested issues
 - The constructors of GeoMeanBridge, StructOfConstraints, and CachingOptimizer
   were changed from outer to inner constructors. This change is technically
   breaking, but does not impact users who followed the documented API.

## v0.10.8 (February 3, 2022)

### Added

 - Added a `Base.read!` for `FileFormats.LP`.

### Fixed

 - Fixed a bug in `MutableSparseMatrix`
 - Fixed a bug when calling `operate!(vcat, ...)` with `Number` arguments
 - Removed unintended export of deprecated symbols
 - Fixed a bug with `PowerCone` and `DualPowerCone` in `MatrixOfConstraints`.

## v0.10.7 (January 5, 2022)

### Added

 - Added test for modifying the constant vector in a
   `VectorAffineFunction-in-Zeros` constraint.

### Fixed

 - Fixed the order in which sets are added to a `LazyBridgeOptimizer`. Compared
   to v0.10.6, this may result in bridged models being created with a different
   number (and order) of variables and constraints. However, it was necessary to
   fix cases which were previously rejected as unsupported, even though there
   was a valid bridge transformation.
 - Fixed an error message in `FileFormats.CBF`
 - Fixed comparison in `test_linear_integration_Interval`
 - Fixed errors for `ConstraintPrimal` in a `CachingOptimizer`
 - Fixed printing of models with non-`Float64` coefficients.

### Other

 - Various improvements to reduce time-to-first-solve latency
 - Improved error message when an optimizer does not support `compute_conflict!`

## v0.10.6 (November 30, 2021)

### Added

 - Added new documentation and tests for infeasibility certificates
 - Added a version control system for the tests in `MOI.Test.runtests`. Pass
   `exclude_tests_after = v"0.10.5"` to run tests added in v0.10.5 and earlier.
 - `MOI.Test.runtests` now supports generic number types. To specify the number
   type `T`, pass `MOI.Test.Config(T)`.
 - Added `infeasible_status` to `MOI.Test.Config` for solvers which return
   `LOCALLY_INFEASIBLE`
 - CachingOptimizers now use a fallback for `ConstraintPrimal`. This should
   enable solvers using a CachingOptimizer to pass tests requiring
   `ConstraintPrimal`.

### Fixed

 - Fixed a StackOverflow bug in `copy_to`
 - Fixed error thrown when nonconvex quadratic constraints cannot be bridged
 - Fixed a bug in `copy_to` for `FileFormats.NL.Model`
 - Fixed a bug in `FileFormats.NL` when printing large integers
 - Remove a common test failure for `LowerBoundAlreadySet` tests
 - `Utilities.num_rows` is now exported
 - Remove parts of failing `test_model_copy_to_xxx` tests due to bridges

## v0.10.5 (November 7, 2021)

### Fixed

- Fixed getter in `UniversalFallback`
- Fixed `test_solve_conflict_zeroone_ii`

### Other

- Make `normalize_and_add_constraint` more flexible
- Update paper BibTeX

## v0.10.4 (October 26, 2021)

### Added

- Add `SolverVersion` attribute
- Add new tests:
  - `test_solve_conflict_zeroone_ii`
  - `test_nonlinear_objective`
- `Utilities.VariablesContainer` now supports `ConstraintFunction` and
  `ConstraintSet`
- The documentation is now available as a PDF

### Other

- Update to MutableArithmetics 0.3
- Various improvements to the documentation

## v0.10.3 (September 18, 2021)

### Fixed

- Fixed bug which prevented callbacks from working through a CachingOptimizer
- Fixed bug in `Test` submodule

## v0.10.2 (September 16, 2021)

- Updated MathOptFormat to v1.0
- Updated JSONSchema to v1.0
- Added `Utilities.set_with_dimension`
- Added two-argument `optimize!(::AbstractOptimizer, ::ModelLike)`
- The experimental feature `copy_to_and_optimize!` has been removed
- `Det` bridges now support getting `ConstraintFunction` and `ConstraintSet`
- Various minor bug fixes identified by improved testing

## v0.10.1 (September 8, 2021)

- Various fixes to `MOI.Test`

## v0.10.0 (September 6, 2021)

**MOI v0.10 is a significant breaking release. There are a large number of
user-visible breaking changes and code refactors, as well as a substantial
number of new features.**

### Breaking in MOI

- `SingleVariable` has been removed; use `VariableIndex` instead
- `SingleVariableConstraintNameError` has been renamed to
  `VariableIndexConstraintNameError`
- `SettingSingleVariableFunctionNotAllowed` has been renamed to
  `SettingVariableIndexFunctionNotAllowed`
- `VariableIndex` constraints should not support `ConstraintName`
- `VariableIndex` constraints should not support `ConstraintBasisStatus`;
  implement `VariableBasisStatus` instead
- `ListOfConstraints` has been renamed to `ListOfConstraintTypesPresent`
- `ListOfConstraintTypesPresent` should now return `Tuple{Type,Type}` instead of
  `Tuple{DataType,DataType}`
- `SolveTime` has been renamed to `SolveTimeSec`
- `IndicatorSet` has been renamed to `Indicator`
- `RawParameter` has been renamed to `RawOptimizerAttribute` and now takes
  `String` instead of `Any` as the only argument
- The `.N` field in result attributes has been renamed to `.result_index`
- The `.variable_index` field in `ScalarAffineTerm` has been renamed to
  `.variable`
- The `.variable_index_1` field in `ScalarQuadraticTerm` has been renamed to
  `.variable_1`
- The `.variable_index_2` field in `ScalarQuadraticTerm` has been renamed to
  `.variable_2`
- The order of `affine_terms` and `quadratic_terms` in `ScalarQuadraticFunction`
  and `VectorQuadraticFunction` have been reversed. Both functions now accept
  quadratic, affine, and constant terms in that order.
- The `index_value` function has been removed. Use `.value` instead.
- `isapprox` has been removed for `SOS1` and `SOS2`.
- The `dimension` argument to `Complements(dimension::Int)` should now be the
  length of the corresponding function, instead of half the length. An
  `ArgumentError` is thrown if `dimension` is not even.
- `copy_to` no longer takes keyword arguments:
  - `copy_names`: now copy names if they are supported by the destination solver
  - `filter_constraints`: use `Utilities.ModelFilter` instead
  - `warn_attributes`: never warn about optimizer attributes

### Breaking in `Bridges`

- `Constraint.RSOCBridge` has been renamed to `Constraint.RSOCtoSOCBridge`
- `Constraint.SOCRBridge` has been renamed to `Constraint.SOCtoRSOCBridge`
- Bridges now return vectors that can be modified by the user. Previously, some
  bridges returned views instead of copies.
- `Bridges.IndexInVector` has been unified into a single type. Previously, there
  was a different type for each submodule within `Bridges`
- The signature of indicator bridges has been fixed. Use
  `MOI.Bridges.Constraint.IndicatortoSOS1{Float64}(model)`.

### Breaking in `FileFormats`

- `FileFormats.MOF.Model` no longer accepts `validate` argument. Use the
  JSONSchema package to validate the MOF file. See the documentation for more
  information.

### Breaking in `Utilities`

- The datastructure of `Utilities.Model` (and models created with
  `Utilities.@model`) has been significantly refactored in a breaking way. This
  includes the way that objective functions and variable-related information is
  stored.
- `Utilities.supports_default_copy` has been renamed to
  `supports_incremental_interface`
- `Utilities.automatic_copy_to` has been renamed to `Utilities.default_copy_to`
- The allocate-load API has been removed
- `CachingOptimizer`s are now initialized as `EMPTY_OPTIMIZER` instead of
  `ATTACHED_OPTIMIZER`. If your code relies on the optimizer being attached,
  call `MOIU.attach_optimizer(model)` after creation.
- The field names of `Utilities.IndexMap` have been renamed to `var_map` and
  `con_map`. Accessing these fields directly is considered a private detail that
  may change. Use the public `getindex` and `setindex!` API instead.
- The size argument to `Utilities.CleverDicts.CleverDict(::Integer)` has been
  removed.
- The size argument to `Utilities.IndexMap(::Integer)` has been removed.
- `Utilities.DoubleDicts` have been significantly refactored. Consult the source
  code for details.
- `Utilities.test_models_equal` has been moved to `MOI.Test`

### Breaking in `Test`

- `MOI.Test` has been renamed to `MOI.DeprecatedTest`
- An entirely new `MOI.Test` submodule has been written. See the documentation
  for details. The new `MOI.Test` submodule may find many bugs in the
  implementations of existing solvers that were previously untested.

### Other changes:

- `attribute_value_type` has been added
- `copy_to_and_optimize!` has been added
- `VariableBasisStatus` has been added
- `print(model)` now prints a human-readable description of the model
- Various improvements to the `FileFormats` submodule
  - `FileFormats.CBF` was refactored and received bugfixes
  - Support for MathOptFormat v0.6 was added in `FileFormats.MOF`
  - `FileFormats.MPS` has had bugfixes and support for more features such as
    `OBJSENSE` and objective constants.
  - `FileFormats.NL` has been added to support nonlinear files
- Improved type inference throughout to reduce latency

### Updating

A helpful script when updating is:
```julia
for (root, dirs, files) in walkdir(".")
    for file in files
        if !endswith(file, ".jl")
            continue
        end
        path = joinpath(root, file)
        s = read(path, String)
        for pair in [
            ".variable_index" => ".variable",
            "RawParameter" => "RawOptimizerAttribute",
            "ListOfConstraints" => "ListOfConstraintTypesPresent",
            "TestConfig" => "Config",
            "attr.N" => "attr.result_index",
            "SolveTime" => "SolveTimeSec",
            "DataType" => "Type",
            "Utilities.supports_default_copy_to" =>
                "supports_incremental_interface",
            "SingleVariableConstraintNameError" =>
                "VariableIndexConstraintNameError",
            "SettingSingleVariableFunctionNotAllowed" =>
                "SettingVariableIndexFunctionNotAllowed",
            "automatic_copy_to" => "default_copy_to",
        ]
            s = replace(s, pair)
        end
        write(path, s)
    end
end
```

## v0.9.22 (May 22, 2021)

This release contains backports from the ongoing development of the v0.10 release.

- Improved type inference in `Utilities`, `Bridges` and `FileFormats` submodules
  to reduce latency.
- Improved performance of `Utilities.is_canonical`.
- Fixed `Utilities.pass_nonvariable_constraints` with bridged variables.
- Fixed performance regression of `Utilities.Model`.
- Fixed ordering of objective setting in parser.

## v0.9.21 (April 23, 2021)

- Added `supports_shift_constant`.
- Improve performance of bridging quadratic constraints.
- Add precompilation statements.
- Large improvements to the documentation.
- Fix a variety of inference issues, benefiting precompilation and reducing
  initial latency.
- `RawParameter`s are now ignored when resetting a `CachingOptimizer`.
  Previously, changing the underlying optimizer after `RawParameter`s were set
  would throw an error.
- `Utilities.AbstractModel` is being refactored. This may break users
  interacting with private fields of a model generated using `@model`.

## v0.9.20 (February 20, 2021)

- Improved performance of `Utilities.ScalarFunctionIterator`
- Added support for `compute_conflict` to MOI layers
- Added test with zero off-diagonal quadratic term in objective
- Fixed double deletion of nested bridged `SingleVariable`/`VectorOfVariables`
  constraints
- Fixed modification of un-set objective
- Fixed function modification with duplicate terms
- Made unit tests abort without failing if the problem class is not supported
- Formatted code with JuliaFormatter
- Clarified `BasisStatusCode`'s docstring

## v0.9.19 (December 1, 2020)

- Added `CallbackNodeStatus` attribute
- Added bridge from `GreaterThan` or `LessThan` to `Interval`
- Added tests for infeasibility certificates and double optimize
- Fixed support for Julia v1.6
- Re-organized MOI docs and added documentation for adding a test

## v0.9.18 (November 3, 2020)

- Various improvements for working with complex numbers
- Added `GeoMeantoRelEntrBridge` to bridge a `GeometricMeanCone` constraint to a
  relative entropy constraint

## v0.9.17 (September 21, 2020)

- Fixed `CleverDict` with variable of negative index value
- Implement `supports_add_constrained_variable` for `MockOptimizer`

## v0.9.16 (September 17, 2020)

- Various fixes:
  * 32-bit support
  * `CleverDict` with abstract value type
  * Checks in test suite

## v0.9.15 (September 14, 2020)

- Bridges improvements:
  - (R)SOCtoNonConvexQuad bridge
  - ZeroOne bridge
  - Use `supports_add_constrained_variable` in `LazyBridgeOptimizer`
  - Exposed `VariableBridgeCost` and `ConstraintBridgeCost` attributes
  - Prioritize constraining variables on creation according to these costs
  - Refactor bridge debugging
- Large performance improvements across all submodules
- Lots of documentation improvements
- FileFormats improvements:
  - Update MathOptFormat to v0.5
  - Fix supported objectives in `FileFormats`
- Testing improvements:
  - Add name option for `basic_constraint_test`
- Bug fixes and missing methods
  - Add `length` for iterators
  - Fix bug with duplicate terms
  - Fix order of `LinearOfConstraintIndices`

## v0.9.14 (May 30, 2020)

- Add a solver-independent interface for accessing the set of conflicting
  constraints an Irreducible Inconsistent Subsystem (#1056).
- Bump JSONSchema dependency from v0.2 to v0.3 (#1090).
- Documentation improvements:
  * Fix typos (#1054, #1060, #1061, #1064, #1069, #1070).
  * Remove the outdated recommendation for a package implementing MOI for a
    solver `XXX` to be called `MathOptInterfaceXXX` (#1087).
- Utilities improvements:
  * Fix `is_canonical` for quadratic functions (#1081, #1089).
  * Implement `add_constrained_variable[s]` for `CachingOptimizer`
    so that it is added as constrained variables to the underlying optimizer (#1084).
  * Add support for custom objective functions for
    `UniversalFallback` (#1086).
  * Deterministic ordering of constraints in `UniversalFallback` (#1088).
- Testing improvements:
  * Add `NormOneCone`/`NormInfinityCone` tests (#1045).
- Bridges improvements:
  * Add bridges from `Semiinteger` and `Semicontinuous` (#1059).
  * Implement getting `ConstraintSet` for `Variable.FlipSignBridge` (#1066).
  * Fix setting `ConstraintFunction` for `Constraint.ScalarizeBridge` (#1093).
  * Fix `NormOne`/`NormInf` bridges with nonzero constants (#1045).
  * Fix StackOverflow in `debug` (#1063).
- FileFormats improvements:
  * [SDPA] Implement the extension for integer variables (#1079).
  * [SDPA] Ignore comments after `m` and `nblocks` and detect `dat-s` extension (#1077).
  * [SDPA] No scaling of off-diagonal coefficient (#1076).
  * [SDPA] Add missing negation of constant (#1075).

## v0.9.13 (March 24, 2020)

- Added tests for `Semicontinuous` and `Semiinteger` variables (#1033).
- Added tests for using `ExprGraph`s from NLP evaluators (#1043).
- Update version compatibilities of dependencies (#1034, #1051, #1052).
- Fixed typos in documentation (#1044).

## v0.9.12 (February 28, 2020)

- Fixed writing `NLPBlock` in MathOptFormat (#1037).
- Fixed `MockOptimizer` for result attributes with non-one result index (#1039).
- Updated test template with `instantiate` (#1032).

## v0.9.11 (February 21, 2020)

- Add an option for the model created by `Utilities.@model` to be a subtype
  of `AbstractOptimizer` (#1031).
- Described dual cone in docstrings of `GeoMeanCone` and `RelativeEntropyCone`
  (#1018, #1028).
- Fixed typos in documentation (#1022, #1024).
- Fixed warning of unsupported attribute (#1027).
- Added more rootdet/logdet conic tests (#1026).
- Implemented `ConstraintDual` for `Constraint.GeoMeanBridge`,
  `Constraint.RootDetBridge` and `Constraint.LogDetBridge`
  and test duals in tests with `GeoMeanCone` and `RootDetConeTriangle` and
  `LogDetConeTriangle` cones (#1025, #1026).

## v0.9.10 (January 31, 2020)

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
- Clarified dual solution of `Tests.pow1v` and `Tests.pow1f` (#1013).
- Added support for `EqualTo` and `Zero` in
  `Bridges.Constraint.SplitIntervalBridge` (#1005).
- Fixed `Utilities.vectorize` for empty vector (#1003).
- Fixed free variables in LP writer (#1006).

## v0.9.9 (December 29, 2019)

- Incorporated MathOptFormat.jl as the FileFormats submodule. FileFormats
  provides readers and writers for a number of standard file formats and MOF, a
  file format specialized for MOI (#969).
- Improved performance of deletion of vector of variables in
  `MOI.Utilities.Model` (#983).
- Updated to MutableArithmetics v0.2 (#981).
- Added `MutableArithmetics.promote_operation` allocation tests (#975).
- Fixed inference issue on Julia v1.1 (#982).

## v0.9.8 (December 19, 2019)

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
  * Fixed corner cases and docstring of `GeoMeanBridge` (#961, #962, #966).
  * Fixed choice between variable or constraint bridges for constrained
    variables (#973).
  * Improve performance of bridge shortest path (#945, #946, #956).
  * Added docstring for `test_delete_bridge` (#954).
  * Added Variable bridge tests (#952).

## v0.9.7 (October 30, 2019)

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

## v0.9.6 (October 25, 2019)

- Added complementarity constraints (#913).
- Allowed `ModelLike` objects as value of attributes (#928).
- Testing improvements:
  * Added `dual_objective_value` option to `MOI.Test.TestConfig` (#922).
  * Added `InvalidIndex` tests in `basic_constraint_tests` (#921).
  * Added tests for the constant term in indicator constraint (#929).
- Bridges improvements:
  * Added support for starting values for `Functionize` bridges (#923).
  * Added variable indices context to variable bridges (#920).
  * Fixed a typo in printing o `debug_supports` (#927).

## v0.9.5 (October 9, 2019)

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

## v0.9.4 (October 2, 2019)

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

## v0.9.3 (September 20, 2019)

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
- Improvements for arbitrary coefficient type:
  * Fixed `==` for sets with mutable fields (#887).
  * Removed some `Float64` assumptions in bridges (#878).
  * Automatic selection of `Constraint.[Scalar|Vector]FunctionizeBridge` (#889).

## v0.9.2 (September 5, 2019)

- Implemented model printing for `MOI.ModelLike` and specialized it for models
  defined in MOI (864).
- Generalized `contlinear` tests for arbitrary coefficient type (#855).
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

## v0.9.1 (August 22, 2019)

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

## v0.9.0 (August 13, 2019)

- Support for Julia v0.6 and v0.7 was dropped (#714, #717).
- A `MOI.Utilities.Model` implementation of `ModelLike`, this should replace
  most use cases of `MOI.Utilities.@model` (#781).
- `add_constrained_variable` and `add_constrained_variables` were added (#759).
- Support for indicator constraints was added (#709, #712).
- `DualObjectiveValue` attribute was added (#473).
- `RawParameter` attribute was added (#733).
- A `dual_set` function was added (#804).
- A `Benchmarks` submodule was added to facilitate solver benchmarking (#769).
- A `submit` function was added, this may for instance allow the user to submit
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
    bridged, the bridges now have to implement `ConstraintFunction` and
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

## v0.8.4 (March 13, 2019)

- Performance improvement in `default_copy_to` and bridge optimizer (#696).
- Add `Silent` and implement setting optimizer attributes in caching and mock
  optimizers (#695).
- Add `Functionize` bridges (SingleVariable and VectorOfVariables) (#659).
- Minor typo fixes (#694).

## v0.8.3 (March 6, 2019)

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

## v0.8.2 (February 7, 2019)

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

## v0.8.1 (January 7, 2019)

- Adding an NLP objective now overrides any objective set using the
  `ObjectiveFunction` attribute (#619).
- Rename `fullbridgeoptimizer` into `full_bridge_optimizer` (#621).
- Allow custom constraint types with `full_bridge_optimizer` (#617).
- Add `Vectorize` bridge which transforms scalar linear constraints into vector
  linear constraints (#615).

## v0.8.0 (December 18, 2018)

- Rename all enum values to follow the JuMP naming guidelines for constants,
  for example, `Optimal` becomes `OPTIMAL`, and `DualInfeasible` becomes
  `DUAL_INFEASIBLE`.
- Rename CachingOptimizer methods for style compliance.
- Add an `MOI.TerminationStatusCode` called `ALMOST_DUAL_INFEASIBLE`.

## v0.7.0 (December 13, 2018)

- Test that `MOI.TerminationStatus` is `MOI.OptimizeNotCalled` before
  `MOI.optimize!` is called.
- Check `supports_default_copy_to` in tests (#594).
- Key pieces of information like optimality, infeasibility, etc., are now reported
  through `TerminationStatusCode`. It is typically no longer necessary to check the
  result statuses in addition to the termination status.
- Add perspective dimension to log-det cone (#593).

## v0.6.4 (November 27, 2018)

- Add `OptimizeNotCalled` termination status (#577) and improve documentation of
  other statuses (#575).
- Add a solver naming guideline (#578).
- Make `FeasibilitySense` the default `ObjectiveSense` (#579).
- Fix `Utilities.@model` and `Bridges.@bridge` macros for functions and sets
  defined outside MOI (#582).
- Document solver-specific attributes (#580) and implement them in
  `Utilities.CachingOptimizer` (#565).

## v0.6.3 (November 16, 2018)

- Variables and constraints are now allowed to have duplicate names. An error is
  thrown only on lookup. This change breaks some existing tests. (#549)
- Attributes may now be partially set (some values could be `nothing`). (#563)
- Performance improvements in Utilities.Model (#549, #567, #568)
- Fix bug in QuadtoSOC (#558).
- New `supports_default_copy_to` method that optimizers should implement to
  control caching behavior.
- Documentation improvements.

## v0.6.2 (October 26, 2018)

- Improve hygiene of `@model` macro (#544).
- Fix bug in copy tests (#543).
- Fix bug in UniversalFallback attribute getter (#540).
- Allow all correct solutions for `solve_blank_obj` unit test (#537).
- Add errors for Allocate-Load and bad constraints (#534).
- \[performance\] Add specialized implementation of `hash` for `VariableIndex` (#533).
- \[performance\] Construct the name to object dictionaries lazily in model (#535).
- Add the `QuadtoSOC` bridge which transforms `ScalarQuadraticFunction`
  constraints into `RotatedSecondOrderCone` (#483).

## v0.6.1 (September 22, 2018)

- Enable `PositiveSemidefiniteConeSquare` set and quadratic functions
  in `MOIB.fullbridgeoptimizer` (#524).
- Add warning in the bridge between `PositiveSemidefiniteConeSquare` and
  `PositiveSemidefiniteConeTriangle` when the matrix is almost symmetric (#522).
- Modify `MOIT.copytest` to not add multiples constraints on the same variable
  (#521).
- Add missing keyword argument in one of `MOIU.add_scalar_constraint` methods
  (#520).

## v0.6.0 (August 30, 2018)

- The `MOIU.@model` and `MOIB.@bridge` macros now support functions and sets
  defined in external modules. As a consequence, function and set names in the
  macro arguments need to be prefixed by module name.
- Rename functions according to the [JuMP style guide](https://jump.dev/JuMP.jl/stable/developers/style/):
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
  about whether the element is not supported (for example, it cannot be copied a
  model containing this element) or the operation is not allowed (either
  because it is not implemented, because it cannot be performed in the current
  state of the model, or because it cannot be performed for a specific index)
- `canget` is removed. `NoSolution` is added as a result status to indicate
  that the solver does not have either a primal or dual solution available
  (See #479).

## v0.5.0 (August 5, 2018)

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

## v0.4.1 (June 28, 2018)

- Fixes vector function modification on 32 bits.
- Fixes Bellman-Ford algorithm for bridges.
- Added an NLP test with `FeasibilitySense`.
- Update modification documentation.

## v0.4.0 (June 23, 2018)

- Helper constructors for `VectorAffineTerm` and `VectorQuadraticTerm`.
- Added `modify_lhs` to `TestConfig`.
- Additional unit tests for optimizers.
- Added a type parameter to `CachingOptimizer` for the `optimizer` field.
- New API for problem modification (#388)
- Tests pass without deprecation warnings on Julia 0.7.
- Small fixes and documentation updates.

## v0.3.0 (May 25, 2018)

- Functions have been redefined to use arrays-of-structs instead of
  structs-of-arrays.
- Improvements to `MockOptimizer`.
- Significant changes to `Bridges`.
- New and improved unit tests.
- Fixes for Julia 0.7.

## v0.2.0 (April 24, 2018)

- Improvements to and better coverage of `Tests`.
- Documentation fixes.
- `SolverName` attribute.
- Changes to the NLP interface (new definition of variable order and arrays of
  structs for bound pairs and sparsity patterns).
- Addition of NLP tests.
- Introduction of `UniversalFallback`.
- `copynames` keyword argument to `MOI.copy!`.
- Add Bridges submodule.

## v0.1.0 (February 28, 2018)

- Initial public release.
- The framework for MOI was developed at the JuMP-dev workshop at MIT in June
  2017 as a sorely needed replacement for MathProgBase.
