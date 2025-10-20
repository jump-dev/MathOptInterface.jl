```@meta
CurrentModule = MathOptInterface
```

# Release notes

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## v1.46.0 (October 21, 2025)

### Added

 - Added [`VectorNonlinearOracle`](@ref) (#2860)

### Fixed

 - Fixed [`Bridges.Constraint.IndicatorToMILPBridge`](@ref) when the `z` variable
   was not binary (#2857), (#2868)
 - Fixed an `error` to [`GetAttributeNotAllowed`](@ref) in
   [`Bridges.Variable.ZerosBridge`](@ref) (#2863)
 - Fixed various bridges to throw MOI error subtypes instead of `Base.error()`
   (#2866)

### Other

 - Documentation updates (#2855), (#2858), (#2864)
 - Added `@nospecialize` to some methods (#2830)

## v1.45.0 (September 20, 2025)

### Added

 - Added [`Bridges.Objective.ToScalarNonlinearBridge`](@ref) (#2834), (#2835)
 - Added support for querying [`ConstraintConflictStatus`](@ref) when
   the constraint was bridged (#2839)

### Fixed

 - Fixed a type instability in [`Utilities.set_dot`](@ref) (#2831)
 - Rewrote `Base.read!(::IO, ::FileFormats.LP.Model)` to use a proper recursive
   descent parser. This fixed numerous performance issues, and the resulting
   parser is simpler to maintain and extend. (#2840), (#2841), (#2842), (#2843),
   (#2844), (#2846), (#2847), (#2848), (#2852), (#2853)
 - Rewrote the error handling in `read!(::IO, ::FileFormats.MPS.Model)` to throw
   a `FileFormats.MPS.ParseError` (#2845), (#2849)

### Other

- Temporarily pinned `OpenSSL_jll` to work around an upstream bug (#2850), (#2854)

## v1.44.0 (September 4, 2025)

### Added

 - Added support for [`VariableName`](@ref) in variable bridges (#2822)

### Fixed

 - Fixed use of deprecated `IntDisjointSets` (#2824), (#2828)
 - Fixed `Utilities.get_fallback` for [`DualObjectiveValue`](@ref) with open
   intervals (#2823) (#2826)

## v1.43.0 (August 21, 2025)

### Added

 - Added [`ConflictCount`](@ref) and `conflict_index` to
   [`ConstraintConflictStatus`](@ref) (#2775), (#2800), (#2801)
 - Added [`Bridges.Constraint.IntervalToHyperRectangleBridge`](@ref) (#2754),
   (#2806), (#2809)

### Fixed

 - Fixed the MPS reader to support any whitespace as a field separator (#2798)
 - Fixed tests with duplicate names and added a CI test (#2804), (#2805)
 - Fixed parsing `x * x` as `x^2` in `Nonlinear.Model` (#2799)
 - Fixed a bug in [`Utilities.operate`](@ref) with quadratic outputs when a
   `Integer` coefficient differs from the machine `Integer` (#2807)
 - Fixed `MOI.supports` of `MOI.ObjectiveFunction` in all file formats (#2814)
 - Fixed free rows in [`Bridges.Constraint.SplitHyperRectangleBridge`](@ref) (#2816)
 - Fixed deleting a variable with constraint bridges (#2818)
 - Fixed `Utilities.AbstractModel` with no constraints (#2819)

### Other

 - Added an OPF benchmark (#2739)
 - Updated to DataStructures@0.19 (#2796)
 - Filter `identity_bridge.jl` out from `runtests` (#2812)

## v1.42.1 (August 1, 2025)

### Fixed

 - Fixed reading MPS models with extra ROWS fields (#2793)

### Other

 - Added more packages to solver-tests.yml (#2785)
 - Added a test to MPS reader that duplicate entries are summed (#2787)
 - Fixed docstring of `inverse_trimap` (#2790)
 - Removed useless prefix in `set_dot` (#2791)

## v1.42.0 (July 10, 2025)

### Added

 - Added an option to disable warnings in [`Utilities.PenaltyRelaxation`](@ref)
   (#2774)

### Fixed

 - Fixed a bug writing objective constant in `MAX_SENSE` with `FileFormats.MPS`
   (#2778)
 - Fixed a change in how `==(::Expr, ::Expr)` works on for Julia nightly (#2780)
 - Fixed a performance bug in the Hessian computation of `Nonlinear.ReverseAD`
   (#2783)

## v1.41.0 (June 9, 2025)

### Added

 - Added support for different number types in the various file format readers
   and writers. By default, the files assume `Float64`. To change how the values
   are parsed, use `; coefficient_type = T`. The supported file formats are LP,
   MOF, MPS, and SDPA (#2768) (#2769) (#2770) (#2771) (#2772)

## v1.40.2 (June 2, 2025)

### Fixed

 - Fixed bug reading `.nl` files with `use_nlp_block = false` (#2766)

### Other

 - Improved error thrown in `ToMILPBridge` when variable is not bounded (#2764)

## v1.40.1 (May 20, 2025)

### Fixed

 - Fixed a missing [`dual_set`](@ref) for [`HermitianPositiveSemidefiniteConeTriangle`](@ref)
   (#2749)
 - Fixed [`Utilities.get_fallback`](@ref) of [`DualObjectiveValue`](@ref) with
   [`HyperRectangle`](@ref) (#2755)
 - Fixed reading SDPA files with `{}` punctuation (#2759)

### Other

 - Fixed a flakey doctest that depended on the runtime of a function (#2748)
 - Changed to use `Base.only` when appropriate (#2751)
 - Removed the experimental warning from Nonlinear module docstring (#2752)
 - Changed to use `throw_if_scalar_and_constant_not_zero` when appropriate (#2753)
 - Improved the text of `showerror` for `NotAllowedError` (#2757)

## v1.40.0 (May 4, 2025)

### Added

 - Added [`Utilities.distance_to_set`](@ref) for [`PositiveSemidefiniteConeTriangle`](@ref)
   and [`PositiveSemidefiniteConeSquare`](@ref) (#2729)
 - Added [`Bridges.Constraint.HermitianToComplexSymmetricBridge`](@ref) (#2724)

### Fixed

 - Fixed Bridge tests for non-invertible constraint bridge (#2713)
 - Fixed bridge weights for SOS(1|2)ToMILPBridge (#2723)
 - Fixed `Bridges.Constraint.SetMapBridge` with `Complex` number types (#2733)

### Other

 - Clarified comment about binary format in `FileFormats.NL` (#2720)
 - Added more packages to solver-tests.yml (#2721)
 - Removed `@show` from test of MockOptimizer (#2725)
 - Added `@testset` to [`Bridges.runtests`](@ref) (#2726), (#2734)
 - Updated JuliaFormatter to v2 (#2731)
 - Various improvements to `Nonlinear.ReverseAD` (#2730), (#2736), (#2737),
   (#2738), (#2740), (#2744), (#2745)
 - Bumped Julia version to v1.10 for documentation (#2735)
 - Fixed tests on nightly (#2742)
 - Added import MathOptInterface as MOI to DocTestSetup (#2746)

## v1.39.0 (April 10, 2025)

### Added

 - Added `test_module` keyword to [`Test.runtests`](@ref) (#2710)
 - Added support for reading the binary `b` format in `FileFormats.Nonlinear`
   (#2718)

### Fixed

 - Fixed [`ListOfVariableIndices`](@ref) returned by
   [`Bridges.LazyBridgeOptimizer`](@ref) when there is a chain of variable
   bridges (#2716)

### Other

 - Updated `solver-tests.yml` (#2715)

## v1.38.1 (April 7, 2025)

### Fixed

 - Fixed a redundant constraint in [`Bridges.Constraint.CircuitToMILPBridge`](@ref)
   (#2694)
 - Fixed the ordering of [`ListOfVariableIndices`](@ref) returned by
   [`Bridges.LazyBridgeOptimizer`](@ref) (#2695)
 - Fix `Test.test_nonlinear_constraint_log` to exclude undefined points (#2704)

### Other

 - Improve various docstrings (#2698), (#2699), (#2700), (#2701), (#2712)
 - Add support for `ForwardDiff@1` (#2703), (#2708)
 - Update `solver-tests.yml` (#2697), (#2709)
 - Add a `@test_broken` for issue #2696 (#2705)

## v1.38.0 (March 13, 2025)

### Added

 - Added the `Nonlinear.SymbolicAD` submodule (#2624), (#2685)

### Fixed

 - Fixed a bug in `Utilities.operate(vcat, ) -> VectorNonlinearFunction` which
   previously did not ensure that the returned function could be mutated (#2682)
 - Fixed `get` for [`ConstraintFunction`](@ref) of
   [`Bridges.Constraint.SplitHyperRectangleBridge`](@ref) to not add spurious
   `+0` and `-0` (#2681)
 - Fixed `test_basic_` tests to use [`Nonlinear.SymbolicAD.simplify`](@ref) when
   comparing constraint functions. This fixes some tests with
   [`VectorNonlinearFunction`](@ref) that failed because the bridge
   reformulations were not recognized as being equivalent (#2686)
 - Fixed [`FileFormats.MOF.Model`](@ref) to use `use_nlp_block = false` by
   default if the model contains [`ScalarNonlinearFunction`](@ref). This change
   could be regarded as technically breaking because writing and reading a model
   with [`ScalarNonlinearFunction`](@ref) used to return a [`NLPBlock`](@ref),
   but now it reads functions as the expected [`ScalarNonlinearFunction`](@ref)
   (#2688)
 - Fixed [`Test.version_added`](@ref) for a number of tests that were added in
   recent versions (#2690), (#2691)

### Other

 - Refactor some tests in `Bridges` (#2684)

## v1.37.2 (March 4, 2025)

### Fixed

 - Fixed a bug introduced by (#2661) in which various `ToMILP` bridges were
   broken if [`delete`](@ref) was called before [`Utilities.final_touch`](@ref)
   (#2678)
 - Fixed a bug introduced by (#2665) in which [`attribute_value_type`](@ref) was
   too strict for existing implementations of [`Bridges.ListOfNonstandardBridges`](@ref)
   (#2679)

## v1.37.1 (March 3, 2025)

### Fixed

 - Fixed [`modify`](@ref) with [`MultirowChange`](@ref) in
   [`Bridges.Constraint.SetMapBridge`](@ref) (#2662)
 - Fixed [`attribute_value_type`](@ref) for [`Bridges.ListOfNonstandardBridges`](@ref)
   (#2665)

### Other

 - This release contains a large number of pull requests that increase test
   coverage (#2657), (#2658), (#2659), (#2660), (#2661), (#2663), (#2670),
   (#2671), (#2672), (#2675)
 - Changed [`Bridges.Variable.ParameterToEqualToBridge`](@ref) to be a subtype
   of [`Bridges.Variable.SetMapBridge`](@ref) (#2664)
 - Improved parallelism in the CI tests (#2674)

## v1.37.0 (February 24, 2025)

### Added

 - Added constructors for [`VectorAffineFunction`](@ref) and [`VectorQuadraticFunction`](@ref)
   that accept `AbstractVector{MOI.ScalarAffineFunction}` and
   `AbstractVector{MOI.ScalarQuadraticFunction}` respectively (#2636)

### Fixed

 - Fixed a bug reading `QCMATRIX` in `FileFormats.MPS` (#2628)
 - Fixed reading [`VariablePrimalStart`](@ref), [`ConstraintPrimalStart`](@ref),
   and [`ConstraintDualStart`](@ref) from `FileFormats.MOF` (#2652)
 - Fixed writing unsupported variable types in a number of file formats (#2654)

### Other

 - Removed the "experimental" warning from `Nonlinear` (#2625)
 - This release contains a large number of pull requests that increase test
   coverage (#2630), (#2631), (#2632), (#2637), (#2638), (#2639), (#2640),
   (#2641), (#2642), (#2644), (#2645), (#2646), (#2647), (#2648), (#2649),
   (#2650)
 - Fixed link to the coverage badge (#2634)
 - Removed the mention of MathProgBase from the README (#2635)
 - Removed `precompile` statements. Packages using MOI should address
   precompilation using `PrecompileTools.jl` in their own package (#2643)
 - Removed unneeded `Unicode` dependency (#2656)

## v1.36.0 (February 12, 2025)

### Added

 - Added support for [`Indicator`](@ref) constraints in [`Utilities.Model`](@ref)
   (#2618) (#2619)
 - Added features to improve performance of evaluating univariate operators in
   `Nonlinear` (#2620):
   * Added [`Nonlinear.eval_univariate_function_and_gradient`](@ref)
   * [`Nonlinear.eval_univariate_function`](@ref), [`Nonlinear.eval_univariate_gradient`](@ref),
     and [`Nonlinear.eval_univariate_hessian`](@ref) now support calls with
     the `Integer` index of the operator instead of requiring a `Symbol`

### Other

 * Removed `mutable` from the struct definition of
   `Nonlinear.ReverseAD._FunctionStorage` and
   `Nonlinear.ReverseAD._SubexpressionStorage` (#2621)
 * Fixed docstrings in the `Coloring` submodule (#2622)

## v1.35.2 (January 29, 2025)

### Fixed

 - Fixed including non-`.jl` files in `Bridges` (#2615)

## v1.35.1 (January 17, 2025)

### Fixed

 - Fixed `Base.copy(::MOI.ScalarNonlinearFunction)` (#2612)

### Other

 - Performance improvements to `FileFormats.MOF` reading and writing:
   - Use `NamedTuple` instead of `OrderedDict` (#2606) (#2607)
   - Use `JSON3` instead of `JSON` (#2613)

## v1.35.0 (January 9, 2025)

### Added

 - Added [`Bridges.Constraint.InequalityToComplementsBridge`](@ref). This
   bridge is not added by default (#2582)
 - Added [`Bridges.Constraint.ExponentialConeToScalarNonlinearFunctionBridge`](@ref).
   This bridge is not added by default (#2587)
 - Added support for `SetMap` bridges to use the value of a bridge in
   `map_function` instead of the type (#2198)

### Fixed

 - Fixed a bug querying result attributes in a bridge with `result_index != 1`
   (#2583)
 - Fixed various nonlinear tests to add a starting point (#2585)
 - Fixed a bug with `variable_start` in [`Bridges.runtests`](@ref) (#2592)
 - Fixed `test_basic_` tests for [`Indicator`](@ref) sets to make the first
   variable [`ZeroOne`](@ref) (#2600)
 - Changed the weights of the `SOCtoPSD` and `RSOCtoPSD` bridges so that they
   are used only if necessary. Previously, these depended on the order in which
   they were added to a `LazyBridgeOptimizer` (#2596) (#2598) (#2599)

### Other

 - Added more tests for nonlinear programs (#2584)
 - Renamed files in `Bridges` to reflect the type of the bridge (#2586)
 - Added more tests for [`DualObjectiveValue`](@ref) (#2588)
 - Added a test for multiple PSD variables in the same constraint (#2594)

## v1.34.0 (November 7, 2024)

### Added

 - Added [`Bridges.Constraint.SetConversionBridge`](@ref) (#2536)
 - Added a new method to [`add_constrained_variable`](@ref) to support passing a
   tuple of [`GreaterThan`](@ref) and [`LessThan`](@ref). This is intended to be
   a fast way of adding scalar variables with both lower and upper bounds (#2574)

### Fixed

 - Fixed error when adding unsupported constraints to `Utilities.AbstractModel`
   (#2572)
 - Fixed creating [`Interval`](@ref), [`Semicontinuous`](@ref) and [`Semiinteger`](@ref)
   if the bound values are different types (#2577)
 - Attributes are now ignored in `ListOfXXXAttributesSet` in
   [`Utilities.UniversalFallback`](@ref) if they are set to `nothing` (#2575)

### Other

 - Updated `solver-tests.yml` (#2578)
 - Improved various docstrings (#2579)

## v1.33.0 (October 28, 2024)

### Added

 - Added support for specifying the lower and upper bound suffixes for variable
   duals in [`FileFormats.NL.SolFileResults`](@ref) (#2567)

### Fixed

 - Fixed `MOI.objective_expr(::InvalidEvaluator)` (#2569)

## v1.32.0 (October 21, 2024)

### Added

 - Added support for user-defined univariate operators which do not support
   second derivatives (#2542)
 - Added the [`NodeLimit`](@ref) attribute (#2552)
 - Added support for `Utilities.filter_variables` with
   [`VectorNonlinearFunction`](@ref) (#2556)

### Fixed

 - Fixed [`Bridges.Constraint.TableToMILPBridge`](@ref) if there is a constant
   in the function (#2544)
 - Fixed `FileFormats.MPS` and `FileFormats.LP` to read separate variable bounds
   instead of an [`Interval`](@ref) set (#2548)
 - Fixed the variable domains in `Test.test_nonlinear_expression_hs110` (#2551)
 - Fixed the dimension of `VectorNonlinearFunction` in tests (#2555)
 - Fixed the use of `1:length(x)` in `Nonlinear.Coloring` (#2559)
 - Fixed an index conflict in `Nonlinear.Coloring` (#2561)

### Other

 - Documentation fixes (#2541) (#2543) (#2545) (#2557) (#2560)
 - Formatting improvements (#2554)

## v1.31.2 (August 30, 2024)

### Fixed

 - Fixed a bug writing `FileFormats.MPS` files with `MAX_SENSE` and quadratic
   objectives (#2539)

### Other

 - Improved the docstrings in `src/nlp.jl` (#2537)

## v1.31.1 (August 7, 2024)

### Fixed

 - Fixed `NLPBlock` when used with bridges (#2524)

### Other

 - Use `inverse_trimap` instead of redefining it (#2522)
 - Improved `UnsupportedConstraint` error (#2530)

## v1.31.0 (June 26, 2024)

### Added

 - Added default `show(::IO, ::ModelLike)` method (#2505) (#2510)
 - Set map bridges can now implement the various `map_function` methods to use
   the value of the bridge as the first argument, instead of passing the type of
   the bridge. (#2509)
 - Added `cannot_unbridge` argument to [`Bridges.runtests`](@ref)

### Fixed

 - Fixed [`supports_constraint`](@ref) for `IndicatorSOS1Bridge` (#2507)
 - Fixed getting [`ConstraintDual`](@ref) in slack bridges (#2508) (#2514)
   (#2515)
 - Fixed `FileFormats.NL` to read linear constraints as
   [`ScalarAffineFunction`](@ref) (#2512)
 - Changed `default_copy_to` to maintain the order of variables during `copy_to`.
   This change is marked as non-breaking, but it may cause different (but
   mathematically equivalent) models to be formulated, particularly conic models
   with bridges. (#2495) (#2520)

### Other

 - Updated `solver-tests.yml` (#2516) (#2518)

## v1.30.0 (May 23, 2024)

### Added

 - Added input and output functions to `Bridges.runtests` (#2497)

### Fixed

 - Improved the heuristic of when to write variable cones in `FileFormats.CBF`
   (#2494)
 - Fixed `Bridges.runtests` when model has no variable (#2499)
 - Fixed getting an attribute of an empty vector (#2501)

### Other

 - Fixed links in the documentation (#2502)

## v1.29.0 (April 19, 2024)

### Added

 - Added support for [`Indicator`](@ref) constraints in `FileFormats.LP` (#2483)

### Fixed

 - The CBF writer now attempts to write [`VectorOfVariables`](@ref) constraints
   in the `VAR` section, instead of automatically promoting them to
   [`VectorAffineFunction`](@ref). (#2478) (#2482) (#2486)
 - Fixed a performance issue adding [`ScalarAffineFunction`](@ref) and
   [`ScalarQuadraticFunction`](@ref)s to [`Nonlinear.Model`](@ref) (@2487)
 - Fixed a bug reading MPS files with integer variables and an entry in the
   `BOUNDS` section (#2490)
 - Fixed the MPS writer to use `LI` and `UI` instead of `LO` and `UP` for integer
   variables (#2492)

### Other

 - Updated versions in CI (#2484) (#2489)
 - Fixed duplicate names in tests (#2485)

## v1.28.1 (April 13, 2024)

### Fixed

 - Fixed a bug reading MPS files with integer variables and no corresponding
   entry in the `BOUNDS` section (#2480)

## v1.28.0 (April 11, 2024)

### Added

 - Added [`Bridges.Constraint.ComplexNormInfinityToSecondOrderConeBridge`](@ref) (#2451)

### Fixed

 - Fixed a correctness bug getting the set of a constraint that used both
   variable and constraint bridges (#2464) (#2472)
 - Fixed `MethodError` in some bridges when called with `Complex`-valued
   functions (#2468) (#2475)
 - Fixed reading MPS files that use `*` as the start of a name and not as a
   comment (#2470)

### Other

 - Updated `solver-tests.yml` (#2465)
 - Removed two unused methods from `MOI.Bridges` (#2466)
 - Documentation updates (#2467), (#2473), (#2474)
 - Simplify reading CBF files (#2476)

## v1.27.1 (March 27, 2024)

### Fixed

 - Fixed passing non-`IndexMap` in `Utilities.pass_attributes` (#2458)
 - Fixed getting `MOI.ListOfConstraintAttributesSet` for `VectorOfConstraints`
   (#2459)

### Other

 - Updated `solver-tests.yml` (#2453) (#2455)
 - Fixed path in error message (#2461)

## v1.27.0 (February 27, 2024)

### Added

 - Added support for `sign(x)` in `Nonlinear` (#2444)

### Fixed

 - Fixed [`copy_to`](@ref) to `FileFormats.NL` when [`Name`](@ref) is set (#2446)

## v1.26.0 (February 22, 2024)

### Added

 - Added `.initialize_timer` to [`Nonlinear.Evaluator`](@ref) (#2438)

### Fixed

 - Fixed writing binary variables with bounds in `FileFormats.MPS` (#2431)
 - Fixed parsing suffixes in `FileFormats.NL` (#2436)
 - Fixed writing free constraints in `FileFormats.NL` (#2437)
 - Fixed potential for unsafe out-of-bounds write in Hessian evaluation (#2441)

### Other

 - Fixed formatting of [`BasisStatusCode`](@ref) docstring (#2430)
 - Refactor `test/Fileformats/MPS/MPS.jl` (#2433)

## v1.25.3 (February 14, 2024)

### Fixed

 - Fixed number type in `get_fallback` (#2414)
 - Fixed error type thrown when a variable bridge cannot un-bridge the
   function. It used to throw `ErrorException`. It now throws `MOI.GetAttributeNotAllowed{MOI.ConstraintFunction}`.
   This enables `Utilities.CachingOptimizer` to more uniformly implement
   fallbacks for common bridges like [`Bridges.Variable.ZerosBridge`](@ref).
   (#2415)
 - Fixed tests on upcoming Julia v1.11 (#2428)

### Other

 - Improved performance of [`Bridges.Constraint.CountDistinctToMILPBridge`](@ref)
   (#2416)
 - Improved performance of `FileFormats.MPS` writer (#2421) (#2424) (#2426)
 - Updated `solver-tests.yml` (#2423)
 - Fixed typos in `src/attributes.jl` (#2429)

## v1.25.2 (January 29, 2024)

### Fixed

 - Fixed getting [`ConstraintPrimal`](@ref) if variable bridges are present
   (#2396)
 - Fixed `modify_function!` for [`ScalarQuadraticCoefficientChange`](@ref)
   (#2408)
 - Fixed writing `FileFormats.MOF` files if [`ScalarNonlinearFunction`](@ref)
   contains [`ScalarAffineFunction`](@ref) or [`ScalarQuadraticFunction`](@ref)
   (#2409)

### Other

 - Clarified ordering of nonlinear tape in documentation (#2401)
 - Updated vale.sh (#2403) (#2404)
 - Tidied `src/Utilities/results.jl (#2411)

## v1.25.1 (January 11, 2024)

### Fixed

 - Fixed `map_indices` for `AbstractAutomaticDifferentiation` (#2394)
 - Fixed deleting a variable in [`Bridges.Variable.VectorizeBridge`](@ref)
   (#2393)
 - Fixed super type of [`LowerBoundAlreadySet`](@ref) and [`UpperBoundAlreadySet`](@ref)
   (#2397)

### Other

 - Removed a duplicated test (#2395)

## v1.25.0 (January 5, 2024)

### Added

 - Added the `AutomaticDifferentiationBackend` attribute (#2386)

### Fixed

 - Fixed [`initialize`](@ref) for [`Nonlinear.ExprGraphOnly`](@ref) (#2387)
 - Fixed converting 0-valued [`ScalarAffineFunction`](@ref) and [`ScalarQuadraticFunction`](@ref)
   to [`ScalarNonlinearFunction`](@ref) (#2388)
 - Fixed reading `.nl` files with non-empty variable and constraint names (#2390)
 - Fixed reading `.nl` files with no objective (#2391)
 - Fixed reading `.nl` files with free ranged constraints (#2392)

## v1.24.0 (January 2, 2024)

### Added

 - Added `get_fallback` for `ConstraintDual` of variable bounds (#2373)

### Fixed

 - Fixed `RSOCtoPSDBridge` for dimension 2 (#2359)
 - Fixed getting `ConstraintFunction` in conversion bridge (#2360)
 - Fixed `map_indices` (#2367)
 - Fixed `SlackBridgePrimalDualStart` for non-slack bridges (#2365)
 - Fixed `test_attribute_TimeLimitSec` (#2370)
 - Fixed order of model attributes during `copy_to` (#2372)
 - Fixed `ConstraintIndex` conflicts between variable and constraint bridges
   (#2362)
 - Fixed corner-case deletion in bridges (#2377)
 - Fixed `ListOfVariablesWithAttributeSet` for variable bridges (#2380)
 - Fixed `SlackBridge` if scalar constant is not zero (#2382)
 - Fixed setting multiple bounds on a bridged variable (#2383)

### Other

 - Minor documentation improvements (#2355), (#2374)
 - Improved `side_dimension_for_vectorized_dimension` (#2356)
 - Added DiffOpt and ParametricOptInterface to `solver-tests.yml` (#2368)
 - Refactored `SDPAModel` into a separate test file and test more widely
   (#2364), (#2357)

## v1.23.0 (November 29, 2023)

### Added

 - Added [`ConstraintPrimalStart`](@ref) for [`Bridges.Constraint.GeoMeanBridge`](@ref)
   (#2348)
 - Added `verbose` keyword argument to [`Test.runtests`](@ref) (#2347)
 - Added [`Bridges.FirstBridge`](@ref) attribute (#2211)

### Fixed

 - Fixed `Test.test_model_Name_VariableName_ConstraintName` (#2349)
 - Fixed errors thrown when querying an attribute is not supported, like when
   [`Utilities.CachingOptimizer`](@ref) is not attached to an optimizer (#2350)

## v1.22.0 (November 6, 2023)

### Added

 - Added new bridges (#2318)
   - [`Bridges.Constraint.SOS1ToMILPBridge`](@ref)
   - [`Bridges.Constraint.SOS2ToMILPBridge`](@ref)
   - [`Bridges.Constraint.IndicatorToMILPBridge`](@ref)
 - Added starting values for bridges (#2330) (#2337) (#2338) (#2339)
   - [`Bridges.Constraint.NumberConversionBridge`](@ref)
   - [`Bridges.Constraint.SquareBridge`](@ref)
   - [`Bridges.Variable.HermitianToSymmetricPSDBridge`](@ref)
   - [`Bridges.Variable.ParameterToEqualToBridge`](@ref)
   - [`Bridges.Variable.RSOCtoPSDBridge`](@ref)
 - Added [`ListOfVariablesWithAttributeSet`](@ref) and
   [`ListOfConstraintsWithAttributeSet`](@ref) (#2331) (#2342)
 - Added support for printing [`ConstraintName`](@ref) when models are printed
   (#2336)

### Fixed

 - [`ScalarFunctionConstantNotZero`](@ref) errors are now skipped in
   [`Test.runtests`](@ref) (#2325)
 - Fixed [`VectorNonlinearFunction`](@ref) support in [`Bridges.Constraint.NormOneBridge`](@ref)
   and [`Bridges.Constraint.NormInfinityBridge`](@ref) (#2324)
 - Fixed various `get` for [`ConstraintFunction`](@ref) to return a copy, and
   added tests (#2328)
 - Fixed `supports` of [`ConstraintPrimalStart`](@ref) and [`VariablePrimalStart`](@ref)
   for some bridges (#2340)

### Other

 - Fixed typo in docstring of [`ScalarAffineFunction`](@ref) (#2326)
 - Added Gurobi to `solver-tests.yml` (#2332)
 - Improved the error message when a bridge does not support an attribute (#2329)
 - Improved documentation for implementing a bridge (#2334)
 - Updated `[compat]` bounds in `Project.toml` (#2344)

## v1.21.0 (October 25, 2023)

### Added

 - Added [`SolutionLimit`](@ref) attribute (#2291)
 - Added support for MathOptFormat v1.6 and v1.7 (#2293) (#2298) (#2299) (#2321)
 - Added [`ScalarQuadraticCoefficientChange`](@ref) (#2296) (#2320) (#2322)
 - Added [`Utilities.modify_constants`](@ref) (#2300)
 - Added support for [`is_empty`](@ref) and [`empty!`](@ref) of
   [`Nonlinear.Model`](@ref) (#2305)
 - Added [`Bridges.Objective.FunctionConversionBridge`](@ref) (#2303)
 - Added `Bridges.ModifyBridgeNotAllowed` (#2307)
 - Added more sets for [`Utilities.distance_to_set`](@ref) (#2314)

### Fixed

 - Fixed `test_attribute_TimeLimitSec` (#2256)
 - Fixed conversion cost in objective bridges (#2309)

### Other

 - Fixed flakey link checking in documentation (#2297)
 - Minor documentation fixes (#2304) (#2313)
 - Fixed `[compat]` section of `Project.toml` (#2310)
 - Added MultiObjectiveAlgorithms to `solver-tests.yml` (#2312)

## v1.20.1 (September 24, 2023)

### Fixed

 - Removed incorrect `kwargs` in some `copy_to` methods (#2272)
 - Fixed [`ConstraintDualStart`](@ref) for [`Bridges.Constraint.SplitIntervalBridge`](@ref)
   (#2275)
 - Fixed `MethodError` when trying to modify a variable objective (#2278)
 - Fixed stack-overflow in `Utilities.operate(+,...)` with many arguments (#2285) (#2290)

### Other

 - Added MathOptSetDistances to `solver-tests.yml` (#2265)
 - Updated Documenter (#2266)
 - Fixed various JET errors (#2267) (#2269) (#2270) (#2271) (#2276) (#2277) (#2289)
 - Various style improvements
   - Replaced `using Package` with `import Package` where possible (#2274)
   - Removed `Utilities.EMPTYSTRING` (#2283)
   - Removed unnecessary `const` acronyms in `Utilities` (#2280) (#2281)
   - Removed invalid and unused method (#2286)
 - Refactored `src/Utilities/model.jl` (#2287)

## v1.20.0 (September 7, 2023)

### Added

 - Added [`Scaled`](@ref) set (#2237) (#2263) (#2264)
 - Added [`ObjectiveLimit`](@ref) attribute (#2257)

### Other

 - Updated dependencies (#2258)
 - Improved performance of [`ScalarNonlinearFunction`](@ref) utilities (#2259)
 - Fixed docstrings (#2261)

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
    - `ScaledPositiveSemidefiniteConeTriangle` (#2154)

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
 - Fixed the requirements check in `Test.test_constraint_get_ConstraintIndex`
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
