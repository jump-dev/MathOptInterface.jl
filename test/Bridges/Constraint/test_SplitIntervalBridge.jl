# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module TestConstraintSplitInterval

using Test

import MathOptInterface as MOI

function runtests()
    for name in names(@__MODULE__; all = true)
        if startswith("$(name)", "test_")
            @testset "$(name)" begin
                getfield(@__MODULE__, name)()
            end
        end
    end
    return
end

include("../utilities.jl")

function test_split_basic()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    T = Float64
    bridged_mock = MOI.Bridges.Constraint.SplitInterval{T}(mock)
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_$(F)_$(S)" for F in [
                "VariableIndex",
                "ScalarAffineFunction",
                "ScalarQuadraticFunction",
            ] for S in ["Interval", "EqualTo"]
        ],
    )
    MOI.Test.runtests(
        bridged_mock,
        config,
        include = [
            "test_basic_$(F)_Zeros" for F in [
                "VectorOfVariables",
                "VectorAffineFunction",
                "VectorQuadraticFunction",
            ]
        ],
    )
    return
end

function test_linear_integration_Interval()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.SplitInterval{Float64}(mock)
    config = MOI.Test.Config()
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [5.0, 5.0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [2.5, 2.5],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 1.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [1],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.NONBASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [6.0, 6.0],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.NONBASIC],
            ],
            variable_basis_status = [MOI.BASIC, MOI.BASIC],
        ),
    )
    MOI.Test.test_linear_integration_Interval(bridged_mock, config)
    MOI.empty!(bridged_mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [0.0, 0.0],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [-1],
            constraint_basis_status = [
                (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [MOI.BASIC],
                (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) => [MOI.BASIC],
            ],
            variable_basis_status = [
                MOI.NONBASIC_AT_LOWER,
                MOI.NONBASIC_AT_LOWER,
            ],
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
                [0],
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
                [0],
        ),
    )
    MOI.Test.test_linear_Interval_inactive(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{Float64},
                MOI.Interval{Float64},
            }(),
        ),
    )
    vis = MOI.get(bridged_mock, MOI.ListOfVariableIndices())
    newf =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1.0, -1.0], vis), 0.0)
    MOI.set(bridged_mock, MOI.ConstraintFunction(), ci, newf)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ newf

    MOI.modify(bridged_mock, ci, MOI.ScalarCoefficientChange(vis[2], 1.0))
    modified_f =
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(ones(2), vis), 0.0)
    @test MOI.get(bridged_mock, MOI.ConstraintFunction(), ci) ≈ modified_f
    for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
        @test MOI.supports(bridged_mock, attr, typeof(ci))
        MOI.set(bridged_mock, attr, ci, 2.0)
        @test MOI.get(bridged_mock, attr, ci) == 2.0
        bridge = MOI.Bridges.bridge(bridged_mock, ci)
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(bridged_mock, attr, bridge.lower) == 2.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == 2.0
        else
            @test MOI.get(bridged_mock, attr, bridge.lower) == 2.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == 0.0
        end
        MOI.set(bridged_mock, attr, ci, -2.0)
        @test MOI.get(bridged_mock, attr, ci) == -2.0
        if attr isa MOI.ConstraintPrimalStart
            @test MOI.get(bridged_mock, attr, bridge.lower) == -2.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == -2.0
        else
            @test MOI.get(bridged_mock, attr, bridge.lower) == 0.0
            @test MOI.get(bridged_mock, attr, bridge.upper) == -2.0
        end
    end
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0),
            (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}, 0),
        ),
    )
    return
end

function _test_linear_FEASIBILITY_SENSE(T)
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.SplitInterval{T}(mock)
    MOI.Utilities.set_mock_optimize!(
        mock,
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [one(T), one(T)],
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}) =>
                zeros(T, 2),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}) => zeros(T, 1),
        ),
    )
    MOI.Test.test_linear_FEASIBILITY_SENSE(bridged_mock, MOI.Test.Config(T))
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.ScalarAffineFunction{T},
                MOI.EqualTo{T},
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        2,
        (
            (MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}, 1),
            (MOI.ScalarAffineFunction{T}, MOI.LessThan{T}, 0),
        ),
    )
    return
end

function test_linear_FEASIBILITY_SENSE()
    _test_linear_FEASIBILITY_SENSE(Float64)
    _test_linear_FEASIBILITY_SENSE(Int)
    return
end

function test_conic_linear_VectorOfVariables()
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    config = MOI.Test.Config()
    bridged_mock = MOI.Bridges.Constraint.SplitInterval{Float64}(mock)
    mock.optimize! =
        (mock::MOI.Utilities.MockOptimizer) -> MOI.Utilities.mock_optimize!(
            mock,
            [1.0, 0.0, 2.0],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives) =>
                [[0.0, 0.0]],
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
                [[-3, -1]],
        )
    MOI.Test.test_conic_linear_VectorOfVariables(bridged_mock, config)
    ci = first(
        MOI.get(
            bridged_mock,
            MOI.ListOfConstraintIndices{
                MOI.VectorAffineFunction{Float64},
                MOI.Zeros,
            }(),
        ),
    )
    _test_delete_bridge(
        bridged_mock,
        ci,
        3,
        (
            (MOI.VectorAffineFunction{Float64}, MOI.Nonnegatives, 0),
            (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives, 0),
        ),
    )
    return
end

function _test_interval(
    mock,
    bridged_mock,
    set::MOI.Interval{T},
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{T}},
) where {T}
    haslb = set.lower != typemin(T)
    hasub = set.upper != typemax(T)
    @test MOI.Bridges.is_bridged(bridged_mock, ci)
    bridge = MOI.Bridges.bridge(bridged_mock, ci)
    if haslb
        @test bridge.lower !== nothing
        MOI.set(mock, MOI.ConstraintBasisStatus(), bridge.lower, MOI.BASIC)
    else
        @test bridge.lower === nothing
    end
    if hasub
        @test bridge.upper !== nothing
        MOI.set(mock, MOI.ConstraintBasisStatus(), bridge.upper, MOI.BASIC)
    else
        @test bridge.upper === nothing
    end
    @test set == MOI.get(bridged_mock, MOI.ConstraintSet(), ci)
    attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.GreaterThan{T}}()
    @test 0 == MOI.get(bridged_mock, attr)
    @test (haslb ? 1 : 0) == MOI.get(mock, attr)
    attr = MOI.NumberOfConstraints{MOI.VariableIndex,MOI.LessThan{T}}()
    @test 0 == MOI.get(bridged_mock, attr)
    @test (hasub ? 1 : 0) == MOI.get(mock, attr)
    attr = MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.GreaterThan{T}}()
    @test isempty(MOI.get(bridged_mock, attr))
    @test (haslb ? 1 : 0) == length(MOI.get(mock, attr))
    attr = MOI.ListOfConstraintIndices{MOI.VariableIndex,MOI.LessThan{T}}()
    @test isempty(MOI.get(bridged_mock, attr))
    @test (hasub ? 1 : 0) == length(MOI.get(mock, attr))
    if haslb || hasub
        @test one(T) == MOI.get(bridged_mock, MOI.ConstraintPrimal(), ci)
        @test MOI.BASIC ==
              MOI.get(bridged_mock, MOI.ConstraintBasisStatus(), ci)
        for attr in [MOI.ConstraintPrimalStart(), MOI.ConstraintDualStart()]
            MOI.set(bridged_mock, attr, ci, T(2))
            @test T(2) == MOI.get(bridged_mock, attr, ci)
        end
    else
        for attr in [
            MOI.ConstraintPrimal(),
            MOI.ConstraintPrimalStart(),
            MOI.ConstraintBasisStatus(),
        ]
            err = MOI.GetAttributeNotAllowed{typeof(attr)}
            @test_throws err MOI.get(bridged_mock, attr, ci)
        end
        @test zero(T) == MOI.get(bridged_mock, MOI.ConstraintDualStart(), ci)
    end
    @test zero(T) == MOI.get(bridged_mock, MOI.ConstraintDual(), ci)
end

function test_infinite_bounds(::Type{T} = Float64) where {T<:AbstractFloat}
    mock = MOI.Utilities.MockOptimizer(
        MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}()),
    )
    bridged_mock = MOI.Bridges.Constraint.SplitInterval{T}(mock)
    x = MOI.add_variable(bridged_mock)
    MOI.set(mock, MOI.VariablePrimal(), x, one(T))
    set = MOI.Interval(typemin(T), one(T))
    ci = MOI.add_constraint(bridged_mock, x, set)
    _test_interval(mock, bridged_mock, set, ci)
    set = MOI.Interval(zero(T), typemax(T))
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, set)
    _test_interval(mock, bridged_mock, set, ci)
    set = MOI.Interval(typemin(T), typemax(T))
    MOI.set(bridged_mock, MOI.ConstraintSet(), ci, set)
    _test_interval(mock, bridged_mock, set, ci)
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.VariableIndex, MOI.GreaterThan{T}, 0),
            (MOI.VariableIndex, MOI.LessThan{T}, 0),
        ),
    )
    ci = MOI.add_constraint(bridged_mock, x, set)
    _test_interval(mock, bridged_mock, set, ci)
    _test_delete_bridge(
        bridged_mock,
        ci,
        1,
        (
            (MOI.VariableIndex, MOI.GreaterThan{T}, 0),
            (MOI.VariableIndex, MOI.LessThan{T}, 0),
        ),
    )
    set = MOI.Interval(zero(T), typemax(T))
    ci = MOI.add_constraint(bridged_mock, x, set)
    return _test_interval(mock, bridged_mock, set, ci)
end

function test_runtests()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        x in Interval(1.0, 2.0)
        """,
        """
        variables: x
        x >= 1.0
        x <= 2.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        3.0 * x in Interval(1.0, 2.0)
        """,
        """
        variables: x
        3.0 * x >= 1.0
        3.0 * x <= 2.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        x in Interval(1.0, Inf)
        """,
        """
        variables: x
        x >= 1.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        x in Interval(-Inf, 2.0)
        """,
        """
        variables: x
        x <= 2.0
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        x in Interval(1 // 2, 3 // 2)
        """,
        """
        variables: x
        x in GreaterThan(1 // 2)
        x in LessThan(3 // 2)
        """,
        eltype = Rational{Int},
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        x in Interval(1 // 2, 1 // 0)
        """,
        """
        variables: x
        x in GreaterThan(1 // 2)
        """,
        eltype = Rational{Int},
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        x in Interval(-1 // 0, 1 // 2)
        """,
        """
        variables: x
        x in LessThan(1 // 2)
        """,
        eltype = Rational{Int},
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        ScalarNonlinearFunction(log(x)) in Interval(1.0, 2.0)
        """,
        """
        variables: x
        ScalarNonlinearFunction(log(x)) >= 1.0
        ScalarNonlinearFunction(log(x)) <= 2.0
        """,
    )
    return
end

function test_runtests_vector()
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x
        [x] in Zeros(1)
        """,
        """
        variables: x
        [x] in Nonnegatives(1)
        [x] in Nonpositives(1)
        """,
    )
    MOI.Bridges.runtests(
        MOI.Bridges.Constraint.SplitIntervalBridge,
        """
        variables: x, y
        [1.0 * x + 2.0, x + y] in Zeros(2)
        """,
        """
        variables: x, y
        [1.0 * x + 2.0, x + y] in Nonnegatives(2)
        [1.0 * x + 2.0, x + y] in Nonpositives(2)
        """,
    )
    return
end

function test_get_function()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SplitInterval{Float64}(inner)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.Interval(-Inf, Inf))
    @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ 1.0 * x
    return
end

function test_modify_set()
    inner = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}())
    model = MOI.Bridges.Constraint.SplitInterval{Float64}(inner)
    x = MOI.add_variable(model)
    c = MOI.add_constraint(model, 1.0 * x, MOI.Interval(-Inf, Inf))
    for set in [
        MOI.Interval(-Inf, Inf),
        MOI.Interval(1.0, 2.0),
        MOI.Interval(2.0, 3.0),
        MOI.Interval(-Inf, Inf),
    ]
        MOI.set(model, MOI.ConstraintSet(), c, set)
        @test MOI.get(model, MOI.ConstraintSet(), c) == set
        @test MOI.get(model, MOI.ConstraintFunction(), c) ≈ 1.0 * x
    end
    return
end

end  # module

TestConstraintSplitInterval.runtests()
