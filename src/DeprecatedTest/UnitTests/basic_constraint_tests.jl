# x
const dummy_single_variable =
    (x::Vector{MOI.VariableIndex}) -> MOI.SingleVariable(x[1])
# x₁, x₂
const dummy_vectorofvariables =
    (x::Vector{MOI.VariableIndex}) -> MOI.VectorOfVariables(x)
# 1.0 * x + 0.0
const dummy_scalar_affine =
    (x::Vector{MOI.VariableIndex}) ->
        MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(1.0, x), 0.0)
# 1.0 * x + 1.0 * x^2 + 0.0
const dummy_scalar_quadratic =
    (x::Vector{MOI.VariableIndex}) -> MOI.ScalarQuadraticFunction(
        MOI.ScalarAffineTerm.(1.0, x),
        MOI.ScalarQuadraticTerm.(1.0, x, x),
        0.0,
    )
# x₁ +    + 0.0
#    + x₂ + 0.0
const dummy_vector_affine =
    (x::Vector{MOI.VariableIndex}) -> MOI.VectorAffineFunction(
        MOI.VectorAffineTerm.(1:length(x), MOI.ScalarAffineTerm.(1.0, x)),
        zeros(Float64, length(x)),
    )
# x₁ +    + x₁^2        + 0.0
#    + x₂ +      + x₂^2 + 0.0
const dummy_vector_quadratic =
    (x::Vector{MOI.VariableIndex}) -> MOI.VectorQuadraticFunction(
        MOI.VectorAffineTerm.(1:length(x), MOI.ScalarAffineTerm.(1.0, x)),           # affine component
        MOI.VectorQuadraticTerm.(
            1:length(x),
            MOI.ScalarQuadraticTerm.(1.0, x, x),
        ),  # affine component
        zeros(Float64, length(x)),                                                    # constant term
    )

const BasicConstraintTests = Dict(
    (MOI.SingleVariable, MOI.LessThan{Float64}) =>
        (dummy_single_variable, 1, MOI.LessThan(1.0)),
    (MOI.SingleVariable, MOI.GreaterThan{Float64}) =>
        (dummy_single_variable, 1, MOI.GreaterThan(1.0)),
    (MOI.SingleVariable, MOI.EqualTo{Float64}) =>
        (dummy_single_variable, 1, MOI.EqualTo(1.0)),
    (MOI.SingleVariable, MOI.Interval{Float64}) =>
        (dummy_single_variable, 1, MOI.Interval(1.0, 2.0)),
    (MOI.SingleVariable, MOI.ZeroOne) =>
        (dummy_single_variable, 1, MOI.ZeroOne()),
    (MOI.SingleVariable, MOI.Integer) =>
        (dummy_single_variable, 1, MOI.Integer()),
    (MOI.SingleVariable, MOI.Semicontinuous{Float64}) =>
        (dummy_single_variable, 1, MOI.Semicontinuous(1.0, 2.0)),
    (MOI.SingleVariable, MOI.Semiinteger{Float64}) =>
        (dummy_single_variable, 1, MOI.Semiinteger(1.0, 2.0)),
    (MOI.VectorOfVariables, MOI.SOS1{Float64}) =>
        (dummy_vectorofvariables, 2, MOI.SOS1([1.0, 2.0])),
    (MOI.VectorOfVariables, MOI.SOS2{Float64}) =>
        (dummy_vectorofvariables, 2, MOI.SOS2([1.0, 2.0])),
    (MOI.VectorOfVariables, MOI.Zeros) =>
        (dummy_vectorofvariables, 2, MOI.Zeros(2)),
    (MOI.VectorOfVariables, MOI.Nonpositives) =>
        (dummy_vectorofvariables, 2, MOI.Nonpositives(2)),
    (MOI.VectorOfVariables, MOI.NonnegativeCone) =>
        (dummy_vectorofvariables, 2, MOI.NonnegativeCone(2)),
    (MOI.VectorOfVariables, MOI.NormInfinityCone) =>
        (dummy_vectorofvariables, 3, MOI.NormInfinityCone(3)),
    (MOI.VectorOfVariables, MOI.NormOneCone) =>
        (dummy_vectorofvariables, 3, MOI.NormOneCone(3)),
    (MOI.VectorOfVariables, MOI.SecondOrderCone) =>
        (dummy_vectorofvariables, 3, MOI.SecondOrderCone(3)),
    (MOI.VectorOfVariables, MOI.RotatedSecondOrderCone) =>
        (dummy_vectorofvariables, 3, MOI.RotatedSecondOrderCone(3)),
    (MOI.VectorOfVariables, MOI.GeometricMeanCone) =>
        (dummy_vectorofvariables, 3, MOI.GeometricMeanCone(3)),
    (MOI.VectorOfVariables, MOI.ExponentialCone) =>
        (dummy_vectorofvariables, 3, MOI.ExponentialCone()),
    (MOI.VectorOfVariables, MOI.DualExponentialCone) =>
        (dummy_vectorofvariables, 3, MOI.DualExponentialCone()),
    (MOI.VectorOfVariables, MOI.PowerCone{Float64}) =>
        (dummy_vectorofvariables, 3, MOI.PowerCone(0.5)),
    (MOI.VectorOfVariables, MOI.DualPowerCone{Float64}) =>
        (dummy_vectorofvariables, 3, MOI.DualPowerCone(0.5)),
    (MOI.VectorOfVariables, MOI.RelativeEntropyCone) =>
        (dummy_vectorofvariables, 3, MOI.RelativeEntropyCone(3)),
    (MOI.VectorOfVariables, MOI.NormSpectralCone) =>
        (dummy_vectorofvariables, 7, MOI.NormSpectralCone(2, 3)),
    (MOI.VectorOfVariables, MOI.NormNuclearCone) =>
        (dummy_vectorofvariables, 7, MOI.NormNuclearCone(2, 3)),
    (MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle) => (
        dummy_vectorofvariables,
        6,
        MOI.PositiveSemidefiniteConeTriangle(3),
    ),
    (MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeSquare) =>
        (dummy_vectorofvariables, 9, MOI.PositiveSemidefiniteConeSquare(3)),
    (MOI.VectorOfVariables, MOI.LogDetConeTriangle) =>
        (dummy_vectorofvariables, 8, MOI.LogDetConeTriangle(3)),
    (MOI.VectorOfVariables, MOI.LogDetConeSquare) =>
        (dummy_vectorofvariables, 11, MOI.LogDetConeSquare(3)),
    (MOI.VectorOfVariables, MOI.RootDetConeTriangle) =>
        (dummy_vectorofvariables, 7, MOI.RootDetConeTriangle(3)),
    (MOI.VectorOfVariables, MOI.RootDetConeSquare) =>
        (dummy_vectorofvariables, 10, MOI.RootDetConeSquare(3)),
    (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}) =>
        (dummy_scalar_affine, 1, MOI.LessThan(1.0)),
    (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) =>
        (dummy_scalar_affine, 1, MOI.GreaterThan(1.0)),
    (MOI.ScalarAffineFunction{Float64}, MOI.EqualTo{Float64}) =>
        (dummy_scalar_affine, 1, MOI.EqualTo(1.0)),
    (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}) =>
        (dummy_scalar_affine, 1, MOI.Interval(1.0, 2.0)),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.LessThan{Float64}) =>
        (dummy_scalar_quadratic, 1, MOI.LessThan(1.0)),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.GreaterThan{Float64}) =>
        (dummy_scalar_quadratic, 1, MOI.GreaterThan(1.0)),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.EqualTo{Float64}) =>
        (dummy_scalar_quadratic, 1, MOI.EqualTo(1.0)),
    (MOI.ScalarQuadraticFunction{Float64}, MOI.Interval{Float64}) =>
        (dummy_scalar_quadratic, 1, MOI.Interval(1.0, 2.0)),
    (MOI.VectorAffineFunction{Float64}, MOI.Zeros) =>
        (dummy_vector_affine, 2, MOI.Zeros(2)),
    (MOI.VectorAffineFunction{Float64}, MOI.Nonpositives) =>
        (dummy_vector_affine, 2, MOI.Nonpositives(2)),
    (MOI.VectorAffineFunction{Float64}, MOI.NonnegativeCone) =>
        (dummy_vector_affine, 2, MOI.NonnegativeCone(2)),
    (MOI.VectorAffineFunction{Float64}, MOI.Complements) =>
        (dummy_vector_affine, 2, MOI.Complements(2)),
    (MOI.VectorAffineFunction{Float64}, MOI.NormInfinityCone) =>
        (dummy_vector_affine, 3, MOI.NormInfinityCone(3)),
    (MOI.VectorAffineFunction{Float64}, MOI.NormOneCone) =>
        (dummy_vector_affine, 3, MOI.NormOneCone(3)),
    (MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone) =>
        (dummy_vector_affine, 3, MOI.SecondOrderCone(3)),
    (MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone) =>
        (dummy_vector_affine, 3, MOI.RotatedSecondOrderCone(3)),
    (MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone) =>
        (dummy_vector_affine, 3, MOI.GeometricMeanCone(3)),
    (MOI.VectorAffineFunction{Float64}, MOI.RelativeEntropyCone) =>
        (dummy_vector_affine, 3, MOI.RelativeEntropyCone(3)),
    (MOI.VectorAffineFunction{Float64}, MOI.NormSpectralCone) =>
        (dummy_vector_affine, 7, MOI.NormSpectralCone(2, 3)),
    (MOI.VectorAffineFunction{Float64}, MOI.NormNuclearCone) =>
        (dummy_vector_affine, 7, MOI.NormNuclearCone(2, 3)),
    (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeSquare) => (dummy_vector_affine, 9, MOI.PositiveSemidefiniteConeSquare(3)),
    (
        MOI.VectorAffineFunction{Float64},
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.LessThan{Float64}},
    ) => (
        dummy_vector_affine,
        2,
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.LessThan(3.0)),
    ),
    (
        MOI.VectorAffineFunction{Float64},
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE,MOI.GreaterThan{Float64}},
    ) => (
        dummy_vector_affine,
        2,
        MOI.IndicatorSet{MOI.ACTIVATE_ON_ONE}(MOI.GreaterThan(3.0)),
    ),
    (MOI.VectorQuadraticFunction{Float64}, MOI.Zeros) =>
        (dummy_vector_quadratic, 2, MOI.Zeros(2)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.Nonpositives) =>
        (dummy_vector_quadratic, 2, MOI.Nonpositives(2)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.NonnegativeCone) =>
        (dummy_vector_quadratic, 2, MOI.NonnegativeCone(2)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.NormInfinityCone) =>
        (dummy_vector_quadratic, 3, MOI.NormInfinityCone(3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.NormOneCone) =>
        (dummy_vector_quadratic, 3, MOI.NormOneCone(3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.SecondOrderCone) =>
        (dummy_vector_quadratic, 3, MOI.SecondOrderCone(3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.RotatedSecondOrderCone) =>
        (dummy_vector_quadratic, 3, MOI.RotatedSecondOrderCone(3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.GeometricMeanCone) =>
        (dummy_vector_quadratic, 3, MOI.GeometricMeanCone(3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.RelativeEntropyCone) =>
        (dummy_vector_quadratic, 3, MOI.RelativeEntropyCone(3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.NormSpectralCone) =>
        (dummy_vector_quadratic, 7, MOI.NormSpectralCone(2, 3)),
    (MOI.VectorQuadraticFunction{Float64}, MOI.NormNuclearCone) =>
        (dummy_vector_quadratic, 7, MOI.NormNuclearCone(2, 3)),
    (
        MOI.VectorQuadraticFunction{Float64},
        MOI.PositiveSemidefiniteConeSquare,
    ) => (dummy_vector_quadratic, 9, MOI.PositiveSemidefiniteConeSquare(3)),
)

"""
    basic_constraint_tests(model::MOI.ModelLike, config::Config;
        delete                  = true,
        get_constraint_function = true,
        get_constraint_set      = true,
        name::Bool              = true,
        include                 = Any[],
        exclude                 = Any[]
        )

Test basic constraint functionality for all function-in-set combinations that
are supported by `model`.

See also `basic_constraint_test_helper`.

If `delete = true`, it will test the deletion of constraints.
If `get_constraint_function = true`, it will test the getting of `ConstraintFunction`.
If `get_constraint_set = true`, it will test the getting of `ConstraintSet`.
If `name = true`, it will test the getting and setting of `ConstraintName`.

`include` and `exclude` can be used to run a subset of the tests, although only
one can be used in each call.

### Examples

    basic_constraint_tests(model, config; exclude = [
        (MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64})
    ])

    basic_constraint_tests(model, config;
        delete = false,
        include = [
            (MOI.VectorOfVariables, MOI.SOS2{Float64})
        ]
    )
"""
function basic_constraint_tests(
    model::MOI.ModelLike,
    config::Config;
    delete = true,
    get_constraint_function = true,
    get_constraint_set = true,
    name::Bool = true,
    include = Any[],
    exclude = Any[],
)
    if length(include) > 0 && length(exclude) > 0
        error("Don't use `include` and `exclude` together.")
    end
    test_keys =
        length(include) > 0 ? include :
        Iterators.filter(x -> !(x in exclude), keys(BasicConstraintTests))
    for (F, S) in test_keys
        if MOI.supports_constraint(model, F, S)
            @testset "$(F)-$(S)" begin
                (cf, N, set) = BasicConstraintTests[(F, S)]
                basic_constraint_test_helper(
                    model,
                    config,
                    cf,
                    set,
                    N;
                    delete = delete,
                    get_constraint_function = get_constraint_function,
                    get_constraint_set = get_constraint_set,
                    name = name,
                )
            end
        end
    end
end

variables(func::MOI.SingleVariable) = func.variable
variables(func::MOI.VectorOfVariables) = func.variables
function variables(func::MOI.ScalarAffineFunction)
    return Set(term.variable for term in func.terms)
end
function variables(func::MOI.VectorAffineFunction)
    return Set(term.scalar_term.variable for term in func.terms)
end
function variables(func::MOI.ScalarQuadraticFunction)
    return Set(term.variable for term in func.affine_terms) ∪
           Set(term.variable_1 for term in func.quadratic_terms)
    return Set(term.variable_2 for term in func.quadratic_terms)
end
function variables(func::MOI.VectorQuadraticFunction)
    return Set(term.scalar_term.variable for term in func.affine_terms) ∪
           Set(term.scalar_term.variable_1 for term in func.quadratic_terms)
    return Set(term.scalar_term.variable_2 for term in func.quadratic_terms)
end

"""
    basic_constraint_test_helper(
        model::MOI.ModelLike,
        config::Config,
        func::Function,
        set::MOI.AbstractSet,
        N::Int;
        delete::Bool                  = true,
        get_constraint_function::Bool = true,
        get_constraint_set::Bool      = true,
        name::Bool                    = true
    )

A helper function for `basic_constraint_tests`.

This function tests the basic functionality of the constraint type `F`-in-`S`
where `S = typeof(set)`, and `func` is a function that takes a vector of `N`
variables and returns a `MOI.AbstractFunction` of type `F`.

If `delete = true`, it will test the deletion of constraints.
If `get_constraint_function = true`, it will test the getting of `ConstraintFunction`.
If `get_constraint_set = true`, it will test the getting of `ConstraintSet`.
If `name = true`, it will test the getting and setting of `ConstraintName`.

### Example

    basic_constraint_test_helper(model, config,
        (x) -> MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(1.0, x)], 0.0),
        MOI.LessThan(1.0),
        1;
        delete=false
    )

"""
function basic_constraint_test_helper(
    model::MOI.ModelLike,
    config::Config,
    func::Function,
    set::MOI.AbstractSet,
    N::Int = 1;
    delete::Bool = true,
    get_constraint_function::Bool = true,
    get_constraint_set::Bool = true,
    name::Bool = true,
)
    MOI.empty!(model)
    x = MOI.add_variables(model, N)
    constraint_function = func(x)
    F, S = typeof(constraint_function), typeof(set)
    @test MOI.supports_constraint(model, F, S)
    @testset "add_constraint" begin
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 0
        c = MOI.add_constraint(model, constraint_function, set)
        @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 1
        if name
            @testset "ConstraintName" begin
                if F == MOI.SingleVariable
                    @test_throws(
                        MOI.SingleVariableConstraintNameError(),
                        MOI.supports(model, MOI.ConstraintName(), typeof(c)),
                    )
                    @test_throws(
                        MOI.SingleVariableConstraintNameError(),
                        MOI.set(model, MOI.ConstraintName(), c, "c"),
                    )
                else
                    @test MOI.get(model, MOI.ConstraintName(), c) == ""
                    @test MOI.supports(model, MOI.ConstraintName(), typeof(c))
                    MOI.set(model, MOI.ConstraintName(), c, "c")
                    @test MOI.get(model, MOI.ConstraintName(), c) == "c"
                end
            end
        end
        if get_constraint_function
            @testset "ConstraintFunction" begin
                func = MOI.get(model, MOI.ConstraintFunction(), c)
                @test func ≈ constraint_function
                @test variables(func) == variables(constraint_function)
            end
        end
        if get_constraint_set
            @testset "ConstraintSet" begin
                @test MOI.get(model, MOI.ConstraintSet(), c) == set
            end
        end
    end
    @testset "ListOfConstraintIndices" begin
        c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        @test length(c_indices) ==
              MOI.get(model, MOI.NumberOfConstraints{F,S}()) ==
              1
    end
    if F != MOI.SingleVariable && F != MOI.VectorOfVariables
        # We can't add multiple variable constraints as these are
        # interpreted as bounds etc.
        @testset "add_constraints" begin
            n = MOI.get(model, MOI.NumberOfConstraints{F,S}())
            cc = MOI.add_constraints(
                model,
                [constraint_function, constraint_function],
                [set, set],
            )
            @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) == 3
            @test length(MOI.get(model, MOI.ListOfConstraintIndices{F,S}())) ==
                  3
        end
    end
    @testset "is_valid" begin
        c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
        @test all(MOI.is_valid.(model, c_indices))
    end
    if delete
        @testset "delete" begin
            c_indices = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            c_index = c_indices[1]
            MOI.delete(model, c_index)
            @test MOI.get(model, MOI.NumberOfConstraints{F,S}()) ==
                  length(c_indices) - 1
            @test !MOI.is_valid(model, c_index)
            @test_throws MOI.InvalidIndex(c_index) MOI.delete(model, c_index)
            if get_constraint_function
                @test_throws MOI.InvalidIndex(c_index) MOI.get(
                    model,
                    MOI.ConstraintFunction(),
                    c_index,
                )
            end
            if get_constraint_set
                @test_throws MOI.InvalidIndex(c_index) MOI.get(
                    model,
                    MOI.ConstraintSet(),
                    c_index,
                )
            end
        end
    end
end
