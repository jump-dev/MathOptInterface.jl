MOIU.@model LPModel () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

@testset "Name test" begin
    MOIT.nametest(Model{Float64}())
end

@testset "Valid test" begin
    MOIT.validtest(Model{Float64}())
end

@testset "Empty test" begin
    MOIT.emptytest(Model{Float64}())
end

@testset "canaddconstraint test" begin
    MOIT.canaddconstrainttest(Model{Float64}(), Float64, Int)
    MOIT.canaddconstrainttest(Model{Int}(), Int, Float64)
end

# Only config.query is true as MOI.optimize! is not implemented
const config = MOIT.TestConfig(solve=false)

@testset "Continuous Linear tests" begin
    MOIT.contlineartest(Model{Float64}(), config)
end

@testset "Continuous Conic tests" begin
    MOIT.contconictest(Model{Float64}(), config)
end

@testset "Quadratic functions" begin

    model = Model{Int}()

    x, y = MOI.addvariables!(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarQuadraticFunction([x], [3], [x, y, x], [x, y, y], [1, 2, 3], 7)
    c1 = MOI.addconstraint!(model, f1, MOI.Interval(-1, 1))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}()) == 1
    @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.VectorQuadraticFunction([1, 2, 2], [x, x, y], [3, 1, 2], [1, 1, 2], [x, y, x], [x, y, y], [1, 2, 3], [7, 3, 4])
    c2 = MOI.addconstraint!(model, f2, MOI.PositiveSemidefiniteConeTriangle(3))

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}()) == 1
    @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())) == [c2]

    @test MOI.canget(model, MOI.ListOfConstraints())
    loc = MOI.get(model, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc

    f3 = MOIU.modifyfunction(f1, MOI.ScalarConstantChange(9))
    f3 = MOIU.modifyfunction(f3, MOI.ScalarCoefficientChange(y, 2))

    @test !(MOI.get(model, MOI.ConstraintFunction(), c1) ≈ f3)
    MOI.modifyconstraint!(model, c1, f3)
    @test MOI.get(model, MOI.ConstraintFunction(), c1) ≈ f3

    f4 = MOI.VectorAffineFunction([1, 1, 2], [x, y, y], [2, 4, 3], [5, 7])
    c4 = MOI.addconstraint!(model, f4, MOI.SecondOrderCone(2))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1

    f5 = MOI.VectorOfVariables([x])
    c5 = MOI.addconstraint!(model, f5, MOI.RotatedSecondOrderCone(1))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

    f6 = MOI.VectorAffineFunction([1, 2], [x, y], [2, 9], [6, 8])
    c6 = MOI.addconstraint!(model, f6, MOI.SecondOrderCone(2))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 2

    f7 = MOI.VectorOfVariables([x, y])
    c7 = MOI.addconstraint!(model, f7, MOI.Nonpositives(2))
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1

    loc1 = MOI.get(model, MOI.ListOfConstraints())
    loc2 = Vector{Tuple{DataType, DataType}}()
    function _pushloc{F, S}(constrs::Vector{MOIU.C{F, S}})
        if !isempty(constrs)
            push!(loc2, (F, S))
        end
    end
    MOIU.broadcastcall(_pushloc, model)
    for loc in (loc1, loc2)
        @test length(loc) == 5
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorOfVariables,MOI.RotatedSecondOrderCone) in loc
        @test (MOI.VectorAffineFunction{Int},MOI.SecondOrderCone) in loc
        @test (MOI.VectorOfVariables,MOI.Nonpositives) in loc
    end

    @test MOI.isvalid(model, c4)
    MOI.delete!(model, c4)
    @test !MOI.isvalid(model, c4)

    @test MOI.get(model, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1
    @test MOI.get(model, MOI.ConstraintFunction(), c6).constant == f6.constant

    MOI.delete!(model, y)

    f = MOI.get(model, MOI.ConstraintFunction(), c2)
    @test f.affine_outputindex == [1, 2]
    @test f.affine_variables == [x, x]
    @test f.affine_coefficients == [3, 1]
    @test f.quadratic_outputindex == [1]
    @test f.quadratic_rowvariables == [x]
    @test f.quadratic_colvariables == [x]
    @test f.quadratic_coefficients == [1]
    @test f.constant == [7, 3, 4]

    f =  MOI.get(model, MOI.ConstraintFunction(), c6)
    @test f.outputindex == [1]
    @test f.variables == [x]
    @test f.coefficients == [2]
    @test f.constant == [6, 8]

    f =  MOI.get(model, MOI.ConstraintFunction(), c7)
    @test f.variables == [x]

    s =  MOI.get(model, MOI.ConstraintSet(), c7)
    @test MOI.dimension(s) == 1

end
