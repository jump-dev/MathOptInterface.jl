using SparseArrays

@testset "Constant" begin
    @test MOI.constant(MOI.EqualTo(3)) == 3
    @test MOI.constant(MOI.GreaterThan(6)) == 6
    @test MOI.constant(MOI.LessThan(2)) == 2
end

@testset "Shifts" begin
    @test MOIU.shift_constant(MOI.EqualTo(3), 1) == MOI.EqualTo(4)
    @test MOIU.shift_constant(MOI.GreaterThan(6), -1) == MOI.GreaterThan(5)
    @test MOIU.shift_constant(MOI.LessThan(2), 2) == MOI.LessThan(4)
    @test MOIU.shift_constant(MOI.Interval(-2, 3), 1) == MOI.Interval(-1, 4)
end

@testset "Dimension" begin
    @test MOI.dimension(MOI.EqualTo(3.0)) === 1
    @test MOI.dimension(MOI.Reals(8)) === 8
    @test MOI.dimension(MOI.DualExponentialCone()) === 3
    @test MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(4)) === 10
    @test MOI.dimension(MOI.PositiveSemidefiniteConeSquare(5)) === 25
    @test MOI.dimension(MOI.RootDetConeTriangle(6)) === 22
    @test MOI.dimension(MOI.LogDetConeTriangle(6)) === 23
    @test MOI.dimension(MOI.RootDetConeSquare(4)) === 17
    @test MOI.dimension(MOI.LogDetConeSquare(4)) === 18
    @test MOI.dimension(MOI.SOS2(collect(1:6))) === 6
end

@testset "Dual Set" begin
    # Nonpositives
    nonpositives3 = MOI.Nonpositives(3)
    nonpositives4 = MOI.Nonpositives(4)
    @test MOI.dual_set(nonpositives3) == nonpositives3
    @test MOI.dual_set(nonpositives3) != nonpositives4
    @test MOI.dual_set(nonpositives4) == nonpositives4
    # Nonnegatives
    nonnegatives3 = MOI.Nonnegatives(3)
    nonnegatives4 = MOI.Nonnegatives(4)
    @test MOI.dual_set(nonnegatives3) == nonnegatives3
    @test MOI.dual_set(nonnegatives3) != nonnegatives4
    @test MOI.dual_set(nonnegatives4) == nonnegatives4
    # Zeros and Reals
    zeros3 = MOI.Zeros(3)
    zeros4 = MOI.Zeros(4)
    reals3 = MOI.Reals(3)
    reals4 = MOI.Reals(4)
    @test MOI.dual_set(zeros3) == reals3
    @test MOI.dual_set(reals3) == zeros3
    @test MOI.dual_set(reals3) != zeros4
    @test MOI.dual_set(zeros4) == reals4
    @test MOI.dual_set(reals4) == zeros4
    @test MOI.dual_set(zeros4) != reals3
    #SOC
    soc2 = MOI.SecondOrderCone(2)
    soc3 = MOI.SecondOrderCone(3)
    @test MOI.dual_set(soc2) == soc2
    @test MOI.dual_set(soc2) != soc3
    @test MOI.dual_set(soc3) == soc3
    #RSOC
    rsoc2 = MOI.RotatedSecondOrderCone(2)
    rsoc3 = MOI.RotatedSecondOrderCone(3)
    @test MOI.dual_set(rsoc2) == rsoc2
    @test MOI.dual_set(rsoc2) != rsoc3
    @test MOI.dual_set(rsoc3) == rsoc3
    #PSDtriangle
    psd2 = MOI.PositiveSemidefiniteConeTriangle(2)
    psd3 = MOI.PositiveSemidefiniteConeTriangle(3)
    @test MOI.dual_set(psd2) == psd2
    @test MOI.dual_set(psd2) != psd3
    @test MOI.dual_set(psd3) == psd3
    # Exponential
    exp = MOI.ExponentialCone()
    dual_exp = MOI.DualExponentialCone()
    @test MOI.dual_set(exp) == dual_exp
    @test MOI.dual_set(exp) != exp
    @test MOI.dual_set(dual_exp) == exp
    @test MOI.dual_set(dual_exp) != dual_exp
    # Power
    pow03 = MOI.PowerCone(0.3)
    pow04 = MOI.PowerCone(0.4)
    dual_pow03 = MOI.DualPowerCone(0.3)
    @test MOI.dual_set(pow03) == dual_pow03
    @test MOI.dual_set(pow03) != pow03
    @test MOI.dual_set(dual_pow03) == pow03
    @test MOI.dual_set(dual_pow03) != pow04
    @test MOI.dual_set(dual_pow03) != dual_pow03
    # PSDSquare error
    s = MOI.PositiveSemidefiniteConeSquare(4)
    err = ErrorException("""Dual of $s is not defined in MathOptInterface.
                            For more details see the comments in src/Bridges/Constraint/square.jl""")
    @test_throws err MOI.dual_set(MOI.PositiveSemidefiniteConeSquare(4))
    # Not implemented
    s = MOI.LogDetConeTriangle(4)
    err = ErrorException("Dual of $s is not implemented.")
    @test_throws err MOI.dual_set(MOI.LogDetConeTriangle(4))
end

@testset "Set dot" begin
    vec = zeros(6)
    @test MOIU.set_dot(vec, vec, MOI.SecondOrderCone(6)) == 0
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 0
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeSquare(3)) == 0
    vec[5] = 1
    @test MOIU.set_dot(vec, vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 2
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeSquare(3)) == 1
    vec[5] = 0
    vec[3] = 1
    @test MOIU.set_dot(vec, vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 1
    @test MOIU.set_dot(vec, vec, MOI.PositiveSemidefiniteConeSquare(3)) == 1

    vec = zeros(7)
    @test MOIU.set_dot(vec, vec, MOI.RootDetConeTriangle(3)) == 0
    vec[5] = 1
    @test MOIU.set_dot(vec, vec, MOI.RootDetConeTriangle(3)) == 2
    vec = zeros(8)
    @test MOIU. set_dot(vec, vec, MOI.LogDetConeTriangle(3)) == 0
    vec[5] = 1
    @test MOIU.set_dot(vec, vec, MOI.LogDetConeTriangle(3)) == 1
    
    sp_vec = spzeros(6)
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.SecondOrderCone(6)) == 0
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 0
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeSquare(3)) == 0
    sp_vec[5] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 2
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeSquare(3)) == 1
    sp_vec[5] = 0
    sp_vec[3] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.SecondOrderCone(6)) == 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeTriangle(3)) == 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.PositiveSemidefiniteConeSquare(3)) == 1

    sp_vec = spzeros(7)
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.RootDetConeTriangle(3)) == 0
    sp_vec[5] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.RootDetConeTriangle(3)) == 2
    sp_vec = spzeros(8)
    @test MOIU. set_dot(sp_vec, sp_vec, MOI.LogDetConeTriangle(3)) == 0
    sp_vec[5] = 1
    @test MOIU.set_dot(sp_vec, sp_vec, MOI.LogDetConeTriangle(3)) == 1
end

@testset "dot coefficients" begin
    vec = zeros(6)
    @test MOIU.dot_coefficients(vec, MOI.SecondOrderCone(6)) == vec
    @test MOIU.dot_coefficients(vec, MOI.PositiveSemidefiniteConeTriangle(3)) == vec
    vec[5] = 1
    @test MOIU.dot_coefficients(vec, MOI.SecondOrderCone(6)) == vec
    @test MOIU.dot_coefficients(vec, MOI.PositiveSemidefiniteConeTriangle(3)) == vec./2
    vec[5] = 0
    vec[3] = 1
    @test MOIU.dot_coefficients(vec, MOI.SecondOrderCone(6)) == vec
    @test MOIU.dot_coefficients(vec, MOI.PositiveSemidefiniteConeTriangle(3)) == vec

    vec = zeros(7)
    @test MOIU.dot_coefficients(vec, MOI.RootDetConeTriangle(3)) == vec
    vec[5] = 1
    @test MOIU.dot_coefficients(vec, MOI.RootDetConeTriangle(3)) == vec./2
    vec = zeros(8)
    @test MOIU. dot_coefficients(vec, MOI.LogDetConeTriangle(3)) == vec
    vec[5] = 1
    @test MOIU.dot_coefficients(vec, MOI.LogDetConeTriangle(3)) == vec

    sp_vec = spzeros(6)
    @test MOIU.dot_coefficients(sp_vec, MOI.SecondOrderCone(6)) == sp_vec
    @test MOIU.dot_coefficients(sp_vec, MOI.PositiveSemidefiniteConeTriangle(3)) == sp_vec
    sp_vec[5] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.SecondOrderCone(6)) == sp_vec
    @test MOIU.dot_coefficients(sp_vec, MOI.PositiveSemidefiniteConeTriangle(3)) == sp_vec./2
    sp_vec[5] = 0
    sp_vec[3] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.SecondOrderCone(6)) == sp_vec
    @test MOIU.dot_coefficients(sp_vec, MOI.PositiveSemidefiniteConeTriangle(3)) == sp_vec

    sp_vec = spzeros(7)
    @test MOIU.dot_coefficients(sp_vec, MOI.RootDetConeTriangle(3)) == sp_vec
    sp_vec[5] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.RootDetConeTriangle(3)) == sp_vec./2
    sp_vec = spzeros(8)
    @test MOIU. dot_coefficients(sp_vec, MOI.LogDetConeTriangle(3)) == sp_vec
    sp_vec[5] = 1
    @test MOIU.dot_coefficients(sp_vec, MOI.LogDetConeTriangle(3)) == sp_vec
end
