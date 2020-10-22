using Test
using MathOptInterface
const MOI = MathOptInterface

function complex_coefficients()
    x = MOI.SingleVariable(MOI.VariableIndex(1))
    y = MOI.SingleVariable(MOI.VariableIndex(2))
    MOI.Test.io_test(MOI.IJuliaMode, (1im) * x + (2 + im) * y, "i x_{1} + (2 + i)x_{2}")
    MOI.Test.io_test(MOI.REPLMode, (1im) * x + (2 + im) * y, "im x[1] + (2 + im)x[2]")
    MOI.Test.io_test(MOI.IJuliaMode, (2im) * x + (1 + 0im) * y, "2i x_{1} + x_{2}")
    MOI.Test.io_test(MOI.REPLMode, (2im) * x + (1 + 0im) * y, "2im x[1] + x[2]")
    MOI.Test.io_test(MOI.IJuliaMode, (0im) * x + (2 + 0im) * y, "0x_{1} + 2x_{2}")
    MOI.Test.io_test(MOI.REPLMode, (0im) * x + (2 + 0im) * y, "0x[1] + 2x[2]")
    MOI.Test.io_test(MOI.IJuliaMode, (1e-9im) * x + (2e7 + 3e-6im) * y + 4e-10im, "1.0 \\times 10^{-9}i x_{1} + (2.0 \\times 10^{7} + 3.0 \\times 10^{-6}i)x_{2} + 4.0 \\times 10^{-10}i")
    MOI.Test.io_test(MOI.REPLMode, (1e-9im) * x + (2e7 + 3e-6im) * y + 4e-10im, "1.0e-9im x[1] + (2.0e7 + 3.0e-6im)x[2] + 4.0e-10im")
end

@testset "Printing of complex coefficients" begin
    complex_coefficients()
end
