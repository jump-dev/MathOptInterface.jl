#=
    These problems are drawn from the test-suite of MathOptInterface
=#

using MathOptInterface, MathOptFormat
const MOF = MathOptFormat
const MOI = MathOptInterface

solver = MOF.MOFWriter()

# linear1
m = MOI.SolverInstance(solver)
v = MOI.addvariables!(m, 2)
MOI.addconstraint!(m, MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0), MOI.LessThan(1.0))
MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
MOI.addconstraint!(m, v[2], MOI.GreaterThan(0.0))
MOI.setattribute!(m, MOI.ObjectiveFunction(),MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0))
MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
MOI.writeproblem(m, joinpath(Pkg.dir("MathOptFormat"), "test", "problems", "linear1.mof.json"), 1)

# linear2
m = MOI.SolverInstance(solver)
x = MOI.addvariable!(m)
y = MOI.addvariable!(m)
MOI.addconstraint!(m, MOI.ScalarAffineFunction([x, y], [1.0,1.0], 0.0), MOI.LessThan(1.0))
MOI.addconstraint!(m, MOI.SingleVariable(x), MOI.GreaterThan(0.0))
MOI.addconstraint!(m, MOI.SingleVariable(y), MOI.GreaterThan(0.0))
MOI.setattribute!(m, MOI.ObjectiveFunction(),MOI.ScalarAffineFunction([x, y], [-1.0,0.0], 0.0))
MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
MOI.writeproblem(m, joinpath(Pkg.dir("MathOptFormat"), "test", "problems", "linear2.mof.json"), 1)

# LIN1
m = MOI.SolverInstance(solver)
v = MOI.addvariables!(m, 3)
MOI.addconstraint!(m, MOI.VectorOfVariables(v), MOI.Nonnegatives(3))
MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,1,2,2], [v;v[2];v[3]], ones(5), [-3.0,-2.0]), MOI.Zeros(2))
MOI.setattribute!(m, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction(v, [-3.0, -2.0, -4.0], 0.0))
MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
MOI.writeproblem(m, joinpath(Pkg.dir("MathOptFormat"), "test", "problems", "LIN1.mof.json"), 1)

# mixed cones
# min  3x + 2y - 4z + 0s
# st    x           -  s  == -4    (i.e. x >= -4)
#            y            == -3
#       x      +  z       == 12
#       x free
#       y <= 0
#       z >= 0
#       s zero
# Opt solution = -82
# x = -4, y = -3, z = 16, s == 0
m = MOI.SolverInstance(solver)
x,y,z,s = MOI.addvariables!(m, 4)
MOI.setattribute!(m, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction([x,y,z], [3.0, 2.0, -4.0], 0.0))
MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)
MOI.addconstraint!(m, MOI.VectorAffineFunction([1,1,2,3,3], [x,s,y,x,z], [1.0,-1.0,1.0,1.0,1.0], [4.0,3.0,-12.0]), MOI.Zeros(3))
MOI.addconstraint!(m, MOI.VectorOfVariables([y]), MOI.Nonpositives(1))
MOI.addconstraint!(m, [z], MOI.Nonnegatives(1))
MOI.addconstraint!(m, MOI.VectorOfVariables([s]), MOI.Zeros(1))
MOI.writeproblem(m, joinpath(Pkg.dir("MathOptFormat"), "test", "problems", "LIN2.mof.json"), 1)

# an example on mixed integer programming
#
#   maximize 1.1x + 2 y + 5 z
#
#   s.t.  x + y + z <= 10
#         x + 2 y + z <= 15
#
#         x is continuous: 0 <= x <= 5
#         y is integer: 0 <= y <= 10
#         z is binary
m = MOI.SolverInstance(solver)
v = MOI.addvariables!(m, 3)
cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
MOI.addconstraint!(m, cf, MOI.LessThan(10.0))
cf2 = MOI.ScalarAffineFunction(v, [1.0,2.0,1.0], 0.0)
MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.Interval(0.0, 5.0))
MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.Interval(0.0, 10.0))
MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.Integer())
MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.ZeroOne())
objf = MOI.ScalarAffineFunction(v, [1.1, 2.0, 5.0], 0.0)
MOI.setattribute!(m, MOI.ObjectiveFunction(), objf)
MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)
MOI.writeproblem(m, joinpath(Pkg.dir("MathOptFormat"), "test", "problems", "mip01.mof.json"), 1)

# SOS1 from CPLEX.jl
m = MOI.SolverInstance(solver)
v = MOI.addvariables!(m, 3)
MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.LessThan(1.0))
MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.LessThan(1.0))
MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.LessThan(2.0))
MOI.addconstraint!(m, MOI.VectorOfVariables([v[1], v[2]]), MOI.SOS1([1.0, 2.0]))
MOI.addconstraint!(m, MOI.VectorOfVariables([v[1], v[3]]), MOI.SOS1([1.0, 2.0]))
MOI.setattribute!(m, MOI.ObjectiveFunction(), MOI.ScalarAffineFunction(v, [2.0, 1.0, 1.0], 0.0))
MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)
MOI.writeproblem(m, joinpath(Pkg.dir("MathOptFormat"), "test", "problems", "sos1.mof.json"), 1)
