using MathOptFormat, MathOptInterface, NEOS

include("mof_to_mpb.jl")

const MOF = MathOptFormat
const MOI = MathOptInterface

# min 1x + 2x
# s.t        x >= 3
instance = MOF.MOFInstance()
v = MOI.addvariable!(instance)
f = MOI.ScalarAffineFunction([v, v], [1.0, 2.0], 0.0)
MOI.set!(instance, MOI.ObjectiveFunction(),f)
MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)
MOI.addconstraint!(instance,
    MOI.SingleVariable(v),
    MOI.GreaterThan(3.0)
)

(A, collb, colub, c, rowlb, rowub, sense, colcat, sos, Q, modelname,
    colnames, rownames) = MOFtoMPB(instance)

solver = NEOSSolver(email="odow003@aucklanduni.ac.nz", solver=:CPLEX, format=:NL)

m = NEOS.LinearQuadraticModel(solver)
NEOS.loadproblem!(m, A, collb, colub, c, rowlb, rowub, sense)
NEOS.setvartype!(m, colcat)
NEOS.optimize!(m)
NEOS.getsolution(m) == 3.0
