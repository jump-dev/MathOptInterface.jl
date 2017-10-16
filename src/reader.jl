"""
    MOI.SolverInstance(mf::MOFFile, solver)

Create a new MathOptInterface solver instance using `solver` from the MOFFile `mf`
"""
function MOI.SolverInstance(mf::MOFFile, solver)
    m = MOI.SolverInstance(solver)
    v = MOI.addvariables!(m, length(mf["variables"]))
    for (i, dict) in enumerate(mf["variables"])
        mf.ext[dict["name"]] = v[i]
        MOI.setattribute!(m, MOI.VariableName(), v[i], dict["name"])
        if haskey(dict, "VariablePrimalStart")
            MOI.setattribute!(m, MOI.VariablePrimalStart(), v[i], dict["VariablePrimalStart"])
        end
    end
    sense = (mf["sense"] == "min") ? MOI.MinSense : MOI.MaxSense
    MOI.setattribute!(m, MOI.ObjectiveFunction(), parse!(m, mf, mf["objective"]))
    MOI.setattribute!(m, MOI.ObjectiveSense(), sense)
    for con in mf["constraints"]
        c = MOI.addconstraint!(m, parse!(m, mf, con["function"]), parse!(m, mf, con["set"]))
        MOI.setattribute!(m, MOI.ConstraintName(), c, con["name"])
        if haskey(con, "ConstraintPrimalStart")
            MOI.setattribute!(m, MOI.ConstraintPrimalStart(), c, con["ConstraintPrimalStart"])
        end
        if haskey(con, "ConstraintDualStart")
            MOI.setattribute!(m, MOI.ConstraintDualStart(), c, con["ConstraintDualStart"])
        end
    end
    m
end

"""
    MOI.SolverInstance(file::String, solver)

Create a new MathOptInterface solver instance using `solver` from the MOFFile
located at the path `file`.
"""
MOI.SolverInstance(file::String, solver) = MOI.SolverInstance(MOFFile(file), solver)

#=
    Parse Function objects to MathOptInterface representation
=#

vvec(mf::MOFFile, names::Vector) = MOI.VariableReference[mf.ext[n] for n in names]

# we need to do this because float.(Any[]) returns Any[] rather than Float64[]
floatify(x::Vector{Float64}) = x
floatify(x::Float64) = x
function floatify(x::Vector)
    if length(x) == 0
        Float64[]
    else
        floatify.(x)
    end
end
floatify(x) = Float64(x)

# dispatch on "head" Val types to avoid a big if .. elseif ... elseif ... end
parse!(m, mf::MOFFile, obj::Object) = parse!(Val{Symbol(obj["head"])}(), m, mf, obj)

function parse!(::Val{:SingleVariable}, m, mf::MOFFile, f::Object)
    MOI.SingleVariable(
        mf.ext[f["variable"]]
    )
end

function parse!(::Val{:VectorOfVariables}, m, mf::MOFFile, f::Object)
    MOI.VectorOfVariables(
        vvec(mf, f["variables"])
    )
end

function parse!(::Val{:ScalarAffineFunction}, m, mf::MOFFile, f::Object)
    MOI.ScalarAffineFunction(
        vvec(mf, f["variables"]),
        floatify(f["coefficients"]),
        floatify(f["constant"])
    )
end

function parse!(::Val{:VectorAffineFunction}, m, mf::MOFFile, f::Object)
    MOI.VectorAffineFunction(
        Int.(f["outputindex"]),
        vvec(mf, f["variables"]),
        floatify(f["coefficients"]),
        floatify(f["constant"])
    )
end

function parse!(::Val{:ScalarQuadraticFunction}, m, mf::MOFFile, f::Object)
    MOI.ScalarQuadraticFunction(
        vvec(mf, f["affine_variables"]),
        floatify(f["affine_coefficients"]),
        vvec(mf, f["quadratic_rowvariables"]),
        vvec(mf, f["quadratic_colvariables"]),
        floatify(f["quadratic_coefficients"]),
        floatify(f["constant"])
    )
end

function parse!(::Val{:VectorQuadraticFunction}, m, mf::MOFFile, f::Object)
    MOI.VectorQuadraticFunction(
        Int.(f["affine_outputindex"]),
        vvec(mf, f["affine_variables"]),
        floatify(f["affine_coefficients"]),
        Int.(f["quadratic_outputindex"]),
        vvec(mf, f["quadratic_rowvariables"]),
        vvec(mf, f["quadratic_colvariables"]),
        floatify(f["quadratic_coefficients"]),
        floatify(f["constant"])
    )
end

#=
    Parse Set objects to MathOptInterface representation
=#

parse!(::Val{:EqualTo}, m, mf, set)        = MOI.EqualTo(set["value"])
parse!(::Val{:LessThan}, m, mf, set)       = MOI.LessThan(set["upper"])
parse!(::Val{:GreaterThan}, m, mf, set)    = MOI.GreaterThan(set["lower"])
parse!(::Val{:Interval}, m, mf, set)       = MOI.Interval(set["lower"], set["upper"])
parse!(::Val{:Integer}, m, mf, set)        = MOI.Integer()
parse!(::Val{:ZeroOne}, m, mf, set)        = MOI.ZeroOne()
parse!(::Val{:Reals}, m, mf, set)          = MOI.Reals(set["dimension"])
parse!(::Val{:Zeros}, m, mf, set)          = MOI.Zeros(set["dimension"])
parse!(::Val{:Nonnegatives}, m, mf, set)   = MOI.Nonnegatives(set["dimension"])
parse!(::Val{:Nonpositives}, m, mf, set)   = MOI.Nonpositives(set["dimension"])
parse!(::Val{:Semicontinuous}, m, mf, set) = MOI.Semicontinuous(set["lower"], set["upper"])
parse!(::Val{:Semiinteger}, m, mf, set)    = MOI.Semiinteger(set["lower"], set["upper"])
parse!(::Val{:SOS1}, m, mf, set)           = MOI.SOS1(floatify(set["weights"]))
parse!(::Val{:SOS2}, m, mf, set)           = MOI.SOS2(floatify(set["weights"]))
parse!(::Val{:SecondOrderCone}, m, mf, set)                  = MOI.SecondOrderCone(set["dimension"])
parse!(::Val{:RotatedSecondOrderCone}, m, mf, set)           = MOI.RotatedSecondOrderCone(set["dimension"])
parse!(::Val{:ExponentialCone}, m, mf, set)                  = MOI.ExponentialCone()
parse!(::Val{:DualExponentialCone}, m, mf, set)              = MOI.DualExponentialCone()
parse!(::Val{:PowerCone}, m, mf, set)                        = MOI.PowerCone(floatify(set["exponent"]))
parse!(::Val{:DualPowerCone}, m, mf, set)                    = MOI.DualPowerCone(floatify(set["exponent"]))
parse!(::Val{:PositiveSemidefiniteConeTriangle}, m, mf, set) = MOI.PositiveSemidefiniteConeTriangle(set["dimension"])
parse!(::Val{:PositiveSemidefiniteConeScaled}, m, mf, set)   = MOI.PositiveSemidefiniteConeScaled(set["dimension"])
