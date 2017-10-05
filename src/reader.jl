"""
    MOI.SolverInstance(mf::MOFFile, solver)

Create a new MathOptInterface solver instance using `solver` from the MOFFile `mf`
"""
function MOI.SolverInstance(mf::MOFFile, solver)
    m = MOI.SolverInstance(solver)
    v = MOI.addvariables!(m, length(mf["variables"]), String.(mf["variables"]))
    for (s, vf) in zip(mf["variables"], v)
        mf.ext[s] = vf
    end
    sense = mf["sense"] == "min"?MOI.MinSense:MOI.MaxSense

    MOI.setobjective!(m, sense, parse!(m, mf, mf["objective"]))
    for con in mf["constraints"]
        MOI.addconstraint!(m, parse!(m, mf, con["function"]), parse!(m, mf, con["set"]), con["name"])
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

# we need to do this because float.(Any[]) returns Any[] rahter than Float64[]
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
parse!(::Val{:Reals}, m, mf, set)          = MOI.Reals(set["dim"])
parse!(::Val{:Zeros}, m, mf, set)          = MOI.Zeros(set["dim"])
parse!(::Val{:Nonnegatives}, m, mf, set)   = MOI.Nonnegatives(set["dim"])
parse!(::Val{:Nonpositives}, m, mf, set)   = MOI.Nonpositives(set["dim"])
parse!(::Val{:Semicontinuous}, m, mf, set) = MOI.Semicontinuous(set["l"], set["u"])
parse!(::Val{:Semiinteger}, m, mf, set)    = MOI.Semiinteger(set["l"], set["u"])
parse!(::Val{:SOSI}, m, mf, set)           = MOI.SOS1(floatify(set["weights"]))
parse!(::Val{:SOSII}, m, mf, set)          = MOI.SOS2(floatify(set["weights"]))
