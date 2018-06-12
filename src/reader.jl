"""
    MOFInstance(file::String)

Read a MOF file located at `file`

### Example

    MOFInstance("path/to/model.mof.json")
"""
function MOFInstance(file::String)
    m = MOFInstance()
    MOI.read!(m, file)
    m
end

function MOI.read!(m::MOFInstance, file::String)
    d = open(file, "r") do io
        JSON.parse(io, dicttype=OrderedDict{String, Any})
    end
    if length(m["variables"]) > 0
        error("Unable to load the model from $(file). Instance is not empty!")
    end
    src = MOFInstance(d, Dict{String, MOI.VariableIndex}(), Dict{MOI.VariableIndex, Int}(), Dict{UInt64, Int}(), CurrentReference(UInt64(0), UInt64(0)))
    for (i, v) in enumerate(src["variables"])
        src.namemap[v["name"]] = MOI.VariableIndex(UInt64(i))
        src.varmap[MOI.VariableIndex(UInt64(i))] = i
        src.current_reference.variable += 1
    end
    for (i, c) in enumerate(src["constraints"])
        src.constrmap[UInt64(i)] = i
        src.current_reference.constraint += 1
    end
    # delete everything in the current instance
    empty!(m.d)
    m.d["name"]        = "MathOptFormat Model"
    m.d["version"]     = "0.0"
    m.d["sense"]       = "min"
    m.d["variables"]   = Object[]
    m.d["objective"]   = Object("head"=>"ScalarAffineFunction", "variables"=>String[], "coefficients"=>Float64[], "constant"=>0.0)
    m.d["constraints"] = Object[]
    MOI.copy!(m, src)
end

function tryset!(dest, dict, ref, attr, str)
    if haskey(dict, str) && MOI.canset(dest, attr, ref)
        MOI.set!(dest, attr, ref, dict[str])
    end
end

function getset!(dest, destref, src, srcref, attr)
    if MOI.canget(src, attr, srcref) && MOI.canset(dest, attr, destref)
        MOI.set!(dest, attr, destref, MOI.get(src, attr, srcref))
    end
end

function rereference!(x::Vector{MOI.VariableIndex}, vmap::Dict{MOI.VariableIndex, MOI.VariableIndex})
    for (i, v) in enumerate(x)
        x[i] = vmap[v]
    end
end
function rereference!(f::MOI.SingleVariable, vmap)
    MOI.SingleVariable(vmap[f.variable])
end
function rereference!(f::Union{
        MOI.VectorOfVariables,
        MOI.ScalarAffineFunction,
        MOI.VectorAffineFunction
    }, vmap)
    rereference!(f.variables, vmap)
    f
end
function rereference!(f::Union{
        MOI.ScalarQuadraticFunction,
        MOI.VectorQuadraticFunction
    }, vmap)
    rereference!(f.affine_variables, vmap)
    rereference!(f.quadratic_rowvariables, vmap)
    rereference!(f.quadratic_colvariables, vmap)
    f
end

function MOI.get(m::MOFInstance, ::Type{MOI.ListOfConstraintIndices})
    c = Vector{MOI.ConstraintIndex}(length(m["constraints"]))
    for (uid, row) in m.constrmap
        con = m["constraints"][row]
        (F, S) = (functiontype!(m, con["function"]), settype!(m, con["set"]))
         c[row] = MOI.ConstraintIndex{F,S}(uid)
    end
    c
end
MOI.canget(m::MOFInstance, ::Type{MOI.ListOfConstraintIndices}) = true

function MOI.copy!(dest::MOI.AbstractOptimizer, src::MOFInstance)
    numvar = MOI.get(src, MOI.NumberOfVariables())
    destv = MOI.addvariables!(dest, numvar)
    srcv  = MOI.get(src, MOI.ListOfVariableIndices())
    variablemap = Dict{MOI.VariableIndex, MOI.VariableIndex}()
    for i in 1:numvar
        getset!(dest, destv[i], src, srcv[i], MOI.VariableName())
        getset!(dest, destv[i], src, srcv[i], MOI.VariablePrimalStart())
        variablemap[srcv[i]] = destv[i]
    end

    if MOI.canget(src, MOI.Name())
        name = MOI.get(src, MOI.Name())
        MOI.set!(dest, MOI.Name(), name)
    end

    sense = MOI.get(src, MOI.ObjectiveSense())
    MOI.set!(dest, MOI.ObjectiveSense(), sense)

    objfunc = MOI.get(src, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}())
    objfunc = rereference!(objfunc, variablemap)
    MOI.set!(dest, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(), objfunc)

    # ============
    #   Find a way to loop through constraint references
    # ============
    for srcc in MOI.get(src, MOI.ListOfConstraintIndices)
        func = MOI.get(src, MOI.ConstraintFunction(), srcc)
        func = rereference!(func, variablemap)
        set  = MOI.get(src, MOI.ConstraintSet(), srcc)

        if !MOI.canaddconstraint(dest, func, set)
            error("Unable to add the constraint of type ($(typeof(func)), $(typeof(set)))")
        end

        destc = MOI.addconstraint!(dest, func, set)
        getset!(dest, destc, src, srcc, MOI.ConstraintName())
        getset!(dest, destc, src, srcc, MOI.ConstraintPrimalStart())
        getset!(dest, destc, src, srcc, MOI.ConstraintDualStart())
    end
    dest
end

#=
    Parse Function objects to MathOptInterface representation
=#

vvec(m::MOFInstance, names::Vector) = MOI.VariableIndex[m.namemap[n] for n in names]

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
function parse!(m::MOFInstance, obj::Object)
    parse!(Val{Symbol(obj["head"])}(), m, obj)
end

function parse!(::Val{:SingleVariable}, m::MOFInstance, f::Object)
    MOI.SingleVariable(
        m.namemap[f["variable"]]
    )
end

function parse!(::Val{:VectorOfVariables}, m::MOFInstance, f::Object)
    MOI.VectorOfVariables(
        vvec(m, f["variables"])
    )
end

function parse!(::Val{:ScalarAffineFunction}, m::MOFInstance, f::Object)
    MOI.ScalarAffineFunction(
        vvec(m, f["variables"]),
        floatify(f["coefficients"]),
        floatify(f["constant"])
    )
end

function parse!(::Val{:VectorAffineFunction}, m::MOFInstance, f::Object)
    MOI.VectorAffineFunction(
        Int.(f["outputindex"]),
        vvec(m, f["variables"]),
        floatify(f["coefficients"]),
        floatify(f["constant"])
    )
end

function parse!(::Val{:ScalarQuadraticFunction}, m::MOFInstance, f::Object)
    MOI.ScalarQuadraticFunction(
        vvec(m, f["affine_variables"]),
        floatify(f["affine_coefficients"]),
        vvec(m, f["quadratic_rowvariables"]),
        vvec(m, f["quadratic_colvariables"]),
        floatify(f["quadratic_coefficients"]),
        floatify(f["constant"])
    )
end

function parse!(::Val{:VectorQuadraticFunction}, m::MOFInstance, f::Object)
    MOI.VectorQuadraticFunction(
        Int.(f["affine_outputindex"]),
        vvec(m, f["affine_variables"]),
        floatify(f["affine_coefficients"]),
        Int.(f["quadratic_outputindex"]),
        vvec(m, f["quadratic_rowvariables"]),
        vvec(m, f["quadratic_colvariables"]),
        floatify(f["quadratic_coefficients"]),
        floatify(f["constant"])
    )
end

#=
    Parse Set objects to MathOptInterface representation
=#

parse!(::Val{:EqualTo}, m, set)        = MOI.EqualTo(set["value"])
parse!(::Val{:LessThan}, m, set)       = MOI.LessThan(set["upper"])
parse!(::Val{:GreaterThan}, m, set)    = MOI.GreaterThan(set["lower"])
parse!(::Val{:Interval}, m, set)       = MOI.Interval(set["lower"], set["upper"])
parse!(::Val{:Integer}, m, set)        = MOI.Integer()
parse!(::Val{:ZeroOne}, m, set)        = MOI.ZeroOne()
parse!(::Val{:Reals}, m, set)          = MOI.Reals(set["dimension"])
parse!(::Val{:Zeros}, m, set)          = MOI.Zeros(set["dimension"])
parse!(::Val{:Nonnegatives}, m, set)   = MOI.Nonnegatives(set["dimension"])
parse!(::Val{:Nonpositives}, m, set)   = MOI.Nonpositives(set["dimension"])
parse!(::Val{:Semicontinuous}, m, set) = MOI.Semicontinuous(set["lower"], set["upper"])
parse!(::Val{:Semiinteger}, m, set)    = MOI.Semiinteger(set["lower"], set["upper"])
parse!(::Val{:SOS1}, m, set)           = MOI.SOS1(floatify(set["weights"]))
parse!(::Val{:SOS2}, m, set)           = MOI.SOS2(floatify(set["weights"]))
parse!(::Val{:SecondOrderCone}, m, set)                  = MOI.SecondOrderCone(set["dimension"])
parse!(::Val{:RotatedSecondOrderCone}, m, set)           = MOI.RotatedSecondOrderCone(set["dimension"])
parse!(::Val{:ExponentialCone}, m, set)                  = MOI.ExponentialCone()
parse!(::Val{:DualExponentialCone}, m, set)              = MOI.DualExponentialCone()
parse!(::Val{:PowerCone}, m, set)                        = MOI.PowerCone(floatify(set["exponent"]))
parse!(::Val{:DualPowerCone}, m, set)                    = MOI.DualPowerCone(floatify(set["exponent"]))
parse!(::Val{:PositiveSemidefiniteConeTriangle}, m, set) = MOI.PositiveSemidefiniteConeTriangle(set["dimension"])
parse!(::Val{:PositiveSemidefiniteConeSquare}, m, set)   = MOI.PositiveSemidefiniteConeSquare(set["dimension"])
parse!(::Val{:GeometricMeanCone}, m, set)   = MOI.GeometricMeanCone(set["dimension"])
parse!(::Val{:LogDetConeTriangle}, m, set)  = MOI.LogDetConeTriangle(set["dimension"])
parse!(::Val{:LogDetConeSquare}, m, set)    = MOI.LogDetConeSquare(set["dimension"])
parse!(::Val{:RootDetConeTriangle}, m, set) = MOI.RootDetConeTriangle(set["dimension"])
parse!(::Val{:RootDetConeSquare}, m, set)   = MOI.RootDetConeSquare(set["dimension"])

settype!(m::MOFInstance, set::Object) = typeof(parse!(m, set))
functiontype!(m::MOFInstance, func::Object) = typeof(parse!(m, func))
