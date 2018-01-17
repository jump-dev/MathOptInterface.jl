MOI.get(m::MOFInstance, ::MOI.Name) = m["name"]
MOI.canget(m::MOFInstance, ::MOI.Name) = haskey(m.d, "name")
MOI.set!(m::MOFInstance, ::MOI.Name, name::String) = (m["name"] = name)
MOI.canset(m::MOFInstance, ::MOI.Name) = true

function MOI.get(m::MOFInstance, ::Type{MOI.VariableIndex}, name::String)
    m.namemap[name]
end
MOI.canget(m::MOFInstance, ::Type{MOI.VariableIndex}, name::String) = haskey(m.namemap, name)

# MOI.get(m::MOFInstance, ::Type{MOI.ConstraintIndex}, name::String)
# MOI.get(m::AbstractInstance, ::Type{ConstraintIndex{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}
# MOI.canget(m::MOFInstance, ::Type{MOI.ConstraintIndex}, name::String)
# MOI.canget(m::AbstractInstance, ::Type{ConstraintIndex{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}

MOI.get(m::MOFInstance, ::MOI.ListOfVariableIndices) = [m.namemap[v["name"]] for v in m["variables"]]
MOI.canget(m::MOFInstance, ::MOI.ListOfVariableIndices) = true

MOI.get(m::MOFInstance, ::MOI.NumberOfVariables) = length(m["variables"])
MOI.canget(m::MOFInstance, ::MOI.NumberOfVariables) = true

function MOI.set!(m::MOFInstance, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction)
    m["objective"] = object!(m, func)
end
MOI.canset(m::MOFInstance, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction) = true

MOI.get(m::MOFInstance, ::MOI.ObjectiveFunction) = parse!(m, m["objective"])
MOI.canget(m::MOFInstance, ::MOI.ObjectiveFunction) = true

function MOI.set!(m::MOFInstance, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    m["sense"] = object(sense)
end
MOI.canset(m::MOFInstance, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense) = true

function MOI.get(m::MOFInstance, ::MOI.ObjectiveSense)
    if m["sense"] == "min"
        return MOI.MinSense
    elseif m["sense"] == "max"
        return MOI.MaxSense
    else
        # what about feasibility sense?
        error("Unknown objective sense $(m["sense"])")
    end
end
MOI.canget(m::MOFInstance, ::MOI.ObjectiveSense) = true

function object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end

"""
    MOI.set!(m::MOFInstance, ::MOI.VariableName, v::MOI.VariableIndex, name::String)

Rename the variable `v` in the MOFInstance `m` to `name`.

*WARNING*: This has to loop through all constraints searching for the string of
the variable name. If you have many constraints, this can be very slow.
"""
function MOI.set!(m::MOFInstance, ::MOI.VariableName, v::MOI.VariableIndex, name::String)
    current_name = MOI.get(m, MOI.VariableName(), v)
    if name == current_name
        return
    end
    if haskey(m.namemap, name)
        error("Name $(name) already exists!")
    end
    delete!(m.namemap, current_name)
    setattr!(m, v, "name", name)
    m.namemap[name] = v
    rename!(m["objective"], current_name, name)
    for c in m["constraints"]
        rename!(c["function"], current_name, name)
    end
end

function rename!(x::Vector{String}, src, dest)
    for (i, v) in enumerate(x)
        if v == src
            x[i] = dest
        end
    end
end
function rename!(obj::Object, src, dest)
    rename!(Val{Symbol(obj["head"])}(), obj, src, dest)
end

function rename!(::Val{:SingleVariable}, obj, src, dest)
    if obj["variable"] == src
        obj["variable"] = dest
    end
end
function rename!(::Union{
        Val{:VectorOfVariables},
        Val{:ScalarAffineFunction},
        Val{:VectorAffineFunction}
    }, obj, src, dest)
    rename!(obj["variables"], src, dest)
end
function rename!(f::Union{
        Val{:ScalarQuadraticFunction},
        Val{:VectorQuadraticFunction}
    }, obj, src, dest)
    rename!(obj["affine_variables"], src, dest)
    rename!(obj["quadratic_rowvariables"], src, dest)
    rename!(obj["quadratic_colvariables"], src, dest)
end


MOI.set!(m::MOFInstance, ::MOI.VariablePrimalStart, v::MOI.VariableIndex, value) = setattr!(m, v, "VariablePrimalStart", value)

function setattr!(m::MOFInstance, v::MOI.VariableIndex, key::String, val)
    m[v][key] = val
end
function getattr(m::MOFInstance, v::MOI.VariableIndex, key::String)
    m[v][key]
end
function hasattr(m::MOFInstance, v::MOI.VariableIndex, key::String)
    MOI.isvalid(m, v) && haskey(m[v], key)
end

MOI.get(m::MOFInstance, ::MOI.VariableName, v::MOI.VariableIndex) = getattr(m, v, "name")
MOI.get(m::MOFInstance, ::MOI.VariablePrimalStart, v::MOI.VariableIndex) = getattr(m, v, "VariablePrimalStart")

MOI.canset(m::MOFInstance, ::MOI.VariableName, v::MOI.VariableIndex) = MOI.isvalid(m, v)
MOI.canset(m::MOFInstance, ::MOI.VariablePrimalStart, v::MOI.VariableIndex) = MOI.isvalid(m, v)

MOI.canget(m::MOFInstance, ::MOI.VariableName, v::MOI.VariableIndex) = hasattr(m, v, "name")
MOI.canget(m::MOFInstance, ::MOI.VariablePrimalStart, v::MOI.VariableIndex) = hasattr(m, v, "VariablePrimalStart")

MOI.set!(m::MOFInstance, ::MOI.ConstraintName, c::MOI.ConstraintIndex, name::String) = setattr!(m, c, "name", name)
MOI.set!(m::MOFInstance, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintIndex, value) = setattr!(m, c, "ConstraintPrimalStart", value)
MOI.set!(m::MOFInstance, ::MOI.ConstraintDualStart, c::MOI.ConstraintIndex, value) = setattr!(m, c, "ConstraintDualStart", value)

function setattr!(m::MOFInstance, c::MOI.ConstraintIndex, key::String, val)
    m["constraints"][m.constrmap[c.value]][key] = val
end
function getattr(m::MOFInstance, c::MOI.ConstraintIndex, key::String)
    m["constraints"][m.constrmap[c.value]][key]
end
function hasattr(m::MOFInstance, c::MOI.ConstraintIndex, key::String)
    MOI.isvalid(m, c) && haskey(m["constraints"][m.constrmap[c.value]], key)
end
MOI.get(m::MOFInstance, ::MOI.ConstraintName, c::MOI.ConstraintIndex) = getattr(m, c, "name")
MOI.get(m::MOFInstance, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintIndex) = getattr(m, c, "ConstraintPrimalStart")
MOI.get(m::MOFInstance, ::MOI.ConstraintDualStart, c::MOI.ConstraintIndex) = getattr(m, c, "ConstraintDualStart")

MOI.canset(m::MOFInstance, ::MOI.ConstraintName, c::MOI.ConstraintIndex) = MOI.isvalid(m, c)
MOI.canset(m::MOFInstance, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintIndex) = MOI.isvalid(m, c)
MOI.canset(m::MOFInstance, ::MOI.ConstraintDualStart, c::MOI.ConstraintIndex) = MOI.isvalid(m, c)

MOI.canget(m::MOFInstance, ::MOI.ConstraintName, c::MOI.ConstraintIndex) = hasattr(m, c, "name")
MOI.canget(m::MOFInstance, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintIndex) = hasattr(m, c, "ConstraintPrimalStart")
MOI.canget(m::MOFInstance, ::MOI.ConstraintDualStart, c::MOI.ConstraintIndex) = hasattr(m, c, "ConstraintDualStart")

function MOI.get(m::MOFInstance, ::MOI.ConstraintFunction, c::MOI.ConstraintIndex)
    parse!(m, m[c]["function"])
end
MOI.canget(m::MOFInstance, ::MOI.ConstraintFunction, c::MOI.ConstraintIndex) = MOI.isvalid(m, c)
function MOI.get(m::MOFInstance, ::MOI.ConstraintSet, c::MOI.ConstraintIndex)
    parse!(m, m[c]["set"])
end
MOI.canget(m::MOFInstance, ::MOI.ConstraintSet, c::MOI.ConstraintIndex) = MOI.isvalid(m, c)
