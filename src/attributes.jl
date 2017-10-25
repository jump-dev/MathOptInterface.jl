# MOI.get(m::MOFFile, ::Type{MOI.VariableReference}, name::String)
# MOI.get(m::MOFFile, ::Type{MOI.ConstraintReference}, name::String)
# MOI.get(m::AbstractInstance, ::Type{ConstraintReference{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}
# MOI.canget(m::MOFFile, ::Type{MOI.VariableReference}, name::String)
# MOI.canget(m::MOFFile, ::Type{MOI.ConstraintReference}, name::String)
# MOI.canget(m::AbstractInstance, ::Type{ConstraintReference{F,S}}, name::String) where {F<:AbstractFunction,S<:AbstractSet}

MOI.get(m::MOFFile, ::MOI.ListOfVariableReferences) = [m.namemap[v["name"]] for v in m["variables"]]
MOI.canget(m::MOFFile, ::MOI.ListOfVariableReferences) = true

MOI.get(m::MOFFile, ::MOI.NumberOfVariables) = length(m["variables"])
MOI.canget(m::MOFFile, ::MOI.NumberOfVariables) = true

function MOI.set!(m::MOFFile, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction)
    m["objective"] = object!(m, func)
end
MOI.canset(m::MOFFile, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction) = true

MOI.get(m::MOFFile, ::MOI.ObjectiveFunction) = parse!(m, m["objective"])
MOI.canget(m::MOFFile, ::MOI.ObjectiveFunction) = true

function MOI.set!(m::MOFFile, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    m["sense"] = object(sense)
end
MOI.canset(m::MOFFile, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense) = true

function MOI.get(m::MOFFile, ::MOI.ObjectiveSense)
    if m["sense"] == "min"
        return MOI.MinSense
    elseif m["sense"] == "max"
        return MOI.MaxSense
    else
        # what about feasibility sense?
        error("Unknown objective sense $(m["sense"])")
    end
end
MOI.canget(m::MOFFile, ::MOI.ObjectiveSense) = true

function object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end

"""
    MOI.set!(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference, name::String)

Rename the variable `v` in the MOFFile `m` to `name`. This should be done
immediately after introducing a variable and before it is used in any constraints.

If the variable has already been used, this function will _not_ update the
previous references.
"""
function MOI.set!(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference, name::String)
    current_name = MOI.get(m, MOI.VariableName(), v)
    delete!(m.namemap, current_name)
    setattr!(m, v, "name", name)
    if haskey(m.namemap, name)
        error("Name $(name) already exists!")
    end
    m.namemap[name] = v
end
MOI.set!(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference, value) = setattr!(m, v, "VariablePrimalStart", value)

function setattr!(m::MOFFile, v::MOI.VariableReference, key::String, val)
    m[v][key] = val
end
function getattr(m::MOFFile, v::MOI.VariableReference, key::String)
    m[v][key]
end
function hasattr(m::MOFFile, v::MOI.VariableReference, key::String)
    MOI.isvalid(m, v) && haskey(m[v], key)
end

MOI.get(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference) = getattr(m, v, "name")
MOI.get(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference) = getattr(m, v, "VariablePrimalStart")

MOI.canset(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference) = MOI.isvalid(m, v)
MOI.canset(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference) = MOI.isvalid(m, v)

MOI.canget(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference) = hasattr(m, v, "name")
MOI.canget(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference) = hasattr(m, v, "VariablePrimalStart")

MOI.set!(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference, name::String) = setattr!(m, c, "name", name)
MOI.set!(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference, value) = setattr!(m, c, "ConstraintPrimalStart", value)
MOI.set!(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference, value) = setattr!(m, c, "ConstraintDualStart", value)

function setattr!(m::MOFFile, c::MOI.ConstraintReference, key::String, val)
    m["constraints"][m.constrmap[c.value]][key] = val
end
function getattr(m::MOFFile, c::MOI.ConstraintReference, key::String)
    m["constraints"][m.constrmap[c.value]][key]
end
function hasattr(m::MOFFile, c::MOI.ConstraintReference, key::String)
    MOI.isvalid(m, c) && haskey(m["constraints"][m.constrmap[c.value]], key)
end
MOI.get(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = getattr(m, c, "name")
MOI.get(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = getattr(m, c, "ConstraintPrimalStart")
MOI.get(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = getattr(m, c, "ConstraintDualStart")

MOI.canset(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
MOI.canset(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
MOI.canset(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = MOI.isvalid(m, c)

MOI.canget(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = hasattr(m, c, "name")
MOI.canget(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = hasattr(m, c, "ConstraintPrimalStart")
MOI.canget(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = hasattr(m, c, "ConstraintDualStart")

function MOI.get(m::MOFFile, ::MOI.ConstraintFunction, c::MOI.ConstraintReference)
    parse!(m, m[c]["function"])
end
MOI.canget(m::MOFFile, ::MOI.ConstraintFunction, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
function MOI.get(m::MOFFile, ::MOI.ConstraintSet, c::MOI.ConstraintReference)
    parse!(m, m[c]["set"])
end
MOI.canget(m::MOFFile, ::MOI.ConstraintSet, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
