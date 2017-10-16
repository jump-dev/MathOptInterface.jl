MOI.getattribute(m::MOFFile, ::MOI.NumberOfVariables) = length(m["variables"])
MOI.cangetattribute(m::MOFFile, ::MOI.NumberOfVariables) = true

function MOI.setattribute!(m::MOFFile, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction)
    m["objective"] = Object!(m, func)
end
MOI.cansetattribute(m::MOFFile, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction) = true

MOI.getattribute(m::MOFFile, ::MOI.ObjectiveFunction) = parse!(m, m["objective"])
MOI.cangetattribute(m::MOFFile, ::MOI.ObjectiveFunction) = true

function MOI.setattribute!(m::MOFFile, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    m["sense"] = Object(sense)
end
MOI.cansetattribute(m::MOFFile, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense) = true

function MOI.getattribute(m::MOFFile, ::MOI.ObjectiveSense)
    if m["sense"] == "min"
        return MOI.MinSense
    elseif m["sense"] == "max"
        return MOI.MaxSense
    else
        error("Unknown objective sense $(m["sense"])")
    end
end
MOI.cangetattribute(m::MOFFile, ::MOI.ObjectiveSense) = true

function Object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end

"""
    MOI.setattribute!(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference, name::String)

Rename the variable `v` in the MOFFile `m` to `name`. This should be done
immediately after introducing a variable and before it is used in any constraints.

If the variable has already been used, this function will _not_ update the
previous references.
"""
function MOI.setattribute!(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference, name::String)
    current_name = MOI.getattribute(m, MOI.VariableName(), v)
    delete!(m.namemap, current_name)
    setattr!(m, v, "name", name)
    m.namemap[name] = v
end
MOI.setattribute!(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference, value) = setattr!(m, v, "VariablePrimalStart", value)

function setattr!(m::MOFFile, v::MOI.VariableReference, key::String, val)
    m[v][key] = val
end
function getattr(m::MOFFile, v::MOI.VariableReference, key::String)
    m[v][key]
end
function hasattr(m::MOFFile, v::MOI.VariableReference, key::String)
    MOI.isvalid(m, v) && haskey(m[v], key)
end

MOI.getattribute(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference) = getattr(m, v, "name")
MOI.getattribute(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference) = getattr(m, v, "VariablePrimalStart")

MOI.cansetattribute(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference) = MOI.isvalid(m, v)
MOI.cansetattribute(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference) = MOI.isvalid(m, v)

MOI.cangetattribute(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference) = hasattr(m, v, "name")
MOI.cangetattribute(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference) = hasattr(m, v, "VariablePrimalStart")

MOI.setattribute!(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference, name::String) = setattr!(m, c, "name", name)
MOI.setattribute!(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference, value) = setattr!(m, c, "ConstraintPrimalStart", value)
MOI.setattribute!(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference, value) = setattr!(m, c, "ConstraintDualStart", value)

function setattr!(m::MOFFile, c::MOI.ConstraintReference, key::String, val)
    m["constraints"][m.constrmap[c.value]][key] = val
end
function getattr(m::MOFFile, c::MOI.ConstraintReference, key::String)
    m["constraints"][m.constrmap[c.value]][key]
end
function hasattr(m::MOFFile, c::MOI.ConstraintReference, key::String)
    MOI.isvalid(m, c) && haskey(m["constraints"][m.constrmap[c.value]], key)
end
MOI.getattribute(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = getattr(m, c, "name")
MOI.getattribute(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = getattr(m, c, "ConstraintPrimalStart")
MOI.getattribute(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = getattr(m, c, "ConstraintDualStart")

MOI.cansetattribute(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
MOI.cansetattribute(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
MOI.cansetattribute(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = MOI.isvalid(m, c)

MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = hasattr(m, c, "name")
MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = hasattr(m, c, "ConstraintPrimalStart")
MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = hasattr(m, c, "ConstraintDualStart")

function MOI.getattribute(m::MOFFile, ::MOI.ConstraintFunction, c::MOI.ConstraintReference)
    parse!(m, m[c]["function"])
end
MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintFunction, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
function MOI.getattribute(m::MOFFile, ::MOI.ConstraintSet, c::MOI.ConstraintReference)
    parse!(m, m[c]["set"])
end
MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintSet, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
