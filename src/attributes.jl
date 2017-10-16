function MOI.setattribute!(m::MOFFile, ::MOI.ObjectiveFunction, func::MOI.AbstractScalarFunction)
    m["objective"] = Object!(m, func)
end

function MOI.setattribute!(m::MOFFile, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    m["sense"] = Object(sense)
end

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
MOI.setattribute!(m::MOFFile, ::MOI.VariableName, v::MOI.VariableReference, name::String) = setattr!(m, v, "name", name)
MOI.setattribute!(m::MOFFile, ::MOI.VariablePrimalStart, v::MOI.VariableReference, value) = setattr!(m, v, "VariablePrimalStart", value)

function setattr!(m::MOFFile, v::MOI.VariableReference, key::String, val)
    m["variables"][m.ext[v]][key] = val
end
function getattr(m::MOFFile, v::MOI.VariableReference, key::String)
    m["variables"][m.ext[v]][key]
end
function hasattr(m::MOFFile, v::MOI.VariableReference, key::String)
    MOI.isvalid(m, v) && haskey(m["variables"][m.ext[v]], key)
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
MOI.getattribute(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = getattr(m, v, "name")
MOI.getattribute(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = getattr(m, v, "ConstraintPrimalStart")
MOI.getattribute(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = getattr(m, v, "ConstraintDualStart")

MOI.cansetattribute(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
MOI.cansetattribute(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = MOI.isvalid(m, c)
MOI.cansetattribute(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = MOI.isvalid(m, c)

MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintName, c::MOI.ConstraintReference) = hasattr(m, c, "name")
MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintPrimalStart, c::MOI.ConstraintReference) = hasattr(m, c, "ConstraintPrimalStart")
MOI.cangetattribute(m::MOFFile, ::MOI.ConstraintDualStart, c::MOI.ConstraintReference) = hasattr(m, c, "ConstraintDualStart")
