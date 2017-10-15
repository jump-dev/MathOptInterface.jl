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
