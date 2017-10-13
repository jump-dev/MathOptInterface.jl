function MOI.setobjective!(m::MOFFile, sense::MOI.OptimizationSense, func::MOI.AbstractScalarFunction)
    m["sense"] = Object(sense)
    m["objective"] = Object!(m, func)
end

function Object(sense::MOI.OptimizationSense)
    if sense == MOI.MaxSense
        return "max"
    elseif sense == MOI.MinSense
        return "min"
    end
    error("Sense $(sense) not recognised.")
end
