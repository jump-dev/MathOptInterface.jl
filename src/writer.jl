function MOI.writeproblem(m::MOFFile, io::IO, indent::Int=0)
    if indent > 0
        write(io, JSON.json(m.d, indent))
    else
        write(io, JSON.json(m.d))
    end
end
function MOI.writeproblem(m::MOFFile, f::String, indent::Int=0)
    open(f, "w") do io
        MOI.writeproblem(m, io, indent)
    end
end


function MOI.setobjective!(m::MOFFile, sense::MOI.OptimizationSense, func::MOI.AbstractFunction)
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
