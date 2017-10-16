function MOI.addvariable!(m::MOFFile)
    i = length(m["variables"]) + 1
    v = MOI.VariableReference(i)
    push!(m["variables"], Object("name"=>"x$(i)"))
    m.ext[v] = i
    v
end
MOI.addvariables!(m::MOFFile, n::Int) = [MOI.addvariable!(m) for i in 1:n]

MOI.isvalid(m::MOFFile, ref::MOI.VariableReference) = haskey(m.ext, ref)
