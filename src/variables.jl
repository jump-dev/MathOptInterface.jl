function MOI.addvariable!(m::MOFFile)
    m.current_reference.variable += 1
    idx = m.current_reference.variable

    v = MOI.VariableReference(idx)
    push!(m["variables"], Object("name"=>"x$(idx)"))
    m.varmap[v] = length(m["variables"])
    m.namemap["x$(idx)"] = v
    v
end
MOI.addvariables!(m::MOFFile, n::Int) = [MOI.addvariable!(m) for i in 1:n]

MOI.isvalid(m::MOFFile, ref::MOI.VariableReference) = haskey(m.varmap, ref)

Base.getindex(m::MOFFile, v::MOI.VariableReference) = m["variables"][m.varmap[v]]
