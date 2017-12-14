function MOI.addvariable!(m::MOFInstance)
    m.current_reference.variable += 1
    idx = m.current_reference.variable

    v = MOI.VariableIndex(idx)
    push!(m["variables"], Object("name"=>"x$(idx)"))
    m.varmap[v] = length(m["variables"])
    m.namemap["x$(idx)"] = v
    v
end
MOI.addvariables!(m::MOFInstance, n::Int) = [MOI.addvariable!(m) for i in 1:n]

MOI.isvalid(m::MOFInstance, ref::MOI.VariableIndex) = haskey(m.varmap, ref)

Base.getindex(m::MOFInstance, v::MOI.VariableIndex) = m["variables"][m.varmap[v]]
