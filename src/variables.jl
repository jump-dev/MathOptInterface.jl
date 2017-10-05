function MOI.addvariable!(m::MOFFile, name::String="")
    i = length(m["variables"]) + 1
    v = MOI.VariableReference(i)
    if name == ""
        push!(m["variables"], "x$(i)")
    else
        push!(m["variables"], name)
    end
    m.ext[v] = i
    v
end
MOI.addvariables!(m::MOFFile, n::Int, names::Vector{String}=fill("", n)) = [MOI.addvariable!(m, names[i]) for i in 1:n]

"""
    rename!(m::MOFFile, v::MOI.VariableReference, name::String)

Rename the variable `v` in the MOFFile `m` to `name`. This should be done
immediately after introducing a variable and before it is used in any constraints.

If the variable has already been used, this function will _not_ update the
previous references.
"""
function rename!(m::MOFFile, v::MOI.VariableReference, name::String)
    i = m.ext[v]
    m["variables"][i] = name
end
