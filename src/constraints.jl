function MOI.addconstraint!(m::MOFFile, func::F, set::S, name::String="") where F<:MOI.AbstractFunction where S<:MOI.AbstractSet
    idx = length(m["constraints"]) + 1
    if name == ""
        name = "c$(idx)"
    end
    push!(m["constraints"],
        Object(
            "name"     => name,
            "set"      => Object(set),
            "function" =>  Object!(m, func)
        )
    )
    return MOI.ConstraintReference{F,S}(idx)
end

MOI.isvalid(m::MOFFile, ref::MOI.ConstraintReference) = ref.value <= m["constraints"]
