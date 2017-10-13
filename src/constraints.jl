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
    m.constrmap[UInt64(idx)] = idx
    return MOI.ConstraintReference{F,S}(idx)
end

MOI.canaddconstraint(m::MOFFile, func::MOI.AbstractFunction, set::MOI.AbstractSet) = Base.applicable(Object!, m, func) && Base.applicable(Object, set)

MOI.isvalid(m::MOFFile, ref::MOI.ConstraintReference) = haskey(m.constrmap, ref.value)

getconstraint(m::MOFFile, c::MOI.ConstraintReference) = m["constraints"][m.constrmap[c.value]]

function MOI.delete!(m::MOFFile, c::MOI.ConstraintReference)
    idx = m.constrmap[c.value]
    splice!(m["constraints"], idx)
    for (key, val) in m.constrmap
        if val > idx
            m.constrmap[key] -= 1
        end
    end
end
MOI.candelete(m::MOFFile, c::MOI.ConstraintReference) = MOI.isvalid(m, c)

function MOI.transformconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, newset::S2) where F where S where S2<:MOI.AbstractSet
    con = getconstraint(m, c)
    con["set"] = Object(newset)
    idx = m.constrmap(c.value)
    delete!(m.constrmap, c.value)
    MOI.ConstraintReference{F,S2}(idx)
end

MOI.cantransformconstraint(m::MOFFile, c::MOI.ConstraintReference, set::MOI.AbstractSet) = Base.applicable(Object, set)

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, newfunc::F) where F where S
    con = getconstraint(m, c)
    con["function"] = Object!(m, newfunc)
    c
end

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, newset::S) where F where S
    con = getconstraint(m, c)
    con["set"] = Object(newset)
    c
end

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.ScalarConstantChange) where F<:MOI.AbstractScalarFunction where S
    con = getconstraint(m, c)
    con["function"]["constant"] = chg.new_constant
end

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.VectorConstantChange) where F<:MOI.AbstractVectorFunction where S
    con = getconstraint(m, c)
    con["function"]["constant"] = chg.new_constant
end

MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference, set::MOI.AbstractSet) = Base.applicable(Object, set)
MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference, func::MOI.AbstractFunction) = Base.applicable(Object!, m, func)

function MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.VectorConstantChange) where F<:MOI.AbstractVectorFunction where S
    true
end
function MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.ScalarConstantChange) where F<:MOI.AbstractScalarFunction where S
    true
end
