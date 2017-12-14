function MOI.addconstraint!(m::MOFInstance, func::F, set::S) where F<:MOI.AbstractFunction where S<:MOI.AbstractSet
    m.current_reference.constraint += 1
    idx = m.current_reference.constraint
    push!(m["constraints"],
        Object(
            "name"     => "c$(idx)",
            "set"      => object(set),
            "function" =>  object!(m, func)
        )
    )
    m.constrmap[UInt64(idx)] = length(m["constraints"])
    return MOI.ConstraintIndex{F,S}(idx)
end

MOI.canaddconstraint(m::MOFInstance, func::MOI.AbstractFunction, set::MOI.AbstractSet) = Base.applicable(object!, m, func) && Base.applicable(object, set)

MOI.isvalid(m::MOFInstance, ref::MOI.ConstraintIndex) = haskey(m.constrmap, ref.value)

Base.getindex(m::MOFInstance, c::MOI.ConstraintIndex) = m["constraints"][m.constrmap[c.value]]

function MOI.delete!(m::MOFInstance, c::MOI.ConstraintIndex)
    idx = m.constrmap[c.value]
    splice!(m["constraints"], idx)
    for (key, val) in m.constrmap
        if val > idx
            m.constrmap[key] -= 1
        end
    end
end
MOI.candelete(m::MOFInstance, c::MOI.ConstraintIndex) = MOI.isvalid(m, c)

function MOI.transformconstraint!(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, newset::S2) where F where S where S2<:MOI.AbstractSet
    m[c]["set"] = object(newset)
    idx = m.constrmap[c.value]
    delete!(m.constrmap, c.value)
    MOI.ConstraintIndex{F,S2}(idx)
end

MOI.cantransformconstraint(m::MOFInstance, c::MOI.ConstraintIndex, set::MOI.AbstractSet) = Base.applicable(object, set)

function MOI.modifyconstraint!(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, newfunc::F) where F where S
    m[c]["function"] = object!(m, newfunc)
    c
end

function MOI.modifyconstraint!(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, newset::S) where F where S
    m[c]["set"] = object(newset)
    c
end

function MOI.modifyconstraint!(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, chg::MOI.ScalarConstantChange) where F<:MOI.AbstractScalarFunction where S
    m[c]["function"]["constant"] = chg.new_constant
end

function MOI.modifyconstraint!(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, chg::MOI.VectorConstantChange) where F<:MOI.AbstractVectorFunction where S
    m[c]["function"]["constant"] = chg.new_constant
end

MOI.canmodifyconstraint(m::MOFInstance, c::MOI.ConstraintIndex, set::MOI.AbstractSet) = Base.applicable(object, set)
MOI.canmodifyconstraint(m::MOFInstance, c::MOI.ConstraintIndex, func::MOI.AbstractFunction) = Base.applicable(object!, m, func)

function MOI.canmodifyconstraint(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, chg::MOI.VectorConstantChange) where F<:MOI.AbstractVectorFunction where S
    true
end
function MOI.canmodifyconstraint(m::MOFInstance, c::MOI.ConstraintIndex{F,S}, chg::MOI.ScalarConstantChange) where F<:MOI.AbstractScalarFunction where S
    true
end
