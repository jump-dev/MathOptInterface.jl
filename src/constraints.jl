function MOI.addconstraint!(m::MOFFile, func::F, set::S) where F<:MOI.AbstractFunction where S<:MOI.AbstractSet
    m.current_reference.constraint += 1
    idx = m.current_reference.constraint
    push!(m["constraints"],
        Object(
            "name"     => "c$(idx)",
            "set"      => object(set),
            "function" =>  object!(m, func),
            # "ConstraintPrimalStart" => NaN,
            # "ConstraintDualStart" => NaN,
        )
    )
    m.constrmap[UInt64(idx)] = length(m["constraints"])
    return MOI.ConstraintReference{F,S}(idx)
end

MOI.canaddconstraint(m::MOFFile, func::MOI.AbstractFunction, set::MOI.AbstractSet) = Base.applicable(object!, m, func) && Base.applicable(object, set)

MOI.isvalid(m::MOFFile, ref::MOI.ConstraintReference) = haskey(m.constrmap, ref.value)

Base.getindex(m::MOFFile, c::MOI.ConstraintReference) = m["constraints"][m.constrmap[c.value]]

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
    m[c]["set"] = object(newset)
    idx = m.constrmap[c.value]
    delete!(m.constrmap, c.value)
    MOI.ConstraintReference{F,S2}(idx)
end

MOI.cantransformconstraint(m::MOFFile, c::MOI.ConstraintReference, set::MOI.AbstractSet) = Base.applicable(object, set)

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, newfunc::F) where F where S
    m[c]["function"] = object!(m, newfunc)
    c
end

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, newset::S) where F where S
    m[c]["set"] = object(newset)
    c
end

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.ScalarConstantChange) where F<:MOI.AbstractScalarFunction where S
    m[c]["function"]["constant"] = chg.new_constant
end

function MOI.modifyconstraint!(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.VectorConstantChange) where F<:MOI.AbstractVectorFunction where S
    m[c]["function"]["constant"] = chg.new_constant
end

MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference, set::MOI.AbstractSet) = Base.applicable(object, set)
MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference, func::MOI.AbstractFunction) = Base.applicable(object!, m, func)

function MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.VectorConstantChange) where F<:MOI.AbstractVectorFunction where S
    true
end
function MOI.canmodifyconstraint(m::MOFFile, c::MOI.ConstraintReference{F,S}, chg::MOI.ScalarConstantChange) where F<:MOI.AbstractScalarFunction where S
    true
end
