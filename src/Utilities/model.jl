const C{F, S} = Tuple{CI{F, S}, F, S}

const EMPTYSTRING = ""

# Implementation of MOI for vector of constraint
function _addconstraint!(constrs::Vector{C{F, S}}, ci::CI, f::F, s::S) where {F, S}
    push!(constrs, (ci, f, s))
    length(constrs)
end

function _delete!(constrs::Vector, ci::CI, i::Int)
    deleteat!(constrs, i)
    @view constrs[i:end] # will need to shift it in constrmap
end

_getindex(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = ci
function _getindex(constrs::Vector, ci::CI, i::Int)
    _getindex(constrs[i]...)
end

_getfun(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = f
function _getfunction(constrs::Vector, ci::CI, i::Int)
    @assert ci.value == constrs[i][1].value
    _getfun(constrs[i]...)
end

_gets(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = s
function _getset(constrs::Vector, ci::CI, i::Int)
    @assert ci.value == constrs[i][1].value
    _gets(constrs[i]...)
end

_modifyconstr(ci::CI{F, S}, f::F, s::S, change::F) where {F, S} = (ci, change, s)
_modifyconstr(ci::CI{F, S}, f::F, s::S, change::S) where {F, S} = (ci, f, change)
_modifyconstr(ci::CI{F, S}, f::F, s::S, change::MOI.AbstractFunctionModification) where {F, S} = (ci, modifyfunction(f, change), s)
function _modifyconstraint!(constrs::Vector{C{F, S}}, ci::CI{F}, i::Int, change) where {F, S}
    constrs[i] = _modifyconstr(constrs[i]..., change)
end

_getnoc(constrs::Vector{C{F, S}}, noc::MOI.NumberOfConstraints{F, S}) where {F, S} = length(constrs)
# Might be called when calling NumberOfConstraint with different coefficient type than the one supported
_getnoc(constrs::Vector, noc::MOI.NumberOfConstraints) = 0

function _getloc(constrs::Vector{C{F, S}})::Vector{Tuple{DataType, DataType}} where {F, S}
    isempty(constrs) ? [] : [(F, S)]
end

_getlocr(constrs::Vector{C{F, S}}, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = map(constr -> constr[1], constrs)
_getlocr(constrs::Vector{<:C}, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = CI{F, S}[]

# Implementation of MOI for AbstractModel
abstract type AbstractModel{T} <: MOI.ModelLike end

getconstrloc(model::AbstractModel, ci::CI) = model.constrmap[ci.value]

# Variables
MOI.get(model::AbstractModel, ::MOI.NumberOfVariables) = length(model.varindices)
MOI.canaddvariable(model::AbstractModel) = true
function MOI.addvariable!(model::AbstractModel)
    v = VI(model.nextvariableid += 1)
    push!(model.varindices, v)
    v
end
function MOI.addvariables!(model::AbstractModel, n::Integer)
    [MOI.addvariable!(model) for i in 1:n]
end

function _removevar(ci::CI, f, s, vi::VI)
    (ci, removevariable(f, vi), s)
end
function _removevar(ci::CI, f::MOI.VectorOfVariables, s, vi::VI)
    g = removevariable(f, vi)
    if length(g.variables) != length(f.variables)
        t = updatedimension(s, length(g.variables))
    else
        t = s
    end
    (ci, g, t)
end
function _removevar!(constrs::Vector, vi::VI)
    for i in eachindex(constrs)
        constrs[i] = _removevar(constrs[i]..., vi)
    end
    []
end
function _removevar!(constrs::Vector{<:C{MOI.SingleVariable}}, vi::VI)
    # If a variable is removed, the SingleVariable constraints using this variable
    # need to be removed too
    rm = []
    for (ci, f, s) in constrs
        if f.variable == vi
            push!(rm, ci)
        end
    end
    rm
end
function MOI.delete!(model::AbstractModel, vi::VI)
    model.objective = removevariable(model.objective, vi)
    rm = broadcastvcat(constrs -> _removevar!(constrs, vi), model)
    for ci in rm
        MOI.delete!(model, ci)
    end
    delete!(model.varindices, vi)
    if haskey(model.varnames, vi)
        delete!(model.namesvar, model.varnames[vi])
        delete!(model.varnames, vi)
    end
end

function MOI.isvalid(model::AbstractModel, ci::CI{F, S}) where {F, S}
    if ci.value > length(model.constrmap)
        false
    else
        loc = getconstrloc(model, ci)
        if iszero(loc) # This means that it has been deleted
            false
        elseif loc > MOI.get(model, MOI.NumberOfConstraints{F, S}())
            false
        else
            ci == _getindex(model, ci, getconstrloc(model, ci))
        end
    end
end
MOI.isvalid(model::AbstractModel, vi::VI) = in(vi, model.varindices)

function MOI.get(model::AbstractModel, ::MOI.ListOfVariableIndices)
    vis = collect(model.varindices)
    sort!(vis, by=vi->vi.value) # It needs to be sorted by order of creation
    vis
end

# Names
MOI.canset(::AbstractModel, ::MOI.Name) = true
function MOI.set!(model::AbstractModel, ::MOI.Name, name::String)
    model.name = name
end
MOI.canget(model::AbstractModel, ::MOI.Name) = true
MOI.get(model::AbstractModel, ::MOI.Name) = model.name

MOI.canset(::AbstractModel, ::MOI.VariableName, vi::Type{VI}) = true
function MOI.set!(model::AbstractModel, ::MOI.VariableName, vi::VI, name::String)
    if !isempty(name) && haskey(model.namesvar, name) && model.namesvar[name] != vi
        error("Variable name $name is already used by $(model.namesvar[name])")
    end
    if haskey(model.varnames, vi)
        delete!(model.namesvar, model.varnames[vi])
    end
    model.varnames[vi] = name
    model.namesvar[name] = vi
end
MOI.canget(::AbstractModel, ::MOI.VariableName, ::Type{VI}) = true
MOI.get(model::AbstractModel, ::MOI.VariableName, vi::VI) = get(model.varnames, vi, EMPTYSTRING)

MOI.canget(model::AbstractModel, ::Type{VI}, name::String) = haskey(model.namesvar, name)
MOI.get(model::AbstractModel, ::Type{VI}, name::String) = model.namesvar[name]

MOI.canget(::AbstractModel, ::MOI.ListOfVariableAttributesSet) = true
function MOI.get(model::AbstractModel, ::MOI.ListOfVariableAttributesSet)::Vector{MOI.AbstractVariableAttribute}
    isempty(model.varnames) ? [] : [MOI.VariableName()]
end

MOI.canset(model::AbstractModel, ::MOI.ConstraintName, ::Type{<:CI}) = true
function MOI.set!(model::AbstractModel, ::MOI.ConstraintName, ci::CI, name::String)
    if !isempty(name) && haskey(model.namescon, name) && model.namescon[name] != ci
        error("Constraint name $name is already used by $(model.namescon[name])")
    end
    if haskey(model.connames, ci)
        delete!(model.namescon, model.connames[ci])
    end
    model.connames[ci] = name
    model.namescon[name] = ci
end
MOI.canget(model::AbstractModel, ::MOI.ConstraintName, ::Type{<:CI}) = true
MOI.get(model::AbstractModel, ::MOI.ConstraintName, ci::CI) = get(model.connames, ci, EMPTYSTRING)

MOI.canget(model::AbstractModel, CT::Type{<:CI}, name::String) = haskey(model.namescon, name) && model.namescon[name] isa CT
MOI.get(model::AbstractModel, ::Type{<:CI}, name::String) = model.namescon[name]

MOI.canget(::AbstractModel, ::MOI.ListOfConstraintAttributesSet) = true
function MOI.get(model::AbstractModel, ::MOI.ListOfConstraintAttributesSet)::Vector{MOI.AbstractConstraintAttribute}
    isempty(model.connames) ? [] : [MOI.ConstraintName()]
end

# Objective
MOI.canget(model::AbstractModel, ::MOI.ObjectiveSense) = model.senseset
MOI.get(model::AbstractModel, ::MOI.ObjectiveSense) = model.sense
MOI.canset(model::AbstractModel, ::MOI.ObjectiveSense) = true
function MOI.set!(model::AbstractModel, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    model.senseset = true
    model.sense = sense
end
MOI.canget(model::AbstractModel, ::MOI.ObjectiveFunction{T}) where T = model.objectiveset && typeof(model.objective) == T
function MOI.get(model::AbstractModel, ::MOI.ObjectiveFunction{T})::T where T
    if typeof(model.objective) != T
        throw(InexactError())
    end
    model.objective
end
MOI.canset(model::AbstractModel, ::MOI.ObjectiveFunction) = true
function MOI.set!(model::AbstractModel, ::MOI.ObjectiveFunction, f::MOI.AbstractFunction)
    model.objectiveset = true
    # f needs to be copied, see #2
    model.objective = deepcopy(f)
end

MOI.canmodifyobjective(model::AbstractModel, ::Type{<:MOI.AbstractFunctionModification}) = true
function MOI.modifyobjective!(model::AbstractModel, change::MOI.AbstractFunctionModification)
    model.objective = modifyfunction(model.objective, change)
end

MOI.canget(::AbstractModel, ::MOI.ListOfOptimizerAttributesSet) = true
MOI.get(::AbstractModel, ::MOI.ListOfOptimizerAttributesSet) = MOI.AbstractOptimizerAttribute[]
MOI.canget(::AbstractModel, ::MOI.ListOfModelAttributesSet) = true
function MOI.get(model::AbstractModel, ::MOI.ListOfModelAttributesSet)::Vector{MOI.AbstractModelAttribute}
    listattr = MOI.AbstractModelAttribute[]
    if model.senseset
        push!(listattr, MOI.ObjectiveSense())
    end
    if model.objectiveset
        push!(listattr, MOI.ObjectiveFunction{typeof(model.objective)}())
    end
    if !isempty(model.name)
        push!(listattr, MOI.Name())
    end
    listattr
end

# Constraints
function MOI.addconstraint!(model::AbstractModel, f::F, s::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # We give the index value `nextconstraintid + 1` to the new constraint.
    # As the same counter is used for all pairs of F-in-S constraints,
    # the index value is unique across all constraint types as mentionned in `@model`'s doc.
    ci = CI{F, S}(model.nextconstraintid += 1)
    # f needs to be copied, see #2
    push!(model.constrmap, _addconstraint!(model, ci, deepcopy(f), deepcopy(s)))
    ci
end

MOI.candelete(model::AbstractModel, i::MOI.Index) = MOI.isvalid(model, i)
function MOI.delete!(model::AbstractModel, ci::CI)
    for (ci_next, _, _) in _delete!(model, ci, getconstrloc(model, ci))
        model.constrmap[ci_next.value] -= 1
    end
    model.constrmap[ci.value] = 0
    if haskey(model.connames, ci)
        delete!(model.namescon, model.connames[ci])
        delete!(model.connames, ci)
    end
end

MOI.canmodifyconstraint(model::AbstractModel, ci::CI, change) = true
function MOI.modifyconstraint!(model::AbstractModel, ci::CI, change)
    _modifyconstraint!(model, ci, getconstrloc(model, ci), change)
end

MOI.get(model::AbstractModel, noc::MOI.NumberOfConstraints) = _getnoc(model, noc)

function MOI.get(model::AbstractModel, loc::MOI.ListOfConstraints)
    broadcastvcat(_getloc, model)
end

function MOI.get(model::AbstractModel, loc::MOI.ListOfConstraintIndices)
    broadcastvcat(constrs -> _getlocr(constrs, loc), model)
end

MOI.canget(model::AbstractModel, ::Union{MOI.NumberOfVariables,
                                         MOI.ListOfVariableIndices,
                                         MOI.NumberOfConstraints,
                                         MOI.ListOfConstraints,
                                         MOI.ListOfConstraintIndices,
                                         MOI.ObjectiveSense}) = true

function MOI.get(model::AbstractModel, ::MOI.ConstraintFunction, ci::CI)
    _getfunction(model, ci, getconstrloc(model, ci))
end

function MOI.get(model::AbstractModel, ::MOI.ConstraintSet, ci::CI)
    _getset(model, ci, getconstrloc(model, ci))
end

function MOI.isempty(model::AbstractModel)
    isempty(model.name) && !model.senseset && !model.objectiveset &&
    isempty(model.objective.terms) && iszero(model.objective.constant) &&
    iszero(model.nextvariableid) && iszero(model.nextconstraintid)
end

MOI.supports(::AbstractModel, ::MOI.ObjectiveFunction) = true
MOI.supportsconstraint(model::AbstractModel, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = MOI.canaddconstraint(model, F, S)
MOI.copy!(dest::AbstractModel, src::MOI.ModelLike; copynames=true) = defaultcopy!(dest, src, copynames)

# Allocate-Load Interface
# Even if the model does not need it and use defaultcopy!, it could be used by a layer that needs it
needsallocateload(model::AbstractModel) = false

allocatevariables!(model::AbstractModel, nvars) = MOI.addvariables!(model, nvars)
allocate!(model::AbstractModel, attr...) = MOI.set!(model, attr...)
canallocate(model::AbstractModel, attr::MOI.AnyAttribute) = MOI.canset(model, attr)
canallocate(model::AbstractModel, attr::MOI.AnyAttribute, IdxT::Type{<:MOI.Index}) = MOI.canset(model, attr, IdxT)
allocateconstraint!(model::AbstractModel, f::MOI.AbstractFunction, s::MOI.AbstractSet) = MOI.addconstraint!(model, f, s)
canallocateconstraint(model::AbstractModel, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = MOI.canaddconstraint(model, F, S)

function loadvariables!(::AbstractModel, nvars) end
function load!(::AbstractModel, attr...) end
canload(model::AbstractModel, attr::MOI.AnyAttribute) = MOI.canset(model, attr)
canload(model::AbstractModel, attr::MOI.AnyAttribute, IdxT::Type{<:MOI.Index}) = MOI.canset(model, attr, IdxT)
function loadconstraint!(::AbstractModel, ::CI, ::MOI.AbstractFunction, ::MOI.AbstractSet) end
canloadconstraint(model::AbstractModel, F::Type{<:MOI.AbstractFunction}, S::Type{<:MOI.AbstractSet}) = MOI.canaddconstraint(model, F, S)

# Can be used to access constraints of a model
"""
broadcastcall(f::Function, model::AbstractModel)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model.

# Examples

To add all constraints of the model to a solver `solver`, one can do
```julia
_addcon(solver, ci, f, s) = MOI.addconstraint!(solver, f, s)
function _addcon(solver, constrs::Vector)
    for constr in constrs
        _addcon(solver, constr...)
    end
end
MOIU.broadcastcall(constrs -> _addcon(solver, constrs), model)
```
"""
function broadcastcall end

"""
broadcastvcat(f::Function, model::AbstractModel)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the model and concatenate the results with `vcat` (this is used internally for `ListOfConstraints`).

# Examples

To get the list of all functions:
```julia
_getfun(ci, f, s) = f
_getfun(cindices::Tuple) = _getfun(cindices...)
_getfuns(constrs::Vector) = _getfun.(constrs)
MOIU.broadcastvcat(_getfuns, model)
```
"""
function broadcastvcat end

# Macro to generate Model
abstract type Constraints{F} end

abstract type SymbolFS end
struct SymbolFun <: SymbolFS
    s::Symbol
    typed::Bool
    cname::Symbol
end
struct SymbolSet <: SymbolFS
    s::Symbol
    typed::Bool
end

# QuoteNode prevents s from being interpolated and keeps it as a symbol
# Expr(:., MOI, s) would be MOI.s
# Expr(:., MOI, $s) would be Expr(:., MOI, EqualTo)
# Expr(:., MOI, :($s)) would be Expr(:., MOI, :EqualTo)
# Expr(:., MOI, :($(QuoteNode(s)))) is Expr(:., MOI, :(:EqualTo)) <- what we want

# (MOI, :Zeros) -> :(MOI.Zeros)
_mod(m::Module, s::Symbol) = Expr(:., m, :($(QuoteNode(s))))
# (:Zeros) -> :(MOI.Zeros)
_moi(s::Symbol) = _mod(MOI, s)
_set(s::SymbolSet) = _moi(s.s)
_fun(s::SymbolFun) = _moi(s.s)
function _typedset(s::SymbolSet)
    if s.typed
        :($(_set(s)){T})
    else
        _set(s)
    end
end
function _typedfun(s::SymbolFun)
    if s.typed
        :($(_fun(s)){T})
    else
        _fun(s)
    end
end

# Base.lowercase is moved to Unicode.lowercase in Julia v0.7
if VERSION >= v"0.7.0-DEV.2813"
    using Unicode
end
_field(s::SymbolFS) = Symbol(lowercase(string(s.s)))

_getC(s::SymbolSet) = :($MOIU.C{F, $(_typedset(s))})
_getC(s::SymbolFun) = _typedfun(s)

_getCV(s::SymbolSet) = :($(_getC(s))[])
_getCV(s::SymbolFun) = :($(s.cname){T, $(_getC(s))}())

_callfield(f, s::SymbolFS) = :($f(model.$(_field(s))))
_broadcastfield(b, s::SymbolFS) = :($b(f, model.$(_field(s))))

"""
macro model(modelname, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, typedscalarfunctions, vectorfunctions, typedvectorfunctions)

Creates a type `modelname` implementing the MOI model interface and containing `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions, `typedscalarfunctions` typed scalar functions, `vectorfunctions` vector functions and `typedvectorfunctions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

This implementation of the MOI model certifies that the constraint indices, in addition to being different between constraints `F`-in-`S` for the same types `F` and `S`,
are also different between constraints for different types `F` and `S`.
This means that for constraint indices `ci1`, `ci2` of this model, `ci1 == ci2` if and only if `ci1.value == ci2.value`.
This fact can be used to use the the value of the index directly in a dictionary representing a mapping between constraint indices and something else.

### Examples

The model describing an linear program would be:
```julia
@model LPModel () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)
```

Let `MOI` denote `MathOptInterface`, `MOIU` denote `MOI.Utilities` and `MOIU.C{F, S}` be defined as `MOI.Tuple{CI{F, S}, F, S}`.
The macro would create the types:
```julia
struct LPModelScalarConstraints{T, F <: MOI.AbstractScalarFunction} <: MOIU.Constraints{F}
    equalto::Vector{MOIU.C{F, MOI.EqualTo{T}}}
    greaterthan::Vector{MOIU.C{F, MOI.GreaterThan{T}}}
    lessthan::Vector{MOIU.C{F, MOI.LessThan{T}}}
    interval::Vector{MOIU.C{F, MOI.Interval{T}}}
end
struct LPModelVectorConstraints{T, F <: MOI.AbstractVectorFunction} <: MOIU.Constraints{F}
    zeros::Vector{MOIU.C{F, MOI.Zeros}}
    nonnegatives::Vector{MOIU.C{F, MOI.Nonnegatives}}
    nonpositives::Vector{MOIU.C{F, MOI.Nonpositives}}
end
mutable struct LPModel{T} <: MOIU.AbstractModel{T}
    name::String
    sense::MOI.OptimizationSense
    objective::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}
    nextvariableid::Int64
    varindices::Set{MOI.VariableIndex}
    varnames::Dict{MOI.VariableIndex, String}
    namesvar::Dict{String, MOI.VariableIndex}
    nextconstraintid::Int64
    connames::Dict{MOI.ConstraintIndex, String}
    namescon::Dict{String, MOI.ConstraintIndex}
    constrmap::Vector{Int}
    singlevariable::LPModelScalarConstraints{T, MOI.SingleVariable}
    scalaraffinefunction::LPModelScalarConstraints{T, MOI.ScalarAffineFunction{T}}
    vectorofvariables::LPModelVectorConstraints{T, MOI.VectorOfVariables}
    vectoraffinefunction::LPModelVectorConstraints{T, MOI.VectorAffineFunction{T}}
end
```
The type `LPModel` implements the MathOptInterface API except methods specific to solver models like `optimize!` or `getattribute` with `VariablePrimal`.
"""
macro model(modelname, ss, sst, vs, vst, sf, sft, vf, vft)
    scalarsets = [SymbolSet.(ss.args, false); SymbolSet.(sst.args, true)]
    vectorsets = [SymbolSet.(vs.args, false); SymbolSet.(vst.args, true)]

    scname = Symbol(string(modelname) * "ScalarConstraints")
    vcname = Symbol(string(modelname) * "VectorConstraints")

    scalarfuns = [SymbolFun.(sf.args, false, scname); SymbolFun.(sft.args, true, scname)]
    vectorfuns = [SymbolFun.(vf.args, false, vcname); SymbolFun.(vft.args, true, vcname)]
    funs = [scalarfuns; vectorfuns]

    scalarconstraints = :(struct $scname{T, F<:$MOI.AbstractScalarFunction} <: $MOIU.Constraints{F}; end)
    vectorconstraints = :(struct $vcname{T, F<:$MOI.AbstractVectorFunction} <: $MOIU.Constraints{F}; end)
    for (c, sets) in ((scalarconstraints, scalarsets), (vectorconstraints, vectorsets))
        for s in sets
            field = _field(s)
            push!(c.args[3].args, :($field::Vector{$(_getC(s))}))
        end
    end

    modeldef = quote
        mutable struct $modelname{T} <: $MOIU.AbstractModel{T}
            name::String
            senseset::Bool
            sense::$MOI.OptimizationSense
            objectiveset::Bool
            objective::Union{$MOI.SingleVariable, $MOI.ScalarAffineFunction{T}, $MOI.ScalarQuadraticFunction{T}}
            nextvariableid::Int64
            varindices::Set{$VI}
            varnames::Dict{$VI, String}
            namesvar::Dict{String, $VI}
            nextconstraintid::Int64
            connames::Dict{$CI, String}
            namescon::Dict{String, $CI}
            constrmap::Vector{Int} # Constraint Reference value ci -> index in array in Constraints
        end
    end
    for f in funs
        cname = f.cname
        field = _field(f)
        push!(modeldef.args[2].args[3].args, :($field::$cname{T, $(_getC(f))}))
    end

    code = quote
        function $MOIU.broadcastcall(f::Function, model::$modelname)
            $(Expr(:block, _broadcastfield.(:($MOIU.broadcastcall), funs)...))
        end
        function $MOIU.broadcastvcat(f::Function, model::$modelname)
            vcat($(_broadcastfield.(:($MOIU.broadcastvcat), funs)...))
        end
        function $MOI.empty!(model::$modelname{T}) where T
            model.name = ""
            model.senseset = false
            model.sense = $MOI.FeasibilitySense
            model.objectiveset = false
            model.objective = $SAF{T}(MOI.ScalarAffineTerm{T}[], zero(T))
            model.nextvariableid = 0
            model.varindices = Set{$VI}()
            model.varnames = Dict{Int64, String}()
            model.namesvar = Dict{String, $VI}()
            model.nextconstraintid = 0
            model.connames = Dict{Int64, String}()
            model.namescon = Dict{String, $CI}()
            model.constrmap = Int[]
            $(Expr(:block, _callfield.(:($MOI.empty!), funs)...))
        end
    end
    for (cname, sets) in ((scname, scalarsets), (vcname, vectorsets))
        code = quote
            $code
            function $MOIU.broadcastcall(f::Function, model::$cname)
                $(Expr(:block, _callfield.(:f, sets)...))
            end
            function $MOIU.broadcastvcat(f::Function, model::$cname)
                vcat($(_callfield.(:f, sets)...))
            end
            function $MOI.empty!(model::$cname)
                $(Expr(:block, _callfield.(:(Base.empty!), sets)...))
            end
        end
    end

    for (func, T) in ((:_addconstraint!, CI), (:_modifyconstraint!, CI), (:_delete!, CI), (:_getindex, CI), (:_getfunction, CI), (:_getset, CI), (:_getnoc, MOI.NumberOfConstraints))
        funct = _mod(MOIU, func)
        for (c, sets) in ((scname, scalarsets), (vcname, vectorsets))
            for s in sets
                set = _set(s)
                field = _field(s)
                code = quote
                    $code
                    $funct(model::$c, ci::$T{F, <:$set}, args...) where F = $funct(model.$field, ci, args...)
                end
            end
        end

        for f in funs
            fun = _fun(f)
            field = _field(f)
            code = quote
                $code
                $funct(model::$modelname, ci::$T{<:$fun}, args...) = $funct(model.$field, ci, args...)
            end
        end
    end

    return esc(quote
        $scalarconstraints
        function $scname{T, F}() where {T, F}
            $scname{T, F}($(_getCV.(scalarsets)...))
        end

        $vectorconstraints
        function $vcname{T, F}() where {T, F}
            $vcname{T, F}($(_getCV.(vectorsets)...))
        end

        $modeldef
        function $modelname{T}() where T
            $modelname{T}("", false, $MOI.FeasibilitySense, false, $SAF{T}($MOI.ScalarAffineTerm{T}[], zero(T)),
                   0, Set{$VI}(), Dict{$VI, String}(), Dict{String, $VI}(),
                   0, Dict{$CI, String}(), Dict{String, $CI}(), Int[],
                   $(_getCV.(funs)...))
        end

        $MOI.canget(model::$modelname{T}, ::Union{MOI.ConstraintFunction,
                                                  MOI.ConstraintSet}, ::Type{$CI{F, S}}) where {T, F<:Union{$(_typedfun.(scalarfuns)...)},
                                                                                                   S<:Union{$(_typedset.(scalarsets)...)}} = true
        $MOI.canget(model::$modelname{T}, ::Union{MOI.ConstraintFunction,
                                                  MOI.ConstraintSet}, ::Type{$CI{F, S}}) where {T, F<:Union{$(_typedfun.(vectorfuns)...)},
                                                                                                   S<:Union{$(_typedset.(vectorsets)...)}} = true

        $MOI.canaddconstraint(model::$modelname{T}, ::Type{<:Union{$(_typedfun.(scalarfuns)...)}}, ::Type{<:Union{$(_typedset.(scalarsets)...)}}) where T = true
        $MOI.canaddconstraint(model::$modelname{T}, ::Type{<:Union{$(_typedfun.(vectorfuns)...)}}, ::Type{<:Union{$(_typedset.(vectorsets)...)}}) where T = true

        $code

    end)
end
