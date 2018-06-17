using Compat
using Compat.Test

"""
    evalvariables(varval::Function, f::AbstractFunction)

Returns the value of function `f` if each variable index `vi` is evaluate as `varval(vi)`.
"""
function evalvariables end
evalvariables(varval::Function, f::MOI.SingleVariable) = varval(f.variable)
evalvariables(varval::Function, f::MOI.VectorOfVariables) = varval.(f.variables)
evalvariables(varval::Function, f::MOI.ScalarAffineFunction) = sum(evalterm.(varval, f.terms)) + f.constant
function evalvariables(varval::Function, f::MOI.VectorAffineFunction)
    out = copy(f.constants)
    for t in f.terms
        out[t.output_index] += evalterm(varval, t.scalar_term)
    end
    out
end
evalvariables(varval::Function, f::MOI.ScalarQuadraticFunction) = sum(evalterm.(varval, f.affine_terms)) + sum(evalterm.(varval, f.quadratic_terms)) + f.constant
function evalvariables(varval::Function, f::MOI.VectorQuadraticFunction)
    out = copy(f.constants)
    for t in f.affine_terms
        out[t.output_index] += evalterm(varval, t.scalar_term)
    end
    for t in f.quadratic_terms
        out[t.output_index] += evalterm(varval, t.scalar_term)
    end
    out
end
# Affine term
evalterm(varval::Function, t::MOI.ScalarAffineTerm) = t.coefficient * varval(t.variable_index)
# Quadratic term
function evalterm(varval::Function, t::MOI.ScalarQuadraticTerm)
    tval = t.coefficient * varval(t.variable_index_1) * varval(t.variable_index_2)
    t.variable_index_1 == t.variable_index_2 ? tval/2 : tval
end

mapvariable(varmap::Function, t::MOI.ScalarAffineTerm) = MOI.ScalarAffineTerm(t.coefficient, varmap(t.variable_index))
mapvariable(varmap::Function, t::MOI.VectorAffineTerm) = MOI.VectorAffineTerm(t.output_index, mapvariable(varmap, t.scalar_term))
mapvariable(varmap::Function, t::MOI.ScalarQuadraticTerm) = MOI.ScalarQuadraticTerm(t.coefficient, varmap(t.variable_index_1), varmap(t.variable_index_2))
mapvariable(varmap::Function, t::MOI.VectorQuadraticTerm) = MOI.VectorQuadraticTerm(t.output_index, mapvariable(varmap, t.scalar_term))

mapvariables(varmap::Function, f::MOI.SingleVariable) = MOI.SingleVariable(varmap(f.variable))
mapvariables(varmap::Function, f::MOI.VectorOfVariables) = MOI.VectorOfVariables(varmap.(f.variables))
mapvariables(varmap::Function, f::Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}) = typeof(f)(mapvariable.(varmap, f.terms), _constant(f))
mapvariables(varmap::Function, f::Union{MOI.ScalarQuadraticFunction, MOI.VectorQuadraticFunction}) = typeof(f)(mapvariable.(varmap, f.affine_terms), mapvariable.(varmap, f.quadratic_terms), _constant(f))

mapvariables(varmap, f::MOI.AbstractFunction) = mapvariables(vi -> varmap[vi], f)

mapvariables(varmap::Function, change::Union{MOI.ScalarConstantChange, MOI.VectorConstantChange}) = change
mapvariables(varmap::Function, change::MOI.ScalarCoefficientChange) = MOI.ScalarCoefficientChange(varmap(change.variable), change.new_coefficient)
mapvariables(varmap::Function, change::MOI.MultirowChange) = MOI.MultirowChange(varmap(change.variable), change.new_coefficients)

mapvariables(varmap, f::MOI.AbstractFunctionModification) = mapvariables(vi -> varmap[vi], f)

# Cat for MOI sets
moilength(f::Union{MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction}) = 1
moilength(f::Union{MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}) = length(f.constants)
_constant(f::Union{MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction}) = f.constant
_constant(f::Union{MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}) = f.constants
constant(f::Union{MOI.ScalarAffineFunction, MOI.ScalarQuadraticFunction}) = [f.constant]
constant(f::Union{MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}) = f.constants
offsetterm(t::MOI.ScalarAffineTerm, offset::Int) = MOI.VectorAffineTerm(offset+1, t)
offsetterm(t::MOI.VectorAffineTerm, offset::Int) = MOI.VectorAffineTerm(offset+t.output_index, t.scalar_term)
offsetterms(f::Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}, offset::Int) = offsetterm.(f.terms, offset)
function moivcat(f::Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}...)
    n = length(f)
    offsets = cumsum(collect(moilength.(f)))
    offsets = [0; offsets[1:(n-1)]]
    terms = vcat((offsetterms.(f, offsets))...)
    cst = vcat(constant.(f)...)
    MOI.VectorAffineFunction(terms, cst)
end

# Define conversion SingleVariable -> ScalarAffineFunction and VectorOfVariable -> VectorAffineFunction{T}
function MOI.ScalarAffineFunction{T}(f::MOI.SingleVariable) where T
    MOI.ScalarAffineFunction([MOI.ScalarAffineTerm(one(T), f.variable)], zero(T))
end
function MOI.VectorAffineFunction{T}(f::MOI.VectorOfVariables) where T
    n = length(f.variables)
    MOI.VectorAffineFunction(map(i -> MOI.VectorAffineTerm(i, MOI.ScalarAffineTerm(one(T), f.variables[i])), 1:n), zeros(T, n))
end

# Implements iterator interface
struct ScalarFunctionIterator{F<:MOI.AbstractVectorFunction}
    f::F
end
eachscalar(f::MOI.AbstractVectorFunction) = ScalarFunctionIterator(f)
Base.start(it::ScalarFunctionIterator) = 1
Base.done(it::ScalarFunctionIterator, state) = state > length(it)
Base.next(it::ScalarFunctionIterator, state) = (it[state], state+1)
Base.length(it::ScalarFunctionIterator{MOI.VectorOfVariables}) = length(it.f.variables)
Base.length(it::ScalarFunctionIterator{<:Union{MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}}) = moilength(it.f)
Base.eltype(it::ScalarFunctionIterator{MOI.VectorOfVariables}) = MOI.SingleVariable
Base.eltype(it::ScalarFunctionIterator{MOI.VectorAffineFunction{T}}) where T = MOI.ScalarAffineFunction{T}
Base.eltype(it::ScalarFunctionIterator{MOI.VectorQuadraticFunction{T}}) where T = MOI.ScalarQuadraticFunction{T}
Compat.lastindex(it::ScalarFunctionIterator) = length(it)

# Define getindex for Vector functions

Base.getindex(it::ScalarFunctionIterator{MOI.VectorOfVariables}, i::Integer) = MOI.SingleVariable(it.f.variables[i])
# Returns the scalar terms of output_index i
function scalar_terms_at_index(terms::Vector{<:Union{MOI.VectorAffineTerm, MOI.VectorQuadraticTerm}}, i::Int)
    I = findall(t -> t.output_index == i, terms)
    map(i -> terms[i].scalar_term, I)
end
function Base.getindex(it::ScalarFunctionIterator{<:MOI.VectorAffineFunction}, i::Integer)
    MOI.ScalarAffineFunction(scalar_terms_at_index(it.f.terms, i), it.f.constants[i])
end
function Base.getindex(it::ScalarFunctionIterator{<:MOI.VectorQuadraticFunction}, i::Integer)
    MOI.ScalarQuadraticFunction(scalar_terms_at_index(it.f.affine_terms, i), scalar_terms_at_index(it.f.quadratic_terms, i), it.f.constants[i])
end

function Base.getindex(it::ScalarFunctionIterator{MOI.VectorAffineFunction{T}}, I::AbstractVector) where T
    terms = MOI.VectorAffineTerm{T}[]
    constant = Vector{T}(undef, length(I))
    for (i, j) in enumerate(I)
        g = it[j]
        append!(terms, map(t -> MOI.VectorAffineTerm(i, t), g.terms))
        constant[i] = g.constant
    end
    MOI.VectorAffineFunction(terms, constant)
end

"""
    canonical(f::AbstractFunction)

Returns the function in a canonical form, i.e.
* A term appear only once.
* The coefficients are nonzero.
* The terms appear in increasing order of variable where there the order of the variables is the order of their value.
* For a `AbstractVectorFunction`, the terms are sorted in ascending order of output index.

### Examples
If `x` (resp. `y`, `z`) is `VariableIndex(1)` (resp. 2, 3).
The canonical representation of `ScalarAffineFunction([y, x, z, x, z], [2, 1, 3, -2, -3], 5)` is `ScalarAffineFunction([x, y], [-1, 2], 5)`.

"""
function canonical(f::SAF{T}) where T
    sorted_terms = sort(f.terms, by = t -> t.variable_index.value)
    terms = MOI.ScalarAffineTerm{T}[]
    for t in sorted_terms
        if !isempty(terms) && t.variable_index == last(terms).variable_index
            terms[end] = MOI.ScalarAffineTerm(terms[end].coefficient + t.coefficient, t.variable_index)
        elseif !iszero(t.coefficient)
            if !isempty(terms) && iszero(last(terms).coefficient)
                terms[end] = t
            else
                push!(terms, t)
            end
        end
    end
    if !isempty(terms) && iszero(last(terms).coefficient)
        pop!(terms)
    end
    SAF{T}(terms, f.constant)
end
function canonical(f::VAF{T}) where T
    sorted_terms = sort(f.terms, by = t -> (t.output_index, t.scalar_term.variable_index.value))
    terms = MOI.VectorAffineTerm{T}[]
    for t in sorted_terms
        if !isempty(terms) && t.output_index == last(terms).output_index && t.scalar_term.variable_index == last(terms).scalar_term.variable_index
            terms[end] = MOI.VectorAffineTerm(t.output_index, MOI.ScalarAffineTerm(terms[end].scalar_term.coefficient + t.scalar_term.coefficient, t.scalar_term.variable_index))
        elseif !iszero(t.scalar_term.coefficient)
            if !isempty(terms) && iszero(last(terms).scalar_term.coefficient)
                terms[end] = t
            else
                push!(terms, t)
            end
        end
    end
    if !isempty(terms) && iszero(last(terms).scalar_term.coefficient)
        pop!(terms)
    end
    VAF{T}(terms, f.constants)
end

function test_variablenames_equal(model, variablenames)
    seen_name = Dict(name => false for name in variablenames)
    for index in MOI.get(model, MOI.ListOfVariableIndices())
        vname = MOI.get(model, MOI.VariableName(), index)
        if !haskey(seen_name, vname)
            error("Variable with name $vname present in model but not expected list of variable names.")
        end
        if seen_name[vname]
            error("Variable with name $vname present twice in model (shouldn't happen!)")
        end
        seen_name[vname] = true
    end
    for (vname,seen) in seen_name
        if !seen
            error("Did not find variable with name $vname in intance.")
        end
    end
end
function test_constraintnames_equal(model, constraintnames)
    seen_name = Dict(name => false for name in constraintnames)
    for (F,S) in MOI.get(model, MOI.ListOfConstraints())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            cname = MOI.get(model, MOI.ConstraintName(), index)
            if !haskey(seen_name, cname)
                error("Constraint with name $cname present in model but not expected list of constraint names.")
            end
            if seen_name[cname]
                error("Constraint with name $cname present twice in model (shouldn't happen!)")
            end
            seen_name[cname] = true
        end
    end
    for (cname,seen) in seen_name
        if !seen
            error("Did not find constraint with name $cname in intance.")
        end
    end
end

map_variables(f::Vector{MOI.VariableIndex}, variablemap::Dict{MOI.VariableIndex,MOI.VariableIndex}) = map(v -> variablemap[v], f)
map_variables(f, variablemap) = f


for moiname in [MOI.ScalarAffineFunction,MOI.VectorAffineFunction,
                 MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction,
                 MOI.SingleVariable,MOI.VectorOfVariables]
    fields = fieldnames(moiname)
    constructor = Expr(:call, moiname, [Expr(:call,:map_variables,Expr(:.,:f,Base.Meta.quot(field)),:variablemap) for field in fields]...)
    @eval map_variables(f::$moiname, variablemap::Dict{MOI.VariableIndex,MOI.VariableIndex}) = $constructor
end

"""
    test_models_equal(model1::ModelLike, model2::ModelLike, variablenames::Vector{String}, constraintnames::Vector{String})

Test that `model1` and `model2` are identical using `variablenames` as as keys for the variable names and `constraintnames` as keys for the constraint names. Uses `Base.Test` macros.
"""
function test_models_equal(model1::MOI.ModelLike, model2::MOI.ModelLike, variablenames::Vector{String}, constraintnames::Vector{String})
    # TODO: give test-friendly feedback instead of errors?
    test_variablenames_equal(model1, variablenames)
    test_variablenames_equal(model2, variablenames)
    test_constraintnames_equal(model1, constraintnames)
    test_constraintnames_equal(model2, constraintnames)

    variablemap_2to1 = Dict{MOI.VariableIndex,MOI.VariableIndex}()
    for vname in variablenames
        index1 = MOI.get(model1, MOI.VariableIndex, vname)
        index2 = MOI.get(model2, MOI.VariableIndex, vname)
        variablemap_2to1[index2] = index1
    end

    for cname in constraintnames
        index1 = MOI.get(model1, MOI.ConstraintIndex, cname)
        index2 = MOI.get(model2, MOI.ConstraintIndex, cname)
        f1 = MOI.get(model1, MOI.ConstraintFunction(), index1)
        f2 = MOI.get(model2, MOI.ConstraintFunction(), index2)
        s1 = MOI.get(model1, MOI.ConstraintSet(), index1)
        s2 = MOI.get(model2, MOI.ConstraintSet(), index2)
        @test isapprox(f1, map_variables(f2, variablemap_2to1))
        @test s1 == s2
    end

    for src in (model1, model2)
        for attr in MOI.get(src, MOI.ListOfModelAttributesSet())
            @test MOI.canget(model1, attr)
            value1 = MOI.get(model1, attr)
            @test MOI.canget(model2, attr)
            value2 = MOI.get(model2, attr)
            if value1 isa MOI.AbstractFunction
                @test value2 isa MOI.AbstractFunction
                @test isapprox(value1, attribute_value_map(variablemap_2to1, value2))
            else
                @test !(value2 isa MOI.AbstractFunction)
                @test value1 == value2
            end
        end
    end
end


_hasvar(v::MOI.VariableIndex, vi::MOI.VariableIndex) = v == vi
_hasvar(t::MOI.ScalarAffineTerm, vi::MOI.VariableIndex) = t.variable_index == vi
_hasvar(t::MOI.ScalarQuadraticTerm, vi::MOI.VariableIndex) = t.variable_index_1 == vi || t.variable_index_2 == vi
_hasvar(t::Union{MOI.VectorAffineTerm, MOI.VectorQuadraticTerm}, vi::MOI.VariableIndex) = _hasvar(t.scalar_term, vi)
# Removes terms or variables in `vis_or_terms` that contains the variable of index `vi`
_rmvar(vis_or_terms::Vector, vi::MOI.VariableIndex) = vis_or_terms[findall(t -> !_hasvar(t, vi), vis_or_terms)]

"""
    removevariable(f::AbstractFunction, vi::VariableIndex)

Return a new function `f` with the variable vi removed.
"""
function removevariable(f::MOI.VectorOfVariables, vi)
    MOI.VectorOfVariables(_rmvar(f.variables, vi))
end
function removevariable(f::Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}, vi)
    typeof(f)(_rmvar(f.terms, vi), _constant(f))
end
function removevariable(f::Union{MOI.ScalarQuadraticFunction, MOI.VectorQuadraticFunction}, vi)
    typeof(f)(_rmvar(f.affine_terms, vi), _rmvar(f.quadratic_terms, vi), _constant(f))
end

"""
    modifyfunction(f::AbstractFunction, change::AbstractFunctionModification)

Return a new function `f` modified according to `change`.
"""
modifyfunction(f::MOI.ScalarAffineFunction, change::MOI.ScalarConstantChange) = MOI.ScalarAffineFunction(f.terms, change.new_constant)
modifyfunction(f::MOI.VectorAffineFunction, change::MOI.VectorConstantChange) = MOI.VectorAffineFunction(f.terms, change.new_constant)
modifyfunction(f::MOI.ScalarQuadraticFunction, change::MOI.ScalarConstantChange) = MOI.ScalarQuadraticFunction(f.affine_terms, f.quadratic_terms, change.new_constant)
modifyfunction(f::MOI.VectorQuadraticFunction, change::MOI.VectorConstantChange) = MOI.VectorQuadraticFunction(f.affine_terms, f.quadratic_terms, change.new_constant)

function _modifycoefficient(terms::Vector{<:MOI.ScalarAffineTerm}, variable::MOI.VariableIndex, new_coefficient)
    terms = copy(terms)
    i = something(findfirst(t -> _hasvar(t, variable), terms), 0)
    if iszero(i)
        # The variable was not already in the function
        if !iszero(new_coefficient)
            push!(terms, MOI.ScalarAffineTerm(new_coefficient, variable))
        end
    else
        # The variable was already in the function
        if iszero(new_coefficient)
            deleteat!(terms, i)
        else
            terms[i] = MOI.ScalarAffineTerm(new_coefficient, variable)
        end
    end
    terms
end
function modifyfunction(f::MOI.ScalarAffineFunction, change::MOI.ScalarCoefficientChange)
    MOI.ScalarAffineFunction(_modifycoefficient(f.terms, change.variable, change.new_coefficient), f.constant)
end
function modifyfunction(f::MOI.ScalarQuadraticFunction, change::MOI.ScalarCoefficientChange)
    MOI.ScalarQuadraticFunction(_modifycoefficient(f.affine_terms, change.variable, change.new_coefficient),
                                f.quadratic_terms, f.constant)

end
function _modifycoefficients(n, terms::Vector{<:MOI.VectorAffineTerm}, variable::MOI.VariableIndex, new_coefficients)
    terms = copy(terms)
    # Maps between rows in the `VectorAffineTerm`s and indices in new_coefficients
    rowmap = zeros(Int, n)
    rowmap[map(c -> c[1], new_coefficients)] = 1:length(new_coefficients)
    del = Int[]
    for i in 1:length(terms)
        if _hasvar(terms[i], variable)
            row = terms[i].output_index
            j = rowmap[row]
            if !iszero(j) # If it is zero, it means that the row should not be changed
                if iszero(new_coefficients[j][2])
                    push!(del, i)
                else
                    terms[i] = MOI.VectorAffineTerm(row, MOI.ScalarAffineTerm(new_coefficients[j][2], variable))
                end
                rowmap[row] = 0 # We only change the first term of a row
            end
        end
    end
    deleteat!(terms, del)
    for (row, j) in enumerate(rowmap)
        if !iszero(j)
            push!(terms, MOI.VectorAffineTerm(row, MOI.ScalarAffineTerm(new_coefficients[j][2], variable)))
        end
    end
    terms
end
function modifyfunction(f::MOI.VectorAffineFunction, change::MOI.MultirowChange)
    MOI.VectorAffineFunction(_modifycoefficients(moilength(f), f.terms, change.variable, change.new_coefficients), f.constants)
end
function modifyfunction(f::MOI.VectorQuadraticFunction, change::MOI.MultirowChange)
    MOI.VectorQuadraticFunction(_modifycoefficients(moilength(f), f.affine_terms, change.variable, change.new_coefficients),
                                f.quadratic_terms, f.constants)
end
