using Compat
using Compat.Test

"""
    evalvariables(varval::Function, f::AbstractFunction)

Returns the value of function `f` if each variable index `vi` is evaluated as `varval(vi)`.
"""
function evalvariables end
evalvariables(varval::Function, f::SVF) = varval(f.variable)
evalvariables(varval::Function, f::VVF) = varval.(f.variables)
function evalvariables(varval::Function, f::SAF)
    @static if VERSION >= v"0.7-"
        return mapreduce(t->evalterm(varval, t), +, f.terms, init=f.constant)
    else
        return mapreduce(t->evalterm(varval, t), +, f.constant, f.terms)
    end
end
function evalvariables(varval::Function, f::VAF)
    out = copy(f.constants)
    for t in f.terms
        out[t.output_index] += evalterm(varval, t.scalar_term)
    end
    out
end
function evalvariables(varval::Function, f::SQF)
    init = zero(f.constant)
    @static if VERSION >= v"0.7-"
        lin = mapreduce(t->evalterm(varval, t), +, f.affine_terms, init=init)
        quad = mapreduce(t->evalterm(varval, t), +, f.quadratic_terms, init=init)
    else
        lin = mapreduce(t->evalterm(varval, t), +, init, f.affine_terms)
        quad = mapreduce(t->evalterm(varval, t), +, init, f.quadratic_terms)
    end
    return lin + quad + f.constant
end
function evalvariables(varval::Function, f::VQF)
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
function evalterm(varval::Function, t::MOI.ScalarAffineTerm)
    return t.coefficient * varval(t.variable_index)
end
# Quadratic term
function evalterm(varval::Function, t::MOI.ScalarQuadraticTerm)
    tval = t.coefficient * varval(t.variable_index_1) * varval(t.variable_index_2)
    t.variable_index_1 == t.variable_index_2 ? tval/2 : tval
end

function mapvariable(varmap::Function, t::MOI.ScalarAffineTerm)
    return MOI.ScalarAffineTerm(t.coefficient, varmap(t.variable_index))
end
function mapvariable(varmap::Function, t::MOI.VectorAffineTerm)
    return MOI.VectorAffineTerm(t.output_index, mapvariable(varmap, t.scalar_term))
end
function mapvariable(varmap::Function, t::MOI.ScalarQuadraticTerm)
    inds = varmap.((t.variable_index_1, t.variable_index_2))
    return MOI.ScalarQuadraticTerm(t.coefficient, inds...)
end
function mapvariable(varmap::Function, t::MOI.VectorQuadraticTerm)
    MOI.VectorQuadraticTerm(t.output_index, mapvariable(varmap, t.scalar_term))
end
function mapvariables(varmap::Function, f::MOI.SingleVariable)
    return MOI.SingleVariable(varmap(f.variable))
end
function mapvariables(varmap::Function, f::MOI.VectorOfVariables)
    return MOI.VectorOfVariables(varmap.(f.variables))
end
function mapvariables(varmap::Function, f::Union{SAF, VAF})
    typeof(f)(mapvariable.(varmap, f.terms), MOI._constant(f))
end
function mapvariables(varmap::Function, f::Union{SQF, VQF})
    lin = mapvariable.(varmap, f.affine_terms)
    quad = mapvariable.(varmap, f.quadratic_terms)
    return typeof(f)(lin, quad, MOI._constant(f))
end
mapvariables(varmap, f::MOI.AbstractFunction) = mapvariables(vi -> varmap[vi], f)
mapvariables(varmap::Function, change::Union{MOI.ScalarConstantChange, MOI.VectorConstantChange}) = change
function mapvariables(varmap::Function, change::MOI.ScalarCoefficientChange)
    return MOI.ScalarCoefficientChange(varmap(change.variable), change.new_coefficient)
end
function mapvariables(varmap::Function, change::MOI.MultirowChange)
    return MOI.MultirowChange(varmap(change.variable), change.new_coefficients)
end
function mapvariables(varmap, f::MOI.AbstractFunctionModification)
    return mapvariables(vi -> varmap[vi], f)
end

# Vector of constants
constant(f::Union{SAF, SQF}) = [f.constant]
constant(f::Union{VAF, VQF}) = f.constants

# Implements iterator interface
"""
    scalar_type(F::Type{<:MOI.AbstractVectorFunction})

Type of functions obtained by indexing objects obtained by calling `eachscalar`
on functions of type `F`.
"""
function scalar_type end
scalar_type(::Type{MOI.VectorOfVariables}) = MOI.SingleVariable
scalar_type(::Type{MOI.VectorAffineFunction{T}}) where T = MOI.ScalarAffineFunction{T}
scalar_type(::Type{MOI.VectorQuadraticFunction{T}}) where T = MOI.ScalarQuadraticFunction{T}

struct ScalarFunctionIterator{F<:MOI.AbstractVectorFunction}
    f::F
end
eachscalar(f::MOI.AbstractVectorFunction) = ScalarFunctionIterator(f)
@static if VERSION >= v"0.7-"
    function Base.iterate(it::ScalarFunctionIterator, state = 1)
        if state > length(it)
            return nothing
        else
            return (it[state], state + 1)
        end
    end
else
    Base.start(it::ScalarFunctionIterator) = 1
    Base.done(it::ScalarFunctionIterator, state) = state > length(it)
    Base.next(it::ScalarFunctionIterator, state) = (it[state], state+1)
end
function Base.length(it::ScalarFunctionIterator{<:MOI.AbstractVectorFunction})
    return MOI.output_dimension(it.f)
end
Base.eltype(it::ScalarFunctionIterator{VVF}) = SVF
Base.eltype(it::ScalarFunctionIterator{VAF{T}}) where T = SAF{T}
Base.eltype(it::ScalarFunctionIterator{VQF{T}}) where T = SQF{T}
Compat.lastindex(it::ScalarFunctionIterator) = length(it)

# Define getindex for Vector functions

function Base.getindex(it::ScalarFunctionIterator{MOI.VectorOfVariables},
                       i::Integer)
    return MOI.SingleVariable(it.f.variables[i])
end
# Returns the scalar terms of output_index i
function scalar_terms_at_index(terms::Vector{<:Union{MOI.VectorAffineTerm, MOI.VectorQuadraticTerm}}, i::Int)
    I = findall(t -> t.output_index == i, terms)
    map(i -> terms[i].scalar_term, I)
end
function Base.getindex(it::ScalarFunctionIterator{<:VAF}, i::Integer)
    SAF(scalar_terms_at_index(it.f.terms, i), it.f.constants[i])
end
function Base.getindex(it::ScalarFunctionIterator{<:VQF}, i::Integer)
    lin = scalar_terms_at_index(it.f.affine_terms, i)
    quad = scalar_terms_at_index(it.f.quadratic_terms, i)
    return SQF(lin, quad, it.f.constants[i])
end

function Base.getindex(it::ScalarFunctionIterator{MOI.VectorOfVariables},
                       I::AbstractVector)
    return MOI.VectorOfVariables(it.f.variables[I])
end
function Base.getindex(it::ScalarFunctionIterator{VAF{T}}, I::AbstractVector) where T
    terms = MOI.VectorAffineTerm{T}[]
    constant = Vector{T}(undef, length(I))
    for (i, j) in enumerate(I)
        g = it[j]
        append!(terms, map(t -> MOI.VectorAffineTerm(i, t), g.terms))
        constant[i] = g.constant
    end
    VAF(terms, constant)
end

"""
    unsafe_add(t1::MOI.ScalarAffineTerm, t2::MOI.ScalarAffineTerm)

Sums the coefficients of `t1` and `t2` and returns an output `MOI.ScalarAffineTerm`. It is unsafe because it uses the `variable_index` of `t1` as the `variable_index` of the output without checking that it is equal to that of `t2`.
"""
function unsafe_add(t1::MOI.ScalarAffineTerm, t2::MOI.ScalarAffineTerm)
    return MOI.ScalarAffineTerm(t1.coefficient + t2.coefficient, t1.variable_index)
end

"""
    unsafe_add(t1::MOI.ScalarQuadraticTerm, t2::MOI.ScalarQuadraticTerm)

Sums the coefficients of `t1` and `t2` and returns an output
`MOI.ScalarQuadraticTerm`. It is unsafe because it uses the `variable_index`'s
of `t1` as the `variable_index`'s of the output without checking that they are
the same (up to permutation) to those of `t2`.
"""
function unsafe_add(t1::MOI.ScalarQuadraticTerm, t2::MOI.ScalarQuadraticTerm)
    return MOI.ScalarQuadraticTerm(t1.coefficient + t2.coefficient,
                                   t1.variable_index_1,
                                   t1.variable_index_2)
end

"""
    unsafe_add(t1::MOI.VectorAffineTerm, t2::MOI.VectorAffineTerm)

Sums the coefficients of `t1` and `t2` and returns an output `MOI.VectorAffineTerm`. It is unsafe because it uses the `output_index` and `variable_index` of `t1` as the `output_index` and `variable_index` of the output term without checking that they are equal to those of `t2`.
"""
function unsafe_add(t1::VT, t2::VT) where VT <: Union{MOI.VectorAffineTerm,
                                                      MOI.VectorQuadraticTerm}
    scalar_term = unsafe_add(t1.scalar_term, t2.scalar_term)
    return MOI.VectorAffineTerm(t1.output_index, scalar_term)
end

"""
    iscanonical(f::Union{ScalarAffineFunction, ScalarQuadraticFunction
                         VectorAffineFunction, VectorQuadraticTerm})

Returns a Bool indicating whether the function is in canonical form.
See [`canonical`](@ref).
"""
function iscanonical(f::Union{SAF, VAF, SQF, VQF})
    is_strictly_sorted(f.terms, MOI.term_indices,
                       t -> !iszero(MOI.coefficient(t)))
end

"""
    is_strictly_sorted(x::AbstractVector, by, filter)

Returns `true` if `by(x[i]) < by(x[i + 1])` and `filter(x[i]) == true` for
all indices i.
"""
function is_strictly_sorted(x::AbstractVector, by, filter)
    if isempty(x)
        return true
    end
    if !filter(first(x))
        return false
    end
    for i in eachindex(x)[2:end]
        if by(x[i]) <= by(x[i - 1])
            return false
        end
        if !filter(x[i])
            return false
        end
    end
    return true
end

"""
    canonical(f::Union{ScalarAffineFunction, VectorAffineFunction})

Returns the function in a canonical form, i.e.
* A term appear only once.
* The coefficients are nonzero.
* The terms appear in increasing order of variable where there the order of the variables is the order of their value.
* For a `AbstractVectorFunction`, the terms are sorted in ascending order of output index.

### Examples
If `x` (resp. `y`, `z`) is `VariableIndex(1)` (resp. 2, 3).
The canonical representation of `ScalarAffineFunction([y, x, z, x, z], [2, 1, 3, -2, -3], 5)` is `ScalarAffineFunction([x, y], [-1, 2], 5)`.

"""
canonical(f::Union{SAF, VAF, SQF, VQF}) = canonicalize!(copy(f))

"""
    canonicalize!(f::Union{ScalarAffineFunction, VectorAffineFunction})

Convert a function to canonical form in-place, without allocating a copy to hold
the result. See [`canonical`](@ref).
"""
function canonicalize!(f::Union{SAF, VAF})
    sort_and_compress!(f.terms, MOI.term_indices,
                       t -> !iszero(MOI.coefficient(t)), unsafe_add)
    return f
end

"""
    canonicalize!(f::Union{ScalarQuadraticFunction, VectorQuadraticFunction})

Convert a function to canonical form in-place, without allocating a copy to hold
the result. See [`canonical`](@ref).
"""
function canonicalize!(f::Union{SQF, VQF})
    sort_and_compress!(f.affine_terms, MOI.term_indices,
                       t -> !iszero(MOI.coefficient(t)), unsafe_add)
    sort_and_compress!(f.quadratic_terms, MOI.term_indices,
                       t -> !iszero(MOI.coefficient(t)), unsafe_add)
    return f
end

"""
    sort_and_compress!(x::AbstractVector, by::Function, keep::Function, combine::Function)

Sort the vector `x` in-place using `by` as the function from elements to comparable keys, then
combine all entries for which `by(x[i]) == by(x[j])` using the function `x[i] = combine(x[i], x[j])`,
and remove any entries for which `keep(x[i]) == false`. This may result in `x` being resized to
a shorter length.
"""
function sort_and_compress!(x::AbstractVector, by, keep, combine)
    if length(x) > 0
        sort!(x, QuickSort, Base.Order.ord(isless, by, false, Base.Sort.Forward))
        i1 = firstindex(x)
        for i2 in eachindex(x)[2:end]
            if by(x[i1]) == by(x[i2])
                x[i1] = combine(x[i1], x[i2])
            else
                if !keep(x[i1])
                    x[i1] = x[i2]
                else
                    x[i1 + 1] = x[i2]
                    i1 += 1
                end
            end
        end
        if !keep(x[i1])
            i1 -= 1
        end
        resize!(x, i1)
    end
    return x
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

isapprox_zero(α::AbstractFloat, tol) = -tol < α < tol
isapprox_zero(α::Union{Integer, Rational}, tol) = iszero(α)
function isapprox_zero(t::Union{MOI.ScalarAffineTerm,
                                MOI.ScalarQuadraticTerm}, tol)
    isapprox_zero(MOI.coefficient(t), tol)
end

"""
    isapprox_zero(f::MOI.AbstractFunction, tol)

Return a `Bool` indicating whether the function `f` is approximately zero using
`tol` as a tolerance.

## Important note

This function assumes that `f` does not contain any duplicate terms, you might
want to first call [`canonical`](@ref) if that is not guaranteed.
For instance, given
```julia
f = MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.([1, -1], [x, x]), 0)`.
```
then `isapprox_zero(f)` is `false` but `isapprox_zero(MOIU.canonical(f))` is
`true`.
"""
function isapprox_zero end

function isapprox_zero(f::MOI.ScalarAffineFunction, tol)
    isapprox_zero(f.constant, tol) && all(t -> isapprox_zero(t, tol),
                                           f.terms)
end
function isapprox_zero(f::MOI.ScalarQuadraticFunction, tol)
    isapprox_zero(f.constant, tol) &&
        all(t -> isapprox_zero(t, tol),
            f.affine_terms) &&
        all(t -> isapprox_zero(t, tol),
            f.quadratic_terms)
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

    variablemap_2to1 = Dict{VI,VI}()
    for vname in variablenames
        index1 = MOI.get(model1, VI, vname)
        index2 = MOI.get(model2, VI, vname)
        variablemap_2to1[index2] = index1
    end

    for cname in constraintnames
        index1 = MOI.get(model1, CI, cname)
        index2 = MOI.get(model2, CI, cname)
        f1 = MOI.get(model1, MOI.ConstraintFunction(), index1)
        f2 = MOI.get(model2, MOI.ConstraintFunction(), index2)
        s1 = MOI.get(model1, MOI.ConstraintSet(), index1)
        s2 = MOI.get(model2, MOI.ConstraintSet(), index2)
        @test isapprox(f1, mapvariables(variablemap_2to1, f2))
        @test s1 == s2
    end
    attrs1 = MOI.get(model1, MOI.ListOfModelAttributesSet())
    attrs2 = MOI.get(model2, MOI.ListOfModelAttributesSet())
    attr_list = attrs1 ∪ attrs2
    for attr in attr_list
        value1 = MOI.get(model1, attr)
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


_hasvar(v::VI, vi::VI) = v == vi
_hasvar(t::MOI.ScalarAffineTerm, vi::VI) = t.variable_index == vi
_hasvar(t::MOI.ScalarQuadraticTerm, vi::VI) = t.variable_index_1 == vi || t.variable_index_2 == vi
_hasvar(t::Union{MOI.VectorAffineTerm, MOI.VectorQuadraticTerm}, vi::VI) = _hasvar(t.scalar_term, vi)
# Removes terms or variables in `vis_or_terms` that contains the variable of index `vi`
function _rmvar(vis_or_terms::Vector, vi::VI)
    return vis_or_terms[.!_hasvar.(vis_or_terms, Ref(vi))]
end

"""
    removevariable(f::AbstractFunction, vi::VariableIndex)

Return a new function `f` with the variable vi removed.
"""
function removevariable(f::VVF, vi)
    VVF(_rmvar(f.variables, vi))
end
function removevariable(f::Union{SAF, VAF}, vi)
    typeof(f)(_rmvar(f.terms, vi), MOI._constant(f))
end
function removevariable(f::Union{SQF, VQF}, vi)
    terms = _rmvar.((f.affine_terms, f.quadratic_terms), Ref(vi))
    typeof(f)(terms..., MOI._constant(f))
end

"""
    modifyfunction(f::AbstractFunction, change::AbstractFunctionModification)

Return a new function `f` modified according to `change`.
"""
function modifyfunction(f::SAF, change::MOI.ScalarConstantChange)
    return SAF(f.terms, change.new_constant)
end
function modifyfunction(f::VAF, change::MOI.VectorConstantChange)
    return VAF(f.terms, change.new_constant)
end
function modifyfunction(f::SQF, change::MOI.ScalarConstantChange)
    return SQF(f.affine_terms, f.quadratic_terms, change.new_constant)
end
function modifyfunction(f::VQF, change::MOI.VectorConstantChange)
    return VQF(f.affine_terms, f.quadratic_terms, change.new_constant)
end
function _modifycoefficient(terms::Vector{<:MOI.ScalarAffineTerm}, variable::VI, new_coefficient)
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
function modifyfunction(f::SAF, change::MOI.ScalarCoefficientChange)
    lin = _modifycoefficient(f.terms, change.variable, change.new_coefficient)
    return SAF(lin, f.constant)
end
function modifyfunction(f::SQF, change::MOI.ScalarCoefficientChange)
    lin = _modifycoefficient(f.affine_terms, change.variable, change.new_coefficient)
    return SQF(lin, f.quadratic_terms, f.constant)
end
function _modifycoefficients(n, terms::Vector{<:MOI.VectorAffineTerm}, variable::VI, new_coefficients)
    terms = copy(terms)
    # Maps between rows in the `MOI.VectorAffineTerm`s and indices in new_coefficients
    rowmap = Dict(c[1]=>i for (i,c) in enumerate(new_coefficients))
    del = Int[]
    for i in 1:length(terms)
        if _hasvar(terms[i], variable)
            row = terms[i].output_index
            j = Base.get(rowmap, row, 0)
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
    for (row, j) in rowmap
        if !iszero(j)
            push!(terms, MOI.VectorAffineTerm(row, MOI.ScalarAffineTerm(new_coefficients[j][2], variable)))
        end
    end
    terms
end
function modifyfunction(f::VAF, change::MOI.MultirowChange)
    dim = MOI.output_dimension(f)
    coefficients = change.new_coefficients
    lin = _modifycoefficients(dim, f.terms, change.variable, coefficients)
    VAF(lin, f.constants)
end
function modifyfunction(f::VQF, change::MOI.MultirowChange)
    dim = MOI.output_dimension(f)
    coefficients = change.new_coefficients
    lin = _modifycoefficients(dim, f.affine_terms, change.variable, coefficients)
    return VQF(lin, f.quadratic_terms, f.constants)
end

# Arithmetic

function Base.zero(::Type{MOI.ScalarAffineFunction{T}}) where T
    return MOI.ScalarAffineFunction(MOI.ScalarAffineTerm{T}[], zero(T))
end
function Base.zero(::Type{MOI.ScalarQuadraticFunction{T}}) where T
    return MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{T}[],
                                       MOI.ScalarQuadraticTerm{T}[], zero(T))
end

"""
    operate(op::Function, ::Type{T},
            args::Union{T, MOI.AbstractFunction}...)::MOI.AbstractFunction where T

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)`. No argument can be modified.
"""
function operate end

"""
    operate!(op::Function, ::Type{T},
             args::Union{T, MOI.AbstractFunction}...)::MOI.AbstractFunction where T

Returns an `MOI.AbstractFunction` representing the function resulting from the
operation `op(args...)`. The first argument can be modified. The return type
is the same than the method `operate(op, T, args...)` without `!`.
"""
function operate! end

"""
    promote_operation(op::Function, ::Type{T},
                      ArgsTypes::Type{<:Union{T, MOI.AbstractFunction}}...) where T

Returns the type of the `MOI.AbstractFunction` returned to the call
`operate(op, T, args...)` where the types of the arguments `args` are
`ArgsTypes`.
"""
function promote_operation end

# Helpers

function operate_term(::typeof(-), term::MOI.ScalarAffineTerm)
    return MOI.ScalarAffineTerm(-term.coefficient, term.variable_index)
end
function operate_term(::typeof(-), term::MOI.ScalarQuadraticTerm)
    return MOI.ScalarQuadraticTerm(-term.coefficient,
                                   term.variable_index_1,
                                   term.variable_index_2)
end
function operate_term(::typeof(-), term::MOI.VectorAffineTerm)
    return MOI.VectorAffineTerm(term.output_index, operate_term(-, term.scalar_term))
end
function operate_term(::typeof(-), term::MOI.VectorQuadraticTerm)
    return MOI.VectorQuadraticTerm(term.output_index, operate_term(-, term.scalar_term))
end

function operate_term(::typeof(*), α::T, t::MOI.ScalarAffineTerm{T}) where T
    MOI.ScalarAffineTerm(α * t.coefficient, t.variable_index)
end
function operate_term(::typeof(*), α::T, t::MOI.ScalarQuadraticTerm{T}) where T
    MOI.ScalarQuadraticTerm(α * t.coefficient, t.variable_index_1,
                            t.variable_index_2)
end
function operate_term(::typeof(*), t1::MOI.ScalarAffineTerm,
                      t2::MOI.ScalarAffineTerm)
    MOI.ScalarQuadraticTerm(t1.coefficient * t2.coefficient, t1.variable_index,
                            t2.variable_index)
end

function operate_term(::typeof(*), α::T, t::MOI.VectorAffineTerm{T}) where T
    MOI.VectorAffineTerm(t.output_index, operate_term(*, α, t.scalar_term))
end
function operate_term(::typeof(*), α::T, t::MOI.VectorQuadraticTerm{T}) where T
    MOI.VectorQuadraticTerm(t.output_index, operate_term(*, α, t.scalar_term))
end
function operate_term(::typeof(*), t1::MOI.VectorAffineTerm,
                      t2::MOI.VectorAffineTerm)
    @assert t1.output_index == t2.output_index
    MOI.VectorQuadraticTerm(t1.output_index, operate_term(*, t1.scalar_term, t2.scalar_term))
end

function operate_term(::typeof(/), t::MOI.ScalarAffineTerm{T}, α::T) where T
    MOI.ScalarAffineTerm(t.coefficient / α, t.variable_index)
end
function operate_term(::typeof(/), t::MOI.ScalarQuadraticTerm{T}, α::T) where T
    MOI.ScalarQuadraticTerm(t.coefficient / α, t.variable_index_1,
                            t.variable_index_2)
end
function operate_term(::typeof(/), t::MOI.VectorAffineTerm{T}, α::T) where T
    MOI.VectorAffineTerm(t.output_index, operate_term(/, t.scalar_term, α))
end
function operate_term(::typeof(/), t::MOI.VectorQuadraticTerm{T}, α::T) where T
    MOI.VectorQuadraticTerm(t.output_index, operate_term(/, t.scalar_term, α))
end

# Avoid a copy in the case of +
function operate_terms(::typeof(+),
                       terms::Vector{<:Union{MOI.ScalarAffineTerm,
                                             MOI.ScalarQuadraticTerm}})
    return terms
end
function operate_terms(::typeof(+),
        terms::Vector{<:Union{MOI.VectorAffineTerm,
                              MOI.VectorQuadraticTerm}})
    return terms
end
function operate_terms(::typeof(-),
                       terms::Vector{<:Union{MOI.ScalarAffineTerm,
                                             MOI.ScalarQuadraticTerm}})
    return map(term -> operate_term(-, term), terms)
end
function operate_terms(::typeof(-),
        terms::Vector{<:Union{MOI.VectorAffineTerm,
                            MOI.VectorQuadraticTerm}})
    return map(term -> operate_term(-, term), terms)
end

function map_terms!(op, func::Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction})
    map!(op, func.terms, func.terms)
end
function map_terms!(op, func::Union{MOI.ScalarQuadraticFunction, MOI.VectorQuadraticFunction})
    map!(op, func.affine_terms, func.affine_terms)
    map!(op, func.quadratic_terms, func.quadratic_terms)
end

# Functions convertible to a ScalarAffineFunction
const ScalarAffineLike{T} = Union{T, MOI.SingleVariable, MOI.ScalarAffineFunction{T}}
# Functions convertible to a ScalarQuadraticFunction
const ScalarQuadraticLike{T} = Union{ScalarAffineLike{T}, MOI.ScalarQuadraticFunction{T}}

# Used for overloading Base operator functions so `T` is not in the union to
# avoid overloading e.g. `+(::Float64, ::Float64)`
const ScalarLike{T} = Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T},
                            MOI.ScalarQuadraticFunction{T}}

# Functions convertible to a VectorAffineFunction
const VectorAffineLike{T} = Union{Vector{T}, MOI.VectorOfVariables, MOI.VectorAffineFunction{T}}
# Functions convertible to a VectorQuadraticFunction
const VectorQuadraticLike{T} = Union{VectorAffineLike{T}, MOI.VectorQuadraticFunction{T}}

# Used for overloading Base operator functions so `T` is not in the union to
# avoid overloading e.g. `+(::Float64, ::Float64)`
const VectorLike{T} = Union{MOI.VectorOfVariables, MOI.VectorAffineFunction{T},
                            MOI.VectorQuadraticFunction{T}}

###################################### +/- #####################################
## promote_operation

function promote_operation(::Union{typeof(+), typeof(-)}, ::Type{T},
                           ::Type{<:ScalarAffineLike{T}},
                           ::Type{<:ScalarAffineLike{T}}) where T
    return MOI.ScalarAffineFunction{T}
end
function promote_operation(::Union{typeof(+), typeof(-)}, ::Type{T},
                           ::Type{<:ScalarQuadraticLike{T}},
                           ::Type{<:ScalarQuadraticLike{T}}) where T
    return MOI.ScalarQuadraticFunction{T}
end

## operate!
# + with at least 3 arguments
function operate!(op::typeof(+), ::Type{T}, f, g, h, args...) where T
    operate!(op, T, f, g)
    return operate!(+, T, f, h, args...)
end

# Scalar Variable +/- ...
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.SingleVariable,
                  g::ScalarQuadraticLike) where T
    return operate(op, T, f, g)
end
# Scalar Affine +/-! ...
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarAffineFunction{T},
                  g::T) where T
    f.constant = op(f.constant, g)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarAffineFunction{T},
                  g::MOI.SingleVariable) where T
    push!(f.terms, MOI.ScalarAffineTerm(op(one(T)), g.variable))
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarAffineFunction{T},
                  g::MOI.ScalarAffineFunction{T}) where T
    append!(f.terms, operate_terms(op, g.terms))
    f.constant = op(f.constant, g.constant)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarAffineFunction{T},
                  g::MOI.ScalarQuadraticFunction{T}) where T
    return operate(op, T, f, g)
end
# Scalar Quadratic +/-! ...
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarQuadraticFunction{T},
                  g::T) where T
    f.constant = op(f.constant, g)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarQuadraticFunction{T},
                  g::MOI.SingleVariable) where T
    push!(f.affine_terms, MOI.ScalarAffineTerm(op(one(T)), g.variable))
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarQuadraticFunction{T},
                  g::MOI.ScalarAffineFunction{T}) where T
    append!(f.affine_terms, operate_terms(op, g.terms))
    f.constant = op(f.constant, g.constant)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.ScalarQuadraticFunction{T},
                  g::MOI.ScalarQuadraticFunction{T}) where T
    append!(f.affine_terms, operate_terms(op, g.affine_terms))
    append!(f.quadratic_terms, operate_terms(op, g.quadratic_terms))
    f.constant = op(f.constant, g.constant)
    return f
end

## operate
# + with at least 3 arguments, can use in-place as the user cannot use
# intermediate results
function operate(op::typeof(+), ::Type{T}, f, g, h, args...) where T
    return operate!(+, T, operate(+, T, f, g), h, args...)
end

# Scalar number +/- ...
function operate(op::typeof(+), ::Type{T}, α::T, f::ScalarLike{T}) where T
    return operate(op, T, f, α)
end
function operate(op::typeof(-), ::Type{T}, α::T, f::ScalarLike{T}) where T
    return operate!(+, T, operate(-, T, f), α)
end

# Scalar Variable +/- ...
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
                 f::MOI.SingleVariable, α::T) where T
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T),
                                                             f.variable)],
                                       op(α))
end
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
                 f::MOI.SingleVariable,
                 g::MOI.SingleVariable) where T
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(one(T), f.variable),
                                        MOI.ScalarAffineTerm(op(one(T)), g.variable)],
                                       zero(T))
end
function operate(op::typeof(+), ::Type{T},
                 f::MOI.SingleVariable,
                 g::Union{MOI.ScalarAffineFunction{T},
                          MOI.ScalarQuadraticFunction{T}}) where T
    return operate(op, T, g, f)
end
function operate(op::typeof(-), ::Type{T},
                 f::MOI.SingleVariable,
                 g::Union{MOI.ScalarAffineFunction{T},
                          MOI.ScalarQuadraticFunction{T}}) where T
    return operate!(+, T, operate(-, T, g), f)
end
# Scalar Affine +/- ...
function operate(op::Union{typeof(-)}, ::Type{T},
                 f::MOI.ScalarAffineFunction{T}) where T
    return MOI.ScalarAffineFunction(operate_terms(op, f.terms),
                                    op(f.constant))
end
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
                 f::MOI.ScalarAffineFunction{T},
                 g::ScalarAffineLike{T}) where T
    return operate!(op, T, copy(f), g)
end
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
                 f::MOI.ScalarAffineFunction{T},
                 g::MOI.ScalarQuadraticFunction{T}) where T
    MOI.ScalarQuadraticFunction([f.terms; operate_terms(op, g.affine_terms)],
                                operate_terms(op, g.quadratic_terms),
                                op(f.constant, g.constant))
end
# Scalar Quadratic +/- ...
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
                 f::MOI.ScalarQuadraticFunction{T},
                 g::ScalarQuadraticLike{T}) where T
    operate!(op, T, copy(f), g)
end

function Base.:+(args::ScalarLike{T}...) where T
    return operate(+, T, args...)
end
function Base.:+(α::T, f::ScalarLike{T}...) where T
    return operate(+, T, α, f...)
end
function Base.:+(f::ScalarLike{T}, α::T) where T
    return operate(+, T, f, α)
end
function Base.:-(args::ScalarLike{T}...) where T
    return operate(-, T, args...)
end
function Base.:-(f::ScalarLike{T}, α::T) where T
    return operate(-, T, f, α)
end
function Base.:-(α::T, f::ScalarLike{T}) where T
    return operate(-, T, α, f)
end

# Vector +/-
###############################################################################
# function promote_operation(::Union{typeof(+), typeof(-)}, ::Type{T},
#     ::Type{<:VectorAffineLike{T}}) where T
#     return MOI.VectorAffineFunction{T}
# end
function promote_operation(::Union{typeof(+), typeof(-)}, ::Type{T},
                           ::Type{<:VectorAffineLike{T}},
                           ::Type{<:VectorAffineLike{T}}) where T
    return MOI.VectorAffineFunction{T}
end
function promote_operation(::Union{typeof(+), typeof(-)}, ::Type{T},
                           ::Type{<:VectorQuadraticLike{T}},
                           ::Type{<:VectorQuadraticLike{T}}) where T
    return MOI.VectorQuadraticFunction{T}
end

# Vector Variable +/- ...
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorOfVariables,
                  g::VectorQuadraticLike) where T
    return operate(op, T, f, g)
end
# Vector Affine +/-! ...
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorAffineFunction{T},
                  g::Vector{T}) where T
    @assert MOI.output_dimension(f) == length(g)
    f.constants .= op.(f.constants, g)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorAffineFunction{T},
                  g::MOI.VectorOfVariables) where T
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(f.terms, MOI.VectorAffineTerm.(
                         collect(1:d), 
                         MOI.ScalarAffineTerm.(op(one(T)), g.variables)))
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorAffineFunction{T},
                  g::MOI.VectorAffineFunction{T}) where T
    append!(f.terms, operate_terms(op, g.terms))
    f.constants .= op.(f.constants, g.constants)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorAffineFunction{T},
                  g::MOI.VectorQuadraticFunction{T}) where T
    return operate(op, T, f, g)
end
# Vector Quadratic +/-! ...
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorQuadraticFunction{T},
                  g::Vector{T}) where T
    @assert MOI.output_dimension(f) == length(g)
    f.constants .= op.(f.constants, g)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorQuadraticFunction{T},
                  g::MOI.VectorOfVariables) where T
    d = MOI.output_dimension(g)
    @assert MOI.output_dimension(f) == d
    append!(f.affine_terms, MOI.VectorAffineTerm.(collect(1:d), MOI.ScalarAffineTerm.(op(one(T)), g.variables)))
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorQuadraticFunction{T},
                  g::MOI.VectorAffineFunction{T}) where T
    append!(f.affine_terms, operate_terms(op, g.terms))
    f.constants .= op.(f.constants, g.constants)
    return f
end
function operate!(op::Union{typeof(+), typeof(-)}, ::Type{T},
                  f::MOI.VectorQuadraticFunction{T},
                  g::MOI.VectorQuadraticFunction{T}) where T
    append!(f.affine_terms, operate_terms(op, g.affine_terms))
    append!(f.quadratic_terms, operate_terms(op, g.quadratic_terms))
    f.constants .= op.(f.constants, g.constants)
    return f
end

## operate
# + with at least 3 arguments, can use in-place as the user cannot use
# intermediate results
# overload
# function operate(op::typeof(+), ::Type{T}, f, g, h, args...) where T
#     return operate!(+, T, operate(+, T, f, g), h, args...)
# end

# function operate(op::typeof(+), ::Type{T}, f::VectorOfVariables) where T
#     return f
# end

function operate(op::typeof(-), ::Type{T}, f::MOI.VectorOfVariables) where T
    d = MOI.output_dimension(f)
    return MOI.VectorAffineFunction{T}(
               MOI.VectorAffineTerm.(
                   collect(1:d),
                   MOI.ScalarAffineTerm.(-one(T), f.variables)),
               fill(zero(T),d))
end

# Vector number +/- ...
function operate(op::typeof(+), ::Type{T}, α::Vector{T}, f::VectorLike{T}) where T
    return operate(op, T, f, α)
end
function operate(op::typeof(-), ::Type{T}, α::Vector{T}, f::VectorLike{T}) where T
    return operate!(+, T, operate(-, T, f), α)
end

# Vector Variable +/- ...
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
    f::MOI.VectorOfVariables, α::Vector{T}) where T
    d = MOI.output_dimension(f)
    @assert length(α) == d
    return MOI.VectorAffineFunction{T}(
               MOI.VectorAffineTerm.(
                   collect(1:d),
                   MOI.ScalarAffineTerm.(one(T), f.variables)),
               op.(α))
end
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
                 f::MOI.VectorOfVariables,
                 g::MOI.VectorOfVariables) where T
    d = MOI.output_dimension(f)
    @assert MOI.output_dimension(g) == d
    return MOI.VectorAffineFunction{T}(
               vcat(
                   MOI.VectorAffineTerm.(
                       collect(1:d),
                       MOI.ScalarAffineTerm.(one(T), f.variables)),
                   MOI.VectorAffineTerm.(
                       collect(1:d),
                       MOI.ScalarAffineTerm.(op(one(T)), g.variables))),
               fill(zero(T),d))
end
function operate(op::typeof(+), ::Type{T},
    f::MOI.VectorOfVariables,
    g::Union{MOI.VectorAffineFunction{T},
                MOI.VectorQuadraticFunction{T}}) where T
    return operate(op, T, g, f)
end
function operate(op::typeof(-), ::Type{T},
    f::MOI.VectorOfVariables,
    g::Union{MOI.VectorAffineFunction{T},
                MOI.VectorQuadraticFunction{T}}) where T
    return operate!(+, T, operate(-, T, g), f)
end
# Vector Affine +/- ...
function operate(op::Union{typeof(-)}, ::Type{T},
    f::MOI.VectorAffineFunction{T}) where T
    return MOI.VectorAffineFunction(operate_terms(op, f.terms),
                      op.(f.constants))
end
function operate(op::Union{typeof(-)}, ::Type{T},
    f::MOI.VectorQuadraticFunction{T}) where T
    return MOI.VectorQuadraticFunction(
        operate_terms(op, f.affine_terms),
        operate_terms(op, f.quadratic_terms),
                      op.(f.constants))
end
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::VectorAffineLike{T}) where T
    return operate!(op, T, copy(f), g)
end
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
    f::MOI.VectorAffineFunction{T},
    g::MOI.VectorQuadraticFunction{T}) where T
    MOI.VectorQuadraticFunction([f.terms; operate_terms(op, g.affine_terms)],
                  operate_terms(op, g.quadratic_terms),
                  op.(f.constants, g.constants))
end

# Vector Quadratic +/- ...
function operate(op::Union{typeof(+), typeof(-)}, ::Type{T},
    f::MOI.VectorQuadraticFunction{T},
    g::VectorQuadraticLike{T}) where T
    operate!(op, T, copy(f), g)
end

function Base.:+(args::VectorLike{T}...) where T
    return operate(+, T, args...)
end
function Base.:+(α::Vector{T}, f::VectorLike{T}...) where T
    return operate(+, T, α, f...)
end
function Base.:+(f::VectorLike{T}, α::Vector{T}) where T
    return operate(+, T, f, α)
end
function Base.:-(args::VectorLike{T}...) where T
    return operate(-, T, args...)
end
function Base.:-(f::VectorLike{T}, α::Vector{T}) where T
    return operate(-, T, f, α)
end
function Base.:-(α::Vector{T}, f::VectorLike{T}) where T
    return operate(-, T, α, f)
end

####################################### * ######################################
function promote_operation(::typeof(*), ::Type{T}, ::Type{T},
                           ::Type{<:Union{MOI.SingleVariable,
                                          MOI.ScalarAffineFunction{T}}}) where T
    return MOI.ScalarAffineFunction{T}
end
function promote_operation(::typeof(*), ::Type{T},
                           ::Type{<:Union{MOI.SingleVariable,
                                          MOI.ScalarAffineFunction{T}}},
                           ::Type{<:Union{MOI.SingleVariable,
                                          MOI.ScalarAffineFunction{T}}}) where T
    return MOI.ScalarQuadraticFunction{T}
end


function operate!(::typeof(*), ::Type{T}, f::MOI.SingleVariable, α::T) where T
    return operate(*, T, α, f)
end
function operate(::typeof(*), ::Type{T}, α::T, f::MOI.SingleVariable) where T
    MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(α, f.variable)], zero(T))
end

function operate!(::typeof(*), ::Type{T},
                  f::Union{MOI.ScalarAffineFunction{T},
                           MOI.ScalarQuadraticFunction{T}}, α::T) where T
    map_terms!(term -> operate_term(*, α, term), f)
    f.constant *= α
    return f
end
function operate(::typeof(*), ::Type{T}, α::T, f::MOI.ScalarAffineFunction) where T
    return operate!(*, T, copy(f), α)
end

function operate(::typeof(*), ::Type{T}, α::T,
                 f::MOI.ScalarQuadraticFunction) where T
    return operate!(*, T, copy(f), α)
end

function operate(::typeof(*), ::Type{T}, f::MOI.SingleVariable,
                 g::MOI.SingleVariable) where T
    return MOI.ScalarQuadraticFunction(MOI.ScalarAffineTerm{T}[],
                                       [MOI.ScalarQuadraticTerm(one(T),
                                                                f.variable,
                                                                g.variable)],
                                       zero(T))
end

function operate(::typeof(*), ::Type{T}, f::MOI.ScalarAffineFunction{T},
                 g::MOI.SingleVariable) where T
    aff_terms = [MOI.ScalarAffineTerm(f.constant, g.variable)]
    quad_terms = map(t -> MOI.ScalarQuadraticTerm(t.coefficient,
                                                  t.variable_index,
                                                  g.variable),
                     f.terms)
    return MOI.ScalarQuadraticFunction(aff_terms, quad_terms, zero(T))
end

function operate(::typeof(*), ::Type{T}, f::MOI.ScalarAffineFunction{T},
                 g::MOI.ScalarAffineFunction{T}) where T
    nfterms = length(f.terms)
    ngterms = length(g.terms)
    quad_terms = Vector{MOI.ScalarQuadraticTerm{T}}(undef, nfterms * ngterms)
    k = 0
    for t1 in f.terms
        for t2 in g.terms
            k += 1
            quad_terms[k] = operate_term(*, t1, t2)
        end
    end
    @assert k == length(quad_terms)
    if iszero(f.constant)
        if iszero(g.constant)
            aff_terms = MOI.ScalarAffineTerm{T}[]
        else
            aff_terms = operate_term.(*, g.constant, f.terms)
        end
    else
        if iszero(g.constant)
            aff_terms = operate_term.(*, f.constant, g.terms)
        else
            aff_terms = Vector{MOI.ScalarAffineTerm{T}}(undef,
                                                        nfterms + ngterms)
            map!(t -> operate_term(*, g.constant, t), aff_terms, f.terms)
            for i in 1:ngterms
                aff_terms[nfterms + i] = operate_term(*, f.constant, g.terms[i])
            end
        end
    end
    constant = f.constant * g.constant
    return MOI.ScalarQuadraticFunction(aff_terms, quad_terms, constant)
end

function Base.:*(args::ScalarLike{T}...) where T
    return operate(*, T, args...)
end
function Base.:*(f::T, g::ScalarLike{T}) where T
    return operate(*, T, f, g)
end


####################################### / ######################################
function promote_operation(::typeof(/), ::Type{T},
                           ::Type{<:Union{MOI.SingleVariable,
                                          MOI.ScalarAffineFunction{T}}},
                           ::Type{T}) where T
    MOI.ScalarAffineFunction{T}
end
function promote_operation(::typeof(/), ::Type{T},
                           ::Type{MOI.ScalarQuadraticFunction{T}},
                           ::Type{T}) where T
    MOI.ScalarQuadraticFunction{T}
end

function operate!(::typeof(/), ::Type{T}, f::MOI.SingleVariable,
                  α::T) where T
    return operate(/, T, f, α)
end
function operate(::typeof(/), ::Type{T}, f::MOI.SingleVariable,
                 α::T) where T
    return MOI.ScalarAffineFunction{T}([MOI.ScalarAffineTerm(inv(α),
                                                             f.variable)],
                                       zero(T))
end

function operate!(::typeof(/), ::Type{T}, f::MOI.ScalarAffineFunction{T},
                  α::T) where T
    f.terms .= operate_term.(/, f.terms, α)
    f.constant /= α
    return f
end
function operate(::typeof(/), ::Type{T}, f::MOI.ScalarAffineFunction{T},
                 α::T) where T
    return operate(/, T, copy(f), α)
end

function Base.:/(args::ScalarLike{T}...) where T
    return operate(/, T, args...)
end
function Base.:/(f::ScalarLike{T}, g::T) where T
    return operate(/, T, f, g)
end

## sum
function operate(::typeof(sum), ::Type{T}, vis::Vector{MOI.VariableIndex}) where T
    return MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(one(T), vis), zero(T))
end

#################### Concatenation of MOI functions: `vcat` ####################
"""
    fill_vector(vector::Vector, ::Type{T}, fill_func::Function,
                dim_func::Function, funcs) where T

Fill the vector `vector` with
`fill_func(vector, vector_offset, output_offset, func)` for each function `func`
in `funcs` where `vector_offset` (resp. `output_offset`) is the sum of
`dim_func(T, func)` (resp. `output_dim(T, func)`) of previous functions of
`func`.

    fill_vector(vector::Vector, ::Type{T}, vector_offset::Int,
                     output_offset::Int, fill_func::Function,
                     dim_func::Function, funcs...) where T

Same than previous method but starting with possible nonzero `vector_offset` and
`output_offset`.
"""
function fill_vector end

function fill_vector(vector::Vector, ::Type{T}, fill_func::Function,
                     dim_func::Function, funcs) where T
    vector_offset = 0
    output_offset = 0
    for func in funcs
        fill_func(vector, vector_offset, output_offset, func)
        vector_offset += dim_func(T, func)
        output_offset += output_dim(T, func)
    end
    @assert length(vector) == vector_offset
end
function fill_vector(vector::Vector, ::Type, vector_offset::Int,
                     output_offset::Int, fill_func::Function,
                     dim_func::Function)
    @assert length(vector) == vector_offset
end
function fill_vector(vector::Vector, ::Type{T}, vector_offset::Int,
                     output_offset::Int, fill_func::Function,
                     dim_func::Function, func, funcs...) where T
    fill_func(vector, vector_offset, output_offset, func)
    fill_vector(vector, T, vector_offset + dim_func(T, func),
                output_offset + output_dim(T, func), fill_func, dim_func,
                funcs...)
end

number_of_affine_terms(::Type{T}, ::T) where T = 0
number_of_affine_terms(::Type, ::SVF) = 1
number_of_affine_terms(::Type, f::VVF) = length(f.variables)
number_of_affine_terms(::Type{T},
                       f::Union{SAF{T}, VAF{T}}) where T  = length(f.terms)

function offset_term(t::MOI.ScalarAffineTerm, offset::Int)
    return MOI.VectorAffineTerm(offset + 1, t)
end
function offset_term(t::MOI.VectorAffineTerm, offset::Int)
    return MOI.VectorAffineTerm(offset + t.output_index, t.scalar_term)
end

function fill_terms(terms::Vector{MOI.VectorAffineTerm{T}}, offset::Int,
                    output_offset::Int, func::T) where T
end
function fill_terms(terms::Vector{MOI.VectorAffineTerm{T}}, offset::Int,
                    output_offset::Int, func::SVF) where T
    terms[offset + 1] = offset_term(MOI.ScalarAffineTerm(one(T), func.variable),
                                    output_offset)
end
function fill_terms(terms::Vector{MOI.VectorAffineTerm{T}}, offset::Int,
                    output_offset::Int, func::VVF) where T
    n = number_of_affine_terms(T, func)
    terms[offset .+ (1:n)] .= MOI.VectorAffineTerm.(output_offset .+ (1:n),
                                                    MOI.ScalarAffineTerm.(one(T),
                                                                          func.variables))
end
function fill_terms(terms::Vector{MOI.VectorAffineTerm{T}}, offset::Int,
                    output_offset::Int, func::Union{SAF{T}, VAF{T}}) where T
    n = number_of_affine_terms(T, func)
    terms[offset .+ (1:n)] .= offset_term.(func.terms, output_offset)
end

output_dim(::Type{T}, ::T) where T = 1
output_dim(::Type, func::MOI.AbstractFunction) = MOI.output_dimension(func)
function fill_constant(constant::Vector{T}, offset::Int,
                       output_offset::Int, func::T) where T
    constant[offset + 1] = func
end
function fill_constant(constant::Vector{T}, offset::Int,
                       output_offset::Int, func::Union{SVF, VVF}) where T
end
function fill_constant(constant::Vector{T}, offset::Int,
                       output_offset::Int, func::SAF{T}) where T
    constant[offset + 1] = func.constant
end
function fill_constant(constant::Vector{T}, offset::Int,
                       output_offset::Int, func::VAF{T}) where T
    n = MOI.output_dimension(func)
    constant[offset .+ (1:n)] .= func.constants
end

"""
    vectorize(funcs::AbstractVector{MOI.ScalarAffineFunction{T}}) where T

Returns the vector of scalar affine functions in the form of a
`MOI.VectorAffineFunction{T}`.
"""
function vectorize(funcs::AbstractVector{MOI.ScalarAffineFunction{T}}) where T
    nterms = sum(func -> number_of_affine_terms(T, func), funcs)
    out_dim = sum(func -> output_dim(T, func), funcs)
    terms = Vector{MOI.VectorAffineTerm{T}}(undef, nterms)
    constant = zeros(T, out_dim)
    fill_vector(terms, T, fill_terms, number_of_affine_terms, funcs)
    fill_vector(constant, T, fill_constant, output_dim, funcs)
    return VAF(terms, constant)
end

function promote_operation(::typeof(vcat), ::Type{T},
                           ::Type{<:Union{ScalarAffineLike{T}, VVF, VAF{T}}}...) where T
    return VAF{T}
end
function operate(::typeof(vcat), ::Type{T},
                 funcs::Union{ScalarAffineLike{T}, VVF, VAF{T}}...) where T
    nterms = sum(func -> number_of_affine_terms(T, func), funcs)
    out_dim = sum(func -> output_dim(T, func), funcs)
    terms = Vector{MOI.VectorAffineTerm{T}}(undef, nterms)
    constant = zeros(T, out_dim)
    fill_vector(terms, T, 0, 0, fill_terms, number_of_affine_terms, funcs...)
    fill_vector(constant, T, 0, 0, fill_constant, output_dim, funcs...)
    return VAF(terms, constant)
end
