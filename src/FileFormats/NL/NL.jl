# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module NL

import MathOptInterface as MOI
import NaNMath

include("NLExpr.jl")

### ============================================================================
### Nonlinear constraints
### ============================================================================

struct _NLConstraint
    lower::Float64
    upper::Float64
    opcode::Int
    expr::_NLExpr
end

"""
    _NLConstraint(expr::Expr, bound::MOI.NLPBoundsPair)

Convert a constraint in the form of a `expr` into a `_NLConstraint` object.

See `MOI.constraint_expr` for details on the format.

As a validation step, the right-hand side of each constraint must be a constant
term that is given by the `bound`. (If the constraint is an interval constraint,
both the left-hand and right-hand sides must be constants.)

The six NL constraint types are:

    l <= g(x) <= u : 0
         g(x) >= l : 1
         g(x) <= u : 2
         g(x)      : 3
         g(x) == c : 4
     x ⟂ g(x)      : 5  # TODO(odow): Complementarity constraints
"""
function _NLConstraint(expr::Expr, bound::MOI.NLPBoundsPair)
    if expr.head == :comparison
        @assert length(expr.args) == 5
        if !(expr.args[1] ≈ bound.lower && bound.upper ≈ expr.args[5])
            _warn_invalid_bound(expr, bound)
        end
        return _NLConstraint(
            expr.args[1],
            expr.args[5],
            0,
            _NLExpr(expr.args[3]),
        )
    else
        @assert expr.head == :call
        @assert length(expr.args) == 3
        if expr.args[1] == :(<=)
            if !(-Inf ≈ bound.lower && bound.upper ≈ expr.args[3])
                _warn_invalid_bound(expr, bound)
            end
            return _NLConstraint(-Inf, expr.args[3], 1, _NLExpr(expr.args[2]))
        elseif expr.args[1] == :(>=)
            if !(expr.args[3] ≈ bound.lower && bound.upper ≈ Inf)
                _warn_invalid_bound(expr, bound)
            end
            return _NLConstraint(expr.args[3], Inf, 2, _NLExpr(expr.args[2]))
        else
            @assert expr.args[1] == :(==)
            if !(expr.args[3] ≈ bound.lower ≈ bound.upper)
                _warn_invalid_bound(expr, bound)
            end
            return _NLConstraint(
                expr.args[3],
                expr.args[3],
                4,
                _NLExpr(expr.args[2]),
            )
        end
    end
end

function _warn_invalid_bound(expr::Expr, bound::MOI.NLPBoundsPair)
    return @warn(
        "Invalid bounds detected in nonlinear constraint. Expected " *
        "`$(bound.lower) <= g(x) <= $(bound.upper)`, but got the constraint " *
        "$(expr)",
    )
end

### ============================================================================
### Nonlinear models
### ============================================================================

@enum(_VariableType, _BINARY, _INTEGER, _CONTINUOUS)

mutable struct _VariableInfo
    # Variable lower bound.
    lower::Float64
    # Variable upper bound.
    upper::Float64
    # Whether variable is binary or integer.
    type::_VariableType
    # Primal start of the variable.
    start::Union{Float64,Nothing}
    # Number of constraints that the variable appears in.
    jacobian_count::Int
    # If the variable appears in the objective.
    in_nonlinear_objective::Bool
    # If the variable appears in a nonlinear constraint.
    in_nonlinear_constraint::Bool
    # The 0-indexed column of the variable. Computed right at the end.
    order::Int
    function _VariableInfo()
        return new(-Inf, Inf, _CONTINUOUS, nothing, 0, false, false, 0)
    end
end

"""
    Model(; use_nlp_block::Bool = true)

Create a new Optimizer object.
"""
mutable struct Model <: MOI.ModelLike
    # Store MOI.Name().
    name::String
    # The objective expression.
    f::_NLExpr
    sense::MOI.OptimizationSense
    # Number of nonlinear constraints in NLPBlock
    nlpblock_dim::Int
    # A vector of nonlinear constraints
    g::Vector{_NLConstraint}
    # A vector of linear constraints
    h::Vector{_NLConstraint}
    # A dictionary of info for the variables.
    x::Dict{MOI.VariableIndex,_VariableInfo}
    # A struct to help sort the mess that is variable ordering in NL files.
    types::Vector{Vector{MOI.VariableIndex}}
    # A vector of the final ordering of the variables.
    order::Vector{MOI.VariableIndex}
    model::Union{
        Nothing,
        MOI.Utilities.UniversalFallback{MOI.Utilities.Model{Float64}},
    }
    use_nlp_block::Bool

    function Model(; use_nlp_block::Bool = true)
        return new(
            "",
            _NLExpr(false, _NLTerm[], Dict{MOI.VariableIndex,Float64}(), 0.0),
            MOI.FEASIBILITY_SENSE,
            0,
            _NLConstraint[],
            _NLConstraint[],
            Dict{MOI.VariableIndex,_VariableInfo}(),
            [MOI.VariableIndex[] for _ in 1:9],
            MOI.VariableIndex[],
            nothing,
            use_nlp_block,
        )
    end
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.NL.Model")

MOI.get(model::Model, ::MOI.SolverName) = "AmplNLWriter"

MOI.supports(::Model, ::MOI.NLPBlock) = true

MOI.supports(::Model, ::MOI.Name) = true
MOI.get(model::Model, ::MOI.Name) = model.name
MOI.set(model::Model, ::MOI.Name, name::String) = (model.name = name)

function MOI.empty!(model::Model)
    model.f = _NLExpr(false, _NLTerm[], Dict{MOI.VariableIndex,Float64}(), 0.0)
    empty!(model.g)
    model.nlpblock_dim = 0
    empty!(model.h)
    empty!(model.x)
    for i in 1:9
        empty!(model.types[i])
    end
    empty!(model.order)
    model.model = nothing
    return
end

function MOI.is_empty(model::Model)
    return isempty(model.g) && isempty(model.h) && isempty(model.x)
end

const _SCALAR_FUNCTIONS = Union{
    MOI.VariableIndex,
    MOI.ScalarAffineFunction{Float64},
    MOI.ScalarQuadraticFunction{Float64},
    MOI.ScalarNonlinearFunction,
}

const _SCALAR_SETS = Union{
    MOI.LessThan{Float64},
    MOI.GreaterThan{Float64},
    MOI.EqualTo{Float64},
    MOI.Interval{Float64},
}

function MOI.supports_constraint(
    ::Model,
    ::Type{<:_SCALAR_FUNCTIONS},
    ::Type{<:_SCALAR_SETS},
)
    return true
end

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{<:Union{MOI.ZeroOne,MOI.Integer}},
)
    return true
end

MOI.supports(::Model, ::MOI.ObjectiveSense) = true
MOI.supports(::Model, ::MOI.ObjectiveFunction{<:_SCALAR_FUNCTIONS}) = true

# ==============================================================================

function MOI.supports(
    ::Model,
    ::MOI.VariablePrimalStart,
    ::Type{MOI.VariableIndex},
)
    return true
end

function MOI.set(
    model::Model,
    ::MOI.VariablePrimalStart,
    x::MOI.VariableIndex,
    v::Union{Nothing,Real},
)
    model.x[x].start = v === nothing ? nothing : convert(Float64, v)::Float64
    return
end

# ==============================================================================

function MOI.copy_to(dest::Model, model::MOI.ModelLike)
    if !MOI.is_empty(dest)
        MOI.empty!(dest)
    end
    mapping = MOI.Utilities.IndexMap()
    has_nlp_objective = false
    for attr in MOI.get(model, MOI.ListOfModelAttributesSet())
        if attr == MOI.NLPBlock()
            nlp_block = MOI.get(model, MOI.NLPBlock())
            if !(:ExprGraph in MOI.features_available(nlp_block.evaluator))
                error(
                    "Unable to use AmplNLWriter because the nonlinear " *
                    "evaluator does not supply expression graphs.",
                )
            end
            MOI.initialize(nlp_block.evaluator, [:ExprGraph])
            if nlp_block.has_objective
                dest.f = _NLExpr(MOI.objective_expr(nlp_block.evaluator))
            end
            has_nlp_objective = nlp_block.has_objective
            for (i, bound) in enumerate(nlp_block.constraint_bounds)
                expr = MOI.constraint_expr(nlp_block.evaluator, i)
                push!(dest.g, _NLConstraint(expr, bound))
            end
            dest.nlpblock_dim = length(dest.g)
        elseif attr == MOI.ObjectiveSense()
            dest.sense = MOI.get(model, MOI.ObjectiveSense())
        elseif !has_nlp_objective && attr isa MOI.ObjectiveFunction
            dest.f = _NLExpr(MOI.get(model, attr))
        elseif attr == MOI.Name()
            dest.name = MOI.get(model, MOI.Name())
        else
            throw(MOI.UnsupportedAttribute(attr))
        end
    end
    x_src = MOI.get(model, MOI.ListOfVariableIndices())
    for x in x_src
        dest.x[x] = _VariableInfo()
        mapping[x] = x
    end
    MOI.Utilities.pass_attributes(dest, model, mapping, x_src)
    resize!(dest.order, length(dest.x))
    # Now deal with the normal MOI constraints.
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if !MOI.supports_constraint(dest, F, S)
            throw(MOI.UnsupportedConstraint{F,S}())
        end
        _process_constraint(dest, model, F, S, mapping)
    end
    # Correct bounds of binary variables. Mainly because AMPL doesn't have the
    # concept of binary nonlinear variables, but it does have binary linear
    # variables. How annoying.
    for (_, v) in dest.x
        if v.type == _BINARY
            v.lower = max(0.0, v.lower)
            v.upper = min(1.0, v.upper)
        end
    end
    # Jacobian counts. The zero terms for nonlinear constraints should have
    # been added when the expression was constructed.
    for g in dest.g, v in keys(g.expr.linear_terms)
        dest.x[v].jacobian_count += 1
    end
    for h in dest.h, v in keys(h.expr.linear_terms)
        dest.x[v].jacobian_count += 1
    end
    # Now comes the confusing part.
    #
    # AMPL, in all its wisdom, orders variables in a _very_ specific way.
    # The only hint in "Writing NL files" is the line "Variables are ordered as
    # described in Tables 3 and 4 of [5]".
    #
    # Reading these
    #
    # https://cfwebprod.sandia.gov/cfdocs/CompResearch/docs/nlwrite20051130.pdf
    # https://ampl.com/REFS/hooking2.pdf
    #
    # leads us to the following order
    #
    # 1) Continuous variables that appear in a
    #       nonlinear objective AND a nonlinear constraint
    # 2) Discrete variables that appear in a
    #       nonlinear objective AND a nonlinear constraint
    # 3) Continuous variables that appear in a
    #       nonlinear constraint, but NOT a nonlinear objective
    # 4) Discrete variables that appear in a
    #       nonlinear constraint, but NOT a nonlinear objective
    # 5) Continuous variables that appear in a
    #       nonlinear objective, but NOT a nonlinear constraint
    # 6) Discrete variables that appear in a
    #       nonlinear objective, but NOT a nonlinear constraint
    # 7) Continuous variables that DO NOT appear in a
    #       nonlinear objective or a nonlinear constraint
    # 8) Binary variables that DO NOT appear in a
    #       nonlinear objective or a nonlinear constraint
    # 9) Integer variables that DO NOT appear in a
    #       nonlinear objective or a nonlinear constraint
    #
    # Yes, nonlinear variables are broken into continuous/discrete, but linear
    # variables are partitioned into continuous, binary, and integer. (See also,
    # the need to modify bounds for binary variables.)
    if !dest.f.is_linear
        for x in keys(dest.f.linear_terms)
            dest.x[x].in_nonlinear_objective = true
        end
        for x in dest.f.nonlinear_terms
            if x isa MOI.VariableIndex
                dest.x[x].in_nonlinear_objective = true
            end
        end
    end
    for con in dest.g
        for x in keys(con.expr.linear_terms)
            dest.x[x].in_nonlinear_constraint = true
        end
        for x in con.expr.nonlinear_terms
            if x isa MOI.VariableIndex
                dest.x[x].in_nonlinear_constraint = true
            end
        end
    end
    # This ordering is quite confusing. Consult the README for details.
    types = dest.types
    for (x, v) in dest.x
        if v.in_nonlinear_constraint && v.in_nonlinear_objective
            push!(v.type == _CONTINUOUS ? types[1] : types[2], x)
        elseif v.in_nonlinear_constraint
            push!(v.type == _CONTINUOUS ? types[3] : types[4], x)
        elseif v.in_nonlinear_objective
            push!(v.type == _CONTINUOUS ? types[5] : types[6], x)
        elseif v.type == _CONTINUOUS
            push!(types[7], x)
        elseif v.type == _BINARY
            push!(types[8], x)
        else
            @assert v.type == _INTEGER
            push!(types[9], x)
        end
    end
    # Now we can order the variables.
    n = 0
    for i in 1:9
        # Since variables come from a dictionary, there may be differences in
        # the order depending on platform and Julia version. Sort by creation
        # time for consistency.
        for x in sort!(types[i]; by = y -> y.value)
            dest.x[x].order = n
            dest.order[n+1] = x
            n += 1
        end
    end
    return mapping
end

function _set_to_bounds(set::MOI.Interval)
    if set.lower == -Inf && set.upper == Inf
        return (3, set.lower, set.upper)
    elseif set.lower == -Inf
        return (1, set.lower, set.upper)
    elseif set.upper == Inf
        return (2, set.lower, set.upper)
    else
        return (0, set.lower, set.upper)
    end
end

_set_to_bounds(set::MOI.LessThan) = (1, -Inf, set.upper)
_set_to_bounds(set::MOI.GreaterThan) = (2, set.lower, Inf)
_set_to_bounds(set::MOI.EqualTo) = (4, set.value, set.value)

function _process_constraint(
    dest::Model,
    model,
    ::Type{F},
    ::Type{S},
    mapping,
) where {F,S}
    ci_src = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    for ci in ci_src
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        op, l, u = _set_to_bounds(s)
        con = _NLConstraint(l, u, op, _NLExpr(f))
        if isempty(con.expr.linear_terms) && isempty(con.expr.nonlinear_terms)
            if !(l <= con.expr.constant <= u)
                error(
                    "Malformed constraint. There are no variables and the " *
                    "function constant $(con.expr.constant) is not in [$l, $u]",
                )
            end
            # Just use a placeholder for the constraint index. It's not going to
            # be used.
            mapping[ci] = MOI.ConstraintIndex{F,S}(-abs(ci.value))
        elseif con.expr.is_linear
            push!(dest.h, con)
            mapping[ci] = MOI.ConstraintIndex{F,S}(length(dest.h))
        else
            push!(dest.g, con)
            mapping[ci] = MOI.ConstraintIndex{F,S}(length(dest.g))
        end
    end
    MOI.Utilities.pass_attributes(dest, model, mapping, ci_src)
    return
end

function _process_constraint(
    dest::Model,
    model,
    F::Type{MOI.VariableIndex},
    S::Type{<:_SCALAR_SETS},
    mapping,
)
    ci_src = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    for ci in ci_src
        mapping[ci] = ci
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        s = MOI.get(model, MOI.ConstraintSet(), ci)
        _, l, u = _set_to_bounds(s)
        if l > -Inf
            dest.x[f].lower = l
        end
        if u < Inf
            dest.x[f].upper = u
        end
    end
    MOI.Utilities.pass_attributes(dest, model, mapping, ci_src)
    return
end

function _process_constraint(
    dest::Model,
    model,
    F::Type{MOI.VariableIndex},
    S::Type{<:Union{MOI.ZeroOne,MOI.Integer}},
    mapping,
)
    ci_src = MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
    for ci in ci_src
        mapping[ci] = ci
        f = MOI.get(model, MOI.ConstraintFunction(), ci)
        dest.x[f].type = S == MOI.ZeroOne ? _BINARY : _INTEGER
    end
    MOI.Utilities.pass_attributes(dest, model, mapping, ci_src)
    return
end

function _str(x::Float64)
    if isinteger(x) && (typemin(Int) <= x <= typemax(Int))
        return string(round(Int, x))
    end
    return string(x)
end

_write_term(io::IO, ::Model, x::Float64) = println(io, "n", _str(x))

_write_term(io::IO, ::Model, x::Int) = println(io, "o", x)

function _write_term(io::IO, model::Model, x::MOI.VariableIndex)
    return println(io, "v", model.x[x].order)
end

_is_nary(x::Int) = x in _NARY_OPCODES

_is_nary(x) = false

function _write_nlexpr(io::IO, expr::_NLExpr, model::Model)
    if expr.is_linear || length(expr.nonlinear_terms) == 0
        # If the expression is linear, just write out the constant term.
        _write_term(io, model, expr.constant)
        return
    end
    if !iszero(expr.constant)
        # If the constant term is non-zero, we need to write it out.
        _write_term(io, model, OPPLUS)
        _write_term(io, model, expr.constant)
    end
    last_nary = false
    for term in expr.nonlinear_terms
        if last_nary
            println(io, term::Int)
            last_nary = false
        else
            _write_term(io, model, term)
            last_nary = _is_nary(term)
        end
    end
    return
end

function _write_linear_block(io::IO, expr::_NLExpr, model::Model)
    elements = [(c, model.x[v].order) for (v, c) in expr.linear_terms]
    for (c, x) in sort!(elements; by = i -> i[2])
        println(io, x, " ", _str(c))
    end
    return
end

function Base.write(io::IO, model::Model)
    # ==========================================================================
    # Header
    # Line 1: Always the same
    # Notes:
    #  * I think these are magic bytes used by AMPL internally for stuff.
    #  * The first "1" is if the next row includes the number of logical
    #    constraints.
    println(io, "g3 1 1 0")

    # Line 2: vars, constraints, objectives, ranges, eqns, logical constraints
    # Notes:
    #  * We assume there is always one objective, even if it is just `min 0`.
    n_con, n_ranges, n_eqns = 0, 0, 0
    for cons in (model.g, model.h), c in cons
        n_con += 1
        if c.opcode == 0
            n_ranges += 1
        elseif c.opcode == 4
            n_eqns += 1
        end
    end
    println(io, " $(length(model.x)) $(n_con) 1 $(n_ranges) $(n_eqns) 0")

    # Line 3: nonlinear constraints, objectives
    # Notes:
    #  * We assume there is always one objective, even if it is just `min 0`.
    n_nlcon = length(model.g)
    println(io, " ", n_nlcon, " ", 1)

    # Line 4: network constraints: nonlinear, linear
    # Notes:
    #  * We don't support network constraints. I don't know how they are
    #    represented.
    println(io, " 0 0")

    # Line 5: nonlinear vars in constraints, objectives, both
    # Notes:
    #  * This order is confusingly different to the standard "b, c, o" order.
    #  * It's also confusing because nlvo doesn't mean what you think it means.
    #    Consult the README for details.
    nlvb = length(model.types[1]) + length(model.types[2])
    nlvc = nlvb + length(model.types[3]) + length(model.types[4])
    nlvo = nlvb + length(model.types[5]) + length(model.types[6])
    if nlvo == nlvb
        println(io, " ", nlvc, " ", nlvo, " ", nlvb)
    else
        nl_total = nlvo + nlvc - nlvb
        println(io, " ", nlvc, " ", nl_total, " ", nlvb)
    end

    # Line 6: linear network variables; functions; arith, flags
    # Notes:
    #  * I don't know what this line means. It is what it is. Apparently `flags`
    #    is set to 1 to get suffixes in .sol file.
    println(io, " 0 0 0 1")

    # Line 7: discrete variables: binary, integer, nonlinear (b,c,o)
    # Notes:
    #  * The order is
    #    - binary variables in linear only
    #    - integer variables in linear only
    #    - binary or integer variables in nonlinear objective and constraint
    #    - binary or integer variables in nonlinear constraint
    #    - binary or integer variables in nonlinear objective
    nbv = length(model.types[8])
    niv = length(model.types[9])
    nl_both = length(model.types[2])
    nl_cons = length(model.types[4])
    nl_obj = length(model.types[6])
    println(io, " ", nbv, " ", niv, " ", nl_both, " ", nl_cons, " ", nl_obj)

    # Line 8: nonzeros in Jacobian, gradients
    # Notes:
    #  * Make sure to include a 0 element for every variable that appears in an
    #    objective or constraint, even if the linear coefficient is 0.
    nnz_jacobian = 0
    for g in model.g
        nnz_jacobian += length(g.expr.linear_terms)
    end
    for h in model.h
        nnz_jacobian += length(h.expr.linear_terms)
    end
    nnz_gradient = length(model.f.linear_terms)
    println(io, " ", nnz_jacobian, " ", nnz_gradient)

    # Line 9: max name lengths: constraints, variables
    # Notes:
    #  * We don't add names, so this is just 0, 0.
    println(io, " 0 0")

    # Line 10: common exprs: b,c,o,c1,o1
    # Notes:
    #  * We don't add common subexpressions (that is, V blocks).
    #  * I assume the notation means
    #     - b = in nonlinear objective and constraint
    #     - c = in nonlinear constraint
    #     - o = in nonlinear objective
    #     - c1 = in linear constraint
    #     - o1 = in linear objective
    println(io, " 0 0 0 0 0")
    # ==========================================================================
    # Constraints
    # Notes:
    #  * Nonlinear constraints first, then linear.
    #  * For linear constraints, write out the constant term here.
    for (i, g) in enumerate(model.g)
        println(io, "C", i - 1)
        _write_nlexpr(io, g.expr, model)
    end
    for (i, h) in enumerate(model.h)
        println(io, "C", i - 1 + n_nlcon)
        _write_nlexpr(io, h.expr, model)
    end
    # ==========================================================================
    # Objective
    # Notes:
    #  * NL files support multiple objectives, but we're just going to write 1,
    #    so it's always `O0`.
    #  * For linear objectives, write out the constant term here.
    println(io, "O0 ", model.sense == MOI.MAX_SENSE ? "1" : "0")
    _write_nlexpr(io, model.f, model)
    # ==========================================================================
    # VariablePrimalStart
    # Notes:
    #  * Make sure to write out the variables in order.
    println(io, "x", length(model.x))
    for (i, x) in enumerate(model.order)
        start = model.x[x].start
        println(io, i - 1, " ", start === nothing ? 0 : _str(start))
    end
    # ==========================================================================
    # Constraint bounds
    # Notes:
    #  * Nonlinear constraints go first, then linear.
    #  * The constant term for linear constraints gets written out in the
    #    "C" block.
    if n_con > 0
        println(io, "r")
        # Nonlinear constraints
        for g in model.g
            print(io, g.opcode)
            if g.opcode == 0
                println(io, " ", _str(g.lower), " ", _str(g.upper))
            elseif g.opcode == 1
                println(io, " ", _str(g.upper))
            elseif g.opcode == 2
                println(io, " ", _str(g.lower))
            elseif g.opcode == 3
                println(io)
            else
                @assert g.opcode == 4
                println(io, " ", _str(g.lower))
            end
        end
        # Linear constraints
        for h in model.h
            print(io, h.opcode)
            if h.opcode == 0
                println(io, " ", _str(h.lower), " ", _str(h.upper))
            elseif h.opcode == 1
                println(io, " ", _str(h.upper))
            elseif h.opcode == 2
                println(io, " ", _str(h.lower))
            elseif h.opcode == 3
                println(io)
            else
                @assert h.opcode == 4
                println(io, " ", _str(h.lower))
            end
        end
    end
    # ==========================================================================
    # Variable bounds
    # Notes:
    #  * Not much to note, other than to make sure you iterate the variables in
    #    the correct order.
    println(io, "b")
    for x in model.order
        v = model.x[x]
        if v.lower == v.upper
            println(io, "4 ", _str(v.lower))
        elseif -Inf < v.lower && v.upper < Inf
            println(io, "0 ", _str(v.lower), " ", _str(v.upper))
        elseif -Inf == v.lower && v.upper < Inf
            println(io, "1 ", _str(v.upper))
        elseif -Inf < v.lower && v.upper == Inf
            println(io, "2 ", _str(v.lower))
        else
            println(io, "3")
        end
    end
    # ==========================================================================
    # Jacobian block
    # Notes:
    #  * If a variable appears in a constraint, it needs to have a corresponding
    #    entry in the Jacobian block, even if the linear coefficient is zero.
    #    AMPL uses this to determine the Jacobian sparsity.
    #  * As before, nonlinear constraints go first, then linear.
    #  * Don't write out the `k` entry for the last variable, because it can be
    #    inferred from the total number of elements in the Jacobian as given in
    #    the header.
    if n_con > 0
        println(io, "k", length(model.x) - 1)
        total = 0
        for i in 1:(length(model.order)-1)
            total += model.x[model.order[i]].jacobian_count
            println(io, total)
        end
        for (i, g) in enumerate(model.g)
            println(io, "J", i - 1, " ", length(g.expr.linear_terms))
            _write_linear_block(io, g.expr, model)
        end
        for (i, h) in enumerate(model.h)
            println(io, "J", i - 1 + n_nlcon, " ", length(h.expr.linear_terms))
            _write_linear_block(io, h.expr, model)
        end
    end
    # ==========================================================================
    # Gradient block
    # Notes:
    #  * You only need to write this out if there are linear terms in the
    #    objective.
    if nnz_gradient > 0
        println(io, "G0 ", nnz_gradient)
        _write_linear_block(io, model.f, model)
    end
    return model
end

include("read.jl")
include("sol.jl")

function _assert_has_model(::Nothing, attr)
    return error(
        "Unable get attribute $attr because `NL.Model` only supports getting " *
        "attributes when the model was read from a file.",
    )
end

_assert_has_model(model::MOI.Utilities.UniversalFallback, ::Any) = model

function MOI.get(model::Model, attr::MOI.AbstractModelAttribute)
    inner = _assert_has_model(model.model, attr)
    return MOI.get(inner, attr)
end

function MOI.get(
    model::Model,
    attr::MOI.AbstractConstraintAttribute,
    ci::MOI.ConstraintIndex,
)
    inner = _assert_has_model(model.model, attr)
    return MOI.get(inner, attr, ci)
end

function MOI.get(
    model::Model,
    attr::MOI.AbstractVariableAttribute,
    x::MOI.VariableIndex,
)
    inner = _assert_has_model(model.model, attr)
    return MOI.get(inner, attr, x)
end

end
