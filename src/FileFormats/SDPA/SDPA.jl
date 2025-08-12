# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module SDPA

# See http://plato.asu.edu/ftp/sdpa_format.txt

import ..FileFormats
import MathOptInterface as MOI

MOI.Utilities.@model(
    Model,
    (),
    (),
    (MOI.Nonnegatives, MOI.PositiveSemidefiniteConeTriangle),
    (),
    (),
    (),
    (),
    (MOI.VectorAffineFunction,)
)

function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.VariableIndex},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end

function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.VariableIndex},
    ::Type{MOI.Integer},
)
    return true
end

MOI.supports(::Model, ::MOI.ObjectiveFunction) = false

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}},
) where {T}
    return true
end

struct Options end

get_options(m::Model) = get(m.ext, :SDPA_OPTIONS, Options())

"""
    Model(; coefficient_type::Type{T} = Float64) where {T}

Create an empty instance of `FileFormats.SDPA.Model{T}`.

It is important to be aware that the SDPA file format is interpreted in
*geometric* form and not *standard conic* form.
The *standard conic* form and *geometric conic* form are two dual standard forms
for semidefinite programs (SDPs).
The *geometric conic* form of an SDP is as follows:
```math
\\begin{align}
& \\min_{y \\in \\mathbb{R}^m} & b^T y
\\\\
& \\;\\;\\text{s.t.} & \\sum_{i=1}^m A_i y_i - C & \\in \\mathbb{K}
\\end{align}
```
where ``\\mathcal{K}`` is a cartesian product of nonnegative orthant and
positive semidefinite matrices that align with a block diagonal structure
shared with the matrices `A_i` and `C`.

In other words, the geometric conic form contains free variables and affine
constraints in either the nonnegative orthant or the positive semidefinite cone.
That is, in the MathOptInterface's terminology,
[`MOI.VectorAffineFunction`](@ref)-in-[`MOI.Nonnegatives`](@ref)
and
[`MOI.VectorAffineFunction`](@ref)-in-[`MOI.PositiveSemidefiniteConeTriangle`](@ref)
constraints.

The corresponding *standard conic* form of the dual SDP is as follows:
```math
\\begin{align}
& \\max_{X \\in \\mathbb{K}} & \\text{tr}(CX)
\\\\
& \\;\\;\\text{s.t.} & \\text{tr}(A_iX) & = b_i & i = 1, \\ldots, m.
\\end{align}
```

In other words, the standard conic form contains nonnegative and positive
semidefinite variables with equality constraints.
That is, in the MathOptInterface's terminology,
[`MOI.VectorOfVariables`](@ref)-in-[`MOI.Nonnegatives`](@ref),
[`MOI.VectorOfVariables`](@ref)-in-[`MOI.PositiveSemidefiniteConeTriangle`](@ref)
and
[`MOI.ScalarAffineFunction`](@ref)-in-[`MOI.EqualTo`](@ref)
constraints.

If a model is in standard conic form, use `Dualization.jl` to transform it into
the geometric conic form before writting it. Otherwise, the nonnegative (resp.
positive semidefinite) variables will be bridged into free variables with
affine constraints constraining them to belong to the nonnegative orthant
(resp. positive semidefinite cone) by the
[`MOI.Bridges.Constraint.VectorFunctionizeBridge`](@ref). Moreover, equality
constraints will be bridged into pairs of affine constraints in the nonnegative
orthant by the
[`MOI.Bridges.Constraint.SplitIntervalBridge`](@ref)
and then the
[`MOI.Bridges.Constraint.VectorizeBridge`](@ref).

If a solver is in standard conic form, use `Dualization.jl` to transform the
model read into standard conic form before copying it to the solver. Otherwise,
the free variables will be bridged into pairs of variables in the nonnegative
orthant by the
[`MOI.Bridges.Variable.FreeBridge`](@ref)
and affine constraints will be bridged into equality constraints
by creating a slack variable by the
[`MOI.Bridges.Constraint.VectorSlackBridge`](@ref).
"""
function Model(;
    number_type::Type{T} = Float64,
    coefficient_type::Type{S} = number_type,
) where {T,S}
    model = Model{S}()
    model.ext[:SDPA_OPTIONS] = Options()
    return model
end

Base.summary(io::IO, ::Model) = print(io, "MOI.FileFormats.SDPA.Model")

# ==============================================================================
#
#   Base.write
#
# ==============================================================================

"""
    Base.write(io::IO, model::FileFormats.SDPA.Model)

Write `model` to `io` in the SemiDefinite Programming Application file format.
"""
function Base.write(io::IO, model::Model{T}) where {T}
    options = get_options(model)
    # Helper functions for MOI constraints.
    function model_cons(con_func, con_set)
        return MOI.get(model, MOI.ListOfConstraintIndices{con_func,con_set}())
    end
    con_function(con_idx) = MOI.get(model, MOI.ConstraintFunction(), con_idx)
    con_set(con_idx) = MOI.get(model, MOI.ConstraintSet(), con_idx)

    # Model name
    model_name = MOI.get(model, MOI.Name())
    if !isempty(model_name)
        # Lines starting with `"` are comments that should be ignored by a reader.
        println(io, '"', model_name)
    end

    num_vars = MOI.get(model, MOI.NumberOfVariables())
    println(io, num_vars)
    function _check_variable(vi::MOI.VariableIndex)
        if vi.value > num_vars
            error(
                "Non-contiguous variable indices not supported. This might " *
                "be due to deleted variables.",
            )
        end
    end

    nonneg = model_cons(MOI.VectorAffineFunction{T}, MOI.Nonnegatives)
    psd = model_cons(
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle,
    )
    println(io, length(nonneg) + length(psd))

    for block in eachindex(nonneg)
        print(io, -MOI.dimension(con_set(nonneg[block])))
        if block != length(nonneg) || !isempty(psd)
            print(io, " ")
        end
    end
    max_dim = 0
    for i in eachindex(psd)
        dim = MOI.side_dimension(con_set(psd[i]))
        max_dim = max(max_dim, dim)
        print(io, dim)
        if i != length(psd)
            print(io, " ")
        end
    end
    println(io)

    c = zeros(T, num_vars)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense != MOI.FEASIBILITY_SENSE
        obj =
            MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
        if !iszero(MOI.constant(obj))
            error(
                "Nonzero constant in objective function not supported. Note " *
                "that the constant may be added by the substitution of a " *
                "bridged variable.",
            )
        end
        for term in obj.terms
            _check_variable(term.variable)
            c[term.variable.value] = term.coefficient
        end
        if sense == MOI.MAX_SENSE
            for i in eachindex(c)
                c[i] = -c[i]
            end
        end
    end
    for i in eachindex(c)
        print(io, c[i])
        if i != length(c)
            print(io, " ")
        end
    end
    println(io)

    max_index_dim = MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(max_dim))
    index_map = Vector{Tuple{Int,Int}}(undef, max_index_dim)
    k = 0
    for col in 1:max_dim
        for row in 1:col
            k += 1
            index_map[k] = (row, col)
        end
    end

    function _print_entry(matrix, block, psd, k, value)
        if psd
            row, col = index_map[k]
        else
            row = k
            col = k
        end
        return println(io, matrix, ' ', block, ' ', row, ' ', col, ' ', value)
    end
    function _print_constraint(block, psd, ci::MOI.ConstraintIndex)
        func = MOI.Utilities.canonical(con_function(ci))
        F0 = MOI.constant(func)
        for k in eachindex(F0)
            if !iszero(F0[k])
                _print_entry(0, block, psd, k, -F0[k])
            end
        end
        for term in func.terms
            vi = term.scalar_term.variable
            _check_variable(vi)
            α = term.scalar_term.coefficient
            if !iszero(α)
                _print_entry(vi.value, block, psd, term.output_index, α)
            end
        end
    end
    for block in eachindex(nonneg)
        _print_constraint(block, false, nonneg[block])
    end
    for i in eachindex(psd)
        _print_constraint(length(nonneg) + i, true, psd[i])
    end

    # Integrality constraints.
    # Based on the extension: http://www.opt.tu-darmstadt.de/scipsdp/downloads/data_format.txt
    integer_cons = model_cons(MOI.VariableIndex, MOI.Integer)
    if length(integer_cons) > 0
        println(io, "*INTEGER")
        for con_idx in integer_cons
            println(io, "*$(con_function(con_idx).value)")
        end
    end
    return
end

# ==============================================================================
#
#   `Base.read!`
#
# ==============================================================================

# Convert a pair of row and column indices of a symmetric matrix into a vector
# index for the col-wise upper triangle
function mat_to_vec_idx(i::Int, j::Int)
    if i > j
        return mat_to_vec_idx(j, i)
    else
        return div((j - 1) * j, 2) + i
    end
end

function _dim_to_set(s::AbstractString)
    block_dim = parse(Int, s)
    if block_dim > 0
        return MOI.PositiveSemidefiniteConeTriangle(block_dim)
    else
        return MOI.Nonnegatives(-block_dim)
    end
end

function _split(line)
    # In some variations of SDPA, there is the comment:
    #
    # The special characters `,`, `(`, `)`, `{`, and `}` can be used as
    # punctuation and are ignored.
    #
    # As one example, see https://github.com/vsdp/SDPLIB
    line = replace(line, r"[,{}\(\)]"=>' ')
    return split(line)
end

"""
    Base.read!(io::IO, model::FileFormats.SDPA.Model)

Read `io` in the SDPA file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model{T}) where {T<:Real}
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    num_variables_read = false
    num_blocks = nothing
    block_sets = Union{MOI.PositiveSemidefiniteConeTriangle,MOI.Nonnegatives}[]
    block_sets_read = false
    objective_read = false
    integer_read = false
    scalar_vars = MOI.VariableIndex[]
    intvar_idx = Int[]
    c = nothing
    funcs = MOI.VectorAffineFunction{T}[]
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    while !eof(io)
        line = strip(readline(io))
        # Skip blank lines and comments (SDPA comments start with `"`).
        if startswith(line, '"')
            continue
        end
        # The lines starting with * should also be skipped
        # according to http://plato.asu.edu/ftp/sdpa_format.txt.
        if startswith(line, '*')
            # Exceptions for integer variables
            if startswith(line, "*INTEGER")
                integer_read = true
            elseif integer_read
                if !num_variables_read
                    error(
                        "The number of variables should be given before *INTEGER section.",
                    )
                end
                push!(intvar_idx, parse(Int, strip(line[2:end])))
            end
            continue
        end
        if !num_variables_read
            if isempty(line)
                continue
            end
            num_variables_read = true
            # According to http://plato.asu.edu/ftp/sdpa_format.txt,
            # additional text after the number of variables should be ignored.
            scalar_vars =
                MOI.add_variables(model, parse(Int, first(_split(line))))
        elseif num_blocks === nothing
            if isempty(line)
                continue
            end
            # According to http://plato.asu.edu/ftp/sdpa_format.txt,
            # additional text after the number of blocks should be ignored.
            num_blocks = parse(Int, first(_split(line)))
        elseif !block_sets_read
            if isempty(line) && !iszero(num_blocks)
                continue
            end
            block_sets = _dim_to_set.(_split(line))
            block_sets_read = true
            if length(block_sets) != num_blocks
                error(
                    "The number of blocks ($num_blocks) does not match the length of the list of blocks dimensions ($(length(block_sets))).",
                )
            end
            for i in 1:num_blocks
                push!(
                    funcs,
                    MOI.VectorAffineFunction(
                        MOI.VectorAffineTerm{T}[],
                        zeros(T, MOI.dimension(block_sets[i])),
                    ),
                )
            end
        elseif !objective_read
            num_vars = MOI.get(model, MOI.NumberOfVariables())
            if isempty(line) && !iszero(num_vars)
                continue
            end
            objective_read = true
            c = parse.(T, _split(line))
            if length(c) != num_vars
                error(
                    "The number of variables ($num_vars) does not match the length of the list of coefficients for the objective function vector of coefficients ($(length(c))).",
                )
            end
            obj = zero(MOI.ScalarAffineFunction{T})
            for i in eachindex(c)
                if !iszero(c[i])
                    push!(obj.terms, MOI.ScalarAffineTerm(c[i], scalar_vars[i]))
                end
            end
            MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
        else
            if isempty(line)
                continue
            end
            values = _split(line)
            if length(values) != 5
                error(
                    "Invalid line specifying entry: $line. There are $(length(values)) values instead of 5.",
                )
            end
            matrix = parse(Int, values[1])
            block = parse(Int, values[2])
            row = parse(Int, values[3])
            col = parse(Int, values[4])
            if block_sets[block] isa MOI.PositiveSemidefiniteConeTriangle
                k = mat_to_vec_idx(row, col)
            else
                if row != col
                    error(
                        "Invalid line specifying entry: $line. `$row != $col` while block $block has dimension $(MOI.dimension(block_sets[block])) so it is a diagonal block.",
                    )
                end
                k = row
            end
            coef = parse(T, values[5])
            if iszero(matrix)
                if !iszero(coef)
                    funcs[block].constants[k] -= coef
                end
            else
                if !iszero(coef)
                    push!(
                        funcs[block].terms,
                        MOI.VectorAffineTerm(
                            k,
                            MOI.ScalarAffineTerm(coef, scalar_vars[matrix]),
                        ),
                    )
                end
            end
        end
    end
    for block in 1:(num_blocks::Int)
        MOI.add_constraint(model, funcs[block], block_sets[block])
    end
    for var_idx in intvar_idx
        MOI.add_constraint(model, scalar_vars[var_idx], MOI.Integer())
    end
    return
end

end
