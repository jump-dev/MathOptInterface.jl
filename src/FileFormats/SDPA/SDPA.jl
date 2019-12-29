module SDPA

# See http://plato.asu.edu/ftp/sdpa_format.txt

import ..FileFormats
import MathOptInterface

const MOI = MathOptInterface

MOI.Utilities.@model(Model,
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
    ::Type{MOI.SingleVariable},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}}
) where {T}

    return false
end

function MOI.supports(
    ::Model,
    ::MOI.ObjectiveFunction{MOI.SingleVariable}
)
    return false
end

function MOI.supports(
    ::Model{T},
    ::MOI.ObjectiveFunction{MOI.ScalarQuadraticFunction{T}}
) where {T}
    return false
end

struct Options end

get_options(m::Model) = get(m.ext, :SDPA_OPTIONS, Options())

"""
    Model(; number_type::Type = Float64)

Create an empty instance of `FileFormats.SDPA.Model{number_type}`.

Note that the model is in geometric form. That is, the SDP model is represented
as a minimization with free variables and affine constraints in either the
nonnegative orthant or the positive semidefinite cone.

If a model is in standard form, that is, nonnegative and positive semidefinite
variables with equality constraints, use `Dualization.jl` to transform it into
the geometric form.
"""
function Model(; number_type::Type = Float64)
    model = Model{number_type}()
    model.ext[:SDPA_OPTIONS] = Options()
    return model
end

function Base.show(io::IO, ::Model)
    print(io, "A SemiDefinite Programming Algorithm Format (SDPA) model")
end

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
        MOI.get(model, MOI.ListOfConstraintIndices{con_func, con_set}())
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
    function _check_variable_index(vi::MOI.VariableIndex)
        if vi.value > num_vars
            error(
                "Non-contiguous variable indices not supported. This might " *
                "be due to deleted variables."
            )
        end
    end

    nonneg = model_cons(MOI.VectorAffineFunction{T}, MOI.Nonnegatives)
    psd = model_cons(
        MOI.VectorAffineFunction{T},
        MOI.PositiveSemidefiniteConeTriangle
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
        obj = MOI.get(model, MOI.ObjectiveFunction{MOI.ScalarAffineFunction{T}}())
        if !iszero(MOI.constant(obj))
            error(
                "Nonzero constant in objective function not supported. Note " *
                "that the constant may be added by the substitution of a " *
                "bridged variable."
            )
        end
        for term in obj.terms
            _check_variable_index(term.variable_index)
            c[term.variable_index.value] = term.coefficient
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

    index_map = Vector{Tuple{Int, Int}}(
        undef, MOI.dimension(MOI.PositiveSemidefiniteConeTriangle(max_dim))
    )
    k = 0
    for col = 1:max_dim
        for row in 1:col
            k += 1
            index_map[k] = (row, col)
        end
    end

    function _print_entry(matrix, block, psd, k, value)
        if psd
            row, col = index_map[k]
            if row == col
                entry = value
            else
                entry = value / 2
            end
        else
            row = k
            col = k
            entry = value
        end
        println(io, matrix, ' ', block, ' ', row, ' ', col, ' ', entry)
    end
    function _print_constraint(block, psd, ci::MOI.ConstraintIndex)
        func = MOI.Utilities.canonical(con_function(ci))
        F0 = MOI.constant(func)
        for k in eachindex(F0)
            if !iszero(F0[k])
                _print_entry(0, block, psd, k, F0[k])
            end
        end
        for term in func.terms
            vi = term.scalar_term.variable_index
            _check_variable_index(vi)
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
    return
end

# ==============================================================================
#
#   Base.read!
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

"""
    Base.read!(io::IO, model::FileFormats.SDPA.Model)

Read `io` in the SDPA file format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model{T}) where T
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    num_variables_read = false
    num_blocks = nothing
    block_sets = nothing
    function dim_to_set(s::AbstractString)
        block_dim = parse(Int, s)
        if block_dim > 0
            return MOI.PositiveSemidefiniteConeTriangle(block_dim)
        else
            return MOI.Nonnegatives(-block_dim)
        end
    end
    objective_read = false
    c = nothing
    funcs = nothing
    MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    while !eof(io)
        line = strip(readline(io))
        # Skip blank lines and comments (SDPA comments start with `"`).
        if startswith(line, '"')
            continue
        end
        if !num_variables_read
            if isempty(line)
                continue
            end
            num_variables_read = true
            MOI.add_variables(model, parse(Int, line))
        elseif num_blocks === nothing
            if isempty(line)
                continue
            end
            num_blocks = parse(Int, line)
        elseif block_sets === nothing
            if isempty(line) && !iszero(num_blocks)
                continue
            end
            block_sets = dim_to_set.(split(line))
            if length(block_sets) != num_blocks
                error("The number of blocks ($num_blocks) does not match the length of the list of blocks dimensions ($(length(block_sets))).")
            end
            funcs = [MOI.VectorAffineFunction(MOI.VectorAffineTerm{T}[], zeros(T, MOI.dimension(block_sets[i]))) for i in 1:num_blocks]
        elseif !objective_read
            num_vars = MOI.get(model, MOI.NumberOfVariables())
            if isempty(line) && !iszero(num_vars)
                continue
            end
            objective_read = true
            c = parse.(T, split(line))
            if length(c) != num_vars
                error("The number of variables ($num_vars) does not match the length of the list of coefficients for the objective function vector of coefficients ($(length(c))).")
            end
            obj = zero(MOI.ScalarAffineFunction{T})
            for i in eachindex(c)
                if !iszero(c[i])
                    push!(obj.terms, MOI.ScalarAffineTerm(c[i], MOI.VariableIndex(i)))
                end
            end
            MOI.set(model, MOI.ObjectiveFunction{typeof(obj)}(), obj)
        else
            if isempty(line)
                continue
            end
            values = split(line)
            if length(values) != 5
                error("Invalid line specifying entry: $line. There are $(length(values)) values instead of 5.")
            end
            matrix = parse(Int, values[1])
            block = parse(Int, values[2])
            row = parse(Int, values[3])
            col = parse(Int, values[4])
            if block_sets[block] isa MOI.PositiveSemidefiniteConeTriangle
                k = mat_to_vec_idx(row, col)
            else
                if row != col
                    error("Invalid line specifying entry: $line. `$row != $col` while block $block has dimension $(MOI.dimension(block_sets[block])) so it is a diagonal block.")
                end
                k = row
            end
            entry = parse(T, values[5])
            if col == row
                coef = entry
            else
                coef = entry * 2
            end
            if iszero(matrix)
                if !iszero(coef)
                    funcs[block].constants[k] += coef
                end
            else
                if !iszero(coef)
                    push!(funcs[block].terms, MOI.VectorAffineTerm(k,
                        MOI.ScalarAffineTerm(coef, MOI.VariableIndex(matrix))))
                end
            end
        end
    end
    for block in 1:num_blocks
        MOI.add_constraint(model, funcs[block], block_sets[block])
    end
    return
end

end
