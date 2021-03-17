module CBF

import ..FileFormats
import MathOptInterface

const MOI = MathOptInterface

MOI.Utilities.@model(
    Model,
    (MOI.Integer,),
    (),
    (
        MOI.Reals,
        MOI.Zeros,
        MOI.Nonnegatives,
        MOI.Nonpositives,
        MOI.SecondOrderCone,
        MOI.RotatedSecondOrderCone,
        MOI.PositiveSemidefiniteConeTriangle,
        MOI.ExponentialCone,
        MOI.DualExponentialCone,
    ),
    (MOI.PowerCone, MOI.DualPowerCone),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    (MOI.VectorAffineFunction,)
)
function MOI.supports_constraint(
    ::Model{T},
    ::Type{MOI.SingleVariable},
    ::Type{<:MOI.Utilities.SUPPORTED_VARIABLE_SCALAR_SETS{T}},
) where {T}
    return false
end
function MOI.supports_constraint(
    ::Model,
    ::Type{MOI.SingleVariable},
    ::Type{MOI.Integer},
)
    return true
end

struct Options end

get_options(m::Model) = get(m.ext, :CBF_OPTIONS, Options())

"""
    Model()

Create an empty instance of `FileFormats.CBF.Model`.
"""
function Model()
    model = Model{Float64}()
    model.ext[:CBF_OPTIONS] = Options()
    return model
end

Base.show(io::IO, ::Model) = print(io, "A Conic Benchmark Format (CBF) model")

# ==============================================================================
#
#   Base.write
#
# ==============================================================================

"""
    Base.write(io::IO, model::FileFormats.CBF.Model)

Write `model` to `io` in the Conic Benchmark Format.
"""
function Base.write(io::IO, model::Model)
    options = get_options(model)
    # Helper functions for MOI constraints.
    function model_cons(con_func, con_set)
        return MOI.get(model, MOI.ListOfConstraintIndices{con_func,con_set}())
    end
    con_function(con_idx) = MOI.get(model, MOI.ConstraintFunction(), con_idx)
    con_set(con_idx) = MOI.get(model, MOI.ConstraintSet(), con_idx)

    # CBF version number.
    println(io, "VER")
    println(io, 3)
    println(io)

    # Power cone parameters.
    powcone_cons = vcat(
        model_cons(MOI.VectorOfVariables, MOI.PowerCone{Float64}),
        model_cons(MOI.VectorAffineFunction{Float64}, MOI.PowerCone{Float64}),
    )
    dpowcone_cons = vcat(
        model_cons(MOI.VectorOfVariables, MOI.DualPowerCone{Float64}),
        model_cons(
            MOI.VectorAffineFunction{Float64},
            MOI.DualPowerCone{Float64},
        ),
    )
    for (cons, powcone_str) in
        ((powcone_cons, "POWCONES"), (dpowcone_cons, "POW*CONES"))
        if !isempty(cons)
            println(io, powcone_str)
            num_powcones = length(cons)
            println(io, num_powcones, " ", 2 * num_powcones)
            for powcone in cons
                println(io, 2)
                powcone_param = con_set(powcone).exponent
                println(io, powcone_param)
                println(io, 1.0 - powcone_param)
            end
            println(io)
        end
    end

    # Objective sense.
    println(io, "OBJSENSE")
    obj_sense = MOI.get(model, MOI.ObjectiveSense())
    if obj_sense == MOI.MAX_SENSE
        println(io, "MAX")
    else
        println(io, "MIN") # Includes the case of feasibility sense.
    end
    println(io)

    # Variables.
    num_var = MOI.get(model, MOI.NumberOfVariables())
    println(io, "VAR")
    println(io, num_var, " 1")
    println(io, "F ", num_var)
    println(io)

    # Integrality constraints.
    integer_cons = model_cons(MOI.SingleVariable, MOI.Integer)
    if length(integer_cons) > 0
        println(io, "INT")
        println(io, length(integer_cons))
        for con_idx in integer_cons
            println(io, con_function(con_idx).variable.value - 1)
        end
        println(io)
    end

    # Objective function terms.
    obj_type = MOI.get(model, MOI.ObjectiveFunctionType())
    obj_function = MOI.get(model, MOI.ObjectiveFunction{obj_type}())
    if obj_type == MOI.SingleVariable
        println(io, "OBJACOORD")
        println(io, 1)
        println(io, obj_function.variable.value - 1, " ", 1.0)
        println(io)
    elseif obj_type == MOI.ScalarAffineFunction{Float64}
        if !isempty(obj_function.terms)
            println(io, "OBJACOORD")
            println(io, length(obj_function.terms))
            for t in obj_function.terms
                println(io, t.variable_index.value - 1, " ", t.coefficient)
            end
            println(io)
        end
        if !iszero(obj_function.constant)
            println(io, "OBJBCOORD")
            println(io, obj_function.constant)
            println(io)
        end
    else
        error("Objective function type $obj_type is not supported.")
    end

    # Non-PSD constraints.
    num_rows = 0 # Number of constraint rows.
    con_cones = Tuple{String,Int}[] # List of cone types/dimensions.
    acoord = Tuple{Int,Int,Float64}[] # Affine terms.
    bcoord = Tuple{Int,Float64}[] # Constant terms.
    powcone_idx = 0
    dpowcone_idx = 0

    for (set_type, cone_str) in (
        (MOI.Zeros, "L="),
        (MOI.Reals, "F"),
        (MOI.Nonnegatives, "L+"),
        (MOI.Nonpositives, "L-"),
        (MOI.SecondOrderCone, "Q"),
        (MOI.RotatedSecondOrderCone, "QR"),
        (MOI.ExponentialCone, "EXP"),
        (MOI.DualExponentialCone, "EXP*"),
        (MOI.PowerCone{Float64}, ""),
        (MOI.DualPowerCone{Float64}, ""),
    )
        for con_idx in model_cons(MOI.VectorOfVariables, set_type)
            vars = con_function(con_idx).variables
            if set_type in (MOI.ExponentialCone, MOI.DualExponentialCone)
                # Reverse order of indices.
                for vj in reverse(vars)
                    num_rows += 1
                    push!(acoord, (num_rows, vj.value, 1.0))
                end
            else
                for vj in vars
                    num_rows += 1
                    push!(acoord, (num_rows, vj.value, 1.0))
                end
            end
            if set_type == MOI.PowerCone{Float64}
                cone_str = "@$(powcone_idx):POW"
                powcone_idx += 1
            elseif set_type == MOI.DualPowerCone{Float64}
                cone_str = "@$(dpowcone_idx):POW*"
                dpowcone_idx += 1
            end
            push!(con_cones, (cone_str, MOI.dimension(con_set(con_idx))))
        end

        for con_idx in model_cons(MOI.VectorAffineFunction{Float64}, set_type)
            con_func = con_function(con_idx)
            con_dim = MOI.dimension(con_set(con_idx))
            if set_type in (MOI.ExponentialCone, MOI.DualExponentialCone)
                # Reverse order of indices.
                @assert con_dim == 3
                for t in con_func.terms
                    push!(
                        acoord,
                        (
                            num_rows + 4 - t.output_index,
                            t.scalar_term.variable_index.value,
                            t.scalar_term.coefficient,
                        ),
                    )
                end
                for row_constant in reverse(con_func.constants)
                    num_rows += 1
                    push!(bcoord, (num_rows, row_constant))
                end
            else
                for t in con_func.terms
                    push!(
                        acoord,
                        (
                            num_rows + t.output_index,
                            t.scalar_term.variable_index.value,
                            t.scalar_term.coefficient,
                        ),
                    )
                end
                for row_constant in con_func.constants
                    num_rows += 1
                    push!(bcoord, (num_rows, row_constant))
                end
            end
            if set_type == MOI.PowerCone{Float64}
                cone_str = "@$(powcone_idx):POW"
                powcone_idx += 1
            elseif set_type == MOI.DualPowerCone{Float64}
                cone_str = "@$(dpowcone_idx):POW*"
                dpowcone_idx += 1
            end
            push!(con_cones, (cone_str, con_dim))
        end
    end

    if !isempty(con_cones)
        println(io, "CON")
        println(io, num_rows, " ", length(con_cones))
        for (cone_str, con_dim) in con_cones
            println(io, cone_str, " ", con_dim)
        end
        println(io)
    end

    if !isempty(acoord)
        println(io, "ACOORD")
        println(io, length(acoord))
        for (row, var, coef) in acoord
            println(io, row - 1, " ", var - 1, " ", coef)
        end
        println(io)
    end

    if !isempty(bcoord)
        println(io, "BCOORD")
        println(io, length(bcoord))
        for (row, constant) in bcoord
            println(io, row - 1, " ", constant)
        end
        println(io)
    end

    # PSD constraints.
    psd_side_dims = Int[] # List of side dimensions.
    hcoord = Tuple{Int,Int,Int,Int,Float64}[] # Affine terms.
    dcoord = Tuple{Int,Int,Int,Float64}[] # Constant terms.

    for con_idx in
        model_cons(MOI.VectorOfVariables, MOI.PositiveSemidefiniteConeTriangle) # MOI variable constraints.
        side_dim = con_set(con_idx).side_dimension
        push!(psd_side_dims, side_dim)
        vars = con_function(con_idx).variables
        k = 0
        for i in 1:side_dim, j in 1:i
            k += 1
            push!(hcoord, (length(psd_side_dims), vars[k].value, i, j, 1.0))
        end
        @assert k == length(vars)
    end

    for con_idx in model_cons(
        MOI.VectorAffineFunction{Float64},
        MOI.PositiveSemidefiniteConeTriangle,
    ) # MOI affine constraints.
        side_dim = con_set(con_idx).side_dimension
        push!(psd_side_dims, side_dim)
        con_func = con_function(con_idx)
        for t in con_func.terms
            # Get (i,j) index in symmetric matrix from lower triangle index.
            i = div(1 + isqrt(8 * t.output_index - 7), 2)
            j = t.output_index - div(i * (i - 1), 2)
            push!(
                hcoord,
                (
                    length(psd_side_dims),
                    t.scalar_term.variable_index.value,
                    i,
                    j,
                    t.scalar_term.coefficient,
                ),
            )
        end
        k = 0
        for i in 1:side_dim, j in 1:i
            k += 1
            row_constant = con_func.constants[k]
            if !iszero(row_constant)
                push!(dcoord, (length(psd_side_dims), i, j, row_constant))
            end
        end
        @assert k == MOI.output_dimension(con_func)
    end

    if !isempty(psd_side_dims)
        println(io, "PSDCON")
        println(io, length(psd_side_dims))
        for side_dim in psd_side_dims
            println(io, side_dim)
        end
        println(io)
    end

    if !isempty(hcoord)
        println(io, "HCOORD")
        println(io, length(hcoord))
        for (psd_idx, var, i, j, coef) in hcoord
            println(
                io,
                psd_idx - 1,
                " ",
                var - 1,
                " ",
                i - 1,
                " ",
                j - 1,
                " ",
                coef,
            )
        end
        println(io)
    end

    if !isempty(dcoord)
        println(io, "DCOORD")
        println(io, length(dcoord))
        for (psd_idx, i, j, constant) in dcoord
            println(io, psd_idx - 1, " ", i - 1, " ", j - 1, " ", constant)
        end
        println(io)
    end

    return
end

# ==============================================================================
#
#   MOI.read_from_file
#
# The CBF file format (version 3) is described at
# http://cblib.zib.de/doc/format3.pdf.
# Note CBF indices start at 0.
#
# ==============================================================================

# Convert a pair of row and column indices of a symmetric matrix into a vector
# index for the row-wise lower triangle
function mat_to_vec_idx(i::Int, j::Int)
    if i < j
        return div((j - 1) * j, 2) + i
    else
        return div((i - 1) * i, 2) + j
    end
end

function cbf_to_moi_cone(cone_str::AbstractString, cone_dim::Int)
    if cone_str == "F" # Free cones (redundant but add anyway).
        return MOI.Reals(cone_dim)
    elseif cone_str == "L=" # Zero cones.
        return MOI.Zeros(cone_dim)
    elseif cone_str == "L-" # Nonpositive cones.
        return MOI.Nonpositives(cone_dim)
    elseif cone_str == "L+" # Nonnegative cones.
        return MOI.Nonnegatives(cone_dim)
    elseif cone_str == "Q" # Second-order cones.
        @assert cone_dim >= 2
        return MOI.SecondOrderCone(cone_dim)
    elseif cone_str == "QR" # Rotated second-order cones.
        @assert cone_dim >= 3
        return MOI.RotatedSecondOrderCone(cone_dim)
    else
        error("CBF cone name $cone_str is not recognized or supported.")
    end
end

function powcone_to_moi_cone(
    cone_str::AbstractString,
    powcone_alphas::Vector{Vector{Float64}},
    dpowcone_alphas::Vector{Vector{Float64}},
)
    raw_powcone_info = split(cone_str[2:end], ":")
    powcone_idx = parse(Int, raw_powcone_info[1]) + 1
    powcone_type = raw_powcone_info[2]
    if powcone_type == "POW"
        alpha = powcone_alphas[powcone_idx]
        return MOI.PowerCone{Float64}(first(alpha) / sum(alpha))
    elseif powcone_type == "POW*"
        alpha = dpowcone_alphas[powcone_idx]
        return MOI.DualPowerCone{Float64}(first(alpha) / sum(alpha))
    else
        error("Failed to parse parametric cone $powcone_type information.")
    end
end

"""
    Base.read!(io::IO, model::FileFormats.CBF.Model)

Read `io` in the Conic Benchmark Format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end

    scalar_vars = MOI.VariableIndex[]
    psd_vars = Vector{MOI.VariableIndex}[]
    obj_terms = MOI.ScalarAffineTerm{Float64}[]
    obj_constant = 0.0
    powcone_alphas = Vector{Float64}[]
    dpowcone_alphas = Vector{Float64}[]
    con_cones = Tuple{String,Int}[]
    row_terms = Vector{MOI.ScalarAffineTerm{Float64}}[]
    row_constants = Float64[]
    psd_side_dims = Int[]
    psd_row_starts = Int[]
    psd_row_terms = Vector{MOI.ScalarAffineTerm{Float64}}[]
    psd_row_constants = Float64[]

    while !eof(io)
        line = strip(readline(io))

        # Skip blank lines and comments (CBF comments start with #).
        if isempty(line) || startswith(line, "#")
            continue
        end

        # CBF version number.
        if line == "VER"
            ver = parse(Int, split(strip(readline(io)))[1])
            if ver < 1 || ver > 3
                error("CBF version number $ver is not yet supported.")
            end
            continue
        end

        # Power cone parameters.
        if line == "POWCONES" || line == "POW*CONES"
            alpha = (line == "POWCONES") ? powcone_alphas : dpowcone_alphas
            raw_powcone_info = split(strip(readline(io)))
            @assert length(raw_powcone_info) == 2
            num_powcone = parse(Int, raw_powcone_info[1])
            num_lines = parse(Int, raw_powcone_info[2])
            alpha_idx = 0
            for j in 1:num_powcone
                num_alphas = parse(Int, strip(readline(io)))
                if num_alphas != 2
                    error("Only 3-dimensional power cones are supported.")
                end
                push!(
                    alpha,
                    [parse(Float64, strip(readline(io))) for k in 1:num_alphas],
                )
                alpha_idx += num_alphas
            end
            @assert num_lines == alpha_idx
            continue
        end

        # Objective sense.
        if line == "OBJSENSE"
            obj_sense = strip(readline(io))
            if obj_sense == "MIN"
                MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
            elseif obj_sense == "MAX"
                MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
            else
                error("Objective sense $obj_sense is not supported.")
            end
            continue
        end

        # Non-PSD variable constraints.
        if line == "VAR"
            raw_var_info = split(strip(readline(io)))
            @assert length(raw_var_info) == 2
            num_var = parse(Int, raw_var_info[1])
            num_lines = parse(Int, raw_var_info[2])
            append!(scalar_vars, MOI.add_variables(model, num_var))
            var_idx = 0
            for k in 1:num_lines
                raw_cone_info = split(strip(readline(io)))
                @assert length(raw_cone_info) == 2
                cone_str = raw_cone_info[1]
                cone_dim = parse(Int, raw_cone_info[2])
                if cone_str == "F" # Free cones (no constraint).
                    var_idx += cone_dim
                    continue
                end
                if cone_str in ("EXP", "EXP*") # Exponential cones.
                    # Reverse order of indices.
                    @assert cone_dim == 3
                    con_func = MOI.VectorOfVariables(
                        scalar_vars[[var_idx + 3, var_idx + 2, var_idx + 1]],
                    )
                    con_set =
                        (cone_str == "EXP") ? MOI.ExponentialCone() :
                        MOI.DualExponentialCone()
                else
                    con_func = MOI.VectorOfVariables(
                        scalar_vars[(var_idx+1):(var_idx+cone_dim)],
                    )
                    if startswith(cone_str, "@") # Power cones (parametric).
                        @assert cone_dim == 3
                        con_set = powcone_to_moi_cone(
                            cone_str,
                            powcone_alphas,
                            dpowcone_alphas,
                        )
                    else
                        con_set = cbf_to_moi_cone(cone_str, cone_dim)
                    end
                end
                MOI.add_constraint(model, con_func, con_set)
                var_idx += cone_dim
            end
            @assert var_idx == num_var
            continue
        end

        # Integrality constraints.
        if line == "INT"
            for k in 1:parse(Int, strip(readline(io)))
                var_idx = parse(Int, strip(readline(io))) + 1
                MOI.add_constraint(
                    model,
                    MOI.SingleVariable(scalar_vars[var_idx]),
                    MOI.Integer(),
                )
            end
            continue
        end

        # PSD variable constraints.
        if line == "PSDVAR"
            for k in 1:parse(Int, strip(readline(io)))
                side_dim = parse(Int, strip(readline(io)))
                cone_dim = div(side_dim * (side_dim + 1), 2)
                psd_vars_k = MOI.add_variables(model, cone_dim)
                push!(psd_vars, psd_vars_k)
                MOI.add_constraint(
                    model,
                    MOI.VectorOfVariables(psd_vars_k),
                    MOI.PositiveSemidefiniteConeTriangle(side_dim),
                )
            end
            continue
        end

        # Objective function terms.
        if line == "OBJFCOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 4
                (psd_var_idx, i, j) =
                    (parse(Int, raw_coord[i]) + 1 for i in 1:3)
                coef = parse(Float64, raw_coord[end])
                if i != j
                    coef += coef # scale off-diagonals
                end
                push!(
                    obj_terms,
                    MOI.ScalarAffineTerm{Float64}(
                        coef,
                        psd_vars[psd_var_idx][mat_to_vec_idx(i, j)],
                    ),
                )
            end
            continue
        end

        if line == "OBJACOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 2
                var_idx = parse(Int, raw_coord[1]) + 1
                coef = parse(Float64, raw_coord[end])
                push!(
                    obj_terms,
                    MOI.ScalarAffineTerm{Float64}(coef, scalar_vars[var_idx]),
                )
            end
            continue
        end

        if line == "OBJBCOORD"
            obj_constant += parse(Float64, strip(readline(io)))
            continue
        end

        # Non-PSD constraints.
        if line == "CON"
            raw_con_info = split(strip(readline(io)))
            @assert length(raw_con_info) == 2
            num_rows = parse(Int, raw_con_info[1])
            num_lines = parse(Int, raw_con_info[2])
            row_idx = 0
            for k in 1:num_lines
                raw_cone_info = split(strip(readline(io)))
                @assert length(raw_cone_info) == 2
                cone_str = raw_cone_info[1]
                cone_dim = parse(Int, raw_cone_info[2])
                push!(con_cones, (cone_str, cone_dim))
                row_idx += cone_dim
            end
            @assert row_idx == num_rows
            append!(
                row_terms,
                Vector{MOI.ScalarAffineTerm{Float64}}() for k in 1:num_rows
            )
            append!(row_constants, zeros(num_rows))
            continue
        end

        if line == "FCOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 5
                (row_idx, psd_var_idx, i, j) =
                    (parse(Int, raw_coord[i]) + 1 for i in 1:4)
                coef = parse(Float64, raw_coord[end])
                if i != j
                    coef += coef # scale off-diagonals
                end
                push!(
                    row_terms[row_idx],
                    MOI.ScalarAffineTerm{Float64}(
                        coef,
                        psd_vars[psd_var_idx][mat_to_vec_idx(i, j)],
                    ),
                )
            end
            continue
        end

        if line == "ACOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 3
                (row_idx, var_idx) = (parse(Int, raw_coord[i]) + 1 for i in 1:2)
                coef = parse(Float64, raw_coord[end])
                push!(
                    row_terms[row_idx],
                    MOI.ScalarAffineTerm{Float64}(coef, scalar_vars[var_idx]),
                )
            end
            continue
        end

        if line == "BCOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 2
                row_idx = parse(Int, raw_coord[1]) + 1
                row_constants[row_idx] = parse(Float64, raw_coord[end])
            end
            continue
        end

        # PSD constraints.
        if line == "PSDCON"
            idx = 0
            for k in 1:parse(Int, strip(readline(io)))
                side_dim = parse(Int, strip(readline(io)))
                push!(psd_side_dims, side_dim)
                push!(psd_row_starts, idx)
                idx += div(side_dim * (side_dim + 1), 2)
            end
            append!(
                psd_row_terms,
                Vector{MOI.ScalarAffineTerm{Float64}}() for i in 1:idx
            )
            append!(psd_row_constants, zeros(idx))
            continue
        end

        if line == "HCOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 5
                (psd_idx, var_idx, i, j) =
                    (parse(Int, raw_coord[i]) + 1 for i in 1:4)
                coef = parse(Float64, raw_coord[end])
                row_idx = psd_row_starts[psd_idx] + mat_to_vec_idx(i, j)
                push!(
                    psd_row_terms[row_idx],
                    MOI.ScalarAffineTerm{Float64}(coef, scalar_vars[var_idx]),
                )
            end
            continue
        end

        if line == "DCOORD"
            for k in 1:parse(Int, strip(readline(io)))
                raw_coord = split(strip(readline(io)))
                @assert length(raw_coord) == 4
                (psd_idx, i, j) = (parse(Int, raw_coord[i]) + 1 for i in 1:3)
                row_idx = psd_row_starts[psd_idx] + mat_to_vec_idx(i, j)
                psd_row_constants[row_idx] += parse(Float64, raw_coord[end])
            end
            continue
        end

        error("Failed to parse CBF file due to corrupted line: $line")
    end

    # Objective function.
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(obj_terms, obj_constant),
    )

    # Non-PSD constraints.
    row_idx = 0
    for (cone_str, cone_dim) in con_cones
        if cone_str in ("EXP", "EXP*") # Exponential cones.
            # Reverse order of indices.
            @assert cone_dim == 3
            con_func = MOI.VectorAffineFunction(
                [
                    MOI.VectorAffineTerm{Float64}(4 - l, t) for l in 1:cone_dim for t in row_terms[row_idx+l]
                ],
                row_constants[[row_idx + 3, row_idx + 2, row_idx + 1]],
            )
            con_set =
                (cone_str == "EXP") ? MOI.ExponentialCone() :
                MOI.DualExponentialCone()
        else
            con_func = MOI.VectorAffineFunction(
                [
                    MOI.VectorAffineTerm{Float64}(l, t) for l in 1:cone_dim
                    for t in row_terms[row_idx+l]
                ],
                row_constants[(row_idx+1):(row_idx+cone_dim)],
            )
            if startswith(cone_str, "@") # Power cones (parametric).
                @assert cone_dim == 3
                con_set = powcone_to_moi_cone(
                    cone_str,
                    powcone_alphas,
                    dpowcone_alphas,
                )
            else
                con_set = cbf_to_moi_cone(cone_str, cone_dim)
            end
        end
        MOI.add_constraint(model, con_func, con_set)
        row_idx += cone_dim
    end

    # PSD constraints.
    for psd_idx in eachindex(psd_side_dims)
        row_start = psd_row_starts[psd_idx]
        side_dim = psd_side_dims[psd_idx]
        cone_dim = div(side_dim * (side_dim + 1), 2)
        con_func = MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm{Float64}(l, t) for l in 1:cone_dim for
                t in psd_row_terms[row_start+l]
            ],
            psd_row_constants[(row_start+1):(row_start+cone_dim)],
        )
        MOI.add_constraint(
            model,
            con_func,
            MOI.PositiveSemidefiniteConeTriangle(side_dim),
        )
    end

    return
end

end
