_parse(::Type{T}, x::AbstractString) where {T} = parse(T, x)
_parse(::Type{String}, x::AbstractString) = String(x)

_read(io::IO, ::Type{T}) where {T} = _parse(T, strip(readline(io)))

function _read(io::IO, args...)
    x = split(strip(readline(io)))
    @assert length(x) == length(args)
    return _parse.(args, x)
end

mutable struct _CBFReadData
    scalar_vars::Vector{MOI.VariableIndex}
    psd_vars::Vector{Vector{MOI.VariableIndex}}
    psd_side_dims::Vector{Int}
    psd_row_starts::Vector{Int}
    psd_row_terms::Vector{Vector{MOI.ScalarAffineTerm{Float64}}}
    psd_row_constants::Vector{Float64}
    obj_terms::Vector{MOI.ScalarAffineTerm{Float64}}
    obj_constant::Float64
    power_cone_alpha::Vector{Vector{Float64}}
    dual_power_cone_alpha::Vector{Vector{Float64}}
    con_cones::Vector{Tuple{String,Int}}
    row_terms::Vector{Vector{MOI.ScalarAffineTerm{Float64}}}
    row_constants::Vector{Float64}

    function _CBFReadData()
        return new(
            MOI.VariableIndex[],
            Vector{MOI.VariableIndex}[],
            Int[],
            Int[],
            Vector{MOI.ScalarAffineTerm{Float64}}[],
            Float64[],
            MOI.ScalarAffineTerm{Float64}[],
            0.0,
            Vector{Float64}[],
            Vector{Float64}[],
            Tuple{String,Int}[],
            Vector{MOI.ScalarAffineTerm{Float64}}[],
            Float64[],
        )
    end
end

# Convert a pair of row and column indices of a symmetric matrix into a vector
# index for the row-wise lower triangle
function _mat_to_vec_idx(i::Int, j::Int)
    if i < j
        return div((j - 1) * j, 2) + i
    else
        return div((i - 1) * i, 2) + j
    end
end

function _cbf_to_moi_cone(
    data::_CBFReadData,
    cone_str::AbstractString,
    cone_dim::Int,
)
    if cone_str == "F"
        return MOI.Reals(cone_dim)
    elseif cone_str == "L="
        return MOI.ZeroCone(cone_dim)
    elseif cone_str == "L-"
        return MOI.NonpositiveCone(cone_dim)
    elseif cone_str == "L+"
        return MOI.NonnegativeCone(cone_dim)
    elseif cone_str == "Q"
        @assert cone_dim >= 2
        return MOI.SecondOrderCone(cone_dim)
    elseif cone_str == "QR"
        @assert cone_dim >= 3
        return MOI.RotatedSecondOrderCone(cone_dim)
    elseif cone_str == "EXP"
        return MOI.ExponentialCone()
    elseif cone_str == "EXP*"
        return MOI.DualExponentialCone()
    elseif startswith(cone_str, "@")
        raw_powcone_info = split(cone_str[2:end], ":")
        powcone_idx = parse(Int, raw_powcone_info[1]) + 1
        if raw_powcone_info[2] == "POW"
            alpha = data.power_cone_alpha[powcone_idx]
            return MOI.PowerCone{Float64}(first(alpha) / sum(alpha))
        elseif raw_powcone_info[2] == "POW*"
            alpha = data.dual_power_cone_alpha[powcone_idx]
            return MOI.DualPowerCone{Float64}(first(alpha) / sum(alpha))
        end
    end
    return error("CBF cone name $cone_str is not recognized or supported.")
end

function _read_VER(io::IO)
    if !(1 <= _read(io, Int) <= 3)
        error("CBF version number $ver is not yet supported.")
    end
    return
end

function _read_POWCONES(io::IO, model::Model, alpha::Vector{Vector{Float64}})
    num_powcone, num_lines = _read(io, Int, Int)
    alpha_idx = 0
    for _ in 1:num_powcone
        num_alpha = _read(io, Int)
        if num_alpha != 2
            error("Only 3-dimensional power cones are supported.")
        end
        push!(alpha, [_read(io, Float64) for _ in 1:num_alpha])
        alpha_idx += num_alpha
    end
    @assert num_lines == alpha_idx
    return
end

function _read_OBJSENSE(io::IO, model::Model)
    obj_sense = strip(readline(io))
    if obj_sense == "MIN"
        MOI.set(model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
    else
        @assert obj_sense == "MAX"
        MOI.set(model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
    end
    return
end

function _read_PSDVAR(io::IO, model::Model, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        side_dim = _read(io, Int)
        cone_dim = div(side_dim * (side_dim + 1), 2)
        psd_vars_k = MOI.add_variables(model, cone_dim)
        push!(data.psd_vars, psd_vars_k)
        MOI.add_constraint(
            model,
            MOI.VectorOfVariables(psd_vars_k),
            MOI.PositiveSemidefiniteConeTriangle(side_dim),
        )
    end
    return
end

function _read_VAR(io::IO, model::Model, data::_CBFReadData)
    num_var, num_lines = _read(io, Int, Int)
    append!(data.scalar_vars, MOI.add_variables(model, num_var))
    var_idx = 0
    for _ in 1:num_lines
        cone_str, cone_dim = _read(io, String, Int)
        if cone_str == "F"
            var_idx += cone_dim
            continue # Free cones (no constraint).
        end
        indices = 1:cone_dim
        if cone_str == "EXP" || cone_str == "EXP*"
            indices = (3:-1:1)
        end
        MOI.add_constraint(
            model,
            MOI.VectorOfVariables(data.scalar_vars[var_idx.+indices]),
            _cbf_to_moi_cone(data, cone_str, cone_dim),
        )
        var_idx += cone_dim
    end
    @assert var_idx == num_var
    return
end

function _read_INT(io::IO, model::Model, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        MOI.add_constraint(
            model,
            MOI.SingleVariable(data.scalar_vars[_read(io, Int)+1]),
            MOI.Integer(),
        )
    end
    return
end

function _read_PSDCON(io::IO, data::_CBFReadData)
    idx = 0
    for _ in 1:_read(io, Int)
        side_dim = _read(io, Int)
        push!(data.psd_side_dims, side_dim)
        push!(data.psd_row_starts, idx)
        idx += div(side_dim * (side_dim + 1), 2)
    end
    for _ in 1:idx
        push!(data.psd_row_terms, MOI.ScalarAffineTerm{Float64}[])
        push!(data.psd_row_constants, 0.0)
    end
    return
end

function _read_CON(io::IO, data::_CBFReadData)
    num_rows, num_lines = _read(io, Int, Int)
    row_idx = 0
    for _ in 1:num_lines
        cone_str, cone_dim = _read(io, String, Int)
        push!(data.con_cones, (cone_str, cone_dim))
        row_idx += cone_dim
    end
    for _ in 1:num_rows
        push!(data.row_terms, MOI.ScalarAffineTerm{Float64}[])
        push!(data.row_constants, 0.0)
    end
    return
end

function _read_OBJFCOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        psd_var_idx, i, j, coef = _read(io, Int, Int, Int, Float64)
        if i != j
            coef += coef # scale off-diagonals
        end
        push!(
            data.obj_terms,
            MOI.ScalarAffineTerm{Float64}(
                coef,
                data.psd_vars[psd_var_idx+1][_mat_to_vec_idx(i + 1, j + 1)],
            ),
        )
    end
    return
end

function _read_OBJACOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        var_idx, coef = _read(io, Int, Float64)
        push!(
            data.obj_terms,
            MOI.ScalarAffineTerm{Float64}(coef, data.scalar_vars[var_idx+1]),
        )
    end
    return
end

function _read_OBJBCOORD(io::IO, data::_CBFReadData)
    data.obj_constant += _read(io, Float64)
    return
end

function _read_FCOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        row_idx, psd_var_idx, i, j, coef =
            _read(io, Int, Int, Int, Int, Float64)
        if i != j
            coef += coef # scale off-diagonals
        end
        push!(
            data.row_terms[row_idx+1],
            MOI.ScalarAffineTerm{Float64}(
                coef,
                data.psd_vars[psd_var_idx+1][_mat_to_vec_idx(i + 1, j + 1)],
            ),
        )
    end
    return
end

function _read_ACOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        row, col, coef = _read(io, Int, Int, Float64)
        push!(
            data.row_terms[row+1],
            MOI.ScalarAffineTerm{Float64}(coef, data.scalar_vars[col+1]),
        )
    end
    return
end

function _read_BCOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        row, coef = _read(io, Int, Float64)
        data.row_constants[row+1] = coef
    end
    return
end

function _read_HCOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        psd_idx, var_idx, i, j, coef = _read(io, Int, Int, Int, Int, Float64)
        row_idx = data.psd_row_starts[psd_idx+1] + _mat_to_vec_idx(i + 1, j + 1)
        push!(
            data.psd_row_terms[row_idx],
            MOI.ScalarAffineTerm{Float64}(coef, data.scalar_vars[var_idx+1]),
        )
    end
    return
end

function _read_DCOORD(io::IO, data::_CBFReadData)
    for _ in 1:_read(io, Int)
        psd_idx, i, j, coef = _read(io, Int, Int, Int, Float64)
        row_idx = data.psd_row_starts[psd_idx+1] + _mat_to_vec_idx(i + 1, j + 1)
        data.psd_row_constants[row_idx] += coef
    end
    return
end

"""
    Base.read!(io::IO, model::FileFormats.CBF.Model)

Read `io` in the Conic Benchmark Format and store the result in `model`.
"""
function Base.read!(io::IO, model::Model)
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    data = _CBFReadData()
    while !eof(io)
        line = strip(readline(io))
        if isempty(line) || startswith(line, "#")
            # Skip blank lines and comments.
        elseif line == "VER"
            _read_VER(io)
        elseif line == "POWCONES"
            _read_POWCONES(io, model, data.power_cone_alpha)
        elseif line == "POW*CONES"
            _read_POWCONES(io, model, data.dual_power_cone_alpha)
        elseif line == "OBJSENSE"
            _read_OBJSENSE(io, model)
        elseif line == "PSDVAR"
            _read_PSDVAR(io, model, data)
        elseif line == "VAR"
            _read_VAR(io, model, data)
        elseif line == "INT"
            _read_INT(io, model, data)
        elseif line == "PSDCON"
            _read_PSDCON(io, data)
        elseif line == "CON"
            _read_CON(io, data)
        elseif line == "OBJFCOORD"
            _read_OBJFCOORD(io, data)
        elseif line == "OBJACOORD"
            _read_OBJACOORD(io, data)
        elseif line == "OBJBCOORD"
            _read_OBJBCOORD(io, data)
        elseif line == "FCOORD"
            _read_FCOORD(io, data)
        elseif line == "ACOORD"
            _read_ACOORD(io, data)
        elseif line == "BCOORD"
            _read_BCOORD(io, data)
        elseif line == "HCOORD"
            _read_HCOORD(io, data)
        elseif line == "DCOORD"
            _read_DCOORD(io, data)
        else
            error("Failed to parse CBF file due to corrupted line: $line")
        end
    end
    MOI.set(
        model,
        MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
        MOI.ScalarAffineFunction(data.obj_terms, data.obj_constant),
    )

    # Non-PSD constraints.
    row_idx = 0
    for (cone_str, cone_dim) in data.con_cones
        con_func = if cone_str == "EXP" || cone_str == "EXP*"
            # Reverse order of indices.
            MOI.VectorAffineFunction(
                [
                    MOI.VectorAffineTerm{Float64}(4 - l, t) for l in 1:cone_dim for t in data.row_terms[row_idx+l]
                ],
                data.row_constants[row_idx.+(3:-1:1)],
            )
        else
            MOI.VectorAffineFunction(
                [
                    MOI.VectorAffineTerm{Float64}(l, t) for l in 1:cone_dim
                    for t in data.row_terms[row_idx+l]
                ],
                data.row_constants[row_idx.+(1:cone_dim)],
            )
        end
        con_set = _cbf_to_moi_cone(data, cone_str, cone_dim)
        MOI.add_constraint(model, con_func, con_set)
        row_idx += cone_dim
    end

    # PSD constraints.
    for psd_idx in eachindex(data.psd_side_dims)
        row_start = data.psd_row_starts[psd_idx]
        side_dim = data.psd_side_dims[psd_idx]
        cone_dim = div(side_dim * (side_dim + 1), 2)
        con_func = MOI.VectorAffineFunction(
            [
                MOI.VectorAffineTerm{Float64}(l, t) for l in 1:cone_dim for
                t in data.psd_row_terms[row_start+l]
            ],
            data.psd_row_constants[row_start.+(1:cone_dim)],
        )
        MOI.add_constraint(
            model,
            con_func,
            MOI.PositiveSemidefiniteConeTriangle(side_dim),
        )
    end

    return
end
