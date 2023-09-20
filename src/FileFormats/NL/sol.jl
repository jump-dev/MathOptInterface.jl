# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# We fake the supertype to aid method dispatch
struct SolFileResults <: MOI.ModelLike
    model::Union{Nothing,Model}
    raw_status_string::String
    termination_status::MOI.TerminationStatusCode
    primal_status::MOI.ResultStatusCode
    dual_status::MOI.ResultStatusCode
    objective_value::Float64
    variable_primal::Dict{MOI.VariableIndex,Float64}
    constraint_dual::Vector{Float64}
    zL_out::Dict{MOI.VariableIndex,Float64}
    zU_out::Dict{MOI.VariableIndex,Float64}
end

"""
    SolFileResults(filename::String, model::Model)

Parse the `.sol` file `filename` created by solving `model` and return a
`SolFileResults` struct.

The returned struct supports the `MOI.get` API for querying result attributes
such as [`MOI.TerminationStatus`](@ref), [`MOI.VariablePrimal`](@ref), and
[`MOI.ConstraintDual`](@ref).
"""
function SolFileResults(filename::String, model::Model)
    return open(io -> SolFileResults(io, model), filename, "r")
end

"""
    SolFileResults(
        raw_status::String,
        termination_status::MOI.TerminationStatusCode,
    )

Return a `SolFileResults` struct with [`MOI.RawStatusString`](@ref) set to
`raw_status`, [`MOI.TerminationStatus`](@ref) set to `termination_status`, and
[`MOI.PrimalStatus`](@ref) and [`MOI.DualStatus`](@ref) set to `NO_SOLUTION`.

All other attributes are un-set.
"""
function SolFileResults(
    raw_status::String,
    termination_status::MOI.TerminationStatusCode,
)
    return SolFileResults(
        nothing,
        raw_status,
        termination_status,
        MOI.NO_SOLUTION,
        MOI.NO_SOLUTION,
        NaN,
        Dict{MOI.VariableIndex,Float64}(),
        Float64[],
        Dict{MOI.VariableIndex,Float64}(),
        Dict{MOI.VariableIndex,Float64}(),
    )
end

function MOI.get(sol::SolFileResults, ::MOI.ResultCount)
    return isempty(sol.variable_primal) ? 0 : 1
end

function MOI.get(sol::SolFileResults, ::MOI.RawStatusString)
    return sol.raw_status_string
end

function MOI.get(sol::SolFileResults, ::MOI.TerminationStatus)
    return sol.termination_status
end

function MOI.get(sol::SolFileResults, attr::MOI.PrimalStatus)
    if attr.result_index != 1
        return MOI.NO_SOLUTION
    end
    return sol.primal_status
end

function MOI.get(sol::SolFileResults, attr::MOI.DualStatus)
    if attr.result_index != 1
        return MOI.NO_SOLUTION
    end
    return sol.dual_status
end

function MOI.get(sol::SolFileResults, attr::MOI.ObjectiveValue)
    MOI.check_result_index_bounds(sol, attr)
    return sol.objective_value
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.VariablePrimal,
    x::MOI.VariableIndex,
)
    MOI.check_result_index_bounds(sol, attr)
    return sol.variable_primal[x]
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintPrimal,
    ci::MOI.ConstraintIndex{<:MOI.VariableIndex},
)
    MOI.check_result_index_bounds(sol, attr)
    return sol.variable_primal[MOI.VariableIndex(ci.value)]
end

# Helper function to assert that the model is not nothing
_model(sol::SolFileResults) = sol.model::Model

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintPrimal,
    ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction},
)
    MOI.check_result_index_bounds(sol, attr)
    return _evaluate(_model(sol).h[ci.value].expr, sol.variable_primal)
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintPrimal,
    ci::MOI.ConstraintIndex{F},
) where {F<:Union{MOI.ScalarQuadraticFunction,MOI.ScalarNonlinearFunction}}
    MOI.check_result_index_bounds(sol, attr)
    return _evaluate(_model(sol).g[ci.value].expr, sol.variable_primal)
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.LessThan{Float64}},
)
    MOI.check_result_index_bounds(sol, attr)
    dual = get(sol.zU_out, MOI.VariableIndex(ci.value), 0.0)
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.GreaterThan{Float64}},
)
    MOI.check_result_index_bounds(sol, attr)
    dual = get(sol.zL_out, MOI.VariableIndex(ci.value), 0.0)
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.EqualTo{Float64}},
)
    MOI.check_result_index_bounds(sol, attr)
    x = MOI.VariableIndex(ci.value)
    dual = get(sol.zL_out, x, 0.0) + get(sol.zU_out, x, 0.0)
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{MOI.VariableIndex,MOI.Interval{Float64}},
)
    MOI.check_result_index_bounds(sol, attr)
    x = MOI.VariableIndex(ci.value)
    dual = get(sol.zL_out, x, 0.0) + get(sol.zU_out, x, 0.0)
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{<:MOI.ScalarAffineFunction},
)
    MOI.check_result_index_bounds(sol, attr)
    dual = sol.constraint_dual[length(_model(sol).g)+ci.value]
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

function MOI.get(
    sol::SolFileResults,
    attr::MOI.ConstraintDual,
    ci::MOI.ConstraintIndex{F},
) where {F<:Union{MOI.ScalarQuadraticFunction,MOI.ScalarNonlinearFunction}}
    MOI.check_result_index_bounds(sol, attr)
    dual = sol.constraint_dual[ci.value]
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

function MOI.get(sol::SolFileResults, attr::MOI.NLPBlockDual)
    MOI.check_result_index_bounds(sol, attr)
    dual = sol.constraint_dual[1:_model(sol).nlpblock_dim]
    return _model(sol).sense == MOI.MIN_SENSE ? dual : -dual
end

"""
    _interpret_status(solve_result_num::Int, raw_status_string::String)

Convert the `solve_result_num` and `raw_status_string` into MOI-type statuses.

For the primal status, assume a solution is present. Other code is responsible
for returning `MOI.NO_SOLUTION` if no primal solution is present.
"""
function _interpret_status(solve_result_num::Int, raw_status_string::String)
    message = lowercase(raw_status_string)
    # Handle some status strings that are, in most cases, solver-specific.
    if occursin("time limit, feasible solution", message)
        return MOI.TIME_LIMIT, MOI.FEASIBLE_POINT
    end
    # No handle the solve status codes. These are solver-specific, although they
    # should be grouped into XYZ codes, where the first digit is a generic
    # group (optimal, infeasible, limit, etc), and the YZ are solver-specific
    # codes with more meaning.
    if 0 <= solve_result_num < 100
        # Solved, and nothing went wrong. Even though we say `LOCALLY_SOLVED`,
        # some solvers like SHOT use this status to represent problems that are
        # provably globally optimal.
        return MOI.LOCALLY_SOLVED, MOI.FEASIBLE_POINT
    elseif 100 <= solve_result_num < 200
        # Solved, but the solver can't be sure for some reason. e.g., SHOT
        # uses this for non-convex problems it isn't sure is the global optima.
        return MOI.LOCALLY_SOLVED, MOI.FEASIBLE_POINT
    elseif 200 <= solve_result_num < 300
        return MOI.LOCALLY_INFEASIBLE, MOI.UNKNOWN_RESULT_STATUS
    elseif 300 <= solve_result_num < 400
        return MOI.DUAL_INFEASIBLE, MOI.UNKNOWN_RESULT_STATUS
    elseif 400 <= solve_result_num < 500
        return MOI.OTHER_LIMIT, MOI.UNKNOWN_RESULT_STATUS
    elseif 500 <= solve_result_num < 600
        return MOI.OTHER_ERROR, MOI.UNKNOWN_RESULT_STATUS
    end
    # If all else fails, attempt to infer the status from `raw_status_string`.
    if occursin("optimal", message)
        return MOI.LOCALLY_SOLVED, MOI.FEASIBLE_POINT
    elseif occursin("infeasible", message)
        return MOI.LOCALLY_INFEASIBLE, MOI.UNKNOWN_RESULT_STATUS
    elseif occursin("unbounded", message)
        return MOI.DUAL_INFEASIBLE, MOI.UNKNOWN_RESULT_STATUS
    elseif occursin("limit", message)
        return MOI.OTHER_LIMIT, MOI.UNKNOWN_RESULT_STATUS
    elseif occursin("error", message)
        return MOI.OTHER_ERROR, MOI.UNKNOWN_RESULT_STATUS
    else
        return MOI.OTHER_ERROR, MOI.UNKNOWN_RESULT_STATUS
    end
end

function _readline(io::IO)
    if eof(io)
        error("Reached end of sol file unexpectedly.")
    end
    return strip(readline(io))
end

_readline(io::IO, T) = parse(T, _readline(io))

function SolFileResults(io::IO, model::Model)
    # This function is based on a Julia translation of readsol.c, available at
    # https://github.com/ampl/asl/blob/64919f75fa7a438f4b41bce892dcbe2ae38343ee/src/solvers/readsol.c
    # and under the following license:
    #
    # Copyright (C) 2017 AMPL Optimization, Inc.; written by David M. Gay.
    # Permission to use, copy, modify, and distribute this software and its
    # documentation for any purpose and without fee is hereby granted,
    # provided that the above copyright notice appear in all copies and that
    # both that the copyright notice and this permission notice and warranty
    # disclaimer appear in supporting documentation.
    #
    # The author and AMPL Optimization, Inc. disclaim all warranties with
    # regard to this software, including all implied warranties of
    # merchantability and fitness.  In no event shall the author be liable
    # for any special, indirect or consequential damages or any damages
    # whatsoever resulting from loss of use, data or profits, whether in an
    # action of contract, negligence or other tortious action, arising out
    # of or in connection with the use or performance of this software.
    raw_status_string = ""
    line = ""
    while !startswith(line, "Options")
        raw_status_string *= line
        line = _readline(io)
    end
    # Read through all the options. Direct copy of reference implementation.
    @assert startswith(line, "Options")
    num_options = _readline(io, Int)
    need_vbtol = false
    if num_options > 0
        @assert 3 <= num_options <= 9
        _readline(io, Int)  # Skip this line
        if _readline(io, Int) == 3
            num_options -= 2
            need_vbtol = true
        end
        for _ in 3:num_options
            _readline(io, Int)  # Skip the rest of the option lines
        end
    end
    # Read number of constraints
    num_cons = _readline(io, Int)
    @assert(num_cons == length(model.g) + length(model.h))
    # Read number of dual solutions to read in
    num_duals_to_read = _readline(io, Int)
    @assert(num_duals_to_read == 0 || num_duals_to_read == num_cons)
    # Read number of variables
    num_vars = _readline(io, Int)
    @assert(num_vars == length(model.x))
    # Read number of primal solutions to read in
    num_vars_to_read = _readline(io, Int)
    @assert(num_vars_to_read == 0 || num_vars_to_read == num_vars)
    # Skip over vbtol line if present
    if need_vbtol
        _readline(io)
    end
    # Read dual solutions
    constraint_dual =
        Float64[_readline(io, Float64) for _ in 1:num_duals_to_read]
    # Read primal solutions
    variable_primal = Dict{MOI.VariableIndex,Float64}()
    if num_vars_to_read > 0
        for xi in model.order
            variable_primal[xi] = _readline(io, Float64)
        end
    end
    # Check for status code
    solve_result_num = -1
    while !eof(io)
        linevals = split(_readline(io), " ")
        if length(linevals) > 0 && linevals[1] == "objno"
            @assert parse(Int, linevals[2]) == 0
            solve_result_num = parse(Int, linevals[3])
            break
        end
    end
    zL_out = Dict{MOI.VariableIndex,Float64}()
    zU_out = Dict{MOI.VariableIndex,Float64}()
    while !eof(io)
        line = _readline(io)
        if startswith(line, "suffix")
            items = split(line, " ")
            n_suffix = parse(Int, items[3])
            suffix = _readline(io)
            if !(suffix == "ipopt_zU_out" || suffix == "ipopt_zL_out")
                for _ in 1:n_suffix
                    _ = readline(io)
                end
                continue
            end
            for i in 1:n_suffix
                items = split(_readline(io), " ")
                x = model.order[parse(Int, items[1])+1]
                dual = parse(Float64, items[2])
                if suffix == "ipopt_zU_out"
                    zU_out[x] = dual
                else
                    @assert suffix == "ipopt_zL_out"
                    zL_out[x] = dual
                end
            end
        end
    end
    termination_status, primal_status =
        _interpret_status(solve_result_num, raw_status_string)
    objective_value = NaN
    if length(variable_primal) > 0
        # .sol files don't seem to be able to return the objective
        # value. Evaluate it here instead.
        objective_value = _evaluate(model.f, variable_primal)
    end
    n_duals = length(constraint_dual) + length(zL_out) + length(zU_out)
    dual_status = if n_duals == 0 || termination_status != MOI.LOCALLY_SOLVED
        MOI.NO_SOLUTION
    else
        MOI.FEASIBLE_POINT
    end
    return SolFileResults(
        model,
        raw_status_string,
        termination_status,
        length(variable_primal) > 0 ? primal_status : MOI.NO_SOLUTION,
        dual_status,
        objective_value,
        variable_primal,
        constraint_dual,
        zL_out,
        zU_out,
    )
end
