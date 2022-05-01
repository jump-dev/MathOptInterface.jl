# TODO: introduce a way for solvers to indicate that they support some features. For now, only the barest FZN file is generated, with extremely little structure.

# A few generic helpers for parsing the FZN output.

mutable struct _FznResults
    raw_status_string::String
    termination_status::MOI.TerminationStatusCode
    primal_status::MOI.ResultStatusCode
    objective_value::Real # TODO: implement this. Compute it from the solutions? Fzn doesn't really output it (unless the FZN file is tweaked).
    primal_solutions::Vector{Dict{MOI.VariableIndex, Real}} # Solution ID -> (variable index -> value)
end

function _FznResults()
    return _FznResults(
        "Optimize not called.",
        MOI.OPTIMIZE_NOT_CALLED,
        MOI.NO_SOLUTION,
        NaN,
        Dict{MOI.VariableIndex, Float64}[],
    )
end

function _parse_fzn_value(str::AbstractString)
    # Heuristically guess the type of the output value: either integer or 
    # float.
    if '.' in str
        return parse(Float64, str)
    else
        return parse(Int, str)
    end
end

"""
    _parse_to_assignments(str::String)::Vector{Dict{String, Vector{Number}}}

Parses the output of a FlatZinc-compatible solver into a list of dictionaries
mapping the name of the variables to their values (either a scalar or a vector
of numbers). The values are automatically transformed into the closest type
(integer or float).
"""
function _parse_to_assignments(str::String)::Vector{Dict{String, Vector{Number}}}
    results = Dict{String, Vector{Number}}[]

    # If the infeasibility marker is discovered, return an empty list.
    if occursin("=====UNSATISFIABLE=====", str)
        return results
    end

    # Remove comments from the output (starting with % and ending with a new line).
    str = replace(str, r"%(.*)(\\r|\\n|\\r\\n)" => s"")

    # There may be several results returned by the solver. Each solution is 
    # separated from the others by `'-' ^ 10`.
    str_split = split(str, '-' ^ 10)[1:(end - 1)]
    n_results = length(str_split)
    sizehint!(results, n_results)

    for i in 1:n_results
        push!(results, Dict{String, Vector{Number}}())

        # Each value is indicated in its own statement, separated by a 
        # semi-colon.
        for part in split(strip(str_split[i]), ';')
            if isempty(part)
                continue
            end

            var, val = split(part, '=')
            var = strip(var)
            val = strip(val)

            # Either an array or a scalar. Always return an array for 
            # simplicity. A scalar is simply an array with one element.
            if !occursin("array", val)
                # Scalar. Just a value: "1", "1.0".
                results[i][var] = [_parse_fzn_value(val)]
            else
                # Array. Several arguments: "array1d(1..2, [1, 2])", 
                # "array2d(1..2, 1..2, [1, 2, 3, 4])". 
                # TODO: should dimensions be preserved? (First argument[s] of arrayNd.)
                val = split(split(val, '[')[2], ']')[1]
                results[i][var] = map(_parse_fzn_value, map(strip, split(val, ',')))
            end
        end
    end

    return results
end

# MOI wrapper.
# Based on AmplNLWriter.jl's _NLResults and Optimizer. 
# The main difference is that typical solutions do not have a Float64 type,
# but rather Int. However, it all depends on the actual FZN solver that is
# used below (some of them can still deal with floats).

mutable struct Optimizer <: MOI.AbstractOptimizer
    # Solver to call and options.
    inner::CP.FlatZinc.Model
    solver_command::Cmd
    time_limit_ms::Float64
    verboseness::Bool # True: verbose (default).
    options::Vector{String} # Custom-set options.

    # Results once solver called.
    results::_FznResults
    solve_time::Float64 # For the call to optimize!
    fzn_time::Float64 # For writing the FZN file
    parse_time::Float64 # For reading the FZN solution
end

"""
    Optimizer(
        solver_command::Union{String, Function},
        solver_args::Vector{String},
    )

Create a new FlatZinc-backed Optimizer object. `solver_command` is the path
to the FlatZinc-compatible CLI of the solver or a `Cmd` object to call the
solver (typically, this is useful to set environment variables).

`solver_args` is a vector of `String` arguments passed solver executable.
However, prefer passing `key=value` options via `MOI.RawOptimizerAttribute`. 
These arguments are passed to `Base.pipeline`. 
[See the Julia documentation for more details](https://docs.julialang.org/en/v1/base/base/#Base.pipeline-Tuple{Base.AbstractCmd}).

## Examples

A string to an executable, no required argument:

```julia
Optimizer("/path/to/fzn.exe")

A `Cmd` object with an environment variable:

```julia
Optimizer(Cmd(`/path/to/fzn.exe`, env=["PATH=/usr/bin"]))
```
"""
function Optimizer(
    solver_command::Union{String, Cmd}="",
    solver_args::Vector{String}=String[],
)
    return Optimizer(
        CP.FlatZinc.Model(),
        `$(solver_command)`,
        0.0,
        true,
        solver_args,

        _FznResults(),
        NaN,
        NaN,
        NaN,
    )
end

Base.show(io::IO, ::Optimizer) = print(io, "A FlatZinc (flattened MiniZinc) model")

MOI.get(model::Optimizer, ::MOI.SolverName) = "FlatZincWriter"

function MOI.supports(model::Optimizer, attr::MOI.AnyAttribute, x...) 
    return MOI.supports(model.inner, attr, x...)::Bool
end

function MOI.supports_add_constrained_variable(model::Optimizer, x::Type{S}) where {S <: MOI.AbstractScalarSet}
    return MOI.supports_add_constrained_variable(model.inner, x)::Bool
end

function MOI.supports_add_constrained_variables(model::Optimizer, x::Type{S}) where {S <: MOI.AbstractScalarSet}
    return MOI.supports_add_constrained_variables(model.inner, x)::Bool
end

function MOI.supports_constraint(model::Optimizer, f::Type{F}, s::Type{S}) where {F <: MOI.AbstractFunction, S <: MOI.AbstractSet}
    return MOI.supports_constraint(model.inner, f, s)::Bool
end

function MOI.get(model::Optimizer, attr::MOI.AnyAttribute, x...) 
    return MOI.get(model.inner, attr, x...)
end

function MOI.get(model::Optimizer, ::Type{MOI.VariableIndex}, name::String)
    return MOI.get(model.inner, MOI.VariableIndex, name)
end

function MOI.set(model::Optimizer, attr::MOI.AnyAttribute, x...) 
    MOI.set(model.inner, attr, x...)
    return
end

function MOI.add_variable(model::Optimizer) 
    return MOI.add_variable(model.inner)
end

function MOI.add_constrained_variable(model::Optimizer, x::MOI.AbstractScalarSet) 
    return MOI.add_constrained_variable(model.inner, x)
end

function MOI.add_constraint(model::Optimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet) 
    return MOI.add_constraint(model.inner, f, s)
end

function MOI.copy_to(dest::Optimizer, src::MOI.ModelLike)
    return MOIU.default_copy_to(dest, src)
end

function MOI.supports_incremental_interface(::Optimizer)
    return true
end

function MOI.empty!(model::Optimizer)
    MOI.empty!(model.inner)
    # Only two attributes to empty, the other ones link the actual solver.
    model.results = _FznResults()
    model.solve_time = NaN
    return
end

MOI.is_empty(model::Optimizer) = MOI.is_empty(model.inner)

function MOI.optimize!(model::Optimizer)
    start_time = time()

    # Generate the FZN file.
    temp_dir = mktempdir()
    fzn_file = joinpath(temp_dir, "model.fzn")
    open(io -> write(io, model.inner), fzn_file, "w")
    
    model.fzn_time = time() - start_time

    # Generate the list of options. Always put the user-defined options at 
    # the end.
    opts = copy(model.options)
    if !iszero(model.time_limit_ms)
        prepend!(opts, ["-t $(model.time_limit_ms)"])
    end
    if model.verboseness
        # Interpret as statistics to stdout and log to stderr.
        prepend!(opts, ["-s", "-v"])
    end

    # Call the FZN solver and gather the results in a string.
    try
        io = IOBuffer()
        ret = run(
            pipeline(
                `$(model.solver_command) $(opts) $(fzn_file)`,
                stdout=io,
            ),
            wait=true
        )

        if ret.exitcode != 0
            error("Nonzero exit code: $(ret.exitcode)")
        end

        start_parse_time = time()

        seekstart(io)
        sols_str = String(take!(io))
        sols_parsed = _parse_to_assignments(sols_str)

        model.parse_time = start_parse_time - time()

        _parse_to_moi_solutions(sols_parsed, model)
    catch err
        model.results = _FznResults(
            "Error calling the solver. Failed with: $(err)",
            MOI.OTHER_ERROR,
            MOI.NO_SOLUTION,
            NaN,
            Dict{MOI.VariableIndex, Float64}[],
        )
        # throw(err)
    end

    # Compute the total time of solving the MOI model.
    model.solve_time = time() - start_time
    return
end

function MOI.get(model::Optimizer, ::MOI.TerminationStatus)
    return model.results.termination_status
end

function MOI.get(model::Optimizer, attr::MOI.VariablePrimal, vi::MOI.VariableIndex)
    if length(model.results.primal_solutions) >= attr.result_index
        return model.results.primal_solutions[attr.result_index][vi]
    else
        # No solution with this number. In particular, with infeasibility,
        # there is no solution, and this always returns nothing.
        return nothing
    end
end

MOI.supports(::Optimizer, ::MOI.SolveTimeSec) = true
MOI.get(model::Optimizer, ::MOI.SolveTimeSec) = model.solve_time

MOI.supports(::Optimizer, ::MOI.TimeLimitSec) = true
MOI.get(model::Optimizer, ::MOI.TimeLimitSec) = model.time_limit_ms / 1_000.0
function MOI.set(model::Optimizer, ::MOI.TimeLimitSec, limit::Real) 
    model.time_limit_ms = limit * 1_000.0
end
function MOI.set(model::Optimizer, ::MOI.TimeLimitSec, limit::Nothing) 
    model.time_limit_ms = 0.0
end

MOI.supports(::Optimizer, ::MOI.Silent) = true
MOI.get(model::Optimizer, ::MOI.Silent) = !model.verboseness
function MOI.set(model::Optimizer, ::MOI.Silent, silentness::Bool) 
    model.verboseness = !silentness
end

MOI.supports(::Optimizer, ::MOI.ResultCount) = true
MOI.get(model::Optimizer, ::MOI.ResultCount) = length(model.results.primal_solutions)

# Specific case of dual solution: getting it must be supported (MOI 
# requirement), but few CP solvers have it accessible (none?).
# https://github.com/jump-dev/MathOptInterface.jl/pull/1561#pullrequestreview-740032701

MOI.supports(::Optimizer, ::MOI.DualStatus) = true
MOI.get(::Optimizer, ::MOI.DualStatus) = MOI.NO_SOLUTION

# Final helper to parse FZN output. It needs access to the model above...

"""
    _parse_to_assignments(sols::Vector{Dict{String, Vector{Number}}}, model::CP.FlatZinc.Model)

Parses the output of `_parse_to_assignments` and stores the solutions into 
`model`. This function is responsible for filling `model.results`.
"""
function _parse_to_moi_solutions(sols::Vector{Dict{String, Vector{Number}}}, model::Optimizer)
    @assert length(model.results.primal_solutions) == 0

    for i in 1:length(sols)
        sol = sols[i]
        push!(model.results.primal_solutions, Dict{MOI.VariableIndex, Real}())

        for (var_name, val) in sol
            # At least for now: no vector at the FlatZinc layer. Just ensure 
            # that this is consistent.
            @assert length(val) == 1 

            var_idx = MOI.get(model, MOI.VariableIndex, var_name)
            model.results.primal_solutions[i][var_idx] = val[1]
        end
    end
    
    model.results.termination_status = (length(sols) == 0) ? MOI.INFEASIBLE : MOI.OPTIMAL
    model.results.primal_status = (length(sols) == 0) ? MOI.NO_SOLUTION : MOI.FEASIBLE_POINT
    
    @assert length(model.results.primal_solutions) == length(sols)
    return 
end
