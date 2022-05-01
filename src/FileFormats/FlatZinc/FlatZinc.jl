# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

module FlatZinc

import ..FileFormats
import MathOptInterface

const MOI = MathOptInterface

MOI.Utilities.@model(
    Model,
    (MOI.ZeroOne, MOI.Integer),
    (MOI.EqualTo, MOI.GreaterThan, MOI.LessThan, MOI.Interval),
    (
        MOI.AllDifferent,
        MOI.Among,
        MOI.CountAtLeast,
        MOI.CountDistinct,
        MOI.CountGreaterThan,
    ),
    (),
    (),
    (MOI.ScalarAffineFunction,),
    (MOI.VectorOfVariables,),
    ()
)

function _get_bounds(model, x)
    lb, ub = -Inf, Inf
    F = MOI.VariableIndex
    ci = MOI.ConstraintIndex{F,MOI.GreaterThan{Float64}}(x.value)
    if MOI.is_valid(model, ci)
        lb = MOI.get(model, MOI.ConstraintSet(), ci).lower
    end
    ci = MOI.ConstraintIndex{F,MOI.LessThan{Float64}}(x.value)
    if MOI.is_valid(model, ci)
        ub = MOI.get(model, MOI.ConstraintSet(), ci).upper
    end
    ci = MOI.ConstraintIndex{F,MOI.Interval{Float64}}(x.value)
    if MOI.is_valid(model, ci)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        lb, ub = set.lower, set.upper
    end
    ci = MOI.ConstraintIndex{F,MOI.EqualTo{Float64}}(x.value)
    if MOI.is_valid(model, ci)
        set = MOI.get(model, MOI.ConstraintSet(), ci)
        lb = ub = set.value
    end
    return lb, ub
end

function Base.write(io::IO, model::Model)
    MOI.FileFormats.create_unique_variable_names(
        model,
        false,
        [
            s -> match(r"^[^a-zA-Z]", s) !== nothing ? "x" * s : s,
            s -> replace(s, r"[^A-Za-z0-9_]" => "_"),
        ],
    )
    # Print variables
    for x in MOI.get(model, MOI.ListOfVariableIndices())
        name = MOI.get(model, MOI.VariableName(), x)
        lb, ub = _get_bounds(model, x)
        zero_one = MOI.ConstraintIndex{MOI.VariableIndex,MOI.ZeroOne}(x.value)
        integer = MOI.ConstraintIndex{MOI.VariableIndex,MOI.Integer}(x.value)
        if MOI.is_valid(model, zero_one)
            println(io, "var bool: $(name) :: output_var;")
        elseif MOI.is_valid(model, integer)
            if lb == ub
                println(io, "var int: $(name) :: output_var = $lb;")
            elseif isfinite(lb) && isfinite(ub)
                lb, ub = ceil(Int, lb), floor(Int, ub)
                println(io, "var $lb .. $ub: $(name) :: output_var;")
            else
                println(io, "var int: $(name) :: output_var;")
            end
        else
            if lb == ub
                println(io, "var float: $(name) :: output_var = $lb;")
            elseif isfinite(lb) && isfinite(ub)
                println(io, "var $lb .. $ub: $(name) :: output_var;")
            else
                println(io, "var float: $(name) :: output_var;")
            end
        end
    end
    return
end

# Formal grammar: https://www.minizinc.org/doc-2.5.5/en/fzn-spec.html#grammar
# include("model.jl")
# include("export.jl")
# include("import.jl")
# include("optimizer.jl")

end
