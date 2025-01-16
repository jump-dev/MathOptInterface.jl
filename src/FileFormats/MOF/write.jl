# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.
using JSON3

"""
    Base.write(io::IO, model::FileFormats.MOF.Model)

Write `model` to `io` in the MathOptFormat file format.
"""
function Base.write(io::IO, model::Model)
    options = get_options(model)
    FileFormats.create_unique_names(model, warn = options.warn)
    variables, constraints = NamedTuple[], NamedTuple[]
    name_map = _write_variables(variables, model)
    objective = _write_nlpblock(constraints, model, name_map)
    if objective === nothing
        objective = _write_objective(model, name_map)
    end
    _write_constraints(constraints, model, name_map)
    object = (;
        name = "MathOptFormat Model",
        version = (
            major = Int(_SUPPORTED_VERSIONS[1].major),
            minor = Int(_SUPPORTED_VERSIONS[1].minor),
        ),
        variables = variables,
        objective = objective,
        constraints = constraints,
    )
    Base.write(io, JSON3.write(object))
    return
end

function _write_variables(variables::Vector{NamedTuple}, model::Model)
    name_map = Dict{MOI.VariableIndex,String}()
    for index in MOI.get(model, MOI.ListOfVariableIndices())
        variable = moi_to_object(index, model)
        name_map[index] = variable[:name]
        push!(variables, variable)
    end
    return name_map
end

function _lift_variable_indices(expr::Expr)
    if expr.head == :ref && length(expr.args) == 2 && expr.args[1] == :x
        return expr.args[2]
    else
        for (index, arg) in enumerate(expr.args)
            expr.args[index] = _lift_variable_indices(arg)
        end
    end
    return expr
end

_lift_variable_indices(arg) = arg  # Recursion fallback.

# TODO(odow): Used by PolyJuMP. Make private in future.
function extract_function_and_set(expr::Expr)
    if expr.head == :call  # One-sided constraint or foo-in-set.
        @assert length(expr.args) == 3
        if expr.args[1] == :in
            # return expr.args[2], expr.args[3]
            error("Constraints of the form foo-in-set aren't supported yet.")
        elseif expr.args[1] == :(<=)
            return expr.args[2], MOI.LessThan(expr.args[3])
        elseif expr.args[1] == :(>=)
            return expr.args[2], MOI.GreaterThan(expr.args[3])
        elseif expr.args[1] == :(==)
            return expr.args[2], MOI.EqualTo(expr.args[3])
        end
    elseif expr.head == :comparison  # Two-sided constraint.
        @assert length(expr.args) == 5
        if expr.args[2] == expr.args[4] == :(<=)
            return expr.args[3], MOI.Interval(expr.args[1], expr.args[5])
        elseif expr.args[2] == expr.args[4] == :(>=)
            return expr.args[3], MOI.Interval(expr.args[5], expr.args[1])
        end
    end
    return error("Oops. The constraint $(expr) wasn't recognised.")
end

function _write_nlpblock(
    constraints::Vector{NamedTuple},
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
)
    nlp_block = MOI.get(model, MOI.NLPBlock())
    if nlp_block === nothing
        return
    end
    MOI.initialize(nlp_block.evaluator, [:ExprGraph])
    variables = MOI.get(model, MOI.ListOfVariableIndices())
    for (row, bounds) in enumerate(nlp_block.constraint_bounds)
        constraint = MOI.constraint_expr(nlp_block.evaluator, row)
        (func, set) = extract_function_and_set(constraint)
        func = _lift_variable_indices(func)
        push!(
            constraints,
            (;
                :function => moi_to_object(Nonlinear(func), name_map),
                :set => moi_to_object(set, name_map),
            ),
        )
    end
    if nlp_block.has_objective
        objective = MOI.objective_expr(nlp_block.evaluator)
        objective = _lift_variable_indices(objective)
        sense = MOI.get(model, MOI.ObjectiveSense())
        return (;
            :sense => moi_to_object(sense),
            :function => moi_to_object(Nonlinear(objective), name_map),
        )
    end
    return
end

function _write_objective(
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        return (; :sense => moi_to_object(sense))
    end
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    objective_function = MOI.get(model, MOI.ObjectiveFunction{F}())
    return (;
        :sense => moi_to_object(sense),
        :function => moi_to_object(objective_function, name_map),
    )
end

function _write_constraints(
    constraints::Vector{NamedTuple},
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
)
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        for index in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            push!(constraints, moi_to_object(index, model, name_map))
        end
    end
    return
end

"""
    moi_to_object(x, model::Model)

Convert `x` into a NamedTuple representation.
"""
function moi_to_object end

function moi_to_object(index::MOI.VariableIndex, model::Model)
    name = MOI.get(model, MOI.VariableName(), index)
    primal_start = MOI.get(model, MOI.VariablePrimalStart(), index)
    if name == ""
        error("Variable name for $(index) cannot be blank in an MOF file.")
    elseif isnothing(primal_start)
        return (name = name,)
    else
        return (name = name, primal_start = primal_start)
    end
end

function moi_to_object(
    index::MOI.ConstraintIndex{F,S},
    model::Model,
    name_map::Dict{MOI.VariableIndex,String},
) where {F,S}
    func = MOI.get(model, MOI.ConstraintFunction(), index)
    set = MOI.get(model, MOI.ConstraintSet(), index)
    dual_start = MOI.get(model, MOI.ConstraintDualStart(), index)
    primal_start = MOI.get(model, MOI.ConstraintPrimalStart(), index)
    pairs = Pair{Symbol,Any}[]
    if F != MOI.VariableIndex
        name = MOI.get(model, MOI.ConstraintName(), index)
        if name != ""
            push!(pairs, :name => name)
        end
    end
    push!(pairs, :function => moi_to_object(func, name_map))
    push!(pairs, :set => moi_to_object(set, name_map))
    if !isnothing(dual_start)
        push!(pairs, :dual_start => dual_start)
    end
    if !isnothing(primal_start)
        push!(pairs, :primal_start => primal_start)
    end
    return NamedTuple(pairs)
end

function moi_to_object(sense::MOI.OptimizationSense)
    if sense == MOI.MIN_SENSE
        return "min"
    elseif sense == MOI.MAX_SENSE
        return "max"
    else
        @assert sense == MOI.FEASIBILITY_SENSE
        return "feasibility"
    end
end

# ========== Non-typed scalar functions ==========

function moi_to_object(
    foo::MOI.VariableIndex,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (type = "Variable", name = name_map[foo])
end

function _convert_nonlinear_to_mof(
    expr::Expr,
    node_list::Vector{Any},
    name_map::Dict{MOI.VariableIndex,String},
)
    if expr.head != :call
        error("Expected an expression that was a function. Got $(expr).")
    end
    node = (type = string(expr.args[1]), args = Any[])
    for i in 2:length(expr.args)
        push!(
            node[:args],
            _convert_nonlinear_to_mof(expr.args[i], node_list, name_map),
        )
    end
    push!(node_list, node)
    return (type = "node", index = length(node_list))
end

function _convert_nonlinear_to_mof(
    f::MOI.ScalarNonlinearFunction,
    node_list::Vector{Any},
    name_map::Dict{MOI.VariableIndex,String},
)
    node = (type = string(f.head), args = Any[])
    for arg in f.args
        push!(node[:args], _convert_nonlinear_to_mof(arg, node_list, name_map))
    end
    push!(node_list, node)
    return (type = "node", index = length(node_list))
end

function _convert_nonlinear_to_mof(
    variable::MOI.VariableIndex,
    ::Vector{Any},
    name_map::Dict{MOI.VariableIndex,String},
)
    return name_map[variable]
end

function _convert_nonlinear_to_mof(
    f::MOI.AbstractScalarFunction,
    node_list::Vector{Any},
    name_map::Dict{MOI.VariableIndex,String},
)
    return _convert_nonlinear_to_mof(
        convert(MOI.ScalarNonlinearFunction, f),
        node_list,
        name_map,
    )
end

function _convert_nonlinear_to_mof(
    value::Real,
    ::Vector{Any},
    ::Dict{MOI.VariableIndex,String},
)
    return value
end

function _convert_nonlinear_to_mof(
    value::Complex,
    ::Vector{Any},
    ::Dict{MOI.VariableIndex,String},
)
    return (type = "complex", real = real(value), imag = imag(value))
end

function moi_to_object(foo::Nonlinear, name_map::Dict{MOI.VariableIndex,String})
    node_list = Any[]
    root = _convert_nonlinear_to_mof(foo.expr, node_list, name_map)
    return (
        type = "ScalarNonlinearFunction",
        root = root,
        node_list = node_list,
    )
end

function moi_to_object(
    foo::MOI.ScalarNonlinearFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    node_list = Any[]
    root = _convert_nonlinear_to_mof(foo, node_list, name_map)
    return (
        type = "ScalarNonlinearFunction",
        root = root,
        node_list = node_list,
    )
end

# ========== Typed scalar functions ==========

function moi_to_object(
    foo::MOI.ScalarAffineTerm,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (coefficient = foo.coefficient, variable = name_map[foo.variable])
end

function moi_to_object(
    foo::MOI.ScalarAffineFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        type = "ScalarAffineFunction",
        terms = moi_to_object.(foo.terms, Ref(name_map)),
        constant = foo.constant,
    )
end

function moi_to_object(
    foo::MOI.ScalarQuadraticTerm,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        coefficient = foo.coefficient,
        variable_1 = name_map[foo.variable_1],
        variable_2 = name_map[foo.variable_2],
    )
end

function moi_to_object(
    foo::MOI.ScalarQuadraticFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        type = "ScalarQuadraticFunction",
        affine_terms = moi_to_object.(foo.affine_terms, Ref(name_map)),
        quadratic_terms = moi_to_object.(foo.quadratic_terms, Ref(name_map)),
        constant = foo.constant,
    )
end

# ========== Non-typed vector functions ==========

function moi_to_object(
    foo::MOI.VectorOfVariables,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        type = "VectorOfVariables",
        variables = [name_map[variable] for variable in foo.variables],
    )
end

function moi_to_object(
    foo::MOI.VectorNonlinearFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    node_list = Any[]
    rows = map(foo.rows) do f
        return _convert_nonlinear_to_mof(f, node_list, name_map)
    end
    return (
        type = "VectorNonlinearFunction",
        rows = rows,
        node_list = node_list,
    )
end

# ========== Typed vector functions ==========

function moi_to_object(
    foo::MOI.VectorAffineTerm,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        output_index = foo.output_index,
        scalar_term = moi_to_object(foo.scalar_term, name_map),
    )
end

function moi_to_object(
    foo::MOI.VectorAffineFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        type = "VectorAffineFunction",
        terms = moi_to_object.(foo.terms, Ref(name_map)),
        constants = foo.constants,
    )
end

function moi_to_object(
    foo::MOI.VectorQuadraticTerm,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        output_index = foo.output_index,
        scalar_term = moi_to_object(foo.scalar_term, name_map),
    )
end

function moi_to_object(
    foo::MOI.VectorQuadraticFunction,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (
        type = "VectorQuadraticFunction",
        affine_terms = moi_to_object.(foo.affine_terms, Ref(name_map)),
        quadratic_terms = moi_to_object.(foo.quadratic_terms, Ref(name_map)),
        constants = foo.constants,
    )
end

# ========== Default ==========
"""
    head_name(::Type{SetType}) where SetType <: MOI.AbstractSet

Return the string that is stored in the `"type"` field of the MOF object for a
set of type `SetType`.
"""
function head_name end
# We don't need a fallback that throws an error because it is impossible for
# this to be called for a set that is not defined in the MOIU Model constructor.

# Add every field as the field is named in MathOptInterface.
function moi_to_object(
    set::SetType,
    ::Dict{MOI.VariableIndex,String},
) where {SetType}
    pairs = Pair{Symbol,Any}[:type=>head_name(SetType)]
    for key in fieldnames(SetType)
        push!(pairs, Symbol(string(key)) => getfield(set, key))
    end
    return NamedTuple(pairs)
end

# ========== Non-typed scalar sets ==========
head_name(::Type{MOI.ZeroOne}) = "ZeroOne"
head_name(::Type{MOI.Integer}) = "Integer"

# ========== Typed scalar sets ==========
head_name(::Type{<:MOI.LessThan}) = "LessThan"
head_name(::Type{<:MOI.GreaterThan}) = "GreaterThan"
head_name(::Type{<:MOI.EqualTo}) = "EqualTo"
head_name(::Type{<:MOI.Interval}) = "Interval"
head_name(::Type{<:MOI.Semiinteger}) = "Semiinteger"
head_name(::Type{<:MOI.Semicontinuous}) = "Semicontinuous"
head_name(::Type{<:MOI.Parameter}) = "Parameter"

# ========== Non-typed vector sets ==========
head_name(::Type{MOI.Zeros}) = "Zeros"
head_name(::Type{MOI.Reals}) = "Reals"
head_name(::Type{MOI.Nonnegatives}) = "Nonnegatives"
head_name(::Type{MOI.Nonpositives}) = "Nonpositives"
head_name(::Type{MOI.SecondOrderCone}) = "SecondOrderCone"
head_name(::Type{MOI.RotatedSecondOrderCone}) = "RotatedSecondOrderCone"
head_name(::Type{MOI.GeometricMeanCone}) = "GeometricMeanCone"
head_name(::Type{MOI.ExponentialCone}) = "ExponentialCone"
head_name(::Type{MOI.DualExponentialCone}) = "DualExponentialCone"
head_name(::Type{MOI.NormOneCone}) = "NormOneCone"
head_name(::Type{MOI.NormInfinityCone}) = "NormInfinityCone"
head_name(::Type{MOI.NormCone}) = "NormCone"
head_name(::Type{MOI.RelativeEntropyCone}) = "RelativeEntropyCone"
head_name(::Type{MOI.NormSpectralCone}) = "NormSpectralCone"
head_name(::Type{MOI.NormNuclearCone}) = "NormNuclearCone"
head_name(::Type{MOI.RootDetConeTriangle}) = "RootDetConeTriangle"
head_name(::Type{MOI.RootDetConeSquare}) = "RootDetConeSquare"
head_name(::Type{MOI.LogDetConeTriangle}) = "LogDetConeTriangle"
head_name(::Type{MOI.LogDetConeSquare}) = "LogDetConeSquare"
function head_name(::Type{MOI.PositiveSemidefiniteConeTriangle})
    return "PositiveSemidefiniteConeTriangle"
end
function head_name(::Type{MOI.PositiveSemidefiniteConeSquare})
    return "PositiveSemidefiniteConeSquare"
end
head_name(::Type{MOI.Complements}) = "Complements"
function head_name(::Type{MOI.HermitianPositiveSemidefiniteConeTriangle})
    return "HermitianPositiveSemidefiniteConeTriangle"
end

head_name(::Type{MOI.AllDifferent}) = "AllDifferent"
head_name(::Type{MOI.Circuit}) = "Circuit"
head_name(::Type{MOI.CountAtLeast}) = "CountAtLeast"
head_name(::Type{MOI.CountBelongs}) = "CountBelongs"
head_name(::Type{MOI.CountDistinct}) = "CountDistinct"
head_name(::Type{MOI.CountGreaterThan}) = "CountGreaterThan"
head_name(::Type{MOI.Cumulative}) = "Cumulative"
head_name(::Type{MOI.Path}) = "Path"

# ========== Typed vector sets ==========
head_name(::Type{<:MOI.PowerCone}) = "PowerCone"
head_name(::Type{<:MOI.DualPowerCone}) = "DualPowerCone"
head_name(::Type{<:MOI.SOS1}) = "SOS1"
head_name(::Type{<:MOI.SOS2}) = "SOS2"
head_name(::Type{<:MOI.BinPacking}) = "BinPacking"
head_name(::Type{<:MOI.HyperRectangle}) = "HyperRectangle"

function moi_to_object(
    set::MOI.Indicator{I,S},
    name_map::Dict{MOI.VariableIndex,String},
) where {I,S}
    @assert I == MOI.ACTIVATE_ON_ONE || I == MOI.ACTIVATE_ON_ZERO
    return (
        type = "Indicator",
        set = moi_to_object(set.set, name_map),
        activate_on = (I == MOI.ACTIVATE_ON_ONE) ? "one" : "zero",
    )
end

function moi_to_object(
    set::MOI.Reified,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (type = "Reified", set = moi_to_object(set.set, name_map))
end

function moi_to_object(
    set::MOI.Table{T},
    ::Dict{MOI.VariableIndex,String},
) where {T}
    return (
        type = "Table",
        table = [set.table[i, :] for i in 1:size(set.table, 1)],
    )
end

function moi_to_object(
    set::MOI.Scaled,
    name_map::Dict{MOI.VariableIndex,String},
)
    return (type = "Scaled", set = moi_to_object(set.set, name_map))
end
