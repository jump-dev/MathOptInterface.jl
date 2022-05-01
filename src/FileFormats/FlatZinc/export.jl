# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

const START_REG = r"^[^a-zA-Z]"
const NAME_REG = r"[^A-Za-z0-9_]"

"""
    Base.write(io::IO, model::FlatZinc.Model)

Write `model` to `io` in the FlatZinc (fzn) file format.
"""
function Base.write(io::IO, model::Model)
    # Reset the data structures holding set and array names.
    empty!(model.sets_id)
    empty!(model.arrs_id)

    # Ensure variable names are unique.
    MOI.FileFormats.create_unique_variable_names(
        model,
        false,
        [
            s -> match(START_REG, s) !== nothing ? "x" * s : s,
            s -> replace(s, NAME_REG => "_"),
        ],
    )

    # Actually write the contents.
    write_variables(io, model)
    write_sets(io, model)
    write_arrays(io, model)
    write_constraints(io, model)
    write_objective(io, model)

    return
end

function write_variables(io::IO, model::Model)
    for var in values(model.variable_info)
        # Variables either start with "var" or "array of var", let
        # write_variable decide (even though only "var" s implemented for now).
        write_variable(io, var.name, var.set)
        println(io)
    end
    println(io)
    return nothing
end

function write_sets(io::IO, model::Model)
    for cons in model.constraint_info
        if !cons.output_as_part_of_variable
            write_set(io, model, cons, cons.s)
        end
    end
    println(io)
    return nothing
end

function write_arrays(io::IO, model::Model)
    for cons in model.constraint_info
        if !cons.output_as_part_of_variable
            write_array(io, model, cons, cons.s)
        end
    end
    println(io)
    return nothing
end

function write_constraints(io::IO, model::Model)
    for cons in model.constraint_info
        if !cons.output_as_part_of_variable
            print(io, "constraint ")
            write_constraint(io, model, cons.index, cons.f, cons.s)
            print(io, ";")
            println(io)
        end
    end
    println(io)
    return nothing
end

# Variable printing.

function write_variable(io::IO, name::String, s::MOI.EqualTo{Float64})
    print(io, "var float: $(name) :: output_var = $(s.value);")
    return nothing
end

function write_variable(io::IO, name::String, s::MOI.EqualTo{Int})
    print(io, "var int: $(name) :: output_var = $(s.value);")
    return nothing
end

function write_variable(io::IO, name::String, s::MOI.EqualTo{Bool})
    print(io, "var bool: $(name) :: output_var = $(s.value);")
    return nothing
end

function write_variable(io::IO, name::String, s::MOI.LessThan{Float64})
    # typemin(Float64) is -Inf, which is "-Inf" as a string (which is not
    # allowed by FlatZinc). Take the next smallest value as a proxy, because
    # it has a standard scientific notation (and this is allowed in FlatZinc).
    print(io, "var $(nextfloat(typemin(Float64)))..$(s.upper): $(name) :: output_var;")
    return nothing
end

function write_variable(io::IO, name::String, s::MOI.LessThan{Int})
    print(io, "var $(typemin(Int))..$(s.upper): $(name) :: output_var;")
    return nothing
end

function write_variable(io::IO, name::String, s::MOI.GreaterThan{Float64})
    # typemax(Float64) is Inf, which is "Inf" as a string (which is not
    # allowed by FlatZinc). Take the next largest value as a proxy, because
    # it has a standard scientific notation (and this is allowed in FlatZinc).
    print(io, "var $(s.lower)..$(prevfloat(typemax(Float64))): $(name) :: output_var;")
    return nothing
end

function write_variable(io::IO, name::String, s::MOI.GreaterThan{Int})
    print(io, "var $(s.lower)..$(typemax(Int)): $(name) :: output_var;")
    return nothing
end

function write_variable(
    io::IO,
    name::String,
    s::MOI.Interval{T},
) where {T <: Union{Int, Float64}}
    print(io, "var $(s.lower)..$(s.upper): $(name) :: output_var;")
    return nothing
end

function write_variable(io::IO, name::String, ::MOI.Reals)
    print(io, "var float: $(name) :: output_var;")
    return nothing
end

function write_variable(io::IO, name::String, ::MOI.ZeroOne)
    print(io, "var bool: $(name) :: output_var;")
    return nothing
end

function write_variable(io::IO, name::String, ::MOI.Integer)
    print(io, "var int: $(name) :: output_var;")
    return nothing
end

# Set printing.

function write_set(::IO, ::Model, ::ConstraintInfo, ::MOI.AbstractSet)
    # In general, nothing to do.
    return nothing
end

function write_set(
    io::IO,
    model::Model,
    con::ConstraintInfo,
    s::CP.Domain{Int},
)
    set_name = "SET" * string(length(model.sets_id))
    model.sets_id[con.index] = set_name
    set_value = join(collect(s.values), ", ")

    print(io, "set of int: $(set_name) = {$(set_value)};")
    println(io)
    return nothing
end

# Array printing.

function write_array(::IO, ::Model, ::ConstraintInfo, ::MOI.AbstractSet)
    # In general, nothing to do.
    return nothing
end

function write_array(
    io::IO,
    model::Model,
    con::ConstraintInfo,
    s::CP.Element{Bool},
)
    array_name = "ARRAY" * string(length(model.arrs_id))
    model.arrs_id[con.index] = array_name
    array_value = join(collect(s.values), ", ")
    array_length = length(s.values)

    values = join([ifelse(v, "1", "0") for v in s.values], ", ")

    print(
        io,
        "array [1..$(array_length)] of bool: $(array_name) = [$(array_value)];",
    )
    println(io)
    return nothing
end

function write_array(
    io::IO,
    model::Model,
    con::ConstraintInfo,
    s::CP.Element{Int},
)
    array_name = "ARRAY" * string(length(model.arrs_id))
    model.arrs_id[con.index] = array_name
    array_value = join(collect(s.values), ", ")
    array_length = length(s.values)

    print(
        io,
        "array [1..$(array_length)] of int: $(array_name) = [$(array_value)];",
    )
    println(io)
    return nothing
end

function write_array(
    io::IO,
    model::Model,
    con::ConstraintInfo,
    s::CP.Element{Float64},
)
    array_name = "ARRAY" * string(length(model.arrs_id))
    model.arrs_id[con.index] = array_name
    array_value = join(collect(s.values), ", ")
    array_length = length(s.values)

    print(
        io,
        "array [1..$(array_length)] of float: $(array_name) = [$(array_value)];",
    )
    println(io)
    return nothing
end

# Constraint printing.
# Based on the built-in predicates: https://www.minizinc.org/doc-2.5.5/en/lib-flatzinc.html
# In the same order as the documentation.

# - Dispatch on variable types, if needed.

function _promote_type(model::MOI.ModelLike, vars::Vector{MOI.VariableIndex})
    smallest = :bool

    for var in vars
        if smallest == :bool && !CP.is_binary(model, var)
            smallest = :int
        end
        if smallest == :int && !CP.is_integer(model, var)
            smallest = :float
            break
        end
    end

    return smallest
end

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::Union{
        MOI.EqualTo{T},
        MOI.LessThan{T},
        CP.Strictly{MOI.LessThan{T}},
        CP.DifferentFrom{T},
    },
) where {T}
    # *_eq, *_le, *_lt, *_ne
    write_constraint(
        io,
        model,
        index,
        f,
        s,
        Val(_promote_type(model, [f])),
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction{T},
    s::Union{MOI.EqualTo{U}, MOI.LessThan{U}},
) where {T, U}
    # *_lin_eq, *_lin_le
    variables, _ = _saf_to_coef_vars(f)
    write_constraint(
        io,
        model,
        index,
        f,
        s,
        Val(_promote_type(model, variables)),
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::Union{
        CP.Reification{MOI.EqualTo{T}},
        CP.Reification{MOI.LessThan{T}},
        CP.Reification{CP.Strictly{MOI.LessThan{T}, T}},
        CP.Reification{CP.DifferentFrom{T}},
    },
) where {T}
    # *_eq_reif, *_le_reif, *_lt_reif, *_ne_reif
    write_constraint(
        io,
        model,
        index,
        f,
        s,
        Val(_promote_type(model, f.variables)),
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction{T},
    s::Union{
        CP.Reification{MOI.EqualTo{U}},
        CP.Reification{MOI.LessThan{U}},
        CP.Reification{CP.Strictly{MOI.LessThan{U}, U}},
        CP.Reification{CP.DifferentFrom{T}},
    },
) where {T, U}
    # *_lin_eq_reif, *_lin_le_reif, *_lin_lt_reif, *_lin_ne_reif
    variables = _vaf_to_vars(f)
    write_constraint(
        io,
        model,
        index,
        f,
        s,
        Val(_promote_type(model, variables)),
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::Union{CP.MaximumAmong, CP.MinimumAmong},
)
    # array_*_maximum, array_*_minimum
    write_constraint(
        io,
        model,
        index,
        f,
        s,
        Val(_promote_type(model, f.variables)),
    )
    return nothing
end

# - Integer constraints.

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.Element{Int},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_integer(model, f.variables[1])
    @assert CP.is_integer(model, f.variables[2])

    array_name = model.arrs_id[index]
    value = f.variables[1]
    index = f.variables[2]
    print(
        io,
        "array_int_element($(_fzn_f(model, index)), $(array_name), $(_fzn_f(model, value)))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.MaximumAmong,
    ::Val{:int},
)
    array = f.variables[2:end]
    value = f.variables[1]
    print(
        io,
        "array_int_maximum($(_fzn_f(model, value)), [$(_fzn_f(model, array))])",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.MinimumAmong,
    ::Val{:int},
)
    array = f.variables[2:end]
    value = f.variables[1]
    print(
        io,
        "array_int_minimum($(_fzn_f(model, value)), [$(_fzn_f(model, array))])",
    )
    return nothing
end

# TODO: absolute value. int_abs.
# TODO: integer division. int_div

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.LessThan{Int},
    ::Val{:int},
)
    @assert CP.is_integer(model, f) || CP.is_binary(model, f)
    print(io, "int_le($(_fzn_f(model, f)), $(s.upper))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.LessThan{Bool},
    ::Val{:bool},
)
    @assert CP.is_binary(model, f)
    print(io, "bool_le($(_fzn_f(model, f)), $(Bool(s.upper)))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{MOI.LessThan{Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])
    @assert CP.is_integer(model, f.variables[2]) ||
            CP.is_binary(model, f.variables[2])

    print(
        io,
        "int_le_reif($(_fzn_f(model, f.variables[1])), $(s.set.upper), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.EqualTo{Int},
    ::Val{:int},
)
    # Hypothesis: !cons.output_as_part_of_variable.
    print(io, "int_eq($(_fzn_f(model, f)), $(s.value))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::MOI.EqualTo{Int},
    ::Val{:int},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = s.value - f.constant
    print(
        io,
        "int_lin_eq($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{MOI.EqualTo{Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])

    print(
        io,
        "int_eq_reif($(_fzn_f(model, f.variables[2])), $(s.set.value), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{MOI.EqualTo{Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    for v in _vaf_to_vars(f, 2)
        @assert CP.is_integer(model, v) || CP.is_binary(model, v)
    end
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = s.set.value - constant

    print(
        io,
        "int_lin_eq_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::MOI.LessThan{Int},
    ::Val{:int},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = s.upper - f.constant
    print(
        io,
        "int_lin_le($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::CP.DifferentFrom{Int},
)
    @assert CP.is_integer(model, f) || CP.is_binary(model, f)
    print(io, "int_ne($(_fzn_f(model, f)), $(s.value))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::CP.DifferentFrom{Bool},
)
    @assert CP.is_binary(model, f)
    print(io, "int_ne($(_fzn_f(model, f)), $(s.value))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::CP.DifferentFrom{Bool},
)
    variables, coefficients = _saf_to_coef_vars(f)
    for v in variables
        @assert CP.is_binary(model, v)
    end

    value = s.value - f.constant
    print(
        io,
        "int_lin_ne($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::CP.DifferentFrom{Int},
)
    variables, coefficients = _saf_to_coef_vars(f)
    for v in variables
        @assert CP.is_integer(model, v) || CP.is_binary(model, v)
    end

    value = s.value - f.constant
    print(
        io,
        "int_lin_ne($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{CP.DifferentFrom{Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])
    @assert CP.is_integer(model, f.variables[2])

    print(
        io,
        "int_ne_reif($(_fzn_f(model, f.variables[2])), $(s.set.value), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{CP.DifferentFrom{Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    for v in _vaf_to_vars(f, 2)
        @assert CP.is_integer(model, v) || CP.is_binary(model, v)
    end
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = s.set.value - constant

    print(
        io,
        "int_lin_ne_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::CP.Strictly{MOI.LessThan{Int}},
)
    @assert CP.is_integer(model, f) || CP.is_binary(model, f)
    print(io, "int_lt($(_fzn_f(model, f)), $(s.set.upper))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::CP.Strictly{MOI.LessThan{Bool}},
)
    @assert CP.is_binary(model, f)
    print(io, "bool_lt($(_fzn_f(model, f)), $(s.set.upper))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{CP.Strictly{MOI.LessThan{Int}, Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])
    @assert CP.is_integer(model, f.variables[2])

    print(
        io,
        "int_lt_reif($(_fzn_f(model, f.variables[2])), $(s.set.set.upper), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{CP.Strictly{MOI.LessThan{Int}, Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    for v in _vaf_to_vars(f, 2)
        @assert CP.is_integer(model, v) || CP.is_binary(model, v)
    end
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = s.set.set.upper - constant

    print(
        io,
        "int_lin_lt_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

# TODO: int_max (CP equivalent!?)
# TODO: int_min (CP equivalent!?)
# TODO: int_mod, modulo

# int_ne, int_ne_reif: meaningless for MOI, no way to represent "x == y"
# natively (goes through affine expressions).

# TODO: int_pow.
# TODO: int_times.

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    ::CP.Domain{Int},
)
    print(io, "set_in($(_fzn_f(model, f)), $(model.sets_id[index]))")
    return nothing
end

# - Boolean constraints.

# TODO: array_bool_and, no conjunction between variables for now in CP.

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.Element{Bool},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])
    @assert CP.is_binary(model, f.variables[2])

    array_name = model.arrs_id[index]
    value = f.variables[1]
    index = f.variables[2]

    print(
        io,
        "array_bool_element($(_fzn_f(model, index)), $(array_name), $(_fzn_f(model, value)))",
    )
    return nothing
end

# TODO: array_bool_or, no disjunction between variables for now in CP.
# TODO: array_bool_xor, no XOR for now in CP.
# TODO: array_var_bool_element, no CP.Element for array of variables.
# TODO: bool2int, not in CP for now.
# TODO: bool_and, like array_bool_and.
# TODO: bool_clause, not in CP for now.

# bool_eq, bool_eq_reif: meaningless for MOI, no way to represent "x == y"
# natively (goes through affine expressions).
# bool_le, bool_le_reif: meaningless for MOI, no way to represent "x <= y"
# natively (goes through affine expressions).

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.EqualTo{T},
    ::Val{:bool},
) where {T <: Union{Int, Bool}}
    # Hypothesis: !cons.output_as_part_of_variable.
    print(io, "bool_eq($(_fzn_f(model, f)), $(Bool(s.value)))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::MOI.EqualTo{T},
    ::Val{:bool},
) where {T <: Union{Int, Bool}}
    variables, coefficients = _saf_to_coef_vars(f)
    value = s.value - f.constant
    print(
        io,
        "bool_lin_eq($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::MOI.LessThan{Int},
    ::Val{:bool},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = s.upper - f.constant
    print(
        io,
        "bool_lin_le($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

# bool_lt, bool_lt_reif: meaningless for MOI, no way to represent "x < y"
# natively (goes through affine expressions).
# bool_ne, bool_ne_reif: meaningless for MOI, no way to represent "x != y"
# natively (goes through affine expressions).

# TODO: bool_or, no disjunction between variables for now in CP.
# TODO: bool_xor, no XOR between variables for now in CP.

# - Set constraints.

# TODO: no notion of set in MOI!

# - Float constraints.

function write_constraint(
    io::IO,
    model::Model,
    index::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.Element{Float64},
)
    @assert MOI.output_dimension(f) == 2

    array_name = model.arrs_id[index]
    value = f.variables[1]
    index = f.variables[2]

    print(
        io,
        "array_float_element($(_fzn_f(model, index)), $(array_name), $(_fzn_f(model, value)))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.MaximumAmong,
    ::Val{:float},
)
    array = f.variables[2:end]
    value = f.variables[1]
    return print(
        io,
        "array_float_maximum($(_fzn_f(model, value)), [$(_fzn_f(model, array))])",
    )
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    ::CP.MinimumAmong,
    ::Val{:float},
)
    array = f.variables[2:end]
    value = f.variables[1]
    return print(
        io,
        "array_float_minimum($(_fzn_f(model, value)), [$(_fzn_f(model, array))])",
    )
end

# TODO: array_var_float_element, i.e. CP.Element with a variable array.

# TODO: float_abs, float_acos, float_acosh, float_asin, float_asinh,
# float_atan, float_atanh, float_cos, float_cosh, float_div.

# float_dom: could be useful to merge several MOI.Interval as one constraint,
# for now several float_in.

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.EqualTo{Float64},
    ::Val{:float},
)
    # Hypothesis: !cons.output_as_part_of_variable.
    print(io, "float_eq($(_fzn_f(model, f)), $(s.value))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{MOI.EqualTo{Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])

    print(
        io,
        "float_eq_reif($(_fzn_f(model, f.variables[2])), $(s.set.value), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

# TODO: float_exp

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.Interval{Float64},
)
    print(io, "float_in($(_fzn_f(model, f)), $(s.lower), $(s.upper))")
    return nothing
end

# TODO: float_in_reif

# float_le, float_le_reif: meaningless for MOI, no way to represent "x <= y"
# natively (goes through affine expressions).

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::MOI.EqualTo{Float64},
    ::Val{:float},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = Float64(s.value - f.constant)
    print(
        io,
        "float_lin_eq($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{MOI.EqualTo{Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = Float64(s.set.value - constant)

    print(
        io,
        "float_lin_eq_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::MOI.LessThan{Float64},
    ::Val{:float},
)
    print(io, "float_le($(_fzn_f(model, f)), $(s.upper))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::CP.Strictly{MOI.LessThan{Float64}, Float64},
    ::Val{:float},
)
    print(io, "float_lt($(_fzn_f(model, f)), $(s.set.upper))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::MOI.LessThan{Float64},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = Float64(s.upper - f.constant)
    print(
        io,
        "float_lin_le($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{MOI.LessThan{Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])

    print(
        io,
        "float_le_reif($(_fzn_f(model, f.variables[2])), $(s.set.upper), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{MOI.LessThan{Int}},
    ::Val{:int},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    for v in _vaf_to_vars(f, 2)
        @assert CP.is_integer(model, v) || CP.is_binary(model, v)
    end
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = s.set.upper - constant

    print(
        io,
        "int_lin_le_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{MOI.LessThan{Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = Float64(s.set.upper - constant)

    print(
        io,
        "float_lin_le_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::CP.Strictly{MOI.LessThan{Float64}},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = Float64(s.set.upper - f.constant)
    print(
        io,
        "float_lin_lt($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{CP.Strictly{MOI.LessThan{Float64}, Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])

    print(
        io,
        "float_lt_reif($(_fzn_f(model, f.variables[2])), $(s.set.set.upper), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{CP.Strictly{MOI.LessThan{Float64}, Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = Float64(s.set.set.upper - constant)

    print(
        io,
        "float_lin_lt_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.ScalarAffineFunction,
    s::CP.DifferentFrom{Float64},
)
    variables, coefficients = _saf_to_coef_vars(f)
    value = Float64(s.value - f.constant)
    print(
        io,
        "float_lin_ne($(coefficients), [$(_fzn_f(model, variables))], $(value))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VariableIndex,
    s::CP.DifferentFrom{Float64},
)
    print(io, "float_ne($(_fzn_f(model, f)), $(s.value))")
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorOfVariables,
    s::CP.Reification{CP.DifferentFrom{Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    @assert CP.is_binary(model, f.variables[1])

    print(
        io,
        "float_ne_reif($(_fzn_f(model, f.variables[2])), $(s.set.value), $(_fzn_f(model, f.variables[1])))",
    )
    return nothing
end

function write_constraint(
    io::IO,
    model::Model,
    ::MOI.ConstraintIndex,
    f::MOI.VectorAffineFunction,
    s::CP.Reification{CP.DifferentFrom{Float64}},
    ::Val{:float},
)
    @assert MOI.output_dimension(f) == 2
    vars_1, coeffs_1 = _vaf_to_coef_vars(f, 1)
    @assert length(vars_1) == 1
    @assert length(coeffs_1) == 1
    @assert CP.is_binary(model, vars_1[1])
    @assert coeffs_1[1] == 1

    variables, coefficients, constant = _vaf_to_coef_vars(f, 2)
    value = Float64(s.set.value - constant)

    print(
        io,
        "float_lin_ne_reif($(coefficients), [$(_fzn_f(model, variables))], $(value), $(_fzn_f(model, vars_1[1])))",
    )
    return nothing
end

# TODO: float_ln, float_log10, float_log2

# float_lt, float_lt_reif: meaningless for MOI, no way to represent "x < y"
# natively (goes through affine expressions).

# TODO: float_max (CP equivalent!?)
# TODO: float_min (CP equivalent!?)

# float_net, float_ne_reif: meaningless for MOI, no way to represent "x != y"
# natively (goes through affine expressions).

# TODO: float_pow, float_sin, float_sinh, float_sqrt, float_tan, float_tanh, float_times
# TODO: int2float, not in CP for now.

# - MiniZinc 2.0.0
# TODO: bool_clause_reif.

# - MiniZinc 2.0.2
# TODO: array_var_bool_element_nonshifted, array_var_float_element_nonshifted,
# array_var_int_element_nonshifted, array_var_set_element_nonshifted

# - MiniZinc 2.1.0
# Already included above, like in the docs.

# - MiniZinc 2.2.1
# TODO: int_pow_fixed

# - MiniZinc 2.3.3
# TODO: float_set_in

# - MiniZinc 2.5.2
# TODO: array_var_bool_element2d_nonshifted, array_var_float_element2d_nonshifted,
# array_var_int_element2d_nonshifted, array_var_set_element2d_nonshifted

# Objective printing.

function write_objective(io::IO, model::Model)
    print(io, "solve ")
    if model.objective_sense == MOI.FEASIBILITY_SENSE &&
       model.objective_function === nothing
        print(io, "satisfy")
    elseif model.objective_sense == MOI.MIN_SENSE &&
           model.objective_function !== nothing
        print(io, "minimize $(_fzn_f(model, model.objective_function))")
    elseif model.objective_sense == MOI.MAX_SENSE &&
           model.objective_function !== nothing
        print(io, "maximize $(_fzn_f(model, model.objective_function))")
    else
        error(
            "Assertion failed when printing the objective. Sense: $(model.objective_sense). Function: $(model.objective_function).",
        )
    end
    print(io, ";")
    println(io)
    return nothing
end

# Function printing.

_fzn_f(model::Model, f::MOI.VariableIndex) = model.variable_info[f].name
function _fzn_f(model::Model, fs::Vector{MOI.VariableIndex})
    return join([_fzn_f(model, f) for f in fs], ", ")
end

# Destructuring.

function _saf_to_coef_vars(f::MOI.ScalarAffineFunction)
    MOIU.canonicalize!(f)
    variables = MOI.VariableIndex[t.variable for t in f.terms]
    coefficients = [t.coefficient for t in f.terms]

    return variables, coefficients
end

function _vaf_to_vars(f::MOI.VectorAffineFunction)
    MOIU.canonicalize!(f)
    variables = MOI.VariableIndex[t.scalar_term.variable for t in f.terms]
    return variables
end

function _vaf_to_vars(f::MOI.VectorAffineFunction, dim::Int)
    MOIU.canonicalize!(f)
    variables = MOI.VariableIndex[
        t.scalar_term.variable for t in f.terms if t.output_index == dim
    ]
    return variables
end

function _vaf_to_coef_vars(f::MOI.VectorAffineFunction, dim::Int)
    MOIU.canonicalize!(f)
    variables = MOI.VariableIndex[
        t.scalar_term.variable for t in f.terms if t.output_index == dim
    ]
    coefficients =
        [t.scalar_term.coefficient for t in f.terms if t.output_index == dim]
    constant = f.constants[dim]

    return variables, coefficients, constant
end
