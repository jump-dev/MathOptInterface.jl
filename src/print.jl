# Used for dispatching
abstract type PrintMode end
abstract type REPLMode <: PrintMode end
abstract type IJuliaMode <: PrintMode end

# REPL-specific symbols
# Anything here: https://en.wikipedia.org/wiki/Windows-1252
# should probably work fine on Windows
function _math_symbol(::Type{REPLMode}, name::Symbol)
    if name == :leq
        return Sys.iswindows() ? "<=" : "≤"
    elseif name == :geq
        return Sys.iswindows() ? ">=" : "≥"
    elseif name == :eq
        return Sys.iswindows() ? "==" : "="
    elseif name == :times
        return "*"
    elseif name == :sq
        return "²"
    elseif name == :ind_open
        return "["
    elseif name == :ind_close
        return "]"
    elseif name == :for_all
        return Sys.iswindows() ? "for all" : "∀"
    elseif name == :in
        return Sys.iswindows() ? "in" : "∈"
    elseif name == :open_set
        return "{"
    elseif name == :dots
        return Sys.iswindows() ? ".." : "…"
    elseif name == :close_set
        return "}"
    elseif name == :union
        return Sys.iswindows() ? "or" : "∪"
    elseif name == :infty
        return Sys.iswindows() ? "Inf" : "∞"
    elseif name == :open_rng
        return "["
    elseif name == :close_rng
        return "]"
    elseif name == :integer
        return "integer"
    elseif name == :succeq0
        return " is semidefinite"
    elseif name == :Vert
        return Sys.iswindows() ? "||" : "‖"
    elseif name == :sub2
        return Sys.iswindows() ? "_2" : "₂"
    else
        error("Internal error: Unrecognized symbol $name.")
    end
end

# IJulia-specific symbols.
function _math_symbol(::Type{IJuliaMode}, name::Symbol)
    if name == :leq
        return "\\leq"
    elseif name == :geq
        return "\\geq"
    elseif name == :eq
        return "="
    elseif name == :times
        return "\\times "
    elseif name == :sq
        return "^2"
    elseif name == :ind_open
        return "_{"
    elseif name == :ind_close
        return "}"
    elseif name == :for_all
        return "\\quad\\forall"
    elseif name == :in
        return "\\in"
    elseif name == :open_set
        return "\\{"
    elseif name == :dots
        return "\\dots"
    elseif name == :close_set
        return "\\}"
    elseif name == :union
        return "\\cup"
    elseif name == :infty
        return "\\infty"
    elseif name == :open_rng
        return "\\["
    elseif name == :close_rng
        return "\\]"
    elseif name == :integer
        return "\\in \\mathbb{Z}"
    elseif name == :succeq0
        return "\\succeq 0"
    elseif name == :Vert
        return "\\Vert"
    elseif name == :sub2
        return "_2"
    else
        error("Internal error: Unrecognized symbol $name.")
    end
end

Base.show(io::IO, model::ModelLike) = Utilities.print_with_acronym(io, summary(model))

# Helper function that rounds carefully for the purposes of printing
# e.g.   5.3  =>  5.3
#        1.0  =>  1
function _string_round(f::Float64)
    iszero(f) && return "0" # strip sign off zero
    str = string(f)
    length(str) >= 2 && str[end-1:end] == ".0" ? str[1:end-2] : str
end
_string_round(f) = string(f)

_wrap_in_math_mode(str) = "\$\$ $str \$\$"
_wrap_in_inline_math_mode(str) = "\$ $str \$"

function Base.print(io::IO, model::ModelLike, variable_name = name_or_noname)
    print(io, model_string(REPLMode, model, variable_name))
end
function Base.show(io::IO, ::MIME"text/latex", model::ModelLike, variable_name = name_or_noname)
    print(io, _wrap_in_math_mode(model_string(IJuliaMode, model, variable_name)))
end

function model_string(print_mode, model::ModelLike, variable_name = name_or_noname)
    ijl = print_mode == IJuliaMode
    sep = ijl ? " & " : " "
    eol = ijl ? "\\\\\n" : "\n"
    sense = get(model, ObjectiveSense())
    str = ""
    if sense == MAX_SENSE
        str *= ijl ? "\\max" : "Max"
    elseif sense == MIN_SENSE
        str *= ijl ? "\\min" : "Min"
    else
        str *= ijl ? "\\text{feasibility}" : "Feasibility"
    end
    if sense != FEASIBILITY_SENSE
        if ijl
            str *= "\\quad"
        end
        str *= sep
        str *= objective_function_string(print_mode, model, variable_name)
    end
    str *= eol
    str *= ijl ? "\\text{Subject to} \\quad" : "Subject to" * eol
    constraints = constraints_string(print_mode, model, variable_name)
    if print_mode == REPLMode
        constraints = map(str -> replace(str, '\n' => eol * sep), constraints)
    end
    if !isempty(constraints)
        str *= sep
    end
    str *= join(constraints, eol * sep)
    if !isempty(constraints)
        str *= eol
    end
    if ijl
        str = "\\begin{alignat*}{1}" * str * "\\end{alignat*}\n"
    end
    return str
end

"""
    constraints_string(print_mode, model::MOI.ModelLike)::Vector{String}

Return a list of `String`s describing each constraint of the model.
"""
function constraints_string(print_mode, model::ModelLike, variable_name = name_or_noname)
    strings = String[]
    for (F, S) in get(model, ListOfConstraints())
        for ci in get(model, ListOfConstraintIndices{F, S}())
            push!(strings, constraint_string(print_mode, model, ci, variable_name, in_math_mode = true))
        end
    end
    return strings
end

function constraint_string(print_mode, model::ModelLike, func::AbstractFunction, set::AbstractSet, variable_name = name_or_noname)
    func_str = function_string(print_mode, model, func, variable_name)
    in_set_str = in_set_string(print_mode, set)
    if print_mode == REPLMode
        lines = split(func_str, '\n')
        lines[1 + div(length(lines), 2)] *= " " * in_set_str
        return join(lines, '\n')
    else
        return func_str * " " * in_set_str
    end
end
function constraint_string(print_mode, model::ModelLike, constraint_name,
                           func::AbstractFunction,
                           set::AbstractSet,
                           variable_name = name_or_noname;
                           in_math_mode = false)
    constraint_without_name = constraint_string(print_mode, model, func, set, variable_name)
    if print_mode == IJuliaMode && !in_math_mode
        constraint_without_name = _wrap_in_inline_math_mode(constraint_without_name)
    end
    # Names don't print well in LaTeX math mode
    if isempty(constraint_name) || (print_mode == IJuliaMode && in_math_mode)
        return constraint_without_name
    else
        return constraint_name * " : " * constraint_without_name
    end
end
function constraint_string(print_mode, model::ModelLike, ci::ConstraintIndex, variable_name = name_or_noname; in_math_mode = false)
    func = get(model, ConstraintFunction(), ci)
    set = get(model, ConstraintSet(), ci)
    if supports(model, ConstraintName(), typeof(ci))
        name = get(model, ConstraintName(), ci)
        return constraint_string(print_mode, model, name, func, set, variable_name, in_math_mode = in_math_mode)
    else
        return constraint_string(print_mode, model, func, set, variable_name, in_math_mode = in_math_mode)
    end
end

"""
    objective_function_string(print_mode, model::AbstractModel)::String

Return a `String` describing the objective function of the model.
"""
function objective_function_string(print_mode, model::ModelLike, variable_name = name_or_noname)
    objective_function_type = get(model, ObjectiveFunctionType())
    attr = ObjectiveFunction{objective_function_type}()
    objective_function = get(model, attr)
    return function_string(print_mode, model, objective_function, variable_name)
end

function name_or_noname(model, vi)
    name = get(model, VariableName(), vi)
    if isempty(name)
        return "noname"
    else
        return name
    end
end
function name_or_default_name(model, vi)
    name = get(model, VariableName(), vi)
    if isempty(name)
        return default_name(vi)
    else
        return name
    end
end

function function_string(print_mode, model::ModelLike, func::AbstractFunction, variable_name = name_or_noname)
    if supports(model, VariableName(), VariableIndex)
        return function_string(print_mode, func, vi -> variable_name(model, vi))
    else
        return function_string(print_mode, func)
    end
end

default_name(vi) = string("x[", vi.value, "]")
function_string(::Type{REPLMode}, v::VariableIndex, variable_name = default_name) = variable_name(v)
function function_string(::Type{IJuliaMode}, v::VariableIndex, variable_name = default_name)
    # TODO: This is wrong if variable name constains extra "]"
    return replace(replace(variable_name(v), "[" => "_{", count = 1), "]" => "}")
end
function_string(mode, func::SingleVariable, variable_name = default_name) = function_string(mode, func.variable, variable_name)
# Whether something is zero or not for the purposes of printing it
# oneunit is useful e.g. if coef is a Unitful quantity. The second `abs` is import if it is complex.
_is_zero_for_printing(coef) = abs(coef) < 1e-10 * abs(oneunit(coef))
# Whether something is one or not for the purposes of printing it.
_is_one_for_printing(coef) = _is_zero_for_printing(abs(coef) - oneunit(coef))
_is_one_for_printing(coef::Complex) = _is_one_for_printing(real(coef)) && _is_zero_for_printing(imag(coef))
_sign_string(coef) = coef < zero(coef) ? " - " : " + "
function function_string(mode, func::ScalarAffineFunction, variable_name = default_name; show_constant=true)
    # If the expression is empty, return the constant (or 0)
    if isempty(func.terms)
        return show_constant ? _string_round(constant(func)) : "0"
    end

    term_str = Vector{String}(undef, 2length(func.terms))
    elm = 1

    for term in func.terms
    pre = _is_one_for_printing(term.coefficient) ? "" : _string_round(term.coefficient) * " "

        term_str[2 * elm - 1] = "+"
        term_str[2 * elm] = string(pre, function_string(mode, term.variable_index, variable_name))
        elm += 1
    end

    if elm == 1
        # Will happen with cancellation of all terms
        # We should just return the constant, if its desired
        return show_constant ? _string_round(a.constant) : "0"
    else
        # Correction for very first term - don't want a " + "/" - "
        term_str[1] = (term_str[1] == " - ") ? "-" : ""
        ret = join(term_str[1 : 2 * (elm - 1)])
        if !_is_zero_for_printing(constant(func)) && show_constant
            ret = string(ret, "+",
                         _string_round(constant(func)))
        end
        return ret
    end
end
function Base.show(io::IO, f::AbstractScalarFunction, variable_name = default_name)
    print(io, function_string(REPLMode, f, variable_name))
end
function Base.show(io::IO, ::MIME"text/latex", f::AbstractScalarFunction, variable_name = default_name)
    print(io, _wrap_in_math_mode(function_string(IJuliaMode, f, variable_name)))
end

function in_set_string(print_mode, set::LessThan)
    return string(_math_symbol(print_mode, :leq), " ", set.upper)
end

function in_set_string(print_mode, set::GreaterThan)
    return string(_math_symbol(print_mode, :geq), " ", set.lower)
end

function in_set_string(print_mode, set::EqualTo)
    return string(_math_symbol(print_mode, :eq), " ", set.value)
end

function in_set_string(print_mode, set::Interval)
    return string(_math_symbol(print_mode, :in), " ",
                  _math_symbol(print_mode, :open_rng), set.lower, ", ",
                  set.upper, _math_symbol(print_mode, :close_rng))
end

in_set_string(print_mode, ::ZeroOne) = "binary"
in_set_string(print_mode, ::Integer) = "integer"

# TODO: Consider fancy latex names for some sets. They're currently printed as
# regular text in math mode which looks a bit awkward.
"""
    in_set_string(print_mode::Type{<:MOI.PrintMode},
                  set::MOI.AbstractSet)

Return a `String` representing the membership to the set `set` using print mode
`print_mode`.
"""
function in_set_string(print_mode, set::AbstractSet)
    return string(_math_symbol(print_mode, :in), " ", set)
end
