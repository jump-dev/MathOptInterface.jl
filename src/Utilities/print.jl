using Printf

_drop_moi(s) = replace(string(s), "MathOptInterface." => "")

struct _PrintOptions{T<:MIME}
    simplify_coefficients::Bool
    default_name::String
    print_types::Bool

    """
        _PrintOptions(
            mime::MIME;
            simplify_coefficients::Bool = false,
            default_name::String = "v",
            print_types::Bool = true,
        )

    A struct to control options for printing.

    ## Arguments

     * `simplify_coefficients` : Simplify coefficients if possible by omitting
       them or removing trailing zeros.
     * `default_name` : The name given to variables with an empty name.
     * `print_types` : Print the MOI type of each function and set for clarity.
    """
    function _PrintOptions(
        mime::MIME;
        simplify_coefficients::Bool = false,
        default_name::String = "v",
        print_types::Bool = true,
    )
        return new{typeof(mime)}(
            simplify_coefficients,
            default_name,
            print_types,
        )
    end
end

function _to_string(mime::MIME, args...; kwargs...)
    return _to_string(_PrintOptions(mime), args...; kwargs...)
end

#------------------------------------------------------------------------
# Math Symbols
#------------------------------------------------------------------------

# REPL-specific symbols
# Anything here: https://en.wikipedia.org/wiki/Windows-1252
# should probably work fine on Windows

_to_string(::_PrintOptions, ::typeof(*)) = "*"
_to_string(::_PrintOptions{MIME"text/latex"}, ::typeof(*)) = "\\times "

function _to_string(::_PrintOptions, ::typeof(^), n::Int)
    return n == 2 ? "²" : string('^', n)
end

function _to_string(::_PrintOptions{MIME"text/latex"}, ::typeof(^), n::Int)
    return string('^', n)
end

_to_string(::_PrintOptions, ::typeof(in)) = @static Sys.iswindows() ? "in" : "∈"

#------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    v::MOI.VariableIndex,
)
    var_name = MOI.get(model, MOI.VariableName(), v)
    if isempty(var_name)
        return string(options.default_name, "[", v.value, "]")
    else
        return var_name
    end
end

function _to_string(
    options::_PrintOptions{MIME"text/latex"},
    model::MOI.ModelLike,
    v::MOI.VariableIndex,
)
    var_name = MOI.get(model, MOI.VariableName(), v)
    if isempty(var_name)
        return string(options.default_name, "_{", v.value, "}")
    end
    # We need to escape latex math characters that appear in the name.
    # However, it's probably impractical to catch everything, so let's just
    # escape the common ones:
    # Escape underscores to prevent them being treated as subscript markers.
    var_name = replace(var_name, "_" => "\\_")
    # Escape carets to prevent them being treated as superscript markers.
    var_name = replace(var_name, "^" => "\\^")
    # Convert any x[args] to x_{args} so that indices on x print as subscripts.
    m = match(r"^(.*)\[(.+)\]$", var_name)
    if m !== nothing
        var_name = m[1] * "_{" * m[2] * "}"
    end
    return var_name
end

function _shorten(options::_PrintOptions, x::Float64)
    if options.simplify_coefficients && isinteger(x)
        return string(round(Int, x))
    end
    return string(x)
end

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    f::MOI.SingleVariable,
)
    return _to_string(options, model, f.variable)
end

"""
    _to_string(options::_PrintOptions, c::Float64, x::String)

Write a coefficient-name pair to string. There are a few cases to handle.

          | is_first | !is_first
    -----------------------------
    +2.1x | "2.1 x"  | " + 2.1 x"
    -2.1x | "-2.1 x" | " - 2.1 x"
    -----------------------------
    +2.0x | "2 x"    | " + 2 x"
    -2.0x | "-2 x"   | " - 2 x"
    +1.0x | "x"      | " + x"
    -1.0x | "-x"     | " - x"
"""
function _to_string(
    options::_PrintOptions,
    c::Float64,
    x::String;
    is_first::Bool,
)
    prefix = if is_first
        c < 0 ? "-" : ""
    else
        c < 0 ? " - " : " + "
    end
    s = _shorten(options, abs(c))
    if options.simplify_coefficients && s == "1"
        return string(prefix, x)
    else
        return string(prefix, s, " ", x)
    end
end

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    term::MOI.ScalarAffineTerm;
    is_first::Bool,
)
    name = _to_string(options, model, term.variable)
    return _to_string(options, term.coefficient, name; is_first = is_first)
end

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    f::MOI.ScalarAffineFunction,
)
    s = _shorten(options, f.constant)
    if options.simplify_coefficients && iszero(f.constant)
        s = ""
    end
    is_first = isempty(s)
    for term in f.terms
        s *= _to_string(options, model, term; is_first = is_first)
        is_first = false
    end
    return s
end

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    term::MOI.ScalarQuadraticTerm;
    is_first::Bool,
)
    name_1 = _to_string(options, model, term.variable_1)
    name_2 = _to_string(options, model, term.variable_2)
    # Be careful here when printing the coefficient. ScalarQuadraticFunction
    # assumes an additional 0.5 factor!
    coef = term.coefficient
    name = if term.variable_1 == term.variable_2
        coef /= 2
        string(name_1, _to_string(options, ^, 2))
    else
        string(name_1, _to_string(options, *), name_2)
    end
    return _to_string(options, coef, name; is_first = is_first)
end

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    f::MOI.ScalarQuadraticFunction,
)
    s = _shorten(options, f.constant)
    if options.simplify_coefficients && iszero(f.constant)
        s = ""
    end
    is_first = isempty(s)
    for term in f.affine_terms
        s *= _to_string(options, model, term; is_first = is_first)
        is_first = false
    end
    for term in f.quadratic_terms
        s *= _to_string(options, model, term; is_first = is_first)
        is_first = false
    end
    return s
end

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    f::MOI.AbstractVectorFunction,
)
    rows = map(fi -> _to_string(options, model, fi), eachscalar(f))
    max_length = maximum(length.(rows))
    s = join(map(r -> string("│", rpad(r, max_length), "│"), rows), '\n')
    return string(
        "┌",
        rpad("", max_length),
        "┐\n",
        s,
        "\n└",
        rpad("", max_length),
        "┘",
    )
end

function _to_string(
    options::_PrintOptions{MIME"text/latex"},
    model::MOI.ModelLike,
    f::MOI.AbstractVectorFunction,
)
    return string(
        "\\begin{bmatrix}\n",
        join(
            map(fi -> _to_string(options, model, fi), eachscalar(f)),
            "\\\\\n",
        ),
        "\\end{bmatrix}",
    )
end

#------------------------------------------------------------------------
# Sets
#------------------------------------------------------------------------

function _to_string(options::_PrintOptions, set::MOI.LessThan)
    return string("<= ", _shorten(options, set.upper))
end

function _to_string(options::_PrintOptions{MIME"text/latex"}, set::MOI.LessThan)
    return string("\\le ", _shorten(options, set.upper))
end

function _to_string(options::_PrintOptions, set::MOI.GreaterThan)
    return string(">= ", _shorten(options, set.lower))
end

function _to_string(
    options::_PrintOptions{MIME"text/latex"},
    set::MOI.GreaterThan,
)
    return string("\\ge ", _shorten(options, set.lower))
end

function _to_string(options::_PrintOptions, set::MOI.EqualTo)
    return string("== ", _shorten(options, set.value))
end

function _to_string(options::_PrintOptions{MIME"text/latex"}, set::MOI.EqualTo)
    return string("= ", _shorten(options, set.value))
end

function _to_string(options::_PrintOptions, set::MOI.Interval)
    return string(
        _to_string(options, in),
        " [",
        _shorten(options, set.lower),
        ", ",
        _shorten(options, set.upper),
        "]",
    )
end

function _to_string(options::_PrintOptions{MIME"text/latex"}, set::MOI.Interval)
    return string(
        "\\in \\[",
        _shorten(options, set.lower),
        ", ",
        _shorten(options, set.upper),
        "\\]",
    )
end

function _to_string(options::_PrintOptions, ::MOI.ZeroOne)
    return string(_to_string(options, in), " {0, 1}")
end

_to_string(::_PrintOptions{MIME"text/latex"}, ::MOI.ZeroOne) = "\\in \\{0, 1\\}"

function _to_string(options::_PrintOptions, ::MOI.Integer)
    return string(_to_string(options, in), " ℤ")
end

function _to_string(::_PrintOptions{MIME"text/latex"}, ::MOI.Integer)
    return "\\in \\mathbb{Z}"
end

function _to_string(options::_PrintOptions, set::MOI.AbstractSet)
    return string(_to_string(options, in), " ", _drop_moi(set))
end

function _to_string(::_PrintOptions{MIME"text/latex"}, set::MOI.AbstractSet)
    set_str = replace(replace(_drop_moi(set), "{" => "\\{"), "}" => "\\}")
    return string("\\in \\text{", set_str, "}")
end

#------------------------------------------------------------------------
# Constraints
#------------------------------------------------------------------------

function _to_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    cref::MOI.ConstraintIndex,
)
    f = MOI.get(model, MOI.ConstraintFunction(), cref)
    s = MOI.get(model, MOI.ConstraintSet(), cref)
    return string(_to_string(options, model, f), " ", _to_string(options, s))
end

#------------------------------------------------------------------------
# Nonlinear constraints
#------------------------------------------------------------------------

"""
    _VariableNode

A type used to work-around the default printing of Julia expressions.

Without this type, if we subsititued the variable names into the expression
and then converted to a string, each variable would be printed with enclosing
`"`.

To work-around this, create a new type and overload `show`.
"""
struct _VariableNode
    x::String
end
Base.show(io::IO, x::_VariableNode) = print(io, x.x)

_replace_names(::_PrintOptions, ::MOI.ModelLike, x, ::Any) = x
function _replace_names(
    options::_PrintOptions,
    model::MOI.ModelLike,
    x::Expr,
    lookup,
)
    if x.head == :ref
        return get!(lookup, x.args[2]) do
            return _VariableNode(_to_string(options, model, x.args[2]))
        end
    else
        for (i, arg) in enumerate(x.args)
            x.args[i] = _replace_names(options, model, arg, lookup)
        end
    end
    return x
end

function _replace_nonlinear_latex(::_PrintOptions{MIME"text/latex"}, s::String)
    s = replace(s, " * " => " \\times ")
    s = replace(s, " >= " => " \\ge ")
    s = replace(s, " <= " => " \\le ")
    s = replace(s, " == " => " = ")
    return s
end
_replace_nonlinear_latex(::_PrintOptions, s::String) = s

function _print_nonlinear_constraints(
    io::IO,
    options::_PrintOptions{MIME"text/plain"},
    model::MOI.ModelLike,
    block::MOI.NLPBlockData,
)
    lookup = Dict{MOI.VariableIndex,_VariableNode}()
    if options.print_types
        println(io, "\nNonlinear")
    end
    has_expr = :ExprGraph in MOI.features_available(block.evaluator)
    for (i, bound) in enumerate(block.constraint_bounds)
        if has_expr
            ex = MOI.constraint_expr(block.evaluator, i)
            println(io, " ", _replace_names(options, model, ex, lookup))
        else
            println(io, " ", bound.lower, " <= g_$(i)(x) <= ", bound.upper)
        end
    end
end

function _print_nonlinear_constraints(
    io::IO,
    options::_PrintOptions{MIME"text/latex"},
    model::MOI.ModelLike,
    block::MOI.NLPBlockData,
)
    lookup = Dict{MOI.VariableIndex,_VariableNode}()
    if options.print_types
        println(io, " & \\text{Nonlinear} \\\\")
    end
    has_expr = :ExprGraph in MOI.features_available(block.evaluator)
    for (i, bound) in enumerate(block.constraint_bounds)
        if has_expr
            ex = MOI.constraint_expr(block.evaluator, i)
            nl_c = string(_replace_names(options, model, ex, lookup))
            println(io, " & ", _replace_nonlinear_latex(options, nl_c), " \\\\")
        else
            println(
                io,
                "& ",
                bound.lower,
                " \\le g_$(i)(x) \\le ",
                bound.upper,
                " \\\\",
            )
        end
    end
end

function _print_nonlinear_constraints(
    ::IO,
    ::_PrintOptions,
    ::MOI.ModelLike,
    ::Nothing,
)
    return
end

#------------------------------------------------------------------------
# ObjectiveFunction
#------------------------------------------------------------------------

function _objective_function_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    ::Nothing,
)
    F = MOI.get(model, MOI.ObjectiveFunctionType())
    f = MOI.get(model, MOI.ObjectiveFunction{F}())
    return _drop_moi(F), _to_string(options, model, f)
end

function _objective_function_string(
    options::_PrintOptions,
    model::MOI.ModelLike,
    block::MOI.NLPBlockData,
)
    lookup = Dict{MOI.VariableIndex,_VariableNode}()
    if block.has_objective
        f = "f(x)"
        if :ExprGraph in MOI.features_available(block.evaluator)
            ex = MOI.objective_expr(block.evaluator)
            f = string(_replace_names(options, model, ex, lookup))
            f = _replace_nonlinear_latex(options, f)
        end
        return "Nonlinear", f
    else
        return _objective_function_string(options, model, nothing)
    end
end

#------------------------------------------------------------------------
# MOI.ModelLike
#------------------------------------------------------------------------

function _nlp_block(model::MOI.ModelLike)
    try
        block = MOI.get(model, MOI.NLPBlock())
        if :ExprGraph in MOI.features_available(block.evaluator)
            MOI.initialize(block.evaluator, [:ExprGraph])
        end
        return block
    catch
        return nothing
    end
end

"""
    _print_model(
        io::IO,
        options::_PrintOptions{MIME"text/plain"},
        model::MOI.ModelLike,
    )

Print a plain-text formulation of `model` to `io`.
"""
function _print_model(
    io::IO,
    options::_PrintOptions{MIME"text/plain"},
    model::MOI.ModelLike,
)
    nlp_block = _nlp_block(model)
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        println(io, "Feasibility")
    else
        F, f = _objective_function_string(options, model, nlp_block)
        sense_s = sense == MOI.MIN_SENSE ? "Minimize" : "Maximize"
        if options.print_types
            println(io, sense_s, " ", F, ":\n ", f)
        else
            println(io, sense_s, ": ", f)
        end
    end
    println(io, "\nSubject to:")
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if options.print_types
            println(io, "\n$(_drop_moi(F))-in-$(_drop_moi(S))")
        end
        for cref in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            s = _to_string(options, model, cref)
            println(io, " ", replace(s, '\n' => "\n "))
        end
    end
    _print_nonlinear_constraints(io, options, model, nlp_block)
    return
end

"""
    _print_model(
        io::IO,
        options::_PrintOptions{MIME"text/latex"},
        model::MOI.ModelLike,
    )

Print a LaTeX formulation of `model` to `io`.
"""
function _print_model(
    io::IO,
    options::_PrintOptions{MIME"text/latex"},
    model::MOI.ModelLike,
)
    nlp_block = _nlp_block(model)
    println(io, "\$\$ \\begin{aligned}")
    sense = MOI.get(model, MOI.ObjectiveSense())
    if sense == MOI.FEASIBILITY_SENSE
        println(io, "\\text{feasibility}\\\\")
    else
        F, f = _objective_function_string(options, model, nlp_block)
        sense_s = sense == MOI.MIN_SENSE ? "min" : "max"
        println(io, "\\", sense_s, "\\quad & ", f, " \\\\")
    end
    println(io, "\\text{Subject to}\\\\")
    for (F, S) in MOI.get(model, MOI.ListOfConstraintTypesPresent())
        if options.print_types
            println(io, " & \\text{$(_drop_moi(F))-in-$(_drop_moi(S))} \\\\")
        end
        for cref in MOI.get(model, MOI.ListOfConstraintIndices{F,S}())
            println(io, " & ", _to_string(options, model, cref), " \\\\")
        end
    end
    _print_nonlinear_constraints(io, options, model, nlp_block)
    return print(io, "\\end{aligned} \$\$")
end

#------------------------------------------------------------------------
# Latex
#------------------------------------------------------------------------

struct _LatexModel{T<:MOI.ModelLike}
    model::T
    kwargs::Any
end

"""
    latex_formulation(model::MOI.ModelLike; kwargs...)

Wrap `model` in a type so that it can be pretty-printed as `text/latex` in a
notebook like IJulia, or in Documenter.

To render the model, end the cell with `latex_formulation(model)`, or call
`display(latex_formulation(model))` in to force the display of the model from
inside a function.

Possible keyword arguments are:

 * `simplify_coefficients` : Simplify coefficients if possible by omitting
   them or removing trailing zeros.
 * `default_name` : The name given to variables with an empty name.
 * `print_types` : Print the MOI type of each function and set for clarity.
"""
latex_formulation(model::MOI.ModelLike; kwargs...) = _LatexModel(model, kwargs)

function Base.show(io::IO, model::_LatexModel)
    return _print_model(
        io,
        _PrintOptions(MIME("text/latex"); model.kwargs...),
        model.model,
    )
end

Base.show(io::IO, ::MIME"text/latex", model::_LatexModel) = show(io, model)

function Base.print(model::MOI.ModelLike; kwargs...)
    for d in Base.Multimedia.displays
        if Base.Multimedia.displayable(d, "text/latex") &&
           startswith("$(typeof(d))", "IJulia.")
            return display(d, "text/latex", latex_formulation(model; kwargs...))
        end
    end
    return print(stdout, model; kwargs...)
end

function Base.print(io::IO, model::MOI.ModelLike; kwargs...)
    return _print_model(io, _PrintOptions(MIME("text/plain"); kwargs...), model)
end
