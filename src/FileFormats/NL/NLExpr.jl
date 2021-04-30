### ============================================================================
### Opcodes, and their AMPL <-> Julia conversions.
### ============================================================================

include("opcode.jl")

"""
    _JULIA_TO_AMPL

This dictionary is manualy curated, based on the list of opcodes in `opcode.jl`.

The goal is to map Julia functions to their AMPL opcode equivalent.

Sometimes, there is ambiguity, such as the `:+`, which Julia uses for unary,
binary, and n-ary addition, while AMPL doesn't support unary addition, uses
OPPLUS for binary, and OPSUMLIST for n-ary. In these cases, introduce a
different symbol to disambiguate them in the context of this dictionary, and add
logic to `_process_expr!` to rewrite the Julia expression.

Commented out lines are opcodes supported by AMPL that don't have a clear Julia
equivalent. If you can think of one, feel free to add it. But then go and make
similar changes to `_AMPL_TO_JULIA` and `_NARY_OPCODES`.
"""
const _JULIA_TO_AMPL = Dict{Symbol,Int}(
    :+ => OPPLUS,  # binary-plus
    :- => OPMINUS,
    :* => OPMULT,
    :/ => OPDIV,
    :rem => OPREM,
    :^ => OPPOW,
    # OPLESS = 6
    :min => MINLIST,  # n-ary
    :max => MAXLIST,  # n-ary
    # FLOOR = 13
    # CEIL = 14
    :abs => ABS,
    :neg => OPUMINUS,
    :|| => OPOR,
    :&& => OPAND,
    :(<) => LT,
    :(<=) => LE,
    :(==) => EQ,
    :(>=) => GE,
    :(>) => GT,
    :(!=) => NE,
    :(!) => OPNOT,
    :ifelse => OPIFnl,
    :tanh => OP_tanh,
    :tan => OP_tan,
    :sqrt => OP_sqrt,
    :sinh => OP_sinh,
    :sin => OP_sin,
    :log10 => OP_log10,
    :log => OP_log,
    :exp => OP_exp,
    :cosh => OP_cosh,
    :cos => OP_cos,
    :atanh => OP_atanh,
    # OP_atan2 = 48,
    :atan => OP_atan,
    :asinh => OP_asinh,
    :asin => OP_asin,
    :acosh => OP_acosh,
    :acos => OP_acos,
    :sum => OPSUMLIST,  # n-ary plus
    # OPintDIV = 55
    # OPprecision = 56
    # OPround = 57
    # OPtrunc = 58
    # OPCOUNT = 59
    # OPNUMBEROF = 60
    # OPNUMBEROFs = 61
    # OPATLEAST = 62
    # OPATMOST = 63
    # OPPLTERM = 64
    # OPIFSYM = 65
    # OPEXACTLY = 66
    # OPNOTATLEAST = 67
    # OPNOTATMOST = 68
    # OPNOTEXACTLY = 69
    # ANDLIST = 70
    # ORLIST = 71
    # OPIMPELSE = 72
    # OP_IFF = 73
    # OPALLDIFF = 74
    # OPSOMESAME = 75
    # OP1POW = 76
    # OP2POW = 77
    # OPCPOW = 78
    # OPFUNCALL = 79
    # OPNUM = 80
    # OPHOL = 81
    # OPVARVAL = 82
    # N_OPS = 83
)

"""
    _AMPL_TO_JULIA

This dictionary is manualy curated, based on the list of supported opcodes
`_JULIA_TO_AMPL`.

The goal is to map AMPL opcodes to their Julia equivalents. In addition, we need
to know the arity of each opcode.

If the opcode is an n-ary function, use `-1`.
"""
const _AMPL_TO_JULIA = Dict{Int,Tuple{Int,Function}}(
    OPPLUS => (2, +),
    OPMINUS => (2, -),
    OPMULT => (2, *),
    OPDIV => (2, /),
    OPREM => (2, rem),
    OPPOW => (2, ^),
    # OPLESS = 6
    MINLIST => (-1, minimum),
    MAXLIST => (-1, maximum),
    # FLOOR = 13
    # CEIL = 14
    ABS => (1, abs),
    OPUMINUS => (1, -),
    OPOR => (2, |),
    OPAND => (2, &),
    LT => (2, <),
    LE => (2, <=),
    EQ => (2, ==),
    GE => (2, >=),
    GT => (2, >),
    NE => (2, !=),
    OPNOT => (1, !),
    OPIFnl => (3, ifelse),
    OP_tanh => (1, tanh),
    OP_tan => (1, tan),
    OP_sqrt => (1, sqrt),
    OP_sinh => (1, sinh),
    OP_sin => (1, sin),
    OP_log10 => (1, log10),
    OP_log => (1, log),
    OP_exp => (1, exp),
    OP_cosh => (1, cosh),
    OP_cos => (1, cos),
    OP_atanh => (1, atanh),
    # OP_atan2 = 48,
    OP_atan => (1, atan),
    OP_asinh => (1, asinh),
    OP_asin => (1, asin),
    OP_acosh => (1, acosh),
    OP_acos => (1, acos),
    OPSUMLIST => (-1, sum),
    # OPintDIV = 55
    # OPprecision = 56
    # OPround = 57
    # OPtrunc = 58
    # OPCOUNT = 59
    # OPNUMBEROF = 60
    # OPNUMBEROFs = 61
    # OPATLEAST = 62
    # OPATMOST = 63
    # OPPLTERM = 64
    # OPIFSYM = 65
    # OPEXACTLY = 66
    # OPNOTATLEAST = 67
    # OPNOTATMOST = 68
    # OPNOTEXACTLY = 69
    # ANDLIST = 70
    # ORLIST = 71
    # OPIMPELSE = 72
    # OP_IFF = 73
    # OPALLDIFF = 74
    # OPSOMESAME = 75
    # OP1POW = 76
    # OP2POW = 77
    # OPCPOW = 78
    # OPFUNCALL = 79
    # OPNUM = 80
    # OPHOL = 81
    # OPVARVAL = 82
    # N_OPS = 83
)

"""
    _NARY_OPCODES

A manually curated list of n-ary opcodes, taken from Table 8 of "Writing .nl
files."

Not all of these are implemented. See `_REV_OPCODES` for more detail.
"""
const _NARY_OPCODES = Set([
    MINLIST,
    MAXLIST,
    OPSUMLIST,
    OPCOUNT,
    OPNUMBEROF,
    OPNUMBEROFs,
    ANDLIST,
    ORLIST,
    OPALLDIFF,
])

"""
    _UNARY_SPECIAL_CASES

This dictionary defines a set of unary functions that are special-cased. They
don't exist in the NL file format, but they may be called from Julia, and
they can easily be converted into NL-compatible expressions.

If you have a new unary-function that you want to support, add it here.
"""
const _UNARY_SPECIAL_CASES = Dict{Symbol,Function}(
    :cbrt => (x) -> :($x^(1 / 3)),
    :abs2 => (x) -> :($x^2),
    :inv => (x) -> :(1 / $x),
    :log2 => (x) -> :(log($x) / log(2)),
    :log1p => (x) -> :(log(1 + $x)),
    :exp2 => (x) -> :(2^$x),
    :expm1 => (x) -> :(exp($x) - 1),
    :sec => (x) -> :(1 / cos($x)),
    :csc => (x) -> :(1 / sin($x)),
    :cot => (x) -> :(1 / tan($x)),
    :asec => (x) -> :(acos(1 / $x)),
    :acsc => (x) -> :(asin(1 / $x)),
    :acot => (x) -> :(pi / 2 - atan($x)),
    :sind => (x) -> :(sin(pi / 180 * $x)),
    :cosd => (x) -> :(cos(pi / 180 * $x)),
    :tand => (x) -> :(tan(pi / 180 * $x)),
    :secd => (x) -> :(1 / cos(pi / 180 * $x)),
    :cscd => (x) -> :(1 / sin(pi / 180 * $x)),
    :cotd => (x) -> :(1 / tan(pi / 180 * $x)),
    :asind => (x) -> :(asin($x) * 180 / pi),
    :acosd => (x) -> :(acos($x) * 180 / pi),
    :atand => (x) -> :(atan($x) * 180 / pi),
    :asecd => (x) -> :(acos(1 / $x) * 180 / pi),
    :acscd => (x) -> :(asin(1 / $x) * 180 / pi),
    :acotd => (x) -> :((pi / 2 - atan($x)) * 180 / pi),
    :sech => (x) -> :(1 / cosh($x)),
    :csch => (x) -> :(1 / sinh($x)),
    :coth => (x) -> :(1 / tanh($x)),
    :asech => (x) -> :(acosh(1 / $x)),
    :acsch => (x) -> :(asinh(1 / $x)),
    :acoth => (x) -> :(atanh(1 / $x)),
)

"""
    _BINARY_SPECIAL_CASES

This dictionary defines a set of binary functions that are special-cased. They
don't exist in the NL file format, but they may be called from Julia, and
they can easily be converted into NL-compatible expressions.

If you have a new binary-function that you want to support, add it here.
"""
const _BINARY_SPECIAL_CASES = Dict{Symbol,Function}(:\ => (x, y) -> :($y / $x))

### ============================================================================
### Nonlinear expressions
### ============================================================================

# TODO(odow): This type isn't great. We should experiment with something that is
# type-stable, like
#
# @enum(_NLType, _INTEGER, _DOUBLE, _VARIABLE)
# struct _NLTerm
#     type::_NLType
#     data::Int64
# end
# _NLTerm(x::Int) = _NLTerm(_INTEGER, x)
# _NLTerm(x::Float64) = _NLTerm(_DOUBLE, reinterpret(Int64, x))
# _NLTerm(x::MOI.VariableIndex) = _NLTerm(_VARIABLE, x.value)
# function _value(x::_NLTerm)
#     if x.type == _INTEGER
#         return x.data
#     elseif x.type == _DOUBLE
#         return reinterpret(Float64, x.data)
#     else
#         @assert x.type == _VARIABLE
#         return MOI.VariableIndex(x.data)
#     end
# end

const _NLTerm = Union{Int,Float64,MOI.VariableIndex}

struct _NLExpr
    is_linear::Bool
    nonlinear_terms::Vector{_NLTerm}
    linear_terms::Dict{MOI.VariableIndex,Float64}
    constant::Float64
end

function Base.:(==)(x::_NLExpr, y::_NLExpr)
    return x.is_linear == y.is_linear &&
           x.nonlinear_terms == y.nonlinear_terms &&
           x.linear_terms == y.linear_terms &&
           x.constant == y.constant
end

_NLExpr(x::MOI.VariableIndex) = _NLExpr(true, _NLTerm[], Dict(x => 1.0), 0.0)

_NLExpr(x::MOI.SingleVariable) = _NLExpr(x.variable)

function _add_or_set(dict, key, value)
    if haskey(dict, key)
        dict[key] += value
    else
        dict[key] = value
    end
    return
end

function _NLExpr(x::MOI.ScalarAffineFunction)
    linear = Dict{MOI.VariableIndex,Float64}()
    for (i, term) in enumerate(x.terms)
        _add_or_set(linear, term.variable_index, term.coefficient)
    end
    return _NLExpr(true, _NLTerm[], linear, x.constant)
end

function _NLExpr(x::MOI.ScalarQuadraticFunction)
    linear = Dict{MOI.VariableIndex,Float64}()
    for (i, term) in enumerate(x.affine_terms)
        _add_or_set(linear, term.variable_index, term.coefficient)
    end
    terms = _NLTerm[]
    N = length(x.quadratic_terms)
    if N == 0 || N == 1
        # If there are 0 or 1 terms, no need for an addition node.
    elseif N == 2
        # If there are two terms, use binary addition.
        push!(terms, OPPLUS)
    elseif N > 2
        # If there are more, use n-ary addition.
        push!(terms, OPSUMLIST)
        push!(terms, N)
    end
    for term in x.quadratic_terms
        coefficient = term.coefficient
        # MOI defines quadratic as 1/2 x' Q x :(
        if term.variable_index_1 == term.variable_index_2
            coefficient *= 0.5
        end
        # Optimization: no need for the OPMULT if the coefficient is 1.
        if !isone(coefficient)
            push!(terms, OPMULT)
            push!(terms, coefficient)
        end
        push!(terms, OPMULT)
        push!(terms, term.variable_index_1)
        push!(terms, term.variable_index_2)
        # For the Jacobian sparsity patterns, we need to add a linear term, even
        # if the variable only appears nonlinearly.
        _add_or_set(linear, term.variable_index_1, 0.0)
        _add_or_set(linear, term.variable_index_2, 0.0)
    end
    return _NLExpr(false, terms, linear, x.constant)
end

function _NLExpr(expr::Expr)
    nlexpr = _NLExpr(false, _NLTerm[], Dict{MOI.VariableIndex,Float64}(), 0.0)
    _process_expr!(nlexpr, expr)
    return nlexpr
end

function _process_expr!(expr::_NLExpr, arg::Real)
    return push!(expr.nonlinear_terms, Float64(arg))
end

# Assume the symbol is a numeric constant like `pi`.
_process_expr!(expr::_NLExpr, arg::Symbol) = _process_expr!(expr, eval(arg))

function _process_expr!(expr::_NLExpr, arg::MOI.VariableIndex)
    _add_or_set(expr.linear_terms, arg, 0.0)
    return push!(expr.nonlinear_terms, arg)
end

# TODO(odow): these process_expr! functions use recursion. For large models,
# this may exceed the stack. At some point, we may have to rewrite this to not
# use recursion.
function _process_expr!(expr::_NLExpr, arg::Expr)
    if arg.head == :call
        if length(arg.args) == 2
            f = get(_UNARY_SPECIAL_CASES, arg.args[1], nothing)
            if f !== nothing
                return _process_expr!(expr, f(arg.args[2]))
            end
        elseif length(arg.args) == 3
            f = get(_BINARY_SPECIAL_CASES, arg.args[1], nothing)
            if f !== nothing
                return _process_expr!(expr, f(arg.args[2], arg.args[3]))
            end
        end
        return _process_expr!(expr, arg.args)
    elseif arg.head == :ref
        return _process_expr!(expr, arg.args[2])
    elseif arg == :()
        return  # Some evalators return a null objective of `:()`.
    else
        error("Unsupported expression: $(arg)")
    end
    return
end

function _process_expr!(expr::_NLExpr, args::Vector{Any})
    op = first(args)
    N = length(args) - 1
    # Before processing the arguments, do some re-writing.
    if op == :+
        if N == 1  # +x, so we can just drop the op and process the args.
            return _process_expr!(expr, args[2])
        elseif N > 2  # nary-addition!
            op = :sum
        end
    elseif op == :- && N == 1
        op = :neg
    elseif op == :* && N > 2  # nary-multiplication.
        # NL doesn't define an nary multiplication operator, so we need to
        # rewrite our expression as a sequence of chained binary operators.
        while N > 2
            # Combine last term with previous to form a binary * expression
            arg = pop!(args)
            args[end] = Expr(:call, :*, args[end], arg)
            N = length(args) - 1
        end
    end
    # Now convert the Julia expression into an _NLExpr.
    opcode = get(_JULIA_TO_AMPL, op, nothing)
    if opcode === nothing
        error("Unsupported operation $(op)")
    end
    push!(expr.nonlinear_terms, opcode)
    if opcode in _NARY_OPCODES
        push!(expr.nonlinear_terms, N)
    end
    for i in 1:N
        _process_expr!(expr, args[i+1])
    end
    return
end

### ============================================================================
### Evaluate nonlinear expressions
### ============================================================================

function _evaluate(expr::_NLExpr, x::Dict{MOI.VariableIndex,Float64})
    y = expr.constant
    for (v, c) in expr.linear_terms
        y += c * x[v]
    end
    if length(expr.nonlinear_terms) > 0
        ret, n = _evaluate(expr.nonlinear_terms[1], expr.nonlinear_terms, x, 1)
        @assert n == length(expr.nonlinear_terms) + 1
        y += ret
    end
    return y
end

function _evaluate(
    head::MOI.VariableIndex,
    ::Vector{_NLTerm},
    x::Dict{MOI.VariableIndex,Float64},
    head_i::Int,
)::Tuple{Float64,Int}
    return x[head], head_i + 1
end

function _evaluate(
    head::Float64,
    ::Vector{_NLTerm},
    ::Dict{MOI.VariableIndex,Float64},
    head_i::Int,
)::Tuple{Float64,Int}
    return head, head_i + 1
end

function _evaluate(
    head::Int,
    terms::Vector{_NLTerm},
    x::Dict{MOI.VariableIndex,Float64},
    head_i::Int,
)::Tuple{Float64,Int}
    N, f = _AMPL_TO_JULIA[head]
    is_nary = (N == -1)
    head_i += 1
    if is_nary
        N = terms[head_i]::Int
        head_i += 1
    end
    args = Vector{Float64}(undef, N)
    for n in 1:N
        args[n], head_i = _evaluate(terms[head_i], terms, x, head_i)
    end
    return is_nary ? f(args) : f(args...), head_i
end
