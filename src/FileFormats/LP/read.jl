# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

struct Cache{T}
    model::Model{T}
    variable_name_to_index::Dict{String,MOI.VariableIndex}
    variable_with_default_bound::Set{MOI.VariableIndex}
    function Cache(model::Model{T}) where {T}
        return new{T}(
            model,
            Dict{String,MOI.VariableIndex}(),
            Set{MOI.VariableIndex}(),
        )
    end
end

"""
    Base.read!(io::IO, model::FileFormats.LP.Model)

Read `io` in the LP file format and store the result in `model`.

This reader attempts to follow the CPLEX LP format, because others like the
lpsolve version are very...flexible...in how they accept input.

Read more about the format here:
 * http://lpsolve.sourceforge.net
 * https://web.mit.edu/lpsolve/doc/CPLEX-format.htm
"""
function Base.read!(io::IO, model::Model{T}) where {T}
    if !MOI.is_empty(model)
        error("Cannot read in file because model is not empty.")
    end
    state = LexerState(io)
    cache = Cache(model)
    keyword = :UNKNOWN
    while (token = peek(state, Token)) !== nothing
        if token.kind == _TOKEN_KEYWORD
            _ = read(state, Token)
            keyword = Symbol(token.value)
            continue
        elseif token.kind == _TOKEN_NEWLINE
            _ = read(state, Token)
            continue
        elseif keyword == :MINIMIZE
            MOI.set(cache.model, MOI.ObjectiveSense(), MOI.MIN_SENSE)
            _parse_objective(state, cache)
            keyword = :UNKNOWN
        elseif keyword == :MAXIMIZE
            MOI.set(cache.model, MOI.ObjectiveSense(), MOI.MAX_SENSE)
            _parse_objective(state, cache)
            keyword = :UNKNOWN
        elseif keyword == :CONSTRAINTS
            _parse_constraint(state, cache)
        elseif keyword == :BINARY
            x = _parse_variable(state, cache)
            MOI.add_constraint(cache.model, x, MOI.ZeroOne())
        elseif keyword == :INTEGER
            x = _parse_variable(state, cache)
            MOI.add_constraint(cache.model, x, MOI.Integer())
        elseif keyword == :BOUNDS
            _parse_bound(state, cache)
        elseif keyword == :SOS
            _parse_constraint(state, cache)
        elseif keyword == :END
            _throw_unexpected_token(
                state,
                token,
                "No file contents are allowed after `end`.",
            )
        else
            _throw_unexpected_token(
                state,
                token,
                "Parsing this section is not supported by the current reader.",
            )
        end
    end
    # if keyword != :END
    #     TODO(odow): decide if we should throw an error here.
    # end
    for x in cache.variable_with_default_bound
        MOI.add_constraint(model, x, MOI.GreaterThan(0.0))
    end
    return
end

"""
    const _KEYWORDS::Dict{String,Symbol}

The LP file format is very permissive in what it allows users to call the
various sections. Here is a dictionary that maps possible user words
(normalized to lowercase, even though users can use mixed case) to the section.

If you find new spellings for the section names, add them here.

Special handling is needed in the lexer for the keywords that contain spaces.
"""
const _KEYWORDS = Dict(
    # MAXIMIZE
    "max" => :MAXIMIZE,
    "maximize" => :MAXIMIZE,
    "maximise" => :MAXIMIZE,
    "maximum" => :MAXIMIZE,
    # MINIMIZE
    "min" => :MINIMIZE,
    "minimize" => :MINIMIZE,
    "minimise" => :MINIMIZE,
    "minimum" => :MINIMIZE,
    # CONSTRAINTS
    "subject to" => :CONSTRAINTS,
    "such that" => :CONSTRAINTS,
    "st" => :CONSTRAINTS,
    "s.t." => :CONSTRAINTS,
    "st." => :CONSTRAINTS,
    # BOUNDS
    "bounds" => :BOUNDS,
    "bound" => :BOUNDS,
    # INTEGER
    "gen" => :INTEGER,
    "general" => :INTEGER,
    "generals" => :INTEGER,
    "integer" => :INTEGER,
    "integers" => :INTEGER,
    # BINARY
    "bin" => :BINARY,
    "binary" => :BINARY,
    "binaries" => :BINARY,
    # SOS
    "sos" => :SOS,
    # END
    "end" => :END,
)

@enum(
    _TokenKind,
    _TOKEN_KEYWORD,
    _TOKEN_IDENTIFIER,
    _TOKEN_NUMBER,
    _TOKEN_ADDITION,
    _TOKEN_SUBTRACTION,
    _TOKEN_MULTIPLICATION,
    _TOKEN_DIVISION,
    _TOKEN_EXPONENT,
    _TOKEN_OPEN_BRACKET,
    _TOKEN_CLOSE_BRACKET,
    _TOKEN_GREATER_THAN,
    _TOKEN_LESS_THAN,
    _TOKEN_EQUAL_TO,
    _TOKEN_COLON,
    _TOKEN_IMPLIES,
    _TOKEN_NEWLINE,
    _TOKEN_UNKNOWN,
)

const _KIND_TO_MSG = Dict{_TokenKind,String}(
    _TOKEN_KEYWORD => "a keyword",
    _TOKEN_IDENTIFIER => "a variable name",
    _TOKEN_NUMBER => "a number",
    _TOKEN_ADDITION => "the symbol `+`",
    _TOKEN_SUBTRACTION => "the symbol `-`",
    _TOKEN_MULTIPLICATION => "the symbol `*`",
    _TOKEN_DIVISION => "the symbol `/`",
    _TOKEN_EXPONENT => "the symbol `^`",
    _TOKEN_OPEN_BRACKET => "the symbol `[`",
    _TOKEN_CLOSE_BRACKET => "the symbol `]`",
    _TOKEN_GREATER_THAN => "the symbol `>=`",
    _TOKEN_LESS_THAN => "the symbol `<=`",
    _TOKEN_EQUAL_TO => "the symbol `==`",
    _TOKEN_COLON => "the symbol `:`",
    _TOKEN_IMPLIES => "the symbol `->`",
    _TOKEN_NEWLINE => "a new line",
    _TOKEN_UNKNOWN => "some unknown symbol",
)

"""
    const _OPERATORS::Dict{Char,_TokenKind}

This dictionary is used to simplify the lexer for common operators.

These operators must not contain spaces.
"""
const _OPERATORS = Dict{Char,_TokenKind}(
    '+' => _TOKEN_ADDITION,
    '-' => _TOKEN_SUBTRACTION,
    '*' => _TOKEN_MULTIPLICATION,
    '/' => _TOKEN_DIVISION,
    '^' => _TOKEN_EXPONENT,
    '[' => _TOKEN_OPEN_BRACKET,
    ']' => _TOKEN_CLOSE_BRACKET,
    '>' => _TOKEN_GREATER_THAN,
    '<' => _TOKEN_LESS_THAN,
    '=' => _TOKEN_EQUAL_TO,
    ':' => _TOKEN_COLON,
    '\n' => _TOKEN_NEWLINE,
)

"""
    struct Token
        kind::_TokenKind
        value::Union{Nothing,String}
    end

This struct is used to represent each token from the lexer. The `value` is the
unprocessed value.
"""
struct Token
    kind::_TokenKind
    value::Union{Nothing,String}
    pos::Int
end

"""
    mutable struct LexerState
        io::IO
        peek_char::Union{Nothing,Char}
        peek_tokens::Vector{Token}
    end

A struct that is used to manage state when lexing.

It stores:

 * `io`: the IO object that we are streaming
 * `peek_char`: the next `Char` in the `io`
 * `peek_tokens`: the list of upcoming tokens that we have already peeked.
"""
mutable struct LexerState
    io::IO
    line::Int
    peek_char::Union{Nothing,Char}
    peek_tokens::Vector{Token}
    LexerState(io::IO) = new(io, 1, nothing, Token[])
end

"""
    struct UnexpectedToken <: Exception
        token::Token
    end

This error is thrown when we encounter an unexpected token when parsing the LP
file. No other information is available.
"""
struct UnexpectedToken <: Exception
    token::Token
    line::Int
    msg::String
end

function _throw_unexpected_token(state::LexerState, token::Token, msg::String)
    offset = min(40, token.pos)
    seek(state.io, token.pos - offset)
    line = String(read(state.io, 2 * offset))
    i = something(findprev('\n', line, offset-1), 0)
    j = something(findnext('\n', line, offset), length(line) + 1)
    help = string(line[(i+1):(j-1)], "\n", " "^(offset - i + - 1), "^\n", msg)
    return throw(UnexpectedToken(token, state.line, help))
end

function Base.showerror(io::IO, err::UnexpectedToken)
    return print(
        io,
        "Error parsing LP file. Got an unexpected token on line $(err.line):\n",
        err.msg,
    )
end

function _expect(state::LexerState, token::Token, kind::_TokenKind)
    if token.kind != kind
        _throw_unexpected_token(
            state,
            token,
            string("We expected this token to be ", _KIND_TO_MSG[kind]),
        )
    end
    return token
end

function Base.peek(state::LexerState, ::Type{Char})
    if state.peek_char === nothing && !eof(state.io)
        state.peek_char = read(state.io, Char)
    end
    return state.peek_char
end

function Base.read(state::LexerState, ::Type{Char})
    c = peek(state, Char)
    state.peek_char = nothing
    return c
end

function Base.read(state::LexerState, ::Type{Token})
    token = peek(state, Token, 1)
    if isempty(state.peek_tokens)
        _throw_unexpected_token(
            state,
            Token(_TOKEN_UNKNOWN, "EOF", position(state.io)),
            "Unexpected end to the file. We weren't finished yet.",
        )
    end
    popfirst!(state.peek_tokens)
    return token
end

function Base.read(state::LexerState, ::Type{Token}, kind::_TokenKind)
    token = read(state, Token)
    return _expect(state, token, kind)
end

# We're a bit more relaxed than typical, allowing any letter or digit, not just
# ASCII.
function _is_identifier(c::Char)
    return isletter(c) || isdigit(c) || c in "!\"#\$%&()/,.;?@_`'{}|~"
end

function _is_starting_identifier(c::Char)
    return isletter(c) || c in "!\"#\$%&(),;?@_`'{}|~"
end

_is_number(c::Char) = isdigit(c) || c in ('.', 'e', 'E', '+', '-')

function Base.peek(state::LexerState, ::Type{Token}, n::Int = 1)
    @assert n >= 1
    while length(state.peek_tokens) < n
        token = _peek_inner(state)
        if token === nothing
            return nothing
        end
        push!(state.peek_tokens, token)
    end
    return state.peek_tokens[n]
end

function _peek_inner(state::LexerState)
    while (c = peek(state, Char)) !== nothing
        pos = position(state.io)
        if c == '\n'
            state.line += 1
            _ = read(state, Char)
            return Token(_TOKEN_NEWLINE, nothing, pos)
        elseif isspace(c)  # Whitespace
            _ = read(state, Char)
        elseif c == '\\'  # Comment: backslash until newline
            while (c = read(state, Char)) !== nothing && c != '\n'
            end
        elseif isdigit(c) || (c == '-' && isdigit(peek(state, Char))) # Number
            buf = IOBuffer()
            while (c = peek(state, Char)) !== nothing && _is_number(c)
                write(buf, c)
                _ = read(state, Char)
            end
            return Token(_TOKEN_NUMBER, String(take!(buf)), pos)
        elseif _is_starting_identifier(c)  # Identifier / keyword
            buf = IOBuffer()
            while (c = peek(state, Char)) !== nothing && _is_identifier(c)
                write(buf, c)
                _ = read(state, Char)
            end
            val = String(take!(buf))
            l_val = lowercase(val)
            if l_val == "subject"
                t = peek(state, Token)
                if t.kind == _TOKEN_IDENTIFIER && lowercase(t.value) == "to"
                    _ = read(state, Token)  # Skip "to"
                    return Token(_TOKEN_KEYWORD, "CONSTRAINTS", pos)
                end
            elseif l_val == "such"
                t = peek(state, Token)
                if t.kind == _TOKEN_IDENTIFIER && lowercase(t.value) == "that"
                    _ = read(state, Token)  # Skip "such"
                    return Token(_TOKEN_KEYWORD, "CONSTRAINTS", pos)
                end
            end
            if (kw = get(_KEYWORDS, l_val, nothing)) !== nothing
                return Token(_TOKEN_KEYWORD, string(kw), pos)
            end
            return Token(_TOKEN_IDENTIFIER, val, pos)
        elseif (op = get(_OPERATORS, c, nothing)) !== nothing
            _ = read(state, Char) # Skip c
            if c == '-' && peek(state, Char) == '>'
                _ = read(state, Char)
                return Token(_TOKEN_IMPLIES, nothing, pos)
            elseif c == '=' && peek(state, Char) in ('<', '>')
                c = read(state, Char) # Allow =< and => as <= and >=
                return Token(_OPERATORS[c], nothing, pos)
            elseif c in ('<', '>', '=') && peek(state, Char) == '='
                _ = read(state, Char)  # Allow <=, >=, and ==
            end
            return Token(op, nothing, pos)
        else
            _throw_unexpected_token(
                state,
                Token(_TOKEN_UNKNOWN, "$c", pos),
                "This character is not supported an LP file.",
            )
        end
    end
    return
end

"""
    _next_token_is(state::LexerState, kind::_TokenKind, n::Int = 1)

A helper function to check if the token in `n` steps is of kind `kind`.
"""
function _next_token_is(state::LexerState, kind::_TokenKind, n::Int = 1)
    if (t = peek(state, Token, n)) !== nothing
        return t.kind == kind
    end
    return false
end

function _skip_newlines(state::LexerState)
    while _next_token_is(state, _TOKEN_NEWLINE)
        _ = read(state, Token)
    end
    return
end

# IDENTIFIER := "string"
#
#   There _are_ rules to what an identifier can be. We handle these when lexing.
#   Anything that makes it here is deemed acceptable.
function _parse_variable(state::LexerState, cache::Cache)::MOI.VariableIndex
    _skip_newlines(state)
    token = read(state, Token, _TOKEN_IDENTIFIER)
    x = get(cache.variable_name_to_index, token.value, nothing)
    if x !== nothing
        return x
    end
    x = MOI.add_variable(cache.model)
    if length(token.value) > get_options(cache.model).maximum_length
        error("Name exceeds maximum length: $(token.value)")
    end
    MOI.set(cache.model, MOI.VariableName(), x, token.value)
    cache.variable_name_to_index[token.value] = x
    push!(cache.variable_with_default_bound, x)
    return x
end

# NUMBER :=
#   "+" NUMBER
#   | "-" NUMBER
#   | "inf"
#   | "infinity"
#   | :(parse(T, x))
function _parse_number(state::LexerState, cache::Cache{T})::T where {T}
    _skip_newlines(state)
    token = read(state, Token)
    if token.kind == _TOKEN_ADDITION
        return _parse_number(state, cache)
    elseif token.kind == _TOKEN_SUBTRACTION
        return -_parse_number(state, cache)
    elseif token.kind == _TOKEN_IDENTIFIER
        v = lowercase(token.value)
        if v == "inf" || v == "infinity"
            return typemax(T)
        else
            _throw_unexpected_token(
                state,
                token,
                "We expected this to be a number.",
            )
        end
    end
    _expect(state, token, _TOKEN_NUMBER)
    ret = tryparse(T, token.value)
    if ret === nothing
        _throw_unexpected_token(
            state,
            token,
            "We expected this to be a number.",
        )
    end
    return ret
end

# QUAD_TERM :=
#   "+" QUAD_TERM
#   | "-" QUAD_TERM
#   | [NUMBER] [*] IDENTIFIER "^" "2"
#   | [NUMBER] [*] IDENTIFIER "*" IDENTIFIER
function _parse_quad_term(
    state::LexerState,
    cache::Cache{T},
    prefix::T,
) where {T}
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_ADDITION)
        _ = read(state, Token)
        return _parse_quad_term(state, cache, prefix)
    elseif _next_token_is(state, _TOKEN_SUBTRACTION)
        _ = read(state, Token)
        return _parse_quad_term(state, cache, -prefix)
    end
    coef = prefix
    if _next_token_is(state, _TOKEN_NUMBER)
        coef = prefix * _parse_number(state, cache)
    end
    if _next_token_is(state, _TOKEN_MULTIPLICATION)
        _skip_newlines(state)
        _ = read(state, Token)  # Skip optional multiplication
    end
    x1 = _parse_variable(state, cache)
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_EXPONENT)
        _ = read(state, Token) # ^
        _skip_newlines(state)
        n = read(state, Token, _TOKEN_NUMBER)
        if n.value != "2"
            _throw_unexpected_token(state, n, "Only `^ 2` is supported.")
        end
        return MOI.ScalarQuadraticTerm(T(2) * coef, x1, x1)
    end
    token = read(state, Token, _TOKEN_MULTIPLICATION)
    x2 = _parse_variable(state, cache)
    if x1 == x2
        coef *= T(2)
    end
    return MOI.ScalarQuadraticTerm(coef, x1, x2)
end

# QUADRATIC_EXPRESSION :=
#   "[" QUAD_TERM (("+" | "-") QUAD_TERM)* "]"
#   | "[" QUAD_TERM (("+" | "-") QUAD_TERM)* "]/2"
function _parse_quad_expression(
    state::LexerState,
    cache::Cache{T},
    prefix::T,
) where {T}
    token = read(state, Token, _TOKEN_OPEN_BRACKET)
    f = zero(MOI.ScalarQuadraticFunction{T})
    push!(f.quadratic_terms, _parse_quad_term(state, cache, prefix))
    while (p = peek(state, Token)) !== nothing
        if p.kind == _TOKEN_ADDITION
            p = read(state, Token)
            push!(f.quadratic_terms, _parse_quad_term(state, cache, prefix))
        elseif p.kind == _TOKEN_SUBTRACTION
            p = read(state, Token)
            push!(f.quadratic_terms, _parse_quad_term(state, cache, -prefix))
        elseif p.kind == _TOKEN_NEWLINE
            _ = read(state, Token)
        elseif p.kind == _TOKEN_CLOSE_BRACKET
            _ = read(state, Token)
            break
        else
            _throw_unexpected_token(
                state,
                p,
                "We expected this to be a ] to end the quadratic expresssion.",
            )
        end
    end
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_DIVISION)
        _ = read(state, Token) # /
        # Must be /2
        n = read(state, Token, _TOKEN_NUMBER)
        if n.value != "2"
            _throw_unexpected_token(
                state,
                n,
                "The only supported value here is `] / 2`.",
            )
        end
        for (i, term) in enumerate(f.quadratic_terms)
            f.quadratic_terms[i] = MOI.ScalarQuadraticTerm(
                term.coefficient / T(2),
                term.variable_1,
                term.variable_2,
            )
        end
    end
    return f
end

# TERM :=
#   "+" TERM
#   | "-" TERM
#   | NUMBER
#   | IDENTIFIER
#   | NUMBER IDENTIFIER
#   | NUMBER "*" IDENTIFIER
#   | QUADRATIC_EXPRESSION
function _parse_term(
    state::LexerState,
    cache::Cache{T},
    prefix::T = one(T),
) where {T}
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_ADDITION)
        # "+" TERM
        _ = read(state, Token, _TOKEN_ADDITION)
        return _parse_term(state, cache, prefix)
    elseif _next_token_is(state, _TOKEN_SUBTRACTION)
        # "-" TERM
        _ = read(state, Token, _TOKEN_SUBTRACTION)
        return _parse_term(state, cache, -prefix)
    elseif _next_token_is(state, _TOKEN_IDENTIFIER)
        # IDENTIFIER
        x = _parse_variable(state, cache)
        return MOI.ScalarAffineTerm(prefix, x)
    elseif _next_token_is(state, _TOKEN_NUMBER)
        coef = prefix * _parse_number(state, cache)
        if _next_token_is(state, _TOKEN_IDENTIFIER)
            # NUMBER IDENTIFIER
            x = _parse_variable(state, cache)
            return MOI.ScalarAffineTerm(coef, x)
        elseif _next_token_is(state, _TOKEN_MULTIPLICATION)
            # NUMBER * IDENTIFIER
            _ = read(state, Token, _TOKEN_MULTIPLICATION)
            x = _parse_variable(state, cache)
            return MOI.ScalarAffineTerm(coef, x)
        elseif _next_token_is(state, _TOKEN_NEWLINE) ||
               _next_token_is(state, _TOKEN_ADDITION) ||
               _next_token_is(state, _TOKEN_SUBTRACTION)
            # NUMBER
            return coef
        end
    elseif _next_token_is(state, _TOKEN_OPEN_BRACKET)
        # QUADRATIC_EXPRESSION
        return _parse_quad_expression(state, cache, prefix)
    end
    token = peek(state, Token)
    return _throw_unexpected_token(
        state,
        token,
        "Got $(_KIND_TO_MSG[token.kind]), but we expected this to be a new term in the expression.",
    )
end

function _add_to_expression!(f::MOI.ScalarQuadraticFunction{T}, x::T) where {T}
    f.constant += x
    return
end

function _add_to_expression!(
    f::MOI.ScalarQuadraticFunction{T},
    x::MOI.ScalarAffineTerm{T},
) where {T}
    push!(f.affine_terms, x)
    return
end

function _add_to_expression!(
    f::MOI.ScalarQuadraticFunction{T},
    x::MOI.ScalarQuadraticFunction{T},
) where {T}
    MOI.Utilities.operate!(+, T, f, x)
    return
end

# EXPRESSION :=
#   TERM (("+" | "-") TERM)*
function _parse_expression(state::LexerState, cache::Cache{T}) where {T}
    f = zero(MOI.ScalarQuadraticFunction{T})
    _add_to_expression!(f, _parse_term(state, cache))
    while (p = peek(state, Token)) !== nothing
        if p.kind == _TOKEN_ADDITION
            p = read(state, Token)
            _add_to_expression!(f, _parse_term(state, cache))
        elseif p.kind == _TOKEN_SUBTRACTION
            p = read(state, Token)
            _add_to_expression!(f, _parse_term(state, cache, -one(T)))
        elseif p.kind == _TOKEN_NEWLINE
            _ = read(state, Token)
        else
            break
        end
    end
    if isempty(f.quadratic_terms)
        return MOI.ScalarAffineFunction(f.affine_terms, f.constant)
    end
    return f
end

# SET_SUFFIX :=
#   "free"
#   | ">=" NUMBER
#   | "<=" NUMBER
#   | "==" NUMBER
#
# There are other inequality operators that are supported, like `>`, `<`, and
# `=`. These are normalized when lexing.
function _parse_set_suffix(state, cache)
    _skip_newlines(state)
    p = read(state, Token)
    if p.kind == _TOKEN_IDENTIFIER && lowercase(p.value) == "free"
        return nothing
    end
    _skip_newlines(state)
    if p.kind == _TOKEN_GREATER_THAN
        rhs = _parse_number(state, cache)
        return MOI.GreaterThan(rhs)
    elseif p.kind == _TOKEN_LESS_THAN
        rhs = _parse_number(state, cache)
        return MOI.LessThan(rhs)
    elseif p.kind == _TOKEN_EQUAL_TO
        rhs = _parse_number(state, cache)
        return MOI.EqualTo(rhs)
    else
        _throw_unexpected_token(
            state,
            p,
            "We expected this to be an inequality like `>=`, `<=` ,or `==`.",
        )
    end
end

# SET_PREFIX :=
#   NUMBER ">="
#   | NUMBER "<="
#   | NUMBER "=="
#
# There are other inequality operators that are supported, like `>`, `<`, and
# `=`. These are normalized when lexing.
function _parse_set_prefix(state, cache)
    lhs = _parse_number(state, cache)
    _skip_newlines(state)
    p = read(state, Token)
    if p.kind == _TOKEN_GREATER_THAN
        return MOI.LessThan(lhs)
    elseif p.kind == _TOKEN_LESS_THAN
        return MOI.GreaterThan(lhs)
    elseif p.kind == _TOKEN_EQUAL_TO
        return MOI.EqualTo(lhs)
    else
        _throw_unexpected_token(
            state,
            p,
            "We expected this to be an inequality like `>=`, `<=` ,or `==`.",
        )
    end
end

# NAME := [IDENTIFIER :]
function _parse_optional_name(state::LexerState, cache::Cache)
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_IDENTIFIER, 1) &&
       _next_token_is(state, _TOKEN_COLON, 2)
        name = read(state, Token)
        _ = read(state, Token)  # Skip :
        return name.value
    end
    return nothing
end

# OBJECTIVE := [NAME] [EXPRESSION]
function _parse_objective(state::LexerState, cache::Cache)
    _ = _parse_optional_name(state, cache)
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_KEYWORD)
        return  # A line like `obj:\nsubject to`
    end
    f = _parse_expression(state, cache)
    MOI.set(cache.model, MOI.ObjectiveFunction{typeof(f)}(), f)
    return
end

function _add_bound(cache::Cache, x::MOI.VariableIndex, set::MOI.GreaterThan)
    delete!(cache.variable_with_default_bound, x)
    if isfinite(set.lower)
        MOI.add_constraint(cache.model, x, set)
    end
    return
end

function _add_bound(cache::Cache, x::MOI.VariableIndex, set::MOI.LessThan)
    if set.upper < 0
        delete!(cache.variable_with_default_bound, x)
    end
    if isfinite(set.upper)
        MOI.add_constraint(cache.model, x, set)
    end
    return
end

function _add_bound(cache::Cache, x::MOI.VariableIndex, set::MOI.EqualTo)
    delete!(cache.variable_with_default_bound, x)
    MOI.add_constraint(cache.model, x, set)
    return
end

# x free
function _add_bound(cache::Cache, x::MOI.VariableIndex, ::Nothing)
    delete!(cache.variable_with_default_bound, x)
    return
end

# BOUND :=
#   IDENFITIER SET_SUFFIX
#   | SET_PREFIX IDENTIFIER
#   | SET_PREFIX IDENTIFIER SET_SUFFIX
function _parse_bound(state, cache)
    if _next_token_is(state, _TOKEN_IDENTIFIER)  # `x free` or `x op b`
        x = _parse_variable(state, cache)
        set = _parse_set_suffix(state, cache)
        _add_bound(cache, x, set)
        return
    end
    # `a op x` or `a op x op b`
    lhs_set = _parse_set_prefix(state, cache)
    x = _parse_variable(state, cache)
    _add_bound(cache, x, lhs_set)
    if _next_token_is(state, _TOKEN_GREATER_THAN) ||
       _next_token_is(state, _TOKEN_LESS_THAN) ||
       _next_token_is(state, _TOKEN_EQUAL_TO)  # `a op x op b`
        # We don't add MOI.Interval constraints to follow JuMP's convention of
        # separate bounds.
        rhs_set = _parse_set_suffix(state, cache)
        _add_bound(cache, x, rhs_set)
    end
    return
end

function _is_sos_constraint(state)
    return _next_token_is(state, _TOKEN_IDENTIFIER, 1) &&
           _next_token_is(state, _TOKEN_COLON, 2) &&
           _next_token_is(state, _TOKEN_COLON, 3)
end

# SOS_CONSTRAINT :=
#   [NAME] S1:: (IDENTIFIER:NUMBER)+ \n
#   | [NAME] S2:: (IDENTIFIER:NUMBER)+ \n
#
# The newline character is required.
function _parse_sos_constraint(state::LexerState, cache::Cache{T}) where {T}
    t = read(state, Token, _TOKEN_IDENTIFIER) # Si
    if !(t.value == "S1" || t.value == "S2")
        _throw_unexpected_token(
            state,
            t,
            "This must be either `S1` for SOS-I or `S2` for SOS-II.",
        )
    end
    _ = read(state, Token, _TOKEN_COLON)
    _ = read(state, Token, _TOKEN_COLON)
    f, w = MOI.VectorOfVariables(MOI.VariableIndex[]), T[]
    while true
        if _next_token_is(state, _TOKEN_NEWLINE)
            t = peek(state, Token)
            _throw_unexpected_token(
                state,
                t,
                "SOS constraints cannot be spread across lines.",
            )
        end
        push!(f.variables, _parse_variable(state, cache))
        _ = read(state, Token, _TOKEN_COLON)
        push!(w, _parse_number(state, cache))
        if _next_token_is(state, _TOKEN_NEWLINE)
            break
        end
    end
    if t.value == "S1"
        return MOI.add_constraint(cache.model, f, MOI.SOS1(w))
    else
        return MOI.add_constraint(cache.model, f, MOI.SOS2(w))
    end
end

function _is_indicator_constraint(state)
    return _next_token_is(state, _TOKEN_IDENTIFIER, 1) &&
           _next_token_is(state, _TOKEN_EQUAL_TO, 2) &&
           _next_token_is(state, _TOKEN_NUMBER, 3) &&
           _next_token_is(state, _TOKEN_IMPLIES, 4)
end

# INDICATOR_CONSTRAINT :=
#   IDENTIFIER "=" "0" "->" EXPRESSION SET_SUFFIX
#   | IDENTIFIER "=" "1" "->" EXPRESSION SET_SUFFIX
function _parse_indicator_constraint(
    state::LexerState,
    cache::Cache{T},
) where {T}
    z = _parse_variable(state, cache)
    _ = read(state, Token, _TOKEN_EQUAL_TO)
    t = read(state, Token, _TOKEN_NUMBER)
    indicator = if t.value == "0"
        MOI.ACTIVATE_ON_ZERO
    elseif t.value == "1"
        MOI.ACTIVATE_ON_ONE
    else
        _throw_unexpected_token(state, t, "This must be either `= 0` or `= 1`.")
    end
    _ = read(state, Token, _TOKEN_IMPLIES)
    f = _parse_expression(state, cache)
    set = _parse_set_suffix(state, cache)
    return MOI.add_constraint(
        cache.model,
        MOI.Utilities.operate(vcat, T, z, f),
        MOI.Indicator{indicator}(set),
    )
end

# CONSTRAINT :=
#   [NAME] EXPRESSION SET_SUFFIX
#   | [NAME] SOS_CONSTRAINT
#   | [NAME] INDICATOR_CONSTRAINT
function _parse_constraint(state::LexerState, cache::Cache)
    name = _parse_optional_name(state, cache)
    # Check if this is an SOS constraint
    c = if _is_sos_constraint(state)
        _parse_sos_constraint(state, cache)
    elseif _is_indicator_constraint(state)
        _parse_indicator_constraint(state, cache)
    else
        f = _parse_expression(state, cache)
        set = _parse_set_suffix(state, cache)
        MOI.add_constraint(cache.model, f, set)
    end
    if name !== nothing
        MOI.set(cache.model, MOI.ConstraintName(), c, name)
    end
    return
end
