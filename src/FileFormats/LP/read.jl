# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    _ReadCache(model::Model{T}) where {T}

This struct stores a few things to help reading the file:

 * `variable_name_to_index`: this maps variable names to their MOI index
 * `variable_with_default_bound`: by default, variables have a lower bound of
   `0`. When we read through the `Bounds` section, we remove from this set any
   variable that explicitly sets the lower bound, or that has an upper bound
   that is negative. At the end of `read!` we iterate through the remaining
   variables and add a lower bound of `0`.
"""
struct _ReadCache{T}
    model::Model{T}
    variable_name_to_index::Dict{String,MOI.VariableIndex}
    variable_with_default_bound::Set{MOI.VariableIndex}
    function _ReadCache(model::Model{T}) where {T}
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
    state = _LexerState(io)
    cache = _ReadCache(model)
    keyword = :UNKNOWN
    while (token = peek(state, _Token)) !== nothing
        if token.kind == _TOKEN_KEYWORD
            _ = read(state, _Token)
            keyword = Symbol(token.value)
        elseif token.kind == _TOKEN_NEWLINE
            _ = read(state, _Token, _TOKEN_NEWLINE)
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
            x = _parse_identifier(state, cache)
            MOI.add_constraint(cache.model, x, MOI.ZeroOne())
        elseif keyword == :INTEGER
            x = _parse_identifier(state, cache)
            MOI.add_constraint(cache.model, x, MOI.Integer())
        elseif keyword == :BOUNDS
            _parse_bound_expression(state, cache)
        elseif keyword == :SOS
            _parse_constraint(state, cache)
        elseif keyword == :END
            _throw_parse_error(
                state,
                token,
                "No file contents are allowed after `end`.",
            )
        else
            if token.kind == _TOKEN_IDENTIFIER
                # We didn't identify the token as an keyword during lexing. But
                # it might be one that is missing surrounding `\n`. Since our
                # alternative at this point is to throw an error, we ,might as
                # well attempt to see it can be interpreted as one.
                kw = _case_insenstive_identifier_to_keyword(token.value)
                if kw !== nothing
                    _ = read(state, _Token, _TOKEN_IDENTIFIER)
                    keyword = Symbol(kw)
                    continue
                elseif _compare_case_insenstive(token.value, "subject")
                    p = peek(state, _Token, 2)
                    if p !== nothing && p.kind == _TOKEN_IDENTIFIER
                        if _compare_case_insenstive(p.value, "to")
                            _ = read(state, _Token, _TOKEN_IDENTIFIER)
                            _ = read(state, _Token, _TOKEN_IDENTIFIER)
                            keyword = :CONSTRAINTS
                            continue
                        end
                    end
                elseif _compare_case_insenstive(token.value, "such")
                    p = peek(state, _Token, 2)
                    if p !== nothing && p.kind == _TOKEN_IDENTIFIER
                        if _compare_case_insenstive(p.value, "that")
                            _ = read(state, _Token, _TOKEN_IDENTIFIER)
                            _ = read(state, _Token, _TOKEN_IDENTIFIER)
                            keyword = :CONSTRAINTS
                            continue
                        end
                    end
                end
            end
            _expect(state, token, _TOKEN_KEYWORD)
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

# We want an efficient way to check if `test` is a case-insensitive version of
# `target`. We won't want `lowercase(test) == target` because this involves
# allocating a new string, and we check many identifiers to see if they are
# keywords.
function _compare_case_insenstive(test::String, target::String)
    if length(test) != length(target)
        return false
    end
    return all(lowercase(a) == b for (a, b) in zip(test, target))
end

function _compare_case_insenstive(input::String, c::Char, args)
    if lowercase(first(input)) != c
        return false
    end
    return any(_compare_case_insenstive(input, arg) for arg in args)
end

const _MAXIMIZE_KEYWORDS = ("max", "maximize", "maximise", "maximum")
const _MINIMIZE_KEYWORDS = ("min", "minimize", "minimise", "minimum")

"""
    _case_insenstive_identifier_to_keyword(input::String)

We need to check if identifiers are case insensitive keywords.

An obvious way to do this is something like `dict[lowercase(identifier)]`, but
this involves a moderately expensive `lowercase` operation and a dict lookup for
every identifier.

This function tries to be a little cleverer and doesn't allocate.
"""
function _case_insenstive_identifier_to_keyword(input::String)
    if !(2 <= length(input) <= 8)
        return nothing  # identifiers outside these lengths are not recognized
    elseif _compare_case_insenstive(input, 'm', _MAXIMIZE_KEYWORDS)
        return "MAXIMIZE"
    elseif _compare_case_insenstive(input, 'm', _MINIMIZE_KEYWORDS)
        return "MINIMIZE"
    elseif _compare_case_insenstive(input, 's', ("st", "s.t.", "st."))
        # `subject to` and `such that` handled in `peek`
        return "CONSTRAINTS"
    elseif _compare_case_insenstive(input, "sos")
        return "SOS"
    elseif _compare_case_insenstive(input, 'b', ("bound", "bounds"))
        return "BOUNDS"
    elseif _compare_case_insenstive(input, 'g', ("gen", "general", "generals"))
        return "INTEGER"
    elseif _compare_case_insenstive(input, 'i', ("integer", "integers"))
        return "INTEGER"
    elseif _compare_case_insenstive(input, 'b', ("bin", "binary", "binaries"))
        return "BINARY"
    elseif _compare_case_insenstive(input, "end")
        return "END"
    end
    return nothing
end

"""
    _TokenKind

This enum is the list of tokens that we might encounter when lexing the file.
Hopefully they're all self-explanatory.
"""
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

"""
    const _KIND_TO_MSG::Dict{_TokenKind,String}

This dictionary makes `_TokenKind` to a string that is used when printing error
messages. The string must complete the sentence "We expected this token to be ".
"""
const _KIND_TO_MSG = Dict{_TokenKind,String}(
    _TOKEN_KEYWORD => "a keyword defining a new section",
    _TOKEN_IDENTIFIER => "an identifier",
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
    _TOKEN_UNKNOWN => "a token",
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
    struct _Token
        kind::_TokenKind
        value::Union{Nothing,String}
        pos::Int
    end

This struct is used to represent each token from the lexer. The `value` is the
unprocessed value.

`pos` is the position of the `io::IO` in the lexer that begins this token. We
use the `pos` to provide nice error messages.
"""
struct _Token
    kind::_TokenKind
    value::Union{Nothing,String}
    pos::Int
end

function _compare_case_insenstive(test::_Token, target::String)
    if test.kind != _TOKEN_IDENTIFIER
        return false
    end
    return _compare_case_insenstive(test.value, target)
end

"""
    mutable struct _LexerState{O<:IO}
        io::O
        line::Int
        peek_char::Union{Nothing,Char}
        peek_tokens::Vector{_Token}
    end

A struct that is used to manage state when lexing. It stores:

 * `io`: the IO object that we are streaming
 * `line`: counts the number of `\n` characters, so that we can provide a nice
   error message to the user on a parse error
 * `peek_char`: the next `Char` in the `io`
 * `peek_tokens`: the list of upcoming tokens that we have already peeked
 * `current_token`: the most recent token that we have `read`
"""
mutable struct _LexerState{O<:IO}
    io::O
    line::Int
    peek_char::Union{Nothing,Char}
    peek_tokens::Vector{_Token}
    current_token::Union{Nothing,_Token}
    _LexerState(io::IO) = new{typeof(io)}(io, 1, nothing, _Token[], nothing)
end

"""
    struct ParseError <: Exception
        line::Int
        msg::String
    end

This error is thrown when we encounter an error parsing the LP file.
"""
struct ParseError <: Exception
    line::Int
    msg::String
end

_is_utf8_start(b::UInt8) = b < 0x80 || (0xC0 <= b <= 0xF7)

function _get_line_about_pos(io::IO, pos::Int, width::Int)
    seek(io, max(0, pos - width))
    # This byte might be an invalid or continuation byte. We need to seek
    # forward until we reach a new valid byte.
    while !_is_utf8_start(peek(io, UInt8))
        read(io, UInt8)
    end
    char = Char[]
    mark = 0
    while !eof(io) && position(io) <= pos + width
        c = read(io, Char)
        if c == '\n'
            if position(io) < pos
                empty!(char)
            else
                break
            end
        elseif c != '\r'
            push!(char, c)
        end
        if position(io) == pos
            mark = length(char)
        end
    end
    if mark == 0
        mark = length(char) + 1
    end
    return String(char), mark
end

function _throw_parse_error(state::_LexerState, token::_Token, msg::String)
    line, mark = _get_line_about_pos(state.io, token.pos, 40)
    help = string(
        line,
        "\n",
        " "^(mark - 1),
        "^\nGot ",
        _KIND_TO_MSG[token.kind],
        _with_value(token.value),
        ". ",
        msg,
    )
    return throw(ParseError(state.line, help))
end

function Base.showerror(io::IO, err::ParseError)
    return print(io, "Error parsing LP file on line $(err.line):\n", err.msg)
end

_with_value(::Nothing) = ""
_with_value(x::String) = string(" with value `", x, "`")

function _expect(state::_LexerState, token::_Token, kind::_TokenKind)
    if token.kind != kind
        _throw_parse_error(
            state,
            token,
            "We expected this token to be $(_KIND_TO_MSG[kind]).",
        )
    end
    return token
end

function Base.peek(state::_LexerState, ::Type{Char})
    if state.peek_char === nothing && !eof(state.io)
        state.peek_char = read(state.io, Char)
    end
    return state.peek_char
end

function Base.read(state::_LexerState, ::Type{Char})
    c = peek(state, Char)
    state.peek_char = nothing
    return c
end

function Base.read(state::_LexerState, ::Type{_Token})
    token = peek(state, _Token, 1)
    if isempty(state.peek_tokens)
        _throw_parse_error(
            state,
            _Token(_TOKEN_UNKNOWN, "EOF", position(state.io)),
            "Unexpected end to the file. We weren't finished yet.",
        )
    end
    popfirst!(state.peek_tokens)
    state.current_token = token
    if token.kind == _TOKEN_NEWLINE
        state.line += 1
    end
    return token
end

function Base.read(state::_LexerState, ::Type{_Token}, kind::_TokenKind)
    token = read(state, _Token)
    return _expect(state, token, kind)
end

_nothing_or_newline(::Nothing) = true
_nothing_or_newline(t::_Token) = t.kind == _TOKEN_NEWLINE

function _prior_token(state::_LexerState)
    if length(state.peek_tokens) <= 1
        return state.current_token
    end
    return state.peek_tokens[end-1]
end

function Base.peek(state::_LexerState, ::Type{_Token}, n::Int = 1)
    @assert n >= 1
    while length(state.peek_tokens) < n
        token = _peek_inner(state)
        if token === nothing
            return nothing
        end
        push!(state.peek_tokens, token)
        if token.kind != _TOKEN_IDENTIFIER
            continue
        end
        # Here we have a _TOKEN_IDENTIFIER. But if it is not preceeded by a
        # _TOKEN_NEWLINE, it cannot be a _TOKEN_KEYWORD.
        if !_nothing_or_newline(_prior_token(state))
            continue
        end
        # It might be a _TOKEN_KEYWORD.
        (kw = _case_insenstive_identifier_to_keyword(token.value))
        if kw !== nothing
            # The token matches a single word keyword. All keywords are followed
            # by a new line, or an EOF.
            t = _peek_inner(state)
            if _nothing_or_newline(t)
                state.peek_tokens[end] = _Token(_TOKEN_KEYWORD, kw, token.pos)
            end
            if t !== nothing
                push!(state.peek_tokens, t)
            end
            continue
        end
        # There are two keyword that contain whitespace: `subject to` and
        # `such that`
        for (a, b) in ("subject" => "to", "such" => "that")
            if !_compare_case_insenstive(token, a)
                continue
            end
            # This _might_ be `subject to`, or it might just be a variable
            # named `subject`, like `obj:\n subject\n`.
            token_b = _peek_inner(state)
            if token_b === nothing
                # The next token is EOF. Nothing to do here.
                break
            elseif !_compare_case_insenstive(token_b, b)
                # The second token doesn't match. Store `token_b` and break
                push!(state.peek_tokens, token_b)
                break
            end
            # We have something that matches (a, b), but a TOKEN_KEYWORD needs
            # to be followed by a new line.
            token_nl = _peek_inner(state)
            if _nothing_or_newline(token_nl)
                state.peek_tokens[end] =
                    _Token(_TOKEN_KEYWORD, "CONSTRAINTS", token.pos)
            else
                push!(state.peek_tokens, token_b)
            end
            if token_nl !== nothing
                push!(state.peek_tokens, token_nl)
            end
            break
        end
    end
    return state.peek_tokens[n]
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

function _peek_inner(state::_LexerState)
    while (c = peek(state, Char)) !== nothing
        pos = position(state.io)
        if c == '\n'
            _ = read(state, Char)
            return _Token(_TOKEN_NEWLINE, nothing, pos)
        elseif isspace(c)  # Whitespace
            _ = read(state, Char)
        elseif c == '\\'  # Comment: backslash until newline
            while (c = peek(state, Char)) !== nothing && c != '\n'
                _ = read(state, Char)
            end
        elseif isdigit(c) || (c == '-' && isdigit(peek(state, Char))) # Number
            buf = IOBuffer()
            while (c = peek(state, Char)) !== nothing && _is_number(c)
                write(buf, c)
                _ = read(state, Char)
            end
            return _Token(_TOKEN_NUMBER, String(take!(buf)), pos)
        elseif _is_starting_identifier(c)  # Identifier / keyword
            buf = IOBuffer()
            while (c = peek(state, Char)) !== nothing && _is_identifier(c)
                write(buf, c)
                _ = read(state, Char)
            end
            return _Token(_TOKEN_IDENTIFIER, String(take!(buf)), pos)
        elseif (op = get(_OPERATORS, c, nothing)) !== nothing
            _ = read(state, Char) # Skip c
            if c == '-' && peek(state, Char) == '>'
                _ = read(state, Char)
                return _Token(_TOKEN_IMPLIES, nothing, pos)
            elseif c == '=' && peek(state, Char) in ('<', '>')
                c = read(state, Char) # Allow =< and => as <= and >=
                return _Token(_OPERATORS[c], nothing, pos)
            elseif c in ('<', '>', '=') && peek(state, Char) == '='
                _ = read(state, Char)  # Allow <=, >=, and ==
            end
            return _Token(op, nothing, pos)
        elseif _is_identifier(c) && !_is_starting_identifier(c)
            _throw_parse_error(
                state,
                _Token(_TOKEN_UNKNOWN, "$c", pos),
                "This character is not supported at the start of an identifier.",
            )
        else
            _throw_parse_error(
                state,
                _Token(_TOKEN_UNKNOWN, "$c", pos),
                "This character is not supported in an LP file.",
            )
        end
    end
    return
end

"""
    _next_token_is(state::_LexerState, kind::_TokenKind, n::Int = 1)

A helper function to check if the token in `n` steps is of kind `kind`.
"""
function _next_token_is(state::_LexerState, kind::_TokenKind, n::Int = 1)
    if (t = peek(state, _Token, n)) !== nothing
        return t.kind == kind
    end
    return false
end

function _skip_newlines(state::_LexerState)
    while _next_token_is(state, _TOKEN_NEWLINE)
        _ = read(state, _Token, _TOKEN_NEWLINE)
    end
    return
end

function _next_non_newline(state::_LexerState)
    n = 1
    while true
        t = peek(state, _Token, n)
        if t === nothing
            return nothing
        elseif t.kind != _TOKEN_NEWLINE
            return t
        end
        n += 1
    end
end

# <identifier> :== "string"
#
#   There _are_ rules to what an identifier can be. We handle these when lexing.
#   Anything that makes it here is deemed acceptable.
function _parse_identifier(
    state::_LexerState,
    cache::_ReadCache,
)::MOI.VariableIndex
    _skip_newlines(state)
    token = read(state, _Token, _TOKEN_IDENTIFIER)
    x = get(cache.variable_name_to_index, token.value, nothing)
    if x !== nothing
        return x
    end
    x = MOI.add_variable(cache.model)
    len = get_options(cache.model).maximum_length
    if length(token.value) > len
        _throw_parse_error(
            state,
            token,
            "Name ($(token.value)) exceeds maximum length ($len)",
        )
    end
    MOI.set(cache.model, MOI.VariableName(), x, token.value)
    cache.variable_name_to_index[token.value] = x
    push!(cache.variable_with_default_bound, x)
    return x
end

# <number> :==
#   "+" <number>
#   | "-" <number>
#   | <digit>*[.(<digit>)*][("e" | "E")("+" | "-")(<digit>)+]
#   | i"inf"
#   | i"infinity"
function _parse_number(state::_LexerState, cache::_ReadCache{T})::T where {T}
    _skip_newlines(state)
    token = read(state, _Token)
    if token.kind == _TOKEN_ADDITION
        return _parse_number(state, cache)
    elseif token.kind == _TOKEN_SUBTRACTION
        return -_parse_number(state, cache)
    elseif token.kind == _TOKEN_IDENTIFIER
        if _compare_case_insenstive(token.value, 'i', ("inf", "infinity"))
            return typemax(T)
        end
        _throw_parse_error(state, token, "We expected this to be a number.")
    end
    _expect(state, token, _TOKEN_NUMBER)
    ret = tryparse(T, token.value)
    if ret === nothing
        _throw_parse_error(
            state,
            _Token(_TOKEN_IDENTIFIER, token.value, token.pos),
            "We were unable to parse this as a number.",
        )
    end
    return ret
end

# <quadratic-term> :==
#   "+" <quadratic-term>
#   | "-" <quadratic-term>
#   | [<number> [*]] <identifier> "^" "2"
#   | [<number> [*]] <identifier> "*" <identifier>
function _parse_quadratic_term(
    state::_LexerState,
    cache::_ReadCache{T},
    prefix::T,
) where {T}
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_ADDITION)
        _ = read(state, _Token)
        return _parse_quadratic_term(state, cache, prefix)
    elseif _next_token_is(state, _TOKEN_SUBTRACTION)
        _ = read(state, _Token)
        return _parse_quadratic_term(state, cache, -prefix)
    end
    coef = prefix
    if _next_token_is(state, _TOKEN_NUMBER)
        coef = prefix * _parse_number(state, cache)
    end
    if _next_token_is(state, _TOKEN_MULTIPLICATION)
        _skip_newlines(state)
        _ = read(state, _Token)  # Skip optional multiplication
    end
    x1 = _parse_identifier(state, cache)
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_EXPONENT)
        _ = read(state, _Token) # ^
        _skip_newlines(state)
        n = read(state, _Token, _TOKEN_NUMBER)
        if n.value != "2"
            _throw_parse_error(state, n, "Only `^ 2` is supported.")
        end
        return MOI.ScalarQuadraticTerm(T(2) * coef, x1, x1)
    end
    token = read(state, _Token, _TOKEN_MULTIPLICATION)
    x2 = _parse_identifier(state, cache)
    if x1 == x2
        coef *= T(2)
    end
    return MOI.ScalarQuadraticTerm(coef, x1, x2)
end

# <quadratic-expression> :==
#   "[" <quadratic-term> (("+" | "-") <quadratic-term>)* "]" ["/" "2"]
function _parse_quadratic_expression(
    state::_LexerState,
    cache::_ReadCache{T},
    prefix::T,
) where {T}
    token = read(state, _Token, _TOKEN_OPEN_BRACKET)
    f = zero(MOI.ScalarQuadraticFunction{T})
    push!(f.quadratic_terms, _parse_quadratic_term(state, cache, prefix))
    while (p = peek(state, _Token)) !== nothing
        if p.kind == _TOKEN_ADDITION
            p = read(state, _Token)
            push!(
                f.quadratic_terms,
                _parse_quadratic_term(state, cache, prefix),
            )
        elseif p.kind == _TOKEN_SUBTRACTION
            p = read(state, _Token)
            push!(
                f.quadratic_terms,
                _parse_quadratic_term(state, cache, -prefix),
            )
        elseif p.kind == _TOKEN_NEWLINE
            _ = read(state, _Token)
        elseif p.kind == _TOKEN_CLOSE_BRACKET
            _ = read(state, _Token)
            break
        else
            _throw_parse_error(
                state,
                p,
                "We expected this to be a ] to end the quadratic expresssion.",
            )
        end
    end
    t = _next_non_newline(state)
    if t !== nothing && t.kind == _TOKEN_DIVISION
        _skip_newlines(state)
        _ = read(state, _Token, _TOKEN_DIVISION) # /
        # Must be /2
        n = read(state, _Token, _TOKEN_NUMBER)
        if n.value != "2"
            _throw_parse_error(
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

# <term> :==
#     "+" <term>
#   | "-" <term>
#   | <identifier>
#   | <number>
#   | <number> <identifier>
#   | <number> "*" <identifier>
#   | <quadratic-expression>
function _parse_term(
    state::_LexerState,
    cache::_ReadCache{T},
    prefix::T = one(T),
) where {T}
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_ADDITION)
        # "+" <term>
        _ = read(state, _Token, _TOKEN_ADDITION)
        return _parse_term(state, cache, prefix)
    elseif _next_token_is(state, _TOKEN_SUBTRACTION)
        # "-" <term>
        _ = read(state, _Token, _TOKEN_SUBTRACTION)
        return _parse_term(state, cache, -prefix)
    elseif _next_token_is(state, _TOKEN_IDENTIFIER)
        # <identifier>
        x = _parse_identifier(state, cache)
        return MOI.ScalarAffineTerm(prefix, x)
    elseif _next_token_is(state, _TOKEN_NUMBER)
        coef = prefix * _parse_number(state, cache)
        if _next_token_is(state, _TOKEN_IDENTIFIER)
            # <number> <identifier>
            x = _parse_identifier(state, cache)
            return MOI.ScalarAffineTerm(coef, x)
        elseif _next_token_is(state, _TOKEN_MULTIPLICATION)
            # <number> * <identifier>
            _ = read(state, _Token, _TOKEN_MULTIPLICATION)
            x = _parse_identifier(state, cache)
            return MOI.ScalarAffineTerm(coef, x)
        elseif _next_token_is(state, _TOKEN_NEWLINE)
            # This could either be <number> \nEND-OF-<term>, or it could be a term
            # split by a new line, like `2\nx`.
            t = _next_non_newline(state)
            if t === nothing
                # <number>
                return coef
            elseif t.kind == _TOKEN_MULTIPLICATION
                # <number> \n * [\n] <identifier>
                _skip_newlines(state)
                _ = read(state, _Token, _TOKEN_MULTIPLICATION)
                _skip_newlines(state)
                x = _parse_identifier(state, cache)
                return MOI.ScalarAffineTerm(coef, x)
            elseif t.kind == _TOKEN_IDENTIFIER
                # <number> \n <identifier>
                x = _parse_identifier(state, cache)
                return MOI.ScalarAffineTerm(coef, x)
            end
        end
        # <number>
        return coef
    elseif _next_token_is(state, _TOKEN_OPEN_BRACKET)
        # <quadratic-expression>
        return _parse_quadratic_expression(state, cache, prefix)
    end
    token = peek(state, _Token)
    return _throw_parse_error(
        state,
        token,
        "We expected this to be a new term in the expression.",
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

# <expression> :== <term> (("+" | "-") <term>)*
function _parse_expression(state::_LexerState, cache::_ReadCache{T}) where {T}
    f = zero(MOI.ScalarQuadraticFunction{T})
    _add_to_expression!(f, _parse_term(state, cache))
    while (p = peek(state, _Token)) !== nothing
        if p.kind == _TOKEN_ADDITION
            p = read(state, _Token)
            _add_to_expression!(f, _parse_term(state, cache))
        elseif p.kind == _TOKEN_SUBTRACTION
            p = read(state, _Token)
            _add_to_expression!(f, _parse_term(state, cache, -one(T)))
        elseif p.kind == _TOKEN_NEWLINE
            if _next_token_is(state, _TOKEN_KEYWORD, 2)
                break
            end
            _ = read(state, _Token)
        else
            break
        end
    end
    if isempty(f.quadratic_terms)
        return MOI.ScalarAffineFunction(f.affine_terms, f.constant)
    end
    return f
end

# <set-suffix> :==
#   "free"
#   | ">=" <number>
#   | "<=" <number>
#   | "==" <number>
#
# There are other inequality operators that are supported, like `>`, `<`, and
# `=`. These are normalized when lexing.
function _parse_set_suffix(state, cache)
    _skip_newlines(state)
    p = read(state, _Token)
    if _compare_case_insenstive(p, "free")
        return nothing
    elseif p.kind == _TOKEN_GREATER_THAN
        rhs = _parse_number(state, cache)
        return MOI.GreaterThan(rhs)
    elseif p.kind == _TOKEN_LESS_THAN
        rhs = _parse_number(state, cache)
        return MOI.LessThan(rhs)
    elseif p.kind == _TOKEN_EQUAL_TO
        rhs = _parse_number(state, cache)
        return MOI.EqualTo(rhs)
    else
        _throw_parse_error(
            state,
            p,
            "We expected this to be an inequality like `>=`, `<=`, or `==`.",
        )
    end
end

# <set-prefix> :==
#   <number> ">="
#   | <number> "<="
#   | <number> "=="
#
# There are other inequality operators that are supported, like `>`, `<`, and
# `=`. These are normalized when lexing.
function _parse_set_prefix(state, cache)
    lhs = _parse_number(state, cache)
    _skip_newlines(state)
    p = read(state, _Token)
    if p.kind == _TOKEN_GREATER_THAN
        return MOI.LessThan(lhs)
    elseif p.kind == _TOKEN_LESS_THAN
        return MOI.GreaterThan(lhs)
    elseif p.kind == _TOKEN_EQUAL_TO
        return MOI.EqualTo(lhs)
    else
        _throw_parse_error(
            state,
            p,
            "We expected this to be an inequality like `>=`, `<=`, or `==`.",
        )
    end
end

# <name> :== [<identifier> :]
function _parse_name(state::_LexerState, cache::_ReadCache)
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_IDENTIFIER, 1) &&
       _next_token_is(state, _TOKEN_COLON, 2)
        name = read(state, _Token)
        _ = read(state, _Token)  # Skip :
        return name.value
    end
    return nothing
end

# <objective> :== <name> [<expression>]
function _parse_objective(state::_LexerState, cache::_ReadCache)
    _ = _parse_name(state, cache)
    _skip_newlines(state)
    if _next_token_is(state, _TOKEN_KEYWORD)
        return  # A line like `obj:\nsubject to`
    end
    f = _parse_expression(state, cache)
    MOI.set(cache.model, MOI.ObjectiveFunction{typeof(f)}(), f)
    return
end

function _add_bound(
    cache::_ReadCache,
    x::MOI.VariableIndex,
    set::MOI.GreaterThan,
)
    delete!(cache.variable_with_default_bound, x)
    if isfinite(set.lower)
        MOI.add_constraint(cache.model, x, set)
    end
    return
end

function _add_bound(cache::_ReadCache, x::MOI.VariableIndex, set::MOI.LessThan)
    if set.upper < 0
        delete!(cache.variable_with_default_bound, x)
    end
    if isfinite(set.upper)
        MOI.add_constraint(cache.model, x, set)
    end
    return
end

function _add_bound(cache::_ReadCache, x::MOI.VariableIndex, set::MOI.EqualTo)
    delete!(cache.variable_with_default_bound, x)
    MOI.add_constraint(cache.model, x, set)
    return
end

# x free
function _add_bound(cache::_ReadCache, x::MOI.VariableIndex, ::Nothing)
    delete!(cache.variable_with_default_bound, x)
    return
end

# <bound-expression> :==
#   <identifier> <set-suffix>
#   | <set-prefix> <identifier>
#   | <set-prefix> <identifier> <set-suffix>
function _parse_bound_expression(state, cache)
    if _next_token_is(state, _TOKEN_IDENTIFIER)  # `x free` or `x op b`
        x = _parse_identifier(state, cache)
        set = _parse_set_suffix(state, cache)
        _add_bound(cache, x, set)
        return
    end
    # `a op x` or `a op x op b`
    lhs_set = _parse_set_prefix(state, cache)
    x = _parse_identifier(state, cache)
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

# <constraint-sos> :==
#     S1:: (<identifier>:<number>)+
#   | S2:: (<identifier>:<number>)+
#
# New lines are not supported within the line.
function _parse_constraint_sos(
    state::_LexerState,
    cache::_ReadCache{T},
) where {T}
    t = read(state, _Token, _TOKEN_IDENTIFIER) # Si
    if !(t.value == "S1" || t.value == "S2")
        _throw_parse_error(
            state,
            t,
            "This must be either `S1` for SOS-I or `S2` for SOS-II.",
        )
    end
    _ = read(state, _Token, _TOKEN_COLON)
    _ = read(state, _Token, _TOKEN_COLON)
    f, w = MOI.VectorOfVariables(MOI.VariableIndex[]), T[]
    while true
        if _next_token_is(state, _TOKEN_NEWLINE)
            t = peek(state, _Token)
            _throw_parse_error(
                state,
                t,
                "SOS constraints cannot be spread across lines.",
            )
        end
        push!(f.variables, _parse_identifier(state, cache))
        _ = read(state, _Token, _TOKEN_COLON)
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

# <constraint-indicator> :==
#   <identifier> "=" ("0" | "1") "->" <expression> <set-suffix>
function _parse_constraint_indicator(
    state::_LexerState,
    cache::_ReadCache{T},
) where {T}
    z = _parse_identifier(state, cache)
    _ = read(state, _Token, _TOKEN_EQUAL_TO)
    t = read(state, _Token, _TOKEN_NUMBER)
    indicator = if t.value == "0"
        MOI.ACTIVATE_ON_ZERO
    elseif t.value == "1"
        MOI.ACTIVATE_ON_ONE
    else
        _throw_parse_error(state, t, "This must be either `= 0` or `= 1`.")
    end
    _ = read(state, _Token, _TOKEN_IMPLIES)
    f = _parse_expression(state, cache)
    set = _parse_set_suffix(state, cache)
    return MOI.add_constraint(
        cache.model,
        MOI.Utilities.operate(vcat, T, z, f),
        MOI.Indicator{indicator}(set),
    )
end

# <constraint> :==
#     <name> <expression> <set-suffix>
#   | <name> <constraint-sos>
#   | <name> <constraint-indicator>
function _parse_constraint(state::_LexerState, cache::_ReadCache)
    name = _parse_name(state, cache)
    # Check if this is an SOS constraint
    c = if _is_sos_constraint(state)
        _parse_constraint_sos(state, cache)
    elseif _is_indicator_constraint(state)
        _parse_constraint_indicator(state, cache)
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
