# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

"""
    UnsupportedError <: Exception

Abstract type for error thrown when an element is not supported by the model.
"""
abstract type UnsupportedError <: Exception end

"""
    element_name(err::UnsupportedError)

Return the name of the element that is not supported.
"""
function element_name end

function Base.showerror(io::IO, err::UnsupportedError)
    print(
        io,
        typeof(err),
        ": ",
        element_name(err),
        " is not supported by the model",
    )
    m = message(err)
    if Base.isempty(m)
        print(io, ".")
    else
        print(io, ": ", m)
    end
end

"""
    NotAllowedError <: Exception

Abstract type for error thrown when an operation is supported but cannot be
applied in the current state of the model.
"""
abstract type NotAllowedError <: Exception end

"""
    operation_name(err::NotAllowedError)

Return the name of the operation throwing the error in a gerund (that is, -ing
form).
"""
function operation_name end

function Base.showerror(io::IO, err::NotAllowedError)
    println(io, typeof(err), ":\n")
    println(io, "## Cause\n")
    print(io, operation_name(err), " cannot be performed")
    m = message(err)
    if !isempty(m)
        println(io, " because:\n\n", m)
    end
    println(io)
    print(
        io,
        """
        ## Fixing this error

        An `MOI.NotAllowedError` error occurs when you have tried to do something that
        is not implemented by the solver.

        The most common way to fix this error is to wrap the optimizer in a
        `MOI.Utilities.CachingOptimizer`.

        For example, if you are using `JuMP.Model` or `JuMP.set_optimizer`, do:
        ```julia
        model = JuMP.Model(optimizer; with_cache_type = Float64)
        model = JuMP.GenericModel{T}(optimizer; with_cache_type = T)
        JuMP.set_optimizer(model, optimizer; with_cache_type = Float64)
        ```
        Similarly, if you are using `MOI.instantiate`, do:
        ```julia
        model = MOI.instantiate(optimizer; with_cache_type = Float64)
        ```
        """,
    )
    return
end

"""
    message(err::Union{UnsupportedError, NotAllowedError})

Return a `String` containing a human-friendly explanation of why the operation
is not supported/cannot be performed. It is printed in the error message if it
is not empty. By convention, it should be stored in the `message` field; if
this is the case, the `message` method does not have to be implemented.
"""
message(err::Union{UnsupportedError,NotAllowedError}) = err.message
