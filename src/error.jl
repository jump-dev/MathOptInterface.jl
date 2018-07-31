"""
    operation_name(err::Union{UnsupportedError, CannotError})

Return the name of the operation throwing the error in a gerund (i.e. -ing form).
"""
function operation_name end

"""
    UnsupportedError <: Exception

Abstract type for error thrown when an operation is not supported by the model.
"""
abstract type UnsupportedError <: Exception end

function Base.showerror(io::IO, err::UnsupportedError)
    print(io, typeof(err), ": ", operation_name(err), " is not supported by the the model.")
end

"""
    CannotError <: Exception

Abstract type for error thrown when an operation is supported but cannot be
applied in the current state of the model.
"""
abstract type CannotError <: Exception end

function Base.showerror(io::IO, err::CannotError)
    print(io, typeof(err), ": ", operation_name(err), " cannot be done in the current state of the model even if the operation is supported. You may want to use a `CachingOptimizer` in `Automatic` mode or you may need to call `resetoptimizer!` before doing this operation if the `CachingOptimizer` is in `Manual` mode.")
end
