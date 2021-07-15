"""
    abstract type SetMapBridge{T,S1,S2} <: AbstractBridge end

Consider two type of sets `S1`, `S2` and a linear mapping `A` that
the image of a set of type `S1` under `A` is a set of type `S2`.
A `SetMapBridge{T,S2,S1,F,G}` is a bridge that substitutes constrained variables
in `S2` into the image through `A` of constrained variables in `S1`.

The linear map `A` is described by [`map_set`](@ref), [`map_function`](@ref).
Implementing a method for these two functions is sufficient to bridge
constraints. In order for the getters and setters of dual solutions,
starting values, etc...  to work as well a method for the following
functions should be implemented as well: [`inverse_map_set`](@ref),
[`inverse_map_function`](@ref), [`adjoint_map_function`](@ref) and
[`inverse_adjoint_map_function`](@ref). See the docstrings of the function
to see which feature would be missing it it was not implemented for a given
bridge.

"""
abstract type SetMapBridge{T,S2,S1} <: AbstractBridge end
