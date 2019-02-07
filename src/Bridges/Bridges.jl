module Bridges

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

const CI = MOI.ConstraintIndex

include("bridge.jl")
include("set_map.jl")
include("bridge_optimizer.jl")

include("Variable/Variable.jl")
include("Constraint/Constraint.jl")
include("Objective/Objective.jl")

include("flip_sign.jl")
include("soc_rsoc.jl")

include("lazy_bridge_optimizer.jl")
include("debug.jl")

"""
    full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where {T}

Returns a [`LazyBridgeOptimizer`](@ref) bridging `model` for every bridge
defined in this package (see below for the few exceptions) and for the
coefficient type `T` in addition to the bridges in the list returned by
`MOI.get(model, MOI.Bridges.ToAdd{T}())`.

See also [`ToAdd`](@ref).

!!! note
    The following bridges are not added by `full_bridge_optimizer` except if
    they are in the list returned by `MOI.get(model, MOI.Bridges.ToAdd{T}())`
    (see the docstrings of the corresponding bridge for the reason they are not
    added):
    * [`Constraint.SOCtoNonConvexQuad`](@ref),
      [`Constraint.RSOCtoNonConvexQuad`](@ref) and
      [`Constraint.SOCtoPSDBridge`](@ref).
    * The subtypes of [`Constraint.AbstractToIntervalBridge`](@ref) (i.e.
      [`Constraint.GreaterToIntervalBridge`](@ref) and
      [`Constraint.LessToIntervalBridge`](@ref)) if `T` is not a subtype of
      `AbstractFloat`.
"""
function full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where {T}
    bridged_model = LazyBridgeOptimizer(model)
    for BT in MOI.get(model, ToAdd{T}())
        MOIB.add_bridge(bridged_model, BT)
    end
    Variable.add_all_bridges(bridged_model, T)
    Constraint.add_all_bridges(bridged_model, T)
    Objective.add_all_bridges(bridged_model, T)
    return bridged_model
end

"""
    ToAdd{T}()

A list of additional bridges that should be added to the
[`LazyBridgeOptimizer`](@ref) in `full_bridge_optimizer(optimizer, T)`.

See also [`full_bridge_optimizer`](@ref).

## Examples

Suppose an optimizer can exploit specific structure of a constraint.
For instance, suppose it can exploit the structure of the matrix `A`
in the linear system of equations `A * x = b`.
The optimizer can create a function:
```julia
struct MatrixAffineFunction{T,M<:AbstractMatrix{T}} <: MOI.AbstractVectorFunction
    A::M
    b::Vector{T}
end
```
and then a bridge from `VectorAffineFunction{T}` to
`MatrixAffineFunction{T,SparseMatrixCSC{T,Int}}`.
The bridge is then included in the list returned as the value of the
`ToAdd` attribute. That way, the optimizer can exploit a specific matrix
structure if the user uses a `MatrixAffineFunction` and in case the user uses
`VectorAffineFunction` then it's automatically bridged.
"""
struct ToAdd{T} <: MOI.AbstractOptimizerAttribute end

MOI.is_copyable(::ToAdd) = false

MOI.get(model::MOI.ModelLike, ::ToAdd) = []

print_num_bridges(io::IO, ::Variable.EmptyMap) = nothing

print_num_bridges(io::IO, ::Constraint.EmptyMap) = nothing

print_num_bridges(io::IO, ::Objective.EmptyMap) = nothing

function print_num_bridges(io::IO, B::Variable.Map)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n = length(B)
    print(io, "\n$(indent)with $(n) variable bridge$(s(n))")
    return
end

function print_num_bridges(io::IO, B::Constraint.Map)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n = length(B)
    print(io, "\n$(indent)with $(n) constraint bridge$(s(n))")
    return
end

function print_num_bridges(io::IO, B::Objective.Map)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n = length(B)
    print(io, "\n$(indent)with $(n) objective bridge$(s(n))")
    return
end

function Base.show(io::IO, B::AbstractBridgeOptimizer)
    MOIU.print_with_acronym(io, summary(B))
    print_num_bridges(io, Variable.bridges(B))
    print_num_bridges(io, Constraint.bridges(B))
    print_num_bridges(io, Objective.bridges(B))
    if :model in propertynames(B)
        indent = " "^get(io, :indent, 0)
        print(io, "\n$(indent)with inner model ")
        show(IOContext(io, :indent => get(io, :indent, 0) + 2), B.model)
    end
    return
end

end
