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
`MOI.get(model, MOI.Bridges.ListOfNonstandardBridges{T}())`.

See also [`ListOfNonstandardBridges`](@ref).

!!! note
    The following bridges are not added by `full_bridge_optimizer` except if
    they are in the list returned by `MOI.get(model, MOI.Bridges.ListOfNonstandardBridges{T}())`
    (see the docstrings of the corresponding bridge for the reason they are not
    added):
    * [`Constraint.SOCtoNonConvexQuadBridge`](@ref),
      [`Constraint.RSOCtoNonConvexQuadBridge`](@ref) and
      [`Constraint.SOCtoPSDBridge`](@ref).
    * The subtypes of [`Constraint.AbstractToIntervalBridge`](@ref) (i.e.
      [`Constraint.GreaterToIntervalBridge`](@ref) and
      [`Constraint.LessToIntervalBridge`](@ref)) if `T` is not a subtype of
      `AbstractFloat`.
"""
function full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where {T}
    bridged_model = LazyBridgeOptimizer(model)
    for BT in MOI.get(model, ListOfNonstandardBridges{T}())
        add_bridge(bridged_model, BT)
    end
    Variable.add_all_bridges(bridged_model, T)
    Constraint.add_all_bridges(bridged_model, T)
    Objective.add_all_bridges(bridged_model, T)
    return bridged_model
end

"""
    ListOfNonstandardBridges{T}() <: MOI.AbstractOptimizerAttribute

Any optimizer can be wrapped in a [`LazyBridgeOptimizer`](@ref) using
[`full_bridge_optimizer`](@ref). However, by default [`LazyBridgeOptimizer`](@ref)
uses a limited set of bridges that are:

  1. implemented in `MOI.Bridges`
  2. generally applicable for all optimizers.

For some optimizers however, it is useful to add additional bridges, such as
those that are implemented in external packages (e.g., within the solver package
itself) or only apply in certain circumstances (e.g.,
[`Constraint.SOCtoNonConvexQuadBridge`](@ref)).

Such optimizers should implement the `ListOfNonstandardBridges` attribute to
return a vector of bridge types that are added by [`full_bridge_optimizer`](@ref)
in addition to the list of default bridges.

Note that optimizers implementing `ListOfNonstandardBridges` may require
package-specific functions or sets to be used if the non-standard bridges
are not added. Therefore, you are recommended to use
`model = MOI.instantiate(Package.Optimizer; with_bridge_type = T)` instead of
`model = MOI.instantiate(Package.Optimizer)`. See
[`MathOptInterface.instantiate`](@ref).

## Examples

### An optimizer using a non-default bridge in `MOI.Bridges`

Solvers supporting [`MOI.ScalarQuadraticFunction`](@ref) can support
[`MOI.SecondOrderCone`](@ref) and [`MOI.RotatedSecondOrderCone`](@ref) by
defining:
```julia
function MOI.get(::MyQuadraticOptimizer, ::ListOfNonstandardBridges{Float64})
    return Type[
        MOI.Bridges.Constraint.SOCtoNonConvexQuadBridge{Float64},
        MOI.Bridges.Constraint.RSOCtoNonConvexQuadBridge{Float64},
    ]
end
```

### An optimizer defining an internal bridge

Suppose an optimizer can exploit specific structure of a constraint, e.g., it
can exploit the structure of the matrix `A` in the linear system of equations
`A * x = b`.

The optimizer can define the function:
```julia
struct MatrixAffineFunction{T} <: MOI.AbstractVectorFunction
    A::SomeStructuredMatrixType{T}
    b::Vector{T}
end
```
and then a bridge
```julia
struct MatrixAffineFunctionBridge{T} <: MOI.Constraint.AbstractBridge
    # ...
end
# ...
```
from `VectorAffineFunction{T}` to the `MatrixAffineFunction`. Finally, it
defines:
```julia
function MOI.get(::Optimizer{T}, ::ListOfNonstandardBridges{T}) where {T}
    return Type[MatrixAffineFunctionBridge{T}]
end
```
"""
struct ListOfNonstandardBridges{T} <: MOI.AbstractOptimizerAttribute end

attribute_value_type(::ListOfNonstandardBridges) = Vector{Type}

MOI.is_copyable(::ListOfNonstandardBridges) = false

MOI.get_fallback(model::MOI.ModelLike, ::ListOfNonstandardBridges) = Type[]

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
