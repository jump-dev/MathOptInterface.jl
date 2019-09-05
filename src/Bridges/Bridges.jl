module Bridges

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MOI.Utilities

const CI = MOI.ConstraintIndex

include("bridge.jl")
include("bridge_optimizer.jl")

# Variable bridges
include("Variable/Variable.jl")
# Constraint bridges
include("Constraint/Constraint.jl")
# Objective bridges
include("Objective/Objective.jl")

include("lazy_bridge_optimizer.jl")

"""
    full_bridge_optimizer(model::MOI.ModelLike, ::Type{T}) where T

Returns a `LazyBridgeOptimizer` bridging `model` for every bridge defined in
this package and for the coefficient type `T`.
"""
function full_bridge_optimizer(model::MOI.ModelLike, T::Type)
    bridged_model = LazyBridgeOptimizer(model)
    Variable.add_all_bridges(bridged_model, T)
    Constraint.add_all_bridges(bridged_model, T)
    Objective.add_all_bridges(bridged_model, T)
    return bridged_model
end

print_num_bridges(io::IO, ::Variable.EmptyMap) = nothing
print_num_bridges(io::IO, ::Constraint.EmptyMap) = nothing
print_num_bridges(io::IO, ::Objective.EmptyMap) = nothing
function print_num_bridges(io::IO, B::Variable.Map)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n = length(B)
    print(io, "\n$(indent)with $(n) variable bridge$(s(n))")
end
function print_num_bridges(io::IO, B::Constraint.Map)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n = length(B)
    print(io, "\n$(indent)with $(n) constraint bridge$(s(n))")
end
function print_num_bridges(io::IO, B::Objective.Map)
    s(n) = n == 1 ? "" : "s"
    indent = " "^get(io, :indent, 0)
    n = length(B)
    print(io, "\n$(indent)with $(n) objective bridge$(s(n))")
end

function Base.show(io::IO, B::AbstractBridgeOptimizer)
    MOIU.print_with_acronym(io, summary(B))
    print_num_bridges(io, Variable.bridges(B))
    print_num_bridges(io, Constraint.bridges(B))
    print_num_bridges(io, Objective.bridges(B))
    if :model in propertynames(B)
        indent = " "^get(io, :indent, 0)
        print(io, "\n$(indent)with inner model ")
        show(IOContext(io, :indent => get(io, :indent, 0)+2), B.model)
    end
end

end # module
