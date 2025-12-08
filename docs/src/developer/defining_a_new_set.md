```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    import MathOptInterface as MOI
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Defining a new set

The easiest way to extend the behavior of MathOptInterface is to design a new
set. The purpose of this tutorial is to explain how to define a new set and some
of the associated nuances.

Defining a new function is an order of magnitude (if not two) harder than
defining a new set. Don't consider it as an option unless you have already tried
to support your behavior as a set.

As a motivation for this tutorial, we consider the _LinMax_ constraint type,
which appears often in constraint programming:
```math
t = \max(f_1(x), f_2(x), \ldots, f_N(x))
```

The first step to design a new set for MathOptInterface is to define the
mathematical relationship you want to model as a _function-in-set_ $f(x) \in S$.

Your initial thought for representing the _LinMax_ constraint in MathOptInterface
may be to represent it as:
```math
F(x) \in LinMax(t)
```
where $F(x)$ is the vector-valued function created by concatenating the $f_i$
functions. This formulation violates a basic rule of MathOptInterface:

!!! rule
    Sets cannot contain references to decision variables or other constraints.

Instead, we could model the _LinMax_ constraint as:
```math
[t, f_1(x), f_2(x), \ldots, f_N(x)] \in LinMax(N+1)
```

In the language of MathOptInterface, this is a [`AbstractVectorFunction`](@ref)
in the `LinMax` set of dimension $N+1$. The type of the function depends on the
types of the component scalar functions, with a special convention that the
first element in the function is interpretable as a [`VariableIndex`](@ref)
`t`.

Now `LinMax` can be trivially defined as a new [`AbstractVectorSet`](@ref):
```jldoctest define_new_set
julia> import MathOptInterface as MOI

julia> struct LinMax <: MOI.AbstractVectorSet
           dimension::Int
       end
```
and it can immediately be used in MathOptInterface:
```jldoctest define_new_set
julia> model = MOI.Utilities.UniversalFallback(MOI.Utilities.Model{Float64}());

julia> t = MOI.VariableIndex(1);

julia> x = MOI.VariableIndex.(2:3);

julia> F = 1.0 .* x .+ 2.0;

julia> g = MOI.Utilities.operate(vcat, Float64, t, F...);

julia> MOI.add_constraint(model, g, LinMax(3))
MathOptInterface.ConstraintIndex{MathOptInterface.VectorAffineFunction{Float64}, LinMax}(1)

julia> print(model)
Feasibility

Subject to:

VectorAffineFunction{Float64}-in-LinMax
 ┌              ┐
 │0.0 + 1.0 v[1]│
 │2.0 + 1.0 v[2]│
 │2.0 + 1.0 v[3]│
 └              ┘ ∈ LinMax(3)
```
