```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# Solving a problem using MathOptInterface

In this example, we want to solve a binary-constrained knapsack problem:
```math
\begin{aligned}
\max \; & c^\top x       \\
s.t. \; & w^\top x \le C \\
        & x_i \in \{0,1\},\quad \forall i=1,\ldots,n
\end{aligned}
```

Load the MathOptInterface module and define the shorthand `MOI`:
```julia
using MathOptInterface
const MOI = MathOptInterface
```

As an optimizer, we choose GLPK:
```julia
using GLPK
optimizer = GLPK.Optimizer()
```

## Define the data

We first define the constants of the problem:
```jldoctest knapsack; setup = :(optimizer = MOI.Utilities.MockOptimizer(MOI.Utilities.Model{Float64}()); MOI.Utilities.set_mock_optimize!(optimizer, mock -> MOI.Utilities.mock_optimize!(mock, ones(3))))
julia> c = [1.0, 2.0, 3.0]
3-element Vector{Float64}:
 1.0
 2.0
 3.0

julia> w = [0.3, 0.5, 1.0]
3-element Vector{Float64}:
 0.3
 0.5
 1.0

julia> C = 3.2
3.2
```

## Add the variables

```jldoctest knapsack
julia> x = MOI.add_variables(optimizer, length(c));
```

## [Set the objective](@id set_objective_example)

```jldoctest knapsack
julia> MOI.set(
           optimizer,
           MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
           MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(c, x), 0.0),
       );

julia> MOI.set(optimizer, MOI.ObjectiveSense(), MOI.MAX_SENSE)
```

!!! tip
    `MOI.ScalarAffineTerm.(c, x)` is a shortcut for
    `[MOI.ScalarAffineTerm(c[i], x[i]) for i = 1:3]`. This is Julia's broadcast
    syntax in action, and is used quite often throughout MOI.

## Add the constraints

We add the knapsack constraint and integrality constraints:
```jldoctest knapsack
julia> MOI.add_constraint(
           optimizer,
           MOI.ScalarAffineFunction(MOI.ScalarAffineTerm.(w, x), 0.0),
           MOI.LessThan(C),
       );
```

Add integrality constraints:
```jldoctest knapsack
julia> for x_i in x
           MOI.add_constraint(optimizer, x_i, MOI.ZeroOne())
       end
```

## Optimize the model

```jldoctest knapsack
julia> MOI.optimize!(optimizer)
```

## Understand why the solver stopped

The first thing to check after optimization is why the solver stopped, e.g.,
did it stop because of a time limit or did it stop because it found the optimal
solution?
```jldoctest knapsack
julia> MOI.get(optimizer, MOI.TerminationStatus())
OPTIMAL::TerminationStatusCode = 1
```
Looks like we found an optimal solution!

## Understand what solution was returned


```jldoctest knapsack
julia> MOI.get(optimizer, MOI.ResultCount())
1

julia> MOI.get(optimizer, MOI.PrimalStatus())
FEASIBLE_POINT::ResultStatusCode = 1

julia> MOI.get(optimizer, MOI.DualStatus())
NO_SOLUTION::ResultStatusCode = 0
```

## Query the objective

What is its objective value?
```jldoctest knapsack
julia> MOI.get(optimizer, MOI.ObjectiveValue())
6.0
```

## Query the primal solution

And what is the value of the variables `x`?
```jldoctest knapsack
julia> MOI.get(optimizer, MOI.VariablePrimal(), x)
3-element Vector{Float64}:
 1.0
 1.0
 1.0
```
