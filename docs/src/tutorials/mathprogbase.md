# Transitioning from MathProgBase

MathOptInterface is a replacement for [MathProgBase.jl](https://github.com/JuliaOpt/MathProgBase.jl).
However, it is not a direct replacement.

## Transitioning a solver interface

Writing a solver interface in MathOptInterface is more work than the equivalent
interface in MathProgBase.

For more information, read [Implementing a solver interface](@ref).

## Transitioning the high-level functions

MathOptInterface doesn't provide replacements for the high-level interfaces in
MathProgBase. We recommend you use [JuMP](https://github.com/jump-dev/JuMP.jl)
as a modeling interface instead.

!!! tip
    If you haven't used JuMP before, start with the tutorial
    [Getting started with JuMP](https://jump.dev/JuMP.jl/stable/tutorials/Getting%20started/getting_started_with_JuMP/)

### linprog

Here is one way of transitioning from `linprog`:

```julia
using JuMP

function linprog(c, A, sense, b, l, u, solver)
    N = length(c)
    model = Model(solver)
    @variable(model, l[i] <= x[i=1:N] <= u[i])
    @objective(model, Min, c' * x)
    eq_rows, ge_rows, le_rows = sense .== '=', sense .== '>', sense .== '<'
    @constraint(model, A[eq_rows, :] * x .== b[eq_rows])
    @constraint(model, A[ge_rows, :] * x .>= b[ge_rows])
    @constraint(model, A[le_rows, :] * x .<= b[le_rows])
    optimize!(model)
    return (
        status = termination_status(model),
        objval = objective_value(model),
        sol = value.(x)
    )
end
```

### mixintprog

Here is one way of transitioning from `mixintprog`:

```julia
using JuMP

function mixintprog(c, A, rowlb, rowub, vartypes, lb, ub, solver)
    N = length(c)
    model = Model(solver)
    @variable(model, lb[i] <= x[i=1:N] <= ub[i])
    for i in 1:N
        if vartypes[i] == :Bin
            set_binary(x[i])
        elseif vartypes[i] == :Int
            set_integer(x[i])
        end
    end
    @objective(model, Min, c' * x)
    @constraint(model, rowlb .<= A * x .<= rowub)
    optimize!(model)
    return (
        status = termination_status(model),
        objval = objective_value(model),
        sol = value.(x)
    )
end
```

### quadprog

Here is one way of transitioning from `quadprog`:

```julia
using JuMP

function quadprog(c, Q, A, rowlb, rowub, lb, ub, solver)
    N = length(c)
    model = Model(solver)
    @variable(model, lb[i] <= x[i=1:N] <= ub[i])
    @objective(model, Min, c' * x + 0.5 * x' * Q * x)
    @constraint(model, rowlb .<= A * x .<= rowub)
    optimize!(model)
    return (
        status = termination_status(model),
        objval = objective_value(model),
        sol = value.(x)
    )
end
```
