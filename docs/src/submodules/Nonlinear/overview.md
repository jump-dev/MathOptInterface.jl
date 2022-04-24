```@meta
CurrentModule = MathOptInterface
DocTestSetup = quote
    using MathOptInterface
    const MOI = MathOptInterface
end
DocTestFilters = [r"MathOptInterface|MOI"]
```

# [Nonlinear](@id nonlinear_developers)

!!! warning
    The Nonlinear submodule is experimental. Until this message is removed,
    breaking changes may be introduced in any minor or patch release of
    MathOptInterface.

The `Nonlinear` submodule contains data structures and functions for
working with a nonlinear optimization problem in the form of an expression
graph. This page explains the API and describes the rationale behind its design.

## Standard form

[Nonlinear programs (NLPs)](https://en.wikipedia.org/wiki/Nonlinear_programming)
are a class of optimization problems in which some of the constraints or the
objective function are nonlinear:
```math
\begin{align}
    \min_{x \in \mathbb{R}^n} & f_0(x) \\
    \;\;\text{s.t.} & l_j \le f_j(x) \le u_j & j = 1 \ldots m
\end{align}
```
There may be additional constraints, as well as things like variable bounds
and integrality restrictions, but we do not consider them here because they are
best dealt with by other components of MathOptInterface.

## API overview

The core element of the `Nonlinear` submodule is
[`Nonlinear.NonlinearData`](@ref):
```jldoctest nonlinear_developer
julia> const Nonlinear = MathOptInterface.Nonlinear;

julia> data = Nonlinear.NonlinearData()
NonlinearData with available features:
  * :ExprGraph
```
[`Nonlinear.NonlinearData`](@ref) is a mutable struct that stores all of the
nonlinear information added to the model.

### Decision variables

Decision variables are represented by [`VariableIndex`](@ref)es. The user is
responsible for creating these using `MOI.VariableIndex(i)`, where `i` is the
column associated with the variable.

### [Expressions](@id Nonlinear_Expressions)

The input data structure is a Julia `Expr`. The input expressions can
incorporate [`VariableIndex`](@ref)es, but these must be interpolated into
the expression with `$`:
```jldoctest nonlinear_developer
julia> x = MOI.VariableIndex(1)
MathOptInterface.VariableIndex(1)

julia> input = :(1 + sin($x)^2)
:(1 + sin(MathOptInterface.VariableIndex(1)) ^ 2)
```
There are a number of restrictions on the input `Expr`:
 * It cannot contain macros
 * It cannot contain broadcasting
 * It cannot contain splatting (except in limited situations)
 * It cannot contain linear algebra, such as matrix-vector products
 * It cannot contain generator expressions, including `sum(i for i in S)`

Given an input expression, add an expression using
[`Nonlinear.add_expression`](@ref):
```jldoctest nonlinear_developer
julia> expr = Nonlinear.add_expression(data, input)
MathOptInterface.Nonlinear.ExpressionIndex(1)
```
The return value, `expr`, is a [`Nonlinear.ExpressionIndex`](@ref) that can
then be interpolated into other input expressions.

### [Parameters](@id Nonlinear_Parameters)

In addition to constant literals like `1` or `1.23`, you can create parameters.
Parameters are placeholders whose values can change before passing the
expression to the solver. Create a parameter using
[`Nonlinear.add_parameter`](@ref), which accepts a default value:
```jldoctest nonlinear_developer
julia> p = Nonlinear.add_parameter(data, 1.23)
MathOptInterface.Nonlinear.ParameterIndex(1)
```
The return value, `p`, is a [`Nonlinear.ParameterIndex`](@ref) that can then be
interpolated into other input expressions.

Update a parameter as follows:
```jldoctest nonlinear_developer
julia> data[p]
1.23

julia> data[p] = 4.56
4.56

julia> data[p]
4.56
```

### [Objectives](@id Nonlinear_Objectives)

Set a nonlinear objective using [`Nonlinear.set_objective`](@ref):
```jldoctest nonlinear_developer
julia> Nonlinear.set_objective(data, :($p + $expr + $x))
```
Clear a nonlinear objective by passing `nothing`:
```jldoctest nonlinear_developer
julia> Nonlinear.set_objective(data, nothing)
```

### [Constraints](@id Nonlinear_Constraints)

Add a constraint using [`Nonlinear.add_constraint`](@ref):
```jldoctest nonlinear_developer
julia> c = Nonlinear.add_constraint(data, :(1 + sqrt($x) <= 2.0))
MathOptInterface.Nonlinear.ConstraintIndex(1)
```
The return value, `c`, is a [`Nonlinear.ConstraintIndex`](@ref) that is a unique
identifier for the constraint. Interval constraints are also supported:
```jldoctest nonlinear_developer
julia> c2 = Nonlinear.add_constraint(data, :(-1.0 <= 1 + sqrt($x) <= 2.0))
MathOptInterface.Nonlinear.ConstraintIndex(2)
```

Delete a constraint using [`Nonlinear.delete`](@ref):
```jldoctest nonlinear_developer
julia> Nonlinear.delete(data, c2)
```

### User-defined operators

By default, `Nonlinear` supports a wide range of univariate and multivariate
operators. However, you can also define your own operators by _registering_
them.

#### Univariate operators

Register a univariate user-defined operator using
[`Nonlinear.register_operator`](@ref):
```jldoctest nonlinear_developer
julia> f(x) = 1 + sin(x)^2
f (generic function with 1 method)

julia> Nonlinear.register_operator(data, :my_f, 1, f)
```
Now, you can use `:my_f` in expressions:
```jldoctest nonlinear_developer
julia> new_expr = Nonlinear.add_expression(data, :(my_f($x + 1)))
MathOptInterface.Nonlinear.ExpressionIndex(2)
```
By default, `Nonlinear` will compute first- and second-derivatives of the
registered operator using `ForwardDiff.jl`. Override this by passing functions
which compute the respective derivative:
```jldoctest nonlinear_developer
julia> f′(x) = 2 * sin(x) * cos(x)
f′ (generic function with 1 method)

julia> Nonlinear.register_operator(data, :my_f2, 1, f, f′)
```
or
```jldoctest nonlinear_developer
julia> f′′(x) = 2 * (cos(x)^2 - sin(x)^2)
f′′ (generic function with 1 method)

julia> Nonlinear.register_operator(data, :my_f3, 1, f, f′, f′′)
```

#### Multivariate operators

Register a multivariate user-defined operator using
[`Nonlinear.register_operator`](@ref):
```jldoctest nonlinear_developer
julia> g(x...) = x[1]^2 + x[1] * x[2] + x[2]^2
g (generic function with 1 method)

julia> Nonlinear.register_operator(data, :my_g, 2, g)
```
Now, you can use `:my_f` in expressions:
```jldoctest nonlinear_developer
julia> new_expr = Nonlinear.add_expression(data, :(my_g($x + 1, $x)))
MathOptInterface.Nonlinear.ExpressionIndex(3)
```
By default, `Nonlinear` will compute the gradient of the registered
operator using `ForwardDiff.jl`. (Hessian information is not supported.)
Override this by passing a function to compute the gradient:
```jldoctest nonlinear_developer
julia> function ∇g(ret, x...)
           ret[1] = 2 * x[1] + x[2]
           ret[2] = x[1] + 2 * x[2]
           return
       end
∇g (generic function with 1 method)

julia> Nonlinear.register_operator(data, :my_g2, 2, g, ∇g)
```

### [MathOptInterface](@id Nonlinear_MOI_interface)

MathOptInterface communicates the nonlinear portion of an optimization problem
to solvers using concrete subtypes of [`AbstractNLPEvaluator`](@ref), which
implement the [Nonlinear programming](@ref) API.

[`NonlinearData`](@ref) is a subtype of [`AbstractNLPEvaluator`](@ref), but the
functions of the [Nonlinear programming](@ref) API that it implements depends
upon the chosen [`Nonlinear.AbstractAutomaticDifferentiation`](@ref) backend.

There are two to choose from within MOI, although other packages may add more
options by sub-typing [`Nonlinear.AbstractAutomaticDifferentiation`](@ref):
 * [`Nonlinear.ExprGraphOnly`](@ref)

Set the differentiation backend using [`Nonlinear.set_differentiation_backend`](@ref).
If we set [`Nonlinear.ExprGraphOnly`](@ref), then we get access to `:ExprGraph`:
```jldoctest nonlinear_developer
julia> Nonlinear.set_differentiation_backend(
           data,
           Nonlinear.ExprGraphOnly(),
           [x],
       )

julia> data
NonlinearData with available features:
  * :ExprGraph
```

[`Nonlinear.set_differentiation_backend`](@ref) requires an ordered list of the
variables that are included in the model. This order corresponds to the order of
the primal decision vector `x` which is passed to the various functions in MOI's
nonlinear API.

The `:ExprGraph` feature means we can call [`objective_expr`](@ref) and
[`constraint_expr`](@ref) to retrieve the expression graph of the problem.
However, we cannot call gradient terms such as
[`eval_objective_gradient`](@ref) because [`Nonlinear.ExprGraphOnly`](@ref) does
not have the capability to differentiate a nonlinear expression.

Instead of passing [`AbstractNLPEvaluator`](@ref)s directly to solvers,
MathOptInterface instead passes an [`NLPBlockData`](@ref), which wraps an
[`AbstractNLPEvaluator`](@ref) and includes other information such as constraint
bounds and whether the evaluator has a nonlinear objective. Create an
`NLPBlockData`](@ref) as follows:
```jldoctest nonlinear_developer
julia> MOI.NLPBlockData(data);
```

## Expression-graph representation

[`Nonlinear.NonlinearData`](@ref) stores nonlinear expressions in
[`Nonlinear.NonlinearExpression`](@ref)s. This section explains the design of
the expression graph datastructure in [`Nonlinear.NonlinearExpression`](@ref).

Given a nonlinear function like `f(x) = sin(x)^2 + x`, a conceptual aid for
thinking about the graph representation of the expression is to convert it into
[Polish prefix notation](https://en.wikipedia.org/wiki/Polish_notation):
```
f(x, y) = (+ (^ (sin x) 2) x)
```
This format identifies each operator (function), as well as a list of arguments.
Operators can be univariate, like `sin`, or multivariate, like `+`.

A common way of representing Polish prefix notation in code is as follows:
```jldoctest expr_graph
julia> x = MOI.VariableIndex(1);

julia> struct ExprNode
           op::Symbol
           children::Vector{Union{ExprNode,Float64,MOI.VariableIndex}}
       end

julia> expr = ExprNode(:+, [ExprNode(:^, [ExprNode(:sin, [x]), 2.0]), x]);
```

This datastructure follows our Polish prefix notation very closely, and we can
easily identify the arguments to an operator. However, it has a significant
draw-back: each node in the graph requires a `Vector`, which is heap-allocated
and tracked by Julia's garbage collector (GC). For large models, we can expect
to have millions of nodes in the expression graph, so this overhead quickly
becomes prohibiative for computation.

An alternative is to record the expression as a linear tape:
```jldoctest expr_graph
julia> expr = Any[:+, 2, :^, 2, :sin, 1, x, 2.0, x]
9-element Vector{Any}:
  :+
 2
  :^
 2
  :sin
 1
  MathOptInterface.VariableIndex(1)
 2.0
  MathOptInterface.VariableIndex(1)
```
The `Int` after each operator `Symbol` specifies the number of arguments.

This data-structure is a single vector, which resolves our problem with the GC,
but each element is the abstract type, `Any`, and so any operations on it will
lead to slower dynamic dispatch. It's also hard to identify the children of each
operation without reading the entire tape.

To summarize, representing expression graphs in Julia has the following
challenges:
 * Nodes in the expression graph should not contain a heap-allocated object
 * All data-structures should be concretely typed
 * It should be easy to identify the children of a node

### Sketch of the design in Nonlinear

`Nonlinear` overcomes these problems by decomposing the datastructure into a
number of different concrete-typed vectors.

First, we create vectors of the supported uni- and multivariate operators.
```jldoctest expr_graph
julia> const UNIVARIATE_OPERATORS = [:sin];

julia> const MULTIVARIATE_OPERATORS = [:+, :^];
```
In practice, there are many more supported operations than the ones listed here.

Second, we create an enum to represent the different types of nodes present in
the expression graph:
```jldoctest expr_graph
julia> @enum(
           NodeType,
           NODE_CALL_MULTIVARIATE,
           NODE_CALL_UNIVARIATE,
           NODE_VARIABLE,
           NODE_VALUE,
       )
```
In practice, there are node types other than the ones listed here.

Third, we create two concretely-typed structs as follows:
```jldoctest expr_graph
julia> struct Node
           type::NodeType
           parent::Int
           index::Int
       end

julia> struct NonlinearExpression
           nodes::Vector{Node}
           values::Vector{Float64}
       end
```

For each node `node` in the `.nodes` field, if `node.type` is:
 * `NODE_CALL_MULTIVARIATE`, we look up
   `MULTIVARIATE_OPERATORS[node.index]` to retrieve the operator
 * `NODE_CALL_UNIVARIATE`, we look up
   `UNIVARIATE_OPERATORS[node.index]` to retrieve the operator
 * `NODE_VARIABLE`, we create `MOI.VariableIndex(node.index)`
 * `NODE_VALUE`, we look up `values[node.index]`
The `.parent` field of each node is the integer index of the parent node in
`.nodes`. For the first node, the parent is `-1` by convention.

Therefore, we can represent our function as:
```jldoctest expr_graph
julia> expr = NonlinearExpression(
           [
               Node(NODE_CALL_MULTIVARIATE, 1, -1),
               Node(NODE_CALL_MULTIVARIATE, 2, 1),
               Node(NODE_CALL_UNIVARIATE, 1, 2),
               Node(NODE_VARIABLE, 1, 3),
               Node(NODE_VALUE, 1, 2),
               Node(NODE_VARIABLE, 1, 1),
           ],
           [2.0],
       );
```

This is less readable than the other options, but does this datastructure meet
our design goals?

Instead of a heap-allocated object for each node, we only have two `Vector`s for
each expression, `nodes` and `values`, as well as two constant vectors for the
`OPERATORS`. In addition, all fields are concretely typed, and there are no
`Union` or `Any` tyypes.

For our third goal, it is not easy to identify the children of a node, but it is
easy to identify the _parent_ of any node. Therefore, we can use
[`Nonlinear.adjacency_matrix`](@ref) to compute a sparse matrix that maps
parents to their children.

The tape is also ordered topologically, so that a reverse pass of the nodes
evaluates all children nodes before their parent.

### The design in practice

In practice, `Node` and `NonlinearExpression` are exactly [`Nonlinear.Node`](@ref)
and [`Nonlinear.NonlinearExpression`](@ref). However, [`Nonlinear.NodeType`](@ref)
has more fields to account for comparison operators such as `:>=` and `:<=`,
logic operators such as `:&&` and `:||`, nonlinear parameters, and nested
subexpressions.

Moreover, instead of storing the operators as global constants, they are stored
in [`Nonlinear.OperatorRegistry`](@ref), and it also stores a vector of logic
operators and a vector of comparison operators. In addition to
[`Nonlinear.DEFAULT_UNIVARIATE_OPERATORS`](@ref) and
[`Nonlinear.DEFAULT_MULTIVARIATE_OPERATORS`](@ref), you can register
user-defined functions using [`Nonlinear.register_operator`](@ref).

[`Nonlinear.NonlinearData`](@ref) is a struct that stores the
[`Nonlinear.OperatorRegistry`](@ref), as well as a list of parameters and
subexpressions in the model.
