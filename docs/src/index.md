*The file format is under active development. No backward compatibility yet!*

## Background

In order to use an optimization solver, it is necessary to communicate a model
instance to the solver [^1]. Many different instance formats have been proposed
over the years, but only a few (such as MPS) have become the industry standard.

Each format is a product of its time in history and the problem class it tried
to address. For example, we retain the rigid input format of the MPS file that
was designed for 1960's punchcards despite the obsolescence of this technology
[^2]. Although the MPS format has since been extended to problem classes such as
nonlinear and stochastic linear programming, MPS was not designed with
extensibility in mind. This has led some authors (such as [^3]) to conclude that
developing a new format is easier than extending the existing MPS format.

The LP file-format dates back to the work of Orchard-Hays who attempted to
correct the ''mistakes'' of the MPS file-format by creating a human-readable,
row-oriented format for mathematicians [^2]. However, due to its age, there is
no longer a single standard for the LP file-format. This has led to subtle
differences between implementations in different readers that hampers the
usefulness of the format as a format for interchange. Much like the MPS file,
the LP file is also limited in the types of problems it can represent and was
not designed for extensibility.

In contrast to the LP file, the NL file explicitly aims for machine-readability
at the expense of human-readability [^5]. It is also considerably more flexible
in the problem classes it can represent (in particular, arbitrary nonlinear
functions are supported). However, once again, the format is not extensible to
new problem formats and lacks support for conic problems.

More recently, considerable work has been put into developing the OSiL format
[^4]. In developing OSiL, Fourer et al. identified many of the challenges and
limitations of previous formats and attempted to overcome them. In particular,
they choose to use XML as the basis for their format. This removed the burden of
writing custom readers and writers for each programming language that wished to
interface with optimization software and allowed more focus on the underlying
data-structures. XML is also human-readable and can be rigidly specified with a
schema to prevent the proliferation of similar, but incompatible versions. The
XML approach also allows for easy extensibility and can support multiple problem
classes including nonlinear, stochastic, and conic.

However, despite the many apparent advantages of the OSiL format, we believe it
has enough short-comings to justify the development of a new instance format.
Two of the main reasons are the verbosity of the XML format and the lack of a
strong, extensible standard form.

### Project Goals

With this understanding of the history and evolution of different file-formats,
the following goals guided our development of the MathOptFormat:

1. **Human-readable**

    The format should be able to be read and edited by a human.

2. **Machine-readable**

    The format should be able to be read by a variety of different programming
    languages without needing to write custom parsers in each language.

3. **Standardized**

    The format should describe a very general mathematical ''standard-form'' in
    a manner that is unambiguous.

4. **Extensible**

    The format should be able to be easily extended to incorporate new
    problem-classes as they arise.

## The MathOptInterface Standard Form

MathOptInterface is a solver abstraction layer for mathematical optimization
solvers [^6]. One if the core design goals of MathOptInterface is for it to

> *"be simple and extensible, unifying linear, quadratic, and conic optimization,
> and seamlessly facilitate extensions to essentially arbitrary constraints and
> functions (e.g., indicator constraints, complementarity constraints, and
> piecewise linear functions)."*

The MathOptInterface standard form problem is:

```math
\begin{align}
    & \min_{x \in \mathbb{R}^n} & f_0(x)
    \\
    & \;\;\text{s.t.} & f_i(x) & \in \mathcal{S}_i & i = 1 \ldots m
\end{align}
```

where $f_i(x)$ is an arbitrary function and $\mathcal{S}_i$ is an arbitrary set.

For example, instead of thinking of the constraint $3x + y \le 1$ as a ''less
than or equal to'' constraint, we can think of the constraint as enforcing the
function $3x + y$ to be inside the set $(-\infty, 1]$.

This approach turns out to be very general, as instead of thinking of variable
as being ''binary'', we say the function $x$ belongs to the set $\{0, 1\}$.
Instead of a variable being semicontinuous, we say the function $x$ belongs to
the set ${0} \cup [l, u]$.

## Why JSON?


One reason for developing a new instance format rather than improving OSiL is
its use of XML. Although XML has many advantages (a strictly defined schema for
example), the format is almost too general (and too verbose) for our purposes.

In constrast, JSON is a much simpler format, and is only able to store six
different data types: `string`, `number`, `object`, `array`, `boolean` and
`null`.

In almost all programming languages, these map directly to native language
constructs (`object` being a dictionary or a key-value mapping).

TODO(odow): expand this section.

[https://www.json.org/xml.html](https://www.json.org/xml.html)

## The Format

A MathOptFormat instance is a text representation of the model as a JSON object.
The object must have the following fields: `version`, `variables`, `objective`
and `constraints`. Users may also choose to add optional fields such as `author`
to provide contextual information for humans reading the instance. Parsers may
choose to ignore these fields.

### Versioning

The `version` field stores number of the earliest version of MathOptFormat that
supported all the features in the instance.

    "version": 0

### Variables

The `variables` field contains a list of objects (one for each variable in the
model). Each variable object must contain at least the field `name` which
records a unique string. Duplicate names are not allowed. In addition, the
variable object can optionally contain any MathOptInterface variable attributes
(for example `VariablePrimalStart`).

    "variables": [
        {"name": "x"},
        {"name": "y", "VariablePrimalStart": 1.0}
    ]

### MathOptInterface Functions

A MathOptInterface function can be represented by a JSON object. Every function
must have the field `head` which contains a string that is identical to the name
of the MathOptInterface function.

In addition, there must be a one-to-one mapping between the field names of the
MathOptInterface type and the fields in the JSON object. However, instead of
referring to variables in the model using `VariableIndex`s, the MathOptFormat
version uses the string that corresponds to the name of the variable in the list
`variables` (defined above).

For example, the `SingleVariable` function has a single field `variable`. For
example:

    {"head": "SingleVariable", "variable": "x"}

### MathOptInterface Sets

MathOptInterface Sets are represented in a similar manner to MathOptInterface
functions.

    {"head": "LessThan", "upper": 1.0}

### Objective Functions

Although MathOptInterface only defines a single objective function,
MathOptFormat extends this notion to a list of objectives that are stored in the
`objectives` field.

Each object in the list must contain two fields: `function` and `sense`. The
`function` field contains a MathOptInterface function. The `sense` field must
contain a string that is either `"min"`, `"max"`, or `"feasibility"`. No other
values are allowed.

    "objectives": [
        {
            "sense": "min",
            "function": {"head": "SingleVariable", "variable": "x"}
        }
    ]

### Constraints

Each constraint is a JSON object with two required fields: `set`, and
`function`. The values associated with these fields must be a valid
MathOptInterface set and function respectively. In addition, the object can
contain MathOptInterface constraint attributes such as `name`,
`ConstraintPrimalStart`, and `ConstraintDualStart`.

    {
        "constraints": [
            {
                "name": "c1",
                "set": {
                    "head": "LessThan", "upper": 1.0
                },
                "function": {
                    "head": "ScalarAffineFunction",
                    "terms": [
                        {
                            "head": "ScalarAffineTerm",
                            "coefficient": 1.0,
                            "variable_index": "x"}
                    ],
                    "constant": 1.0

                }
            }

        ]
    }

## Example

Consider the following LP:

```math
\begin{align}
    & \min_{x,y} & 2x + y
    \\
    & \;\;\text{s.t.} & x + y >= 1
    \\
    &                 & x, Binary
\end{align}
```

### MathOptFormat

We can represent this in the MathOptFormat as

    {
        "author": "Oscar Dowson",
        "description": "A simple example for the MathOptFormat documentation",
        "name": "MathOptFormat Model",
        "version": 0,
        "variables": [{"name": "x"}, {"name": "y"}],
        "objectives": [
            {
                "sense": "min",
                "function": {
                    "head": "ScalarAffineFunction",
                     "terms": [
                         {
                             "head": "ScalarAffineTerm",
                             "coefficient": 2.0,
                             "variable_index": "x"
                         },
                         {
                             "head": "ScalarAffineTerm",
                             "coefficient": 1.0,
                             "variable_index": "y"
                         }
                    ],
                    "constant": 0.0
                }
            }
         ],
         "constraints": [
             {
                 "name": "x ∈ {0,1}",
                 "function": {"head": "SingleVariable", "variable": "x"},
                 "set": {"head": "ZeroOne"}
             },
             {
                 "name": "x+y≥1"
                 "function": {
                     "head": "ScalarAffineFunction",
                     "terms": [
                        {
                            "head": "ScalarAffineTerm",
                            "coefficient": 1.0,
                            "variable_index": "x",
                        },
                        {
                            "head": "ScalarAffineTerm",
                            "coefficient": 1.0,
                            "variable_index": "y"
                        }
                    ],
                    "constant": 0.0
                 },
                 "set": {"head": "GreaterThan", "lower": 1.0},
             }
         ]
    }

Note that in addition to the required fields, we can store additional
information (such as the `author` and a `description` of the model) that is not
necessary to define the model instance, but is useful human-readable metadata.

### LP

Compared to the LP formulation (below), the MathOptFormat version is verbose and
less human-readable. However, it does not require a specialized parser to read,
conforms to a well standardized specification, and is extensible.

    / Author: Oscar Dowson
    / Description: A simple example for the MathOptFormat documentation
    Minimize
    obj: 2x + y
    Subject To
    c1: x + y >= 1
    Bounds
    y free
    Binary
    x
    End

### OSiL

Compared to the OSiL version (below), we would argue that the MathOptFormat is
more human-readable, better standardized, and more extensible.

    <?xml version="1.0" encoding="UTF-8"?>
    <osil xmlns="os.optimizationservices.org">
        <instanceHeader>
            <name>MathOptFormat Example</name>
            <source>Oscar Dowson</source>
            <description>A simple example for the MathOptFormat documentation</description>
        </instanceHeader>
        <instanceData>
            <variables numberOfVariables="2">
                <var lb="-INF" name="x" type="B"/>
                <var lb="-INF" name="y"/>
            </variables>
            <objectives numberOfObjectives="1">
                <obj maxOrMin="min" numberOfObjCoef="2">
                    <coef idx="1">2</coef>
                    <coef idx="2">1</coef>
                </obj>
            </objectives>
            <constraints numberOfConstraints="1">
                <con lb="1.0"/>
            </constraints>
            <linearConstraintCoefficients numberOfValues="2">
                <start>
                    <el>0</el><el>1</el>
                </start>
                <colIdx>
                    <el>0</el><el>1</el>
                </colIdx>
                <value>
                    <el>1</el><el>1</el>
                </value>
            </linearConstraintCoefficients>
        </instanceData>
    </osil>


### MathOptFormat.jl

```julia
using MathOptFormat
const MOI = MathOptFormat.MOI

model = MathOptFormat.Model{Float64}()

# Create variables
(x, y) = MOI.add_variables(model, 2)
MOI.set(model, MOI.VariableName(), x, "x")
MOI.set(model, MOI.VariableName(), y, "y")

# Set objective
MOI.set(model, MOI.ObjectiveSense(), MOI.MinSense)
MOI.set(model,
    MOI.ObjectiveFunction{MOI.ScalarAffineFunction{Float64}}(),
    MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([2.0, 1.0], [x, y]),
        0.0)
)

# The constraint: x+y≥1 becomes x+y ∈ [1, ∞)
c1 = MOI.add_constraint(model,
    MOI.ScalarAffineFunction(
        MOI.ScalarAffineTerm.([1.0, 1.0], [x, y]),
        0.0),
    MOI.GreaterThan(1.0)
)
MOI.set(model, MOI.ConstraintName(), c1, "x+y≥1")

# The constraint: x, Binary becomes x ∈ {0, 1}
c2 = MOI.add_constraint(model,
    MOI.SingleVariable(x),
    MOI.ZeroOne()
)
MOI.set(model, MOI.ConstraintName(), c2, "x ∈ {0,1}")

# Write the model to file
MOI.write_to_file(model, "example.mof.json")
```

## References

[^1]: Gassmann, H., Ma, J., Martin, K. (2010). [Instance Formats for Mathematical Optimization Models](https://www.coin-or.org/OS/publications/instanceformats_encyclopedia2009.pdf). In *Wiley Encyclopedia of Operations Research and Management Science*.

[^2]: Orchard-Hays, W. (1984). [History of Mathematical Programming Systems](http://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=4640725). *Annals of the History of Computing, 6*(3).

[^3]: Friberg, H. (2014). *[The conic benchmark format: version 1 - technical reference manual](http://orbit.dtu.dk/files/88492586/Conic_Benchmark_Format.pdf)* (Technical Report E-0047). Department of Wind Energy, Technical University of Denmark.

[^4]: Fourer, R., Jun M., Kipp M. (2010). [OSiL: An Instance Language for Optimization](https://www.coin-or.org/OS/publications/OSiL%201_0%20FINAL.pdf). *Computational Optimization and Applications 45*(1): 181–203.

[^5]: Gay, D. (1995). [*Writing .nl Files*](https://cfwebprod.sandia.gov/cfdocs/CompResearch/docs/nlwrite20051130.pdf) (SAND2005-7907P). Sandia National Laboratories, Albuquerque, NM.

[^6]: Lubin, M. et al. (2017). [MathOptInterface.jl](http://www.juliaopt.org/MathOptInterface.jl/latest/). URL:[MathOptInterface.jl](http://www.juliaopt.org/MathOptInterface.jl/latest/)
