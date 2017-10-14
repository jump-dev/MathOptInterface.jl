# MathOptFormat.jl
#### _A new file format for mathematical optimization_

| **Build Status** | **Coverage** |
|:--------------------:|:----------------:|
| [![Build Status][build-img]][build-url] | [![Codecov branch][codecov-img]][codecov-url]

**The file format is under active development. No backward compatibility yet!**

### Background

In order to use an optimization solver, it is necessary to communicate a model
instance to the solver [1].

### Why Yet Another Format?

 - restrictive input (MPS)
 - restrictive standard form (MPS, LP)
 - no agreed upon standard (MPS, LP)
 - need special parsers (MPS, LP, NL, CBF)
 - too general (OSiL)
 - too verbose (OSiL)



### Design Decisions

[1] identified four key issues that apply to the design of any instance format.
We shall now go through each issue and outline the design decisions behind
MathOptFormat.

#### 1. Separation of Functionality
 - Problem
 - Solver Options
 - Solver Results
 - Instance modification

#### 2. Optimization Instance Format
 - Algebraic
    - pros: human readable, close to mathematical representation
    - cons: difficult to describe in detail
 - Packed:
    - pros: efficient for reading and writing, concise
    - cons: not human readable
 - Markup
    - pros: human readable, extensible, flexible
    - cons: verbose

#### 3. Design of Format: Level of Detail and Flexibility

We choose the "function-in-set" standard form approach of MathOptInterface.jl.

This is a high-level standard form that enables a high degree of flexibility,
yet allows a detailed description of the individual components.

This allows new functions and sets to be added in a compatible way.

The choice of a markup language (JSON) also allows arbitrary fields to be added
to extend the format.

#### 4. Design of In-Memory Instance Object

The design of MathOptFormat corresponds exactly to the in-memory design of
MathOptInterface.

### Specification

### References

[1] Gassmann, H., Ma, J., Martin, K., 2010. Instance Formats for Mathematical Optimization Models. Wiley Encyclopedia of Operations Research and Management Science.

[build-img]: https://travis-ci.org/odow/MathOptFormat.jl.svg?branch=master
[build-url]: https://travis-ci.org/odow/MathOptFormat.jl

[codecov-img]: https://codecov.io/github/odow/MathOptFormat.jl/coverage.svg?branch=master
[codecov-url]: https://codecov.io/github/odow/MathOptFormat.jl?branch=master
