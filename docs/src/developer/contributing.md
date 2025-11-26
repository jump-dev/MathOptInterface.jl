# [Contributing](@id contributing_to_mathoptinterface)

This document explains how to contribute code to MathOptInterface.

## Obtain the source code

The easiest way to obtain the source code for MathOptInterface is to run:
```julia
julia> import Pkg

julia> Pkg.dev("MathOptInterface")
```
This will download the MathOptInterface Git repository to `~/.julia/dev/MathOptInterface`.
If you're on Windows, this will be `C:\\Users\\<my_name>\\.julia\\dev\\MathOptInterface`.

Alternatively, you can use `git` to clone the source to a directory of your
choosing:
```
$ cd /some/local/path
$ git clone https://github.com/jump-dev/MathOptInterface.jl.git
```

## Make changes

Code development in MathOptInterface follows typical git development practices.

First, go to [https://github.com/jump-dev/MathOptInterface.jl](https://github.com/jump-dev/MathOptInterface.jl)
and click the "Fork" button in the top-right corner. This will create a copy of
MathOptInterface under your GitHub account.

Then, tell your local git about your new fork:
```
export GITHUB_ACCOUNT="odow"  # Replace with your name
cd ~/.julia/dev/MathOptInterface
git remote add ${GITHUB_ACCOUNT} https://github.com/${GITHUB_ACCOUNT}/MathOptInterface.jl.git
```

Before making changes to the source, ensure that you have the latest copy, and
checkout a new branch:
```
git checkout master
git pull
git checkout -b my_new_branch
```

Now you can make changes by editing the local source code. See [Running the tests](@ref)
for how to check if your code passes the tests.

Once you have finished making changes, make a commit and push your branch to
GitHub:
```
git add .
git commit -m "A message describing what you changed"
git push -u $GITHUB_ACCOUNT my_new_branch
```
Finally, go to [https://github.com/jump-dev/MathOptInterface.jl](https://github.com/jump-dev/MathOptInterface.jl)
and follow the instructions that pop up to open a pull request.

## Running the tests

There are lot of tests in MathOptInterface. Running them in their entirety can
take a long time.

The easiest way to run the tests is to run:
```julia
julia> import Pkg

julia> Pkg.test("MathOptInterface")
```
The downside to the approach is that it will run all of the tests, and it will
recompile MathOptInterface from scratch, even if you have made very trivial
changes to the source code.

A faster approach is to use [Revise.jl](https://github.com/timholy/Revise.jl).

First, install Revise in your global package environment:
```
$ julia

julia> import Pkg

julia> Pkg.add("Revise")
```

Then, `cd` to the source code of MathOptInterface, and start Julia with the
project set to `.` (for `~/.julia/dev/MathOptInterface/Project.toml`):
```
$ cd ~/.julia/dev/MathOptInterface
$ julia --project=.
```

To run the tests, load Revise, and then include the relevant test file. For
example:
```julia
julia> using Revise

julia> include("test/Nonlinear/runtests.jl")

julia> # make changes to `src/Nonlinear`

julia> include("test/Nonlinear/runtests.jl")
```
You can also run other tests such as `test/Bridges/Constraint/runtests.jl`, or
any individual file, such as `test/Utilities/distance_to_set.jl`.

There is one complication: `JSONSchema` is a test-time dependency that is not
present in the default project. If you want to run `test/FileFormats/MOF/MOF.jl`,
you will first need to install the package (`Pkg.test("MathOptInterface")` does
this automatically):
```julia
julia> import Pkg; Pkg.add("JSONSchema")
```
If you have installed JSONSchema, you can also run all the tests with
`include("test/runtests.jl")`.

Finally, running the tests locally is best practice, but it is not required.
When you open a pull request, our automated CI will run all of the tests and
highlight any failing tests that need to be fixed.

A comment from `@odow`: when I'm working on a feature, I make local changes,
run the most relevant test file with `include`, and then open a PR. If the
change caused a test to fail in some other part of the codebase, I then
`include` the file with the failing test locally to debug my follow-up changes.

## Building the documentation

Building the documentation follows a similar practice with Revise. First, `cd`
to the source code of MathOptInterface, but this time, start Julia with
`--project=docs`.
```
$ cd ~/.julia/dev/MathOptInterface

$ julia --project=docs
```

Then, ensure that MathOptInterface (in our current directory) has been added as
a development dependency:
```julia
julia> import Pkg; Pkg.dev(".")
[...lines omitted...]
```
Finally, load `Revise` and then build the docs:
```julia
julia> using Revise

julia> include("docs/make.jl")
```
If you make changes to the docs or to the Julia source code (for example, the
docstrings), you can re-run `include` to quickly rebuild the docs.
