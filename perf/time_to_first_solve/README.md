# Tracking the time-to-first-solve

This script is used to benchmark the
[time-to-first-solve issue](https://github.com/jump-dev/MathOptInterface.jl/issues/1313).

It can be run as follows:
```
cd ~/.julia/dev/MathOptInterface/perf/time_to_first_solve
julia --project=. script.jl clp
julia --project=. script.jl clp --no-bridge
julia --project=. script.jl glpk
julia --project=. script.jl glpk --no-bridge
```
