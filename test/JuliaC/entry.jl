# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# The entry point of the compiled MyApp. `test_juliac.jl` copies it next to
# `dispatch.trace`, the `--trace-dispatch` output of a run of MyApp in ordinary
# Julia, so that the methods reached through dynamic dispatch are compiled.

import MyApp.main

# The signatures of the trace refer to the modules by name.
for M in values(Base.loaded_modules)
    if !isdefined(@__MODULE__, nameof(M))
        Core.eval(@__MODULE__, :(const $(nameof(M)) = $M))
    end
end

for line in eachline(joinpath(@__DIR__, "dispatch.trace"))
    signature = try
        Core.eval(@__MODULE__, Meta.parse(line).args[2])
    catch
        continue  # For instance, a closure that only exists in the traced run
    end
    Base.Experimental.entrypoint(signature)
end
