# This script builds the list of recognized ASL opcodes using the header files
# in ASL_jll. Only re-run it if AMPL adds new opcodes (which is unlikely).

using ASL_jll

open("opcode.jl", "w") do io
    println("""
    # Do not modify. This file is automatically created by the script in `gen.jl`.
    """)
    filename = joinpath(ASL_jll.artifact_dir, "include", "opcode.hd")
    for line in readlines(filename)
        items = split(line, c -> c == '\t' || c == ' '; keepempty = false)
        println(io, "const ", items[2], " = ", items[3])
    end
end
