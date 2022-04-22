# Copyright (c) 2017: Miles Lubin and contributors
# Copyright (c) 2017: Google Inc.
#
# Use of this source code is governed by an MIT-style license that can be found
# in the LICENSE.md file or at https://opensource.org/licenses/MIT.

# This file is automatically created by the folowing script. Only re-run it if
# AMPL adds new opcodes (which is unlikely).
# ```julia
# using ASL_jll
# open("opcode.jl", "w") do io
#     filename = joinpath(ASL_jll.artifact_dir, "include", "opcode.hd")
#     for line in readlines(filename)
#         items = split(line, c -> c == '\t' || c == ' '; keepempty = false)
#         println(io, "const ", items[2], " = ", items[3])
#     end
# end
# ```

const OPPLUS = 0
const OPMINUS = 1
const OPMULT = 2
const OPDIV = 3
const OPREM = 4
const OPPOW = 5
const OPLESS = 6
const MINLIST = 11
const MAXLIST = 12
const FLOOR = 13
const CEIL = 14
const ABS = 15
const OPUMINUS = 16
const OPOR = 20
const OPAND = 21
const LT = 22
const LE = 23
const EQ = 24
const GE = 28
const GT = 29
const NE = 30
const OPNOT = 34
const OPIFnl = 35
const OP_tanh = 37
const OP_tan = 38
const OP_sqrt = 39
const OP_sinh = 40
const OP_sin = 41
const OP_log10 = 42
const OP_log = 43
const OP_exp = 44
const OP_cosh = 45
const OP_cos = 46
const OP_atanh = 47
const OP_atan2 = 48
const OP_atan = 49
const OP_asinh = 50
const OP_asin = 51
const OP_acosh = 52
const OP_acos = 53
const OPSUMLIST = 54
const OPintDIV = 55
const OPprecision = 56
const OPround = 57
const OPtrunc = 58
const OPCOUNT = 59
const OPNUMBEROF = 60
const OPNUMBEROFs = 61
const OPATLEAST = 62
const OPATMOST = 63
const OPPLTERM = 64
const OPIFSYM = 65
const OPEXACTLY = 66
const OPNOTATLEAST = 67
const OPNOTATMOST = 68
const OPNOTEXACTLY = 69
const ANDLIST = 70
const ORLIST = 71
const OPIMPELSE = 72
const OP_IFF = 73
const OPALLDIFF = 74
const OPSOMESAME = 75
const OP1POW = 76
const OP2POW = 77
const OPCPOW = 78
const OPFUNCALL = 79
const OPNUM = 80
const OPHOL = 81
const OPVARVAL = 82
const N_OPS = 83
