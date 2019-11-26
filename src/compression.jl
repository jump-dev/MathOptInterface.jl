function error_mode(mode::String)
    throw(ArgumentError("Compressed mode must be \"r\" or \"w\". Got: $mode."))
end

"""
    abstract type AbstractCompressionScheme end

Base type to implement a new compression scheme for MathOptFormat.

To do so, create a concrete subtype (e.g., named after the compression scheme)
and implement:

    _compressed_open(f::Function, filename::String, mode::String, ::YourScheme)
"""
abstract type AbstractCompressionScheme end

struct NoCompression <: AbstractCompressionScheme end
function _compressed_open(
    f::Function, filename::String, mode::String, ::NoCompression
)
    return Base.open(f, filename, mode)
end

struct Gzip <: AbstractCompressionScheme end
function _compressed_open(
    f::Function, filename::String, mode::String, ::Gzip
)
    return if mode == "w"
        Base.open(f, CodecZlib.GzipCompressorStream, filename, mode)
    elseif mode == "r"
        Base.open(f, CodecZlib.GzipDecompressorStream, filename, mode)
    else
        error_mode(mode)
    end
end

struct Bzip2 <: AbstractCompressionScheme end
function _compressed_open(
    f::Function, filename::String, mode::String, ::Bzip2
)
    if mode == "w"
        Base.open(f, CodecBzip2.Bzip2CompressorStream, filename, mode)
    elseif mode == "r"
        Base.open(f, CodecBzip2.Bzip2DecompressorStream, filename, mode)
    else
        error_mode(mode)
    end
end

struct AutomaticCompression <: AbstractCompressionScheme end
function _compressed_open(
    f::Function, filename::String, mode::String, ::AutomaticCompression
)
    if endswith(filename, ".bz2")
        return _compressed_open(f, filename, mode, Bzip2())
    elseif endswith(filename, ".gz")
        return _compressed_open(f, filename, mode, Gzip())
    else
        return _compressed_open(f, filename, mode, NoCompression())
    end
end
