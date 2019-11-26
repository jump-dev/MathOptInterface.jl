function error_mode(mode::String)
    throw(ArgumentError("For dealing with compressed data, mode must be \"r\" or \"w\"; $mode given"))
end

"""
    abstract type AbstractCompressionScheme end

Base type to implement a new compression scheme for MathOptFormat. To do so,
create a concrete subtype (e.g., named after the compression scheme) and
implement `open(f::Function, filename::String, mode::String, ::YourScheme)`.
"""
abstract type AbstractCompressionScheme end

struct AutomaticCompression <: AbstractCompressionScheme end
# No open() implementation, this would not make sense (flag to indicate that _filename_to_compression should be called).

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

# struct Xz <: AbstractCompressionScheme end
# function _compressed_open(
#     f::Function, filename::String, mode::String, ::Xz
# )
#     return if mode == "w"
#         Base.open(f, CodecXz.XzDecompressorStream, filename, mode)
#     elseif mode == "r"
#         Base.open(f, CodecXz.XzCompressorStream, filename, mode)
#     else
#         error_mode(mode)
#     end
# end

function _automatic_compression(filename::String, compression::AbstractCompressionScheme)
    if compression == AutomaticCompression()
        return _filename_to_compression(filename)
    end
    return compression
end

function _filename_to_compression(filename::String)
    if endswith(filename, ".bz2")
        return Bzip2()
    elseif endswith(filename, ".gz")
        return Gzip()
    # elseif endswith(filename, ".xz")
    #     return Xz()
    else
        return NoCompression()
    end
end
