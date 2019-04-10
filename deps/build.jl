using HTTP

escape(s::String) = replace(s, "\\" => "\\\\")

function download_schema(schema_git_tag)
    schema_path = joinpath(@__DIR__, "$(schema_git_tag).json")
    r = HTTP.request("GET", "https://raw.githubusercontent.com/odow/MathOptFormat/$(schema_git_tag)/mof.schema.json")
    if r.status == 200
        open(schema_path, "w") do io
            write(io, String(r.body))
        end
        open(joinpath(@__DIR__, "deps.jl"), "w") do io
            write(io, "const SCHEMA_PATH = \"$(escape(schema_path))\"\n")
        end
    else
        error("Unable to download the latest MathOptFormat schema.\n" *
              "HTTP status: $(r.status)\n" *
              "HTTP body: $(String(r.body))")
    end
end

# Update this tag whenever github.com/odow/MathOptFormat releases a new update to
# the schema (providing that any code in this package is also updated).
const SCHEMA_GIT_TAG = "v0.0.0-alpha"

download_schema(SCHEMA_GIT_TAG)
