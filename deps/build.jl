function download_schema(schema_git_tag)
    schema_path = joinpath(@__DIR__, "$(schema_git_tag).json")
    url = "https://raw.githubusercontent.com/odow/MathOptFormat/$(schema_git_tag)/mof.schema.json"
    download(url, schema_path)
    escaped_schema_path = replace(schema_path, "\\" => "\\\\")
    open(joinpath(@__DIR__, "deps.jl"), "w") do io
        write(io, "const SCHEMA_PATH = \"$(escaped_schema_path)\"\n")
    end
end

# Update this tag whenever github.com/odow/MathOptFormat releases a new update
# to the schema (providing that any code in this package is also updated).
const SCHEMA_GIT_TAG = "v0.2.1"

download_schema(SCHEMA_GIT_TAG)
