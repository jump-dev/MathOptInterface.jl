var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "MathOptFormat",
    "title": "MathOptFormat",
    "category": "page",
    "text": "The file format is under active development. No backward compatibility yet!"
},

{
    "location": "index.html#Background-1",
    "page": "MathOptFormat",
    "title": "Background",
    "category": "section",
    "text": "In order to use an optimization solver, it is necessary to communicate a model instance to the solver.[1] Many different instance formats have been proposed over the years, but only a few (such as MPS) have become the industry standard.Each format is a product of its time in history, and the problem class it tried to address. For example, we retain the rigid input format of the MPS file that was designed for 1960's punchcards despite the obsolescence of this technology.[2] Although it has since been  extended to problem classes such as nonlinear and stochastic linear programming, MPS was not designed with extensibility in mind. This has led some authors (such as [3]) to conclude that developing a new format is easier than extending the existing MPS format.The LP file-format also dates back to the work of Orchard-Hays who attempted to correct the ''mistakes'' of the MPS file-format by creating a human-readable, row-oriented format for mathematicians.[2] However, due to its age, there is no longer a single standard for the LP file-format. This has led to subtle differences between implementations in different readers that hampers the usefulness of the format as a format for interchange. Much like the MPS file, it is also limited in the types of problems it can represent and was not designed for extensibility.In constrast to the LP file, the .NL file explicitly aims for machine-readability at the expense of human-readability.[5] It is also considerably more flexible in the problem classes it can represent (in particular, arbitrary nonlinear functions are supported). However, once again, the format is not extensible to new problem formats, and lacks support for conic problems.More recently, considerable work has been put into developing the OSiL format.[4] In developing OSiL, Fourer et al. idenfied many of the challenges and limitations of previous formats and attempted to overcome them. In particular, they choose to use XML as the basis for their format. This removed the burden of writing custom readers and writers for each programming language that wished to interface with optimization software and allowed more focus on the underlying data-structures. XML is also human-readable and can be rigidly specified with a schema to prevent the profilferation of similar, but incompatible versions. The XML approach also allows for easy extensibility and can support multiple problem classes including nonlinear, stochastic, and conic.However, despite the many apparent advantages of the OSiL format, we believe it has enough short-comings to justify the development of a new instance format. Two of the main reasons are the verbosity of the XML format, and the lack of a strong, extensible standard form."
},

{
    "location": "index.html#Project-Goals-1",
    "page": "MathOptFormat",
    "title": "Project Goals",
    "category": "section",
    "text": "With this understanding of the history and evolution of different file-formats, the following goals guided our development of the MathOptFormat:Human-readable: the format should be able to be read and edited by a  human.\nMachine-readable: the format should be able to be read by a variety of different programming languages without needing to write custom parsers in each language.\nStandardized: the format should conform to a well described  ''standard-form'' that is unambiguous.\nExtensible: the format should be able to be easily extended to incorporate  new problem-classes as they arise."
},

{
    "location": "index.html#The-MathOptInterface-Standard-Form-1",
    "page": "MathOptFormat",
    "title": "The MathOptInterface Standard Form",
    "category": "section",
    "text": "MathOptInterface is a solver abstraction layer for mathematical optimization solvers.[6] One if the core design goals of MathOptInterface is for it to\"be simple and extensible, unifying linear, quadratic, and conic optimization, and seamlessly facilitate extensions to essentially arbitrary constraints and functions (e.g., indicator constraints, complementarity constraints, and piecewise linear functions).\"The MathOptInterface standard form problem is:beginalign\n     min_x in mathbbR^n  f_0(x)\n    \n     textst  f_i(x)  in mathcalS_i  i = 1 ldots m\nendalignwhere f_i(x) is an arbitrary function and mathcalS_i is an arbitrary set.For example, instead of thinking of the constraint 3x + y le 1 as a ''less than or equal to\" constraint, we can think of the constraint as enforcing the function 3x + y to be inside the set (-infty 1.This approach turns out to be very general, as instead of thinking of variable as being ''binary'', we say the function x belongs to the set 0 1. Instead of a variable being semicontinuous, we say the function x belongs to the set 0 cup l u."
},

{
    "location": "index.html#Why-JSON?-1",
    "page": "MathOptFormat",
    "title": "Why JSON?",
    "category": "section",
    "text": "One reason for developing a new instance format rather than improving OSiL is its use of XML. Although XML has many advantages (a strictly defined schema for example), the format is almost too general (and too verbose) for our purposes.In constrast, JSON is a much simpler format, and is only able to store six different data types: string, number, object, array, boolean and null.In almost all programming languages, these map directly to native language constructs (object being a dictionary or a key-value mapping).https://www.json.org/xml.html"
},

{
    "location": "index.html#The-Format-1",
    "page": "MathOptFormat",
    "title": "The Format",
    "category": "section",
    "text": "A MathOptFormat instance is a text representation of the model as a JSON object. The object must have the following fields: version, sense, variables, objective and constraints. Users may also choose to add optional fields such as author to provide contextual information for humans reading the instance. Parsers may choose to ignore these fields."
},

{
    "location": "index.html#Versioning-1",
    "page": "MathOptFormat",
    "title": "Versioning",
    "category": "section",
    "text": "The version field stores number of the earliest version of MathOptFormat that supported all the features in the instance.\"version\": 0"
},

{
    "location": "index.html#Optimization-Sense-1",
    "page": "MathOptFormat",
    "title": "Optimization Sense",
    "category": "section",
    "text": "The sense field must contain a string that is either \"min\" or \"max\". No other values are allowed.\"sense\": \"min\""
},

{
    "location": "index.html#Variables-1",
    "page": "MathOptFormat",
    "title": "Variables",
    "category": "section",
    "text": "The variables field contains a list of objects (one for each variable in the model). Each variable object must contain at least the field name which records a unique string. Duplicate names are not allowed. In addition, the variable object can optionally contain any MathOptInterface variable attributes (for example VariablePrimalStart).\"variables\": [\n    {\"name\": \"x\"},\n    {\"name\": \"y\", \"VariablePrimalStart\": 1.0}\n]"
},

{
    "location": "index.html#MathOptInterface-Functions-1",
    "page": "MathOptFormat",
    "title": "MathOptInterface Functions",
    "category": "section",
    "text": "A MathOptInterface function can be represented by a JSON object. Every function must have the field head which contains a string that is identical to the name of the MathOptInterface function ( SingleVariable, VectorOfVariables, ScalarAffineFunction, VectorAffineFunction, ScalarQuadraticFunction, and VectorQuadraticFunction).In addition, there must be a one-to-one mapping between the field names of the MathOptInterface type, and the fields in the JSON object. However, instead of refering to variables in the model using VariableReferences, the MathOptFormat version uses the string that corresonds to the name of the variable in the list variables (defined above).For example, the SingleVariable function has a single field variable. For example:{\n    \"head\": \"SingleVariable\",\n    \"variable\": \"x\"\n}"
},

{
    "location": "index.html#MathOptInterface-Sets-1",
    "page": "MathOptFormat",
    "title": "MathOptInterface Sets",
    "category": "section",
    "text": "MathOptInterface Sets are represented in a similar manner to MathOptInterface functions.{\n    \"head\": \"LessThan\",\n    \"upper\": 1.0\n}"
},

{
    "location": "index.html#Objective-Function-1",
    "page": "MathOptFormat",
    "title": "Objective Function",
    "category": "section",
    "text": "The objective field contains a MathOptInterface function (f_0(x)).\"objective\": {\n    \"head\": \"ScalarAffineFunction\",\n    \"variables\": [\"x\"],\n    \"coefficients\": [2.0],\n    \"constant\": 1.0\n}"
},

{
    "location": "index.html#Constraints-1",
    "page": "MathOptFormat",
    "title": "Constraints",
    "category": "section",
    "text": "Each constraint is a JSON object with two required fields: set, and function. The values associated with these fields must be a valid MathOptInterface set and function respectively. In addition, the object can contain MathOptInterface constraint attributes such as name, ConstraintPrimalStart, and ConstraintDualStart.{\n    \"constraints\": [\n        {\n            \"name\": \"c1\",\n            \"set\": {\n                \"head\": \"LessThan\", \"upper\": 1.0\n            },\n            \"function\": {\n                \"head\": \"ScalarAffineFunction\",\n                \"variables\": [\"x\"],\n                \"coefficients\": [2.0],\n                \"constant\": 1.0\n\n            }\n        }\n\n    ]\n}"
},

{
    "location": "index.html#Example-1",
    "page": "MathOptFormat",
    "title": "Example",
    "category": "section",
    "text": "Consider the following LP:beginalign\n     min_xy  2x + y\n    \n     textst  x + y = 1\n    \n                      x Binary\nendalign"
},

{
    "location": "index.html#MathOptFormat-1",
    "page": "MathOptFormat",
    "title": "MathOptFormat",
    "category": "section",
    "text": "We can represent this in the MathOptFormat as{\n    \"author\": \"Oscar Dowson\",\n    \"description\": \"A simple example for the MathOptFormat documentation\",\n    \"version\": 0,\n    \"sense\": \"min\",\n    \"variables\": [{\"name\": \"x\"}, {\"name\": \"y\"}],\n    \"objective\": {\n        \"head\": \"ScalarAffineFunction\",\n        \"variables\": [\"x\", \"y\"],\n        \"coefficients\": [2, 1],\n        \"constant\": 0\n    }\n    \"constraints\": [\n        {\n            \"name\": \"x+y≥1\",\n            \"set\": {\"head\": \"GreaterThan\", \"lower\": 1}\n            \"function\": {\n                \"head\": \"ScalarAffineFunction\",\n                \"variables\": [\"x\", \"y\"],\n                \"coefficients\": [1, 1],\n                \"constant\": 0\n            }\n        },\n        {\n            \"name\": \"x ∈ {0,1}\",\n            \"set\": {\"head\": \"ZeroOne\"}\n            \"function\": {\n                \"head\": \"SingleVariable\",\n                \"variable\": \"x\",\n            }\n        }\n    ]\n\n}Note that in addition to the required fields, we can store additional information (such as the author and a description of the model) that is not necessary to define the model instance, but is useful human-readable metadata."
},

{
    "location": "index.html#LP-1",
    "page": "MathOptFormat",
    "title": "LP",
    "category": "section",
    "text": "Compared to the LP formulation (below), the MathOptFormat vesion is verbose and less human-readable. However, it does not require a specialised parser to read, conforms to a well standardized specification, and is extensible./ Author: Oscar Dowson\n/ Description: A simple example for the MathOptFormat documentation\nMinimize\nobj: 2x + y\nSubject To\nc1: x + y >= 1\nBounds\ny free\nBinary\nx\nEnd"
},

{
    "location": "index.html#OSiL-1",
    "page": "MathOptFormat",
    "title": "OSiL",
    "category": "section",
    "text": "Compared to the OSiL version (below), we would argue that the MathOptFormat is more human-readable, better standardized, and more extensible.<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<osil xmlns=\"os.optimizationservices.org\">\n    <instanceHeader>\n        <name>MathOptFormat Example</name>\n        <source>Oscar Dowson</source>\n        <description>A simple example for the MathOptFormat documentation</description>\n    </instanceHeader>\n    <instanceData>\n        <variables numberOfVariables=\"2\">\n            <var lb=\"-INF\" name=\"x\" type=\"B\"/>\n            <var lb=\"-INF\" name=\"y\"/>\n        </variables>\n        <objectives numberOfObjectives=\"1\">\n            <obj maxOrMin=\"min\" numberOfObjCoef=\"2\">\n                <coef idx=\"1\">2</coef>\n                <coef idx=\"2\">1</coef>\n            </obj>\n        </objectives>\n        <constraints numberOfConstraints=\"1\">\n            <con lb=\"1.0\"/>\n        </constraints>\n        <linearConstraintCoefficients numberOfValues=\"2\">\n            <start>\n                <el>0</el><el>1</el>\n            </start>\n            <colIdx>\n                <el>0</el><el>1</el>\n            </colIdx>\n            <value>\n                <el>1</el><el>1</el>\n            </value>\n        </linearConstraintCoefficients>\n    </instanceData>\n</osil>"
},

{
    "location": "index.html#MathOptFormat.jl-1",
    "page": "MathOptFormat",
    "title": "MathOptFormat.jl",
    "category": "section",
    "text": "using MathOptFormat, MathOptInterface\nconst MOI = MathOptInterface\n\nm = MathOptFormat.MOFInstance()\n\n# Add in extra metadata\nm[\"author\"] = \"Oscar Dowson\"\nm[\"description\"] = \"A simple example for the MathOptFormat documentation\"\n\n# Create variables\n[x, y] = MOI.addvariables!(m, 2)\nMOI.set!(m, MOI.VariableName(), x, \"x\")\nMOI.set!(m, MOI.VariableName(), y, \"y\")\n\n# Set objective\nMOI.set!(m, MOI.ObjectiveSense(), MOI.MinSense)\nMOI.set!(m,\n    MOI.ObjectiveFunction(),\n    MOI.ScalarAffineFunction([x,y],[2,1],0)\n)\n\n# The constraint: x+y≥1 becomes x+y ∈ [1, ∞)\nc1 = MOI.addconstraint!(m,\n    MOI.ScalarAffineFunction([x,y],[1,1],0),\n    MOI.GreaterThan(1)\n)\nMOI.set!(m, MOI.ConstraintName(), c1, \"x+y≥1\")\n\n# The constraint: x, Binary becomes x ∈ {0, 1}\nc2 = MOI.addconstraint!(m,\n    MOI.SingleVariable(x),\n    MOI.ZeroOne()\n)\nMOI.set!(m, MOI.ConstraintName(), c2, \"x ∈ {0,1}\")\n\n# Write the instance to file\nMOI.writeinstance(m, \"example.mof.json\")"
},

{
    "location": "index.html#References-1",
    "page": "MathOptFormat",
    "title": "References",
    "category": "section",
    "text": "[1]: Gassmann, H., Ma, J., Martin, K. (2010). Instance Formats for Mathematical Optimization Models. In Wiley Encyclopedia of Operations Research and Management Science.[2]: Orchard-Hays, W. (1984). History of Mathematical Programming Systems. Annals of the History of Computing, 6(3).[3]: Friberg, H. (2014). The conic benchmark format: version 1 - technical reference manual (Technical Report E-0047). Department of Wind Energy, Technical University of Denmark.[4]: Fourer, R., Jun M., Kipp M. (2010). OSiL: An Instance Language for Optimization. Computational Optimization and Applications 45(1): 181–203.[5]: Gay, D. (1995). Writing .nl Files (SAND2005-7907P). Sandia National Laboratories, Albuquerque, NM.[6]: Lubin, M. et al. (2017). MathOptInterface.jl. URL:MathOptInterface.jl"
},

]}
