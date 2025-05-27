import sys
import itertools
import re
import platform

# Suivante Programming Language


# Install folders == /opt/suivante or C:\suivante
# Library folders == /opt/suivante/lib or C:\suivante\lib


global memvars
memvars = {}


def ExtractBalancedBlock(code, start_token="for", offset=0):
    blocks = []
    pattern = re.compile(rf'\b{re.escape(start_token)}\b')  # exact word match
    for match in pattern.finditer(code):
        start = match.start()
        brace_index = code.find("{", start)
        if brace_index == -1:
            continue  # no block found

        brace_count = 0
        i = brace_index
        while i < len(code):
            if code[i] == '{':
                brace_count += 1
            elif code[i] == '}':
                brace_count -= 1
                if brace_count == 0:
                    # Return the block along with the starting line number
                    blocks.append((LineMap[code[:start].count("\n")] + 1 + offset, code[start:i+1]))
                    break
            i += 1
        else:
            print(f"W: unmatched braces in block starting at index {start}")
    return blocks


def ExtractFunctions(code, declaration="func"):
    functions = []
    i = 0
    while i < len(code):
        # Skip whitespace to find clean 'func'
        while i < len(code) and code[i] in " \t\r\n":
            i += 1

        if code[i:i+4] == declaration and (i == 0 or not code[i-1].isalnum()):
            start_index = i
            brace_index = code.find("{", i)
            if brace_index == -1:
                break  # No function body found

            Braces = 0
            j = brace_index
            while j < len(code):
                if code[j] == "{":
                    Braces += 1
                elif code[j] == "}":
                    Braces -= 1
                    if Braces == 0:
                        # Full function captured
                        functions.append((LineMap[code[:start_index].count("\n")] + 1, code[start_index:j+1]))
                        i = j + 1  # move i past this function
                        break
                j += 1
            else:
                print(f"E: Missing closing brace for function starting at line {LineMap[code[:start_index].count('\n')]+1}")
                sys.exit()
        else:
            i += 1
    return functions


def FindStructs(STRUCTS):
    for struct in STRUCTS:
        idx, name = struct
        struct_name = name.split()[1]
        foundstructs.append(struct_name)

def ShuntingYard(expression):
    expression = expression.replace(" ", "").replace("*", " * ").replace("/", " / ").replace("-", " - ").replace("+", " + ").replace("^", " ^ ").replace("(", " ( ").replace(")", " ) ")

    output = []  # This will hold the output expression in Reverse Polish Notation (RPN)
    stack = []   # This will hold operators and parentheses
    tokens = expression.split()  # Split the expression into tokens (i.e., numbers and operators)

    # Define operator precedence (lower number = higher precedence)
    precedences = {'+': 1, '-': 1, '*': 2, '/': 2, '%': 2, '^': 3}
    # Define associativity of operators: True for left-associative, False for right-associative
    associativity = {'+': True, '-': True, '*': True, '/': True, '^': False}

    operations = "+-/%*^()"

    for token in tokens:
        if token not in operations:  # If the token is a number, add it to the output
            output.append(token)
        elif token == '(':  # Left parenthesis, just push it to the stack
            stack.append(token)
        elif token == ')':  # Right parenthesis, pop from stack until we see a left parenthesis
            while stack and stack[-1] != '(':
                output.append(stack.pop())
            stack.pop()  # Pop the left parenthesis
        else:  # If the token is an operator
            while (stack and stack[-1] != '(' and
                   (precedences.get(stack[-1], 0) > precedences[token] or
                    (precedences.get(stack[-1], 0) == precedences[token] and associativity[token]))):
                output.append(stack.pop())
            stack.append(token)

    # Pop the remaining operators from the stack to the output
    while stack:
        output.append(stack.pop())

    return ' '.join(output)



def Tokenize(Code):
    # Remove comments
    Code = re.sub(r"//.+", "", Code)
    # Split the code into a list
    Code = Code.split("\n")

    OldCode = Code.copy()
    Code = []

    global LineMap
    LineMap = []

    # Remove empty lines
    for i in range(len(OldCode)):
        if OldCode[i] not in ("", " ", "  ", "   ", "    "):
            Code.append(OldCode[i])
            LineMap.append(i)

    # Join the code again
    Code = "\n".join(Code)
    #print(Code)

    def tokenizevariabledec(code, IDX, constantoverwrite=False, constant=False):
        Attributes = []

        # Check for attributes
        if re.search(r"\bconstvar\b", code):
            Attributes.append("const")
        if re.search(r"\bmanvar\b", code):
            Attributes.append("man")
        if constantoverwrite and "const" not in Attributes:
            Attributes.append("const")

        # Regex to extract type, name, and value
        match = re.search(
            rf"(?:constvar|var|manvar)?\s*"
            rf"(?P<type>{'|'.join(['int', 'str', 'bool', 'float', 'char'] + foundstructs)})\s+"
            rf"(?P<name>\w+)\s*=\s*(?P<value>.+?);?$", code
        )

        if not match:
            raise ValueError(f"Could not parse variable declaration: {code}")

        Type = match.group("type").strip()
        Name = match.group("name").strip()
        Value = match.group("value").strip()

        return IDX, "VARDEC", Attributes, [Type, Name, Value]



    def tokenizefuncvars(code):
        TypeRaw = re.findall(rf"(?:int|str|bool|float|char|{'|'.join(foundstructs)})", code)
        Name = re.sub(r"\s", "", re.sub(rf"(?:int|str|bool|float|char|{'|'.join(foundstructs)})", "", code))
        Attributes = []
        if re.search(r"\s*const.*var", code):       # Checking for constant attribute
            Attributes.append("const")

        #if re.search(r"\s*man.*var", code):         # Checking for manual attribute
        #    Attributes.append("man")

        #Value = re.sub(r"\=\s*", "", re.findall(r"\=\s*.+", code)[0])

        Type = []
        for j in TypeRaw:
            if j not in ('', ' '):
                Type.append(j)

        #print(Type[0])

        return Attributes, Type, Name


    def generatetokens(Code, offset=0):
        global foundstructs
        foundstructs = []


        # Remove unecessary whitespace before each line
        Code = re.sub(r"([\t\r\f\v]+)", "", Code, flags=re.DOTALL)

        # Match REGEX for all the functions

        importsraw = re.finditer(r"\s*#import\s+[<\"].+[>\"];\s", Code)
        ifsraw = ExtractBalancedBlock(Code, start_token="if", offset=offset)
        elsesraw = ExtractBalancedBlock(Code, start_token="else", offset=offset)
        elseifsraw = ExtractBalancedBlock(Code, start_token="elif", offset=offset)
        forloopsraw = ExtractBalancedBlock(Code, offset=offset)
        whileloopsraw = ExtractBalancedBlock(Code, start_token="while", offset=offset)
        structsraw = re.finditer(r"\s*struct\s+\w+\s*\{([\s\S]*?)\}\s*;", Code, flags=re.DOTALL)

                # Calculate the structs for later
        structs = []
        for VAL in structsraw:
            structs.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        FindStructs(structs)

        funcsraw = ExtractFunctions(Code)
        #fnsraw = ExtractFunctions(Code, declaration="fn")       # Support for both "fn" and "func"
        variabledecsraw = re.finditer(rf"\s*(?:(?:const|man)\s+){{0,2}}var\s+(?:int|str|bool|float|char|{'|'.join(foundstructs)})(?:\[\]|\*)*+\s+\w+\s*=\s*[^;]+;", Code)
        constvariabledecsraw = re.finditer(rf"\s*constvar\s+(?:int|str|bool|float|char|{'|'.join(foundstructs)})(?:\[\]|\*)*+\s+\w+\s*=\s*[^;]+;", Code)
        variablechangesraw = re.finditer(r"\s*\w+\s*(?:\+=|-=|\*=|/=|%=|\^=|>>=|<<=|\^=)\s*[^;]+;", Code)
        variablereassignraw = re.finditer(r"\s*\w+\s*(?<![=])=(?![=])\s*[^;]+;", Code)
        returnsraw = re.finditer(r"\s*return\s+.*;", Code)
        funccallsraw = re.finditer(r"\s*\w+\s*\([^\)]*\)\s*;", Code)


        imports = []
        for VAL in importsraw:
            imports.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        variabledecs = []
        for VAL in variabledecsraw:
            variabledecs.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        constantvariabledecs = []
        for VAL in constvariabledecsraw:
            constantvariabledecs.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        variablechanges = []
        for VAL in variablechangesraw:
            variablechanges.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        variablereassigns = []
        for VAL in variablereassignraw:
            variablereassigns.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        returns = []
        for VAL in returnsraw:
            returns.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        funccalls = []
        for VAL in funccallsraw:
            funccalls.append((LineMap[Code[:VAL.start()].count("\n")] + 1, VAL.group()))

        return imports, ifsraw, elsesraw, elseifsraw, forloopsraw, whileloopsraw, funcsraw, variabledecs, constantvariabledecs, variablechanges, variablereassigns, returns, funccalls, structs


    def tokenizescope(Code, offset=0):
        global foundstructs

        imports, ifsraw, elsesraw, elseifsraw, forloopsraw, whileloopsraw, funcsraw, variabledecs, constvariabledecs, variablechanges, variablereassigns, returns, funccalls, structs = generatetokens(Code)

        #print(constvariabledecs)

        Tokens = []

        BannedIDX = []

        for i in ifsraw:
            IDX, code = i
            try:
                Statement = re.findall(r"\s*if\s*\(", code)[0]
            except IndexError:
                print(f"E: Missing Parenthesis on if statement on line {IDX + offset}. \"{code}\"")
                sys.exit()

            args = re.findall(r"\(.+\)", code)[0][1:-1]
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "IF", [args], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in elsesraw:
            IDX, code = i
            Statement = re.findall(r"\s*else\s*", code)[0]
            #args = re.findall(r"\(.+\)", code)[0][1:-1]
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "ELSE", [], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in elseifsraw:
            IDX, code = i
            Statement = re.findall(r"\s*elif\s*", code)[0]
            args = re.findall(r"\(.+\)", code)[0][1:-1]
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "ELSE IF", [args], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in forloopsraw:
            IDX, code = i
            Statement = re.findall(r"\s*for\s*", code)[0]
            args = re.findall(rf"\s+\b(?:int|str|bool|float|char|{'|'.join(foundstructs)})\s+\w+ in .+", code)[0][1:-1]
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "FOR", [args], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in whileloopsraw:
            IDX, code = i
            try:
                Statement = re.findall(r"\s*while\s*", code)[0]
            except IndexError:
                print(f"E: Missing Parenthesis on while loop on line {IDX + offset}. \"{code}\"")
                sys.exit()

            args = re.findall(r"\(.+\)", code)[0][1:-1]
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "WHILE", [args], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in funcsraw:
            IDX, code = i
            Statement = re.sub(r"\s*{", "", re.findall(r"\s*.+[^{]", code)[0][:-1])
            args = re.split(r",\s*", re.findall(r"\(.*\)", code)[0][1:-1])
            Name = re.findall(rf"\b(?:int|str|bool|float|char|{'|'.join(foundstructs)})\s+(\w+)\s*\(", code)[0]
            Type = re.sub(r"func\s+", "", re.findall(rf"func\s+(?:int|str|bool|float|char|void|{'|'.join(foundstructs)})", code)[0])
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "FUNC", [Name, Type, [tokenizefuncvars(i) for i in args]], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in variabledecs:
            IDX, code = i
            Tokens.append(tokenizevariabledec(code, IDX + offset))


        for i in variablechanges:
            IDX, code = i
            Parts = re.split(r"\+=|-=|\*=|/=|%=|\^=|>>=|<<=|\^=", code.replace(" ", ""))
            Name = Parts[0].replace("\n", "")
            Change = Parts[1][:-1]
            Operator = re.findall(r"(?:\+=|-=|\*=|/=|%=|\^=|>>=|<<=|\^=)", code)

            Tokens.append((IDX + offset, "VARCHANGE", [Operator], [Name, Change]))


        for i in variablereassigns:
            IDX, code = i
            Parts = re.split(r"(?<![\+\-\*/%<>\^])=(?!=)", code.replace(" ", ""))
            if Parts[0] != code.replace(" ", "") and not re.search(rf"(?:int|str|bool|float|char|{'|'.join(foundstructs)})", code):
                Name = Parts[0].replace("\n", "")
                Change = Parts[1][:-1]
                Tokens.append((IDX + offset, "VARREASSIGN", [], [Name, Change]))


        for i in returns:
            IDX, code = i
            statement = re.findall(r"return\s*", code)
            args = re.findall(r"return.+", code)[0].replace("return", "").replace(" ", "")[:-1]
            Tokens.append((IDX + offset, "RETURN", [], ["FUNC", args]))


        for i in funccalls:
            IDX, code = i
            function = re.sub(r"\s*", "", re.findall(r".+[^\(]", code)[0])
            args = re.findall(r"\(.+[\)]", code)[0]
            Tokens.append((IDX + offset, "FUNCCALL", [], [function, args]))


        return Tokens#, imports, structs, funcsraw, ifsraw, elsesraw, elseifsraw, forloopsraw, whileloopsraw, variabledecs, variablechanges, variablereassigns, returns, funccalls


    def tokenizefirstscope(Code, offset=0):
        imports, ifsraw, elsesraw, elseifsraw, forloopsraw, whileloopsraw, funcsraw, variabledecs, constvariabledecs, variablechanges, variablereassigns, returns, funccalls, structs = generatetokens(Code)

        Tokens = []

        for i in funcsraw:
            IDX, code = i
            Statement = re.sub(r"\s*{", "", re.findall(r"\s*.+[^{]", code)[0][:-1])
            args = re.split(r",\s*", re.findall(r"\(.*\)", code)[0][1:-1])
            Name = re.findall(rf"\b(?:int|str|bool|float|char|{'|'.join(foundstructs)})\s+(\w+)\s*\(", code)[0]
            Type = re.sub(r"func\s+", "", re.findall(rf"func\s+(?:int|str|bool|float|char|void|{'|'.join(foundstructs)})", code)[0])
            #print(code)
            Cd = re.findall(r"\{.+\}", code, flags=re.DOTALL)[0][1:-1]
            Tokens.append((IDX + offset, "FUNC", [Name, Type, [tokenizefuncvars(i) for i in args]], tokenizescope(Cd, offset=IDX + offset - 1)))


        for i in constvariabledecs:
            IDX, code = i
            Tokens.append(tokenizevariabledec(code, IDX + offset, constantoverwrite=True, constant=True))

        for i in structs:
            IDX, code = i
            Name = re.sub(r"[\n\s]*", "", re.sub(r"\s*{", "", re.sub(r"struct\s+", "", re.findall(r"struct\s+.+[^{]", code)[0])))
            vars = re.split(r"\s*;\s*", re.sub(r"\s*{\s*", "", re.sub(r"\s*}\s*", "", re.findall(r"{.+[^}]", code, flags=re.DOTALL)[0])).replace("\n", ""))

            Tokens.append((IDX + offset, "STRUCT", [], [Name, [tokenizefuncvars(i) for i in vars]]))

        for i in imports:
            IDX, code = i
            if re.search(r"\s*#import\s+<.+>;\s", code):
                Tokens.append((IDX + offset, "IMPORT", ["GLOBAL", "C-HEAD" if ".h" in code else "SUI-HEAD"], re.findall(r"<.+>", code)[0][1:-1]))

            elif re.search(r"\s*#import\s+\".+\";\s", code):
                Tokens.append((IDX + offset, "IMPORT", ["LOCAL", "C-HEAD" if ".h" in code else "SUI-HEAD"], re.findall(r"\".+\"", code)[0][1:-1]))

            else:
                print(f"E: Invalid Import braces \"{code}\" on line {LineMap[Code[:IDX].count('\n')]}. The import must be surrounded by either \"\" or <>")
                sys.exit()

        return Tokens

    return tokenizefirstscope(Code)


def AssembleC(tokens):
    languagefolder = "C:\\suivante\\" if platform.system().lower() == "windows" else "/opt/suivante/"

    i = 0
    ParsedCode = []

    Skip = []

    while i < len(tokens): # Fixing Imports
        if i in Skip:
            i += 1
            #print(f"Skipping: {i}")
            continue

        IDX, Type, Flags, Args = tokens[i]

        if Type == "IMPORT":
            C = Flags[1]
            HType = Flags[0]
            try:
                if C == "SUI-HEAD":
                    if HType == "LOCAL":
                        Extra = Tokenize(open(Args).read())
                        tokens = [item for sublist in [Extra, tokens] for item in sublist]
                        Skip.append(i + len(Extra))
                        i = 0

                    else:
                        Extra = Tokenize(open(f"{languagefolder}{Args}").read())
                        tokens = [item for sublist in [Extra, tokens] for item in sublist]
                        Skip.append(i + len(Extra))
                        #print(f"Adding {i} to skips")
                        i = 0

                else:
                    if HType == "LOCAL":
                        ParsedCode.append((IDX, f"#include \"{Args}\"\n"))
                        Skip.append(i + len(Extra))
                    else:
                        ParsedCode.append((IDX, f"#include <{Args}>\n"))
                        Skip.append(i + len(Extra))

            except FileNotFoundError:
                print(f"E: Library file \"{Args}\" not found")
                sys.exit()

        i += 1

    def ParseScope(tokens, startIDX, scopenum=0, vars={}):
        i = startIDX
        ParsedCode = []

        while i < len(tokens):
            print(tokens[i], i)
            IDX, Type, Flags, Args = tokens[i]
            #print(Type)
            match Type:
                case "IF":
                    pass

                case "ELSE IF":
                    pass

                case "ELSE":
                    pass

                case "FOR":
                    try:
                        Type = re.findall(rf"int\s+", Flags[0])[0]
                    except IndexError:
                        print(f"E: Invalid variable type in for loop declaration on line {IDX}")
                    ForVar = re.findall(rf"int\s+.*?\s", Flags[0])[0].replace(Type, "").replace(" ", "")

                    Loop = re.findall(rf"in\s+.*?\s", Flags[0])[0].replace(" ", "")[2:]

                    LoopVar, StartVal = (Loop[Loop.find(".."):].replace("..", ""), Loop[:Loop.find("..")].replace("..", ""))

                    ParsedCode.append((IDX + startIDX, f"{scopenum * '    '}for (int {ForVar} = {StartVal}; {ForVar} < {LoopVar}; {ForVar}++) {{\n{ParseScope(Args, i, scopenum=scopenum+1, vars=vars)}}}"))

                case "WHILE":
                    pass

                case "VARREASSIGN":
                    pass

                case "VARDEC":
                    if "const" in Flags:
                        ParsedCode.append((IDX + startIDX, f"const {Args[0]} {Args[1]} = {Args[2]};"))
                    else:
                        ParsedCode.append((IDX + startIDX, f"const {Args[0]} {Args[1]} = {Args[2]};"))

                case "VARCHANGE":
                    pass

                case "RETURN":
                    pass

            i += 1

        return ParsedCode

    i = 0

    while i < len(tokens):
        IDX, Type, Flags, Args = tokens[i]
        if Type == "FUNC":
            FuncArgs = "("
            for j in Flags[2]:
                blank, ArgType, ArgName = j
                FuncArgs += f"{ArgType} {ArgName}, "

            FuncArgs = FuncArgs[:-2]
            FuncArgs += ")"
            ParsedCode.append((IDX, f"{Flags[1]} {Flags[0]} {FuncArgs} {{\n{ParseScope(Args, i, scopenum=1, vars=memvars)}}}"))

        i += 1

    return ParsedCode


if __name__ == "__main__":
    tks0 = Tokenize(open(sys.argv[1], "r").read())
    tks = AssembleC(tks0) #Tokenize(open(sys.argv[1], "r").read())

    for i in tks:
        print(i)

    print()

    for i in tks0:
        print(i)
