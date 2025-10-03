import os
from subprocess import Popen, PIPE

def test(name: str, errs_path: str, tests_path: str, skip=[]):
    errs = []
    success = True
    indent = 0
    first = True

    # Get all error messages
    with open(errs_path) as f:
        start = False

        for line in f.readlines():
            # We get the indentation level
            if first and start:
                for c in line:
                    if c == " ":
                        indent += 1
                    else:
                        break

                first = False

            if "union(enum)" in line:
                start = True
            elif "const Self" in line:
                break
            elif start:
                # Could be a inline struct definition which in one indent level higher
                # Check if at least one more space from margin
                if line.startswith(" " * (indent + 1)):
                    continue

                line = line.strip()

                # Empty line
                if not line: continue

                # Check if there is a type
                splitted = line.split(":")

                if len(splitted) > 1:
                    errs.append(splitted[0])
                else:
                    errs.append(line.split(",")[0])

    # Check if there are all tested
    first = True
    for e in errs:
        if e in skip: continue

        # Windows
        if os.name == "nt":
            process = Popen(["findstr", "/s", e, f"{tests_path}\\*"], stdout=PIPE, text=True)
        else:
            process = Popen(["grep", "-r", e, tests_path], stdout=PIPE, text=True)

        (stdout, _) = process.communicate()

        try:
            assert stdout
        except AssertionError:
            success = False
            if first:
                print(f"Untested errors in: {name}")
                first = False

            print(f"  - {e}")

    return success

# -----------
#  Run tests
# -----------

success = test(
    "Parser",
    os.path.join(os.getcwd(), "src", "core", "parser", "parser_msg.zig"),
    os.path.join(os.getcwd(), "tests", "parser")
)

success = test(
    "Analyzer",
    os.path.join(os.getcwd(), "src", "core", "analyzer", "analyzer_msg.zig"),
    os.path.join(os.getcwd(), "tests", "analyzer"),
    ["too_many_types"]
) and success

success = test(
    "Compiler",
    os.path.join(os.getcwd(), "src", "core", "compiler", "compiler_msg.zig"),
    os.path.join(os.getcwd(), "tests", "compiler")
) and success

assert success
