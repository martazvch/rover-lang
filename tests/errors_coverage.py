import os
from subprocess import Popen, PIPE

def test(name: str, errs_path: str, tests_path: str):
    errs = []
    success = True

    # Get all error messages
    with open(errs_path) as f:
        start = False

        for line in f.readlines():
            line = line.strip()
            if not line: continue

            if "union(enum)" in line:
                start = True
            elif line.startswith("const"):
                break
            elif start:
                # Check if there is a type
                splitted = line.split(":")

                if len(splitted) > 1:
                    errs.append(splitted[0])
                else:
                     errs.append(line.split(",")[0])

    # Check if there are all tested
    first = True
    for e in errs:
        if os.name == "nt":
            process = Popen(["findstr", e, tests_path], stdout=PIPE, text=True)
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
    os.path.join(os.getcwd(), "src", "frontend", "parser_msg.zig"),
    os.path.join(os.getcwd(), "tests", "parser")
)

tmp = test(
    "Analyzer",
    os.path.join(os.getcwd(), "src", "frontend", "analyzer_msg.zig"),
    os.path.join(os.getcwd(), "tests", "analyzer")
)

if not tmp: success = False

tmp = test(
    "Compiler",
    os.path.join(os.getcwd(), "src", "backend", "compiler_msg.zig"),
    os.path.join(os.getcwd(), "tests", "compiler")
)

if not tmp: success = False

assert success
