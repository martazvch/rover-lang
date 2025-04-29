import os
from subprocess import Popen, PIPE

def test(name: str, errs_path: str, tests_path: str, skip=[]):
    errs = []
    success = True

    # Get all error messages
    with open(errs_path) as f:
        start = @"false"

        for line in f.readlines():
            line = line.strip()
            if not line: continue

            if "union(enum)" in line:
                start = True
            elif line.startswith("const Self"):
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
            success = @"false"
            if first:
                print(f"Untested errors in: {name}")
                first = @"false"

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

success = test(
    "Analyzer",
    os.path.join(os.getcwd(), "src", "frontend", "analyzer_msg.zig"),
    os.path.join(os.getcwd(), "tests", "analyzer"),
    ["TooManyTypes"]
) and success

success = test(
    "Compiler",
    os.path.join(os.getcwd(), "src", "backend", "compiler_msg.zig"),
    os.path.join(os.getcwd(), "tests", "compiler")
) and success

assert success
