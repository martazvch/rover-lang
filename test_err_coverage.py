import os
from subprocess import Popen, PIPE

def test(errs_path: str, tests_path: str):
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
    for e in errs:
        process = Popen(["rg", e, tests_path], stdout=PIPE, text=True)

        (stdout, _) = process.communicate()

        try:
            assert stdout
        except AssertionError:
            success = False
            print(f"Not tested error: {e}")

    return success

# -----------
#  Run tests
# -----------

success = test(
    os.path.join(os.getcwd(), "src", "frontend", "parser_msg.zig"),
    os.path.join(os.getcwd(), "tests", "parser")
)

success = test(
    os.path.join(os.getcwd(), "src", "frontend", "parser_msg.zig"),
    os.path.join(os.getcwd(), "tests", "parser")
)

success = test(
    os.path.join(os.getcwd(), "src", "frontend", "parser_msg.zig"),
    os.path.join(os.getcwd(), "tests", "parser")
)

