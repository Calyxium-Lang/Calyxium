#!/usr/bin/env python3
import os
import subprocess
import sys

def is_cx_file(f):
    return f.endswith(".cx")

def get_test_files(test_dir):
    return [os.path.join(test_dir, f) for f in os.listdir(test_dir) if is_cx_file(f)]

def run_test(exe, file):
    try:
        result = subprocess.run(
            [exe, file],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, end="", file=sys.stderr)

        if result.returncode == 0:
            print(f"Passed: {file}")
        else:
            print(f"Failed: {file}")
            sys.exit(1)

    except FileNotFoundError:
        clear_running_test_line()
        print(f"Interpreter not found: {exe}")
        sys.exit(1)

def main():
    exe = os.path.join("_build", "default", "bin", "calyxium.exe")
    test_dir = "tests"

    if not os.path.isfile(exe):
        print(f"Interpreter not found: {exe}")
        sys.exit(1)

    if not os.path.isdir(test_dir):
        print(f"Test directory not found: {test_dir}")
        sys.exit(1)

    test_files = get_test_files(test_dir)
    if not test_files:
        print("No .cx test files found.")
        return

    for file in test_files:
        run_test(exe, file)

    print("\n==== All Tests Passed. ====")

if __name__ == "__main__":
    main()
