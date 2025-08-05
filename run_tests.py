#!/usr/bin/env python3
import os, subprocess, sys

def main():
    exe = os.path.join("_build", "default", "bin", "calyxium.exe")
    test_dir = "tests"

    if not os.path.isfile(exe) or not os.path.isdir(test_dir):
        sys.exit(f"Missing: {exe if not os.path.isfile(exe) else test_dir}")

    cx_files = [f for f in os.listdir(test_dir) if f.endswith(".cx")]
    if not cx_files:
        return print("No .cx test files found.")

    for f in cx_files:
        path = os.path.join(test_dir, f)
        try:
            r = subprocess.run([exe, path], capture_output=True, text=True)
            print(r.stdout, end="")
            if r.stderr: print(r.stderr, end="", file=sys.stderr)
            if r.returncode: raise subprocess.CalledProcessError(r.returncode, f)
            print(f"Passed: {path}")
        except Exception:
            print(f"Failed: {path}")
            sys.exit(1)

    print("\n==== All Tests Passed. ====")

if __name__ == "__main__":
    main()