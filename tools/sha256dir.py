#!/usr/bin/env python3

import hashlib
import os
import sys


def sha256_file(path: str, chunk_size: int = 1024 * 1024) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(chunk_size), b""):
            h.update(chunk)
    return h.hexdigest()


def main() -> None:
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} DIRECTORY", file=sys.stderr)
        sys.exit(1)

    root = sys.argv[1]

    if not os.path.isdir(root):
        print(f"Error: '{root}' is not a directory", file=sys.stderr)
        sys.exit(1)

    for dirpath, dirnames, filenames in os.walk(root):
        dirnames.sort()
        filenames.sort()

        for name in filenames:
            path = os.path.join(dirpath, name)
            digest = sha256_file(path)
            print(f"{digest}  {path}")


if __name__ == "__main__":
    main()
