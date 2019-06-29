#!/usr/bin/env python
"Extracts JSON from glTF binary file"

import io
import sys
import struct
from signal import signal, SIGPIPE, SIG_DFL

def main():
    "Process glTF data from stdin and writes JSON to stdout"
    signal(SIGPIPE, SIG_DFL)
    stdin = sys.stdin.buffer
    if stdin.read(4) != b'glTF':
        print("Not a glTF file!", file=sys.stderr)
        sys.exit(-1)
    stdin.read(8)
    [length, _] = struct.unpack("II", stdin.read(8))
    sys.stdout.buffer.write(stdin.read(length))

if __name__ == "__main__":
    main()
