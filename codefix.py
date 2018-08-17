#!/usr/bin/env python2
# This tool can be used to correct the codepage/line width mismatch seen in the scanned code.
# By SopaXorzTaker

from sys import argv

if not len(argv) == 3:
    print "Usage: %s <source-file> <destination-file>" % argv[0]
    exit(1)

with open(argv[1], "r") as infile:
    lines = infile.readlines()

    # We have to convert UTF8 to Windows-1252 first.
    # Then we re-interpret that as CP437.
    # The trailing spaces and line breaks are removed.

    lines = map(lambda x: x.decode("utf-8").encode("windows-1252").decode("cp437").rstrip(), lines)

    # Now we look for wrapped lines and unwrap them
    i = 0
    new_lines = []

    # Fix 80-column text printed in 72
    while i < len(lines):
        line = lines[i]

        # Remove tabs
        sline = line.replace("   ", "")

        # Remove the page numbers (effectively skipping page breaks)
        if line.lstrip().startswith("A-"):
            i += 2
            pass

        # If the line is too long, it has been wrapped.
        elif len(sline) > 73 and i + 1 < len(lines):
            line += lines[i+1]
            new_lines.append(line)
            i += 2

        # If the line started with a box-drawing character, it should be prepended back.
        elif line and ord(line[0]) > 127 and len(new_lines) > 0:
            sep = " " * (79 - len(new_lines[-1]) - len(line))

            new_lines[-1] += sep + line
            i += 1

        # Just append the next line
        else:
            new_lines.append(line)
            i += 1

    # Write the output
    with open(argv[2], "w") as outfile:
        new_text = "\n".join(new_lines).encode("utf-8")

        outfile.write(new_text)
