#!/bin/sh

dune runtest -f --instrument-with bisect_ppx
bisect-ppx-report html
bisect-ppx-report summary

if command -v explorer.exe >/dev/null 2>&1; then
    (cd _coverage && explorer.exe index.html) &
else
    (cd _coverage && xdg-open index.html) &
fi