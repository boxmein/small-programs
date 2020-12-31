# permuter

A collection of command line tools that work well as part of a shell pipeline
and help with CTF word-list generation.

## Project goals

- Easy to pipeline (cat wordlist.txt | permute-capitalization | tool3)
- Streaming, not slurping (doesn't eat RAM proportionally to size of file)
- All tool names prefixed with permute-
- Test coverage

## Things part of this project

**permute-capitalization**: converts between different capitalizations of a string

## Future features

**permute-numbers**: adds numbers to start or end, configure with CLI flags

## Co
