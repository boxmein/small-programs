# bake

Probably the smallest build tool that still involves a dep-graph

Prior art is plentiful, and this is a particular thing I've also tried to fix
earlier:

- Zsh based https://github.com/boxmein/woot

## Syntax

Currently YAML based (easy to serialize/deserialize)

The mental model is this:

- Some input stuff goes in
- Some output stuff goes out
- Some actions are taken

```
# ./bakefile.yml
tasks:
  - inputs:
      - !File x.yml
      - !File y.yml
    outputs:
      - !File z.yml
    actions:
      - !Shell "cat x.yml y.yml > z.yml"
```

Input types:

- `!File filename` -- declare that the output depends on this input file. If the
  input file changes, the actions must be re-run to generate the output file.

Action types:

- `!Shell command` -- invoke `bash -c "command"`. Hopefully the action generates
  the output files.

Output types:

- `!File filename` -- the actions generate this output file. Any tasks that
  depend on the file, will be re-run as well.

## Installing

Get rust:

```
open https://rustup.rs
<some manual steps>
```

Get the repo:

```
git clone git@github.com:boxmein/small-programs
cd small-programs/bake
```

Compile the code:

```
cargo build --release
```

Take the files to /usr/local/bin:

```
sudo install \
    -o root \
    -g root \
    -m 755 \
    target/release/bake \
    /usr/local/bin/bake
```

## Goals

- Start small
- Maybe solve an issue for someone
- Have fun with coding
