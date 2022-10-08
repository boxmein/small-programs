# bake

Probably the smallest build tool that still involves a dep-graph

Prior art is plentiful, and this is a particular thing I've also tried to fix
earlier:

- Zsh based https://github.com/boxmein/woot

## Usage

- [Install it](#installing)

## Examples

TypeScript set-up:

```yaml
tasks:
  - inputs:
      - !File package.json
    outputs:
      - !File yarn.lock
      - !File .yarn/install-state.gz
      - !Glob node_modules/**
    actions:
      - !Shell yarn install
  - inputs:
      - !Glob node_modules/**
      - !Glob src/**/*.ts
    outputs:
      - !Glob dist/**/*.js
    actions:
      - !Shell yarn run tsc -p .
```

Go monorepo set-up:

```yaml
tasks:
  - inputs:
      - !Glob pkg/**/*.go
      - !Glob service/X/**/*.go
      - !File cmd/X/main.go
    outputs:
      - !File bin/X
    actions:
      - !Shell go build -o bin/X ./cmd/X
  - inputs:
      - !Glob pkg/**/*.go
      - !Glob service/Y/**/*.go
      - !File cmd/Y/main.go
    outputs:
      - !File bin/Y
    actions:
      - !Shell go build -o bin/Y ./cmd/Y
```

## Syntax

Currently YAML based (easy to serialize/deserialize)

The mental model is this:

- Some input stuff goes in
- Some output stuff goes out
- Some actions are taken

```yaml
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

### Inputs

Inputs specify what can cause the output of the task to change. If any input
changes, the task is run again next time.

- `!File filename` -- when the file changes, re-run the actions
- `!Glob pattern` -- when any file indicated by the glob changes, re-run the actions

### Actions

Actions specify what to do to generate output from input

- `!Shell command` -- invoke `bash -c "command"`

### Outputs

Specifies what files the actions affect. This is used to correctly determine the
dependency graph, and to skip task execution if outputs are newer than inputs

- `!File filename` -- this task modifies this file
- `!Glob pattern` -- this task modifies these files

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
