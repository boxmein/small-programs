# ideal-build-tool

Z-Shell based domain specific language for tooling.

Helps you write shell scripts that help users run common tasks in your project,
without figuring out the tools, build systems, scripts, run commands etc
involved.

## As a project user

You don't need to install anything, simply invoking ./x --help will show you
how to do things in the project.

## As a project contributor

Use this as a front-end for your various build tools, scripts and other tooling
you have built up. Your stack remains, and there's no real DSL.

Since it's all a shell script, you can generate scripts in a loop.

### Installation

Download the buildtool script from this repo into your own repo:

```shell
wget -o ./build_dsl.inc https://raw.githubusercontent.com/boxmein/small-programs/master/ideal-build-tool/buildtool
```

Author a build script: [see examples](#examples)

```shell
cat <<EOF > tool
#!/bin/zsh
set -e
PROJECT_NAME=myproject
. ./build_dsl.inc 

start with yarn start
EOF
```

Make it executable and add it to your repo:

```shell
chmod +x ./tool
git add ./tool
git commit -m "frontend for tooling"
```

## Features

These are current features of the shell script DSL:

- It's all zsh
- `build with <command>` for invoking build
- `start with <command>` for starting developer mode
- `cleanup ./build` for removing folders as a cleanup task
- `run <task name> with <command>` for adding project-specific scripts
- `tool <command_name> <version_range>` for making sure tools exist
- `check <command_name>` for free-form checks

## Roadmap

- Specializations
  - Specialized sourceable Zsh scripts that help with certain project
    environments
  - Yarn, NPM, Node, Lerna, Yarn Workspaces, etc
  - Gradle
  - Bazel
  - Pip, Poetry, Pipenv
  - Docker


## Non-goals

- **Dependency graphs, caching, etc** -- many other tools do it better. Invoke
  those tools with this.
- **Downloading tools** -- Package managers etc vary a lot. You might have
  vendoring or multiple SDKs installed. You can use `tool TOOLNAME VERSION` to
  check for existence.

## Examples

<a name="examples" id="examples"></a>

<details><summary>Python 3</summary>

```shell
#!/bin/zsh
set -e
TOOL_NAME=./x
PROJECT_NAME=mitmproxy-scripts
. ../ideal-build-tool/buildtool # change this to the right import for your repo

check pip3 show mitmproxy
tool python3 3

start with python3 ./mitmfaultinjection.py

woot
```

</details>

<details><summary>Rust</summary>

```shell
#!/bin/zsh
set -e
TOOL_NAME=./x
PROJECT_NAME=myrustproject
. ../ideal-build-tool/buildtool # change this to the right import for your repo

tool cargo

build with cargo build

run formatter with cargo fmt 
run linter with cargo clippy 
run check with cargo check

start with ./target/debug/myrustproject

cleanup target

woot
```
</details>

<details><summary>Kotlin + Gradle</summary>

```shell
#!/bin/zsh
set -e
TOOL_NAME=./x
PROJECT_NAME=mygradleproject
. ../ideal-build-tool/buildtool # change this to the right import for your repo

build with ./gradlew :build
start with ./gradlew :run 

run gradle task list with ./gradlew :tasks 
run tests with ./gradlew :check
run create jar with ./gradlew :jar 
run create distribution zip with ./gradlew :distZip
run install with ./gradlew :installDist
run docker build with ./gradlew :docker
run upload docker image with ./gradlew :dockerPush
run javadoc with ./gradlew :javadoc
# add more gradle tasks here if needed

cleanup build .gradle

woot
```
</details>

## File structure

Since the script you write with `./x` is a regular zsh script, you have to
source the DSL commands into it.

Here's an empty build file to start with:

```
#!/bin/zsh
set -e
TOOL_NAME=./x
PROJECT_NAME=
. ../ideal-build-tool/buildtool

# commands go here...

woot
```

## Command reference

Commands from ideal-build-tool are implemented as zsh functions and aliases.

### Invocation

Runs registered commands depending on the CLI arguments.

- Parses command line arguments
- Handles -v and --help
- When no arguments are passed: 
  - Starts fuzzy finder `fzf` with list of runnable commands. 
  - When a command is selected, runs it.
- When 1 argument is passed, parses it as a command name and runs it.
  - When `start` is the first argument, runs the command registered as the start
    script.
  - When `build` is the first argument, runs the command registered as the build
    script.
  - Lookups the script registered with that name and runs it.
- When more than 1 arguments are passed:
  - Looks up the script registered with that name and runs it.

```
woot
```

### Start script

Registers a command to run when `./x start` is run.

Shows the "./x start" command in the help text.

Syntax:

```
start with <command>
```

Example:

```
start with python3 ./mycommand.py
```

### Build script 

Registers a command to run when `./x build` is run.

Shows the "./x build" command in the help text.

Syntax:

```
build with <command>
```

Example:

```
build with ./gradlew :build 
```

### Cleanup folders

Registers folders to delete when `./x cleanup` is run.

Syntax:

```
cleanup <folder> <folder...>
```

Example:

```
cleanup target/ ./dist/ ./__cache__/
```

### Custom scripts 

Registeres a custom script.

Custom scripts can be invoked:

- On the command line: `./x <script name>`
- With the fuzzy finder: `./x`, select `<script name>`, press the Enter key.

Custom scripts appear in the help as:

```
additional scripts:
./x <script name>
```

Syntax:

```
run <script name> with <command>
```

Example:

```
run tests with cargo test

run dependency updates with yarn upgrade-interactive
```

### Tool version checks

Checks that `<commandname>` is available in the `PATH`. If not, shows an error
message and exits the script.

If `<version_check>` is provided, runs `<commandname> --version` and matches the
output to the `version_check` string. If the output of `<commandname> --version`
does not contain the `version_check` string, shows an error message and exits
the script.

Syntax:

```
tool <commandname> <version_check>
```

Example:

```
tool yarn 1.22
```

### Generic checks

Runs a command to validate that something is OK before continuing with the 
tooling script.

This is useful to check that the user's environment is set up correctly, or
that the project is in a clean state.

If the command returns a nonzero status code, shows an error message and exits.

If the command returns a zero status code, silently continues.

Syntax:

```
check with <command>
```

Example:

```
# Check that docker is up.
check with docker ps 
```