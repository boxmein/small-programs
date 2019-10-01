# foreach

An utility to run a command on every line in the standard input.

Essentially the same as `xargs -n1`.

NOTE: the current state of the program works, but does not pass around the input :)

## Usage

```bash
$ do-thing | foreach "run-script"
```

