# checc

A tool that runs a bunch of programs and shows a :white_check_mark: if the tools succeed
and :x: if the tools fail.

## Use cases

- Synthetic monitoring
- Automatic vulnerability regression tests

## Usage

1. Build the app: `go build -o checc cmd/checc.go`
2. Copy the binary to a folder
3. Create `checc.lst` in the same folder, with one command per line
4. `watch --color -n120 ./checc`

```
go run ./cmd/checc.go
```


