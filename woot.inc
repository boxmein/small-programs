#!/bin/zsh
# Purpose: DSL for project tooling
# Usage: source this file, and start writing commands
set -e

#
# Configuration
#
# - TOOL_NAME: name of the build tool
# - PROJECT_NAME: name of the project


#
#
# State
#
#

declare -a _bt_cleanup_folders
declare -a _bt_setup_checks
declare -A _bt_task_map
_bt_build_command=""
_bt_start_command=""

#
#
# Splitter
#
#
_bt_split_on_with() {
  local -a pre_with=()
  local -a post_with=()
  local with_passed=0
  while [[ $# -gt 0 ]]; do
    case $1 in
      with)
        with_passed=1
        ;;
      *)
        if [[ "$with_passed" -eq 1 ]]; then 
          post_with+=($1)
        else
          pre_with+=($1)
        fi
    esac
    shift
  done

  echo "pre_with='$pre_with' post_with='$post_with'"
}

#
#
# Tool checkers
#
#

_bt_check_tool_in_path() {
  tool_name="$1"
  tool_expected_version="$2"

  if ! command -v $tool_name >/dev/null 2>/dev/null; then 
    echo "Tool not found: $tool_name"
    exit 1
  fi

  tool_version=$(${tool_name} --version)
  if [[ "$tool_version" != *${tool_expected_version}* ]]; then 
    echo "Tool is the wrong version: $tool_name $tool_version"
    echo "Expected $tool_expected_version"
  fi
}

#
#
# State setters
#
#

_bt_set_build_command() {
  eval $(_bt_split_on_with $*)
  _bt_build_command="$post_with"
}

_bt_set_start_command() {
  eval $(_bt_split_on_with $*)
  _bt_start_command="$post_with"
}

_bt_create_task_command() {
  eval $(_bt_split_on_with $*)
  _bt_task_map[$pre_with]="$post_with"
}

_bt_create_setup_check() {
  _bt_setup_checks+=("$*")
}

_bt_declare_cleanup_folders() {
  _bt_cleanup_folders=($*)
}

#
#
# Runners
#
#

_bt_invoke_task() {
  sh -c "${_bt_task_map[$*]}"
}

_bt_invoke_build_command() {
  sh -c "${_bt_build_command}"
}

_bt_invoke_start_command() {
  sh -c "${_bt_start_command}"
}

_bt_invoke_cleanup() {
  for folder in $_bt_cleanup_folders; do
    rm -rf $folder;
  done
}

_bt_invoke_setup_checks() {
  for check in $_bt_setup_checks; do 
    sh -c "${check}" || echo "Error: $check failed." && exit 1
  done
}

#
#
# CLI
#
#

_bt_usage() {
  local toolName=${TOOL_NAME:-buildtool}
  local projectName=${PROJECT_NAME:-this project}
  cat <<EOF
$toolName - tooling for $projectName

usage: 

  $toolName [args]

  where:

            -v:   enable verbose mode for the build tool
    -h, --help:   get this help text
EOF

  if [[ -n "$_bt_build_command" ]]; then 
    echo "         build:   build $projectName"
  fi
  if [[ -n "$_bt_start_command" ]]; then 
    echo "         start:   start the dev mode for $projectName"
  fi
  if [[ -n "${_bt_clean_folders}" ]]; then
    echo  "         clean:   remove output folders"
  fi
  if [[ -n "${(k)_bt_task_map}" ]]; then 
    echo "          run:   start a script for $projectName"
    echo
    echo "available scripts: "
    for task in ${(k)_bt_task_map}; do
      echo "$toolName $task"
    done
  fi
}

_bt_fuzzy_ui() {
  task_to_run=$(echo ${(j:.:)${(k)_bt_task_map}} | tr '.' '\n' | fzf)
  echo "Running ./x $task_to_run"
  _bt_invoke_task "$task_to_run"
}

_bt_fallback() {
  if [[ -n "${_bt_task_map[$*]}" ]]; then 
    _bt_invoke_task "$*"
  else
    _bt_fuzzy_ui "$*"
  fi
}


_bt_parse_cli_and_invoke() {

  _bt_invoke_setup_checks

  local didSomething=0
  while [[ $# -gt 0 ]]; do
    case $1 in
      -v)
        set -x
        ;;
      -h|--help)
        _bt_usage
        didSomething=1
        ;;
      build)
        _bt_invoke_build_command
        didSomething=1
        ;;
      start)
        _bt_invoke_start_command
        didSomething=1
        ;;
      run)
        shift
        _bt_invoke_task "$*"
        didSomething=1
        ;;
      clean|cleanup)
        _bt_invoke_cleanup
        didSomething=1
        ;;
      *)
        _bt_fallback "$*"
        didSomething=1
        ;;
    esac
    shift
  done

  if [[ "$didSomething" -ne 1 ]]; then 
    _bt_fallback "$*"
  fi
}

alias build="_bt_set_build_command"
alias run="_bt_create_task_command"
alias cleanup="_bt_declare_cleanup_folders"
alias woot='_bt_parse_cli_and_invoke $*'
alias tool='_bt_check_tool_in_path'
alias start='_bt_set_start_command'
alias check='_bt_create_setup_check'