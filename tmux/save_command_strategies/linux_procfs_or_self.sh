#!/usr/bin/env bash

PANE_PID="$1"

if [ -z "$PANE_PID" ]; then
	exit 0
fi

command_from_pid() {
	local pid="$1"
	if [ -r "/proc/${pid}/cmdline" ]; then
		xargs -0 bash -c 'printf "%q " "$0" "$@"' < "/proc/${pid}/cmdline" | sed 's/[[:space:]]*$//'
	fi
}

COMMAND_PID="$(pgrep -P "$PANE_PID" | head -n 1)"

if [ -n "$COMMAND_PID" ]; then
	command_from_pid "$COMMAND_PID"
else
	command_from_pid "$PANE_PID"
fi
