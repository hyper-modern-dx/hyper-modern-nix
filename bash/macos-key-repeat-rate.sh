#!/usr/bin/env bash

# configure maximum keyboard repeat rate
if ! defaults write -g InitialKeyRepeat -int 10 ||
	! defaults write -g KeyRepeat -int 1; then
	echo "Error: Failed to set keyboard preferences"
	exit 1
fi

echo "Settings applied. Log out and back in for changes to take effect."
