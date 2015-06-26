#!/bin/sh

. ~/.nix-profile/etc/profile.d/nix.sh;

nix-shell --pure --command "$@"
