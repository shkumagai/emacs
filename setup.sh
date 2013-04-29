#!/bin/bash

## If already exists `~/.emacs.d' directory, remove it.
[[ -d "$HOME/.emacs.d" ]] && rm -rf "$HOME/.emacs.d"

## make symlink to source on user home.
ln -Fis "$PWD" "$HOME/.emacs.d"
