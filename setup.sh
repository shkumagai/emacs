#!/bin/bash

[[ -d "$HOME/.emacs.d" ]] && rm -rf "$HOME/.emacs.d"

ln -Fis "$PWD" "$HOME/.emacs.d"
