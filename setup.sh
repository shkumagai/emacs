#!/bin/bash

### local functions

function comment_out ()
{
    conf=${1}
    sed -i.orig -e "s/\(^(load-library \".*-${1}\".*$\)/;; \1/g" init.el
}

function auto_config ()
{
    name=${1}
    echo "name: ${name}"
    if [ ! -x "/usr/local/bin/${name}" -a ! -x "/opt/local/bin/${name}" ]; then
        comment_out ${1}
    fi
}

### main

## automatic configuration for init.el
auto_config cmigemo
auto_config ctags
auto_config go

## If already exists `~/.emacs.d' directory, remove it.
[[ -d "$HOME/.emacs.d" ]] && rm -rf "$HOME/.emacs.d"

## make symlink to source on user home.
ln -Fis "$PWD" "$HOME/.emacs.d"
