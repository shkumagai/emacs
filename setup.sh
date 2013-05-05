#!/bin/bash

### local functions

function is_exists ()
{
    res=$(which ${1})
    if [[ "${res}" = "" ]]; then
        echo 0
    else
        echo 1
    fi
}

function comment_out ()
{
    conf=${1}
    sed -i.orig -e "s/\(^.* \".*-${1}\".*$\)/;; \1/g" init.el
}

function auto_config ()
{
    name=${1}
    path=${2}
    if [[ 0 -eq $(is_exists "${path}") ]]; then
        comment_out ${1}
    fi
}

### main

## automatic configuration for init.el
auto_config migemo "/usr/local/bin/cmigemo"
auto_config ctags "/opt/local/bin/ctags"
auto_config go "/opt/local/bin/go"

## If already exists `~/.emacs.d' directory, remove it.
[[ -d "$HOME/.emacs.d" ]] && rm -rf "$HOME/.emacs.d"

## make symlink to source on user home.
ln -Fis "$PWD" "$HOME/.emacs.d"
