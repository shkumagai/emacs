#!/bin/bash

# ## If already exists `~/.emacs.d' directory, remove it.
# [[ -d "$HOME/.emacs.d" ]] && rm -rf "$HOME/.emacs.d"

# ## make symlink to source on user home.
# ln -Fis "$PWD" "$HOME/.emacs.d"

if [ ! -z $(which hg) ]; then
    SED_TARGET="s/^;; \\((load-library \\\"shkumagai-hg\\\")\\)/\\1/g"
fi

# ret=$(which git)
# if [ ! -z $ret ]; then
#     echo $ret
#     SED_TARGET=";s/^;; \\((load-library \\\"shkumagai-git\\\")\\)/\\1/g"
# fi

# ret=$(which erl)
# if [ ! -z $ret ]; then
#     echo $ret
#     SED_TARGET=";s/^;; \\((load-library \\\"shkumagai-erl\\\")\\)/\\1/g"
# fi

# ret=$(which ctags)
# if [ ! -z $ret ]; then
#     echo $ret
#     SED_TARGET=";s/^;; \\((load-library \\\"shkumagai-ctags\\\")\\)/\\1/g"
# fi

sed -e "$SED_TARGET" init.el