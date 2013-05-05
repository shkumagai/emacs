==========================
 shkumagai's Emacs Config
==========================

This is Shoji KUMAGAI's (shkumagai) Emacs Configuration.


setup
=====

1. git clone to local ::

     $ cd <somewhere>
     $ git clone git@github.com:shkumagai/emacs

2. move to 'emacs' directory, then run setup.sh ::

     $ cd emacs
     $ bash setup.sh


.. note::
   ``setup.sh`` will remove current ``~/.emacs.d`` director, if exists.
   So, if you won't to do it, you should move it to other place.

3. Run emacs, and then you exec command as below ::

     M-x package-list-packcages

   All packages that needed marked ``I`` for install.
   Then, hit ``X`` key to execute installation.

   Required package names are listed in elpa/.packages file.


byte-compile elisp
==================

To be faster to load all elisp files, you can do byte-compiling them.
It's easy ::

  C-0 M-x byte-recompile-directory
  Directory: /path/to/.emacs/elisp

.. END
