cmake-ide
==========

`cmake-ide` enables autocompletion and on-the-fly syntax checking
in Emacs for CMake projects with minimal configuration. It depends
on [flycheck](https://github.com/flycheck/flycheck) and
[auto-complete-clang](https://github.com/brianjcj/auto-complete-clang).
Support might be added for other packages later.

It works by running CMake in Emacs in order to obtain the necessary
`-I` and `-D` compiler flags to pass to the other tools. Since all
the dependencies are specified in the CMake scripts, there is no
need to maintain a parallel dependency tracking system for Emacs.
Just ask CMake.

Features
--------
* Automatically reruns CMake when a file is saved. Great when using
  CMake globs, but needs `cmake-ide-dir` to be set.
* Sets variables for `auto-complete-clang` and `flycheck` for a CMake
  project automagically.


Usage
-----

Add this to your `.emacs` / `init.el`:

    (cmake-ide-setup)

If `cmake-ide-clang-flags-c` or `cmake-ide-flags-c++` are set, they
will be added to `ac-clang-flags`.  These variables should be
set. Particularly, they should contain the system include paths
(e.g. `'("-I/usr/include/c++/4.9.1" "...")`. For a system with gcc,
you can get this information by running `gcc -v -xc++ /dev/null
-fsyntax-only` (it's the same prerequisite for `auto-complete-clang`
to work, since that's how clang itself works).

And... that's it. It works by calling cmake and parsing the resulting
JSON file with compiler flags.  Set `cmake-ide-dir` to your project's
root directory and you won't have to call CMake manually again (except
for the first time to specify options). Best done with
[directory local variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).


Related Projects:
----------------
* [cpputils-cmake](https://github.com/redguardtoo/cpputils-cmake).
* [My CMake modules for emacs. This project is better](https://github.com/atilaneves/cmake_modules).
