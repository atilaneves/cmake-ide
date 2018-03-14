;;; file-test.el --- Unit tests for cmake-ide.

;; Copyright (C) 2014-2018

;; Author:  <atila.neves@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'f)

(defvar cmake-ide--test-path)
(defvar cmake-ide--root-path)
(setq cmake-ide--test-path (f-dirname load-file-name))
(setq cmake-ide--root-path (f-parent cmake-ide--test-path))
(add-to-list 'load-path cmake-ide--root-path)

(require 'ert)
(require 'cmake-ide)
(require 'cl-lib)
(require 'auto-complete-clang)
(require 'company)
(require 'company-clang)
(require 'flycheck)


(defvar cmake-ide--sandbox-path (expand-file-name "sandbox" cmake-ide--test-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory cmake-ide--sandbox-path))
     (when (f-dir? cmake-ide--sandbox-path)
       (f-delete cmake-ide--sandbox-path :force))
     (f-mkdir cmake-ide--sandbox-path)
     ,@body))

(defun write-file-str (name str)
  "Write to a file named NAME with contents STR."
  (let ((path (expand-file-name name cmake-ide--sandbox-path)))
    (f-write-text str 'utf-8 path)))


(defun equal-lists (lst1 lst2)
  "If LST1 is the same as LST2 regardless or ordering."
  (and (equal (length lst1) (length lst2))
       (null (cl-set-difference lst1 lst2 :test 'equal))))


(defmacro with-cpp-file (cmakelists-txt file-name file-contents &rest body)
  "Write CMakeLists.txt with CMAKELISTS-TXT then FILE-NAME out to the sandbox with FILE-CONTENTS then evaluate BODY."
  `(let ((file-name ,file-name))
     (with-sandbox
      (write-file-str file-name ,file-contents)
      (write-file-str "CMakeLists.txt" ,cmakelists-txt)
      (find-file file-name)
      (flycheck-mode)
      (cmake-ide-maybe-run-cmake)
      (cmake-ide--register-a-callback
       (lambda (_process _event)
         ,@body))

                                        ;,@body
      )))


(ert-deftest test-one-cpp-file ()
  (with-cpp-file
   "
cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS_DEBUG \"-g -Wall -Wextra\")
include_directories(\"leincludes\")
add_definitions(\"-DDAS_DEF\")
add_executable(app \"foo.cpp\")"
   "foo.cpp"
   "int add(int i, int j) { return i + j; }"

   (let ((leincludes (expand-file-name "leincludes" cmake-ide--sandbox-path)))
     (should (equal-lists ac-clang-flags (list "-DDAS_DEF" (format "-I%s" leincludes) "-Wall" "-Wextra" "-o" "CMakeFiles/app.dir/foo.cpp.o")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path (list leincludes)))
     (should (equal-lists flycheck-clang-definitions '("DAS_DEF")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal-lists flycheck-clang-args '("-g" "-Wall" "-Wextra" "-o" "CMakeFiles/app.dir/foo.cpp.o" "-c"))))))


(ert-deftest test-cmake-ide--get-compile-command-ninja ()
  (with-sandbox
   (write-file-str "build.ninja" "")
   (should (equal (cmake-ide--get-compile-command cmake-ide--sandbox-path) (concat "ninja -C " cmake-ide--sandbox-path)))))

(ert-deftest test-cmake-ide--get-compile-command-make ()
  (with-sandbox
   (write-file-str "Makefile" "")
   (should (equal (cmake-ide--get-compile-command cmake-ide--sandbox-path) (concat "make --no-print-directory -C " cmake-ide--sandbox-path)))))

(ert-deftest test-cmake-ide--get-compile-command-other ()
  (with-sandbox
   (should (equal (cmake-ide--get-compile-command cmake-ide--sandbox-path) nil))))

(ert-deftest test-cmake-ide--idb-obj-depends-on-file ()
  (with-sandbox
   (write-file-str "foo.c" "#include <bar.h>")
   (let ((obj '((file . "foo.c"))))
     (should (equal (cmake-ide--idb-obj-depends-on-file obj "foo.c") nil))
     (should (equal (cmake-ide--idb-obj-depends-on-file obj "foo.h") nil))
     (should (equal (cmake-ide--idb-obj-depends-on-file obj "bar.h") "foo.c")))))

(ert-deftest test-cmake-ide--hash-file ()
  (with-sandbox
   (write-file-str "foo.txt" "abcdefghi")
   (should (equal (cmake-ide--hash-file "foo.txt") "8aa99b1f439ff71293e95357bac6fd94"))))


(defun initialise-caches (cdb-json)
  "Initialise all DB caches using CDB-JSON as the CDB."
  (write-file-str "compile_commands.json" "foobarbaz")
  (setq cmake-ide--idbs (cmake-ide--make-hash-table))
  (setq cmake-ide--cdb-hash (cmake-ide--make-hash-table))
  (setq cmake-ide-build-dir cmake-ide--sandbox-path))


(ert-deftest test-cmake-ide--cdb-idb-from-cache-no-idbs ()
  (with-sandbox
   (initialise-caches "{}")
   ;; no caches, nothing to find
   (should (equal (cmake-ide--cdb-idb-from-cache) nil))))

(ert-deftest test-cmake-ide--cdb-idb-from-cache-one-idb ()
  (with-sandbox
   (initialise-caches "{}")
   (puthash (cmake-ide--get-build-dir) "idb" cmake-ide--idbs)
   ;; put the right hash for the CDB - it won't be considered to have changed
   (puthash (cmake-ide--get-build-dir) (cmake-ide--hash-file "compile_commands.json") cmake-ide--cdb-hash)
   (should (equal (cmake-ide--cdb-idb-from-cache) "idb"))))

(ert-deftest test-cmake-ide--cdb-idb-from-cache-one-changed-idb ()
  (with-sandbox
   (initialise-caches "{}")
   (puthash (cmake-ide--get-build-dir) "idb" cmake-ide--idbs)
   ;; put the wrong hash for the CDB - it will be considered to have changed
   (puthash (cmake-ide--get-build-dir) "wronghash" cmake-ide--cdb-hash)
   (should (equal (cmake-ide--cdb-idb-from-cache) nil))))


(provide 'file-test)
;;; file-test.el ends here
