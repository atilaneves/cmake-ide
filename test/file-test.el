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

(defvar cmake-ide--sandbox-path (expand-file-name "sandbox" cmake-ide--test-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let* ((root-sandbox-path (expand-file-name "sandbox" cmake-ide--test-path))
          (default-directory root-sandbox-path))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body))

(defun write-file-str (name str)
  "Write to a file named NAME with contents STR."
  (let ((path (expand-file-name name cmake-ide--sandbox-path)))
    (f-write-text str 'utf-8 path)))


(defun equal-lists (lst1 lst2)
  "If LST1 is the same as LST2 regardless or ordering."
  (and (equal (length lst1) (length lst2))
       (null (cl-set-difference lst1 lst2 :test 'equal))))

(ert-deftest test-one-cpp-file ()
  (with-sandbox
   (write-file-str "foo.cpp" "int add(int i, int j) { return i + j; }")
   (write-file-str "CMakeLists.txt"
                   "
cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)
set(CMAKE_CXX_FLAGS \"-g -debug\")
add_executable(foo \"foo.cpp\")")
   (cmake-ide--message "Running cmake !!!!!!1oneoneoneoene")
   (cmake-ide-maybe-run-cmake)
   (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F")))
   (should (equal-lists company-clang-arguments ac-clang-flags))
   (should (equal-lists flycheck-clang-include-path '("/tmp/inc1" "/tmp/inc2")))
   (should (equal-lists flycheck-clang-definitions '("foo=bar")))
   (should (equal-lists flycheck-clang-includes nil))
   (should (equal-lists flycheck-clang-args '("-S" "-F" "-g")))))


(provide 'file-test)
;;; file-test.el ends here
