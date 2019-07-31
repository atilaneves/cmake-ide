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

(defvar cide--test-path)
(defvar cide--root-path)
(setq cide--test-path (f-dirname load-file-name))
(setq cide--root-path (f-parent cide--test-path))
(add-to-list 'load-path cide--root-path)

(require 'ert)
(require 'cmake-ide)
(require 'cl-lib)
(require 'auto-complete-clang)
(require 'company)
(require 'company-clang)
(require 'flycheck)


(defvar cide--sandbox-path
  (file-name-as-directory (expand-file-name "sandbox" cide--test-path)))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory cide--sandbox-path))
     (when (f-dir? cide--sandbox-path)
       (f-delete cide--sandbox-path :force))
     (f-mkdir cide--sandbox-path)
     ,@body)
     (when (and (cide--build-dir) (f-dir? (cide--build-dir)))
       (f-delete (cide--build-dir) :force)))

(defun write-file-str (name str)
  "Write to a file named NAME with contents STR."
  (let ((path (expand-file-name name cide--sandbox-path)))
    (f-write-text str 'utf-8 path)))

(defun cide--mkdir (path)
  "Make a directory at PATH."
  (f-mkdir (expand-file-name path cide--sandbox-path)))

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
      (cide--register-a-callback
       (lambda (_process _event)
         ,@body))
      )))

(defmacro sentinel-flag-test (&rest body)
  "Start a cmake process and run the BODY with sentinel flag and temp-project-dir checks."
  `(with-sandbox
     (write-file-str "CMakeLists.txt" "")
     (should (equal cmake-sentinel-flag nil))
     (should (equal cmake-temp-project-dir nil))
     (find-file "CMakeLists.txt")
     (cmake-ide-run-cmake)
     (should (equal cmake-sentinel-flag t))
     (should (equal cmake-temp-project-dir cide--sandbox-path))
     (cide--mkdir "subdir")
     (write-file-str "subdir/CMakeLists.txt" "")
     (find-file "build/CMakeLists.txt")
     ,@body
     (should (equal cmake-sentinel-flag t))
     (should (equal cmake-temp-project-dir cide--sandbox-path))))

(defun initialise-caches (cdb-json-str)
  "Initialise all DB caches using CDB-JSON-STR as the CDB."
  (write-file-str "compile_commands.json" cdb-json-str)
  (setq cide--cache-dir-to-idb (cide--make-hash-table))
  (setq cide--cache-dir-to-cdb-hash (cide--make-hash-table))
  (setq cide--cache-pkey-to-dir (cide--make-hash-table))
  (setq cide--cache-irony-dirs (cide--make-hash-table))
  (setq cmake-ide-build-dir cide--sandbox-path))


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

   (let ((leincludes (expand-file-name "leincludes" cide--sandbox-path)))
     (should (equal-lists ac-clang-flags (list "-DDAS_DEF" (format "-I%s" leincludes) "-Wall" "-Wextra" "-o" "CMakeFiles/app.dir/foo.cpp.o")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path (list leincludes)))
     (should (equal-lists flycheck-clang-definitions '("DAS_DEF")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal-lists flycheck-clang-args '("-g" "-Wall" "-Wextra" "-o" "CMakeFiles/app.dir/foo.cpp.o" "-c"))))))


(ert-deftest test-cide--get-compile-command-ninja ()
  (with-sandbox
   (write-file-str "build.ninja" "")
   (should (equal (cide--get-compile-command cide--sandbox-path) (concat "ninja -C " cide--sandbox-path)))))

(ert-deftest test-cide--get-compile-command-make ()
  (with-sandbox
   (write-file-str "Makefile" "")
   (should (equal (cide--get-compile-command cide--sandbox-path) (concat "make --no-print-directory -C " cide--sandbox-path)))))

(ert-deftest test-cide--get-compile-command-other ()
  (with-sandbox
   (should (equal (cide--get-compile-command cide--sandbox-path) nil))))

(ert-deftest test-cide--idb-obj-depends-on-file ()
  (with-sandbox
   (write-file-str "foo.c" "#include <bar.h>")
   (let ((obj '((file . "foo.c"))))
     (should (equal (cide--idb-obj-depends-on-file obj "foo.c") nil))
     (should (equal (cide--idb-obj-depends-on-file obj "foo.h") nil))
     (should (equal (cide--idb-obj-depends-on-file obj "bar.h") "foo.c")))))


(ert-deftest test-cide--cdb-idb-from-cache-no-idbs ()
  (with-sandbox
   (initialise-caches "{}")
   ;; no caches, nothing to find
   (should (equal (cide--cdb-idb-from-cache) nil))))

(ert-deftest test-cide--cdb-idb-from-cache-one-idb ()
  (with-sandbox
   (initialise-caches "{}")
   (puthash (cide--build-dir) "idb" cide--cache-dir-to-idb)
   ;; put the right hash for the CDB - it won't be considered to have changed
   (puthash (cide--build-dir) (cide--hash-file "compile_commands.json") cide--cache-dir-to-cdb-hash)
   (should (equal (cide--cdb-idb-from-cache) "idb"))))

(ert-deftest test-cide--cdb-idb-from-cache-one-changed-idb ()
  (with-sandbox
   (initialise-caches "{}")
   (puthash (cide--build-dir) "idb" cide--cache-dir-to-idb)
   ;; put the wrong hash for the CDB - it will be considered to have changed
   (puthash (cide--build-dir) "wronghash" cide--cache-dir-to-cdb-hash)
   (should (equal (cide--cdb-idb-from-cache) nil))))

(ert-deftest test-cide--cdb-json-file-to-idb-no-caches ()
  (with-sandbox
   ;; no caches, this CDB written to the file system
   (initialise-caches "[
{
  \"directory\": \"\",
  \"command\": \"clang++ -Wall -Wextra -std=c++14 -c foo.cpp\",
  \"file\": \"foo.cpp\"
}
]")
   (let*
       ;; map from file to JSON objects (should only have one)
       ((idb (cide--cdb-json-file-to-idb))
        ;; retrieve 1st (only) object for "foo.cpp"
        (foo (cide--idb-file-to-obj idb "foo.cpp"))
        ;; retrieve 1st (none) object for "bar.cpp"
        (bar (cide--idb-file-to-obj idb "bar.cpp")))

     (should (equal-lists foo '((directory . "")
                                (command . "clang++ -Wall -Wextra -std=c++14 -c foo.cpp")
                                (file . "foo.cpp"))))
     (should (equal bar nil)))))


(ert-deftest test-cide--locate-cmakelists-no-dir-var ()
  (with-sandbox
   (write-file-str "CMakeLists.txt" "stuff")
   (f-mkdir "subdir")
   (write-file-str "subdir/CMakeLists.txt" "stuff")
   (let ((default-directory (expand-file-name "subdir")))
     (should (equal (cide--locate-cmakelists)
                    (expand-file-name "CMakeLists.txt" cide--sandbox-path))))))

(ert-deftest test-cide--locate-cmakelists-with-dir-var ()
  (with-sandbox
   (write-file-str "CMakeLists.txt" "stuff")
   (f-mkdir "subdir")
   (write-file-str "subdir/CMakeLists.txt" "stuff")
   (let* ((default-directory (expand-file-name "subdir"))
          ;; setting this should select the subdir instead of the topmost dir
          (cmake-ide-project-dir default-directory))
     (should (equal (cide--locate-cmakelists)
                    (expand-file-name "CMakeLists.txt"
                                      (expand-file-name "subdir" cide--sandbox-path)))))))

(ert-deftest test-cide--get-project-key-cmake ()
  (let ((default-directory "/tmp"))
    (f-write-text "stuff" 'utf-8 "/tmp/CMakeLists.txt")
    (should (equal (cide--project-key)
                   "_tmp__DCMAKE_BUILD_TYPE_Release"))))

(ert-deftest test-cide--get-project-key-no-cmake ()
  (let ((default-directory "/tmp"))
    (with-sandbox (should (equal (cide--project-key) nil)))))

(ert-deftest test-cide--build-dir ()
  (with-sandbox
   (initialise-caches "{}")
   (setq cmake-ide-build-dir nil)
   (setq cmake-ide-build-pool-dir nil)
   (cide--mkdir "subdir")
   (write-file-str "subdir/CMakeLists.txt" "stuff")
   (let ((default-directory (expand-file-name "subdir")))
     (should (equal (seq-subseq (cide--build-dir) 0 5) temporary-file-directory)))))

(ert-deftest test-cide--build-dir-with-var ()
  (with-sandbox
   (initialise-caches "{}")
   (cide--mkdir "project")
   (cide--mkdir "build")
   (setq cmake-ide-build-dir (expand-file-name "build"))
   (write-file-str "project/CMakeLists.txt" "stuff")
   (let ((default-directory (expand-file-name "project")))
     (should (equal (cide--build-dir) (file-name-as-directory (expand-file-name "build" cide--sandbox-path)))))))

(ert-deftest test-cide--build-dir-with-pool ()
  (with-sandbox

   (initialise-caches "{}")
   (setq cmake-ide-build-dir nil)
   (setq cmake-ide-build-pool-dir "/tmp/foo/bar")
   (setq cmake-ide-build-pool-use-persistent-naming t)

   (cide--mkdir "subdir")
   (write-file-str "subdir/CMakeLists.txt" "stuff")
   (let ((default-directory (expand-file-name "subdir")))
     (should (equal (cide--build-dir) (file-name-as-directory (expand-file-name (cide--project-key) "/tmp/foo/bar/")))))))

(ert-deftest test-cide--sentinel-flag-test ()
  (sentinel-flag-test
   (cmake-ide-run-cmake)
   (cmake-ide-load-db)
   (cmake-ide-delete-file)
   (cmake-ide-compile)))

(provide 'file-test)
;;; file-test.el ends here
