;;; cmake-ide-test.el --- Unit tests for cmake-ide.

;; Copyright (C) 2014

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

(setq cmake-ide--test-path (f-dirname load-file-name))
(setq cmake-ide--root-path (f-parent cmake-ide--test-path))
(add-to-list 'load-path cmake-ide--root-path)

(require 'ert)
(require 'cmake-ide)
(require 'cl)
(require 'auto-complete-clang)
(require 'company)
(require 'flycheck)

(ert-deftest test-json-to-file-params ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"/foo/bar/dir/foo.cpp\"}]")
         (idb (cmake-ide--cdb-json-string-to-idb json-str))
         (real-params (cmake-ide--idb-file-to-obj idb "/foo/bar/dir/foo.cpp"))
         (fake-params (cmake-ide--idb-file-to-obj idb "oops")))
    (should (equal (cmake-ide--idb-key-to-value 'directory real-params) "/foo/bar/dir"))
    (should (equal (cmake-ide--idb-key-to-value 'directory fake-params) nil))))


(ert-deftest test-params-to-src-flags-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -std=c++14 --foo --bar\"},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file1")))
    (should (equal (cmake-ide--params-to-src-flags file-params)
                   '("-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar")))))

(ert-deftest test-params-to-src-flags-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\",
                  \"command\": \"cmd1 -o file1.c.o -Ifoo -Ibar -std=c++14\"},
                 {\"file\": \"file2.c\",
                  \"command\": \"cmd2 -o file2.c.o foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file2.c")))
    (should (equal (cmake-ide--params-to-src-flags file-params)
                   '("-o" "file2.c.o" "foo" "bar" "-g" "-pg" "-Ibaz" "-Iboo" "-Dloo")))))


(ert-deftest test-flags-to-include-paths ()
  (should (equal (cmake-ide--flags-to-include-paths '("-Ifoo" "-Ibar")) '("foo" "bar")))
  (should (equal (cmake-ide--flags-to-include-paths '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("boo" "baz" "doo"))))


(ert-deftest test-flags-to-defines ()
  (should (equal (cmake-ide--flags-to-defines '("-Ifoo" "-Ibar")) nil))
  (should (equal (cmake-ide--flags-to-defines '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("loo"))))

(ert-deftest test-flags-minus-includes-defs ()
  (should (equal (cmake-ide--flags-filtered '("-Iinc" "-Ddef" "-F/dir")) '("-F/dir")))
  (should (equal (cmake-ide--flags-filtered '("-Iinc" "-Ddef")) nil))
  )


(ert-deftest test-is-src-file ()
  (should (not (eq (cmake-ide--is-src-file "foo.c") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.cpp") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.C") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.cxx") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.cc") nil)))
  (should (eq (cmake-ide--is-src-file "foo.h") nil))
  (should (eq (cmake-ide--is-src-file "foo.hpp") nil))
  (should (eq (cmake-ide--is-src-file "foo.hxx") nil))
  (should (eq (cmake-ide--is-src-file "foo.H") nil))
  (should (eq (cmake-ide--is-src-file "foo.hh") nil))
  (should (eq (cmake-ide--is-src-file "foo.d") nil))
  (should (eq (cmake-ide--is-src-file "foo.py") nil)))


(ert-deftest test-commands-to-hdr-flags-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.h\",
                  \"command\": \"cmd1 -Ifoo -Ibar\"}]"))
         (commands (mapcar (lambda (x) (cmake-ide--idb-key-to-value 'command x)) idb)))

    (should (equal (cmake-ide--commands-to-hdr-flags commands)
                   '("-Ifoo" "-Ibar")))))

(ert-deftest test-commands-to-hdr-flags-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.h\",
                  \"command\": \"cmd1 -Ifoo -Ibar\"},
                 {\"file\": \"/dir2/file2.h\",
                  \"command\": \"cmd2 -Iloo -Dboo\"}]"))
         (commands (mapcar (lambda (x) (cmake-ide--idb-key-to-value 'command x)) idb)))

    (should (equal (cmake-ide--commands-to-hdr-flags commands)
                   '("-Ifoo" "-Ibar" "-Iloo" "-Dboo")))))

(ert-deftest test-commands-to-hdr-flags-3 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.c\",
                  \"command\": \"cmd1 -o file1.c.o otherfile -Ifoo -Ibar -weird\"},
                 {\"file\": \"/dir2/file2.c\",
                  \"command\": \"cmd2 -o file2.c.o -Iloo -Dboo -include foo.h\"},
                 {\"file\": \"/dir2/file2.c\",
                  \"command\": \"cmd2 -o file3.c.o -Iloo -Dboo -include bar.h\"}]"))
         (commands (mapcar (lambda (x) (cmake-ide--idb-key-to-value 'command x)) idb)))
    (should (equal (cmake-ide--commands-to-hdr-flags commands)
                   '( "-Ifoo" "-Ibar" "-Iloo" "-Dboo" "otherfile" "-weird" "-include" "foo.h" "-include" "bar.h")))))


(defun equal-lists (lst1 lst2)
  "If LST1 is the same as LST2 regardless or ordering."
  (and (equal (length lst1) (length lst2))
       (null (set-difference lst1 lst2 :test 'equal))))


(ert-deftest test-params-to-src-includes-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
               {\"file\": \"file2\",
                \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file1")))

    (should (equal-lists
             (cmake-ide--params-to-src-includes file-params)
             '("/foo/bar.h" "a.h")))))

(ert-deftest test-params-to-src-includes-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file2")))
    (should (equal-lists
             (cmake-ide--params-to-src-includes file-params)
             '("h.h")))))

(ert-deftest test-commands-to-hdr-includes-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (commands (mapcar (lambda (x) (cmake-ide--idb-key-to-value 'command x)) idb)))
    (should (equal-lists (cmake-ide--commands-to-hdr-includes commands)
                         '("/foo/bar.h" "a.h" "h.h")))))

(ert-deftest test-commands-to-hdr-includes-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (commands (mapcar (lambda (x) (cmake-ide--idb-key-to-value 'command x)) idb)))
    (should (equal-lists (cmake-ide--commands-to-hdr-includes commands)
                         '("/foo/bar.h" "a.h" "h.h")))))

(ert-deftest test-all-vars ()
  (let ((idb (cmake-ide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"cmd1 -Iinc1 -Iinc2 -Dfoo=bar -S -F -g\"}]")))
    (cmake-ide--set-flags-for-file idb (current-buffer))
    (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F" "-g")))
    (should (equal-lists company-clang-arguments ac-clang-flags))
    (should (equal-lists flycheck-clang-include-path '("inc1" "inc2")))
    (should (equal-lists flycheck-clang-definitions '("foo=bar")))
    (should (equal-lists flycheck-clang-includes nil))
    (should (equal-lists flycheck-clang-args '("-S" "-F" "-g")))))

(ert-deftest test-all-vars-ccache ()
  (let ((idb (cmake-ide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"/usr/bin/ccache clang++ -Iinc1 -Iinc2 -Dfoo=bar -S -F -g -std=c++14\"}]")))
    (cmake-ide--set-flags-for-file idb (current-buffer))
    (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F" "-g" "-std=c++14")))
    (should (equal-lists company-clang-arguments ac-clang-flags))
    (should (equal-lists flycheck-clang-include-path '("inc1" "inc2")))
    (should (equal-lists flycheck-clang-definitions '("foo=bar")))
    (should (equal-lists flycheck-clang-includes nil))
    (should (equal flycheck-clang-language-standard "c++14"))
    (should (equal-lists flycheck-clang-args '("-S" "-F" "-g")))))


(provide 'cmake-ide-test)
;;; cmake-ide-test.el ends here
