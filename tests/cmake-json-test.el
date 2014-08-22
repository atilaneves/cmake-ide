;;; cmake-json-test.el --- Unit tests for cmake-json.

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


; make sure all packages are found
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'ert)
(require 'cmake-json)


(ert-deftest test-json-to-assoc ()
  (should (equal (cmake-json--to-assoc
                  '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                    ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '(("file1" . "-Ifoo -Ibar") ("file2" . "-Ibaz -Iboo -Dloo"))))
  (should (equal (cmake-json--to-assoc
                  '[((file . "file3") (command . "cmd3 -Itre -Dbre"))
                    ((file . "file4") (command . "cmd4 -Dloo -Dboo"))])
                 '(("file3" . "-Itre -Dbre") ("file4" . "-Dloo -Dboo"))))
  (should (equal (cmake-json--to-assoc
                  '[((file . "file1") (command . "/usr/bin/c++    -I/foo/bar/baz/fir    -o CMakeFiles/boo.dir/foo.cpp.o -c /foo/bar/baz/fir/foo.cpp") (directory . "/tmp/foo"))])
                 '(("file1" . "-I/foo/bar/baz/fir")))))

(ert-deftest test-json-to-flags ()
  (should (equal (cmake-json--to-flags "file1"
                                       '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                                         ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '("-Ifoo" "-Ibar")))
  (should (equal (cmake-json--to-flags "file2"
                                       '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                                         ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '("-Ibaz" "-Iboo" "-Dloo"))))

(ert-deftest test-json-to-includes ()
  (should (equal (cmake-json--flags-to-includes '("-Ifoo" "-Ibar")) '("foo" "bar")))
  (should (equal (cmake-json--flags-to-includes '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("boo" "baz" "doo"))))

(ert-deftest test-json-to-defines ()
  (should (equal (cmake-json--flags-to-defines '("-Ifoo" "-Ibar")) nil))
  (should (equal (cmake-json--flags-to-defines '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("loo"))))

(ert-deftest test-is-src-file ()
  (should (not (eq (cmake-json--is-src-file "foo.c") nil)))
  (should (not (eq (cmake-json--is-src-file "foo.cpp") nil)))
  (should (not (eq (cmake-json--is-src-file "foo.C") nil)))
  (should (not (eq (cmake-json--is-src-file "foo.cxx") nil)))
  (should (not (eq (cmake-json--is-src-file "foo.cc") nil)))
  (should (eq (cmake-json--is-src-file "foo.h") nil))
  (should (eq (cmake-json--is-src-file "foo.hpp") nil))
  (should (eq (cmake-json--is-src-file "foo.hxx") nil))
  (should (eq (cmake-json--is-src-file "foo.H") nil))
  (should (eq (cmake-json--is-src-file "foo.hh") nil))
  (should (eq (cmake-json--is-src-file "foo.d") nil))
  (should (eq (cmake-json--is-src-file "foo.py") nil)))

(provide 'cmake-json-test)
;;; cmake-json-test.el ends here
