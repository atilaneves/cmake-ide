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


(ert-deftest test-json-to-src-assoc ()
  (should (equal (cmake-ide--json-to-src-assoc
                  '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                    ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '(("file1" . "-Ifoo -Ibar") ("file2" . "-Ibaz -Iboo -Dloo"))))
  (should (equal (cmake-ide--json-to-src-assoc
                  '[((file . "file3") (command . "cmd3 -Itre -Dbre"))
                    ((file . "file4") (command . "cmd4 -Dloo -Dboo"))])
                 '(("file3" . "-Itre -Dbre") ("file4" . "-Dloo -Dboo"))))
  (should (equal (cmake-ide--json-to-src-assoc
                  '[((file . "file1") (command . "/usr/bin/c++    -I/foo/bar/baz/fir    -o CMakeFiles/boo.dir/foo.cpp.o -c /foo/bar/baz/fir/foo.cpp") (directory . "/tmp/foo"))])
                 '(("file1" . "-I/foo/bar/baz/fir")))))

(ert-deftest test-json-to-src-flags ()
  (should (equal (cmake-ide--json-to-src-flags "file1"
                                       '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                                         ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '("-Ifoo" "-Ibar")))
  (should (equal (cmake-ide--json-to-src-flags "file2"
                                       '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                                         ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '("-Ibaz" "-Iboo" "-Dloo"))))

(ert-deftest test-flags-to-includes ()
  (should (equal (cmake-ide--flags-to-includes '("-Ifoo" "-Ibar")) '("foo" "bar")))
  (should (equal (cmake-ide--flags-to-includes '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("boo" "baz" "doo"))))

(ert-deftest test-flags-to-defines ()
  (should (equal (cmake-ide--flags-to-defines '("-Ifoo" "-Ibar")) nil))
  (should (equal (cmake-ide--flags-to-defines '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("loo"))))

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

(ert-deftest test-json-to-src-hdr-assoc ()
    (should (equal (cmake-ide--json-to-hdr-assoc
                  '[((file . "file1") (command . "cmd1 -Ifoo -Ibar") (directory . "/dir/bir"))
                    ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo") (directory . "/boo/foo"))])
                    '(("/dir/bir" . "-Ifoo -Ibar") ("/boo/foo" . "-Ibaz -Iboo -Dloo"))))
  (should (equal (cmake-ide--json-to-hdr-assoc
                  '[((file . "file3") (command . "cmd3 -Itre -Dbre -c file3.c") (directory . "/dir1"))
                    ((file . "file4") (command . "cmd3 -Iasda -Dtesco -c file4.c") (directory . "/dir1"))])
                  '(("/dir1" . "-Itre -Dbre") ("/dir1" . "-Iasda -Dtesco")))))


(ert-deftest test-json-to-hdr-flags ()
  (should (equal (cmake-ide--json-to-hdr-flags "/dir1/file1.h"
                                               '[((file . "/dir1/file1.h") (command . "cmd1 -Ifoo -Ibar")
                                                  (directory . "/dir1"))])
                 '("-Ifoo" "-Ibar")))
  (should (equal (cmake-ide--json-to-hdr-flags "/dir1/file3.h"
                                               '[((file . "/dir1/file1.h") (command . "cmd1 -Ifoo -Ibar")
                                                  (directory . "/dir1"))])
                 '("-Ifoo" "-Ibar")))
    (should (equal (cmake-ide--json-to-hdr-flags "/dir3/file1.h"
                                               '[((file . "/dir1/file1.h") (command . "cmd1 -Ifoo -Ibar")
                                                  (directory . "/dir1"))])
                 nil))
  (should (equal (cmake-ide--json-to-hdr-flags "/dir1/file2.h"
                                               '[((file . "/dir1/file1.h") (command . "cmd1 -Ifoo -Ibar")
                                                  (directory . "/dir1"))
                                                 ((file . "/dir2/file2.h") (command . "cmd2 -Iloo -Dboo")
                                                  (directory . "/dir2"))])
                 '("-Ifoo" "-Ibar"))))


(provide 'cmake-ide-test)
;;; cmake-ide-test.el ends here
