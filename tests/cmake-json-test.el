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

(require 'ert)
(require 'cmake-json)


(ert-deftest test-json-to-assoc ()
  (should (equal (cmake--json-to-assoc
                  '[((file . "file1") (command . "cmd1 -Ifoo -Ibar"))
                    ((file . "file2") (command . "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))])
                 '(("file1" . "-Ifoo -Ibar") ("file2" . "-Ibaz -Iboo -Dloo"))))
  (should (equal (cmake--json-to-assoc
                  '[((file . "file3") (command . "cmd3 -Itre -Dbre"))
                    ((file . "file4") (command . "cmd4 -Dloo -Dboo"))])
                 '(("file3" . "-Itre -Dbre") ("file4" . "-Dloo -Dboo"))))
  (should (equal (cmake--json-to-assoc
                  '[((file . "file1") (command . "/usr/bin/c++    -I/foo/bar/baz/fir    -o CMakeFiles/boo.dir/foo.cpp.o -c /foo/bar/baz/fir/foo.cpp") (directory . "/tmp/foo"))])
                  '(("file1" . "-I/foo/bar/baz/fir")))))

(provide 'cmake-json-test)
;;; cmake-json-test.el ends here
