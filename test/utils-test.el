;;; utils-test.el --- Unit tests for cmake-ide.

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


(ert-deftest test-cmake-ide--string-empty-p ()
  (should (equal (cmake-ide--string-empty-p "") t))
  (should (equal (cmake-ide--string-empty-p "foo") nil)))

(ert-deftest test-unquote ()
  (should (equal (cmake-ide--unquote "\"foo\"") "foo")))

(ert-deftest test-cmake-ide--string-match ()
  (should (equal (cmake-ide--string-match "foo" "foobar") 0))
  (should (equal (cmake-ide--string-match "oo" "foobar") 1))
  (should (equal (cmake-ide--string-match "foo" "bar") nil)))

(provide 'utils-test)
;;; utils-test.el ends here
