;; -*- lexical-binding: t; -*-
;;; cmake.el --- Calls CMake to find out include paths and other compiler flags

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

(defun set-cmake-json ()
  (let* ((dir-name (file-name-as-directory (make-temp-file "cmake" t)))
         (default-directory dir-name))
    (princ (format "Writing to dir %s" dir-name))

  (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" "/home/aalvesne/sla/sla")
  (set-process-sentinel (get-process "cmake")
                        (lambda (process event)
                          (setq cmake-json-alist (json-read-file (expand-file-name "compile_commands.json" dir-name)))))))


(provide 'cmake-json)
;;; cmake.el ends here
