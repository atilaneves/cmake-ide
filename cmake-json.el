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

(require 'json)

(defun set-cmake-json ()
  (let* ((dir-name (file-name-as-directory (make-temp-file "cmake" t)))
         (default-directory dir-name))
    (message (format "Running cmake in path %s" dir-name))

  (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" "/home/aalvesne/sla/sla")
  (set-process-sentinel (get-process "cmake")
                        (lambda (process event)
                          (let ((json (json-read-file (expand-file-name "compile_commands.json" dir-name))))
                          (setq cmake-json-alist (cmake--json-to-assoc json)))))))


(defun my--filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun cmake--json-to-assoc (json)
  "Transform json object from cmake to an assoc list."
  (mapcar (lambda (x)
            (let* ((filename (cdr (assq 'file x)))
                   (command (cdr (assq 'command x)))
                   (args (split-string command " "))
                   (flags (my--filter (lambda (x) (string-match "^-[ID]\\w+" x)) args))
                   (join-flags (mapconcat 'identity flags " ")))
            (cons filename join-flags)))
          json))


(provide 'cmake-json)
;;; cmake-json.el ends here
