;; -*- lexical-binding: t; -*-

;;; cmake-json --- Calls CMake to find out include paths and other compiler flags

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.1
;; Package-Requires: ((auto-complete "1.3.1") (flycheck "0.17"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/cmake-json

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

;; This package runs CMake and sets variables for on the fly syntax checking
;; and auto-completion using clang.

;;; Usage:

;(add-hook 'c-mode-common-hook (lambda ()
;                                (add-hook 'find-file-hook (lambda ()
;                                                            (cmake-json-run buffer-file-name)))))


;;; Code:

(require 'json)
(require 'auto-complete)
(require 'flycheck)


(defun cmake-json-run (file-name)
  "Run CMake for source file FILE-NAME and set compiler flags for auto-completion and flycheck"
  (when (and (not (get-process "cmake")) (cmake--json-is-src-file file-name))
    (let* ((cmake-dir (cmake--json-locate-cmakelists))
           (dir-name (file-name-as-directory (make-temp-file "cmake" t)))
           (default-directory dir-name))
      (message (format "Running cmake in path %s" dir-name))

      (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" cmake-dir)
      (set-process-sentinel (get-process "cmake")
                            (lambda (process event)
                              (let* ((json (json-read-file (expand-file-name "compile_commands.json" dir-name)))
                                     (flags (cmake--json-to-flags file-name json)))
                                (cmake-json-set-compiler-flags flags)))))))


(defun cmake--json-ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


(defun cmake--json-is-src-file (string)
  "Tests is STRING is a source file or not"
  (or (cmake--json-ends-with string ".c")
      (cmake--json-ends-with string ".cpp")
      (cmake--json-ends-with string ".C")
      (cmake--json-ends-with string ".cxx")
      (cmake--json-ends-with string ".cc")))


(defun my--filter (pred lst)
  "Filter LST based on PRED. Because elisp"
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) lst)))


(defun cmake--json-to-assoc (json)
  "Transform json object from cmake to an assoc list."
  (mapcar (lambda (x)
            (let* ((filename (cdr (assq 'file x)))
                   (command (cdr (assq 'command x)))
                   (args (split-string command " +"))
                   (flags (my--filter (lambda (x) (string-match "^-[ID].+\\b" x)) args))
                   (join-flags (mapconcat 'identity flags " ")))
            (cons filename join-flags)))
          json))


(defun cmake--json-to-flags (file-name json)
  "From JSON to a list of compiler flags"
  (let* ((cmake-json-alist (cmake--json-to-assoc json))
         (flags-string (cdr (assoc file-name cmake-json-alist))))
    (split-string flags-string " +")))


(defun cmake--json-to-simple-flags (flags flag)
  "From JSON to a list of include directories"
  (let* ((include-flags (my--filter (lambda (x)
                                      (let ((match (string-match flag x)))
                                        (and match (zerop match))))
                                    flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) include-flags)))


(defun cmake--json-flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of include directories"
  (cmake--json-to-simple-flags flags "-I"))


(defun cmake--json-flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines"
  (cmake--json-to-simple-flags flags "-D"))


(defun cmake-json-set-compiler-flags (flags)
  "Set ac-clang and flycheck variables from FLAGS"
  (make-local-variable 'ac-clang-flags)
  (make-local-variable 'flycheck-clang-include-path)
  (make-local-variable 'flycheck-clang-definitions)
  (setq ac-clang-flags (append (cmake--json-get-existing-ac-clang-flags) flags))
  (setq flycheck-clang-include-path (cmake--json-flags-to-includes flags))
  (setq flycheck-clang-definitions (cmake--json-flags-to-defines flags))
  (flycheck-clear)
  (message (format "Setting compiler flags from CMake JSON to:\n%s" ac-clang-flags)))


(defun cmake--json-get-existing-ac-clang-flags ()
  "Return existing ac-clang flags for this mode, if set"
  (if (eq major-mode 'c++-mode)
      (cmake--json-symbol-value 'ac-clang-flags-c++)
    (cmake--json-symbol-value 'ac-clang-flags-c)))


(defun cmake--json-symbol-value (sym)
  "Return the value of SYM if bound, nil if not"
  (if (boundp sym) (symbol-value sym) nil))


(defun cmake--json-locate-cmakelists ()
  "Find the topmost CMakeLists.txt file"
  (cmake--json-locate-cmakelists-impl default-directory nil))


(defun cmake--json-locate-cmakelists-impl (dir last-found)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a 'plan B'"
  (let ((new-dir (locate-dominating-file dir "CMakeLists.txt")))
    (if new-dir
        (cmake--json-locate-cmakelists-impl (expand-file-name ".." new-dir) new-dir)
      last-found)))


(provide 'cmake-json)
;;; cmake-json.el ends here
