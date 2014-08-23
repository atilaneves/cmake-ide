;; -*- lexical-binding: t; -*-

;;; cmake-ide --- Calls CMake to find out include paths and other compiler flags

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.1
;; Package-Requires: ((auto-complete-clang "0.1i") (flycheck "0.17"))
;; Keywords: languages
;; URL: http://github.com/atilaneves/cmake-ide

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

;; (cmake-ide-setup)
;
; If cmake-ide-flags-c or cmake-ide-flags-c++ are set, they will be added to ac-clang-flags.
; These variables should be set. Particularly, they should contain the system include paths.
;
;;; Code:

(require 'json)
(require 'auto-complete-clang)
(require 'flycheck)

;;; The C flags for ac-clang-flags
(defvar cmake-ide-flags-c nil)

;;; The C++ flags for ac-clang flags
(defvar cmake-ide-flags-c++ nil)

;;; The directory to run CMake in. If nil, a temporary directory is used.
(defvar cmake-ide-dir nil)

;;; The buffers to set variables for
(defvar cmake-ide--src-buffers nil)
(defvar cmake-ide--hdr-buffers nil)

;;;###autoload
(defun cmake-ide-setup ()
  "Sets up the Emacs hooks for working with CMake projects."
  (add-hook 'c-mode-common-hook (lambda ()
                                  (add-hook 'find-file-hook #'cmake-ide-run-cmake)))
  (add-hook 'before-save-hook (lambda ()
                                (when (and (cmake-ide--is-src-file (buffer-file-name))
                                           (not (file-readable-p (buffer-file-name))))
                                  (add-hook 'after-save-hook 'cmake-ide--new-file-saved nil 'local)))))


(defun cmake-ide--new-file-saved ()
  (cmake-ide-run-cmake)
  (remove-hook 'after-save-hook 'cmake-ide--new-file-saved 'local))


;;;###autoload
(defun cmake-ide-run-cmake ()
  "Run CMake and set compiler flags for auto-completion and
flycheck. This works by calling cmake in a temporary directory
and parsing the json file deposited there with the compiler
flags."
  (when (file-readable-p (buffer-file-name)) ; new files needs not apply
    (let ((project-dir (cmake-ide--locate-cmakelists)))
      (when project-dir ; no point if it's not a CMake project
        ;; register this buffer to be either a header or source file
        ;; waiting for results
        (if (cmake-ide--is-src-file buffer-file-name)
            (add-to-list 'cmake-ide--src-buffers (current-buffer))
          (add-to-list 'cmake-ide--hdr-buffers (current-buffer)))

        ;; run CMake if necessary
        (when (not (get-process "cmake")) ; only run it if not running
          (let* ((cmake-dir (cmake-ide--get-dir))
                 (default-directory cmake-dir))
            (message (format "Running cmake for src path %s in build path %s" project-dir cmake-dir))
            (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-dir)
            ;; register callback to run when cmake is finished
            (set-process-sentinel (get-process "cmake")
                                  (lambda (process event)
                                    (let* ((json-file (expand-file-name "compile_commands.json" cmake-dir))
                                           (json (json-read-file json-file))
                                           (src-flags (cmake-ide--json-to-src-flags buffer-file-name json))
                                           (hdr-flags (cmake-ide--json-to-hdr-flags buffer-file-name json)))
                                        ;set flags for all source files that registered
                                      (mapc (lambda (x)
                                              (cmake-ide-set-compiler-flags x src-flags))
                                            cmake-ide--src-buffers)
                                      (setq cmake-ide--src-buffers nil) ; reset
                                        ;set flags for all header fiels that registered
                                      (mapc (lambda (x)
                                              (cmake-ide-set-compiler-flags x hdr-flags))
                                            cmake-ide--hdr-buffers)
                                      (setq cmake-ide--hdr-buffers nil))))))))))


(defun cmake-ide-set-compiler-flags (buffer flags)
  "Set ac-clang and flycheck variables from FLAGS"
  (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (make-local-variable 'ac-clang-flags)
          (make-local-variable 'flycheck-clang-include-path)
          (make-local-variable 'flycheck-clang-definitions)
          (setq ac-clang-flags (append (cmake-ide--get-existing-ac-clang-flags) flags))
          (setq flycheck-clang-include-path (cmake-ide--flags-to-includes flags))
          (setq flycheck-clang-definitions (cmake-ide--flags-to-defines flags))
          (flycheck-clear)
          (message (format "ac-clang-flags for %s from CMake JSON:\n%s" buffer-file-name ac-clang-flags)))))


(defun cmake-ide--get-dir ()
  "Return the directory name to run CMake in"
  (file-name-as-directory (or cmake-ide-dir (make-temp-file "cmake" t))))


(defun cmake-ide--ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


(defun cmake-ide--is-src-file (string)
  "Tests is STRING is a source file or not"
  (or (cmake-ide--ends-with string ".c")
      (cmake-ide--ends-with string ".cpp")
      (cmake-ide--ends-with string ".C")
      (cmake-ide--ends-with string ".cxx")
      (cmake-ide--ends-with string ".cc")))


(defun cmake-ide--filter (pred lst)
  "Filter LST based on PRED. Because elisp"
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) lst)))


(defun cmake-ide--json-to-hdr-assoc (json)
  "Transform JSON into an assoc list"
    (cmake-ide--json-to-symbol-assoc json 'directory))


(defun cmake-ide--json-to-src-assoc (json)
  "Transform JSON object from cmake to an assoc list."
  (cmake-ide--json-to-symbol-assoc json 'file))


(defun cmake-ide--json-to-symbol-assoc (json symbol)
  "Transform JSON object from cmake to an assoc list."
  (mapcar (lambda (x)
            (let* ((key (cdr (assq symbol x)))
                   (command (cdr (assq 'command x)))
                   (args (split-string command " +"))
                   (flags (cmake-ide--filter (lambda (x) (string-match "^-[ID].+\\b" x)) args))
                   (join-flags (mapconcat 'identity flags " ")))
            (cons key join-flags)))
          json))


(defun cmake-ide--json-to-src-flags (file-name json)
  "From JSON to a list of compiler flags for compileable source files"
  (let* ((cmake-ide-alist (cmake-ide--json-to-src-assoc json))
         (flags-string (cdr (assoc file-name cmake-ide-alist))))
    (split-string flags-string " +")))


(defun cmake-ide--json-to-hdr-flags (file-name json)
  "From JSON to a list of compiler flags for headers"
  (let* ((cmake-ide-alist (cmake-ide--json-to-hdr-assoc json))
         (dir (directory-file-name (file-name-directory file-name)))
         (flags-string (cdr (assoc dir cmake-ide-alist))))
    (if flags-string (split-string flags-string " +") nil)))


(defun cmake-ide--to-simple-flags (flags flag)
  "From JSON to a list of include directories/defines"
  (let* ((include-flags (cmake-ide--filter (lambda (x)
                                      (let ((match (string-match flag x)))
                                        (and match (zerop match))))
                                    flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) include-flags)))


(defun cmake-ide--flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of include directories"
  (cmake-ide--to-simple-flags flags "-I"))


(defun cmake-ide--flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines"
  (cmake-ide--to-simple-flags flags "-D"))


(defun cmake-ide--get-existing-ac-clang-flags ()
  "Return existing ac-clang flags for this mode, if set"
  (if (eq major-mode 'c++-mode)
      (cmake-ide--symbol-value 'cmake-ide-flags-c++)
    (cmake-ide--symbol-value 'cmake-ide-flags-c)))


(defun cmake-ide--symbol-value (sym)
  "Return the value of SYM if bound, nil if not"
  (if (boundp sym) (symbol-value sym) nil))


(defun cmake-ide--locate-cmakelists ()
  "Find the topmost CMakeLists.txt file"
  (cmake-ide--locate-cmakelists-impl default-directory nil))


(defun cmake-ide--locate-cmakelists-impl (dir last-found)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a 'plan B'"
  (let ((new-dir (locate-dominating-file dir "CMakeLists.txt")))
    (if new-dir
        (cmake-ide--locate-cmakelists-impl (expand-file-name ".." new-dir) new-dir)
      last-found)))


(provide 'cmake-ide)
;;; cmake-ide.el ends here
