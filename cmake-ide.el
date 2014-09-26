;;; cmake-ide --- Calls CMake to find out include paths and other compiler flags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.2
;; Package-Requires: ((auto-complete-clang "0.1") (flycheck "0.17")  (emacs "24.1"))
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

(declare-function rtags-call-rc "rtags")

(defvar cmake-ide-flags-c
  nil
  "The C compiler flags to use.  Should have -I flags for system includes.")

(defvar cmake-ide-flags-c++
  nil
  "The C++ compiler flags to use.  Should have -I flags for system includes.")

(defvar cmake-ide-dir
  nil
  "The build directory to run CMake in.  If nil, runs in a temp dir.")

;;; The buffers to set variables for
(defvar cmake-ide--src-buffers nil)
(defvar cmake-ide--hdr-buffers nil)

(defcustom cmake-ide-rdm-executable
  "rdm"
  "Location of rdm executable."
  :group 'rtags
  :type 'file)

(defcustom cmake-ide-rc-executable
  "rc"
  "Location of rc executable."
  :group 'rtags
  :type 'file)

(defconst cmake-ide-rdm-buffer-name "*rdm*" "The rdm buffer name.")

;;;###autoload
(defun cmake-ide-setup ()
  "Set up the Emacs hooks for working with CMake projects."
  (add-hook 'c-mode-common-hook (lambda ()
                                  (add-hook 'find-file-hook #'cmake-ide-run-cmake)
                                    (when (and (featurep 'rtags) (cmake-ide--locate-cmakelists))
                                      (cmake-ide-maybe-start-rdm))))

  (add-hook 'before-save-hook (lambda ()
                                (when (and (cmake-ide--is-src-file (buffer-file-name))
                                           (not (file-readable-p (buffer-file-name))))
                                  (add-hook 'after-save-hook 'cmake-ide--new-file-saved nil 'local)))))

(defun cmake-ide--new-file-saved ()
  "Run CMake to pick up newly created files."
  (cmake-ide-run-cmake)
  (remove-hook 'after-save-hook 'cmake-ide--new-file-saved 'local))


;;;###autoload
(defun cmake-ide-run-cmake ()
  "Run CMake and set compiler flags for auto-completion and flycheck.
This works by calling cmake in a temporary directory
and parsing the json file deposited there with the compiler
flags."
  (interactive)
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
            (cmake-ide--run-cmake-impl project-dir cmake-dir)
            ;; register callback to run when cmake is finished
            (set-process-sentinel (get-process "cmake")
                                  (lambda (_process _event)
                                    (let* ((json-file (expand-file-name "compile_commands.json" cmake-dir))
                                           (json (json-read-file json-file)))
                                      ;; set flags for all source files that registered
                                      (mapc (lambda (x)
                                              (cmake-ide--set-flags-for-file json x))
                                            cmake-ide--src-buffers)
                                      (setq cmake-ide--src-buffers nil) ; reset
                                      ;; set flags for all header files that registered
                                      (mapc (lambda (x)
                                              (cmake-ide--set-flags-for-file json x))
                                            cmake-ide--hdr-buffers)
                                      (setq cmake-ide--hdr-buffers nil)
                                      (when (and (featurep 'rtags) (get-process "rdm"))
                                        (with-current-buffer (get-buffer cmake-ide-rdm-buffer-name)
                                          (rtags-call-rc "-J" cmake-dir))))))))))))



(defun cmake-ide--set-flags-for-file (json buffer)
  "Set the compiler flags from JSON for BUFFER visiting file FILE-NAME."
  (let* ((src-flags (cmake-ide--json-to-src-flags (buffer-file-name buffer) json))
         (hdr-flags (cmake-ide--json-to-hdr-flags json))
         (src-includes (cmake-ide--json-to-src-includes (buffer-file-name buffer) json))
         (hdr-includes (cmake-ide--json-to-hdr-includes json))
         (sys-includes (cmake-ide--json-to-sys-includes (buffer-file-name buffer) json))
         )
    ;; set flags for all source files that registered
    (when src-flags (cmake-ide-set-compiler-flags buffer src-flags src-includes sys-includes))
    (when hdr-flags (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes))))


(defun cmake-ide-set-compiler-flags (buffer flags includes sys-includes)
  "Set ac-clang and flycheck variables for BUFFER from FLAGS and INCLUDES and SYS-INCLUDES."
  (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (make-local-variable 'ac-clang-flags)
          (make-local-variable 'flycheck-clang-include-path)
          (make-local-variable 'flycheck-clang-definitions)
          (setq ac-clang-flags (append (cmake-ide--get-existing-ac-clang-flags) flags))
          (setq flycheck-clang-include-path  (append sys-includes (cmake-ide--flags-to-include-paths flags)))
          (setq flycheck-clang-definitions (append (cmake-ide--get-existing-definitions) (cmake-ide--flags-to-defines flags)))
          (setq flycheck-clang-includes includes)
          (flycheck-clear)
          (run-at-time "0.5 sec" nil 'flycheck-buffer))))


(defun cmake-ide-delete-file ()
  "Remove file connected to current buffer and kill buffer, then run CMake."
  (interactive)
  (if cmake-ide-dir
      (let ((filename (buffer-file-name))
            (buffer (current-buffer))
            (name (buffer-name)))
        (if (not (and filename (file-exists-p filename)))
            (error "Buffer '%s' is not visiting a file!" name)
          (when (yes-or-no-p "Are you sure you want to remove this file? ")
            (delete-file filename)
            (kill-buffer buffer)
            (let ((project-dir (cmake-ide--locate-cmakelists)))
              (when project-dir (cmake-ide--run-cmake-impl project-dir cmake-ide-dir))
              (message "File '%s' successfully removed" filename)))))
    (error "Not possible to delete a file without setting cmake-ide-dir")))


(defun cmake-ide--run-cmake-impl (project-dir cmake-dir)
  "Run the CMake process for PROJECT-DIR in CMAKE-DIR."
  (when project-dir
    (let ((default-directory cmake-dir))
      (message (format "Running cmake for src path %s in build path %s" project-dir cmake-dir))
      (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-dir))))


(defun cmake-ide--get-dir ()
  "Return the directory name to run CMake in."
  (file-name-as-directory (or cmake-ide-dir (make-temp-file "cmake" t))))


(defun cmake-ide--ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


(defun cmake-ide--is-src-file (string)
  "Test if STRING is a source file or not."
  (or (cmake-ide--ends-with string ".c")
      (cmake-ide--ends-with string ".cpp")
      (cmake-ide--ends-with string ".C")
      (cmake-ide--ends-with string ".cxx")
      (cmake-ide--ends-with string ".cc")))


(defun cmake-ide--filter (pred lst)
  "Apply PRED to filter LST."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) lst)))


(defun cmake-ide--json-to-src-assoc (json filter-func)
  "Transform JSON object from cmake to an assoc list using FILTER-FUNC."
  (cmake-ide--json-to-symbol-assoc json 'file filter-func))


(defun cmake-ide--json-to-symbol-assoc (json symbol filter-func)
  "Transform JSON object from cmake to an assoc list for SYMBOL using FILTER-FUNC."
  (mapcar (lambda (x)
            (let* ((key (cdr (assq symbol x)))
                   (command (cdr (assq 'command x)))
                   (args (split-string command " +"))
                   (flags (funcall filter-func args))
                   (join-flags (mapconcat 'identity flags " ")))
              (cons key join-flags)))
          json))


(defun cmake-ide--args-to-include-and-define-flags (args)
  "Filters a list of compiler command ARGS to yield only includes and defines."
  (let ((case-fold-search)) ;; case sensitive matching
    (cmake-ide--filter (lambda (x) (string-match "^-[ID].+\\b" x)) args)))


(defun cmake-ide--json-to-src-flags (file-name json &optional filter-func)
  "Source compiler flags for FILE-NAME from JSON using FILTER-FUNC."
  (let* ((filter-func (or filter-func #'cmake-ide--args-to-include-and-define-flags))
         (cmake-ide-alist (cmake-ide--json-to-src-assoc json filter-func))
         (value (assoc file-name cmake-ide-alist))
         (flags-string (if value (cdr value) nil)))
    (if flags-string (split-string flags-string " +") nil)))


(defun cmake-ide--json-to-hdr-flags (json)
  "Header compiler flags from JSON."
  (let* ((commands (mapcar (lambda (x) (cdr (assq 'command x))) json))
        (args (cmake-ide--flatten (mapcar (lambda (x) (split-string x " +")) commands))))
    (delete-dups (cmake-ide--args-to-include-and-define-flags args))))


(defun cmake-ide--json-to-src-includes (file-name json)
  "-include compiler flags for FILE-NAME from JSON."
  (cmake-ide--flags-to-includes (cmake-ide--json-to-src-flags file-name json 'identity)))



(defun cmake-ide--json-to-sys-includes (file-name json)
  "-include compiler flags for FILE-NAME from JSON."
  (cmake-ide--flags-to-sys-includes (cmake-ide--json-to-src-flags file-name json 'identity)))


(defun cmake-ide--json-to-hdr-includes (json)
  "Header `-include` flags from JSON."
  (let* ((commands (mapcar (lambda (x) (cdr (assq 'command x))) json))
         (args (cmake-ide--flatten (mapcar (lambda (x) (split-string x " +")) commands))))
    (delete-dups (cmake-ide--flags-to-includes args))))


(defun cmake-ide--flatten (lst)
  "Flatten LST."
  (apply 'append lst))


(defun cmake-ide--flags-to-include-paths (flags)
  "From FLAGS (a list of flags) to a list of include paths."
  (cmake-ide--to-simple-flags flags "-I"))


(defun cmake-ide--flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines."
  (cmake-ide--to-simple-flags flags "-D"))


(defun cmake-ide--flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of includes."
  (let ((includes nil))
    (while (member "-include" flags)
      (setq flags (cdr (member "-include" flags)))
      (when flags (setq includes (cons (car flags) includes))))
    includes))

(defun cmake-ide--flags-to-sys-includes (flags)
  "From FLAGS (a list of flags) to a list of isystem includes."
  (let ((sysincludes nil))
    (while (member "-isystem" flags)
      (setq flags (cdr (member "-isystem" flags)))
      (when flags (setq sysincludes (cons (car flags) sysincludes))))
    sysincludes))


(defun cmake-ide--to-simple-flags (flags flag)
  "A list of either directories or defines from FLAGS depending on FLAG."
  (let* ((include-flags (cmake-ide--filter
                         (lambda (x)
                           (let ((match (string-match flag x)))
                             (and match (zerop match))))
                         flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) include-flags)))


(defun cmake-ide--get-existing-ac-clang-flags ()
  "Return existing ac-clang flags for this mode, if set."
  (if (eq major-mode 'c++-mode)
      (cmake-ide--symbol-value 'cmake-ide-flags-c++)
    (cmake-ide--symbol-value 'cmake-ide-flags-c)))

(defun cmake-ide--get-existing-definitions ()
  "Return existing compiler defines, if set."
  (cmake-ide--symbol-value 'cmake-ide-definitions))


(defun cmake-ide--symbol-value (sym)
  "Return the value of SYM if bound, nil if not."
  (if (boundp sym) (symbol-value sym) nil))


(defun cmake-ide--locate-cmakelists ()
  "Find the topmost CMakeLists.txt file."
  (cmake-ide--locate-cmakelists-impl default-directory nil))


(defun cmake-ide--locate-cmakelists-impl (dir last-found)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a 'plan B'."
  (let ((new-dir (locate-dominating-file dir "CMakeLists.txt")))
    (if new-dir
        (cmake-ide--locate-cmakelists-impl (expand-file-name ".." new-dir) new-dir)
      last-found)))


;;;###autoload
(defun cmake-ide-compile ()
  "Compile the project."
  (interactive)
  (if cmake-ide-dir
      (compile (cmake-ide--get-compile-command cmake-ide-dir))
    (let ((command (read-from-minibuffer "Compiler command: " compile-command)))
      (compile command))))


(defun cmake-ide--get-compile-command (dir)
  "Return the compile command to use for DIR."
  (cond ((file-exists-p (expand-file-name "build.ninja" dir)) (concat "ninja -C " dir))
        ((file-exists-p (expand-file-name "Makefile" dir)) (concat "make -C " dir))
        (t nil)))


;;;###autoload
(defun cmake-ide-maybe-start-rdm ()
  "Start the rdm (rtags) server."
  (when (featurep 'rtags)
    (unless (get-process "rdm")
      (let ((buf (get-buffer-create cmake-ide-rdm-buffer-name)))
        (with-current-buffer buf (start-process "rdm" (current-buffer)
                                                cmake-ide-rdm-executable))))))


(provide 'cmake-ide)
;;; cmake-ide.el ends here
