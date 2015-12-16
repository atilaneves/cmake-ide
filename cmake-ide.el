;;; cmake-ide.el --- Calls CMake to find out include paths and other compiler flags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "24.1"))
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

;; This package runs CMake and sets variables for IDE-like functionality
;; provided by other packages such as:
;; On the fly syntax checks with flycheck
;; auto-completion using auto-complete-clang or company-clang
;; Jump to definition and refactoring with rtags
;; These other packages must be installed for the functionality to work

;;; Usage:

;; (cmake-ide-setup)
;;
;; If cmake-ide-flags-c or cmake-ide-flags-c++ are set, they will be added to ac-clang-flags.
;; These variables should be set. Particularly, they should contain the system include paths.
;;
;;; Code:

(require 'json)

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

(defvar cmake-ide-compile-command
  nil
  "The command to use to compile the project.  Can also include running tests.")

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

(defun cmake-ide--mode-hook()
  "Function to add to a major mode hook"
  (add-hook 'find-file-hook #'cmake-ide--maybe-run-cmake nil 'local)
  (when (and (featurep 'rtags) (cmake-ide--locate-cmakelists))
    (cmake-ide-maybe-start-rdm)))

;;;###autoload
(defun cmake-ide-setup ()
  "Set up the Emacs hooks for working with CMake projects."
  (add-hook 'c-mode-hook #'cmake-ide--mode-hook)
  (add-hook 'c++-mode-hook #'cmake-ide--mode-hook)

  ;; When creating a file in Emacs, run CMake again to pick it up
  (add-hook 'before-save-hook (lambda ()
                                (when (and (cmake-ide--is-src-file (buffer-file-name))
                                           (not (file-readable-p (buffer-file-name))))
                                  (add-hook 'after-save-hook 'cmake-ide--new-file-saved nil 'local)))))

(defun cmake-ide--new-file-saved ()
  "Run CMake to pick up newly created files."
  (cmake-ide-run-cmake)
  (remove-hook 'after-save-hook 'cmake-ide--new-file-saved 'local))

(defun cmake-ide--maybe-run-cmake ()
  "Run CMake if the compilation database json file is not found."
  (if (cmake-ide--need-to-run-cmake)
      (cmake-ide-run-cmake)
    (progn
      (cmake-ide--add-file-to-buffer-list)
      (cmake-ide--on-cmake-finished))))

(defun cmake-ide--add-file-to-buffer-list ()
  "Add buffer to the appropriate list for when CMake finishes running."
  (if (cmake-ide--is-src-file buffer-file-name)
      (add-to-list 'cmake-ide--src-buffers (current-buffer))
    (add-to-list 'cmake-ide--hdr-buffers (current-buffer))))

(defun cmake-ide--comp-db-file-name ()
  "The name of the compilation database file."
  (expand-file-name "compile_commands.json" (cmake-ide--get-dir)))

(defun cmake-ide--need-to-run-cmake ()
  "If CMake needs to be run or not."
  (and (not (get-process "cmake")) ; don't run if already running
       (not (file-exists-p (cmake-ide--comp-db-file-name))))) ; no need if the file exists

;;;###autoload
(defun cmake-ide-run-cmake ()
  "Run CMake and set compiler flags for auto-completion and flycheck.
This works by calling cmake in a temporary directory
and parsing the json file deposited there with the compiler
flags."
  (interactive)
  (when (file-readable-p (buffer-file-name)) ; new files need not apply
    (let ((project-dir (cmake-ide--locate-cmakelists)))
      (when project-dir ; no point if it's not a CMake project
        ;; register this buffer to be either a header or source file
        ;; waiting for results
        (cmake-ide--add-file-to-buffer-list)

        (let ((default-directory (cmake-ide--get-dir)))
          (cmake-ide--run-cmake-impl project-dir (cmake-ide--get-dir))
          (cmake-ide--register-callback))))))

(defun cmake-ide--message (str &rest vars)
  "Output a message with STR and formatted by VARS."
  (message (apply #'format (concat "cmake-ide: " str) vars)))

(defun cmake-ide--register-callback ()
  "Register callback for when CMake finishes running."
  (set-process-sentinel (get-process "cmake")
                        (lambda (_process _event)
                          (cmake-ide--message "Finished running CMake")
                          (cmake-ide--on-cmake-finished))))

(defun cmake-ide--on-cmake-finished ()
  "Set compiler flags for all buffers that requested it."
  (let* ((json (json-read-file (cmake-ide--comp-db-file-name)))
         (set-flags (lambda (x) (cmake-ide--set-flags-for-file json x))))
    (mapc set-flags cmake-ide--src-buffers)
    (mapc set-flags cmake-ide--hdr-buffers)
    (setq cmake-ide--src-buffers nil cmake-ide--hdr-buffers nil)
    (cmake-ide--run-rc)))


(defun cmake-ide--run-rc ()
  "Run rc to add definitions to the rtags daemon."
  (when (and (featurep 'rtags) (get-process "rdm"))
    (with-current-buffer (get-buffer cmake-ide-rdm-buffer-name)
      (rtags-call-rc "-J" (cmake-ide--get-dir)))))

(defun cmake-ide--set-flags-for-file (json buffer)
  "Set the compiler flags from JSON for BUFFER visiting file FILE-NAME."
  (cmake-ide--message "Setting flags for file %s" (buffer-file-name buffer))
  (let* ((file-params (cmake-ide--file-params json (buffer-file-name buffer)))
         (commands (mapcar (lambda (x) (cmake-ide--get-file-param 'command x)) json))
         (src-flags (cmake-ide--params-to-src-flags file-params))
         (hdr-flags (cmake-ide--commands-to-hdr-flags commands))
         (src-includes (cmake-ide--params-to-src-includes file-params))
         (hdr-includes (cmake-ide--commands-to-hdr-includes commands))
         (sys-includes (cmake-ide--params-to-sys-includes file-params))
         )
    ;; set flags for all source files that registered
    (if (cmake-ide--is-src-file (buffer-file-name buffer))
        (cmake-ide-set-compiler-flags buffer src-flags src-includes sys-includes)
      (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes))))


(defun cmake-ide-set-compiler-flags (buffer flags includes sys-includes)
  "Set ac-clang and flycheck variables for BUFFER from FLAGS, INCLUDES and SYS-INCLUDES."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer

      (when (featurep 'auto-complete-clang)
        (make-local-variable 'ac-clang-flags)
        (setq ac-clang-flags (cmake-ide--filter-ac-flags (cmake-ide--get-compiler-flags flags))))

      (when (featurep 'company)
        (make-local-variable 'company-clang-arguments)
        (setq company-clang-arguments (cmake-ide--filter-ac-flags (cmake-ide--get-compiler-flags flags))))

      (when (featurep 'company-c-headers)
        (make-local-variable 'company-c-headers-path-user)
        (setq company-c-headers-path-user (cmake-ide--flags-to-include-paths flags)))

      (when (featurep 'flycheck)
        (make-local-variable 'flycheck-clang-include-path)
        (setq flycheck-clang-include-path (append sys-includes (cmake-ide--flags-to-include-paths flags)))

        (make-local-variable 'flycheck-clang-definitions)
        (setq flycheck-clang-definitions
              (append (cmake-ide--get-existing-definitions) (cmake-ide--flags-to-defines flags)))

        (make-local-variable 'flycheck-clang-args)
        (setq flycheck-clang-args (cmake-ide--flags-filtered flags))

        (make-local-variable 'flycheck-cppcheck-include-path)
        (setq flycheck-cppcheck-include-path (append sys-includes (cmake-ide--flags-to-include-paths flags)))

        (setq flycheck-clang-includes includes)
        (flycheck-clear)
        (run-at-time "0.5 sec" nil 'flycheck-buffer)))))

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
              (cmake-ide--message "File '%s' successfully removed" filename)))))
    (error "Not possible to delete a file without setting cmake-ide-dir")))


(defun cmake-ide--run-cmake-impl (project-dir cmake-dir)
  "Run the CMake process for PROJECT-DIR in CMAKE-DIR."
  (when project-dir
    (let ((default-directory cmake-dir))
      (cmake-ide--message "Running cmake for src path %s in build path %s" project-dir cmake-dir)
      (start-process "cmake" "*cmake*" "cmake" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" project-dir))))


(defun cmake-ide--get-dir ()
  "Return the directory name to run CMake in."
  (when (not cmake-ide-dir) (setq cmake-ide-dir (make-temp-file "cmake" t)))
  (file-name-as-directory cmake-ide-dir))


(defun cmake-ide--ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (not (null string))
       (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))


(defun cmake-ide--is-src-file (string)
  "Test if STRING is a source file or not."
  (or (cmake-ide--ends-with string ".c")
      (cmake-ide--ends-with string ".cpp")
      (cmake-ide--ends-with string ".C")
      (cmake-ide--ends-with string ".cxx")
      (cmake-ide--ends-with string ".cc")))


(defun cmake-ide--filter (pred seq)
  "Apply PRED to filter SEQ."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) seq)))


(defun cmake-ide--filter-params (file-params filter-func)
  "Filter FILE-PARAMS with FILTER-FUNC."
  ;; The compilation database is a json array of json objects
  ;; Each object is a file with directory, file and command fields
  ;; Depending on FILTER-FUNC, it maps file names to desired compiler flags
  ;; An example would be -I include flags
  (let* ((command (cmake-ide--get-file-param 'command file-params))
         (args (split-string command " +"))
         (flags (funcall filter-func args)))
    (mapconcat 'identity flags " ")))


(defun cmake-ide--args-to-only-flags (args)
  "Get compiler flags from ARGS."
  (cmake-ide--filter
   (lambda (x) (not (string-match "\\.\\(?:c\\|C\\|cc\\|cxx\\|cpp\\)$" x)))
   args))

(defun cmake-ide--unescape (str)
  "Remove JSON-escaped backslashes in STR."
  (let* ((no-double-backslashes (replace-regexp-in-string "\\\\\\\\" "\\\\" str))
         (no-backslash-quote (replace-regexp-in-string "\\\\\"" "\"" no-double-backslashes)))
    no-backslash-quote))

(defun cmake-ide--params-to-src-flags (file-params &optional filter-func)
  "Source compiler flags for FILE-PARAMS using FILTER-FUNC."
  (if (not file-params) nil
    (let* ((filter-func (or filter-func #'cmake-ide--args-to-only-flags))
           (value (cmake-ide--filter-params file-params filter-func))
           (flags-string (if value value nil)))
      (if flags-string (cmake-ide--cleanup-flags-str flags-string) nil))))

(defun cmake-ide--cleanup-flags-str (str)
  "Clean up and filter STR to yield a list of compiler flags."
  (let* ((unescaped-flags-string (cmake-ide--unescape str))
         (flags (cdr (split-string unescaped-flags-string " +"))))
    flags))

(defun cmake-ide--filter-ac-flags (flags)
  "Filter unwanted compiler arguments out from FLAGS."
  (cmake-ide--filter
   (lambda (x) (not (or (string-match "^-m32$" x) (string-match "^-Werror$" x) (string-match "^-c$" x))))
   flags))

(defun cmake-ide--commands-to-hdr-flags (commands)
  "Header compiler flags from COMMANDS."
  (let ((args (cmake-ide--flatten (mapcar (lambda (x) (cdr (split-string x " +"))) commands))))
    (delete-dups (cmake-ide--args-to-only-flags args))))

(defun cmake-ide--params-to-src-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (cmake-ide--flags-to-includes (cmake-ide--params-to-src-flags file-params 'identity)))


(defun cmake-ide--params-to-sys-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (cmake-ide--flags-to-sys-includes (cmake-ide--params-to-src-flags file-params 'identity)))


(defun cmake-ide--commands-to-hdr-includes (commands)
  "Header `-include` flags from COMMANDS."
  (let ((args (cmake-ide--flatten (mapcar (lambda (x) (split-string x " +")) commands))))
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


(defun cmake-ide--flags-filtered (flags)
  "Filter out defines and includes from FLAGS."
  (cmake-ide--filter
   (lambda (x)
     (let ((imatch (string-match "^-I" x))
           (dmatch (string-match "^-D" x)))
       (and (not imatch) (not dmatch))
       ))
   flags))


(defun cmake-ide--to-simple-flags (flags flag)
  "A list of either directories or defines from FLAGS depending on FLAG."
  (let* ((include-flags (cmake-ide--filter
                         (lambda (x)
                           (let ((match (string-match flag x)))
                             (and match (zerop match))))
                         flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) include-flags)))


(defun cmake-ide--get-compiler-flags (flags)
  "Use FLAGS to return all compiler flags including existing ones."
  (append (cmake-ide--get-existing-compiler-flags) flags))

(defun cmake-ide--get-existing-compiler-flags ()
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

(defun cmake-ide--string-to-json (json-str)
  "Tranform JSON-STR into an opaque json object."
  (json-read-from-string json-str))


(defun cmake-ide--file-params (json file-name)
  "Get parameters from a JSON object for FILE-NAME."
  (cmake-ide--find-in-vector (lambda (x) (equal (cmake-ide--get-file-param 'file x) file-name)) json))


(defun cmake-ide--find-in-vector (pred vec)
  "Find the 1st element satisfying PRED in VEC."
  (let ((i 0)
        (max (length vec))
        (found nil))
    (while (and (not found) (< i max))
      (if (funcall pred (elt vec i))
          (setq found t)
        (setq i (1+ i)))
      )
    (if found (elt vec i) nil)))

(defun cmake-ide--get-file-param (key obj)
  "Get the value for KEY in OBJ."
  (cdr (assoc key obj)))

;;;###autoload
(defun cmake-ide-compile ()
  "Compile the project."
  (interactive)
  (if cmake-ide-dir
      (compile (cmake-ide--get-compile-command cmake-ide-dir))
    (let ((command (read-from-minibuffer "Compiler command: " compile-command)))
      (compile command)))
  (cmake-ide--run-rc))


(defun cmake-ide--get-compile-command (dir)
  "Return the compile command to use for DIR."
  (cond (cmake-ide-compile-command cmake-ide-compile-command)
        ((file-exists-p (expand-file-name "build.ninja" dir)) (concat "ninja -C " dir))
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
