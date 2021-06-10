;;; cmake-ide.el --- Calls CMake to find out include paths and other compiler flags -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Atila Neves

;; Author:  Atila Neves <atila.neves@gmail.com>
;; Version: 0.6
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (seq "1.11") (levenshtein "0") (s "1.11.0"))
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
(require 'find-file)
(require 'levenshtein)
(require 'cl-lib)
(require 'seq)
(require 's)
(require 'dash)

(defsubst cide--string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(if (not (require 'subr-x nil t))
    (progn
      (message "`subr-x' not found, replacing string-empty-p with cide--string-empty-p")
      (fset 'string-empty-p 'cide--string-empty-p))
  (declare-function string-empty-p "subr-x"))

(declare-function rtags-call-rc "rtags")
(declare-function rtags-executable-find "rtags")
(declare-function irony-cdb-json-add-compile-commands-path "irony")
(declare-function flycheck-clear "flycheck")


(defcustom cmake-ide-flags-c
  nil
  "The C compiler flags to use.  Should have -I flags for system includes."
  :group 'cmake-ide
  :type 'string
  :safe #'stringp)

(defcustom cmake-ide-flags-c++
  nil
  "The C++ compiler flags to use.  Should have -I flags for system includes."
  :group 'cmake-ide
  :type 'string
  :safe #'stringp
  )

(defcustom cmake-ide-dir
  nil
  "The build directory to run CMake in.  If nil, runs in a temporary directory under `cmake-ide-build-pool-dir'.  DEPRECATED, use `cmake-ide-build-dir' instead."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom cmake-ide-build-dir
  nil
  "The build directory to run CMake in.  If nil, runs in a temporary directory under `cmake-ide-build-pool-dir'."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom cmake-ide-build-pool-dir
  temporary-file-directory
  "The parent directory for all automatically created build directories."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp
  )

(defcustom cmake-ide-build-pool-use-persistent-naming
  nil
  "Whether or not to use a persistent naming scheme for all automatically created build directories."
  :group 'cmake-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom cmake-ide-project-dir
  nil
  "The project directory."
  :group 'cmake-ide
  :type 'directory
  :safe #'stringp)

(defcustom cmake-ide-compile-command
  nil
  "The command to use to compile the project.  Can also include running tests."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-make-command
  "make --no-print-directory"
  "The command used to execute Makefile builds."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-ninja-command
  "ninja"
  "The command used to execute ninja type builds."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-cmake-command
  "cmake"
  "The command use to invoke cmake."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-cmake-opts
  "-DCMAKE_BUILD_TYPE=Release"
  "The options passed to cmake when calling it.  DEPRECATED, use `cmake-ide-cmake-args' instead."
  :group 'cmake-ide
  :safe #'stringp)

(defcustom cmake-ide-cmake-args
  nil
  "List of options passed to cmake when calling it."
  :group 'cmake-ide
  :type '(repeat string)
  :safe (lambda (val) (and (listp val) (-all-p 'stringp val))))

(defcustom cmake-ide-header-search-other-file
  t
  "Whether or not to search for a corresponding source file for headers when setting flags for them."
  :group 'cmake-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom cmake-ide-header-search-first-including
  t
  "Whether or not to search for the first source file to include a header when setting flags for them."
  :group 'cmake-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom cmake-ide-header-no-flags
  nil
  "Whether to apply compiler flags to header files.  In some projects this takes too long."
  :group 'cmake-ide
  :type 'boolean
  :safe #'booleanp)

(defcustom cmake-ide-flycheck-cppcheck-strict-standards
  nil
  "Whether or not to be strict when setting cppcheck standards for flycheck.
If 't' or otherwise non-nil, the `flycheck-cppcheck-standards'
variable will only be set to contain standards that exactly match
those from the compile database.  (If there are none, it will not
be modified.)  If 'nil', standards will be gracefully degraded to
the closest possible matches available in cppcheck."
  :group 'cmake-ide
  :type 'boolean
  :safe #'booleanp)

;;; The buffers to set variables for
(defvar cide--src-buffers nil)
(defvar cide--hdr-buffers nil)

(defcustom cmake-ide-rdm-executable
  "rdm"
  "Location of rdm executable."
  :group 'rtags
  :type 'file)

(defcustom cmake-ide-rdm-rc-path
  ""
  "Location of a custom rdm run control file."
  :group 'cmake-ide
  :type 'string
  :safe #'stringp)

(defcustom cmake-ide-src-extensions
  '(".c" ".cpp" ".C" ".cxx" ".cc")
  "A list of file extensions that qualify as source files."
  :group 'cmake-ide
  :type '(repeat string))

(defcustom cmake-ide-ac-flags-to-filter
  '("-m32" "-m64" "-Werror" "-c" "-fPIC" "-pipe" "-g" "-ggdb" "-march=native")
  "Flags to remove from arguments passed to auto-completion."
  :group 'cmake-ide
  :type '(repeat string))

(defcustom cmake-ide-cmakelists-dir
  nil
  "The directory where the main CMakelists.txt is.  DEPRECATED use `cmake-ide-projet-dir' instead."
  :group 'cmake-ide
  :type 'file)

(defvar cmake-ide-try-unique-compiler-flags-for-headers
  nil
  "Whether or not to try all unique compiler flags for header files."
  )

(defvar cmake-sentinel-flag
  nil
  "One interactive execution is allowed at the same time."
  )

(defvar cmake-temp-project-dir
  nil
  "The project dir is kept while the sentinel works."
  )

(defun cide--make-hash-table ()
  "Make a hash table with equal for the test function."
  (make-hash-table :test #'equal))

(defvar cide--cache-dir-to-idb
  (cide--make-hash-table)
  "Key: build directory.  Value: IDB for that build directory.")

(defvar cide--cache-dir-to-cdb-hash
  (cide--make-hash-table)
  "Key: build directory.  Value: The hash of the JSON CDB.")

(defvar cide--cache-pkey-to-dir
  (cide--make-hash-table)
  "Key: project key.  Value: build dir.")

;; Build dirs we've already told irony about
(defvar cide--cache-irony-dirs
  (cide--make-hash-table)
  "Used as a set.  Key: build dir.  Value: T or nil.")

(defvar cide--semantic-system-include)

(defconst cmake-ide-rdm-buffer-name "*rdm*" "The rdm buffer name.")

(defun cide--build-dir-var ()
  "Return the value of `cmake-ide-build-dir' or `cmake-ide-dir'."
  (let ((ret (or cmake-ide-build-dir cmake-ide-dir)))
    (when ret (file-name-as-directory ret))))

(defun cide--project-dir-var ()
  "Return the value of `cmake-ide-project-dir' or `cmake-ide-cmakelists-dir'."
  (or cmake-ide-project-dir cmake-ide-cmakelists-dir))

(defun cide--mode-hook()
  "Function to add to a major mode hook"
  (add-hook 'find-file-hook #'cmake-ide-maybe-run-cmake nil 'local)
  (cmake-ide-maybe-start-rdm))

;;;###autoload
(defun cmake-ide-setup ()
  "Set up the Emacs hooks for working with CMake projects."
  (add-hook 'c-mode-hook #'cide--mode-hook)
  (add-hook 'c++-mode-hook #'cide--mode-hook)

  ;; When creating a file in Emacs, run CMake again to pick it up
  (add-hook 'before-save-hook #'cide--before-save))

(defun cide--before-save ()
  "When creating a file in Emacs, run CMake again to pick it up."
  (when (and (cide--is-src-file (buffer-file-name))
             (not (file-readable-p (buffer-file-name))))
    (add-hook 'after-save-hook 'cide--new-file-saved nil 'local)))

(defun cide--new-file-saved ()
  "Run CMake to pick up newly created files."
  (cmake-ide-run-cmake)
  (remove-hook 'after-save-hook 'cide--new-file-saved 'local))

;;;###autoload
(defun cmake-ide-maybe-run-cmake ()
  "Run CMake if the compilation database JSON file is not found."
  (interactive)
  (when (cide--locate-project-dir)
  (cmake-ide-maybe-start-rdm)
  (if (cide--need-to-run-cmake)
    (cmake-ide-run-cmake)
    (progn
      (cide--add-file-to-buffer-list)
        (cide--on-cmake-finished)))))

(defun cide--add-file-to-buffer-list ()
  "Add buffer to the appropriate list for when CMake finishes running."
  (if (cide--is-src-file buffer-file-name)
      (add-to-list 'cide--src-buffers (current-buffer))
    (add-to-list 'cide--hdr-buffers (current-buffer))))

(defun cide--comp-db-file-name ()
  "The name of the compilation database file."
  (when (cide--build-dir)
    (expand-file-name "compile_commands.json" (cide--build-dir))))

(defun cide--need-to-run-cmake ()
  "If CMake needs to be run or not."
  (and (not (get-process "cmake")) ; don't run if already running
       (not (file-exists-p (cide--comp-db-file-name))))) ; no need if the file exists

;;;###autoload
(defun cmake-ide-run-cmake ()
  "Run CMake and set compiler flags for auto-completion and flycheck.
This works by calling cmake in a temporary directory (or `cmake-ide-build-dir')
 and parsing the JSON file deposited there with the compiler
 flags."
  (interactive)
  (if (not cmake-sentinel-flag)
    (when (buffer-file-name) ; if we call cmake-ide-run-cmake from a scatch buffer, do nothing
      (when (file-readable-p (buffer-file-name)) ; new files need not apply
        (save-some-buffers 1)
        (let ((project-dir (cide--locate-project-dir)))
          (if project-dir ; no point if it's not a CMake project
              (if (not (file-exists-p (expand-file-name "CMakeCache.txt" project-dir)))
                  ;; register this buffer to be either a header or source file
                  ;; waiting for results
                  (progn
                    (cide--add-file-to-buffer-list)
                    ;; run cmake only if project dir contains a CMakeLists.txt file.
                    (if (cide--locate-cmakelists)
                        (let ((cmake-dir (cide--build-dir)))
                          (let ((default-directory cmake-dir))
                            (cide--run-cmake-impl project-dir cmake-dir)
                            (cide--register-callback)
			    (setq cmake-temp-project-dir project-dir)
			    (setq cmake-sentinel-flag t)))
                      (cide--message "No CMakeLists.txt found in project dir, skip cmake run.")))
		(cide--message "CMakeCache.txt found in project dir, skip cmake run."))
            (cide--message "try to run cmake on a non cmake project [%s]" default-directory)))))
    (cide--message "Another cmake is already running, skip cmake run.")))


(defun cide--message (str &rest vars)
  "Output a message with STR and formatted by VARS."
  (message (apply #'format (concat "cmake-ide [%s]: " str) (cons (current-time-string) vars))))

(defun cide--register-callback ()
  "Register callback for when CMake finishes running."
  (cide--register-a-callback
   (lambda (process _event)
     (cide--message "Finished running CMake")
     (if (= 0 (process-exit-status process)) ; only perform post cmake operation on success.
         (cide--on-cmake-finished)
       (cide--message "CMake failed, see *cmake* for details."))
     (setq cmake-sentinel-flag nil)
     (setq cmake-temp-project-dir nil))))

(defun cide--register-a-callback (callback)
  "Register CALLBACK to be called when CMake finishes running."
  (set-process-sentinel (get-process "cmake") callback))

(defun cide--on-cmake-finished ()
  "Set compiler flags for all buffers that requested it."
  (let* ((idb (cide--cdb-json-file-to-idb))
         (set-flags (lambda (x) (cide--set-flags-for-file idb x))))
    (mapc set-flags cide--src-buffers)
    (mapc set-flags cide--hdr-buffers)
    (setq cide--src-buffers nil cide--hdr-buffers nil)
    (cide--run-rc)))


;;;###autoload
(defun cmake-ide-load-db ()
  "Load compilation DB and set flags for current buffer."
  (interactive)
  (if (not cmake-sentinel-flag)
    (when (cide--locate-project-dir)
      (cide--message "cmake-ide-load-db for file %s" (buffer-file-name))
      (cmake-ide-maybe-start-rdm)
      (let* ((file-name buffer-file-name)
             (buffers (list (current-buffer)))
             (cide--src-buffers (if (cide--is-src-file file-name) buffers nil))
             (cide--hdr-buffers (if (cide--is-src-file file-name) nil buffers)))
        (cide--on-cmake-finished)))
    (cide--message "cmake is running, skip run.")))

(defvar cide--rdm-executable nil
  "Rdm executable location path.")

(defun cmake-ide-rdm-executable ()
  "Return rdm executable location path."
  (cond (cide--rdm-executable cide--rdm-executable)
        ((file-exists-p cmake-ide-rdm-executable)
         (setq cide--rdm-executable cmake-ide-rdm-executable)
         cide--rdm-executable)
        ((featurep 'rtags)
         (setq cide--rdm-executable (rtags-executable-find "rdm"))
         cide--rdm-executable)
        (t "rdm")))


(defun cide--run-rc ()
  "Run rc to add definitions to the rtags daemon."
  (when (featurep 'rtags)
    (cmake-ide-maybe-start-rdm)
    (cide--message "Running rc for rtags")
    ;; change buffer so as to not insert text into a working file buffer
    (let ((cmake-ide-local-build-dir (cide--build-dir)))
      (if (get-process "rdm")
          (with-current-buffer (get-buffer cmake-ide-rdm-buffer-name)
            (rtags-call-rc "-J" cmake-ide-local-build-dir))
        (with-temp-buffer
          (rtags-call-rc "-J" cmake-ide-local-build-dir))))))


(defun cide--set-flags-for-file (idb buffer)
  "Set the compiler flags from IDB for BUFFER visiting file FILE-NAME."
  (let* ((file-name (cide--get-buffer-file-name buffer))
         (file-params (cide--idb-file-to-obj idb file-name))
         (sys-includes (cide--params-to-sys-includes file-params))
         (all-commands (cide--idb-all-commands idb))
         (hdr-flags (cide--commands-to-hdr-flags all-commands)))
    (cide--message "Setting flags for file %s" file-name)
    ;; set flags for all source files that registered
    (if (cide--is-src-file file-name)
        (cide--set-flags-for-src-file file-params buffer sys-includes)
      (cide--set-flags-for-hdr-file idb buffer (cide--flags-to-sys-includes hdr-flags)))))

(defun cide--get-buffer-file-name (buffer)
  "Get the name of a file for a given buffer."
  (let ((file-name (buffer-file-name buffer)))
    (cide--get-system-filename file-name)))

(defun cide--get-system-filename (file-name)
  "Get the file name considering case sensitivity of the system."
  (if (and file-name (eq system-type 'windows-nt))
      (s-downcase file-name)
    file-name))

(defun cide--set-flags-for-src-file (file-params buffer sys-includes)
  "Set the compiler flags from FILE-PARAMS for source BUFFER with SYS-INCLUDES."
  (let* ((src-flags (cide--params-to-src-flags file-params))
         (src-includes (cide--params-to-src-includes file-params)))
    (cmake-ide-set-compiler-flags buffer src-flags src-includes sys-includes)))

(defun cide--set-flags-for-hdr-file (idb buffer sys-includes)
  "Set the compiler flags from IDB for header BUFFER with SYS-INCLUDES."
  (when (and (not (string-empty-p (cide--buffer-string buffer))) (not cmake-ide-header-no-flags))
    (cond
     ;; try all unique compiler flags until one successfully compiles the header
     (cmake-ide-try-unique-compiler-flags-for-headers (cide--hdr-try-unique-compiler-flags idb buffer sys-includes))
     ;; ask ninja or make depending on what the user chose for the flags to use on the header
     ((cide--hdr-ask-ninja-and-make idb buffer sys-includes) t)
     ;; the default algorithm used so far
     (t (cide--hdr-legacy idb buffer sys-includes)))))

(defun cide--buffer-string (buffer)
  "Return the contents of BUFFER as a string."
  (with-current-buffer buffer
    (buffer-string)))

(defun cide--hdr-try-unique-compiler-flags (idb buffer sys-includes)
  "Try all unique compiler flags in IDB in an attempt to find appropriate flags for header file in BUFFER using SYS-INCLUDES."
  (let ((hdr-flags) (hdr-includes))
    (setq hdr-flags (cide--idb-hdr-compiler-args idb (buffer-file-name buffer)))
    (setq hdr-flags (cide--remove-compiler-from-args-string hdr-flags))
    (setq hdr-includes (cide--flags-to-includes hdr-flags))
    (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))

(defun cide--hdr-ask-ninja-and-make (idb buffer sys-includes)
  "Try to get compiler flags from IDB from a source file that depends on the header BUFFER using SYS-INCLUDES."
  (let ((ninja-hdr-command (cide--ninja-header-command idb (buffer-file-name buffer))))
    (if ninja-hdr-command
        (progn
          (cide--set-flags-for-hdr-exact buffer sys-includes ninja-hdr-command)
          (cide--message "Setting flags for %s from ninja dependency information" (buffer-file-name buffer))
          t) ;; has done something
      nil)))

(defun cide--hdr-legacy (idb buffer sys-includes)
  "Try to set compiler flags from IDB for header BUFFER using SYS-INCLUDES.

First, try to find a source file corresponding to the header.
Then, try to find a source file in IDB that directly includes the header.
If all else fails, use all compiler flags in the project."

  (let* ((other (cide--src-file-for-hdr buffer))
         (src-file-name (or other (cide--first-including-src-file idb buffer))))
    (if src-file-name
        ;; if a source file is found, use its flags
        (cide--set-flags-for-hdr-from-src idb buffer sys-includes src-file-name)
      ;; otherwise use flags from all source files
      (cide--set-flags-for-hdr-from-all-flags idb buffer sys-includes))))

(defun cide--set-flags-for-hdr-exact (buffer sys-includes command)
  "Set flags for BUFFER using SYS-INCLUDES and compiler COMMAND."
  (let* ((hdr-flags (cide--remove-compiler-from-args-string command))
         (hdr-includes (cide--flags-to-includes hdr-flags)))
    (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))

(defun cide--ninja-header-command (idb file-name)
  "Return the command used by a file in IDB that depends on FILE-NAME.

Find an object file that lists FILE-NAME as a dependency, then return the first
compiler command in the project that has that object file in itself."
  (let ((obj-file-name (cide--ninja-obj-file-depending-on-hdr file-name)))
    (if (null obj-file-name) nil
      (let ((all-commands (cide--idb-all-commands idb)))
        (cide--filter-first (lambda (x) (string-match obj-file-name x))
                                 all-commands)))))

(defun cide--ninja-obj-file-depending-on-hdr (file-name)
  "Find the first object file that depends on the header FILE-NAME.

Ask ninja for all dependencies then find FILE-NAME in the output, returning
the object file's name just above."
  (let ((default-directory (cide--build-dir))
        (beg)
        (end))
    (if (not (file-exists-p (expand-file-name "build.ninja" default-directory)))
        nil
      (with-temp-buffer
        (call-process cmake-ide-ninja-command nil t nil "-C" default-directory "-t" "deps")
        (goto-char (point-min))
        (setq beg (search-forward file-name nil t))
        (if (null beg)
            nil
          (search-backward "#deps")
          (setq beg (move-beginning-of-line nil))
          (setq end (1- (search-forward ":")))
          (copy-region-as-kill beg end)
          (car kill-ring))))))

(defun cide--src-file-for-hdr (buffer)
  "Try and find a source file for a header BUFFER (e.g. foo.cpp for foo.hpp)."
  (if cmake-ide-header-search-other-file
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (let ((other-file-name (ff-find-the-other-file)))
            (if other-file-name (expand-file-name other-file-name) nil))))
    nil))

(defun cide--set-flags-for-hdr-from-src (idb buffer sys-includes src-file-name)
  "Use IDB to set flags for a header BUFFER with SYS-INCLUDES from its corresponding SRC-FILE-NAME."
  (cide--message "Found src file %s for %s, using its flags" src-file-name (buffer-file-name buffer))
  (cide--set-flags-for-src-file (cide--idb-file-to-obj idb src-file-name) buffer sys-includes))

(defun cide--first-including-src-file (idb buffer)
  "Use IDB to find first source file that includes the header BUFFER."
  (when (and (buffer-file-name buffer) cmake-ide-header-search-first-including)
    (cide--message "Searching for source file including %s" (buffer-file-name buffer))
    (let* ((file-name (buffer-file-name buffer))
           ret-obj
           ret-file-name)

      (when (featurep 'rtags)
        (setq ret-file-name
              (with-temp-buffer
                (rtags-call-rc "--dependencies" file-name "included-by" :noerror t)
                (cide--filter-first
                 (lambda (a)
                   (gethash a idb))
                 (split-string (buffer-string) "\n" t split-string-default-separators)))))

      (unless ret-file-name
        (setq idb (cide--idb-sorted-by-file-distance idb file-name))
        (setq ret-obj (cide--filter-first
                       (lambda (x) (cide--idb-obj-depends-on-file x file-name))
                       idb))
        (when ret-obj (setq ret-file-name (cide--idb-obj-get ret-obj 'file))))

      (when ret-file-name (cide--message "Found a source file including %s" file-name))

      ret-file-name)))

(defun cide--read-file (path)
  "Return PATH's file content as a string."
  (if (stringp path)
      (if (file-exists-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))
        "")
    (cide--message "cide--read-file ERROR: %s is not a string" path)))

(defun cide--set-flags-for-hdr-from-all-flags (idb buffer sys-includes)
  "Use IDB to set flags from a header BUFFER with SYS-INCLUDES from all project source files."
  (cide--message "Could not find suitable src file for %s, using all compiler flags" (buffer-file-name buffer))
  (let* ((all-commands (cide--idb-all-commands idb))
         (hdr-flags (cide--commands-to-hdr-flags all-commands))
         (hdr-includes (cide--commands-to-hdr-includes all-commands)))
    (cmake-ide-set-compiler-flags buffer hdr-flags hdr-includes sys-includes)))


(defun cmake-ide-set-compiler-flags (buffer flags includes sys-includes)
  "Set ac-clang and flycheck variables for BUFFER from FLAGS, INCLUDES and SYS-INCLUDES."
  ;; FLAGS is a list of strings
  (when (buffer-live-p buffer)
    (with-current-buffer buffer

      (when (featurep 'auto-complete-clang)
        (make-local-variable 'ac-clang-flags)
        (setq ac-clang-flags (cide--filter-ac-flags (cide--get-compiler-flags flags))))

      (when (featurep 'company)
        (make-local-variable 'company-clang-arguments)
        (setq company-clang-arguments (cide--filter-ac-flags (cide--get-compiler-flags flags))))

      (when (featurep 'company-c-headers)
        (make-local-variable 'company-c-headers-path-user)
        (setq company-c-headers-path-user (cide--flags-to-include-paths flags))
        (make-local-variable 'company-c-headers-path-system)
        (when sys-includes
          (setq company-c-headers-path-system (append sys-includes company-c-headers-path-system))))

      (when (and (featurep 'irony) (not (gethash (cide--build-dir) cide--cache-irony-dirs)))
        (irony-cdb-json-add-compile-commands-path (cide--locate-project-dir) (cide--comp-db-file-name))
        (puthash (cide--build-dir) t cide--cache-irony-dirs))

      (when (featurep 'semantic)
        (let ((dirs (cide--flags-to-include-paths flags)))
          (when (boundp 'cide--semantic-system-include)
            (mapc 'semantic-remove-system-include cide--semantic-system-include))
          (mapc 'semantic-add-system-include dirs)
          (setq-local cide--semantic-system-include dirs)))


      (let ((macro-regex "\\(^-std=\\|\\.o$\\|^-o$\\)"))
        (make-local-variable 'c-macro-cppflags)
        (setq c-macro-cppflags
              (mapconcat 'identity (cide--filter (lambda (x) (not (string-match macro-regex x)))
                                                 (cide--filter-ac-flags (cide--get-compiler-flags flags))) " ")))

      (when (featurep 'flycheck)
        (let* ((std-regex "^-std=")
               (include-path (append sys-includes (cide--flags-to-include-paths flags)))
               (definitions (append (cide--get-existing-definitions) (cide--flags-to-defines flags)))
               (args (cide--filter (lambda (x) (not (string-match std-regex x))) (cide--flags-filtered (cide--get-compiler-flags flags)))))
          (make-local-variable 'flycheck-clang-include-path)
          (make-local-variable 'flycheck-gcc-include-path)
          (setq flycheck-clang-include-path include-path)
          (setq flycheck-gcc-include-path include-path)

          (make-local-variable 'flycheck-clang-definitions)
          (make-local-variable 'flycheck-gcc-definitions)
          (setq flycheck-clang-definitions definitions)
          (setq flycheck-gcc-definitions definitions)

          (make-local-variable 'flycheck-clang-args)
          (make-local-variable 'flycheck-gcc-args)
          (setq flycheck-clang-args args)
          (setq flycheck-gcc-args (cide--filter-output-arg args))

          (setq flycheck-clang-tidy-build-path (cide--comp-db-file-name))

          (make-local-variable 'flycheck-clang-language-standard)
          (make-local-variable 'flycheck-gcc-language-standard)
          (let* ((stds (cide--filter (lambda (x) (string-match std-regex x)) flags))
                 (repls (mapcar (lambda (x) (replace-regexp-in-string std-regex "" x)) stds)))
            (when repls
              (setq flycheck-clang-language-standard (car repls))
              (setq flycheck-gcc-language-standard (car repls))
              (unless cmake-ide-flycheck-cppcheck-strict-standards
                (setq repls (mapcar 'cide--cmake-standard-to-cppcheck-standard repls)))
              (setq repls (cide--filter 'cide--valid-cppcheck-standard-p repls))
              (when repls
                (make-local-variable 'flycheck-cppcheck-standards)
                (setq flycheck-cppcheck-standards repls))))

          (make-local-variable 'flycheck-cppcheck-include-path)
          (setq flycheck-cppcheck-include-path (append sys-includes (cide--flags-to-include-paths flags))))

        (setq flycheck-clang-includes includes)
        (setq flycheck-gcc-includes includes)
        (flycheck-clear)
	(when (bound-and-true-p flycheck-mode)
          (run-at-time "0.5 sec" nil 'flycheck-buffer))))))

(defun cmake-ide-delete-file ()
  "Remove file connected to current buffer and kill buffer, then run CMake."
  (interactive)
  (if (not cmake-sentinel-flag)
    (when (cide--locate-project-dir)
      (if (cide--build-dir)
          (let ((filename (buffer-file-name))
                (buffer (current-buffer))
                (name (buffer-name)))
            (if (not (and filename (file-exists-p filename)))
                (error "Buffer '%s' is not visiting a file!" name)
              (when (yes-or-no-p "Are you sure you want to remove this file? ")
                (delete-file filename)
                (kill-buffer buffer)
                (let ((project-dir (cide--locate-project-dir)))
                  (when (and project-dir  (file-exists-p (expand-file-name "CMakeLists.txt" project-dir)))
                    (cide--run-cmake-impl project-dir (cide--build-dir)))
                  (cide--message "File '%s' successfully removed" filename)))))
        (error "Not possible to delete a file without setting cmake-ide-build-dir")))
    (cide--message "cmake is running, skip run.")))


(defun cide--run-cmake-impl (project-dir cmake-dir)
  "Run the CMake process for PROJECT-DIR in CMAKE-DIR."
  (when project-dir
    (let ((default-directory cmake-dir))
      (cide--message "Running cmake for src path %s in build path %s" project-dir cmake-dir)
      (apply 'start-process (append (list "cmake" "*cmake*" cmake-ide-cmake-command)
                                    (cide--cmake-args)
                                    (list "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                          "-B" "." "-S" project-dir))))))


(defun cide--project-key ()
  "Return a unique key for a project based on the project dir and cmake options."
  (let ((project-dir (cide--locate-project-dir)))
    (when project-dir
      ;; if no project-dir, then get-project-key is called from a non cmake project dir, simply ignore
      (replace-regexp-in-string "[-/= ]" "_"  (concat (expand-file-name project-dir)
                                                      (string-join (cide--cmake-args) " "))))))

(defun cide--cmake-args ()
  "Return a list of arguments to pass to CMake when calling it."
  (or cmake-ide-cmake-args (split-string cmake-ide-cmake-opts)))

(defun cide--build-dir ()
  "Return the directory name to run CMake in."
  (let ((build-dir-base
         (or (cide--build-dir-var) ; if use set, use this value (may be relative)
             (cide--build-dir-from-cache)))) ; else get from project-key (return an absolute path)
    (when build-dir-base
      (let ((build-dir (expand-file-name  build-dir-base
                                          (cide--locate-project-dir)))) ; if relative, use project-dir as base directory
        (when (not (file-accessible-directory-p build-dir))
          (cide--message "Making directory %s" build-dir)
          (make-directory build-dir 't))
        (file-name-as-directory build-dir)))))

(defun cide--build-dir-from-cache ()
  "Get the build dir from the cache if there or compute if not.
Return nil for non-CMake project."
  (let ((project-key (cide--project-key)))
    (when project-key
      (let ((build-dir (gethash project-key cide--cache-pkey-to-dir nil)))
        (or build-dir
            (let ((build-parent-directory (or cmake-ide-build-pool-dir temporary-file-directory))
                  (build-directory-name (if cmake-ide-build-pool-use-persistent-naming
                                            project-key
                                          (make-temp-name "cmake"))))
              (setq build-dir (expand-file-name build-directory-name build-parent-directory))
              (progn
                (puthash project-key build-dir cide--cache-pkey-to-dir))
              build-dir))))))


(defun cide--is-src-file (name)
  "Test if NAME is a source file or not."
  (cl-some (lambda (x) (string-suffix-p x name)) cmake-ide-src-extensions))


(defun cide--filter (pred seq)
  "Apply PRED to filter SEQ."
  (delq nil
        (mapcar (lambda (x) (and (funcall pred x) x)) seq)))

(defun cide--filter-first (pred seq)
  "Return first element to satisfy PRED in SEQ."
  (let ((index 0) (ret))
    (while (and (null ret) (< index (length seq)))
      (when (funcall pred (elt seq index))
        (setq ret (elt seq index)))
      (cl-incf index))
    ret))


(defun cide--filter-params (file-params filter-func)
  "Filter FILE-PARAMS with FILTER-FUNC."
  ;; The compilation database is a JSON array of JSON objects
  ;; Each object is a file with directory, file and command fields
  ;; Depending on FILTER-FUNC, it maps file names to desired compiler flags
  ;; An example would be -I include flags
  (let* ((args (cide--file-params-to-args file-params))
         (flags (funcall filter-func args)))
    (mapconcat 'identity flags " ")))

(defun cide--file-params-to-args (file-params)
  "Get the compiler command arguments from FILE-PARAMS."
  ;; file-params is also known as an object in the codebase
  (let ((command (cide--idb-obj-get file-params 'command))
        (arguments (cide--idb-obj-get file-params 'arguments)))
    (cide--resolve-response-file
     (if command
         (mapcar #'cide--quote-if-spaces (cide--split-command command))
       (cide--vector-to-list arguments)))))

(defun cide--resolve-response-file (argument-list)
  "Matches response file string and adds its content to the object parameters."
  (-flatten (mapcar #'cide--replace-response-file argument-list)))

(defun cide--replace-response-file (argument)
  "Matches a response file in string ARGUMENT returning a list of arguments"
  (if (not (stringp argument))
      argument
    (if (string-match "@[^[:space:]]+" argument)
        (let* ((response-file (substring argument 1))
               (file-params (cide--get-file-params response-file)))
          (mapcar #'cide--quote-if-spaces (cide--split-command file-params)))
      argument)))

(defun cide--get-file-params (response-file)
  "Get file parameters from a response file given as compilation argument."
  (replace-regexp-in-string "\\\n" " " (cide--read-file (expand-file-name response-file (cide--build-dir)))))

(defun cide--quote-if-spaces (str)
  "Add quotes to STR if it has spaces."
  (if (string-match-p " " str)
      (concat "\"" str "\"")
    str))

(defun cide--vector-to-list (vector)
  "Convert VECTOR to a list."
  (append vector nil))

(defun cide--args-to-only-flags (args)
  "Get compiler flags from ARGS."
  (cide--filter (lambda (x) (not (cide--is-src-file (cide--unquote x)))) args))

(defun cide--unquote (x)
  "Possibly unquote a string X."
  (if (and (stringp x) (> (length x) 2))
      (if (and (equal (elt x 0) ?\") (equal (elt x (1- (length x))) ?\"))
          (cl-subseq x 1 (1- (length x)))
        x)
    x))

(defun cide--json-unescape (str)
  "Remove JSON-escaped backslashes in STR."
  (let* ((no-double-backslashes (replace-regexp-in-string "\\\\\\\\" "\\\\" str))
         (no-backslash-quote (replace-regexp-in-string "\\\\\"" "\"" no-double-backslashes)))
    no-backslash-quote))

(defun cide--params-to-src-flags (file-params &optional filter-func)
  "Source compiler flags for FILE-PARAMS using FILTER-FUNC."
  (if (not file-params) nil
    (let* ((filter-func (or filter-func #'cide--args-to-only-flags))
           (value (cide--filter-params file-params filter-func))
           (flags-string (if value value nil)))
      (if flags-string (cide--cleanup-flags-str flags-string) nil))))

(defun cide--cleanup-flags-str (str)
  "Clean up and filter STR to yield a list of compiler flags."
  (let ((unescaped-flags-string (cide--json-unescape str)))
    (cide--remove-compiler-from-args-string unescaped-flags-string)))

(defun cide--remove-compiler-from-args-string (str)
  "Remove the compiler command from STR, leaving only arguments.  Return a list of strings."
  (let ((args (cide--split-command str)))
    (cide--remove-compiler-from-args args)))

(defun cide--remove-compiler-from-args (args)
  "Remove the compiler command from ARGS, leaving only arguments."
  (if (string-suffix-p "ccache" (car args))
      (cddr args)
    (cdr args)))


(defun cide--filter-ac-flags (flags)
  "Filter unwanted compiler arguments out from FLAGS."
  (cide--filter
   (lambda (x)
     (cl-loop for flag in cmake-ide-ac-flags-to-filter
              never (string-match (format "^%s$" flag) x)))
   flags))

(defun cide--delete-dup-hdr-flags (flags)
  "Delete duplicates in FLAGS for header files."
  (let* ((rest (cide--flags-filtered flags))
         (dashes (cide--filter #'cide--dash-i-or-dash-d-p flags)))
    (append (delete-dups dashes) rest)))

(defun cide--commands-to-hdr-flags (commands)
  "Header compiler flags from COMMANDS."
  (let* ((args (cide--flatten (mapcar #'cide--remove-compiler-from-args-string commands)))
         (flags (cide--args-to-only-flags args)))
    (setq flags (cide--filter (lambda (x) (not (equal x "-o"))) flags))
    (setq flags (cide--filter (lambda (x) (not (string-suffix-p ".o" x))) flags))
    (setq flags (cide--filter (lambda (x) (not (string-suffix-p ".obj" x))) flags))
    (cide--delete-dup-hdr-flags flags)))

(defun cide--params-to-src-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (cide--flags-to-includes (cide--params-to-src-flags file-params 'identity)))


(defun cide--params-to-sys-includes (file-params)
  "-include compiler flags for from FILE-PARAMS."
  (cide--flags-to-sys-includes (cide--params-to-src-flags file-params 'identity)))


(defun cide--commands-to-hdr-includes (commands)
  "Header `-include` flags from COMMANDS."
  (let ((args (cide--flatten (mapcar #'cide--remove-compiler-from-args-string commands))))
    (delete-dups (cide--flags-to-includes args))))


(defun cide--flatten (lst)
  "Flatten LST."
  (if (equal (length lst) 1)
      (elt lst 0)
    (apply #'append lst)))


(defun cide--flags-to-include-paths (flags)
  "From FLAGS (a list of flags) to a list of include paths."
  (let ((raw-paths (cide--to-simple-flags flags "^-I")))
    (mapcar (lambda (x) (expand-file-name x (cide--build-dir))) raw-paths)))

(defun cide--flags-to-defines (flags)
  "From FLAGS (a list of flags) to a list of defines."
  (cide--to-simple-flags flags "^-D"))

(defun cide--to-simple-flags (flags flag)
  "A list of either directories or defines from FLAGS depending on FLAG."
  (let* ((case-fold-search nil)
         (res-flags (cide--filter
                     (lambda (x)
                       (let ((match (string-match flag x)))
                         (and match (zerop match))))
                     flags)))
    (mapcar (lambda (x) (replace-regexp-in-string flag "" x)) res-flags)))


(defun cide--flags-to-includes (flags)
  "From FLAGS (a list of flags) to a list of includes."
  (let ((includes nil))
    (while (member "-include" flags)
      (setq flags (cdr (member "-include" flags)))
      (when flags (setq includes (cons (car flags) includes))))
    includes))

(defun cide--flags-to-sys-includes (flags)
  "From FLAGS (a list of flags) to a list of isystem includes."
  (let ((sysincludes nil))
    (while (member "-isystem" flags)
      (setq flags (cdr (member "-isystem" flags)))
      (when flags
        (if (member (car flags) sysincludes)
            nil
          (setq sysincludes (cons (car flags) sysincludes)))))
    sysincludes))


(defun cide--dash-i-or-dash-d-p (flag)
  "If FLAG is -I or -D."
  (let* ((case-fold-search nil)
         (imatch (string-match "^-I" flag))
         (dmatch (string-match "^-D" flag)))
    (or imatch dmatch)))

(defun cide--flags-filtered (flags)
  "Filter out defines and includes from FLAGS."
  (cide--filter (lambda (x) (not (cide--dash-i-or-dash-d-p x))) flags))


(defun cide--get-compiler-flags (flags)
  "Use FLAGS to return all compiler flags including existing ones.
Returns a list of strings."
  (condition-case nil
      ;; Assume flags is a string
      (append (split-string (cide--get-existing-compiler-flags)) flags)
    ;; On exception
    (wrong-type-argument
     ;; Treat as list of strings.
     (append (cide--get-existing-compiler-flags) flags))))

(defun cide--get-existing-compiler-flags ()
  "Return existing ac-clang flags for this mode, if set.
Returns a string.
"
  (if (eq major-mode 'c++-mode)
      (cide--symbol-value 'cmake-ide-flags-c++)
    (cide--symbol-value 'cmake-ide-flags-c)))

(defun cide--get-existing-definitions ()
  "Return existing compiler defines, if set."
  (cide--symbol-value 'cmake-ide-definitions))


(defun cide--symbol-value (sym)
  "Return the value of SYM if bound, nil if not."
  (if (boundp sym) (symbol-value sym) nil))


(defun cide--locate-cmakelists ()
  "Find CMakeLists.txt.

Use CMakeLists.txt in user defined project-dir, or find the topmost
CMakeLists.txt file.  Return nil if not found."
  (if (and (cide--project-dir-var)
           (file-exists-p (expand-file-name "CMakeLists.txt" (cide--project-dir-var))))
      (expand-file-name "CMakeLists.txt" (cide--project-dir-var))
    (let ((cmakelist-dir (cide--topmost-cmakelists default-directory nil)))
      (if cmakelist-dir
          (expand-file-name "CMakeLists.txt" cmakelist-dir)
        nil))))

(defun cide--topmost-cmakelists (dir last-found)
  "Find the topmost CMakeLists.txt from DIR using LAST-FOUND as a 'plan B'."
  (let ((new-dir (locate-dominating-file dir "CMakeLists.txt")))
    (if new-dir
        (cide--topmost-cmakelists (expand-file-name ".." new-dir) new-dir)
      last-found)))

(defun cide--locate-project-dir ()
  "Return the path to the project directory."
  (let ((cmakelists (cide--locate-cmakelists)))
    ;; if project dir is set by the user, use this value.
    (or cmake-temp-project-dir
        (and (cide--project-dir-var) (expand-file-name (cide--project-dir-var)))
        (and cmakelists (file-name-directory cmakelists)) ; else try to use cmakelists dir
        nil ; if no CMakeLists.txt nor project-dir set, return nil and prevent cmake-ide to do anything else
        )))

(defun cide--cdb-json-file-to-idb ()
  "Convert the compile_commands.json CDB to an IDB.
First it checks the cache for previously
computed IDBs, and if none are found actually performs the conversion."
  (let ((idb (cide--cdb-idb-from-cache)))
    (unless idb
      (if (not (file-exists-p (cide--comp-db-file-name)))
          (cide--message "Non-existent compilation DB file %s" (cide--comp-db-file-name))
        (progn
          (cide--message "Converting JSON CDB %s to IDB" (cide--comp-db-file-name))
          (setq idb (cide--cdb-json-string-to-idb (cide--read-file (cide--comp-db-file-name))))
          (puthash (cide--build-dir) idb cide--cache-dir-to-idb)
          (puthash (cide--build-dir) (cide--hash-file (cide--comp-db-file-name)) cide--cache-dir-to-cdb-hash)
          (remhash (cide--build-dir) cide--cache-irony-dirs))))
    idb))

(defun cide--cdb-idb-from-cache ()
  "Return the IDB from the cache unless the JSON CDB has changed."
  (let ((idb (gethash (cide--build-dir) cide--cache-dir-to-idb))
        (cached-hash (gethash (cide--build-dir) cide--cache-dir-to-cdb-hash))
        (current-hash (cide--hash-file (cide--comp-db-file-name))))
    (if (equal cached-hash current-hash)
        idb
      nil)))

(defun cide--hash-file (file-name)
  "Calculate the hash of FILE-NAME."
  (secure-hash 'md5 (cide--read-file file-name)))

(defun cide--cdb-json-string-to-idb (json-str)
  "Tranform JSON-STR into an IDB.

The IDB is hash mapping files to all JSON objects (usually only one) in the CDB."
  (let ((idb (cide--make-hash-table))
        (json (json-read-from-string json-str)))
    ;; map over all the JSON objects in JSON, which is an array of objects (CDB)
    (mapc (lambda (obj)
            (let* ((file (cide--get-system-filename (cide--relativize (cide--idb-obj-get obj 'file))))
                   (objs (gethash file idb)))
              (push obj objs)
              (puthash file objs idb)))
          json)
    idb))

(defun cide--relativize (path)
  "Make PATH relative to the build directory, but only if relative path with dots."
  (if (or (equal path ".") (string-prefix-p ".." path))
      (expand-file-name path (cide--build-dir))
    path))

(defun cide--idb-obj-get (obj key)
  "Get the value in OBJ for KEY."
  (cdr (assoc key obj)))

(defmacro cide--idb-obj-set (obj key value)
  "Take OBJ and set KEY to VALUE."
  `(push (cons ,key ,value) ,obj))

(defun cide--idb-file-to-obj (idb file-name)
  "Get object from IDB for FILE-NAME."
  (car (gethash file-name idb)))

(defun cide--idb-all-commands (idb)
  "A list of all commands in IDB."
  (mapcar (lambda (x) (s-join " " (cide--file-params-to-args x)))
          (cide--idb-all-objs idb)))


(defun cide--idb-sorted-by-file-distance (idb file-name)
  "Return a list of IDB entries sorted by their directory's name's distance to FILE-NAME."
  (let ((dir (file-name-directory file-name))
        (ret))

    (setq ret (mapcar (lambda (x) (push `(distance . ,(cide--file-distance dir x)) x)) (cide--idb-all-objs idb)))

    (seq-sort
     (lambda (x y) (< (cide--idb-obj-get x 'distance)
                      (cide--idb-obj-get y 'distance)))
     ret)))

(defun cide--file-distance (dir object)
  "Return the distance between DIR and OBJECT's file."
  (levenshtein-distance dir (file-name-directory (cide--idb-obj-get object 'file))))

(defun cide--idb-all-objs (idb)
  "Return a list of IDB entries."
  (let ((ret))
    (maphash (lambda (_ objs) (setq ret (append ret objs))) idb)
    ret))


(defun cide--idb-obj-depends-on-file (obj file-name)
  "If OBJ is a source file that depends on FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
         (src-file-name (cide--idb-obj-get obj 'file)))
    (if (string-match (concat "# *include +[\"<] *" base-name)
                      (cide--read-file src-file-name))
        src-file-name
      nil)))

(defun cide--idb-hdr-compiler-args (idb file-name)
  "Try every unique compiler command in IDB on FILE-NAME and return the first to succeed."
  (let* ((objects  (cide--idb-sorted-by-file-distance idb file-name))
         (commands (cide--idb-objs-to-unique-commands objects))
         (index 0)
         (ret))
    (while (and (null ret) (< index (length commands)))
      (let* ((tmp-file-name (expand-file-name "tmp.o" (make-temp-file "tryheader" t)))
             (command (concat (elt commands index) " " file-name " " "-o" " " tmp-file-name))
             (_ (cide--message "Trying to compile '%s' with '%s'" file-name command))
             (args (cide--split-command command)))
        (when (eq 0 (apply #'call-process (car args) nil nil nil (cdr args)))
          (setq ret command)))
      (cl-incf index))
    ret))


(defun cide--idb-objs-to-unique-commands (objects)
  "Calculate the list of unique compiler commands in OBJECTS ignoring the source file name."
  (let ((ret (mapcar (lambda (x)
                       (let* ((file (cide--idb-obj-get x 'file))
                              (base-name (file-name-nondirectory file))
                              (command (cide--idb-obj-get x 'command))
                              (args (cide--split-command command)))
                         (setq args (cide--filter (lambda (x) (not (string-match base-name x))) args))
                         (setq args (cide--filter (lambda (x) (not (equal x "-c"))) args))
                         (setq args (cide--filter (lambda (x) (not (equal x "-o"))) args))
                         (mapconcat 'identity args " ")))
                     objects)))
    (delete-dups ret)
    ret))

(defun cide--split-command (command-string)
  "Split COMMAND-STRING and return a list of strings."
  (let (word-break-p parse-word split)
    (setq word-break-p (lambda (str escaped quoted)
                         "True if str is considered a word break"
                         (or (equal str "")
                             (and (not escaped) (not quoted) (equal (substring str 0 1) " "))
                             (and quoted (not escaped) (equal (substring str 0 1) "\"")))))

    (setq parse-word (lambda (str quoted)
                       "Parse a word (if quoted is true then the word is quoted)"
                       (let ((word "") escaped)
                         (if quoted
                             (setq str (substring str 1 nil))
                           ())
                         (while (not (funcall word-break-p str escaped quoted))
                           (if (or escaped (not (equal (substring str 0 1) "\\")))
                               (setq word (concat word (substring str 0 1)))
                             ())
                           (setq escaped (and (not escaped) (equal (substring str 0 1) "\\")))
                           (setq str (substring str 1 nil)))
                         (cond
                          ((and quoted (not (equal str "")) (equal (substring str 0 1) "\""))
                           (setq str (substring str 1 nil)))
                          (quoted (error "\"%s\" contains a odd number of not escaped quotes (\")" command-string)))
                         (list word str))))

    (setq split (lambda (str)
                  "Splits a string into a list of commands"
                  (let ((words '()))
                    (while (not (equal str ""))
                      (if (equal (substring str 0 1) " ")
                          (setq str (substring str 1 nil))
                        (let ((word-and-rest (funcall parse-word str (equal (substring str 0 1) "\""))))
                          (push (car word-and-rest) words)
                          (setq str (nth 1 word-and-rest)))))
                    (reverse words))))

    (funcall split (replace-regexp-in-string "\\\\\"" "\"" command-string))))

;;;###autoload
(defun cmake-ide-compile ()
  "Compile the project."
  (interactive)
  (if (not cmake-sentinel-flag)
    (when (cide--locate-project-dir)
      (if (cide--build-dir)
          (let ((compile-command (cide--get-compile-command (cide--build-dir))))
            ;; compile-command could be nil, if so prompt for compile command (i.e. in a non-cmake project ...)
            (if compile-command
                (if (functionp compile-command)
                    (funcall compile-command)
                  (compile compile-command))
              (call-interactively compile-command)))
        (call-interactively compile-command))
      (cide--run-rc))
    (cide--message "cmake is running, skip run.")))


(defun cide--get-compile-command (dir)
  "Return the compile command to use for DIR."
  (cond (cmake-ide-compile-command cmake-ide-compile-command)
        ((file-exists-p (expand-file-name "build.ninja" dir)) (concat cmake-ide-ninja-command " -C " (shell-quote-argument dir)))
        ((file-exists-p (expand-file-name "Makefile" dir)) (concat cmake-ide-make-command " -C " (shell-quote-argument dir)))
        (t nil)))


;;;###autoload
(defun cmake-ide-maybe-start-rdm ()
  "Start the rdm (rtags) server."
  (interactive)
  (when (and (featurep 'rtags)
             (or (and (cide--comp-db-file-name) (file-exists-p (cide--comp-db-file-name)))
                 (cide--locate-project-dir)))

    (unless (cide--process-running-p "rdm")
      (let ((buf (get-buffer-create cmake-ide-rdm-buffer-name)))
        (cide--message "Starting rdm server")
        (with-current-buffer buf
          (let ((rdm-process (start-process "rdm" (current-buffer)
                                            (cmake-ide-rdm-executable)
                                            "-c" cmake-ide-rdm-rc-path)))
                                        ; add a small delay before going on, since rdm could take some time to be ready to treat rc commands
            (sleep-for 0.8)
            (set-process-query-on-exit-flag rdm-process nil)))))))


(defun cide--process-running-p (name)
  "If a process called NAME is running or not."
  (or (get-process name) (cide--system-process-running-p name)))

(defun cide--system-process-running-p (name)
  "If a process called NAME is running on the system."
  (let* ((all-args (mapcar (lambda (x) (cdr (assq 'args (process-attributes x)))) (list-system-processes)))
         (match-args (cide--filter (lambda (x) (cide--string-match (concat "\\b" name "\\b") x)) all-args)))
    (not (null match-args))))

(defun cide--string-match (regexp name)
  "Wrap 'string-match' of REGEXP and NAME to make sure we don't pass it a nil string."
  (when name
    (string-match regexp name)))

(defun cide--valid-cppcheck-standard-p (standard)
  "If STANDARD is supported by cppcheck."
  (member standard '("posix" "c89" "c99" "c11" "c++03" "c++11")))

(defun cide--cmake-standard-to-cppcheck-standard (standard)
  "Convert a CMake language STANDARD to the closest supported by cppcheck.
If there is no clear and sensible conversion, the input is
returned unchanged."
  (let ((gnu-replaced (replace-regexp-in-string "gnu" "c" standard)))
    (cond
     ;; Convert "close-enough" matches.
     ((equal gnu-replaced "c90") "c89")
     ((equal gnu-replaced "c++98") "c++03")
     ((equal gnu-replaced "c++0x") "c++03")
     ((equal gnu-replaced "c++14") "c++11")
     ((equal gnu-replaced "c++1y") "c++11")
     ((equal gnu-replaced "c++17") "c++11")
     ((equal gnu-replaced "c++1z") "c++11")
     ;; See if what we have matches cppcheck's capabilities exactly.
     ((cide--valid-cppcheck-standard-p gnu-replaced) gnu-replaced)
     ;; Otherwise, just hand back the original input.
     (t standard))))

(defun cide--filter-output-arg (args)
  "Filter out '-o <output>' from the provided 'args' list."
  (let (result)
      (while args
        (if (string-equal "-o" (car args))
            (setq args (nthcdr 2 args))
          (push (car args) result)
          (setq args (cdr args))))
      (nreverse result)))

(provide 'cmake-ide)
;;; cmake-ide.el ends here
