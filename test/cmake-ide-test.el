;;; cmake-ide-test.el --- Unit tests for cmake-ide.

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

(defvar cide--test-path)
(defvar cide--root-path)
(setq cide--test-path (f-dirname load-file-name))
(setq cide--root-path (f-parent cide--test-path))
(add-to-list 'load-path cide--root-path)

(require 'ert)
(require 'cmake-ide)
(require 'cl-lib)
(require 'auto-complete-clang)
(require 'company)
(require 'company-c-headers)
(require 'flycheck)

(defun equal-lists (lst1 lst2)
  "If LST1 is the same as LST2 regardless or ordering."
  (and (equal (length lst1) (length lst2))
       (null (cl-set-difference lst1 lst2 :test 'equal))))

(defmacro with-non-empty-file (&rest body)
  "Execute BODY in the context of a non-empty file buffer."
  `(with-temp-buffer
     (insert "//text so the file isn't empty")
     ,@body
     ))


(ert-deftest test-json-to-file-params ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"/foo/bar/dir/foo.cpp\"}]")
         (idb (cide--cdb-json-string-to-idb json-str))
         (real-params (cide--idb-file-to-obj idb "/foo/bar/dir/foo.cpp"))
         (fake-params (cide--idb-file-to-obj idb "oops")))
    (should (equal (cide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
    (should (equal (cide--idb-obj-get fake-params 'directory) nil))))

(ert-deftest test-json-to-file-params-case-insensitive ()
  (let ((initial-system-type system-type))
    (make-local-variable system-type)
    (setq system-type 'windows-nt)
    (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"/Foo/Bar/Dir/Foo.cpp\"}]")
           (idb (cide--cdb-json-string-to-idb json-str))
           (real-params (cide--idb-file-to-obj idb "/foo/bar/dir/foo.cpp"))
           (fake-params (cide--idb-file-to-obj idb "oops")))
      (should (equal (cide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
      (should (equal (cide--idb-obj-get fake-params 'directory) nil)))
    (setq system-type initial-system-type)))

(ert-deftest test-params-to-src-flags-1 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -std=c++14 --foo --bar\"},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cide--idb-file-to-obj idb "file1")))
    (should (equal (cide--params-to-src-flags file-params)
                   '("-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar")))))

(ert-deftest test-params-to-src-flags-2 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\",
                  \"command\": \"cmd1 -o file1.c.o -Ifoo -Ibar -std=c++14\"},
                 {\"file\": \"file2.c\",
                  \"command\": \"cmd2 -o file2.c.o foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cide--idb-file-to-obj idb "file2.c")))
    (should (equal (cide--params-to-src-flags file-params)
                   '("-o" "file2.c.o" "foo" "bar" "-g" "-pg" "-Ibaz" "-Iboo" "-Dloo")))))


(ert-deftest test-flags-to-include-paths ()
  (let ((cmake-ide-build-dir "/tmp"))
    (should (equal (cide--flags-to-include-paths '("-Ifoo" "-Ibar")) '("/tmp/foo" "/tmp/bar")))
    (should (equal (cide--flags-to-include-paths '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("/tmp/boo" "/tmp/baz" "/tmp/doo")))))


(ert-deftest test-flags-to-defines ()
  (should (equal (cide--flags-to-defines '("-Ifoo" "-Ibar")) nil))
  (should (equal (cide--flags-to-defines '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("loo"))))

(ert-deftest test-flags-minus-includes-defs ()
  (should (equal (cide--flags-filtered '("-Iinc" "-Ddef" "-F/dir")) '("-F/dir")))
  (should (equal (cide--flags-filtered '("-Iinc" "-Ddef")) nil))
  )


(ert-deftest test-is-src-file ()
  (should (not (eq (cide--is-src-file "foo.c") nil)))
  (should (not (eq (cide--is-src-file "foo.cpp") nil)))
  (should (not (eq (cide--is-src-file "foo.C") nil)))
  (should (not (eq (cide--is-src-file "foo.cxx") nil)))
  (should (not (eq (cide--is-src-file "foo.cc") nil)))
  (should (eq (cide--is-src-file "foo.h") nil))
  (should (eq (cide--is-src-file "foo.hpp") nil))
  (should (eq (cide--is-src-file "foo.hxx") nil))
  (should (eq (cide--is-src-file "foo.H") nil))
  (should (eq (cide--is-src-file "foo.hh") nil))
  (should (eq (cide--is-src-file "foo.d") nil))
  (should (eq (cide--is-src-file "foo.py") nil)))


(ert-deftest test-commands-to-hdr-flags-1 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.h\",
                  \"command\": \"cmd1 -Ifoo -Ibar\"}]"))
         (commands (cide--idb-all-commands idb)))

    (should (equal-lists (cide--commands-to-hdr-flags commands)
                         '("-Ifoo" "-Ibar")))))

(ert-deftest test-commands-to-hdr-flags-2 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.h\",
                  \"command\": \"cmd1 -Ifoo -Ibar\"},
                 {\"file\": \"/dir2/file2.h\",
                  \"command\": \"cmd2 -Iloo -Dboo\"}]"))
         (commands (cide--idb-all-commands idb)))

    (should (equal-lists (cide--commands-to-hdr-flags commands)
                         '("-Ifoo" "-Ibar" "-Iloo" "-Dboo")))))

(ert-deftest test-commands-to-hdr-flags-3 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.c\",
                  \"command\": \"cmd1 -o file1.c.o otherfile -Ifoo -Ibar -weird\"},
                 {\"file\": \"/dir2/file2.c\",
                  \"command\": \"cmd2 -o file2.c.o -Iloo -Dboo -include foo.h\"},
                 {\"file\": \"/dir2/file3.c\",
                  \"command\": \"cmd2 -o file3.c.o -Iloo -Dboo -include bar.h\"}]"))
         (commands (cide--idb-all-commands idb)))
    (should (equal-lists (cide--commands-to-hdr-flags commands)
                         '( "-Ifoo" "-Ibar" "-Iloo" "-Dboo" "otherfile" "-weird" "-include" "foo.h" "-include" "bar.h")))))


(ert-deftest test-params-to-src-includes-1 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
               {\"file\": \"file2\",
                \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (file-params (cide--idb-file-to-obj idb "file1")))

    (should (equal-lists
             (cide--params-to-src-includes file-params)
             '("/foo/bar.h" "a.h")))))

(ert-deftest test-params-to-src-includes-2 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (file-params (cide--idb-file-to-obj idb "file2")))
    (should (equal-lists
             (cide--params-to-src-includes file-params)
             '("h.h")))))

(ert-deftest test-commands-to-hdr-includes-1 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (commands (cide--idb-all-commands idb)))
    (should (equal-lists (cide--commands-to-hdr-includes commands)
                         '("/foo/bar.h" "a.h" "h.h")))))

(ert-deftest test-commands-to-hdr-includes-2 ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (commands (cide--idb-all-commands idb)))
    (should (equal-lists (cide--commands-to-hdr-includes commands)
                         '("/foo/bar.h" "a.h" "h.h")))))

(ert-deftest test-all-vars ()
  (let ((cmake-ide-build-dir "/tmp")
        (idb (cide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"cmd1 -Iinc1 -Iinc2 -Dfoo=bar -S -F -g\"}]")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path '("/tmp/inc1" "/tmp/inc2")))
     (should (equal-lists flycheck-clang-definitions '("foo=bar")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal-lists flycheck-clang-args '("-S" "-F" "-g"))))))

(ert-deftest test-all-vars-ccache ()
  (let ((cmake-ide-build-dir "/tmp")
        (idb (cide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"/usr/bin/ccache clang++ -Iinc1 -Iinc2 -Dfoo=bar -S -F -g -std=c++14\"}]")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F" "-std=c++14")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path '("/tmp/inc1" "/tmp/inc2")))
     (should (equal-lists flycheck-clang-definitions '("foo=bar")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal flycheck-clang-language-standard "c++14"))
     (should (equal-lists flycheck-clang-args '("-S" "-F" "-g"))))))

(ert-deftest test-all-vars-ccache-alt ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"/usr/lib/ccache/bin/clang++ -Iinc1 -Iinc2 -Dfoo=bar -S -F -g -std=c++14\"}]"))
        (cmake-ide-project-dir "/tmp"))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F" "-std=c++14"))))))

(ert-deftest test-idb-obj-get ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"}]"))
         (obj (cide--idb-file-to-obj idb "file1.c")))
    (should (equal (cide--idb-obj-get obj 'foo) "the foo is mighty"))
    (should (equal (cide--idb-obj-get obj 'bar) "the bar is weak"))
    (should (equal (cide--idb-obj-get obj 'oops) nil))))


(ert-deftest test-idb-set-value-on-obj ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"}]"))
         (obj (cide--idb-file-to-obj idb "file1.c")))

    (cide--idb-obj-set obj 'extra "extra stuff is nice too")
    (should (equal (cide--idb-obj-get obj 'extra) "extra stuff is nice too"))))

(ert-deftest test-idb-sort-by-file-distance ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[
                    {\"file\": \"foobar/f.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"},
                    {\"file\": \"dootrain/f.c\", \"foo\": \"the foo is ugly\",   \"bar\": \"the bar is cool\"},
                    {\"file\": \"food/f.c\", \"foo\": \"the foo is just a foo\",   \"bar\": \"what bar?\"}
                ]"))
         (sorted (cide--idb-sorted-by-file-distance idb "foo/h.h")))
    (should (equal (cide--idb-obj-get (elt sorted 0) 'file) "food/f.c"))
    (should (equal (cide--idb-obj-get (elt sorted 0) 'distance) 1))))


(ert-deftest test-same-file-twice ()
  (let* ((idb (cide--cdb-json-string-to-idb
               "[
                    {\"file\": \"foobar/f.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"},
                    {\"file\": \"dootrain/f.c\", \"foo\": \"the foo is ugly\",   \"bar\": \"the bar is cool\"},
                    {\"file\": \"food/f.c\", \"foo\": \"the foo is just a foo\",   \"bar\": \"what bar?\"},
                    {\"file\": \"foobar/f.c\", \"foo\": \"the foo is really mighty\", \"bar\": \"the bar is really weak\"}
                ]"))
         (obj (cide--idb-file-to-obj idb "foobar/f.c")))
    (should (equal (cide--idb-obj-get obj 'foo) "the foo is really mighty"))
    ))

(ert-deftest test-issue-43 ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/build-clang/dune-evolving-domains/src\",
 \"command\": \"/home/csunix/scstr/Software/anaconda/bin/clang++   -DENABLE_MPI=1 -DHAVE_CONFIG_H -DMPICH_SKIP_MPICXX -DMPIPP_H -O3 -Wall -Wno-unused-parameter -std=c++11  -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/build-clang/dune-evolving-domains -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains -I/usr/include/openmpi-x86_64 -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-common -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-geometry -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-grid -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-localfunctions -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-istl -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-alugrid -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-fem    -o CMakeFiles/dune_evolving_domains.dir/dune_evolving_domains.cc.o -c /home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains/src/dune_evolving_domains.cc\",
 \"file\": \"/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains/src/dune_evolving_domains.cc\"
 }
 ]
")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-clang-include-path
                          '("/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/build-clang/dune-evolving-domains" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains" "/usr/include/openmpi-x86_64" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-common" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-geometry" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-grid" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-localfunctions" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-istl" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-alugrid" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-fem")))
     (should (equal-lists flycheck-clang-includes nil))
     )))


(ert-deftest test-issue-45 ()
  (should (equal (cide--is-src-file "foo.cpp") t))
  (should (equal (cide--is-src-file "foo.yyy") nil))
  (should (equal (cide--is-src-file "foo.cu") nil))
  (let ((cmake-ide-src-extensions '(".cu")))
    (should (equal (cide--is-src-file "foo.cu") t))
    (should (equal (cide--is-src-file "foo.cpp") nil))))

(ert-deftest test-only-flags ()
  (should (equal (cide--args-to-only-flags '("foo" "bar" "foo.cxx")) '("foo" "bar"))))

(ert-deftest test-issue-52 ()
  (let ((cmake-ide-build-dir "/usr/bin")
        (idb (cide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/project/build\",
 \"command\": \"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc  -DHAVE_CONFIG_H -DHTTP_PARSER_STRICT -DLUASOCKET_DEBUG -DWITH_COMPAT -DWITH_DOM -DWITH_NOIO -DWITH_OPENSSL -D_REENTRANT -I/usr/local/include -I/usr/local/include/luajit-2.0 -Ijansson-2.7/include -I/usr/local/opt/openssl/include -I/usr/local/include/mysql -I. -I../lib -I../lib/gsoap -I../lib/http-parser -I../lib/uthash -Wall -pedantic -O2 -O3 -DNDEBUG   -o src/ddutil/src/lua/socket/CMakeFiles/LUASOCKET_FILES.dir/options.c.o   -c /Users/user/dev/project/src/ddutil/src/lua/socket/options.c\",
 \"file\": \"/project/src/ddutil/src/lua/socket/options.c\"
 }
 ]
")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-clang-include-path
                          '("/usr/local/include" "/usr/local/include/luajit-2.0" "/usr/bin/jansson-2.7/include" "/usr/local/opt/openssl/include" "/usr/local/include/mysql" "/usr/bin" "/usr/lib" "/usr/lib/gsoap" "/usr/lib/http-parser" "/usr/lib/uthash")))
     )))

(ert-deftest test-json-to-file-params-reldir-issue ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"./foo.cpp\"}]")
         (idb (cide--cdb-json-string-to-idb json-str))
         (real-params (cide--idb-file-to-obj idb "./foo.cpp"))
         (fake-params (cide--idb-file-to-obj idb "oops")))
    (should (equal (cide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
    (should (equal (cide--idb-obj-get fake-params 'directory) nil))))

(ert-deftest test-json-to-file-params-reldir-issue-2 ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"foo.cpp\"}]")
         (idb (cide--cdb-json-string-to-idb json-str))
         (real-params (cide--idb-file-to-obj idb "foo.cpp"))
         (fake-params (cide--idb-file-to-obj idb "oops")))
    (should (equal (cide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
    (should (equal (cide--idb-obj-get fake-params 'directory) nil))))

(ert-deftest test-issue-79 ()
  (let ((cmake-ide-build-dir "/usr/bin")
        (idb (cide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/usr/bin\",
 \"command\": \" g++-6    -I../include   -g -std=c++14 -Wall -Wextra -Werror   -o CMakeFiles/soln.dir/src/Source.cpp.o -c ../src/Source.cpp\",
 \"file\": \"../src/Source.cpp\"
 }
 ]
")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-cppcheck-include-path
                          '("/usr/include")))
     )))

(ert-deftest test-issue-79-2 ()
  (let ((cmake-ide-build-dir "/usr")
        (idb (cide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/usr\",
 \"command\": \" g++-6    -Iinclude   -g -std=c++14 -Wall -Wextra -Werror   -o CMakeFiles/soln.dir/src/Source.cpp.o -c src/Source.cpp\",
 \"file\": \"src/Source.cpp\"
 }
 ]
")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-cppcheck-include-path
                          '("/usr/include")))
     )))

(ert-deftest test-issue-108 ()
  "Check that company-c-headers is set correctly by cmake-ide-set-compiler-flags.
This is a regression test for Issue #108 on github. Previously,
after a call to cmake-ide-set-compiler-flags, the
company-c-headers-path-system variable would have the provided
list of sys-includes consed on the front, causing
company-c-headers to break."
  (with-non-empty-file
   (let ((old-company-c-headers-path-system company-c-headers-path-system))
     (cmake-ide-set-compiler-flags (current-buffer) () () '("/foo" "/bar"))
     (should (equal
              (append '("/foo" "/bar") old-company-c-headers-path-system)
              company-c-headers-path-system)))))

(ert-deftest test-cide--valid-cppcheck-standard-p ()
  "Check that cide--valid-cppcheck-standard-p behaves as expected."
  (let ((valid-standards '("posix" "c89" "c99" "c11" "c++03" "c++11"))
        (invalid-standards '("c90" "c94" "c++98"
                             "c++0x" "c++1y" "c++1z"
                             "c++14" "c++17"
                             "foo" "bar" "baz" "")))
    ;; Check that valid standards satisfy the predicate.
    (mapc
     (lambda (candidate) (should (cide--valid-cppcheck-standard-p candidate)))
     valid-standards)
    ;; Check that invalid standards don't.
    (mapc
     (lambda (candidate) (should-not (cide--valid-cppcheck-standard-p candidate)))
     invalid-standards)))

(ert-deftest test-cide--cmake-standard-to-cppcheck-standard ()
  "Check that cide--cmake-standard-to-cppcheck-standard behaves as expected."
  (let ((valid-standards '("posix" "c89" "c99" "c11" "c++03" "c++11"))
        (convertible-standards '("c90" "c++98" "c++0x" "c++1y" "c++1z" "c++14" "c++17"
                                 "gnu90" "gnu99" "gnu11"
                                 "gnu++98" "gnu++03" "gnu++11" "gnu++14" "gnu++17"
                                 "gnu++0x" "gnu++1y" "gnu++1z"))
        (expected-conversions '("c89" "c++03" "c++03" "c++11" "c++11" "c++11" "c++11"
                                "c89" "c99" "c11"
                                "c++03" "c++03" "c++11" "c++11" "c++11"
                                "c++03" "c++11" "c++11"))
        (inconvertible-standards '("foo" "bar" "baz" "iso199009")))
    ;; Valid standards should be left unchanged.
    (mapc
     (lambda (candidate) (should (equal (cide--cmake-standard-to-cppcheck-standard candidate) candidate)))
     valid-standards)
    ;; Invalid but convertible standards should become valid, and be
    ;; converted to an expected cppcheck-compatible standard.
    (let ((conversions (mapcar 'cide--cmake-standard-to-cppcheck-standard convertible-standards)))
      (mapc (lambda (conversion) (should (cide--valid-cppcheck-standard-p conversion))) conversions)
      (should (equal expected-conversions conversions)))
    ;; Invalid and unconvertible standards should be left unchanged.
    (let ((bad-conversions (mapcar 'cide--cmake-standard-to-cppcheck-standard inconvertible-standards)))
      (should (equal inconvertible-standards bad-conversions)))))

(ert-deftest test-cmake-ide-set-compiler-flags-sets-flycheck-cppcheck-standards ()
  "Check that cmake-ide-set-compiler-flags sets flycheck-cppcheck-standards as expected."
  (let ((saved-strict-standards cmake-ide-flycheck-cppcheck-strict-standards))
    (unwind-protect
        (with-non-empty-file
         ;; If strict-standards is on, then passing a set of compiler
         ;; flags with a cppcheck-invalid -std=* entry should leave
         ;; the flycheck-cppcheck-standards variable untouched.
         (setq cmake-ide-flycheck-cppcheck-strict-standards t)
         (let ((original-standards flycheck-cppcheck-standards))
           (cmake-ide-set-compiler-flags (current-buffer) '("-std=gnu++98") () ())
           (should (equal flycheck-cppcheck-standards original-standards)))

         ;; Passing a flag representing a cppcheck-valid standard
         ;; should produce a change, though.
         (cmake-ide-set-compiler-flags (current-buffer) '("-std=c++03") () ())
         (should (equal flycheck-cppcheck-standards '("c++03")))

         ;; If strict standards are turned off, then passing a
         ;; convertible standard in the flags should produce a change
         ;; as well.
         (setq cmake-ide-flycheck-cppcheck-strict-standards nil)
         (cmake-ide-set-compiler-flags (current-buffer) '("-std=gnu++98") () ())
         (should (equal flycheck-cppcheck-standards '("c++03")))
         (cmake-ide-set-compiler-flags (current-buffer) '("-std=c89") () ())
         (should (equal flycheck-cppcheck-standards '("c89")))))

    ;; Restore pre-test state.
    (setq cmake-ide-flycheck-cppcheck-strict-standards saved-strict-standards)))


(ert-deftest test-get-command-args-with-command ()
  "Check getting command arguments from file-params."
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -std=c++14 --foo --bar\"},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cide--idb-file-to-obj idb "file1"))
         (args (cide--file-params-to-args file-params)))
    (should (equal args '("cmd1" "-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar"))))
  )


(ert-deftest test-get-command-args-with-resolve-file-in-command ()
  "Check if resolve file is read in in case it is used by CMake in command"
  (cl-letf (((symbol-function 'cide--build-dir) #'(lambda () nil)))
    (let* ((temporary-filename (make-temp-file "test-get-command-args-with-resolve-file"))
           (idb (cide--cdb-json-string-to-idb
                 (concat "[{\"file\": \"file1\",
                  \"command\": \"cmd1 " "@" temporary-filename " -Ifoo -Ibar -std=c++14 --foo --bar\"},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]")))
           (file-params (cide--idb-file-to-obj idb "file1")))
      (with-temp-file temporary-filename
        (insert "-fmessage-length=0")
        (end-of-line)
        (newline)
        (insert "-nostdlib")
        (end-of-line)
        (newline))
      (let ((args (cide--file-params-to-args file-params)))
        (should (equal args '("cmd1" "-fmessage-length=0" "-nostdlib" "-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar"))))
      (delete-file temporary-filename)))
  )


(ert-deftest test-get-command-args-with-arguments ()
  "Check getting command arguments from file-params."
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"arguments\": [\"cmd1\", \"-Ifoo\", \"-Ibar\", \"-std=c++14\", \"--foo\", \"--bar\"]},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cide--idb-file-to-obj idb "file1"))
         (args (cide--file-params-to-args file-params)))
    (should (equal args '("cmd1" "-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar"))))
  )


(ert-deftest test-get-command-args-with-resolve-file-in-arguments ()
  "Check if resolve file is read in in case it is used by CMake in argument"
  (cl-letf (((symbol-function 'cide--build-dir) #'(lambda () nil)))
    (let* ((temporary-filename (make-temp-file "test-get-command-args-with-resolve-file"))
           (idb (cide--cdb-json-string-to-idb
                 (concat "[{\"file\": \"file1\",
                  \"arguments\": [\"cmd1\", " "\"@" temporary-filename "\", " "\"-Ifoo\", \"-Ibar\", \"-std=c++14\", \"--foo\", \"--bar\"]},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]")))
           (file-params (cide--idb-file-to-obj idb "file1")))
      (with-temp-file temporary-filename
        (insert "-fmessage-length=0 -nostdlib"))
      (let ((args (cide--file-params-to-args file-params)))
        (should (equal args '("cmd1" "-fmessage-length=0" "-nostdlib" "-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar"))))
      (delete-file temporary-filename)))
  )


(ert-deftest test-all-commands ()
  "Check getting command arguments from file-params."
  (let* ((idb (cide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"arguments\": [\"cmd1\", \"-Ifoo\", \"-Ibar\", \"-std=c++14\", \"--foo\", \"--bar\"]},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (commands (cide--idb-all-commands idb)))
    (should (equal commands '("cmd1 -Ifoo -Ibar -std=c++14 --foo --bar" "cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo"))))
  )


(ert-deftest test-all-vars-arguments ()
  (let ((cmake-ide-build-dir "/tmp")
        (idb (cide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"arguments\": [\"cmd1\", \"-Iinc1\", \"-Iinc2\", \"-Dfoo=bar\", \"-S\", \"-F\", \"-g\"]}]")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path '("/tmp/inc1" "/tmp/inc2")))
     (should (equal-lists flycheck-clang-definitions '("foo=bar")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal-lists flycheck-clang-args '("-S" "-F" "-g")))
     (should (equal-lists flycheck-gcc-include-path '("/tmp/inc1" "/tmp/inc2")))
     (should (equal-lists flycheck-gcc-definitions '("foo=bar")))
     (should (equal-lists flycheck-gcc-includes nil))
     (should (equal-lists flycheck-gcc-args '("-S" "-F" "-g"))))))

(ert-deftest test-issue-125 ()
  (setq cmake-ide-build-dir nil cmake-ide-dir nil)
  (setq cide--cache-pkey-to-dir (make-hash-table :test #'equal))
  (setq cmake-ide-project-dir "/tmp")
  (let ((dir1 (cide--build-dir))
        (dir2 (cide--build-dir)))
    (should (equal dir1 dir2))))

(ert-deftest test-flycheck-clang-args ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[
{
  \"directory\": \"\",
  \"command\": \"clang++ -Wall -Wextra -std=c++14 -c foo.cpp\",
  \"file\": \"foo.cpp\"
}
]"))
        (cmake-ide-build-dir "/tmp"))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal flycheck-clang-args '("-Wall" "-Wextra" "-c"))))))

(ert-deftest test-flycheck-clang-args-for-windows ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[
{
  \"directory\": \"\",
  \"command\": \"clang++ -Wall -Wextra -std=c++14 -c foo.cpp\",
  \"file\": \"foo.cpp\"
}
]"))
        (cmake-ide-build-dir "/tmp")
        (cmake-ide-flags-c '("--target" "i686-pc-windows-gnu")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal flycheck-clang-args '("--target" "i686-pc-windows-gnu" "-Wall" "-Wextra" "-c"))))))

(ert-deftest test-flycheck-gcc-args ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[
{
  \"directory\": \"\",
  \"command\": \"/bin/c++ -Dfoo_cpp_EXPORTS -I/usr/local/include -pipe -m64 -std=c++14 -g -fPIC -o CMakeFiles/hello.dir/foo.cpp.o -c foo.cpp\",
  \"file\": \"foo.cpp\"
}
]"))
        (cmake-ide-build-dir "/tmp"))
    (with-non-empty-file
     (let* ((file-params (cide--idb-file-to-obj idb "foo.cpp"))
            (sys-includes (cide--params-to-sys-includes file-params)))
       (cide--set-flags-for-src-file file-params (current-buffer) sys-includes))
     (should (equal flycheck-gcc-args '("-pipe" "-m64" "-g" "-fPIC" "-c"))))))


(ert-deftest test-cide--filter-output-arg ()
  (should (equal (cide--filter-output-arg '()) '()))
  (should (equal (cide--filter-output-arg '("-fPIC" "-O3" "-march=native" "-Wall")) '("-fPIC" "-O3" "-march=native" "-Wall")))
  (should (equal (cide--filter-output-arg '("-o" "output" "-fPIC" "-O3" "-march=native" "-Wall")) '("-fPIC" "-O3" "-march=native" "-Wall")))
  (should (equal (cide--filter-output-arg '("-o" "output1" "-o" "output2" "-fPIC" "-O3" "-march=native" "-Wall")) '("-fPIC" "-O3" "-march=native" "-Wall")))
  (should (equal (cide--filter-output-arg '("-fPIC" "-O3" "-march=native" "-Wall" "-o" "output")) '("-fPIC" "-O3" "-march=native" "-Wall")))
  (should (equal (cide--filter-output-arg '("-fPIC" "-O3" "-march=native" "-o" "output" "-Wall")) '("-fPIC" "-O3" "-march=native" "-Wall"))))

(ert-deftest test-split-command ()
  (should (equal (cide--split-command "foo \"quux toto\" bar") '("foo" "quux toto" "bar"))))

(ert-deftest test-issue-142 ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[
{
  \"directory\": \"/tmp/name - with hyphen\",
  \"command\": \"/usr/lib/ccache/bin/c++ -Wall -Wextra -pedantic -std=c++14 -o CMakeFiles/hello.dir/main.cpp.o -c \\\"/tmp/name - with hyphen/main.cpp\\\"\",
  \"file\": \"/tmp/name - with hyphen/main.cpp\"
}
]")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     (should (equal flycheck-clang-args '("-Wall" "-Wextra" "-pedantic" "-c"))))))

(ert-deftest test-project-key-basic ()
  (setq cmake-ide-build-dir nil cmake-ide-dir nil)
  (setq cide--cache-pkey-to-dir (make-hash-table :test #'equal))
  (setq cmake-ide-project-dir "./test1")
  ;; two run of get-project-key have to return the same result
  (let ((dir1 (cide--project-key))
        (dir2 (cide--project-key)))
    (should (equal dir1 dir2)))
  (let ((dir1 (cide--project-key)))
    (setq cmake-ide-project-dir "./test2")
    ;; since project-key depend on project-dir, two different dir must have different value
    (let ((dir2 (cide--project-key)))
      (should (not (equal dir1 dir2)))))
  (let ((dir1 (cide--project-key)))
    (setq cmake-ide-cmake-opts "-DTest")
    ;; since project-key depend on cmake-opts, two different dir must have different value
    (let ((dir2 (cide--project-key)))
      (should (not (equal dir1 dir2)))))
  )

(ert-deftest test-build-dir-behavior ()
  (setq cmake-ide-build-dir nil cmake-ide-dir nil)
  (setq cide--cache-pkey-to-dir (make-hash-table :test #'equal))
  (setq cmake-ide-project-dir "./test1")
  (setq cmake-ide-build-pool-dir nil)
  (let ((dir1 (cide--build-dir)))
    (cide--message "dir 1 %s" dir1))


  (setq cmake-ide-build-dir "test-build")
  (setq cide--cache-pkey-to-dir (make-hash-table :test #'equal))
  (setq cmake-ide-project-dir "./test1")
  (setq cmake-ide-build-pool-dir nil)
  (let ((dir1 (cide--build-dir)))
    (cide--message "dir 1 %s" dir1))

  (setq cmake-ide-build-dir "/tmp/test-build")
  (setq cide--cache-pkey-to-dir (make-hash-table :test #'equal))
  (setq cmake-ide-project-dir "./test1")
  (let ((dir1 (cide--build-dir)))
    (cide--message "dir 1 %s" dir1)))

(ert-deftest test-issue-148 ()
  (let ((idb (cide--cdb-json-string-to-idb
              "[
{
    \"command\": \"cc -c -Wp,-MD,tools/.fit_image.o.d -Wall -Wstrict-prototypes -O2 -fomit-frame-pointer -include ./include/libfdt_env.h -idirafterinclude -idirafter./arch/arm/include -I./lib/libfdt -I./tools -DUSE_HOSTCC -D__KERNEL_STRICT_NAMES -D_GNU_SOURCE -DMKIMAGE_DTC=\\\\\\\"dtc\\\\\\\" -o tools/fit_image.o tools/fit_image.c\",
    \"directory\": \"/home/ljj/dev/u-boot\",
    \"file\": \"/home/ljj/dev/u-boot/tools/fit_image.c\"
}
]")))
    (with-non-empty-file
     (cide--set-flags-for-file idb (current-buffer))
     ;; no asserts, it just should not error
     )))


(provide 'cmake-ide-test)
;;; cmake-ide-test.el ends here
