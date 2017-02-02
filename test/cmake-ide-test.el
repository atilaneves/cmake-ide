;;; cmake-ide-test.el --- Unit tests for cmake-ide.

;; Copyright (C) 2014-2016

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

(setq cmake-ide--test-path (f-dirname load-file-name))
(setq cmake-ide--root-path (f-parent cmake-ide--test-path))
(add-to-list 'load-path cmake-ide--root-path)

(require 'ert)
(require 'cmake-ide)
(require 'cl)
(require 'auto-complete-clang)
(require 'company)
(require 'flycheck)

(defun equal-lists (lst1 lst2)
  "If LST1 is the same as LST2 regardless or ordering."
  (and (equal (length lst1) (length lst2))
       (null (set-difference lst1 lst2 :test 'equal))))

(defmacro with-non-empty-file (&rest body)
  "Execute BODY in the context of a non-empty file buffer."
  `(with-temp-buffer
     (insert "//text so the file isn't empty")
     ,@body
     ))


(ert-deftest test-json-to-file-params ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"/foo/bar/dir/foo.cpp\"}]")
         (idb (cmake-ide--cdb-json-string-to-idb json-str))
         (real-params (cmake-ide--idb-file-to-obj idb "/foo/bar/dir/foo.cpp"))
         (fake-params (cmake-ide--idb-file-to-obj idb "oops")))
    (should (equal (cmake-ide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
    (should (equal (cmake-ide--idb-obj-get fake-params 'directory) nil))))


(ert-deftest test-params-to-src-flags-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -std=c++14 --foo --bar\"},
                 {\"file\": \"file2\",
                  \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file1")))
    (should (equal (cmake-ide--params-to-src-flags file-params)
                   '("-Ifoo" "-Ibar" "-std=c++14" "--foo" "--bar")))))

(ert-deftest test-params-to-src-flags-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\",
                  \"command\": \"cmd1 -o file1.c.o -Ifoo -Ibar -std=c++14\"},
                 {\"file\": \"file2.c\",
                  \"command\": \"cmd2 -o file2.c.o foo bar -g -pg -Ibaz -Iboo -Dloo\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file2.c")))
    (should (equal (cmake-ide--params-to-src-flags file-params)
                   '("-o" "file2.c.o" "foo" "bar" "-g" "-pg" "-Ibaz" "-Iboo" "-Dloo")))))


(ert-deftest test-flags-to-include-paths ()
  (let ((cmake-ide-build-dir "/tmp"))
    (should (equal (cmake-ide--flags-to-include-paths '("-Ifoo" "-Ibar")) '("/tmp/foo" "/tmp/bar")))
    (should (equal (cmake-ide--flags-to-include-paths '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("/tmp/boo" "/tmp/baz" "/tmp/doo")))))


(ert-deftest test-flags-to-defines ()
  (should (equal (cmake-ide--flags-to-defines '("-Ifoo" "-Ibar")) nil))
  (should (equal (cmake-ide--flags-to-defines '("-Iboo" "-Ibaz" "-Dloo" "-Idoo")) '("loo"))))

(ert-deftest test-flags-minus-includes-defs ()
  (should (equal (cmake-ide--flags-filtered '("-Iinc" "-Ddef" "-F/dir")) '("-F/dir")))
  (should (equal (cmake-ide--flags-filtered '("-Iinc" "-Ddef")) nil))
  )


(ert-deftest test-is-src-file ()
  (should (not (eq (cmake-ide--is-src-file "foo.c") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.cpp") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.C") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.cxx") nil)))
  (should (not (eq (cmake-ide--is-src-file "foo.cc") nil)))
  (should (eq (cmake-ide--is-src-file "foo.h") nil))
  (should (eq (cmake-ide--is-src-file "foo.hpp") nil))
  (should (eq (cmake-ide--is-src-file "foo.hxx") nil))
  (should (eq (cmake-ide--is-src-file "foo.H") nil))
  (should (eq (cmake-ide--is-src-file "foo.hh") nil))
  (should (eq (cmake-ide--is-src-file "foo.d") nil))
  (should (eq (cmake-ide--is-src-file "foo.py") nil)))


(ert-deftest test-commands-to-hdr-flags-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.h\",
                  \"command\": \"cmd1 -Ifoo -Ibar\"}]"))
         (commands (cmake-ide--idb-param-all-files idb 'command)))

    (should (equal-lists (cmake-ide--commands-to-hdr-flags commands)
                         '("-Ifoo" "-Ibar")))))

(ert-deftest test-commands-to-hdr-flags-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.h\",
                  \"command\": \"cmd1 -Ifoo -Ibar\"},
                 {\"file\": \"/dir2/file2.h\",
                  \"command\": \"cmd2 -Iloo -Dboo\"}]"))
         (commands (cmake-ide--idb-param-all-files idb 'command)))

    (should (equal-lists (cmake-ide--commands-to-hdr-flags commands)
                         '("-Ifoo" "-Ibar" "-Iloo" "-Dboo")))))

(ert-deftest test-commands-to-hdr-flags-3 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"/dir1/file1.c\",
                  \"command\": \"cmd1 -o file1.c.o otherfile -Ifoo -Ibar -weird\"},
                 {\"file\": \"/dir2/file2.c\",
                  \"command\": \"cmd2 -o file2.c.o -Iloo -Dboo -include foo.h\"},
                 {\"file\": \"/dir2/file3.c\",
                  \"command\": \"cmd2 -o file3.c.o -Iloo -Dboo -include bar.h\"}]"))
         (commands (cmake-ide--idb-param-all-files idb 'command)))
    (should (equal-lists (cmake-ide--commands-to-hdr-flags commands)
                         '( "-Ifoo" "-Ibar" "-Iloo" "-Dboo" "otherfile" "-weird" "-include" "foo.h" "-include" "bar.h")))))


(ert-deftest test-params-to-src-includes-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
               {\"file\": \"file2\",
                \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file1")))

    (should (equal-lists
             (cmake-ide--params-to-src-includes file-params)
             '("/foo/bar.h" "a.h")))))

(ert-deftest test-params-to-src-includes-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (file-params (cmake-ide--idb-file-to-obj idb "file2")))
    (should (equal-lists
             (cmake-ide--params-to-src-includes file-params)
             '("h.h")))))

(ert-deftest test-commands-to-hdr-includes-1 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (commands (cmake-ide--idb-param-all-files idb 'command)))
    (should (equal-lists (cmake-ide--commands-to-hdr-includes commands)
                         '("/foo/bar.h" "a.h" "h.h")))))

(ert-deftest test-commands-to-hdr-includes-2 ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1\",
                  \"command\": \"cmd1 -Ifoo -Ibar -include /foo/bar.h -include a.h\"},
                  {\"file\": \"file2\",
                   \"command\": \"cmd2 foo bar -g -pg -Ibaz -Iboo -Dloo -include h.h\"}]"))
         (commands (cmake-ide--idb-param-all-files idb 'command)))
    (should (equal-lists (cmake-ide--commands-to-hdr-includes commands)
                         '("/foo/bar.h" "a.h" "h.h")))))

(ert-deftest test-all-vars ()
  (let ((cmake-ide-build-dir "/tmp")
        (idb (cmake-ide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"cmd1 -Iinc1 -Iinc2 -Dfoo=bar -S -F -g\"}]")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path '("/tmp/inc1" "/tmp/inc2")))
     (should (equal-lists flycheck-clang-definitions '("foo=bar")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal-lists flycheck-clang-args '("-S" "-F" "-g"))))))

(ert-deftest test-all-vars-ccache ()
  (let ((cmake-ide-build-dir "/tmp")
        (idb (cmake-ide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"/usr/bin/ccache clang++ -Iinc1 -Iinc2 -Dfoo=bar -S -F -g -std=c++14\"}]")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F" "-std=c++14")))
     (should (equal-lists company-clang-arguments ac-clang-flags))
     (should (equal-lists flycheck-clang-include-path '("/tmp/inc1" "/tmp/inc2")))
     (should (equal-lists flycheck-clang-definitions '("foo=bar")))
     (should (equal-lists flycheck-clang-includes nil))
     (should (equal flycheck-clang-language-standard "c++14"))
     (should (equal-lists flycheck-clang-args '("-S" "-F" "-g"))))))

(ert-deftest test-all-vars-ccache-alt ()
  (let ((idb (cmake-ide--cdb-json-string-to-idb
              "[{\"file\": \"file1.c\",
                  \"command\": \"/usr/lib/ccache/bin/clang++ -Iinc1 -Iinc2 -Dfoo=bar -S -F -g -std=c++14\"}]")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists ac-clang-flags '("-Iinc1" "-Iinc2" "-Dfoo=bar" "-S" "-F" "-std=c++14"))))))

(ert-deftest test-idb-obj-get ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"}]"))
         (obj (cmake-ide--idb-file-to-obj idb "file1.c")))
    (should (equal (cmake-ide--idb-obj-get obj 'foo) "the foo is mighty"))
    (should (equal (cmake-ide--idb-obj-get obj 'bar) "the bar is weak"))
    (should (equal (cmake-ide--idb-obj-get obj 'oops) nil))))

(ert-deftest test-idb-param-all-files ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[
                    {\"file\": \"file1.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"},
                    {\"file\": \"file2.c\", \"foo\": \"the foo is ugly\",   \"bar\": \"the bar is cool\"}
                ]")))
    (should (equal-lists (cmake-ide--idb-param-all-files idb 'foo) '("the foo is mighty" "the foo is ugly")))))

(ert-deftest test-idb-set-value-on-obj ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[{\"file\": \"file1.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"}]"))
         (obj (cmake-ide--idb-file-to-obj idb "file1.c")))

    (cmake-ide--idb-obj-set obj 'extra "extra stuff is nice too")
    (should (equal (cmake-ide--idb-obj-get obj 'extra) "extra stuff is nice too"))))

(ert-deftest test-idb-sort-by-file-distance ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[
                    {\"file\": \"foobar/f.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"},
                    {\"file\": \"dootrain/f.c\", \"foo\": \"the foo is ugly\",   \"bar\": \"the bar is cool\"},
                    {\"file\": \"food/f.c\", \"foo\": \"the foo is just a foo\",   \"bar\": \"what bar?\"}
                ]"))
         (sorted (cmake-ide--idb-sorted-by-file-distance idb "foo/h.h")))
    (should (equal (cmake-ide--idb-obj-get (elt sorted 0) 'file) "food/f.c"))
    (should (equal (cmake-ide--idb-obj-get (elt sorted 0) 'distance) 1))))


(ert-deftest test-same-file-twice ()
  (let* ((idb (cmake-ide--cdb-json-string-to-idb
               "[
                    {\"file\": \"foobar/f.c\", \"foo\": \"the foo is mighty\", \"bar\": \"the bar is weak\"},
                    {\"file\": \"dootrain/f.c\", \"foo\": \"the foo is ugly\",   \"bar\": \"the bar is cool\"},
                    {\"file\": \"food/f.c\", \"foo\": \"the foo is just a foo\",   \"bar\": \"what bar?\"},
                    {\"file\": \"foobar/f.c\", \"foo\": \"the foo is really mighty\", \"bar\": \"the bar is really weak\"}
                ]"))
         (obj (cmake-ide--idb-file-to-obj idb "foobar/f.c")))
    (should (equal (cmake-ide--idb-obj-get obj 'foo) "the foo is really mighty"))
    (should (equal-lists (cmake-ide--idb-param-all-files idb 'foo)
                         '("the foo is mighty" "the foo is ugly" "the foo is just a foo" "the foo is really mighty")))
    ))

(ert-deftest test-issue-43 ()
  (let ((idb (cmake-ide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/build-clang/dune-evolving-domains/src\",
 \"command\": \"/home/csunix/scstr/Software/anaconda/bin/clang++   -DENABLE_MPI=1 -DHAVE_CONFIG_H -DMPICH_SKIP_MPICXX -DMPIPP_H -O3 -Wall -Wno-unused-parameter -std=c++11  -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/build-clang/dune-evolving-domains -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains -I/usr/include/openmpi-x86_64 -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-common -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-geometry -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-grid -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-localfunctions -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-istl -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-alugrid -I/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-fem    -o CMakeFiles/dune_evolving_domains.dir/dune_evolving_domains.cc.o -c /home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains/src/dune_evolving_domains.cc\",
 \"file\": \"/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains/src/dune_evolving_domains.cc\"
 }
 ]
")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-clang-include-path
                          '("/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/build-clang/dune-evolving-domains" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-evolving-domains" "/usr/include/openmpi-x86_64" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-common" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-geometry" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-grid" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-localfunctions" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-istl" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-alugrid" "/home/cserv1_a/soc_staff/scstr/Software/DUNE/dune-2.4/dune-fem")))
     (should (equal-lists flycheck-clang-includes nil))
     )))


(ert-deftest test-issue-45 ()
  (should (equal (cmake-ide--is-src-file "foo.cpp") t))
  (should (equal (cmake-ide--is-src-file "foo.yyy") nil))
  (should (equal (cmake-ide--is-src-file "foo.cu") nil))
  (let ((cmake-ide-src-extensions '(".cu")))
    (should (equal (cmake-ide--is-src-file "foo.cu") t))
    (should (equal (cmake-ide--is-src-file "foo.cpp") nil))))

(ert-deftest test-only-flags ()
  (should (equal (cmake-ide--args-to-only-flags '("foo" "bar" "foo.cxx")) '("foo" "bar"))))

(ert-deftest test-issue-52 ()
  (let ((cmake-ide-build-dir "/usr/bin")
        (idb (cmake-ide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/project/build\",
 \"command\": \"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/cc  -DHAVE_CONFIG_H -DHTTP_PARSER_STRICT -DLUASOCKET_DEBUG -DWITH_COMPAT -DWITH_DOM -DWITH_NOIO -DWITH_OPENSSL -D_REENTRANT -I/usr/local/include -I/usr/local/include/luajit-2.0 -Ijansson-2.7/include -I/usr/local/opt/openssl/include -I/usr/local/include/mysql -I. -I../lib -I../lib/gsoap -I../lib/http-parser -I../lib/uthash -Wall -pedantic -O2 -O3 -DNDEBUG   -o src/ddutil/src/lua/socket/CMakeFiles/LUASOCKET_FILES.dir/options.c.o   -c /Users/user/dev/project/src/ddutil/src/lua/socket/options.c\",
 \"file\": \"/project/src/ddutil/src/lua/socket/options.c\"
 }
 ]
")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-clang-include-path
                          '("/usr/local/include" "/usr/local/include/luajit-2.0" "/usr/bin/jansson-2.7/include" "/usr/local/opt/openssl/include" "/usr/local/include/mysql" "/usr/bin" "/usr/lib" "/usr/lib/gsoap" "/usr/lib/http-parser" "/usr/lib/uthash")))
     )))

(ert-deftest test-json-to-file-params-reldir-issue ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"./foo.cpp\"}]")
         (idb (cmake-ide--cdb-json-string-to-idb json-str))
         (real-params (cmake-ide--idb-file-to-obj idb "./foo.cpp"))
         (fake-params (cmake-ide--idb-file-to-obj idb "oops")))
    (should (equal (cmake-ide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
    (should (equal (cmake-ide--idb-obj-get fake-params 'directory) nil))))

(ert-deftest test-json-to-file-params-reldir-issue-2 ()
  (let* ((json-str "[{\"directory\": \"/foo/bar/dir\",
                      \"command\": \"do the twist\", \"file\": \"foo.cpp\"}]")
         (idb (cmake-ide--cdb-json-string-to-idb json-str))
         (real-params (cmake-ide--idb-file-to-obj idb "foo.cpp"))
         (fake-params (cmake-ide--idb-file-to-obj idb "oops")))
    (should (equal (cmake-ide--idb-obj-get real-params 'directory) "/foo/bar/dir"))
    (should (equal (cmake-ide--idb-obj-get fake-params 'directory) nil))))

(ert-deftest test-issue-79 ()
  (let ((cmake-ide-build-dir "/usr/bin")
        (idb (cmake-ide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/usr/bin\",
 \"command\": \" g++-6    -I../include   -g -std=c++14 -Wall -Wextra -Werror   -o CMakeFiles/soln.dir/src/Source.cpp.o -c ../src/Source.cpp\",
 \"file\": \"../src/Source.cpp\"
 }
 ]
")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-cppcheck-include-path
                          '("/usr/include")))
     )))

(ert-deftest test-issue-79-2 ()
  (let ((cmake-ide-build-dir "/usr")
        (idb (cmake-ide--cdb-json-string-to-idb
              "[
 {
 \"directory\": \"/usr\",
 \"command\": \" g++-6    -Iinclude   -g -std=c++14 -Wall -Wextra -Werror   -o CMakeFiles/soln.dir/src/Source.cpp.o -c src/Source.cpp\",
 \"file\": \"src/Source.cpp\"
 }
 ]
")))
    (with-non-empty-file
     (cmake-ide--set-flags-for-file idb (current-buffer))
     (should (equal-lists flycheck-cppcheck-include-path
                          '("/usr/include")))
     )))

(provide 'cmake-ide-test)
;;; cmake-ide-test.el ends here
