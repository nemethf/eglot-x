;;; eglot-x.el --- Protocol extensions for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Version: 0.1
;; Author: Felicián Németh <felician.nemeth@gmail.com>
;; Maintainer: Felicián Németh <felician.nemeth@gmail.com>
;; URL: https://github.com/nemethf/eglot-x
;; Keywords: convenience, languages
;; Package-Requires: ((eglot "1.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Eglot supports (a subset of) the Language Server Protocol.
;; However, there are useful protocol extensions that are not (yet)
;; part of the official protocol specification.  Eglot-x adds support
;; for some of them.
;;
;; Add the following line to your init file to enable eglot-x
;;
;;     (with-eval-after-load 'eglot (require 'eglot-x))
;;
;; To adjust which extensions are enabled:
;;
;;     M-x customize-group RET eglot-x RET

;;; Code:

(require 'eglot)
(require 'project)


;;; Customization
(defgroup eglot-x nil
  "Protocol extensions for Eglot"
  :group 'eglot
  :prefix "eglot-x-")

(defcustom eglot-x-enable-files t
  "If non-nil, enable the support for the files protocol extension.
The extension allows the client and the server to have separate
file systems.  For example, the server can run inside a Docker
container, or the source code can be on a remote system accessed
by Tramp.  (With emacs-26, the latter is not supported.)
The client can send files to the server only from the result of
`project-files'.  The list of eligible files can further limited
by `eglot-x-files-visible-regexp' and
`eglot-x-files-hidden-regexp'.  This feature works if
`project-roots' and `project-external-roots' are set correctly.
\(project-files was introduced in emacs-27; eglot-x backports the
implementation to emacs-26.)"
  :type 'boolean
  :link `(url-link
          :tag "the documentation of the extesion proposal"
          ,(concat "https://github.com/sourcegraph/language-server-protocol/"
                   "blob/master/extension-files.md")))

(defcustom eglot-x-files-visible-regexp "."
  "Regexp matching filenames that can be sent to the language server."
  :type 'regexp)

(defcustom eglot-x-files-hidden-regexp "/\\."
  "Regexp matching filenames that cannot be sent to the language server.
It has precedence over `eglot-x-files-visible-regexp'."
  :type 'regexp)


;;; Enable the extensions
;;
(cl-defmethod eglot-client-capabilities :around
  (_s)
  "Extend client with non-standard capabilities."
  (let ((capabilities (cl-call-next-method)))
    (when eglot-x-enable-files
      (setq capabilities (append capabilities
                                 (list :xfilesProvider t
                                       :xcontentProvider t))))
    capabilities))


;;; Files extension
;;
;; https://github.com/sourcegraph/language-server-protocol/blob/master/extension-files.md

(defun eglot-x--path-to-TextDocumentIdentifier (path)
  "Convert PATH to TextDocumentIdentifier."
    `(:uri ,(eglot--path-to-uri path)))

(defvar eglot-x--project-files-cache '(0 nil nil)
  "A cache for function `eglot-x--project-files'.
The format for the cache: (timestamp args files).")

;; It seems language servers repeatedly request textDocument/xcontent,
;; so we cache the available files.
(defun eglot-x--project-files (project &optional dirs)
  "Return a list of files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project roots and external roots."
  (let* ((args (list project dirs))
         (files (caddr eglot-x--project-files-cache))
         (timestamp (current-time))
         (time-diff (time-subtract timestamp
                                   (car eglot-x--project-files-cache))))
    (when (or (not (equal args (cadr eglot-x--project-files-cache)))
              (< 0 (car time-diff))
              (< 5 (cadr time-diff)))
      ;; Cache is expired
      (setq files (if (fboundp 'project-files)
                      (project-files project dirs)
                    ;; Copied from an old version of emacs-27
                    (all-completions
                     "" (project-file-completion-table
                         project (or dirs (project-roots project)))))))
    (setq eglot-x--project-files-cache (list timestamp args files))
    files))

(defun eglot-x--files-visible-p (file &optional dir)
  "Return non-nil if FILE can be sent to the language server.
If DIR is non-nil, the file should be in directory DIR.  FILE is
assumed to be an element of `project-files'."
  (and (string-match eglot-x-files-visible-regexp file)
       (not (string-match eglot-x-files-hidden-regexp file))
       (or (not dir)
           (file-in-directory-p file dir))))

(cl-defmethod eglot-handle-request
  (server (_method (eql workspace/xfiles)) &key base)
  "Handle server request workspace/xfiles"
  (let* ((project (eglot--project server))
         (roots (append (project-roots project)
                        (project-external-roots project)))
         (dirs (if base
                   ;; Find the root directory of base
                   `(,(seq-find (lambda (dir) (file-in-directory-p base dir))
                                roots))
		 roots))
         (dirs (seq-remove #'null dirs))
         (files (when dirs
                  (eglot-x--project-files project dirs)))
         (pred (lambda (file) (eglot-x--files-visible-p file base))))
    (apply 'vector
           (mapcar 'eglot-x--path-to-TextDocumentIdentifier
                   (seq-filter pred files)))))

(cl-defmethod eglot-handle-request
  (server (_method (eql textDocument/xcontent)) &key textDocument)
  "Handle server request textDocument/xcontent"
  (let* ((file (eglot--uri-to-path (plist-get textDocument :uri)))
         (buffer (find-buffer-visiting file))
         (project-files (eglot-x--project-files (eglot--project server))))
    (if (or (not (eglot-x--files-visible-p file))
            (not (seq-contains project-files file #'string-equal)))
	(progn
          (eglot--warn "Server-request denied for file: %s" file)
          ;; https://github.com/sourcegraph/language-server-protocol/pull/26
	  (jsonrpc-error :code -32001 :message "Access denied"))
      (if buffer
          (with-current-buffer buffer
            (eglot--TextDocumentItem))
        (condition-case err
            (with-temp-buffer
              (let ((buffer-file-name file))
                (insert-file-literally file)
                (eglot--TextDocumentItem)))
          (file-error
           (let ((msg (error-message-string err)))
             (eglot--warn "Server-request failed for %s: %s" file msg)
             (jsonrpc-error :code -32002 :message msg))))))))

(provide 'eglot-x)
;;; eglot-x.el ends here
