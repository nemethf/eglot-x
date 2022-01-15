;;; eglot-x.el --- Protocol extensions for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Version: 0.2
;; Author: Felicián Németh <felician.nemeth@gmail.com>
;; Maintainer: Felicián Németh <felician.nemeth@gmail.com>
;; URL: https://github.com/nemethf/eglot-x
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "27.1") (project "0.8.1") (eglot "1.8"))

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
by Tramp.

The client can send files to the server only from the result of
`project-files'.  The list of eligible files can further limited
by `eglot-x-files-visible-regexp' and
`eglot-x-files-hidden-regexp'.  This feature works if
`project-roots' and `project-external-roots' are set correctly.

Enabling extension disables Eglot's built-in support for Tramp
files."
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

;; There's no point in disabling this feature.  The variable exists to
;; help discoverability and for documentation purposes.
(defcustom eglot-x-enable-refs t
  "If non-nil, enable the support for additional reference methods.

The command `eglot-x-find-refs' is the entry point for the extra
methods.  You can bind it to a key:

    (define-key eglot-mode-map (kbd \"s-.\") #'eglot-x-find-refs)

Currently, the `ccls' is the only server whose extra reference
methods eglot-x supports.
"
  :type 'boolean
  :link '(url-link
          :tag "LSP extensions of the `ccls' server"
          "https://github.com/MaskRay/ccls/wiki/LSP-Extensions"))

(defcustom eglot-x-enable-encoding-negotiation t
  "If non-nil, automatically negotiate proper encoding of positions.

The extension allows the client and the server to negotiate a
proper encoding to be used in transmitting column positions.

`eglot-x-encoding-alist' defines the details of the client's
behavior."
  :type 'boolean
  :link '(url-link
          :tag "The definition of the extension (clangd)"
          "https://clangd.github.io/extensions.html#utf-8-offsets"))

(defcustom eglot-x-encoding-alist
  '(("utf-32" . eglot-x--encoding-configure-utf-32)
    ("utf-16" . eglot-x--encoding-configure-utf-16))
  "Alist of encoding and configuration function pairs.
The keys are the encodings eglot supports.  Encodings should be
in the order of peference.  It SHOULD include \"utf-16\".  Note
that eglot achieves the best performance with \"utf-32\".  If the
result of the client-server negotiation is a key of the alist,
then the corresponding function is called with the key as an
argument."
  :type '(alist :key-type string :value-type function))


;;; Enable the extensions
;;
(cl-defmethod eglot-client-capabilities :around
  (_s)
  "Extend client with non-standard capabilities."
  (let ((capabilities (copy-tree (cl-call-next-method))))
    (when eglot-x-enable-files
      (setq capabilities (append capabilities
                                 (list :xfilesProvider t
                                       :xcontentProvider t))))
    (when eglot-x-enable-encoding-negotiation
      (add-hook 'eglot--managed-mode-hook
                #'eglot-x--encoding-configure)
      (setq capabilities
            (append capabilities
                    (list :offsetEncoding
                          (apply #'vector
                                 (mapcar #'car eglot-x-encoding-alist))))))
    capabilities))


;;; Files extension
;;
;; https://github.com/sourcegraph/language-server-protocol/blob/master/extension-files.md

(defun eglot-x--disable-built-in-tramp (orig-fun &rest args)
  "Disable Eglot's remote server support when `eglot-x-enable-files' is set."
  (if eglot-x-enable-files
      (cl-flet ((file-remote-p nil)
                (file-local-name (file) file))
        (apply orig-fun args))
    (apply orig-fun args)))
;; defuns containing file-remote-p
(advice-add 'eglot--cmd     :around #'eglot-x--disable-built-in-tramp)
(advice-add 'eglot--connect :around #'eglot-x--disable-built-in-tramp)
(advice-add 'eglot--path-to-uri :around #'eglot-x--disable-built-in-tramp)
(advice-add 'eglot--uri-to-path :around #'eglot-x--disable-built-in-tramp)
;; defuns containing file-local-name (but not file-remote-p)
(advice-add 'eglot--connect :around #'eglot-x--disable-built-in-tramp)


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
                                   (car eglot-x--project-files-cache)))
         (dirs (or dirs
                   (append (project-roots project)
                           (project-external-roots project)))))
    (when (or (not (equal args (cadr eglot-x--project-files-cache)))
              (< 0 (car time-diff))
              (< 5 (cadr time-diff)))
      ;; Cache is expired
      (setq files (project-files project dirs)))
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
            ;; The server might request textDocument/xcontent even
            ;; before `eglot--connect' succesfully finishes and puts
            ;; the server into `eglot--servers-by-project'.  But this
            ;; buffer belongs to the project, so set the local cache,
            ;; and avoid a failure in
            ;; `eglot--TextDocumentItem'/`eglot--current-server-or-lose'.
            (setq eglot--cached-server server)
            (eglot--TextDocumentItem))
        (condition-case err
            (with-temp-buffer
              (let ((buffer-file-name file))
                (insert-file-contents-literally file)
                (eglot--TextDocumentItem)))
          (file-error
           (let ((msg (error-message-string err)))
             (eglot--warn "Server-request failed for %s: %s" file msg)
             (jsonrpc-error :code -32002 :message msg))))))))


;;; Extra reference methods
;;
;; API functions, variables, and the implementation is not yet set in
;; stone.

(defvar eglot-x--extra-refs-alist
  '(("ccls" .
     ;; https://github.com/MaskRay/ccls/wiki/LSP-Extensions
     (("reload" (lambda ()
                  (jsonrpc-notify (eglot-current-server)
                                  :$ccls/reload
                                  (make-hash-table))))
      ("vars"   :$ccls/vars)
      ("call"   :$ccls/call)
      ("callee" :$ccls/call :callee t)
      ("navigate-up"    :$ccls/navigate :direction "U")
      ("navigate-down"  :$ccls/navigate :direction "D")
      ("navigate-right" :$ccls/navigate :direction "R")
      ("navigate-left"  :$ccls/navigate :direction "L")
      ("inheritance"         :$ccls/inheritance)
      ("inheritance-derived" :$ccls/navigate :derived t)
      ("member-var"  :$ccls/member :kind 4)
      ("member-fun"  :$ccls/member :kind 3)
      ("member-type" :$ccls/member :kind 2)
      ("declaration"     eglot-find-declaration)
      ("implementation"  eglot-find-implementation)
      ("type definition" eglot-find-typeDefinition)))
    (t .
       (("declaration"     eglot-find-declaration)
        ("implementation"  eglot-find-implementation)
        ("type definition" eglot-find-typeDefinition)))))

(defun eglot-x-find-refs ()
  "Find additional references for the identifier at point.
The available reference types depend on the server.
See `eglot-x-enable-refs'."
  (interactive)
  (unless eglot-x-enable-refs
    (eglot--error "Feature is disabled (`eglot-x-enable-refs')"))
  (unless (eglot-current-server)
    (eglot--error "No active LSP server"))
  (let* ((server-name
          (plist-get (eglot--server-info (eglot-current-server)) :name))
         (extra-refs-alist
          (alist-get server-name eglot-x--extra-refs-alist
                     (alist-get t eglot-x--extra-refs-alist) nil #'equal))
         (menu `("Extra refs:" ,`("dummy" . ,extra-refs-alist)))
         (selected (tmm-prompt menu)))
    (if (functionp (car selected))
        (apply #'funcall selected)
      (eglot--lsp-xref-helper (car selected)
                              :extra-params (cdr selected)
                              :capability :definitionProvider))))


;;; Encoding negotiation
;; https://clangd.github.io/extensions.html#utf-8-offsets

;; Eglot does not save the full InitializeResult response of the
;; server, so the following chain of advised functions copies the
;; negotiated encoding into the server's capabilities.  It therefore
;; becomes accessible by `eglot-x--encoding-configure'.

(defvar eglot-x--encoding-hack nil)
(defun eglot-x--encoding-enable-hack (orig-fun &rest args)
  (let ((eglot-x--encoding-hack t))
    (apply orig-fun args)))
(advice-add 'eglot--connect :around #'eglot-x--encoding-enable-hack)

(defun eglot-x--encoding-mod-async-request (args)
  (if (not eglot-x--encoding-hack)
      args
    (let* ((plist (seq-drop args 3))
           (head (seq-take args 3))
           (success-fn (plist-get plist :success-fn))
           (success-fn (lambda (&rest args)
                         (let ((args (eglot-x--encoding-mod-result args)))
                           (apply success-fn args))))
           (plist (and plist
                       (plist-put plist :success-fn success-fn))))
      `(,@head ,@plist))))
(advice-add 'jsonrpc--async-request-1 :filter-args
            #'eglot-x--encoding-mod-async-request)

(defun eglot-x--encoding-mod-result (result)
  "If RESULT is initializationOptions, copy offsetEncoding into capabilities."
  (let* ((plist (car result))
         (offset-encoding (plist-get plist :offsetEncoding))
         (capabilities (plist-get plist :capabilities)))
    (if (not (and offset-encoding capabilities))
        result
      (setq plist (copy-tree plist))
      (list (plist-put plist :capabilities
                       (plist-put capabilities :offsetEncoding
                                  offset-encoding))))))

;; Should be in `eglot--managed-mode-hook'.
(defun eglot-x--encoding-configure ()
  "Configure eglot based on the negotiated encoding."
  (when (and eglot-x-enable-encoding-negotiation
             (eglot-current-server))
    (let* ((encoding (eglot--server-capable :offsetEncoding))
           (fn (assoc-default encoding eglot-x-encoding-alist)))
      (when fn
        (funcall fn encoding)))))

(defun eglot-x--encoding-configure-utf-32 (_encoding)
  (let ((pairs
         '((eglot-current-column-function . eglot-current-column)
           (eglot-move-to-column-function . eglot-move-to-column))))
    (dolist (pair pairs)
      (set (make-local-variable (car pair)) (cdr pair)))))

(defun eglot-x--encoding-configure-utf-16 (_encoding)
  (let ((pairs
         '((eglot-current-column-function . eglot-lsp-abiding-column)
           (eglot-move-to-column-function . eglot-move-to-lsp-abiding-column))))
    (dolist (pair pairs)
      (set (make-local-variable (car pair)) (cdr pair)))))


(provide 'eglot-x)
;;; eglot-x.el ends here
