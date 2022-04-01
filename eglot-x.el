;;; eglot-x.el --- Protocol extensions for Eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

;; Version: 0.4
;; Author: Felicián Németh <felician.nemeth@gmail.com>
;; URL: https://github.com/nemethf/eglot-x
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "27.1") (project "0.8.1") (eglot "1.8") (xref "1.4"))

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

(defcustom eglot-x-enable-files nil
  "If non-nil, enable the support for the files protocol extension.

The extension allows the client and the server to have separate
file systems.  For example, the server can run inside a Docker
container, or the source code can be on a remote system accessed
by Tramp.

The client can send files to the server only from the result of
`project-files'.  The list of eligible files can further limited
by `eglot-x-files-visible-regexp' and
`eglot-x-files-hidden-regexp'.  This feature works if
`project-root' and `project-external-roots' are set correctly.

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

Currently, the `ccls' and `rust-analyzer' are the only servers
whose extra reference methods eglot-x supports.
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

(defcustom eglot-x-enable-snippet-text-edit t
  "If non-nil, the server may send SnippetTextEdits.

More precisely, \"WorkspaceEdits returned from codeAction requests
might contain SnippetTextEdits instead of usual TextEdits\".
Example: \"Add derive\" code action transforms \"struct S;\"
into \"#[derive($0)] struct S;\""
  :type 'boolean
  :link '(url-link
          :tag "The definition of the extension (rust-analyzer)"
          "https://github.com/rust-analyzer/rust-analyzer/blob/master/\
docs/dev/lsp-extensions.md#snippet-textedit"))

(defcustom eglot-x-enable-server-status t
  "If non-nil, ask the server to send status messages.

The status is displayed in the mode-line and requires
server-support for capability experimental/serverStatus."
  :type 'boolean
  :link '(url-link
          :tag "the definition of the extension (rust-analyzer)"
          "https://github.com/rust-analyzer/rust-analyzer/blob/master/\
docs/dev/lsp-extensions.md#server-status"))

(defcustom eglot-x-graph-type 'auto
  "Default graph type of `eglot-x-view-crate-graph'."
  :type
  '(radio
    (const :tag "auto: based on display capabilities" auto)
    (const svg)
    (const :tag "boxart: using unicode characters" boxart)
    (const ascii)
    (const :tag "raw: for debugging purposes" rawtext))
  :link '(url-link
          :tag "the definition of the extension"
          "https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#view-crate-graph")
  :link '(url-link
          :tag "documentation of rust-analyzer"
          "https://rust-analyzer.github.io/manual.html#view-crate-graph"))


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
      (add-hook 'eglot-managed-mode-hook
                #'eglot-x--encoding-configure)
      (setq capabilities
            (append capabilities
                    (list :offsetEncoding
                          (apply #'vector
                                 (mapcar #'car eglot-x-encoding-alist))))))
    (when eglot-x-enable-snippet-text-edit
      (let* ((exp (plist-get capabilities :experimental))
             (old (if (eq exp eglot--{}) '() exp))
             (new (plist-put old :snippetTextEdit t)))
        (setq capabilities (plist-put capabilities :experimental new))))
    (when eglot-x-enable-server-status
      (let* ((exp (plist-get capabilities :experimental))
             (old (if (eq exp eglot--{}) '() exp))
             (new (plist-put old :serverStatusNotification t)))
        (setq capabilities (plist-put capabilities :experimental new))))
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
                   (append (list (project-root project))
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
         (roots (append (list (project-root project))
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
            (not (seq-contains-p project-files file #'string-equal)))
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
    ("rust-analyzer" .
     (("parent-module"   :experimental/parentModule)
      ("open-cargo-toml" :experimental/openCargoToml)
      ("matching brace"  eglot-x-matching-brace)
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

;; Should be in `eglot-managed-mode-hook'.
(defun eglot-x--encoding-configure ()
  "Configure eglot based on the negotiated encoding."
  (when (and eglot-x-enable-encoding-negotiation
             (eglot-managed-p)
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



;;; rust-analyzer extensions
;; https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md

;; Implemented elsewhere
;;   - UTF-8 offsets: see variable `eglot-x-enable-encoding-negotiation'.
;;   - Parent Module: implemented in `eglot-x-find-refs'.
;;   - Open Cargo.toml: implemented in `eglot-x-find-refs'.


;; Features not implemented:
;;   Client Commands
;;   CodeAction Groups
;;   Configuration in initializationOptions
;;     - This is in the standard: https://github.com/joaotavora/eglot/discussions/845
;;   Hover Actions
;;   Hover Range
;;   Inlay Hints

(defun eglot-x--check-capability (&rest capabilities)
  (unless (apply #'eglot--server-capable capabilities)
    (eglot--error "Server lacks capability: %s" capabilities)))

(eval-and-compile
  (push '(InlayHint
          ((:label :position) (:kind :tooltip :padding_left :padding_right)))
        eglot--lsp-interface-alist)
  ;; This is an unnamed type within WorkspaceSymbol/location
  (push '(Runnable
          ((:label kind args) (location)))
        eglot--lsp-interface-alist)
  (push '(LocationWithOptionalRange
          ((:uri) (:range)))
        eglot--lsp-interface-alist)
  ;; SnippetTextEdit extends TextEdit
  (push '(SnippetTextEdit
          ((:range :newText)
           (:insertTextFormat :annotationId)))
        eglot--lsp-interface-alist)
  (push '(WorkspaceSymbol
          ((:name  :kind)
           (:tags :location :containerName :deprecated)))
        eglot--lsp-interface-alist))

;;; Snippet TextEdit

(defun eglot-x--apply-text-edits (edits &optional version)
  "Apply EDITS for current buffer if at VERSION, or if it's nil.
This is almost a verbatim copy of `eglot--apply-text-edits', but
it handles the SnippetTextEdit format."
  ;; This is quite rust-analyzer specific.  It assumes there is at
  ;; most one meaningful SnippetTextEdit and that can be identified by
  ;; searching for "$0".
  (unless (or (not version) (equal version eglot--versioned-identifier))
    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version eglot--versioned-identifier))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (make-progress-reporter
                      (format "[eglot] applying %s edits to `%s'..."
                              howmany (current-buffer))
                      0 howmany))
           (done 0)
           snippet snippet-beg snippet-end)
      (mapc (pcase-lambda (`(,newText ,insertTextFormat (,beg . ,end)))
              (let ((source (current-buffer)))
                (with-temp-buffer
                  (insert newText)
                  (let ((temp (current-buffer)))
                    (with-current-buffer source
                      (save-excursion
                        (save-restriction
                          (narrow-to-region beg end)

                          ;; On emacs versions < 26.2,
                          ;; `replace-buffer-contents' is buggy - it calls
                          ;; change functions with invalid arguments - so we
                          ;; manually call the change functions here.
                          ;;
                          ;; See emacs bugs #32237, #32278:
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
                          (let ((inhibit-modification-hooks t)
                                (length (- end beg))
                                (beg (marker-position beg))
                                (end (marker-position end)))
                            (run-hook-with-args 'before-change-functions
                                                beg end)
                            (replace-buffer-contents temp)
                            (run-hook-with-args 'after-change-functions
                                                beg (+ beg (length newText))
                                                length))
                          (when (and (eql insertTextFormat 2)
                                     (string-match "\\$\\(0\\|{0[^}]*}\\)"
                                                   newText))
                            ;; "At the moment, rust-analyzer
                            ;; guarantees that only a single edit will
                            ;; have InsertTextFormat.Snippet."  Well,
                            ;; every one of them has insertTextFormat
                            ;; = 2, and there's no easy, reliable way
                            ;; to tell, which one contains a real
                            ;; snippet. RA's own .ts implementation
                            ;; uses the regexp above.
                            (setq snippet newText)
                            (setq snippet-beg (point-min-marker))
                            (setq snippet-end (point-max-marker)))))
                      (progress-reporter-update reporter (cl-incf done)))))))
            (mapcar (eglot--lambda ((SnippetTextEdit) range newText insertTextFormat)
                      (list newText insertTextFormat (eglot--range-region range 'markers)))
                    (reverse edits)))
      (when snippet
        (goto-char snippet-beg)
        (delete-region snippet-beg snippet-end)
        (funcall (eglot--snippet-expansion-fn) snippet))
      (undo-amalgamate-change-group change-group)
      (progress-reporter-done reporter))))

(defun eglot-x--override-text-edits (oldfun &rest r)
  (if eglot-x-enable-snippet-text-edit
      (apply #'eglot-x--apply-text-edits r)
    (apply oldfun r)))

(advice-add #'eglot--apply-text-edits :around #'eglot-x--override-text-edits)

;;; Join Lines

(defun eglot-x-join-lines (&optional beg end)
  "Request the server to handle 'Join Lines' editor action."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (eglot-x--check-capability :experimental :joinLines)
  (let ((res
         (jsonrpc-request (eglot--current-server-or-lose)
                          :experimental/joinLines
                          (list :textDocument (eglot--TextDocumentIdentifier)
                                :ranges
                                (vector (list :start (eglot--pos-to-lsp-position beg)
                                              :end (eglot--pos-to-lsp-position end)))))))
    (mapc (lambda (textEdit)
            (eglot--dbind ((TextEdit) range newText) textEdit
              (pcase-let ((`(,beg . ,end)
                           (eglot--range-region range)))
                (delete-region beg end)
                (goto-char beg)
                (funcall #'insert newText))))
          res)))

;;; Move Item

(defun eglot-x-move-item-down (arg)
  "Ask server to move down item under point or selection.
With prefix arg move it up."
  (interactive "*P")
  (eglot-x--check-capability :experimental :moveItem)
  (let* ((beg (if (region-active-p) (region-beginning) (point)))
         (end (if (region-active-p) (region-end) (point)))
         (res
          (jsonrpc-request (eglot--current-server-or-lose)
                           :experimental/moveItem
                           `(:direction ,(if arg "Up" "Down")
                             :range ,(list :start (eglot--pos-to-lsp-position beg)
                                           :end (eglot--pos-to-lsp-position end))
                             :textDocument ,(eglot--TextDocumentIdentifier)))))
    (eglot-x--apply-text-edits res)))

(defun eglot-x-move-item-up (arg)
  "Ask server to move up item under point or selection.
With prefix arg move it down."
  (interactive "*P")
  (eglot-x-move-item-down (not arg)))

;;; On Enter

(defvar eglot-x-on-enter-fallback #'newline
  "Fallback function when onEnter does nothing.
It seems onEnter works only in comments, otherwise it returns no
TextEdits.  This variable defines what function to call in that
case.")

(defun eglot-x-on-enter (&optional arg interactive)
  "Request the server to handle the 'Enter' keypress."
  (interactive "*P\np")
  (eglot-x--check-capability :experimental :onEnter)
  (let ((res
         (jsonrpc-request (eglot--current-server-or-lose)
                          :experimental/onEnter
                          (eglot--TextDocumentPositionParams))))
    (if res
        (eglot-x--apply-text-edits res)
      (funcall eglot-x-on-enter-fallback arg interactive))))

;;; Matching Brace

(defun eglot-x-matching-brace ()
  "Jump to matching brace.  Available in `eglot-x-find-refs' as well."
  ;; When is this better than `backward-sexp', `forward-sexp'?
  (interactive)
  (eglot-x--check-capability :experimental :matchingBrace)
  (let ((res (jsonrpc-request
              (eglot--current-server-or-lose)
              :experimental/matchingBrace
              `(:textDocument ,(eglot--TextDocumentIdentifier)
                :positions ,(vector (eglot--pos-to-lsp-position))))))
    (mapc (lambda (position)
            (push-mark)
            (goto-char (eglot--lsp-position-to-point position)))
          res)))

;;; Open External Documentation

(defun eglot-x-open-external-documentation ()
  (interactive)
  "Open a URL to the documentation for the symbol under point."
  (eglot-x--check-capability :experimental :externalDocs)
  (let ((res (jsonrpc-request (eglot--current-server-or-lose)
                              :experimental/externalDocs
                              (eglot--TextDocumentPositionParams))))
    (when res
      (browse-url res))))

;;; Analyzer Status

(defun eglot-x-analyzer-status ()
  "Show server's internal status message, mostly for debugging purposes."
  (interactive)
  (let ((res
         (jsonrpc-request (eglot--current-server-or-lose)
                          :rust-analyzer/analyzerStatus
                          `(:textDocument ,(eglot--TextDocumentIdentifier)))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "%s" res))))))


;;; Reload Workspace

(defun eglot-x-reload-workspace ()
  "Ask server to reload project information (ie, re-execute cargo metadata)."
  (interactive)
  (jsonrpc-request (eglot--current-server-or-lose)
                   :rust-analyzer/reloadWorkspace
                   nil))

;;; Syntax Tree

(defvar eglot-x--source-buffer)

(defun eglot-x--pop-source-buffer (marker)
  (save-excursion
    (goto-char (marker-position marker))
    (beginning-of-line)
    (when (re-search-forward "@\\([0-9]+\\)\\.\\.\\([0-9]+\\)")
      (let ((beg (string-to-number (match-string 1)))
            (end (string-to-number (match-string 2))))
        (pop-to-buffer eglot-x--source-buffer)
        (goto-char (+ 1 end))
        (set-mark-command nil)
        (goto-char (+ 1 beg))))))

(define-button-type 'eglot-x--syntax-tree
  :supertype 'help-xref
  'action #'eglot-x--pop-source-buffer
  'help-echo (purecopy "mouse-1, RET: jump to source"))

(defun eglot-x-show-syntax-tree (&optional beg end)
  "Show textual representation of a parse tree for the file/region.
Primarily for debugging, but very useful for all people working
on rust-analyzer itself."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (let ((res
         (jsonrpc-request (eglot--current-server-or-lose)
                          :rust-analyzer/syntaxTree
                          (list :textDocument (eglot--TextDocumentIdentifier)
                                :ranges
                                (vector (list :start (eglot--pos-to-lsp-position beg)
                                              :end (eglot--pos-to-lsp-position end))))))
        (src-buf (current-buffer)))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (setq-local eglot-x--source-buffer src-buf)
        (insert (format "%s" res))
        (goto-char (point-min))
        (while (re-search-forward "@\\([0-9]+\\)\\.\\.\\([0-9]+\\)" nil t)
          (make-text-button (match-beginning 1) (match-end 2)
                            'type 'eglot-x--syntax-tree
                            'action 'eglot-x--pop-source-buffer))))))

;;; View Hir

(defun eglot-x-view-hir ()
  (interactive)
  "Show textual representation of the HIR of the function containing point.
For debugging or when working on rust-analyzer itself."
  (let ((res (jsonrpc-request (eglot--current-server-or-lose)
                              :rust-analyzer/viewHir
                              (eglot--TextDocumentPositionParams))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "%s" res))))))

;;; View ItemTree

(defun eglot-x-view-item-tree ()
  (interactive)
  "Show a textual representation of the ItemTree of current file.
It is for debugging rust-analyzer."
  (let ((res
         (jsonrpc-request (eglot--current-server-or-lose)
                          :rust-analyzer/viewItemTree
                          `(:textDocument ,(eglot--TextDocumentIdentifier)))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "%s" res))))))

;;; View Crate Graph and Shuffle Crate Graph

(define-button-type 'eglot-x--find-ref
  :supertype 'help-xref
  'face 'font-lock-constant-face
  'mouse-face 'highlight
  'action #'eglot-x--find-ref
  'help-echo (purecopy "mouse-1, RET: find source"))

(defun eglot-x--find-ref (marker)
  "Find ref under MARKER from a linked help buffer.
This assumes rust."
  (let ((symbol (save-excursion
                  (goto-char marker)
                  (symbol-at-point))))
    (xref-push-marker-stack marker)
    (switch-to-buffer eglot-x--source-buffer)
    ;; adapted from `project-find-regexp'
    (let* ((regexp (concat "name\s*=\s*\""
                           (replace-regexp-in-string "_" "[_-]"
                                                     (symbol-name symbol))
                           "\""))
           (pr (project-current))
           (endswith (lambda (str) (string-match "Cargo.toml$" str)))
           (files (delete-dups (cl-remove-if-not endswith (project-files pr))))
           (xrefs
            (lambda ()
              ;; Inefficiently filter out file-duplicates
              (let ((xrefs (project--find-regexp-in-files regexp files))
                    (files))
                (seq-filter
                 (lambda (item)
                   (pcase-let (((cl-struct xref-file-location file)
                                (xref-item-location item)))
                     (when (not (member file files))
                       (push file files))))
                 xrefs)))))
      (condition-case nil
          (let ((xref-show-xrefs-function xref-show-definitions-function))
            (xref-show-xrefs xrefs nil))
        (user-error
         (eglot-x--find-crate-with-cargo (symbol-name symbol)))))))
        ;;  (switch-to-buffer (help-buffer))
        ;;  (message "Can't find: %s" symbol))))))

(defun eglot-x--find-crate-with-cargo (crate)
  "Find CRATE with cargo and jq."
  ;; The jq dependency can be easily eliminated.
  ;; This should be in cargo.el.
  (let* ((regexp (concat "^"
                         (replace-regexp-in-string "_" "[_-]" crate)
                         "$"))
         (res (shell-command-to-string
               (concat "cargo metadata --format-version 1|"
                       "jq '.packages[]|select(.name|test(\""
                       regexp
                       "\")).manifest_path'")))
         (xrefs
          (lambda ()
            (mapcar (lambda (line)
                      (xref-make crate
                                 (xref-make-file-location
                                  (substring line 1 -1) 1 0)))
                    (split-string res)))))
    (if (not (equal res ""))
        (let ((xref-show-xrefs-function xref-show-definitions-function))
          (xref-show-xrefs xrefs nil))
      (switch-to-buffer (help-buffer))
      (message "Can't find: %s" crate))))

(defvar eglot-x--graph-buffer " *eglot-x-crate-graph*"
  "Buffer name to store the output of the background process.")
(defvar eglot-x--image-format nil
  "Image format of the currently displayed graph.")

(defun eglot-x--insert-graph (img image-format)
  "Insert IMG of IMAGE-FORMAT at point and post process it."
  (save-excursion
    (if (eq image-format 'svg)
        (insert-image (create-image img 'svg t))
      ;; Textual graph
      (save-excursion
        (insert img))
      (let ((re (pcase image-format
                  ('rawtext "label=\"\\([^\"]+\\)\"")
                  (_ "\\<\\([a-zA-Z_]+\\)\\>"))))
        (while (re-search-forward re nil t)
          (make-text-button (match-beginning 1) (match-end 1)
                            :type 'eglot-x--find-ref))))))

(defun eglot-x--graph-sentinel (process event)
  (when (not (process-live-p process))
    (if (or (not (eq 'exit (process-status process)))
            (not (eq 0 (process-exit-status process))))
        (progn
          (message "%s" event))
      (with-current-buffer (help-buffer)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (when (re-search-forward "^A background process is" nil t)
              (delete-region (line-beginning-position)
                             (line-end-position))
              (eglot-x--insert-graph
               (with-current-buffer (process-buffer process)
                 (buffer-substring (point-min) (point-max)))
               eglot-x--image-format)
              (when (not (get-buffer-window))
                (message "Process 'eglot-x-graph': %s" event)))))))))

(defun eglot-x-view-crate-graph (full image-format)
  "Render rust-analyzer's crate graph as an image.
With a prefix argument, include non-workspace crates (crates.io
dependencies as well as sysroot crates).  See variable
`eglot-x-graph-type'."
  (interactive "P\ni")
  ;; The 'jump to source' hack works satisfactorily for crates in the
  ;; workspace, but it would be the server's responsibility to link
  ;; crate names with their location.


  ;; TODO: Can we extend graph-easy to genearte ascii output with
  ;;       colors and hyperlinks?  (and emacs to handle them
  ;;       internally)
  ;;       https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda
  ;;
  ;; TODO: Similarly, svg-files can contain URLs, but emacs (librsvg)
  ;;       doesn't seem to make them clickable.
  ;;       https://graphviz.org/docs/attrs/URL/
  ;;
  ;; TODO: Finally, rust-analyzer doesn't send embedded URLs in its
  ;;       crate-graph.  But it would be an interesting feature to
  ;;       navigate to a source file (Cargo.toml) location from the
  ;;       graph.  Another possibility of enhancement for
  ;;       rust-analyzer is to send class attributes. Eg:
  ;;       [class="library"], [class="workspace"].
  ;;
  (let* ((src-buf (current-buffer))
         (image-format (if (not image-format)
                           eglot-x-graph-type
                         image-format))
         (image-format
          (if (not (eq image-format 'auto)) image-format
            (cond ((image-type-available-p 'svg) 'svg)
                  (t 'boxart)))) ;; How to check if boxart looks OK in
                                 ;; a terminal?
         (cmd (pcase image-format
                ('svg '("dot" "-Tsvg"))
                ('rawtext nil)
                (_ `("graph-easy" ,(format "--as=%s" image-format)))))
         (formats '(svg boxart ascii rawtext))
         (next-format
          (nth (mod (+ 1 (or (cl-position image-format formats)
                             1))
                    (length formats))
               formats))
         (res
          (jsonrpc-request (eglot--current-server-or-lose)
                           :rust-analyzer/viewCrateGraph
                           `(:full ,(if full t :json-false))))
         (refresh-fn
          (lambda (args)
            (let ((type (car args))
                  (image-format (cadr args))
                  (buf (current-buffer))
                  (point (point)))
              (switch-to-buffer eglot-x--source-buffer)
              (pcase type
                ('shuffle (eglot-x--shuffle-crate-graph full image-format))
                ('toggle  (eglot-x-view-crate-graph (not full) image-format))
                (_        (eglot-x-view-crate-graph full image-format)))
              (switch-to-buffer buf)
              (goto-char point)))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (setq-local eglot-x--source-buffer src-buf)
        (insert-button "[Shuffle]"
                       :type 'help-xref
                       'action refresh-fn
                       'button-data `(shuffle ,image-format)
                       'help-echo "mouse-1, RET: Shuffle and refresh")
        (insert " ")
        (insert-button "[Toggle non-workspace crates]"
                       :type 'help-xref
                       'action refresh-fn
                       'button-data `(toggle ,image-format)
                       'help-echo "mouse-1, RET: Toggle including \
non-workspace crates (crates.io dependencies as well as sysroot crates)")
        (insert " ")
        (insert-button (format "[Render as %s]" next-format)
                       :type 'help-xref
                       'action refresh-fn
                       'button-data `(keep ,next-format)
                       'help-echo "mouse-1, RET: Re-render in different format")
        (insert " ")
        (insert-button "[Customize]"
                       :type 'help-xref
                       'action #'customize-variable
                       'button-data 'eglot-x-graph-type
                       'help-echo "mouse-1, RET: Customize default view")
        (insert "\n")
        (if (not cmd)
            (eglot-x--insert-graph res image-format)
          (insert "A background process is generating the graph\n")
          (setq-local eglot-x--image-format image-format)
          (when (get-buffer eglot-x--graph-buffer)
            (kill-buffer eglot-x--graph-buffer))
          (with-current-buffer (get-buffer-create eglot-x--graph-buffer)
            (buffer-disable-undo)
            (make-process
             :name "eglot-x-graph"
             :buffer (current-buffer)
             :coding 'utf-8
             :command cmd
             :noquery t
             :stderr (get-buffer-create
                      (concat eglot-x--graph-buffer "-stderr"))
             :sentinel #'eglot-x--graph-sentinel)
            (process-send-string (current-buffer) res)
            (process-send-eof)))))))

(defun eglot-x--shuffle-crate-graph (full image-format)
  "Shuffle the crate IDs in the crate graph then view it.
For debugging purposes."
  (interactive "P\ni")
  (jsonrpc-request (eglot--current-server-or-lose)
                   :rust-analyzer/shuffleCrateGraph
                   nil)
  (eglot-x-view-crate-graph full image-format))


;;; Expand Macro
;; https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#expand-macro

(defun eglot-x-expand-macro ()
  "Expand macro call at point."
  (interactive)
  (let ((res (jsonrpc-request (eglot--current-server-or-lose)
                              :rust-analyzer/expandMacro
                              (eglot--TextDocumentPositionParams))))
    (when (not (plist-get res :name))
      (error "The server found no macro expansions."))
    (pop-to-buffer (format "eglot-macro-%s.rs" (plist-get res :name)))
    (delete-region (point-min) (point-max))
    (insert (plist-get res :expansion))
    (rust-mode)
    ;;; The following might be useful, but doesn't work flawlessly
    ;;(setq eglot--cached-server server)
    ;;(eglot--managed-mode)
    (view-mode)))

;;; (Memory Usage) -- this is not documented in lsp-extensions.md

(defun eglot-x-memory-usage ()
  (interactive)
  (let ((res
         (jsonrpc-request (eglot--current-server-or-lose)
                          :rust-analyzer/memoryUsage
                          nil)))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "%s" res))))))

;;; Structural Search Replace (SSR)

(defun eglot-x-structural-search-replace (query)
  "Perform an automated syntax tree based transformation of the source.
Example, SSR with query:  foo($a, $b) ==>> ($a).foo($b) will transforms,
foo(y + 5, z) into (y + 5).foo(z)."
  (interactive (list (eglot-x--read-ssr)))
  (eglot-x--check-capability :experimental :ssr)
  (let* ((sel
          (if (region-active-p)
              (vector (list :start (eglot--pos-to-lsp-position (region-beginning))
                            :end (eglot--pos-to-lsp-position (region-end))))
            (vector)))
         (res (jsonrpc-request (eglot--current-server-or-lose)
                              :experimental/ssr
                              (append
                               `(:selections ,sel
                                 :query ,query
                                 :parseOnly :json-false)
                               (eglot--TextDocumentPositionParams)))))
    ;;(eglot-x--query-workspace-edit res query)))
    (eglot-x--query-workspace-edit res "SSR")))

(defun eglot-x--replace (rdata _count)
  "Return the replacement text at point for `perform-replace'.
Replacement data RDATA is a list of (cons marker-range
replacement_text)."
  (cl-loop for d in rdata
           when (and (eq (point) (marker-position (cdar d)))
                     (eq (current-buffer) (marker-buffer (cdar d))))
           return (cdr d)))

(defun eglot-x--search-forward (rdata _string &optional bound _noerror _count)
  ;; checkdoc-params: (string bound noerror count)
  "Search like `search-forward', but rely on replacement data RDATA.
See `eglot-x--replace' for the description of RDATA, and
`search-forward' for the rest."
  ;; Move point to the marker closest to the point.
  (let ((range
         (cl-loop for d in rdata
                  when (let ((beg (caar d)))
                          (and (eq (current-buffer) (marker-buffer beg))
                               (< (point) (caar d))
                               (or (not bound)
                                   (< (point) bound))))
                  return (car d))))
    ;; (if (not range)
    ;;     (goto-char (or bound (point-max)))
    (when range
      (set-match-data (list (car range) (cdr range)))
      (goto-char (cdr range)))))

(declare-function fileloop-continue "fileloop" ())

(defun eglot-x--query-workspace-edit (wedit query &optional _start _end _backward)
  ;; Results in wedit are already in (start, end), so there's no need
  ;; to specify that.
  ;; TODO: create markers for changes, not just for documentChanges
  ;; TODO: what about CreateFile, RenameFile, DeleteFile?
  (eglot--dbind ((WorkspaceEdit) _changes documentChanges) wedit
    (let* ((rdata ;; list of cons(cons(beg-marker end-marker) TextEdit).
            (apply #'append
             (mapcar
              (eglot--lambda ((TextDocumentEdit) textDocument edits)
                (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                    textDocument
                  (unless (or (not version)
                              (equal version eglot--versioned-identifier))
                    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version eglot--versioned-identifier))
                  (with-current-buffer (find-file-noselect (eglot--uri-to-path uri))
                    (mapcar (lambda (edit)
                              (eglot--dbind ((TextEdit) range newText) edit
                                (cons (eglot--range-region range t) newText)))
                            edits))))
              documentChanges)))
           (files (delete-dups
                   (mapcar (lambda (item)
                             (buffer-file-name (marker-buffer (caar item))))
                           rdata)))
           (rdata (sort rdata (lambda (a b) (< (caar a) (caar b)))))
           (replace-search-function
            (apply-partially #'eglot-x--search-forward rdata))
           (isearch-search-fun-function
            (lambda ()
              replace-search-function))
           ;; "If QUERY-FLAG is ‘nil’, it replaces all occurrences;
           ;; otherwise, it asks the user what to do about each one."
           ;; There should be a defvar for this.  Better not to reuse
           ;; `eglot-confirm-server-initiated-edits'.
           (query-flag t))
      (if (not rdata)
          (eglot--message "Not found")
        (fileloop-initialize
         files
         (lambda ()
           ;; TODO: check if there's any marker in the buff instead.
           t)
         (lambda ()
           ;; TODO: search buffer from the top, unless its the
           ;;       originating buffer.  orig buffer should be
           ;;       searched from point to the end.  and once again at
           ;;       the end of the fileloop from top to point. or not.
           ;; TODO: check if the lack of goto-char is the culrpit of
           ;;       emacs bug#39224.
           ;;       https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39224
           (goto-char (point-min))
           (perform-replace query (cons 'eglot-x--replace rdata)
                            query-flag nil nil nil multi-query-replace-map)))
        (unwind-protect
            (fileloop-continue)
          (mapc (lambda (d)
                  (set-marker (caar d) nil)
                  (set-marker (cdar d) nil))
                rdata))))))

(defun eglot-x--timer-function (server sel position)
  (when (minibufferp)
    (when (string-equal "SSR:" (buffer-substring 1 5))
      (let ((res
             (condition-case err
                 (and (jsonrpc-request server
                                       :experimental/ssr
                                       (append `(:selections ,sel
                                                 :query ,(minibuffer-contents)
                                                 :parseOnly t)
                                               position))
                      nil)
               (jsonrpc-error err))))
        (when res
          (let ((message-log-max nil)
                (s (cdr (assq 'jsonrpc-error-message res)))
                (intro "^Parse error: "))
            (message "%s" (replace-regexp-in-string intro "" s))))))))

(defvar eglot-x--ssr-history)

(defun eglot-x--read-ssr ()
  (interactive)
  (eglot-x--check-capability :experimental :ssr)
  (let* ((sel
          (if (region-active-p)
              (vector (list :start (eglot--pos-to-lsp-position (region-beginning))
                            :end (eglot--pos-to-lsp-position (region-end))))
            (vector)))
         (position (eglot--TextDocumentPositionParams))
         (timer
          (run-with-idle-timer 1 t #'eglot-x--timer-function
                               (eglot-current-server) sel position)))
    (unwind-protect
        (read-string "SSR: " nil 'eglot-x--ssr-history)
      (when timer
        (cancel-timer timer)))))

;;; Workspace Symbols Filtering
;; https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#workspace-symbols-filtering

(defvar eglot-x-ws-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [remap recenter-top-bottom] #'eglot-x-ws-toggle-scope)
    (define-key map [remap reposition-window] #'eglot-x-ws-toggle-kind)
    map)
  "Keymap used in the minibuffer during `eglot-x-find-workspace-symbol'.")

(defvar eglot-x--ws-args-alist '((scope . workspace)))
(defvar eglot-x--ws-overlay t)

(defun eglot-x-ws-toggle-scope (&optional type vals)
  (interactive)
  (make-local-variable 'eglot-x--ws-overlay)
  ;;(setq eglot-x--ws-args-alist nil)
  (let* ((type (or type 'scope))
         (vals (or vals '((workspaceAndDependencies . "d") (workspace . "w"))))
         (val (if (eq (cdr (assq type eglot-x--ws-args-alist)) (car vals))
                  (cadr vals)
                (car vals))))
    (if (assoc type eglot-x--ws-args-alist)
        (setf (cdr (assoc type eglot-x--ws-args-alist)) val)
      (push (cons type val) eglot-x--ws-args-alist))
    ;; Update prompt
    (let ((flags
           (format " [%s%s]"
                   (or (cddr (assoc 'scope eglot-x--ws-args-alist)) "")
                   (or (cddr (assoc 'kind eglot-x--ws-args-alist)) ""))))
      (unless (overlayp eglot-x--ws-overlay)
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "WS-symbol" nil t)
          (setq eglot-x--ws-overlay
                (make-overlay (match-beginning 0) (match-end 0)))))
      (overlay-put eglot-x--ws-overlay 'after-string flags))))

(defun eglot-x-ws-toggle-kind ()
  (interactive)
  (eglot-x-ws-toggle-scope 'kind '((allSymbols . "a") (onlyTypes . "t"))))

(defun eglot-x--read-workspace-symbol ()
  "Symbol at point is in future history."
  (eglot-x--check-capability :workspaceSymbolProvider)
  (setq eglot-x--ws-args-alist nil)  ; use server's default
  (let ((timer
         (run-with-idle-timer 1 t #'eglot-x--ws-timer-function
                              (current-buffer))))
    (unwind-protect
        (save-window-excursion
          (read-from-minibuffer "WS-symbol: " nil eglot-x-ws-keymap nil nil
                                (symbol-name (symbol-at-point))))
      (when timer
        (cancel-timer timer)))))

(defun eglot-x--ws-xrefs (pattern)
  "Search for workspace symbols matching PATTERN.
Adapted from `eglot--lsp-xref-helper'."
  (unless (eglot--server-capable :workspaceSymbolProvider)
    (eglot--error "Sorry, this server doesn't do %s" :workspaceSymbolProvider))
  (let* ((kind (cadr (assoc 'kind eglot-x--ws-args-alist)))
         (scope (cadr (assoc 'scope eglot-x--ws-args-alist)))
         (extra-params
          (apply #'append
                 ;; search_kind instead of searchKind
                 ;; https://github.com/rust-analyzer/rust-analyzer/issues/11414
                 ;; Issue is fixed, but the commit is not yet in "stable", so
                 ;; use both.
                 (cl-loop for p in (list `(:query ,pattern)
                                         `(:searchKind ,kind)
                                         `(:searchScope ,scope)
                                         `(:search_kind ,kind)
                                         `(:search_scope ,scope))
                          when (cadr p)
                          collect (list (car p)
                                        (format "%s" (cadr p))))))
         (response
          (jsonrpc-request
           (eglot--current-server-or-lose)
           :workspace/symbol
           (append (eglot--TextDocumentPositionParams) extra-params))))
    (eglot--collecting-xrefs (collect)
      (mapc
       (lambda (wss)
         (eglot--dbind ((WorkspaceSymbol) name location) wss
           (eglot--dbind ((LocationWithOptionalRange) uri range) location
             (unless range
               ;; Eglot's helper functions require proper range, xref might not.
               (setq range '(:start (:line 0 :character 0)
                             :end (:line 0 :character 1))))
             (collect (eglot--xref-make-match name uri range)))))
       (if (vectorp response) response (and response (list response)))))))

(defun eglot-x--find-ws (pattern &optional noerror)
  (let ((eglot--lsp-xref-refs (eglot-x--ws-xrefs pattern)))
    (if eglot--lsp-xref-refs
        (xref-find-references "LSP identifier at point.")
      (unless noerror
        (eglot--message "No match for %s" pattern)))))

(defun eglot-x--ws-timer-function (buf)
  (when (minibufferp)
    (when (string-equal "WS-symbol" (buffer-substring 1 10))
      (let ((xref-backend-functions 'eglot-xref-backend)
            (xref-auto-jump-to-first-xref 'show)
            (pattern (minibuffer-contents)))
        (with-current-buffer buf
          (unless (eglot-x--find-ws pattern t)
            (message "No match")))
        (select-window (minibuffer-window) t)))))

(defun eglot-x-find-workspace-symbol (pattern)
  (interactive (list (eglot-x--read-workspace-symbol)))
  (let ((xref-backend-functions 'eglot-xref-backend)
        (xref-auto-jump-to-first-xref 'show)
        (xref-show-xrefs-function 'xref-show-definitions-buffer))
    (eglot-x--find-ws pattern)))

;;; Runnables

(cl-defstruct (xref-loc-runnable
                  (:include xref-item)
                  (:constructor xref-make-loc-runnable (runnable))
                  (:noinline t))
  "An xref location corresponding to a Runnable LSP object."
  runnable)

(defun eglot-x--make-xref-runnable (runnable)
  (eglot--dbind ((Runnable) label) runnable
    (xref-make label (xref-make-loc-runnable runnable))))

(cl-defmethod xref-location-group ((l xref-loc-runnable))
  (eglot--dbind ((Runnable) location)
      (xref-loc-runnable-runnable l)
    (eglot--dbind ((LocationLink) targetUri) location
      (if targetUri
          (eglot--uri-to-path targetUri)
        "workspace"))))

(cl-defmethod xref-location-line ((l xref-loc-runnable))
  (eglot--dbind ((Runnable) location)
      (xref-loc-runnable-runnable l)
    (eglot--dbind ((LocationLink) targetRange) location
      (plist-get (plist-get targetRange :start) :line))))

(cl-defmethod xref-location-marker ((l xref-loc-runnable))
  (eglot--dbind ((Runnable) location args)
      (xref-loc-runnable-runnable l)
    (eglot--dbind ((LocationLink) targetUri targetRange) location
      (let ((file (if targetUri (eglot--uri-to-path targetUri)
                    (plist-get args :workspaceRoot))))
        (with-current-buffer
            (or (get-file-buffer file)
                (let ((find-file-suppress-same-file-warnings t))
                  (find-file-noselect file)))
          (if targetRange
              (eglot--lsp-position-to-point
               (plist-get targetRange :start) t)
            (point-marker)))))))

(defun eglot-x--run-after-jump () ; a complete workout program
  "Run the selected Runnable after an xref jump."
  (eglot--dbind ((Runnable) label args)
      (let ((loc (xref-item-location xref-current-item)))
        (when (xref-loc-runnable-p loc)
          (xref-loc-runnable-runnable loc)))
    (let* ((default-directory (or (plist-get args :workspaceRoot)
                                  default-directory))
           (process-environment
            (append process-environment
                    (when (plist-get args :expectTest)
                      "UPDATE_EXPECT=1")))
           (cargo (or (plist-get args :overrideCargo)
                      "cargo"))
           (cargoExtraArgs (append (plist-get args :cargoExtraArgs) nil))
           (executableArgs (append (plist-get args :executableArgs) nil))
           (compile-command
            (mapconcat #'identity
                       `(,cargo
                         ,@(append (plist-get args :cargoArgs) nil)
                         ,@cargoExtraArgs
                         ,@(if executableArgs
                               `("--" ,@executableArgs)))
                       " "))
           (choice
            (and label
                 (read-multiple-choice
                  (format "[eglot-x] Server wants to run:\n  %s\nProceed? "
                          compile-command)
                  '((?y "yes")
                    (?n "no")
                    (?e "edit" "edit command then run it"))))))
      (when (eq (car choice) ?e)
        (setq compile-command (read-string "" compile-command)))
      (when (member (car choice) '(?e ?y))
          ;; compile-command sets next-error-last-buffer, but xref
          ;; after running its hooks (this defun) reclaims
          ;; next-error-last-buffer.  So:
          (add-hook 'compilation-filter-hook 'eglot-x--set-error-buffer)
          (compile compile-command)))))

(defun eglot-x--set-error-buffer ()
  (setq next-error-last-buffer (current-buffer))
  (remove-hook 'compilation-filter-hook 'eglot-x--set-error-buffer))

;; run after all the other hooks
(add-hook 'xref-after-jump-hook #'eglot-x--run-after-jump 100)

(defun eglot-x-ask-runnables (arg &optional method)
  "Ask server for the list of runnables at point.
With prefix arg request runnables for the whole file."
  (interactive "P")
  (unless method
    (eglot-x--check-capability :experimental :runnables))
  (let* ((res
          (jsonrpc-request (eglot--current-server-or-lose)
                           (or method :experimental/runnables)
                           (if arg
                               `(:textDocument ,(eglot--TextDocumentIdentifier))
                             (eglot--TextDocumentPositionParams))))
         (eglot--lsp-xref-refs
          (mapcar #'eglot-x--make-xref-runnable res)))
    (if eglot--lsp-xref-refs
        (xref-find-references "Runnables at point.")
      (eglot--message "Server returned no runnables."))))

;;; Related tests

(defun eglot-x-ask-related-tests ()
  "Ask server for the list of runnable test at point."
  (interactive)
  (eglot-x-ask-runnables :rust-analyzer/relatedTests))

;;; Server Status

;; Function body copied from `org-plist-delete'.
(defun eglot-x--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defvar eglot-x--remove-hidden-info t)
(defun eglot-x--remove-hidden-info (server-info)
  (if eglot-x--remove-hidden-info
      (eglot-x--plist-delete server-info :eglot-x)
    server-info))

(defun eglot-x--put-in-server (server prop val)
  "Put PROP-VAL into SERVER object just like `plist-put'."
  ;; We can't inherit from eglot-lsp-server and add an additional
  ;; slot, becasue this should work on any object inheriting from
  ;; eglot-lsp-server.  So let's hide PROP-VAL in the eglot-lsp-server
  ;; object.
  (let ((eglot-x--remove-hidden-info nil))
    (setf (eglot--server-info server)
          (plist-put (eglot--server-info server) :eglot-x
                     (plist-put (plist-get (eglot--server-info server) :eglot-x)
                                prop val)))))

(defun eglot-x--get-from-server (server prop)
  "Get PROP from SERVER object just link `plist-get'."
  (let ((eglot-x--remove-hidden-info nil))
    (plist-get (plist-get (eglot--server-info server) :eglot-x)
               prop)))

(advice-add #'eglot--server-info :filter-return #'eglot-x--remove-hidden-info)

(defun eglot-x--hack-mode-line-format (current)
  (pcase-let ((`(,health ,quiescent ,message)
               (eglot-x--get-from-server (eglot--current-server-or-lose)
                                         :server-status)))
    (if (or (not health)
            (and (equal "ok" health)
                 quiescent))
        current
      `(,(funcall (if (not quiescent) #'upcase #'identity)
                  (propertize
                   (substring health 0 1)
                   'help-echo (format "Server status: %s%s\n%s"
                                      (if quiescent "quiescent " "")
                                      health message)
                   'face (pcase health
                           ("warning" 'face 'warning)
                           ("error"  'face 'error)
                           (_))))
        ,@current))))

(advice-add #'eglot--mode-line-format :filter-return
            #'eglot-x--hack-mode-line-format)

(cl-defmethod eglot-handle-notification
  (server (_method (eql experimental/serverStatus))
           &key health quiescent message)
  "Handle notification experimental/serverStatus."
  (eglot-x--put-in-server server :server-status (list health quiescent message))
  (force-mode-line-update))

;;; Inlay Hints
(defun eglot-x-insert-inlay-hint-at-point ()
  "Request and insert inlay-hint for symbol at point.
Or the first hint of the active region."
  (interactive)
  (eglot-x--check-capability :experimental :inlayHints)
  (pcase-let*
      ((insert-pos (cdr (bounds-of-thing-at-point 'symbol)))
       (`(,beg . ,end)
        (if (region-active-p)
            (cons (region-beginning) (region-end))
          ;; Rust-analyzer only sends the hint if the region contains
          ;; "mut n" in "let mut n = 5;", so bounds-of-thing-at-point
          ;; doesn't work here.
          (cons (line-beginning-position) (line-end-position))))
       (res
        (jsonrpc-request (eglot--current-server-or-lose)
                         :experimental/inlayHints
                         `(:range ,(list :start (eglot--pos-to-lsp-position beg)
                                         :end (eglot--pos-to-lsp-position end))
                                  :textDocument ,(eglot--TextDocumentIdentifier))))
       (inserted nil))
    (cl-loop for hint in (append res nil)
             do (eglot--dbind ((InlayHint) label position) hint
                  (let ((pos (eglot--lsp-position-to-point position)))
                    (when (or (eq insert-pos pos)
                              (region-active-p))
                      (save-excursion
                        (goto-char pos)
                        (insert label)
                        (setq inserted t)))))
             when inserted return nil
             finally do (user-error "[eglot-x] Server sent no inlay hints"))))

(provide 'eglot-x)
;;; eglot-x.el ends here
