# Protocol extensions for Eglot

[Eglot][eglot] supports (a subset of) the Language Server Protocol.
However, there are useful protocol extensions that are not (yet) part
of the official protocol specification.  Eglot-x adds support for some
of them.  If you find a bug in Eglot, please, try to repoduce it
without Eglot-x, because Eglot-x is substantially modifies Eglot's
normal behavior as well.

Add the following lines to your init file to enable eglot-x

```elisp
    (with-eval-after-load 'eglot
      (require 'eglot-x)
      (eglot-x-setup))
```
To adjust which extensions are enabled:

```
    M-x customize-group RET eglot-x RET
```

# Supported extensions

## Files extension

The [extension][xfiles] allows the client and the server to have
separate file systems.  For example, the server can run inside a
Docker container, or the source code can be on a remote system
accessed by Tramp.  The client can send files to the server only from
the result of `project-files`.  The list of eligible files can further
limited by `eglot-x-files-visible-regexp` and
`eglot-x-files-hidden-regexp`.  This feature works if `project-roots`
and `project-external-roots` are set correctly.

Enabling extension disables Eglot's built-in support for Tramp
files.

## Additional reference methods

The command `eglot-x-find-refs' is the entry point for the extra
methods.  You can bind it to a key:

```elisp
    (define-key eglot-mode-map (kbd "s-.") #'eglot-x-find-refs)
```

Currently, the [`ccls`][ccls-refs] and `rust-analyzer` are the only
servers whose extra reference methods eglot-x supports.

## Encoding negotiation

[The extension][utf-8-offsets] allows the client and the server to
negotiate a proper encoding to be used in transmitting column
positions.

## rust-analyzer extensions

### experimental extensions

- [Snippet TextEdits]: see variable `eglot-x-enable-snippet-text-edit`.
- [Join Lines]: see defun `eglot-x-join-lines`.
- [Move Item]: see defun `eglot-x-move-item-down` and `eglot-x-move-item-up`.
- [On Enter]: see defun `eglot-x-on-enter`.
- [Matching Brace]: see `eglot-x-matching-brace`.
  However, emacs' own `backward-sexp`, and `forward-sexp` seem to be
  more useful.
- [Open External Documentation]: see defun `eglot-x-open-external-documentation`.
- [Structural Search Replace (SSR)][ssr]: see defun `eglot-x-structural-search-replace`.

  The server checks the correctness of the query while you type:

  ![SSR](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/ssr.png)

  The replacement process is similar to `query-replace`:

  ![SSR2](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/ssr-2.png)

- [Workspace Symbols Filtering]: see defun `eglot-x-find-workspace-symbol`.

  The xref buffer shows the current matches while the user iteratively
  types the query and another buffer shows the location of the first
  match.

  ![ws-symbol](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/ws-symbol.png)

  Additional input refines the results and the point in main.rs is
  changed once again to the location of the new first result:

  ![ws-symbol-2](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/ws-symbol-2.png)

  You can change the search scope and kind with `C-l` and `C-M-l`,
  respectively.  See variable `eglot-x-ws-keymap`.  The non-default
  settings are shown between braces:

  ![ws-symbol-3](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/ws-symbol-3.png)

  Currently, the xref buffer shows the first lines of the matches.
  This is not always helpful.

- [Runnables]: see defun `eglot-x-ask-runnables`.

  Results for the whole buffer:

  ![runnables-1](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/runnables.png)

  Selecting the first "runnable":

  ![runnables-2](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/runnables-2.png)

  The "runnable" is executed as a `compile-command`:

  ![runnables-3](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/runnables-3.png)

- [Server Status]: see variable `eglot-x-enable-server-status`.
  The mode-line displays the status unless it is "permanently OK".

- [Colored diagnostics]: see variable `eglot-x-enable-colored-diagnostics`.

  `flymake-goto-next-error' shows a colored diagnostic message:

  ![colored-diags](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/colored-diags.png)

  (The ansi-color.el of Emacs 27 is too old for this feature, Emacs 29 is OK.)

### rust-analyzer specific extensions
- [Expand Macro]: see defun `eglot-x-expand-macro`.
- [Related Tests]: see defun `eglot-x-ask-related-tests`.
- [Reload Workspace]: see defun `eglot-x-reload-workspace`.
- [Flycheck commands]: see defuns `eglot-x-run-flycheck`, `eglot-x-clear-flycheck`,
  and `eglot-x-cancel-flycheck`.
- [View Crate Graph]: see variable `eglot-x-graph-type` and defun `eglot-x-view-crate-graph`.

  ![crate-graph](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/view-crate-graph.png)

  You can also jump to the crate (Cargo.toml) if the graph-type is not 'svg':

  ![crate-graph-2](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/view-crate-graph-ascii.png)

### rust-analyzer specific extensions for debugging rust-analyzer
- [Analyzer Status]: see defun `eglot-x-analyzer-status`.
- [Syntax Tree]: see defun `eglot-x-show-syntax-tree`.

  ![syntax-tree](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/syntax-tree.png)

- [View Hir]: see defun `eglot-x-view-hir`.
- (Memory Usage): see defun `eglot-x-memory-usage`.

## Taplo specific extensions

Relying on taplo/associatedSchema and taplo/listSchemas
[extensions][Taplo] eglot-x provides the following commands:

- `eglot-x-taplo-show-associated-schema`
- `eglot-x-taplo-find-associated-schema`
- `eglot-x-taplo-list-schemas`

  ![taplo-menu](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/taplo-menu.png)

# Other

Eglot-x provides `ff-find-related-file` backends for some LSP servers.
See the documentation of `eglot-x-enable-ff-related-file-integration`.

# License

[GPLv3+][gpl]

[Analyzer Status]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#analyzer-status
[Colored diagnostics]: https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#colored-diagnostic-output
[Expand Macro]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#expand-macro
[Join Lines]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#join-lines
[Matching Brace]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#matching-brace
[Move Item]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#move-item
[On Enter]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#on-enter
[Open External Documentation]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#open-external-documentation
[Related Tests]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#related-tests
[Reload Workspace]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#reload-workspace
[Flycheck commands]: https://github.com/rust-lang/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#controlling-flycheck
[Runnables]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#runnables
[Server Status]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#server-status
[Snippet TextEdits]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#snippet-textedit
[Syntax Tree]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#syntax-tree
[Taplo]: https://github.com/tamasfe/taplo/tree/master/crates/taplo-lsp/src/lsp_ext
[View Crate Graph]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#view-crate-graph
[View Hir]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#view-hir
[Workspace Symbols Filtering]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#workspace-symbols-filtering
[ccls-refs]: https://github.com/MaskRay/ccls/wiki/LSP-Extensions
[eglot]: https://github.com/joaotavora/eglot/
[gpl]: COPYING
[ssr]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#structural-search-replace-ssr
[utf-8-offsets]: https://clangd.github.io/extensions.html#utf-8-offsets
[xfiles]: https://github.com/sourcegraph/language-server-protocol/blob/master/extension-files.md
