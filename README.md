# Protocol extensions for Eglot

[Eglot][eglot] aims to support the Language Server Protocol, but none
of its unofficial extensions.  Eglot-x adds support for some of these
protocol extensions.

If you find a bug in Eglot, please, try to reproduce it without
Eglot-x, because Eglot-x substantially modifies Eglot's normal
behavior as well.

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

This [extension][xfiles] allows the client and the server to have
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
- [Local Documentation]: see variable `eglot-x-enable-local-docs-support`.
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

- Open Server Logs: LSP servers can ask the client to show their logs,
  see variable `eglot-x-enable-open-server-logs` for details.

### rust-analyzer specific extensions
- [Expand Macro]: see defun `eglot-x-expand-macro`.
- [Related Tests]: see defun `eglot-x-ask-related-tests`.
- [Reload Workspace]: see defun `eglot-x-reload-workspace`.
- [Rebuild proc-macros]: see defun `eglot-x-rebuild-proc-macros`.
- [Flycheck commands]: see defuns `eglot-x-run-flycheck`,
  `eglot-x-clear-flycheck`, and `eglot-x-cancel-flycheck`.  (These
  commands implement lsp-extensions and have nothing to do with the
  flycheck Emacs package.)
- [View Crate Graph]: see variable `eglot-x-graph-type` and defun `eglot-x-view-crate-graph`.

  ![crate-graph](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/view-crate-graph.png)

  You can also jump to the crate (Cargo.toml) if the graph-type is not 'svg':

  ![crate-graph-2](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/view-crate-graph-ascii.png)

  (This command requires [graphviz]/[graph-easy].)
- [Dependency Tree]: see defun `eglot-x-find-crate`.

- [View Recursive Memory Layout]: the command
  `eglot-x-view-recursive-memory-layout` shows the memory layout of
  the thing under point.

  ![view-recursive-memory-layout-1](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/view-recursive-memory-layout-1.png)

  Tooltips show the column names: offset, size, alignement, number of
  gaps, and gap-size.  The output format is inspired by [pahole],
  which (I think) more practical than the
  [upstream graphical output][Upstream memory layout]:

  ![view-recursive-memory-layout-2](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/view-recursive-memory-layout-2.png)

  The built-in outline-minor-mode helps to naviagate / understand a
  recursive layout.

  A better [layout optimization approach][lwn955709] relies on
  run-time statistics as well, which this command does not provide.

### rust-analyzer specific extensions for debugging rust-analyzer
- [Analyzer Status]: see defun `eglot-x-analyzer-status`.
- [Syntax Tree]: see defun `eglot-x-show-syntax-tree`.

  ![syntax-tree](https://raw.githubusercontent.com/wiki/nemethf/eglot-x/syntax-tree.png)

- [View Hir]: see defun `eglot-x-view-hir`.
- [View Mir]: see defun `eglot-x-view-mir`.
- [Interpret Function]: see defun `eglot-x-interpret-function`.
- [View File Text]: see defun `eglot-x-debug-file-sync-problems`.
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

[Analyzer Status]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#analyzer-status
[Colored diagnostics]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#colored-diagnostic-output
[Dependency Tree]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#dependency-tree
[Expand Macro]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#expand-macro
[Flycheck commands]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#controlling-flycheck
[Interpret Function]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#interpret-function
[Join Lines]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#join-lines
[Local Documentation]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#local-documentation
[Matching Brace]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#matching-brace
[Move Item]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#move-item
[On Enter]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#on-enter
[Open External Documentation]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#open-external-documentation
[Rebuild proc-macros]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#rebuild-proc-macros
[Related Tests]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#related-tests
[Reload Workspace]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#reload-workspace
[Runnables]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#runnables
[Server Status]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#server-status
[Snippet TextEdits]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#snippet-textedit
[Syntax Tree]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#syntax-tree
[Taplo]: https://github.com/tamasfe/taplo/tree/master/crates/taplo-lsp/src/lsp_ext
[Upstream memory layout]: https://github.com/rust-lang/rust-analyzer/pull/15081
[View Crate Graph]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#view-crate-graph
[View File Text]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#view-file-text
[View Hir]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#view-hir
[View Mir]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#view-mir
[View Recursive Memory Layout]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#view-recursive-memory-layout
[Workspace Symbols Filtering]: https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#workspace-symbols-filtering

[ccls-refs]: https://github.com/MaskRay/ccls/wiki/LSP-Extensions
[eglot]: https://github.com/joaotavora/eglot/
[gpl]: COPYING
[graph-easy]: https://metacpan.org/dist/Graph-Easy
[graphviz]: https://graphviz.org/
[lwn955709]: https://lwn.net/Articles/955709/
[pahole]: https://manpages.ubuntu.com/manpages/jammy/man1/pahole.1.html
[ssr]: https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/lsp-extensions.md#structural-search-replace-ssr
[utf-8-offsets]: https://clangd.github.io/extensions.html#utf-8-offsets
[xfiles]: https://github.com/sourcegraph/language-server-protocol/blob/master/extension-files.md
