# Protocol extensions for Eglot

[Eglot][eglot] supports (a subset of) the Language Server Protocol.
However, there are useful protocol extensions that are not (yet) part
of the official protocol specification.  Eglot-x adds support for some
of them.

Add the following line to your init file to enable eglot-x

```elisp
    (with-eval-after-load 'eglot (require 'eglot-x))
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
accessed by Tramp.  (With emacs-26, the latter is not supported.)  The
client can send files to the server only from the result of
`project-files`.  The list of eligible files can further limited by
`eglot-x-files-visible-regexp` and `eglot-x-files-hidden-regexp`.
This feature works if `project-roots` and `project-external-roots` are
set correctly.  (project-files was introduced in emacs-27; eglot-x
backports the implementation to emacs-26.)

## Addtional reference methods

The command `eglot-x-find-refs' is the entry point for the extra
methods.  You can bind it to a key:

```elisp
    (define-key eglot-mode-map (kbd "s-.") #'eglot-x-find-refs)
```

Currently, the `ccls` is the only server whose [extra reference
methods][ccls-refs] eglot-x supports.

# License

[GPLv3+][gpl]

[eglot]: https://github.com/joaotavora/eglot/
[xfiles]: https://github.com/sourcegraph/language-server-protocol/blob/master/extension-files.md
[gpl]: COPYING
[ccls-refs]: https://github.com/MaskRay/ccls/wiki/LSP-Extensions