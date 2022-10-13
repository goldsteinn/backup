(load-file "~/.emacs.d/snippets/lsp-default-config.el") 
;; diagnostics Settings

;; When non-nil, clean the diagnostics on change.
(setq lsp-diagnostic-clean-after-change nil)
;; The Attributes used on the diagnostics. The Attributes used on the
;; diagnostics. List containing (tag attributes) where tag is the LSP
;; diagnostic tag and attributes is a plist containing face attributes
;; which will be applied on top the flycheck face for that error
;; level.
(setq lsp-diagnostics-attributes ((unnecessary :foreground gray) (deprecated :strike-through t)))
;; A list of major models for which lsp-diagnostics-mode should be
;; disabled.
(setq lsp-diagnostics-disabled-modes nil)
;; Error level to use when the server does not report back a
;; diagnostic level.
(setq lsp-diagnostics-flycheck-default-level error)
;; The checker backend provider.
(setq lsp-diagnostics-provider :auto)
;; dired Settings

;; Non-nil if Lsp-Dired mode is enabled. Non-nil if Lsp-Dired mode is
;; enabled. See the lsp-dired-mode command for a description of this
;; minor mode. Setting this variable directly does not take effect;
;; either customize it (see the info node Easy Customization) or call
;; the function lsp-dired-mode.
(setq lsp-dired-mode nil)
;; Face used for breadcrumb paths on headerline when there is an error
;; under that path
(setq lsp-dired-path-error-face ((t :underline (:style wave :color Red1))))
;; Face used for breadcrumb paths on headerline.
(setq lsp-dired-path-face ((t :inherit font-lock-string-face)))
;; Face used for breadcrumb paths on headerline when there is an hint
;; under that path
(setq lsp-dired-path-hint-face ((t :underline (:style wave :color Green))))
;; Face used for breadcrumb paths on headerline when there is an info
;; under that path
(setq lsp-dired-path-info-face ((t :underline (:style wave :color Green))))
;; Face used for breadcrumb paths on headerline when there is an
;; warning under that path
(setq lsp-dired-path-warning-face ((t :underline (:style wave :color Yellow))))
;; completion Settings

;; Enable completion-at-point integration.
(setq lsp-completion-enable nil)
;; Whether or not to apply additional text edit when performing
;; completion.
(setq lsp-completion-enable-additional-text-edit nil)
;; Whether or not filter incomplete results.
(setq lsp-completion-filter-on-incomplete nil)
;; Whether or not caching the returned completions from server.
(setq lsp-completion-no-cache nil)
;; The completion backend provider.
(setq lsp-completion-provider :none)
;; Whether or not to show detail of completion candidates.
(setq lsp-completion-show-detail t)
;; Whether or not to show kind of completion candidates.
(setq lsp-completion-show-kind t)
;; Whether or not filter initial results from server.
(setq lsp-completion-sort-initial-results t)
;; Temporarily use last server result when interrupted by keyboard.
;; Temporarily use last server result when interrupted by keyboard.
;; This will help minimize popup flickering issue in company-mode.
(setq lsp-completion-use-last-result t)
;; Enable/disable snippet completion support.
(setq lsp-enable-snippet nil)
;; modeline Settings

;; Define what should display on the modeline when code actions are
;; available.
(setq lsp-modeline-code-action-fallback-icon 💡)
;; Whether to show code actions on modeline.
(setq lsp-modeline-code-actions-enable nil)
;; Face used to code action text on modeline.
(setq lsp-modeline-code-actions-face ((t :inherit homoglyph)))
;; Regex for the code actions kinds to show in the modeline.
(setq lsp-modeline-code-actions-kind-regex $\|quickfix.*\|refactor.*)
;; Face used to code action text on modeline.
(setq lsp-modeline-code-actions-preferred-face ((t :foreground yellow)))
;; Define what should display on the modeline when code actions are
;; available.
(setq lsp-modeline-code-actions-segments (count icon))
;; Whether to show diagnostics on modeline.
(setq lsp-modeline-diagnostics-enable nil)
;; The modeline diagnostics scope.
(setq lsp-modeline-diagnostics-scope :workspace)
;; Whether to show workspace status on modeline.
(setq lsp-modeline-workspace-status-enable nil)
;; lens Settings

;; Debounce interval for loading lenses.
(setq lsp-lens-debounce-interval 0.001)
;; Auto enable lenses if server supports.
(setq lsp-lens-enable nil)
;; The face used for code lens overlays.
(setq lsp-lens-face ((t :inherit lsp-details-face)))
;; The face used for code lens overlays.
(setq lsp-lens-mouse-face ((t :height 0.8 :inherit link)))
;; The position to place lens relative to returned lens position.
(setq lsp-lens-place-position end-of-line)
;; headerline Settings

;; Face used on breadcrumb deprecated text on modeline.
(setq lsp-headerline-breadcrumb-deprecated-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :strike-through t)))
;; Whether to enable breadcrumb on headerline.
(setq lsp-headerline-breadcrumb-enable nil)
;; If non-nil, apply different face on the breadcrumb based on the
;; errors.
(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
;; Whether to label symbols with numbers on the breadcrumb.
(setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;; Face used for breadcrumb paths on headerline when there is an error
;; under that path
(setq lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color Red1) :inherit lsp-headerline-breadcrumb-path-face)))
;; Face used for breadcrumb paths on headerline.
(setq lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face)))
;; Face used for breadcrumb paths on headerline when there is an hint
;; under that path
(setq lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color Green) :inherit lsp-headerline-breadcrumb-path-face)))
;; Face used for breadcrumb paths on headerline when there is an info
;; under that path
(setq lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color Green) :inherit lsp-headerline-breadcrumb-path-face)))
;; Face used for breadcrumb paths on headerline when there is an
;; warning under that path
(setq lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color Yellow) :inherit lsp-headerline-breadcrumb-path-face)))
;; Face used for breadcrumb prefix on headerline. Face used for
;; breadcrumb prefix on headerline. Only if lsp-headerline-breadcrumb-
;; prefix is project-name-only.
(setq lsp-headerline-breadcrumb-project-prefix-face ((t :inherit font-lock-string-face :weight bold)))
;; Segments used in breadcrumb text on headerline.
(setq lsp-headerline-breadcrumb-segments (path-up-to-project file symbols))
;; Face used for breadcrumb separator on headerline.
(setq lsp-headerline-breadcrumb-separator-face ((t :inherit shadow :height 0.8)))
;; Face used for breadcrumb symbols text on headerline when there Face
;; used for breadcrumb symbols text on headerline when there is an
;; error in symbols range.
(setq lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color Red1))))
;; Face used for breadcrumb symbols text on headerline.
(setq lsp-headerline-breadcrumb-symbols-face ((t :inherit font-lock-doc-face :weight bold)))
;; Face used for breadcrumb symbols text on headerline when there Face
;; used for breadcrumb symbols text on headerline when there is an
;; hints in symbols range.
(setq lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color Green))))
;; Face used for breadcrumb symbols text on headerline when there Face
;; used for breadcrumb symbols text on headerline when there is an
;; info in symbols range.
(setq lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color Green))))
;; Face used for breadcrumb symbols text on headerline when there Face
;; used for breadcrumb symbols text on headerline when there is an
;; warning in symbols range.
(setq lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color Yellow))))
;; Face used for breadcrumb prefix on headerline. Face used for
;; breadcrumb prefix on headerline. Only if lsp-headerline-breadcrumb-
;; prefix is project-name-only.
(setq lsp-headerline-breadcrumb-unknown-project-prefix-face ((t :inherit shadow :weight bold)))
;; ido Settings

;; Whether to show the project-relative path to a symbol`s point of
;; definition.
(setq lsp-ido-show-symbol-filename t)
;; Whether to show the symbol`s kind when showing lsp symbols.
(setq lsp-ido-show-symbol-kind t)
;; A vector of 26 itens representing the SymbolKind.
(setq lsp-ido-symbol-kind-to-string [     File Modu Nmsp Pack Clss Meth Prop Fld  Cons Enum Intf Func Var  Cnst Str  Num  Bool  Arr  Obj  Key  Null EmMm Srct Evnt Op   TPar])
;; iedit Settings

;; core Settings

;; Hooks to run when text edit is applied. Hooks to run when text edit
;; is applied. It contains the operation source.
(setq lsp-after-apply-edits-hook nil)
;; Hooks to run after diagnostics are received.
(setq lsp-after-diagnostics-hook nil)
;; List of functions to be called after a Language Server has been
;; initialized List of functions to be called after a Language Server
;; has been initialized for a new workspace.
(setq lsp-after-initialize-hook nil)
;; List of functions to be called after a new file with LSP support is
;; opened.
(setq lsp-after-open-hook nil)
;; List of functions to be called after a Language Server has been
;; uninitialized.
(setq lsp-after-uninitialized-functions nil)
;; Auto configure lsp-mode main features. Auto configure lsp-mode main
;; features. When set to t lsp-mode will auto-configure completion,
;; code-actions, breadcrumb, flycheck, flymake, imenu, symbol
;; highlighting, lenses, links, and so on.
(setq lsp-auto-configure t)
;; Auto-execute single action.
(setq lsp-auto-execute-action t)
;; Automatically guess the project root using projectile/project.
;; Automatically guess the project root using projectile/project. Do
;; not use this setting unless you are familiar with lsp-mode
;; internals and you are sure that all of your projects are following
;; projectile/project.el conventions.
(setq lsp-auto-guess-root nil)
;; Shutdown or restart a single workspace. Shutdown or restart a
;; single workspace. If set and the current buffer has only a single
;; workspace associated with it, lsp-shutdown-workspace and lsp-
;; restart-workspace will act on it without asking.
(setq lsp-auto-select-workspace t)
;; If non-nil ensure the files exist before sending If non-nil ensure
;; the files exist before sending textDocument/didOpen notification.
(setq lsp-auto-touch-files t)
;; Hooks to run before applying edits.
(setq lsp-before-apply-edits-hook nil)
;; List of functions to be called before a Language Server has been
;; initialized List of functions to be called before a Language Server
;; has been initialized for a new workspace.
(setq lsp-before-initialize-hook nil)
;; List of functions to be called before a new file with LSP support
;; is opened.
(setq lsp-before-open-hook nil)
;; If non-nil, lsp-mode will apply edits suggested by the language
;; server If non-nil, lsp-mode will apply edits suggested by the
;; language server before saving a document.
(setq lsp-before-save-edits t)
;; List of the clients to be automatically required.
;(setq lsp-client-packages (ccls lsp-actionscript lsp-ada lsp-angular lsp-bash lsp-beancount lsp-clangd lsp-clojure lsp-cmake lsp-crystal lsp-csharp lsp-css lsp-d lsp-dart lsp-dhall lsp-docker lsp-dockerfile lsp-elm lsp-elixir lsp-erlang lsp-eslint lsp-fortran lsp-fsharp lsp-gdscript lsp-go lsp-graphql lsp-hack lsp-grammarly lsp-groovy lsp-haskell lsp-haxe lsp-java lsp-javascript lsp-json lsp-kotlin lsp-latex lsp-ltex lsp-lua lsp-markdown lsp-nginx lsp-nim lsp-nix lsp-metals lsp-mssql lsp-ocaml lsp-pascal lsp-perl lsp-php lsp-pwsh lsp-pyls lsp-pylsp lsp-pyright lsp-python-ms lsp-purescript lsp-r lsp-remark lsp-rf lsp-rust lsp-solargraph lsp-sorbet lsp-sourcekit lsp-sonarlint lsp-tailwindcss lsp-tex lsp-terraform lsp-toml lsp-v lsp-vala lsp-verilog lsp-vetur lsp-vhdl lsp-vimscript lsp-xml lsp-yaml lsp-sqls lsp-svelte lsp-steep lsp-zig))
;; Default behaviour of InsertReplaceEdit.
(setq lsp-completion-default-behaviour :none)
;; Hooks to run when lsp-configure-buffer is called.
;(setq lsp-configure-hook ((closure (flymake-mode flymake-diagnostic-functions lsp-diagnostics-mode flycheck-checkers flycheck-checker flycheck-check-syntax-automatically flycheck-mode t) nil (if lsp-auto-configure (progn (lsp-diagnostics--enable)))) (closure (company-abort-on-unique-match company-backends yas-indent-line t) nil (if (and lsp-auto-configure lsp-completion-enable) (progn (lsp-completion--enable))))))
;; If non-nil debounce full sync events. If non-nil debounce full sync
;; events. This flag affects only servers which do not support
;; incremental updates.
(setq lsp-debounce-full-sync-notifications t)
;; Time to wait before sending full sync synchronization after buffer
;; modification.
(setq lsp-debounce-full-sync-notifications-interval 1.0)
;; Default error handler customization. Default error handler
;; customization. Handler should give METHOD as argument and return
;; function of one argument ERROR.
(setq lsp-default-create-error-handler-fn nil)
;; Used to display additional information troughout lsp. Used to
;; display additional information troughout lsp. Things like line
;; numbers, signatures, ... are considered additional information.
;; Often, additional faces are defined that inherit from this face by
;; default, like lsp-signature-face, and they may be customized for
;; finer control.
(setq lsp-details-face ((t :height 0.8 :inherit shadow)))
;; Hooks to run after diagnostics are received.
(setq lsp-diagnostics-updated-hook nil)
;; A list of disabled/blacklisted clients. A list of
;; disabled/blacklisted clients. Each entry in the list can be either:
;; a symbol, the server-id for the LSP client, or a cons pair (MAJOR-
;; MODE . CLIENTS), where MAJOR-MODE is the major-mode, and CLIENTS is
;; either a client or a list of clients.
(setq lsp-disabled-clients nil)
;; Showing inline image or not.
(setq lsp-display-inline-image t)
;; How to sync the document with the language server.
(setq lsp-document-sync-method nil)
;; If non-nil, eldoc will display hover info when it is present.
(setq lsp-eldoc-enable-hover nil)
;; Hooks to run for eldoc.
(setq lsp-eldoc-hook (lsp-hover))
;; Display all of the info returned by document/onHover. Display all
;; of the info returned by document/onHover. If this is set to nil,
;; eldoc will show only the symbol information.
(setq lsp-eldoc-render-all nil)
;; If non-nil, enable dap-auto-configure-mode.
(setq lsp-enable-dap-auto-configure nil)
;; If non-nil lsp-mode will watch the files in the workspace if If
;; non-nil lsp-mode will watch the files in the workspace if the
;; server has requested that.
(setq lsp-enable-file-watchers nil)
;; Enable/disable code folding support.
(setq lsp-enable-folding nil)
;; If non-nil, automatically enable imenu integration when server
;; provides If non-nil, automatically enable imenu integration when
;; server provides textDocument/documentSymbol.
(setq lsp-enable-imenu nil)
;; Indent regions using the file formatting functionality provided by
;; the Indent regions using the file formatting functionality provided
;; by the language server.
(setq lsp-enable-indentation nil)
;; If non-nil, all references to links in a file will be made
;; clickable, if If non-nil, all references to links in a file will be
;; made clickable, if supported by the language server.
(setq lsp-enable-links nil)
;; Enable textDocument/onTypeFormatting integration.
(setq lsp-enable-on-type-formatting nil)
;; When non-nil enable server downloading suggestions.
(setq lsp-enable-suggest-server-download nil)
;; Highlight references of the symbol at point.
(setq lsp-enable-symbol-highlighting nil)
;; Enable textDocument/documentColor integration.
(setq lsp-enable-text-document-color nil)
;; Enable xref integration.
(setq lsp-enable-xref t)
;; Face used for highlighting symbols being read.
(setq lsp-face-highlight-read ((t :inherit highlight :underline t)))
;; Face used for textual occurrences of symbols.
(setq lsp-face-highlight-textual ((t :inherit highlight)))
;; Face used for highlighting symbols being written to.
(setq lsp-face-highlight-write ((t :inherit highlight :weight bold)))
;; Face used to highlight the identifier being renamed. Face used to
;; highlight the identifier being renamed. Renaming can be done using
;; lsp-rename.
(setq lsp-face-rename ((t :underline t)))
;; List of regexps matching directory paths which won`t be monitored
;; when List of regexps matching directory paths which won`t be
;; monitored when creating file watches. Customization of this
;; variable is only honored at the global level or at a root of an lsp
;; workspace.
(setq lsp-file-watch-ignored-directories ([/\\]\.git\' [/\\]\.github\' [/\\]\.circleci\' [/\\]\.hg\' [/\\]\.bzr\' [/\\]_darcs\' [/\\]\.svn\' [/\\]_FOSSIL_\' [/\\]\.idea\' [/\\]\.ensime_cache\' [/\\]\.eunit\' [/\\]node_modules [/\\]\.yarn\' [/\\]\.fslckout\' [/\\]\.tox\' [/\\]dist\' [/\\]dist-newstyle\' [/\\]\.stack-work\' [/\\]\.bloop\' [/\\]\.metals\' [/\\]target\' [/\\]\.ccls-cache\' [/\\]\.vscode\' [/\\]\.venv\' [/\\]\.mypy_cache\' [/\\]\.deps\' [/\\]build-aux\' [/\\]autom4te.cache\' [/\\]\.reference\' [/\\]\.lsp\' [/\\]\.clj-kondo\' [/\\]\.shadow-cljs\' [/\\]\.babel_cache\' [/\\]\.cpcache\' [/\\]\checkouts\' [/\\]\.m2\' [/\\]bin/Debug\' [/\\]obj\' [/\\]_opam\' [/\\]_build\' [/\\]\.elixir_ls\' [/\\]\.direnv\'))
;; List of regexps matching files for which change events will List of
;; regexps matching files for which change events will not be sent to
;; the server.
(setq lsp-file-watch-ignored-files ([/\\]flycheck_[^/\\]+\' [/\\]\.#[^/\\]+\' [/\\][^/\\]+~\'))
;; Show warning if the files to watch are more than. Show warning if
;; the files to watch are more than. Set to nil to disable the
;; warning.
(setq lsp-file-watch-threshold 1000)
;; If non-nil, only fold complete lines.
(setq lsp-folding-line-folding-only nil)
;; The maximum number of folding ranges to receive from the language
;; server.
(setq lsp-folding-range-limit nil)
;; The script to decompress a gzipped file. The script to decompress a
;; gzipped file. Should be a format string with one argument for the
;; file to be decompressed in place.
(setq lsp-gunzip-script gzip -d %1$s)
;; Debounce interval for after-change-functions.
(setq lsp-idle-delay 0.5)
;; Face used for installation buffers still in progress. Face used for
;; installation buffers still in progress. Used in lsp-select-
;; installation-buffer.
(setq lsp-installation-buffer-face ((t :foreground green)))
;; Face used for finished installation buffers. Face used for finished
;; installation buffers. Used in lsp-select-installation-buffer.
(setq lsp-installation-finished-buffer-face ((t :foreground orange)))
;; Maximum number of messages that can be locked in a lsp-io buffer.
(setq lsp-io-messages-max t)
;; If non nil keep workspace alive when the last workspace buffer is
;; closed.
(setq lsp-keep-workspace-alive t)
;; LSP-mode keymap prefix.
(setq lsp-keymap-prefix s-l)
;; If non-nil, log all messages from the language server to a lsp-log
;; buffer.
(setq lsp-log-io nil)
;; The methods to filter before print to lsp-log-io.
(setq lsp-log-io-allowlist-methods nil)
;; Maximum number of lines to keep in the log buffer. Maximum number
;; of lines to keep in the log buffer. If nil, disable message
;; logging. If t, log messages but don`t truncate the buffer when it
;; becomes large.
(setq lsp-log-max 1000)
;; Hooks to run when buffer has changed.
(setq lsp-on-change-hook nil)
;; Hooks to run after lsp-idle-delay.
(setq lsp-on-idle-hook nil)
;; Display the char represent the document color in overlay
(setq lsp-overlay-document-color-char ■)
;; Function for handling the progress notifications.
(setq lsp-progress-function lsp-on-progress-modeline)
;; Progress prefix.
(setq lsp-progress-prefix ⌛)
;; If non-nil, display LSP $/progress reports via a spinner in the
;; modeline.
(setq lsp-progress-via-spinner t)
;; Face used to display the rename placeholder in. Face used to
;; display the rename placeholder in. When calling lsp-rename
;; interactively, this will be the face of the new name.
(setq lsp-rename-placeholder-face ((t :inherit font-lock-variable-name-face)))
;; Whether lsp-rename should do a prepareRename first. Whether lsp-
;; rename should do a prepareRename first. For some language servers,
;; textDocument/prepareRename might be too slow, in which case this
;; variable may be set to nil. lsp-rename will then use thing-at-point
;; symbol to determine the symbol to rename at point.
(setq lsp-rename-use-prepare t)
;; Number of seconds to wait for a response from the language server
;; before Number of seconds to wait for a response from the language
;; server before timing out. Nil if no timeout.
(setq lsp-response-timeout 10)
;; Defines how server-exited events must be handled.
(setq lsp-restart interactive)
;; Directory in which the servers will be installed.
(setq lsp-server-install-dir ~/.emacs.d/.cache/lsp)
;; Request tracing on the server side. Request tracing on the server
;; side. The actual trace output at each level depends on the language
;; server in use. Changes take effect only when a new session is
;; started.
(setq lsp-server-trace nil)
;; File where session information is stored.
(setq lsp-session-file ~/.emacs.d/.lsp-session-v1)
;; Auto activate signature conditions.
(setq lsp-signature-auto-activate (:on-trigger-char :on-server-request))
;; Whether lsp-signature-next and prev should cycle.
(setq lsp-signature-cycle t)
;; If number, limit the number of lines to show in the docs.
(setq lsp-signature-doc-lines 20)
;; Used to display signatures in imenu, ....
(setq lsp-signature-face ((t :inherit lsp-details-face)))
;; The function used for displaying signature info. The function used
;; for displaying signature info. It will be called with one param -
;; the signature info. When called with nil the signature info must be
;; cleared.
(setq lsp-signature-function lsp-lv-message)
;; Background and foreground for lsp-signature-posframe.
(setq lsp-signature-posframe ((t :inherit tooltip)))
;; Display signature documentation in eldoc.
(setq lsp-signature-render-documentation t)
;; If non-nil skip current symbol when setting symbol highlights.
(setq lsp-symbol-highlighting-skip-current nil)
;; Alist mapping SymbolKinds to human-readable strings. Alist mapping
;; SymbolKinds to human-readable strings. Various Symbol objects in
;; the LSP protocol have an integral type, specifying what they are.
;; This alist maps such type integrals to readable representations of
;; them. See https://microsoft.github.io/language-server-
;; protocol/specifications/specification-current/, namespace
;; SymbolKind.
(setq lsp-symbol-kinds ((1 . File) (2 . Module) (3 . Namespace) (4 . Package) (5 . Class) (6 . Method) (7 . Property) (8 . Field) (9 . Constructor) (10 . Enum) (11 . Interface) (12 . Function) (13 . Variable) (14 . Constant) (15 . String) (16 . Number) (17 . Boolean) (18 . Array) (19 . Object) (20 . Key) (21 . Null) (22 . Enum Member) (23 . Struct) (24 . Event) (25 . Operator) (26 . Type Parameter)))
;; The timeout for tcp connection in seconds.
(setq lsp-tcp-connection-timeout 2)
;; Hooks to run when lsp-unconfig-buffer is called.
(setq lsp-unconfigure-hook nil)
;; The script to unzip.
(setq lsp-unzip-script bash -c 'mkdir -p %2$s &amp;&amp; unzip -qq -o %1$s -d %2$s')
;; Whether to check GPG signatures of downloaded files.
(setq lsp-verify-signature t)
;; Vscode extension template url.
(setq lsp-vscode-ext-url https://marketplace.visualstudio.com/_apis/public/gallery/publishers/%s/vsextensions/%s/%s/vspackage)
;; Hooks to run after the folders has changed. Hooks to run after the
;; folders has changed. The hook will receive two parameters list of
;; added and removed folders.
(setq lsp-workspace-folders-changed-functions nil)
;; semantic-tokens Settings

;; Face used for classes.
(setq lsp-face-semhl-class ((t (:inherit font-lock-type-face))))
;; Face used for comments.
(setq lsp-face-semhl-comment ((t (:inherit font-lock-comment-face))))
;; Face used for semantic highlighting scopes matching constant
;; scopes.
(setq lsp-face-semhl-constant ((t :inherit font-lock-constant-face)))
;; Face used for defaultLibrary modifier.
(setq lsp-face-semhl-default-library ((t :inherit font-lock-builtin-face)))
;; Face used for definition modifier.
(setq lsp-face-semhl-definition ((t :inherit font-lock-function-name-face :weight bold)))
;; Face used for semantic highlighting scopes matching constant
;; scopes.
(setq lsp-face-semhl-deprecated ((t :strike-through t)))
;; Face used for enums.
(setq lsp-face-semhl-enum ((t (:inherit font-lock-type-face))))
;; Face used for event properties.
(setq lsp-face-semhl-event ((t (:inherit font-lock-variable-name-face))))
;; Face used for semantic highlighting scopes matching
;; entity.name.function.*. Face used for semantic highlighting scopes
;; matching entity.name.function.*. Unless overridden by a more
;; specific face association.
(setq lsp-face-semhl-function ((t :inherit font-lock-function-name-face)))
;; Face used for implementation modifier.
(setq lsp-face-semhl-implementation ((t :inherit font-lock-function-name-face :weight bold)))
;; Face used for interfaces.
(setq lsp-face-semhl-interface ((t (:inherit font-lock-type-face))))
;; Face used for keywords.
(setq lsp-face-semhl-keyword ((t (:inherit font-lock-keyword-face))))
;; Face used for labels.
(setq lsp-face-semhl-label ((t (:inherit font-lock-comment-face))))
;; Face used for macros.
(setq lsp-face-semhl-macro ((t (:inherit font-lock-preprocessor-face))))
;; Face used for members.
(setq lsp-face-semhl-member ((t (:inherit font-lock-variable-name-face))))
;; Face used for semantic highlighting scopes matching
;; entity.name.method.*. Face used for semantic highlighting scopes
;; matching entity.name.method.*. Unless overridden by a more specific
;; face association.
(setq lsp-face-semhl-method ((t :inherit lsp-face-semhl-function)))
;; Face used for semantic highlighting scopes matching
;; entity.name.namespace.*. Face used for semantic highlighting scopes
;; matching entity.name.namespace.*. Unless overridden by a more
;; specific face association.
(setq lsp-face-semhl-namespace ((t :inherit font-lock-type-face :weight bold)))
;; Face used for numbers.
(setq lsp-face-semhl-number ((t (:inherit font-lock-constant-face))))
;; Face used for operators.
(setq lsp-face-semhl-operator ((t (:inherit font-lock-function-name-face))))
;; Face used for parameters.
(setq lsp-face-semhl-parameter ((t (:inherit font-lock-variable-name-face))))
;; Face used for properties.
(setq lsp-face-semhl-property ((t (:inherit font-lock-variable-name-face))))
;; Face used for regexps.
(setq lsp-face-semhl-regexp ((t (:inherit font-lock-string-face :slant italic))))
;; Face used for static modifier.
(setq lsp-face-semhl-static ((t :inherit font-lock-keyword-face)))
;; Face used for keywords.
(setq lsp-face-semhl-string ((t (:inherit font-lock-string-face))))
;; Face used for structs.
(setq lsp-face-semhl-struct ((t (:inherit font-lock-type-face))))
;; Face used for types.
(setq lsp-face-semhl-type ((t (:inherit font-lock-type-face))))
;; Face used for type parameters.
(setq lsp-face-semhl-type-parameter ((t (:inherit font-lock-type-face))))
;; Face used for semantic highlighting scopes matching variable.*.
;; Face used for semantic highlighting scopes matching variable.*.
;; Unless overridden by a more specific face association.
(setq lsp-face-semhl-variable ((t :inherit font-lock-variable-name-face)))
;; Whether to use semantic token delta requests when available.
(setq lsp-semantic-tokens-allow-delta-requests nil)
;; Whether to use ranged semantic token requests when available.
(setq lsp-semantic-tokens-allow-ranged-requests nil)
;; Whether semantic tokens should take token modifiers into account.
(setq lsp-semantic-tokens-apply-modifiers nil)
;; Enable/disable support for semantic tokens. Enable/disable support
;; for semantic tokens. As defined by the Language Server Protocol
;; 3.16.
(setq lsp-semantic-tokens-enable nil)
;; Whether to honor semanticTokens/refresh requests.
(setq lsp-semantic-tokens-honor-refresh-requests nil)
;; Maximum number of on-idle token requests to be dispatched
;; simultaneously.
(setq lsp-semantic-tokens-max-concurrent-idle-requests 1)
;; Warning on missing face for token type/modifier. Warning on missing
;; face for token type/modifier. When non-nil, this option will emit a
;; warning any time a token or modifier type returned by a language
;; server has no face associated with it.
(setq lsp-semantic-tokens-warn-on-missing-face nil)
;; icons Settings

;; If non-nil, icons support is enabled for headerline-breadcrumb.
(setq lsp-headerline-breadcrumb-icons-enable nil)
