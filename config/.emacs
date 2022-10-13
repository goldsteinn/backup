(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
 '(package-selected-packages
   '(tuareg caml ocamlformat rust-mode bison-mode pdf-tools flycheck-pycheckers elpy yapfify which-key use-package nasm-mode magit cmake-mode elf-mode bazel lsp-mode nav-flash go-mode))
 '(safe-local-variable-values
   '((ccls-initialization-options :index
                                  (:threads 6 :initialBlacklist
                                            ["/make-[0-9]" "tests/work/" "/\\.deps" "/\\..*cache" "/\\.git"])))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq exec-path (append exec-path '("/home/noah/.local/bin:$PATH")))
(setq exec-path (append exec-path '("/home/noah/programs/pyscript:$PATH")))
;; unset annoying ass binding
;;(global-set-key (kbd "C-[") nil)
;;(global-unset-key (kbd "C-["))
;; kill-ring to clipboard mapping

(setq global-mark-ring-max '256)
(setq kill-ring-max '256)
(setq initial-major-mode 'text-mode)
(global-set-key [?\M-w] 'clipboard-kill-ring-save)
(global-set-key [?\C-y] 'clipboard-yank)
(global-set-key (kbd "\C-cy") '(lambda ()
                                 (interactive)
                                 (popup-menu 'yank-menu)))


;; (global-set-key [?\M-x 'compilegcc'] (setq compile-command "make package install"))
;;make shell stay in same windows ffs
;;(load "~/.emacs.d/exlisp/ffap-java")

;; display shell in the current window
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

                                        ;(add-to-list 'display-buffer-alist
                                        ;             '("*RTags Diagnostics*" . (display-buffer-same-window)))

;; should read about elpy: https://github.com/jorgenschaefer/elpy
;; https://github.com/jorgenschaefer/elpy/issues/1936
(elpy-enable)
(setq inhibit-startup-screen t)
(setq elpy-rpc-python-command "python3")
(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-r") nil)
     (define-key python-mode-map (kbd "<C-return>") 'company-complete)
     (setq company-idle-delay nil)
     )
  )

(defun my-py-mode-hook ()
  (setq company-idle-delay nil)
  (company-mode 1)
  (define-key elpy-mode-map (kbd "<C-return>") 'my-company-complete)
  (define-key python-mode-map (kbd "<C-return>") 'my-company-complete)
  )

(defun my-elisp-mode-hook ()
  (setq company-idle-delay nil)
  (company-mode 1)
  )

(add-hook 'python-mode-hook 'my-py-mode-hook)
(add-hook 'emacs-lisp-mode 'my-elisp-mode-hook)
(setq python-indent-guess-indent-offset nil)
;;buffer menu on current buffer
(defun close()
  (interactive)
  (if (equal 1 (length (frame-list)))
      (save-buffers-kill-terminal)
    (delete-frame))
  )

(defun generic-clean-buffer()
  (interactive)
  (let (return-to-position)
    (setq return-to-position (point))
    (call-interactively #'mark-whole-buffer)
    (call-interactively #'indent-region)
    (goto-char return-to-position)
    )
  (deactivate-mark)
  (my-whitespace-cleanup)
  )


(defvar compiler "gcc" "Default C compiler")
(defvar default-compiler-opts-extra "-DI_NO_HIDDEN_LABELS")
(defvar compiler-opts-extra default-compiler-opts-extra)
(defvar default-compiler-opts "-O3 -march=native -mtune=native")
(defvar cur-compiler-opts default-compiler-opts)


(defvar rust-compile-cmd "-C panic=abort -C target-cpu=native -C debuginfo=0 -C opt-level=3 -F warnings ")
(defvar rust-compiler "rustc")
(setq rust-compile-cmd "-C panic=abort -C target-cpu=native -C debuginfo=0 -C opt-level=3 -F warnings ")
(setq rust-compiler "rustc")
(defun -set-compiler(new-compiler)
  (setq compiler new-compiler)
  (reset-compiler-opts-extra)
  )

(defun set-compiler()
  (interactive)
  (let ((new-compiler (read-file-name "Enter Compiler: " default-directory)))
    (-set-compiler new-compiler)
    )
  )


(defun reset-compiler-opts-extra ()
  (interactive)
  (setq compiler-opts-extra default-compiler-opts-extra)
  )

(defun set-compiler-opts-extra()
  (interactive)
  (let ((new-compiler-opts-extra (read-string "Enter Extra Compiler Options: ")))
    (setq compiler-opts-extra new-compiler-opts-extra)
    )
  )

(defun reset-compiler-opts ()
  (interactive)
  (setq cur-compiler-opts default-compiler-opts)
  )

(defun set-compiler-opts()
  (interactive)
  (let ((new-compiler-opts (read-string "Enter Extra Compiler Options: ")))
    (setq cur-compiler-opts new-compiler-opts)
    )
  )


(defun use-clang()
  (interactive)
  (-set-compiler "clang"))

(defun use-gcc()
  (interactive)
  (-set-compiler "gcc"))



(defun which-compiler()
  (interactive)
  (message compiler))

(defun -get-c-compiler(compiler-exe)
  (cond ((string-equal compiler-exe "clang") "clang")
        ((string-equal compiler-exe "clang++") "clang")
        ((string-equal compiler-exe "gcc") "gcc")
        ((string-equal compiler-exe "g++") "gcc")
        (t compiler-exe)))


(defun -get-cxx-compiler(compiler-exe)
  (cond ((string-equal compiler-exe "clang") "clang++")
        ((string-equal compiler-exe "clang++") "clang++")
        ((string-equal compiler-exe "gcc") "g++")
        ((string-equal compiler-exe "g++") "g++")
        (t compiler-exe)))


(defun -get-compiler(map-func)
  (let ((ret nil))
    (let ((compiler-exe (file-name-nondirectory compiler)))
      (let ((compiler-dir (file-name-directory compiler)))
        (setq ret
              (format "%s%s"
                      (if compiler-dir compiler-dir "") (funcall map-func compiler-exe)
                      )
              )
        )
      )
    )
  )

(defun get-c-compiler ()
  (interactive)
  (-get-compiler '-get-c-compiler)
  )
(defun get-cxx-compiler ()
  (interactive)
  (-get-compiler '-get-cxx-compiler)
  )


(defun get-mode-compiler()
  (cond ((equal (cur-mode) 'c++-mode) (get-cxx-compiler))
        ((equal (cur-mode) 'rust-mode) rust-compiler)
        (t (get-c-compiler))
        )
  )



(defun default-make()
  (interactive)
  (setq compile-command "make -k")
  (call-interactively #'compile))
(defalias 'll 'do-recompile)
(defalias 'mm 'default-make)

(defalias 'gdif 'magit-diff-buffer-file)
(defalias 'gdiff 'magit-diff-buffer-file)

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun clean-buffer-name ()
  (interactive)
  (replace-in-string "/" "_" (replace-in-string ">" "_" (replace-in-string "<" "_" (replace-in-string "*" "_" (buffer-name))))))

(defun hdr-guard-int()
  (let ((fname0 (buffer-file-name)))
    (let ((pos (string-match-p (regexp-quote "src/") fname0)))
      (let ((fname1 (substring fname0 pos nil)))
        (let ((fname2 (cond ((string-equal (substring fname1 0 1) "/") (substring fname1 1 nil))
                            (t fname1))))
          (let ((fname3 (replace-in-string "/" "__" fname2)))
            (let ((fname4 (replace-in-string "." "_" fname3)))
              (let ((fname5 (replace-in-string "-" "_" fname4)))
                (let ((fname6 (format "_%s_" (upcase fname5))))
                  fname6
                  )
                )
              )
            )
          )
        )
      )
    )
  )

(defun new-hguard()
  (interactive)
  (insert (hdr-guard-int)))

(defun new-hdr()
  (interactive)
  (goto-char (point-min))
  (insert (format "#ifndef %s\n" (hdr-guard-int)))
  (insert (format "#define %s\n\n" (hdr-guard-int)))
  (goto-char (point-max))
  (insert "\n\n#endif\n"))







(defun default-compile(_base_command)
  (interactive)
  (let ((_fname (file-name-nondirectory (buffer-name))))
    (let ((_oname (file-name-base (buffer-name))))
      (let ((_command0 (concat _base_command _fname)))
        (let ((_command1 (concat _command0 " -o ")))
          (let ((_command2 (concat _command1 _oname)))
            (setq compile-command _command2)
            (call-interactively #'compile)
            )
          )
        )
      )
    )
  )

(defun compile-command(cc cmd)
  (format "%s %s %s" cc compiler-opts-extra cmd))

(defun compile-c-command(cmd)
  (compile-command (get-c-compiler) cmd))

(defun compile-cxx-command(cmd)
  (compile-command (get-cxx-compiler) cmd))

(defun default-cxx-compile()
  (interactive)
  (default-compile (compile-cxx-command (concat (concat "-std=c++17 " cur-compiler-opts) " "))))


(defun default-c-compile()
  (interactive)
  (default-compile (compile-c-command (concat cur-compiler-opts " "))))

(defun default-asm-compile()
  (interactive)
  (default-compile "gcc -s -static -nostartfiles -nodefaultlibs -nostdlib -Wl,--build-id=none "))



(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun cur-mode ()
  "Returns the major mode associated with a buffer."
  (with-current-buffer (current-buffer)
    major-mode))

(defun default-rust-compile ()
  (interactive)
  (default-compile (concat rust-compiler " " rust-compile-cmd))
  )



(defun do-compile()
  (interactive)
  (save-some-buffers nil `(lambda () (eq (current-buffer) ,(current-buffer))))
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'default-c-compile))
        ((equal (cur-mode) 'c++-mode) (call-interactively #'default-cxx-compile))
        ((equal (cur-mode) 'python-mode) (call-interactively #'elpy-check))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'default-asm-compile))
        ((equal (cur-mode) 'rust-mode) (call-interactively #'default-rust-compile))
        ((equal (cur-mode) 'markdown-mode) (call-interactively #'md-render))
        ((equal (cur-mode) 'mhtml-mode) (call-interactively #'html-render))
        (t (call-interactively #'compile))))


(defun do-recompile()
  (interactive)
  (save-some-buffers nil `(lambda () (eq (current-buffer) ,(current-buffer))))
  (cond ((equal (cur-mode) 'python-mode) (call-interactively #'elpy-check))
        ((equal (cur-mode) 'markdown-mode) (call-interactively #'md-render))
        ((equal (cur-mode) 'mhtml-mode) (call-interactively #'html-render))
        (t (call-interactively #'recompile))))


(global-unset-key "\C-c\C-m")
(global-set-key "\C-c\C-m" 'default-make)

(global-unset-key "\C-c\C-r")
(global-set-key "\C-c\C-r" 'do-recompile)


(global-unset-key "\C-c\C-k")
(global-set-key "\C-c\C-k" 'kill-compilation)
(global-unset-key "\C-c\C-v")
(global-set-key "\C-c\C-v" 'do-compile)

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'close)

(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-n" 'next-line)
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

(global-unset-key "\C-r")
(global-unset-key "\C-j")
(global-unset-key "\C-s")

(global-set-key "\C-j"
                (lambda ()
                  (interactive)
                  (push-coords)
                  (isearch-backward)
                  )
                )
(global-set-key "\C-s"
                (lambda ()
                  (interactive)
                  (push-coords)
                  (isearch-forward)
                  )
                )



(define-key isearch-mode-map "\C-j" 'isearch-repeat-backward)



(defalias 'toggle-fill 'global-display-fill-column-indicator-mode)
;(global-unset-key "\C-c\C-p")
;(global-set-key "\C-c\C-p" 'toggle-fill)

(setq ring-bell-function 'ignore)
(setq package-check-signature 'nil)
                                        ;(setq mac-command-key-is-meta 't)


(add-to-list 'default-frame-alist '(fullscreen . maximized))

(normal-erase-is-backspace-mode 1)
(add-to-list 'auto-mode-alist '("\\.dat\\'" . hexl-mode))
;;https://emacs.stackexchange.com/questions/31631/gray-out-preprocessor-conditionals


(defun my-set-reg (my-reg)
  (interactive)
  (if mark-active
      (progn
	    (set-register my-reg (buffer-substring-no-properties (region-beginning) (region-end)))
	    (deactivate-mark)
	    )
    (progn
      (set-register my-reg (buffer-substring-no-properties (point) (line-end-position)))
      (deactivate-mark)
      )
    )
  )


(defun my-get-reg (my-reg)
  (interactive)
  (progn
    (insert (get-register my-reg))
    )
  )

(defun c-comment-line ()
  (interactive)
  (progn
    (insert "/********************************************************************/")
    )
  )

(defun comment-line ()
  (interactive)
  (progn
    (insert "//////////////////////////////////////////////////////////////////////")
    )
  )

(defun dash-line ()
  (interactive)
  (progn
    (insert "----------------------------------------------------------------------")
    )
  )

(defun hash-line ()
  (interactive)
  (progn
    (insert "######################################################################")
    )
  )

(global-set-key [?\C-7]
		        '(lambda ()
		           (interactive)
		           (seperate-line)
		           )
		        )

(defun seperate-line ()
  (interactive)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'c-comment-line))
        ((equal (cur-mode) 'c++-mode) (call-interactively #'c-comment-line))
        ((equal (cur-mode) 'rust-mode) (call-interactively #'c-comment-line))
        ((equal (cur-mode) 'python-mode) (call-interactively #'hash-line))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'c-comment-line))
        (t (call-interactively #'hash-line))))



                                        ;(setq my-reg-list '(([?\M-1] . 'r) ([?\M-2] . 't)))

                                        ;(mapcar '(lambda (x)
                                        ;	   (global-set-key (car x)
                                        ;			   (funcall (lambda (y)
                                        ;			      (interactive)
                                        ;			      (my-set-reg y)) (cdr x)))) my-reg-list)


(global-set-key [?\M-1]
		        '(lambda ()
		           (interactive)
		           (my-set-reg 'r)))


(global-set-key [?\C-1]
		        '(lambda ()
		           (interactive)
		           (my-get-reg 'r)))


(global-set-key [?\M-2]
		        '(lambda ()
		           (interactive)
		           (my-set-reg 't)))


(global-set-key [?\C-2]
		        '(lambda ()
		           (interactive)
		           (my-get-reg 't)))


(global-set-key [?\M-3]
		        '(lambda ()
		           (interactive)
		           (my-set-reg 's)))


(global-set-key [?\C-3]
		        '(lambda ()
		           (interactive)
		           (my-get-reg 's)))


(global-set-key [?\M-4]
		        '(lambda ()
		           (interactive)
		           (my-set-reg 'y)))


(global-set-key [?\C-4]
		        '(lambda ()
		           (interactive)
		           (my-get-reg 'y)))

(global-set-key [?\M-5]
		        '(lambda ()
		           (interactive)
		           (my-set-reg 'a)))


(global-set-key [?\C-5]
		        '(lambda ()
		           (interactive)
		           (my-get-reg 'a)))


(global-set-key [?\M-6]
		        '(lambda ()
		           (interactive)
		           (my-set-reg 'b)))


(global-set-key [?\C-6]
		        '(lambda ()
		           (interactive)
		           (my-get-reg 'b)))

(setq company-idle-delay nil)
(defun my-company-complete ()
  (interactive)
  (company-complete)
  )
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


;;gomode stuff
(eval-after-load "go-mode"
  '(require 'flymake-go))



(defun to-underscore () (interactive) (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end)) (downcase-region (region-beginning) (region-end))) )


(defalias 'gt 'goto-line)
(defalias 'rb 'rename-buffer)


(defun for-word (beg end) ""
       (interactive "r")
       (save-excursion
         (narrow-to-region beg end)
         (goto-char beg)
         (replace-regexp "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2")
         (widen)))

(global-unset-key (kbd "\C-t"))
(global-set-key [?\C-t] 'switch-to-buffer)

(global-unset-key "\M-f")
(global-unset-key "\M-b")


(defun fwd-whitespace ()
  (interactive)
  (skip-chars-forward "^[:space:]")
  (skip-chars-forward "[:space:]")
  )
(defun bkwd-whitespace ()
  (interactive)
  (skip-chars-backward "^[:space:]")
  (skip-chars-backward "[:space:]")
  )
(global-set-key "\M-f" 'fwd-whitespace)
(global-set-key "\M-b" 'bkwd-whitespace)


(global-unset-key (kbd "C-."))
(global-unset-key (kbd "C-,"))


(global-set-key (kbd "C-.") 'fwd-whitespace)
(global-set-key (kbd "C-,") 'bkwd-whitespace)


                                        ;(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
                                        ;(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
                                        ;(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

                                        ;(setq rtags-autostart-diagnostics t)
                                        ;(rtags-diagnostics)





                                        ;(require 'package)
(package-initialize)
                                        ;(require 'rtags)
                                        ;(require 'company)




                                        ;(add-hook 'c-mode-hook 'company-init-me)
                                        ;(add-hook 'c++-mode-hook 'company-init-me)
                                        ;(add-hook 'objc-mode-hook 'company-init-me)
                                        ;(add-hook 'emacs-lisp-mode-hook 'company-init-me)
                                        ;(add-hook 'text-mode-hook 'company-init-me)
                                        ;(add-hook 'lisp-interaction-mode-hook 'company-init-me)

;;recompile: (byte-recompile-directory package-user-dir nil 'force)


                                        ;(add-hook 'shell-mode '(setq company-global-modes '(not org-mode)))


;; Alias for C-c r ,

                                        ;(pdf-loader-install)
                                        ;(defalias 'pgt 'pdf-view-goto-page)
                                        ;(defalias 'pgl 'pdf-view-goto-label)



(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(c-set-offset 'comment-intro 0)

                                        ;https://emacs.stackexchange.com/questions/48500/how-to-clang-format-the-current-buffer-on-save
(load "/usr/share/emacs/site-lisp/clang-format-14/clang-format.el")
(load "/usr/share/emacs/site-lisp/llvm-14/tablegen-mode.el")
(defun fill-buffer()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fill-region (point-min) (point-max)))))



(defun clean-region()
  (interactive)
  (when (not (equal (cur-mode) 'shell-mode))
    (my-whitespace-cleanup)
    (cond ((equal (cur-mode) 'c-mode) (call-interactively #'clang-format-region))
          ((equal (cur-mode) 'c++-mode) (call-interactively #'clang-format-region))
          ((equal (cur-mode) 'rust-mode) (call-interactively #'rust-format-buffer))
          ((equal (cur-mode) 'python-mode) (call-interactively #'yapfify-region))
          ((equal (cur-mode) 'asm-mode) (call-interactively #'abfify-region))
          ((equal (cur-mode) 'perl-mode) (call-interactively #'perlify-region))
          ((equal (cur-mode) 'sh-mode) (call-interactively #'my-whitespace-cleanup-region))
          ((equal (cur-mode) 'emacs-lisp-mode) t)
          ((equal (cur-mode) 'cmake-mode) t)
          ((equal (cur-mode) 'racket-mode) t)
          (t (call-interactively #'fill-paragraph)))
    )
  )

(defun clean-buffer()
  (my-whitespace-cleanup)
  (interactive)
  (when (not (equal (cur-mode) 'shell-mode))
    (cond ((equal (cur-mode) 'c-mode) (call-interactively #'clang-format-buffer))
          ((equal (cur-mode) 'c++-mode)
           (call-interactively #'clang-format-buffer)
           )
          ((equal (cur-mode) 'python-mode) (call-interactively #'yapfify-buffer))
          ((equal (cur-mode) 'asm-mode) (call-interactively #'abfify-buffer))
          ((equal (cur-mode) 'rust-mode) (call-interactively 'rust-format-buffer))
          ((equal (cur-mode) 'perl-mode) (call-interactively #'perlify-buffer))
          ((equal (cur-mode) 'latex-mode) (call-interactively #'generic-clean-buffer))
          ((equal (cur-mode) 'emacs-lisp-mode) (call-interactively #'generic-clean-buffer))
          ((equal (cur-mode) 'cmake-mode) (call-interactively #'generic-clean-buffer))
          ((equal (cur-mode) 'sh-mode) (call-interactively #'generic-clean-buffer))
          ((equal (cur-mode) 'makefile-gmake-mode) t)
          ((equal (cur-mode) 'markdown-mode) t)
          ((equal (cur-mode) 'racket-mode) (call-interactively #'rackify-buffer))
          (t (call-interactively #'fill-buffer)))
    )
  )

                                        ;(defun racket-format-buffer()
                                        ;  (interactive)
                                        ;  (cond ((equal buffer-file-name 'nil) (message "Unable to fmt buffer"))
                                        ;        (t (let ((srcfile buffer-file-name))
                                        ;                (let ((ret (call-process "raco" nil nil nil srcfile))))
                                        ;             )
                                        ;           (call-interactively 'my-revert-buffer-noconfirm)
                                        ;           )
                                        ;        )
                                        ;  )

(setq whitespace-space 'underline)
(setq whitespace-style '(face lines lines-tail trailing))

                                        ;(global-unset-key (kbd "C-i"))
                                        ;(global-set-key (kbd "C-i") 'sort-lines)

(global-unset-key (kbd "\M-q"))
(global-set-key (kbd "\M-q") 'clean-region)
(define-key global-map "\M-q" 'clean-region)
(global-unset-key (kbd "\C-q"))
(global-set-key (kbd "\C-q") 'clean-buffer)



(set-face-attribute 'region nil :background "#FF8C00")

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 8)))

;; Cycle between snake case, camel case, etc.
                                        ;    (require 'string-inflection)
                                        ;    (global-set-key (kbd "C-c i") 'string-inflection-cycle)
                                        ;    (global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
                                        ;    (global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
                                        ;    (global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun my-revert-buffer-noconfirm ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (revert-buffer nil t))

(global-unset-key "\C-x\C-u")
(global-set-key "\C-x\C-u" 'my-revert-buffer-noconfirm)

(global-unset-key "\C-x\C-n")
(global-set-key "\C-x\C-n" 'rename-current-buffer-file)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(global-unset-key "\C-x\C-l")
(global-set-key "\C-x\C-l" 'revert-buffer-no-confirm)


(defun xah-delete-current-file-copy-to-kill-ring ()
  "Delete current buffer/file and close the buffer, push content to `kill-ring'.
URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive)
  (progn
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted file: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))
(defalias 'kill-file 'xah-delete-current-file-copy-to-kill-ring)

(defun write-c-header ()
  (interactive)
  (goto-char (point-max))
  (insert "#include <assert.h>\n#include <immintrin.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <sys/mman.h>\n#include <sys/syscall.h>\n#include <time.h>\n#include <unistd.h>\n#include <x86intrin.h>\n\n\n#define ALWAYS_INLINE inline __attribute__((always_inline))\n#define NEVER_INLINE  __attribute__((noinline))\n#define CONST_ATTR    __attribute__((const))\n#define PURE_ATTR     __attribute__((pure))\n#define BENCH_FUNC    __attribute__((noinline, noclone, aligned(4096)))\n\n#define COMPILER_OOE_BARRIER() asm volatile(\"lfence\" : : : \"memory\")\n#define OOE_BARRIER()          asm volatile(\"lfence\" : : :)\n#define COMPILER_BARRIER() asm volatile(\"\" : : : \"memory\");\n#define COMPILER_DO_NOT_OPTIMIZE_OUT(X)                                        \\
    asm volatile(\"\" : : \"i,r,m,v\"(X) : \"memory\")\n\n#define _CAT(X, Y) X##Y\n#define CAT(X, Y) _CAT(X, Y)\n#define _V_TO_STR(X) #X\n#define V_TO_STR(X) _V_TO_STR(X)\n\n#define NO_LSD_RD(tmp, r) \"pop \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n#define NO_LSD_WR(tmp, r) \"push \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n\n#define IMPOSSIBLE(X)                                                          \\
    if (X) {                                                                   \\
        __builtin_unreachable();                                               \\
    }\n\n#define PRINT(...) fprintf(stderr, __VA_ARGS__)\n\n\ndouble BENCH_FUNC\nbench() {\n    enum { NTRIALS = 1000 * 1000 };\n\n    uint64_t start, end;\n\n    start = _rdtsc();\n    for (uint32_t trials = NTRIALS; trials; --trials) {\n    }\n    end = _rdtsc();\n\n    return (double)(end - start) / (double)NTRIALS;\n}\n\n\n\nint\nmain(int argc, char ** argv) {\n}")
                                        ;(set-buffer-modified-p nil)
  )

(defun write-cc-header ()
  (interactive)
  (goto-char (point-max))
  (insert "#include <assert.h>\n#include <immintrin.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <sys/mman.h>\n#include <sys/syscall.h>\n#include <time.h>\n#include <unistd.h>\n#include <x86intrin.h>\n#include <type_traits>\n\n#define ALWAYS_INLINE inline __attribute__((always_inline))\n#define NEVER_INLINE  __attribute__((noinline))\n#define CONST_ATTR    __attribute__((const))\n#define PURE_ATTR     __attribute__((pure))\n#define BENCH_FUNC    __attribute__((noinline, noclone, aligned(4096)))\n\n#define COMPILER_OOE_BARRIER() asm volatile(\"lfence\" : : : \"memory\")\n#define OOE_BARRIER()          asm volatile(\"lfence\" : : :)\n#define COMPILER_BARRIER() asm volatile(\"\" : : : \"memory\");\n#define COMPILER_DO_NOT_OPTIMIZE_OUT(X)                                        \\
    asm volatile(\"\" : : \"i,r,m\"(X) : \"memory\")\n\n#define _CAT(X, Y) X##Y\n#define CAT(X, Y) _CAT(X, Y)\n#define _V_TO_STR(X) #X\n#define V_TO_STR(X) _V_TO_STR(X)\n\n#define NO_LSD_RD(tmp, r) \"pop \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n#define NO_LSD_WR(tmp, r) \"push \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n\n#define IMPOSSIBLE(X)                                                          \\
    if (X) {                                                                   \\
        __builtin_unreachable();                                               \\
    }\n\n#define PRINT(...) fprintf(stderr, __VA_ARGS__)\n\nenum timer_conf { CYCLES = 0, GETTIME = 1 };\ntemplate<timer_conf conf = GETTIME>\nstruct timer {\n    static constexpr clockid_t cid       = CLOCK_MONOTONIC;\n    static constexpr uint64_t  sec_to_ns = 1000 * 1000 * 1000;\n\n    using time_t = typename std::\n        conditional_t<conf == timer_conf::CYCLES, uint64_t, struct timespec>;\n\n    time_t tstart;\n    time_t tend;\n\n    const char * const\n    units() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return \"cycles\";\n        }\n        else {\n            return \"ns\";\n        }\n    }\n\n    void ALWAYS_INLINE\n    start() {\n        if constexpr (conf == timer_conf::CYCLES) {\n            tstart = _rdtsc();\n        }\n        else {\n            clock_gettime(cid, &tstart);\n        }\n    }\n\n    void ALWAYS_INLINE\n    end() {\n        if constexpr (conf == timer_conf::CYCLES) {\n            tend = _rdtsc();\n        }\n        else {\n            clock_gettime(cid, &tend);\n        }\n    }\n\n    uint64_t\n    get_start() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return tstart;\n        }\n        else {\n            return sec_to_ns * tstart.tv_sec + tstart.tv_nsec;\n        }\n    }\n    uint64_t\n    get_end() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return tend;\n        }\n        else {\n            return sec_to_ns * tend.tv_sec + tend.tv_nsec;\n        }\n    }\n\n    uint64_t\n    dif() {\n        return get_end() - get_start();\n    }\n\n    double\n    ddif() {\n        return ((double)dif());\n    }\n\n    double\n    ddif(uint64_t n) {\n        return ddif() / ((double)n);\n    }\n\n    void\n    std_print() {\n        std_print(\"\", 1);\n    }\n\n    void\n    std_print(const char * const hdr) {\n        std_print(hdr, 1);\n    }\n\n    void\n    std_print(uint64_t n) {\n        std_print(\"\", n);\n    }\n\n    void\n    std_print(const char * const hdr, uint64_t n) {\n        if (hdr[0] == 0) {\n            fprintf(stderr, \"%.3E %s\\n\", ddif(n), units());\n        }\n        else {\n            fprintf(stderr, \"%-8s: %.3E %s\\n\", hdr, ddif(n), units());\n        }\n    }\n};\n\n\nint\nmain(int argc, char ** argv) {}\n\n")
                                        ;(set-buffer-modified-p nil)
  )

(defun write-asm-header ()
  (interactive)
  (goto-char (point-max))
  (insert "	.global	_start\n")
  (insert "	.p2align 6\n")
  (insert "	.text\n")
  (insert "_start:\n")
  (insert "\n"    )
  (insert "	movl	$60, %eax\n")
  (insert "	xorl	%edi, %edi\n")
  (insert "	syscall\n\n")
  (insert "#if 0\n")
  (insert "	.section .rodata\n")
  (insert "	.balign	4096\n")
  (insert "buf_start:	.space 4096\n")
  (insert "buf_end:\n")
  (insert "#endif\n")
  )

(defun newc()
  (interactive)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'write-c-header))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'write-asm-header))
        (t (call-interactively #'write-cc-header))))




(defun left-tab ()
  (interactive)
  (beginning-of-line)
  (call-interactively #'fixup-whitespace))
(defun my-asm-tab ()
  (interactive)

                                        ; possible asm directives
                                        ; \.file\\|\.text\\|.globl\\|\.cfi\\|\.type\\|\.size
  (cond ((string-match-p "#\\|:" (thing-at-point 'line)) (call-interactively #'left-tab))
        (t (call-interactively #'indent-for-tab-command))))

(defun my-tab()
  (interactive)
  (cond ((equal (cur-mode) 'asm-mode) (call-interactively #'my-asm-tab))
        (t (call-interactively #'indent-for-tab-command))))

                                        ;(defun asm-beautify ()
                                        ;  (interactive)
                                        ;  (cond ((equal buffer-file-name 'nil) (message "Unable beautify non-file"))
                                        ;        (t (let ((srcfile buffer-file-name))
                                        ;             (let ((tmpbuf (get-buffer-create "*asm-beautifier-tmp*")))
                                        ;               (erase-buffer)
                                        ;               (let ((ret (call-process "asm-beautifier.py" nil "*asm-beautifier-tmp*" srcfile)))

(add-to-list 'load-path "/home/noah/.emacs.d/snippets")
(require 'abfify)
(require 'rackify)
(require 'perlify)

(defun my-obj-dump-c ()
  (interactive)
  (let ((c-compiler-args (split-s " " cur-compiler-opts)))
    (push "-c" c-compiler-args)
    (when (not (string-equal compiler-opts-extra ""))
      (setq c-compiler-args (append c-compiler-args (split-s " " compiler-opts-extra)))
      )
    (my-obj-dump c-compiler-args ".c")
    )
  )

(defun my-filter  (f args)
  (cond ((null args) nil)
        ((if (funcall f (car args))
             (cons (car args) (my-filter  f (cdr args)))
           (my-filter  f (cdr args))))))
(defun zstring (s)
  (> (length s) 0)
  )

(defun split-s (delim s)
  (my-filter #'zstring (s-split delim s)
             )
  )




(defun my-obj-dump-asm ()
  (interactive)
  (my-obj-dump (split-s " "  (concat default-compiler-opts-extra " -c ")) ".S")
  )

(defun my-obj-dump-rs ()
  (interactive)
  (my-obj-dump (split-s " " (concat rust-compile-cmd " --emit obj ")) ".rs")
  )

(defun copy-buffer-to-file (dst-file)
  (interactive)
  (if (file-exists-p dst-file)
      (message (format "File already exists: %s" dst-file))

    (append-to-file (point-min) (point-max) dst-file)

    )
  )


;;(defun my-obj-dump (pass-compiler-args)
;;  (interactive)
;;  (save-some-buffers nil `(lambda () (eq (current-buffer) ,(current-buffer))))
;;  (cond ((equal buffer-file-name 'nil) (message "Unable to objdump non-file"))
;;        (t (let ((srcfile buffer-file-name))
;;             (let ((dstfile (format "/home/noah/.tmp/objdumps/%s-%s.o" (file-name-base buffer-file-name)  (format-time-string "%Y-%m-%d-T%H-%M-%S"))))
;;               (let (return-to-position)
;;                 (let ((existing-buffer (get-buffer "*objdump-result*")))
;;                   (let* ((#1=#:v (get-buffer-create "*objdump-result*")))
;;                     (with-current-buffer #1#
;;                       (setq return-to-position (point))
;;                       (point-min)
;;                       (erase-buffer)
;;                       (asm-mode)
;;                       )
;;                     )
;;                   (let ((process-args (list (get-mode-compiler) nil "*objdump-result*" t)))
;;                     (let ((compiler-args pass-compiler-args))
;;                       (let ((compiler-files (list srcfile "-o" dstfile)))
;;                                        ;    (message (append compiler-args compiler-files))
;;
;;                         (let ((ret (apply 'call-process
;;                                           (append process-args compiler-args compiler-files)
;;                                           )))
;;                           (cond ((equal ret 0) (let* ((#1=#:v (get-buffer-create "*objdump-result*")))
;;                                                  (with-current-buffer #1#
;;                                                    (erase-buffer)
;;                                                    (insert (format "$> %s %s" (get-mode-compiler) compiler-args))
;;                                                    )
;;                                                  )
;;
;;                                  (call-process "objdump" nil "*objdump-result*" nil "-d" dstfile)
;;                                  )
;;                                 )
;;                           )
;;
;;                         (cond ((equal existing-buffer 'nil))
;;                               (t
;;                                (display-buffer existing-buffer)
;;                                )
;;                               )
;;                         (mapc
;;                          (lambda (win)
;;                            (unless (eq (selected-window) win)
;;                              (with-selected-window win
;;                                (goto-char return-to-position)
;;                                )
;;                              )
;;                            )
;;                          (get-buffer-window-list "*objdump-result*" nil t)
;;                          )
;;                         )
;;                       )
;;                     )
;;                   )
;;                 )
;;               )
;;             )
;;           )
;;        )
;;  )



(defun my-obj-dump (pass-compiler-args ext)
  (interactive)
  (save-some-buffers nil `(lambda () (eq (current-buffer) ,(current-buffer))))

  (let ((basefile (format "/home/noah/.tmp/objdumps/%s-%s.o" (clean-buffer-name) (format-time-string "%Y-%m-%d-T%H-%M-%S"))))


    (let ((srcfile (cond ((equal buffer-file-name 'nil) (concat basefile ext))
                         (t (buffer-file-name)))))
      (if (equal buffer-file-name 'nil)
          (copy-buffer-to-file srcfile)
        nil)
      (let ((dstfile (concat basefile ".o")))
        (let (return-to-position)
          (let ((existing-buffer (get-buffer "*objdump-result*")))
            (let* ((#1=#:v (get-buffer-create "*objdump-result*")))
              (with-current-buffer #1#
                (setq return-to-position (point))
                (point-min)
                (erase-buffer)
                (asm-mode)
                )
              )
            (let ((process-args (list (get-mode-compiler) nil "*objdump-result*" t)))
              (let ((compiler-args pass-compiler-args))
                (let ((compiler-files (list srcfile "-o" dstfile)))
                                        ;    (message (append compiler-args compiler-files))

                  (let ((ret (apply 'call-process
                                    (append process-args compiler-args compiler-files)
                                    )))
                    (cond ((equal ret 0) (let* ((#1=#:v (get-buffer-create "*objdump-result*")))
                                           (with-current-buffer #1#
                                             (erase-buffer)
                                             (insert (format "$> %s %s" (get-mode-compiler) compiler-args))
                                             )
                                           )

                           (call-process "objdump" nil "*objdump-result*" nil "-d" dstfile)
                           )
                          )
                    )

                  (cond ((equal existing-buffer 'nil))
                        (t
                         (display-buffer existing-buffer)
                         )
                        )
                  (mapc
                   (lambda (win)
                     (unless (eq (selected-window) win)
                       (with-selected-window win
                         (goto-char return-to-position)
                         )
                       )
                     )
                   (get-buffer-window-list "*objdump-result*" nil t)
                   )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

(defun do-obj-dump()
  (interactive)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'my-obj-dump-c))
        ((equal (cur-mode) 'c++-mode) (call-interactively #'my-obj-dump-c))
        ((equal (cur-mode) 'python-mode) (call-interactively #'elpy-check))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'my-obj-dump-asm))
        ((equal (cur-mode) 'rust-mode) (call-interactively #'my-obj-dump-rs))
        ((equal (cur-mode) 'markdown-mode) (call-interactively #'md-render))
        ((equal (cur-mode) 'mhtml-mode) (call-interactively #'html-render))
        (t (call-interactively #'my-obj-dump))))


(global-unset-key "\C-c\C-d")
(global-set-key "\C-c\C-d" 'magit-diff-buffer-file)

(global-unset-key "\C-c\C-o")
(global-set-key "\C-c\C-o" 'do-obj-dump)
(global-unset-key "\C-x\C-o")
(global-set-key "\C-x\C-o" 'do-obj-dump)

(defun my-whitespace-cleanup ()
  (interactive)
  (whitespace-cleanup)
  (let ((pos (point)))
    (goto-char (point-max))
    (insert "\n")
    (delete-trailing-whitespace)
    (goto-char pos)
    )
  )

(global-unset-key "\C-c\C-q")
(global-set-key "\C-c\C-q" 'my-whitespace-cleanup)


                                        ;         (let ((cmd (format "gcc -c %s -o /home/noah/tmp/%s-%s.o" buffer-file-name (file-name-base buffer-file-name)  (format-time-string "%Y-%m-%d-T%H-%M-%S"))))
                                        ;            (call-process cmd nil nil)))))

;;(call-process )))
;;(format-time-string "%Y-%m-%d-T%H-%M-%S")


(add-hook 'text-mode-hook
          (lambda () (electric-indent-local-mode -1)))

(defun newline-without-break-of-line ()
  (interactive)
  (open-line 1)
  (right-char))

(defun delete-space ()
  (interactive)
  (call-interactively #'delete-char)
  (insert " "))

(global-unset-key "\C-c\C-d")
(global-set-key "\C-c\C-d" 'delete-space)

(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent))
  (local-set-key (kbd "<tab>") 'my-tab)
  (local-set-key (kbd "TAB") 'my-tab)
  (local-unset-key "\C-r")
  (local-unset-key "\C-j")
  (local-set-key "\C-j" 'isearch-backward)
  (local-unset-key (kbd "<C-return>"))
  (local-set-key (kbd "<C-return>") 'newline-without-break-of-line)

  (local-unset-key (kbd "\C-c\C-d"))
  (local-set-key (kbd "\C-c\C-d") 'delete-space)
                                        ;  (whitespace-mode)
  (define-key isearch-mode-map "\C-j" 'isearch-repeat-backward)
  )


(add-hook 'asm-mode-hook 'my-asm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))


(defun my-shell-hook ()
  (define-key shell-mode-map "\C-c\C-r" 'do-recompile)
  (define-key shell-mode-map "\C-c\C-p" 'toggle-fill)

  )
(add-hook 'shell-mode-hook 'my-shell-hook)



(defun common-c-hook(map)
  (define-key map "\C-c\C-o" 'do-obj-dump)
  (define-key map "\C-c\C-q" 'my-whitespace-cleanup)
  (local-unset-key (kbd "\C-c\C-d"))
  (local-set-key (kbd "\C-c\C-d") 'delete-space)
                                        ;  (init-tab)
  (lsp-start-if-active)
  )


(defun my-c-mode-hook ()
  (common-c-hook c-mode-map)
  )



(defun my-c++-mode-hook ()
  (common-c-hook c++-mode-map)
  )


(defun my-rust-mode-hook ()
  (common-c-hook rust-mode-map)
  )



(add-hook 'rust-mode-hook 'my-rust-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(add-to-list 'auto-mode-alist '("\\.flex\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . c-mode))

(defun my-latex-mode-hook ()
  (define-key latex-mode-map "\C-c\C-v" 'do-compile)
  (define-key latex-mode-map "\C-c\C-o" 'latex-render)
  (define-key latex-mode-map "\C-x\C-o" 'latex-render)
  (define-key latex-mode-map "\C-c\C-r" 'do-recompile)
  (local-unset-key "\C-j")
  (local-set-key "\C-j" 'isearch-backward)
                                        ;  (whitespace-mode)
  (define-key isearch-mode-map "\C-j" 'isearch-repeat-backward)
  )

(add-hook 'latex-mode-hook 'my-latex-mode-hook)
(add-hook 'tex-mode-hook 'my-latex-mode-hook)
(add-hook 'TeX-mode-hook 'my-latex-mode-hook)
(add-hook 'plain-TeX-mode-hook 'my-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)
(add-hook 'AmS-TeX-mode-hook 'my-latex-mode-hook)
(add-hook 'ConTeXt-mode-hook 'my-latex-mode-hook)
(add-hook 'Texinfo-mode-hook 'my-latex-mode-hook)
(add-hook 'docTeX-mode-hook 'my-latex-mode-hook)

                                        ; from enberg on #emacs
                                        ;(add-hook 'compilation-finish-functions
                                        ;  (lambda (buf str)
                                        ;    (if (null (string-match ".*exited abnormally.*" str))
                                        ;        ;;no errors, make the compilation window go away in a few seconds
                                        ;        (progn
                                        ;(run-with-timer .25 nil
                                        ;                      (lambda (buf)
                                        ;                        (bury-buffer buf)
                                        ;                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                                        ;                      buffer)
                                        ;          (message "No Compilation Errors!")))))

                                        ;'(display-buffer-alist '(("*compilation*" (display-buffer-in-previous-window))))
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (or (string-match "compilation" (buffer-name buffer))
           (string-match "Python Check" (buffer-name buffer)))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "error:" nil t)))
       (not
        (with-current-buffer buffer
          (search-forward "warning:" nil t)))
       (not
        (with-current-buffer buffer
          (search-forward "warning," nil t)))
       (not
        (with-current-buffer buffer
          (search-forward "exited abnormally" nil t))))
      (run-with-timer .25 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf))
                        )
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(defun md-render ()
  (interactive)
  (cond ((equal buffer-file-name 'nil) (message "Unable to render non-file"))
        (t (let ((mdfile buffer-file-name))
             (let ((cmd (format "display-md %s" buffer-file-name)))
               (shell-command cmd)
               )
             )
           )
        )
  )

(defun latex-render ()
  (interactive)
  (cond ((equal buffer-file-name 'nil) (message "Unable to render non-file"))
        (t (let ((latexfile buffer-file-name))
             (let ((cmd (format "display-latex %s" buffer-file-name)))
               (shell-command cmd)
               )
             )
           )
        )
  )

(defalias 'html-render 'md-render)
(defun new-email ()
  (interactive)
  (let ((curtime (format-time-string "%Y-%m-%d-T%H-%M-%S")))
    (let ((newf (format "/home/noah/emails/email-%s.md" curtime)))
      (find-file-other-frame newf)
      (goto-char (point-max))
      (insert "\n\nBest,\n<br />\nNoah")
      (goto-char (point-min))
      )
    )
  )


(add-hook 'git-commit-mode-hook 'turn-off-auto-fill)

(setq explicit-shell-file-name "/bin/bash")
(defun ssh-username (ssh-host)
  (interactive)
  (cond ((string-match-p (regexp-quote "@")
                         ssh-host) (nth 0 (split-string ssh-host "@")))
        (t user-login-name))
  )

(defun spawn-shell (name)
  "Invoke shell test"
  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create name))
  (shell (current-buffer))
  )

(defun ssh-shell ()
  (interactive)
  (let ((host (read-string "Enter Host: ")))
    (let ((host-name (ssh-username host)))
      (let ((ssh-home-path (concat (concat (concat "/ssh:" host) ":/home/") host-name)))
        (let ((default-directory ssh-home-path))
          (find-file ssh-home-path)
          (let ((shell-name (if (get-buffer "shell") (concat "shell-" host-name) "shell")))
            (spawn-shell shell-name)
            )
          )
        )
      )
    )
  )



                                        ;(require 'flycheck-aspell)
                                        ;(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
                                        ;(add-to-list 'flycheck-checkers 'html-aspell-dynamic)

;;(add-hook 'markdown-mode-hook #'flymake-aspell-setup)

                                        ;(setq ispell-dictionary "english")
                                        ;(setq ispell-program-name "aspell")
                                        ;(setq ispell-silently-savep t)


                                        ;(advice-add #'ispell-pdict-save :after #'flycheck-maybe-recheck)
                                        ;(defun flycheck-maybe-recheck (_)
                                        ;  (when (bound-and-true-p flycheck-mode)
                                        ;    (flycheck-buffer)))

                                        ;echo -e "*nwg\n#" | aspell -a

(defun run-add-to-dict (new-word)
  (let ((cmd (format "add_to_dict %s 0" new-word)))
    (shell-command cmd)))

(defun add-to-dict (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (run-add-to-dict regionp))
    (run-add-to-dict (current-word))))

(defun run-force-add-to-dict (new-word)
  (let ((cmd (format "add_to_dict %s 1" new-word)))
    (shell-command cmd)))

(defun force-add-to-dict (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (run-force-add-to-dict regionp))
    (run-force-add-to-dict (current-word))))


(defun stats-n (start end n)
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (let ((cmd (format "simple-stats %s '%s'" n regionp)))
          (let ((out (shell-command-to-string cmd)))
            (message out)
            )
          )
        )
    )
  )

(defun stats-start (start end)
  (interactive "r")
  (stats-n start end "0"))

(defun stats (start end)
  (interactive "r")
  (stats-n start end (read-number "Field: " 0)))


(defun stats-end (start end)
  (interactive "r")
  (stats-n start end "-1"))



(global-unset-key "\C-x\C-l")


(eval-after-load "smerge-mode"
  '(progn (define-key smerge-mode-map (kbd "M-n") 'smerge-next))
  )
(eval-after-load "smerge-mode"
  '(progn (define-key smerge-mode-map (kbd "M-p") 'smerge-prev))
  )
(eval-after-load "smerge-mode"
  '(progn (define-key smerge-mode-map (kbd "C-u") 'smerge-keep-upper))
  )
(eval-after-load "smerge-mode"
  '(progn (define-key smerge-mode-map (kbd "C-l") 'smerge-keep-lower))
  )

(global-unset-key (kbd "C-8"))
(global-unset-key (kbd "C-9"))

(global-set-key (kbd "C-8") 'downcase-region)
(global-set-key (kbd "C-9") 'upcase-region)

(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun highlight-or-dehighlight-line ()
  (interactive)
  (if (find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
      (overlay-put overlay-highlight 'face '(:background "lightgreen"))
      (overlay-put overlay-highlight 'line-highlight-overlay-marker t)
      (run-with-timer .15 nil
                      (lambda (region)
                        (remove-overlays (car region) (cdr end))
                        )
                      ((line-beginning-position) (+ 1 (line-end-position))))

      )
    )
  )

(defun highlight-and-dehighlight-line ()
  (interactive)
  (let ((overlay-highlight (make-overlay
                            (line-beginning-position)
                            (+ 1 (line-end-position)))))
    (overlay-put overlay-highlight 'face '(:background "lightgreen"))
    (overlay-put overlay-highlight 'line-highlight-overlay-marker t)
    (run-with-timer .01 nil
                    (lambda (region)
                      (remove-overlays (car region) (cdr region))
                      )
                    (cons (line-beginning-position) (+ 1 (line-end-position))))

    )
  )

(defvar _lsp--sessions--active #s(hash-table size 30 data nil))
(defvar old-win-restore-undo '())
(defvar old-win-restore-coords '())
(defvar old-win-restore-idx 0)

(defun kill-xref ()
  (interactive)
  (let ((buf (get-buffer "*xref*")))
    (if buf
        (kill-buffer buf)
      nil)
    )
  )
(require 'nav-flash)


(defun bury-xref ()
  (interactive)
  (let ((buf (get-buffer "*xref*")))
    (if buf
        (progn
          (with-current-buffer buf
            (message (format "Number of reference: %d" (/ (count-lines (point-min) (point-max)) 2)))
            )
          (bury-buffer buf)
          (switch-to-prev-buffer (get-buffer-window buf)
                                 )
          )
      nil)
    )
  )
(defun display-xref ()
  (interactive)
  (let ((buf (get-buffer "*xref*")))
    (if buf
        (display-buffer buf)
      )
    (message "No xref active"))
  )



(defun flash-success(&optional timer)
  (interactive)
  (let ((flash-time  (if timer timer .15)))
    (let ((lstart (line-beginning-position)))
      (let ((lend (line-end-position)))
        (if (eq lstart lend)
            (nav-flash-show
             nil
             nil
             '((t (:background "#7FFF00" :extend t)))
             flash-time)
          (nav-flash-show
           lstart
           (+ 1 lend)
           '((t (:background "#7FFF00" :extend nil)))
           flash-time)
          )
        )
      )
    )
  )

(defun flash-failure(&optional timer)
  (interactive)
  (let ((flash-time  (if timer timer .15)))
    (let ((lstart (line-beginning-position)))
      (let ((lend (line-end-position)))
        (if (eq lstart lend)
            (nav-flash-show
             nil
             nil
             '((t (:background "#DC143C" :extend t)))
             flash-time)
          (nav-flash-show
           lstart
           (+ 1 lend)
           '((t (:background "#DC143C" :extend nil)))
           flash-time)
          )
        )
      )
    )
  )

(set-face-extend 'nav-flash-face t)
(defun butfirst (li)
  (let ((ret nil))
    (if li
        (if (not (eq (length li) 0))
            (progn
              (pop li)
              (setq ret li)
              )
          nil)
      nil)
    ret)
  )


(defun -filter-coords-list-buf (li buf match)
  (let ((li-out '()))
    (dolist (coord li)
      (when (eq (string-equal buf (car coord)) match) (push coord li-out))
      )
    li-out)
  )

(defun reset-coords-not-buf()
  (interactive)
  (setq old-win-restore-coords (-filter-coords-list-buf old-win-restore-coords (buffer-name) t))
  (setq old-win-restore-undo '())
  (setq old-win-restore-idx 0)
  )


(defun -reset-coords-buf(buf-name)
  (interactive)
  (setq old-win-restore-coords (-filter-coords-list-buf old-win-restore-coords buf-name nil))
  (setq old-win-restore-undo (-filter-coords-list-buf old-win-restore-undo buf-name nil))
  )

(defun reset-coords-buf()
  (interactive)
  (-reset-coords-buf (buffer-name))
  (setq old-win-restore-undo '())
  (setq old-win-restore-idx 0)
  )

(defun reset-coords ()
  (interactive)
  (setq old-win-restore-coords '())
  (setq old-win-restore-undo '())
  (setq old-win-restore-idx 0)
  )


(defun pop-coords ()
  (interactive)
  (setq old-win-restore-coords (butfirst old-win-restore-coords))
  (setq old-win-restore-idx 0)
  )

(defun coords-equal (a b)
  (interactive)
  (let ((ret nil))
    (when a
      (when b
        (when (string-equal (car a) (car b))
          (when (eq (cdr a) (cdr a))
            (setq ret t)
            )
          )
        )
      )
    ret)
  )


(defun get-cur-coords ()
  (interactive)
  (let ((buf (buffer-name)))
    (let ((pos (point)))
      (cons buf pos)
      )
    )
  )

(defun push-coords ()
  (interactive)
  (push (get-cur-coords) old-win-restore-coords)
  (setq old-win-restore-idx 0)
  )





(defun restore-to-coords(coords)
  (let ((ret nil))
    (let ((buf (car coords)))
      (let ((pos (cdr coords)))
        (if (get-buffer buf)
            (progn
              (setq ret t)
              (switch-to-buffer buf)
              (goto-char pos)
              (flash-success)
              )
          (-reset-coords-buf buf))
        )
      )
    ret)
  )

(if (eq nil nil) 1 2)


(defun -get-index-coords (idx li)
  (interactive)
  (let ((ret nil))
    (let ((list-len (length li)))
      (if (not (eq list-len 0))
          (let ((coords-idx (mod idx list-len)))
            (let ((coords (nth coords-idx li)))
              (setq ret coords)
              )
            )
        )
      )
    ret)
  )

(defun -restore-coords (coords)
  (let ((ret nil))
    (when coords
      (setq ret (if (restore-to-coords coords) coords t))
      )

    (when (not ret)
      (progn
        (flash-failure)
        (message "No coordinate history")
        )
      )
    ret)
  )

(defun -restore-coords-index (idx li)
  (interactive)
  (let ((ret nil))
    (let ((coords (-get-index-coords idx li)))
      (setq ret (-restore-coords coords))
      )
    ret
    )
  )

(defun restore-coords-index (idx)
  (interactive)
  (-restore-coords-index idx old-win-restore-coords)
  )

(if (> 1 1) t nil)

(defun restore-coords-undo ()
  (interactive)
  (-restore-coords-index 1 old-win-restore-undo)
  (setq old-win-restore-undo (butfirst old-win-restore-undo))
  )


(defun -restore-coords-last ()
  (interactive)
  (let ((ret (restore-coords-index 0)))
    (when (not (eq ret t))
      (setq old-win-restore-coords (butfirst old-win-restore-coords))
      (setq old-win-restore-idx 0)
      (when ret
        (push ret old-win-restore-undo)
        (if (> (length old-win-restore-undo) 3) (pop old-win-restore-undo) nil)
        )
      )
    ret)
  )

(defun -restore-coords-first ()
  (interactive)
  (let ((ret    (restore-coords-index (- (length old-win-restore-coords) 1))))
    (when (not (eq ret t))
      (setq old-win-restore-coords (butfirst old-win-restore-coords))
      (setq old-win-restore-idx 0)
      )
    ret)
  )

(defun -restore-coords-cycle-forward ()
  (interactive)
  (let ((ret nil))
    (when (coords-equal
           (get-cur-coords)
           (-get-index-coords (- old-win-restore-idx 1) old-win-restore-coords)
           )
      (setq old-win-restore-idx (- old-win-restore-idx 1))
      )
    (setq ret  (restore-coords-index (- old-win-restore-idx 1)))
    (when (not (eq ret t))
      (setq old-win-restore-idx (- old-win-restore-idx 1))
      )
    ret)
  )

(defun -restore-coords-cycle-backward ()
  (interactive)
  (let ((ret nil))
    (when (coords-equal
           (get-cur-coords)
           (-get-index-coords old-win-restore-idx old-win-restore-coords)
           )
      (setq old-win-restore-idx (+ old-win-restore-idx 1))
      )

    (setq ret (restore-coords-index old-win-restore-idx))
    (when (not (eq ret t))
      (setq old-win-restore-idx (+ old-win-restore-idx 1))
      )
    ret)
  )

(defun -restore-coords-driver (restore-func)
  (let ((ret t))
    (while (eq ret t)
      (setq ret (funcall restore-func))
      )
    )
  )

(defun restore-coords-cycle-backward ()
  (interactive)
  (-restore-coords-driver '-restore-coords-cycle-backward)
  )
(defun restore-coords-cycle-forward ()
  (interactive)
  (-restore-coords-driver '-restore-coords-cycle-forward)
  )
(defun restore-coords-first ()
  (interactive)
  (-restore-coords-driver '-restore-coords-first)
  )
(defun restore-coords-last ()
  (interactive)
  (-restore-coords-driver '-restore-coords-last)
  )

                                        ;  (define-key c-mode-map "\C-c\C-o" 'my-obj-dump)
(defalias 'lsp-reset 'lsp-workspace-restart)
                                        ;(global-unset-key (kbd "C-."))
(global-unset-key (kbd "C-0"))
(global-unset-key (kbd "C-o"))
                                        ;(global-unset-key (kbd "C-i"))
(global-unset-key (kbd "C--"))
(global-unset-key (kbd "C-="))
(global-unset-key "\C-l")
(global-unset-key (kbd "C-;"))
(global-unset-key (kbd "C-'"))

                                        ;(global-set-key (kbd "C-.") 'restore-coords-undo)
(global-set-key (kbd "C-0") 'reset-coords)
(global-set-key (kbd "C--") 'reset-coords-buf)
(global-set-key (kbd "C-=") 'reset-coords-not-buf)
                                        ;(global-set-key (kbd "C-o") 'push-coords)
(global-set-key (kbd "C-o") 'push-coords)
                                        ;(global-set-key (kbd "C-i") 'pop-coords)
(global-set-key "\C-l" 'restore-coords-last)
(global-set-key (kbd "C-;") 'restore-coords-cycle-backward)
(global-set-key (kbd "C-'") 'restore-coords-cycle-forward)




(global-set-key (kbd "\C-r\C-r")
                (lambda ()
                  (interactive)
                  (push-coords)
                  (lsp-find-definition)))
(global-set-key (kbd "\C-r\C-t")
                (lambda ()
                  (interactive)
                  (push-coords)
                  (lsp-find-declaration)))

(global-set-key "\C-r\C-x" 'display-xref)
                                        ;(global-set-key "\C-r\C-r" 'hidden-lsp-find-references)


(global-set-key (kbd "\C-r\C-s")
                (lambda ()
                  (interactive)
                  (kill-xref)
                  (let ((_old-win (get-buffer-window (current-buffer))))
                    (let ((lsp-ret (lsp-find-references)))
                      (if (not (stringp lsp-ret))
                          (progn
                            (push-coords)
                            (bury-xref)
                            (select-window _old-win)
                            (flash-success)
                            )
                        (flash-failure)
                        )
                      )
                    )
                  )
                )





(defun buffer-get-lsp-workspace (buf-name)
  (let ((ret))
    (let ((_lsp-cur-dir (lsp-session-folders (lsp-session))))
      (dolist (workspace _lsp-cur-dir)
        (when (string-match-p (regexp-quote workspace) buf-name)
          (setq ret workspace))

        )
      )
    ret)
  )




(defun lsp-enable-disable-buffer (cur-workspace buf-name todo)
  (let ((existing-buffer (get-buffer buf-name)))
    (if existing-buffer
        (progn
          (let ((buf-fname (buffer-file-name existing-buffer)))
            (if buf-fname
                (if (string-match-p (regexp-quote cur-workspace) buf-fname)
                    (progn
                      (message buf-fname)
                      (with-current-buffer existing-buffer
                        (if (equal todo 0)
                            (if (bound-and-true-p lsp-mode)
                                (call-interactively #'lsp-disconnect)
                              nil)
                          (if (equal todo 1)
                              (if (not (bound-and-true-p lsp-mode))
                                  (call-interactively #'lsp)
                                nil)
                            (if (equal todo 2)
                                (call-interactively #'lsp-workspace-shutdown)
                              nil)
                            )
                          )
                        )
                      )
                  nil)
              nil)
            )
          )
      nil)
    )
  )

(defun test ()
  (interactive)
  (message (buffer-get-lsp-workspace (buffer-file-name)))
  )

(defun find-buffer-in-workspace (workspace)
  (let ((ret nil))
    (let ((buf-list
           (mapcar
            (function buffer-name)
            (buffer-list)
            )
           )
          )

      (dolist (buf buf-list)
        (let ((existing-buffer (get-buffer buf)))
          (if existing-buffer
              (progn
                (let ((buf-fname (buffer-file-name existing-buffer)))
                  (if buf-fname
                      (if (string-match-p (regexp-quote workspace) buf-fname)
                          (if (equal (buffer-mode buf-fname) 'c-mode)
                              (setq ret buf)
                            nil)
                        (if (equal (buffer-mode buf-fname) 'c++-mode)
                            (setq ret buf)
                          nil)
                        nil)
                    nil)
                  )
                )
            nil)
          )
        )
      )
    ret)
  )


(defun lsp-act-on-workspace-buffers (cur-workspace todo)
  (let ((buf-list
         (mapcar
          (function buffer-name)
          (buffer-list)
          )
         )
        )
    (dolist (buf buf-list)
      (message buf)
      (lsp-enable-disable-buffer cur-workspace buf todo)
      )
    )
  )



(defun lsp-start ()
  (interactive)
  (call-interactively #'lsp)
  (setq company-idle-delay nil)
  (company-mode 1)
  (let ((cur-workspace (buffer-get-lsp-workspace (buffer-file-name))))
    (if cur-workspace
        (progn
          (puthash cur-workspace t  _lsp--sessions--active)
          (lsp-act-on-workspace-buffers cur-workspace 1)
          )
      nil)
    )
  )


(defun lsp-stop-workspace (cur-workspace)
  (if cur-workspace
      (progn
        (call-interactively #'lsp-workspace-shutdown)
        (remhash cur-workspace  _lsp--sessions--active)
        (lsp-act-on-workspace-buffers cur-workspace 0)
        )
    nil)
  )
(defun lsp-stop ()
  (interactive)
  (company-mode 0)
  (lsp-stop-workspace (buffer-get-lsp-workspace (buffer-file-name)))
  )

                                        ;(defun lsp-stop-all ()
                                        ;  (interactive)
                                        ;  (setq-local active-workspaces (hash-table-keys _lsp--sessions--active))
                                        ;  (dolist (workspace active-workspaces)
                                        ;    (setq-local workspace-buf (find-buffer-in-workspace workspace))
                                        ;    (if workspace-buf
                                        ;        (progn
                                        ;          (message (format "Shutting down %s -> %s" workspace workspace-buf))
                                        ;          (lsp-enable-disable-buffer workspace workspace-buf 2)
                                        ;          )
                                        ;      nil)
                                        ;    (remhash workspace  _lsp--sessions--active)
                                        ;    (lsp-act-on-workspace-buffers cur-workspace 0)
                                        ;    )
                                        ;  )

(defun _lsp-start-if-active (buf-file-name buf-name)
  (when (not (eq 0 (length (hash-table-keys _lsp--sessions--active))))
    (let ((cur-workspace (buffer-get-lsp-workspace buffer-file-name)))
      (if cur-workspace
          (if (gethash cur-workspace _lsp--sessions--active nil)
              (if (not (bound-and-true-p lsp-mode))
                  (setq company-idle-delay nil)
                (company-mode 1)
                (call-interactively #'lsp)
                nil)
            nil)
        nil)
      )
    )
  )


(defun lsp-start-if-active ()
  (interactive)
  (_lsp-start-if-active (buffer-file-name) (buffer-name)))





                                        ;  (setq-local cur-workspace (buffer-get-lsp-workspace (buffer-file-name)))




(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-lens-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-diagnostics-provider :none)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-diagnostics nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation nil)
(setq lsp-enable-links nil)
(setq lsp-keep-workspace-alive nil)
(setq lsp-enable-on-type-formatting nil)

(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(setq lsp-completion-provider :capf)
(setq lsp-completion-enable t)



                                        ;(corfu-mode -1)
                                        ;(lsp)
;; Unbind <C-i> from the TAB key and bind it to indent-region.
;; Since TAB and <C-i> cannot be differentiated in TTY emacs,
;; the workaround is to conditionally bind TAB to indent-region
;; when there is an active region selected.

(defun tab-or-complete ()
  (interactive)
  (indent-for-tab-command)
  )

(defun true-tab ()
  (interactive)
  (insert "\t")
  )

(global-unset-key (kbd "<C-return>"))
(global-set-key (kbd "<C-return>") 'my-company-complete)

                                        ;(define-key my-keys-mode-map (kbd "<C-return>") 'my-function)

(defun init-tab ()
  (if (window-system)
                                        ; IF we are not in a TTY, unbind C-i from TAB
      (progn
        (define-key input-decode-map [?\C-i] [C-i])
                                        ; ... and remap it to indent-region
        (global-set-key (kbd "<C-i>") 'true-tab))
                                        ; ELSE IF we are in a TTY, create a replacement for TAB
    (defun my/tab-replacement (&optional START END)
      (interactive "r")
      (if (use-region-p)
                                        ; IF active region, use indent-region
          (indent-region START END)
                                        ; ELSE IF no active region, use default tab command
        (tab-or-complete)))
                                        ; Bind our quick-and-dirty TAB replacement to the TAB key
    (global-set-key (kbd "TAB") 'my/tab-replacement))
  )

                                        ;(global-set-key (kbd "TAB") ')

(init-tab)

                                        ;(setq lsp-completion-provider '(:capf nil t))
                                        ;(global-company-mode)
                                        ;https://github.com/Andersbakken/rtags/wiki/Usage
                                        ;https://github.com/emacs-lsp/lsp-mode/issues/2415



(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))



(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))


(defun su-file ()
  (interactive)
  (let ((cur-buf-file (buffer-file-name)))
    (when cur-buf-file
      (let ((tramp-file-name (concat "/sudo::" cur-buf-file)))
        (find-file tramp-file-name)
        )
      )
    )
  )


(defun add-word ()
  (interactive)
  (let ((cur-word (buffer-substring (region-beginning) (region-end))))
    (call-process "add-word" nil nil nil cur-word)
    )
  )

(defun include-asm-hdr ()
  (interactive)
  (insert "#include \"/home/noah/programs/projects/string-dev/src/asm/libc-asm-common.h\"")
  )


(font-lock-add-keywords 'c-mode '(("__inline" . 'font-lock-keyword-face)))
(font-lock-add-keywords 'c++-mode '(("__inline" . 'font-lock-keyword-face)))

                                        ;
                                        ;(font-lock-add-keywords 'asm-mode '(("#^(if|endif|define|elif|ifdef|ifndef).*$" . 'font-lock-comment-face)))

                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_adjust_cfa_offset" '(font-lock-function-name-face ((((class color)) (:foreground "DarkBlue")))))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_rel_offset" . '(font-lock-face '(:foreground "pink")))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_def_cfa_register" 'font-lock-face '(:foreground "DarkBlue"))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_restore" 'font-lock-face '(:foreground "DarkBlue"))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_restore" 'font-lock-face '(:foreground "DarkBlue"))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_endproc" 'font-lock-face '(:foreground "DarkBlue"))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_startproc" 'font-lock-face '(:foreground "DarkBlue"))))
                                        ;(font-lock-add-keywords 'asm-mode '(("cfi_adjust_cfa_offset" 'font-lock-face '(:foreground "DarkBlue"))))


     ;; Add opam emacs directory to you load paths:
;     (defun opam-path (path)
;        (let ((opam-share-dir (ignore-errors (car (process-lines "opam" "config" "var"
;   "share")))))
;          (concat opam-share-dir "/" path)))
;     (add-to-list 'load-path (opam-path "emacs/site-lisp"))
;     ;; load bap-emacs-goodies
;     (require 'bap-mode)
;     (require 'dot)


(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))        

(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)
(setq hide-ifdef-shadow t)

;;              '((list1 ONE TWO)
;;                (list2 TWO THREE))))
