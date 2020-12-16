;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))))
 '(package-selected-packages
   (quote
    (nasm-mode markdown-mode cmake-mode flycheck-pycheckers elpy which-key use-package auto-complete string-inflection yapfify pdf-tools go-mode rust-mode sml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq exec-path (append exec-path '("/home/noah/.local/bin:$PATH")))
;; unset annoying ass binding
;;(global-set-key (kbd "C-[") nil)
;;(global-unset-key (kbd "C-["))
;; kill-ring to clipboard mapping

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
(elpy-enable)
(setq inhibit-startup-screen t)
(setq elpy-rpc-python-command "python3")
(eval-after-load "python"
  '(progn (define-key python-mode-map (kbd "C-c C-r") nil)))
          


;;buffer menu on current buffer
(defun close()
    (interactive)
    (if (equal 1 (length (frame-list)))
        (save-buffers-kill-terminal)
      (delete-frame))
)

(defun default-make()
  (interactive)
  (setq compile-command "make -k")
  (call-interactively #'compile))
(defalias 'll 'do-recompile)
(defalias 'mm 'default-make)

(defun default-compile()
  (interactive)
  (setq _fname (file-name-nondirectory (buffer-name)))
  (setq _oname (file-name-base (buffer-name)))
  (setq _base_command "g++ -O3 -std=c++17 -march=native -mtune=native ")
  (setq _command0 (concat _base_command _fname))
  (setq _command1 (concat _command0 " -o "))
  (setq _command2 (concat _command1 _oname))
  (setq compile-command _command2)
  (call-interactively #'compile))

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun do-compile()
  (interactive)
  (cond ((equal major-mode 'python-mode) (call-interactively #'elpy-check))
(t (call-interactively #'default-compile))))

(defun do-recompile()
  (interactive)
  (cond ((equal major-mode 'python-mode) (call-interactively #'elpy-check))
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
(global-set-key "\C-j" 'isearch-backward)
(define-key isearch-mode-map "\C-j" 'isearch-repeat-backward)

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
		   (comment-line)
		   )
		)

(global-set-key [?\C-8]
		'(lambda ()
		   (interactive)
		   (hash-line)
		   )
		)

(global-set-key [?\C-6]
		'(lambda ()
		   (interactive)
		   (dash-line)
		   )
		)

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
(load "/usr/share/emacs/site-lisp/clang-format-10/clang-format.el")

(defun fill-buffer()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fill-region (point-min) (point-max)))))
    
(defun clean-region()
  (interactive)
  (cond ((equal major-mode 'c-mode) (call-interactively #'clang-format-region))
        ((equal major-mode 'c++-mode) (call-interactively #'clang-format-region)) 
        ((equal major-mode 'python-mode) (call-interactively #'yapfify-region))
        (t (call-interactively #'fill-buffer))))

(defun clean-buffer()
  (interactive)
  (cond ((equal major-mode 'c-mode) (call-interactively #'clang-format-buffer))
        ((equal major-mode 'c++-mode) (call-interactively #'clang-format-buffer)) 
        ((equal major-mode 'python-mode) (call-interactively #'yapfify-buffer))
        (t (call-interactively #'fill-buffer))))

(global-unset-key (kbd "\M-q"))
(global-set-key (kbd "\M-q") 'clean-region)

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

(global-unset-key "\C-x\C-r")
(global-set-key "\C-x\C-r" 'rename-current-buffer-file)

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

(defun write-std-header ()
  (interactive)
  (goto-char (point-max))
  (insert "#include <assert.h>\n#include <immintrin.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <time.h>\n#include <x86intrin.h>\n#include <type_traits>\n\n#define ALWAYS_INLINE inline __attribute__((always_inline))\n#define NEVER_INLINE  __attribute__((noinline))\n#define CONST_ATTR    __attribute__((const))\n#define PURE_ATTR     __attribute__((pure))\n\n#define COMPILER_BARRIER() asm volatile(\"\" : : : \"memory\");\n#define COMPILER_DO_NOT_OPTIMIZE_OUT(X)                                        \\
    asm volatile(\"\" : : \"i,r,m\"(X) : \"memory\")\n\n#define IMPOSSIBLE(X)                                                          \\
    if (X) {                                                                   \\
        __builtin_unreachable();                                               \\
    }\n\n#define PRINT(...) fprintf(stderr, __VA_ARGS__)\n\nenum timer_conf { CYCLES = 0, GETTIME = 1 };\ntemplate<timer_conf conf = GETTIME>\nstruct timer {\n    static constexpr clockid_t cid       = CLOCK_MONOTONIC;\n    static constexpr uint64_t  sec_to_ns = 1000 * 1000 * 1000;\n\n    using time_t = typename std::\n        conditional_t<conf == timer_conf::CYCLES, uint64_t, struct timespec>;\n\n    time_t tstart;\n    time_t tend;\n\n    const char * const\n    units() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return \"cycles\";\n        }\n        else {\n            return \"ns\";\n        }\n    }\n\n    void ALWAYS_INLINE\n    start() {\n        if constexpr (conf == timer_conf::CYCLES) {\n            tstart = _rdtsc();\n        }\n        else {\n            clock_gettime(cid, &tstart);\n        }\n    }\n\n    void ALWAYS_INLINE\n    end() {\n        if constexpr (conf == timer_conf::CYCLES) {\n            tend = _rdtsc();\n        }\n        else {\n            clock_gettime(cid, &tend);\n        }\n    }\n\n    uint64_t\n    get_start() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return tstart;\n        }\n        else {\n            return sec_to_ns * tstart.tv_sec + tstart.tv_nsec;\n        }\n    }\n    uint64_t\n    get_end() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return tend;\n        }\n        else {\n            return sec_to_ns * tend.tv_sec + tend.tv_nsec;\n        }\n    }\n\n    uint64_t\n    dif() {\n        return get_end() - get_start();\n    }\n\n    double\n    ddif() {\n        return ((double)dif());\n    }\n\n    double\n    ddif(uint64_t n) {\n        return ddif() / ((double)n);\n    }\n\n    void\n    std_print() {\n        std_print(\"\", 1);\n    }\n\n    void\n    std_print(const char * const hdr) {\n        std_print(hdr, 1);\n    }\n\n    void\n    std_print(uint64_t n) {\n        std_print(\"\", n);\n    }\n\n    void\n    std_print(const char * const hdr, uint64_t n) {\n        if (hdr[0] == 0) {\n            fprintf(stderr, \"%.3E %s\\n\", ddif(n), units());\n        }\n        else {\n            fprintf(stderr, \"%-8s: %.3E %s\\n\", hdr, ddif(n), units());\n        }\n    }\n};\n\n\nint\nmain(int argc, char ** argv) {}\n\n")
  (set-buffer-modified-p nil))

(defalias 'newc 'write-std-header)

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
  (cond ((equal major-mode 'asm-mode) (call-interactively #'my-asm-tab))
        (t (call-interactively #'indent-for-tab-command))))

  

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
  (define-key isearch-mode-map "\C-j" 'isearch-repeat-backward)
)


(add-hook 'asm-mode-hook 'my-asm-mode-hook)


