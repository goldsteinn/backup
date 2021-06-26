;; installed packages.  Don't delete this line.  If you don't want it,
;; Added by Package.el.  This must come before configurations of
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
        '(("melpa" . "http://melpa.org/packages/")
          ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
 '(package-selected-packages
        '(lsp-grammarly grammarly flycheck-pycheckers yapfify which-key use-package nasm-mode markdown-mode magit elpy cmake-mode)))
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
          
(setq python-indent-guess-indent-offset nil)
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


(defun default-compile(_base_command)
  (interactive)
  (setq _fname (file-name-nondirectory (buffer-name)))
  (setq _oname (file-name-base (buffer-name)))
  (setq _command0 (concat _base_command _fname))
  (setq _command1 (concat _command0 " -o "))
  (setq _command2 (concat _command1 _oname))
  (setq compile-command _command2)
  (call-interactively #'compile))


(defun default-cxx-compile()
  (interactive)
  (default-compile "g++ -O3 -std=c++17 -march=native -mtune=native "))


(defun default-c-compile()
  (interactive)
  (default-compile "gcc -O3 -march=native -mtune=native "))

(defun default-asm-compile()
  (interactive)
  (default-compile "gcc -static -nostartfiles -nodefaultlibs -nostdlib -Wl,--build-id=none "))



(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun cur-mode ()
  "Returns the major mode associated with a buffer."
  (with-current-buffer (current-buffer)
     major-mode))



(defun do-compile()
  (interactive)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'default-c-compile))
        ((equal (cur-mode) 'c++-mode) (call-interactively #'default-cxx-compile)) 
        ((equal (cur-mode) 'python-mode) (call-interactively #'elpy-check))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'default-asm-compile))
        (t (call-interactively #'compile))))


(defun do-recompile()
  (interactive)
  (cond ((equal (cur-mode) 'python-mode) (call-interactively #'elpy-check))
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

(global-set-key [?\M-5] 
		'(lambda ()
		   (interactive)
		   (my-set-reg 'a)))


(global-set-key [?\C-5] 
		'(lambda ()
		   (interactive)
		   (my-get-reg 'a)))




    
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

(global-set-key "\M-f" 'forward-whitespace)
(global-set-key "\M-b" 'backward-sexp)

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
(load "/usr/share/emacs/site-lisp/clang-format-11/clang-format.el")

(defun fill-buffer()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fill-region (point-min) (point-max)))))



(defun clean-region()
  (interactive)
  (whitespace-cleanup)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'clang-format-region))
        ((equal (cur-mode) 'c++-mode) (call-interactively #'clang-format-region)) 
        ((equal (cur-mode) 'python-mode) (call-interactively #'yapfify-region))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'abfify-region))
        (t (call-interactively #'fill-paragraph)))
		
	)

(defun clean-buffer()
  (whitespace-cleanup)
  (interactive)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'clang-format-buffer))
        ((equal (cur-mode) 'c++-mode) (call-interactively #'clang-format-buffer)) 
        ((equal (cur-mode) 'python-mode) (call-interactively #'yapfify-buffer))
        ((equal (cur-mode) 'asm-mode) (call-interactively #'abfify-buffer))
        (t (call-interactively #'fill-buffer)))

	)
(setq whitespace-space 'underline)  
(setq whitespace-style '(face lines lines-tail trailing))


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

(defun write-c-header ()
  (interactive)
  (goto-char (point-max))
  (insert "#include <assert.h>\n#include <immintrin.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <time.h>\n#include <x86intrin.h>\n\n\n#define ALWAYS_INLINE inline __attribute__((always_inline))\n#define NEVER_INLINE  __attribute__((noinline))\n#define CONST_ATTR    __attribute__((const))\n#define PURE_ATTR     __attribute__((pure))\n#define BENCH_ATTR    __attribute__((noinline, noclone, aligned(4096)))\n\n#define COMPILER_OOE_BARRIER() asm volatile(\"lfence\" : : : \"memory\")\n#define OOE_BARRIER()          asm volatile(\"lfence\" : : :)\n#define COMPILER_BARRIER() asm volatile(\"\" : : : \"memory\");\n#define COMPILER_DO_NOT_OPTIMIZE_OUT(X)                                        \\
    asm volatile(\"\" : : \"i,r,m\"(X) : \"memory\")\n\n#define _CAT(X, Y) X##Y\n#define CAT(X, Y) _CAT(X, Y)\n#define _V_TO_STR(X) #X\n#define V_TO_STR(X) _V_TO_STR(X)\n\n#define NO_LSD_RD(tmp, r) \"pop \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n#define NO_LSD_WR(tmp, r) \"push \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n\n#define IMPOSSIBLE(X)                                                          \\
    if (X) {                                                                   \\
        __builtin_unreachable();                                               \\
    }\n\n#define PRINT(...) fprintf(stderr, __VA_ARGS__)\n\n\nint\nmain(int argc, char ** argv) {\n}")
  ;(set-buffer-modified-p nil)
  )

(defun write-cc-header ()
  (interactive)
  (goto-char (point-max))
  (insert "#include <assert.h>\n#include <immintrin.h>\n#include <stdint.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <time.h>\n#include <x86intrin.h>\n#include <type_traits>\n\n#define ALWAYS_INLINE inline __attribute__((always_inline))\n#define NEVER_INLINE  __attribute__((noinline))\n#define CONST_ATTR    __attribute__((const))\n#define PURE_ATTR     __attribute__((pure))\n#define BENCH_ATTR    __attribute__((noinline, noclone, aligned(4096)))\n\n#define COMPILER_OOE_BARRIER() asm volatile(\"lfence\" : : : \"memory\")\n#define OOE_BARRIER()          asm volatile(\"lfence\" : : :)\n#define COMPILER_BARRIER() asm volatile(\"\" : : : \"memory\");\n#define COMPILER_DO_NOT_OPTIMIZE_OUT(X)                                        \\
    asm volatile(\"\" : : \"i,r,m\"(X) : \"memory\")\n\n#define _CAT(X, Y) X##Y\n#define CAT(X, Y) _CAT(X, Y)\n#define _V_TO_STR(X) #X\n#define V_TO_STR(X) _V_TO_STR(X)\n\n#define NO_LSD_RD(tmp, r) \"pop \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n#define NO_LSD_WR(tmp, r) \"push \" #tmp \"\\nmovq \" #r \", %%rsp\\n\"\n\n#define IMPOSSIBLE(X)                                                          \\
    if (X) {                                                                   \\
        __builtin_unreachable();                                               \\
    }\n\n#define PRINT(...) fprintf(stderr, __VA_ARGS__)\n\nenum timer_conf { CYCLES = 0, GETTIME = 1 };\ntemplate<timer_conf conf = GETTIME>\nstruct timer {\n    static constexpr clockid_t cid       = CLOCK_MONOTONIC;\n    static constexpr uint64_t  sec_to_ns = 1000 * 1000 * 1000;\n\n    using time_t = typename std::\n        conditional_t<conf == timer_conf::CYCLES, uint64_t, struct timespec>;\n\n    time_t tstart;\n    time_t tend;\n\n    const char * const\n    units() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return \"cycles\";\n        }\n        else {\n            return \"ns\";\n        }\n    }\n\n    void ALWAYS_INLINE\n    start() {\n        if constexpr (conf == timer_conf::CYCLES) {\n            tstart = _rdtsc();\n        }\n        else {\n            clock_gettime(cid, &tstart);\n        }\n    }\n\n    void ALWAYS_INLINE\n    end() {\n        if constexpr (conf == timer_conf::CYCLES) {\n            tend = _rdtsc();\n        }\n        else {\n            clock_gettime(cid, &tend);\n        }\n    }\n\n    uint64_t\n    get_start() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return tstart;\n        }\n        else {\n            return sec_to_ns * tstart.tv_sec + tstart.tv_nsec;\n        }\n    }\n    uint64_t\n    get_end() const {\n        if constexpr (conf == timer_conf::CYCLES) {\n            return tend;\n        }\n        else {\n            return sec_to_ns * tend.tv_sec + tend.tv_nsec;\n        }\n    }\n\n    uint64_t\n    dif() {\n        return get_end() - get_start();\n    }\n\n    double\n    ddif() {\n        return ((double)dif());\n    }\n\n    double\n    ddif(uint64_t n) {\n        return ddif() / ((double)n);\n    }\n\n    void\n    std_print() {\n        std_print(\"\", 1);\n    }\n\n    void\n    std_print(const char * const hdr) {\n        std_print(hdr, 1);\n    }\n\n    void\n    std_print(uint64_t n) {\n        std_print(\"\", n);\n    }\n\n    void\n    std_print(const char * const hdr, uint64_t n) {\n        if (hdr[0] == 0) {\n            fprintf(stderr, \"%.3E %s\\n\", ddif(n), units());\n        }\n        else {\n            fprintf(stderr, \"%-8s: %.3E %s\\n\", hdr, ddif(n), units());\n        }\n    }\n};\n\n\nint\nmain(int argc, char ** argv) {}\n\n")
  ;(set-buffer-modified-p nil)
  )

(defun newc()
  (interactive)
  (cond ((equal (cur-mode) 'c-mode) (call-interactively #'write-c-header))
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
(defun my-obj-dump ()
  (interactive)
  (cond ((equal buffer-file-name 'nil) (message "Unable to objdump non-file"))
        (t (let ((srcfile buffer-file-name))
             (let ((dstfile (format "/home/noah/tmp/objdumps/%s-%s.o" (file-name-base buffer-file-name)  (format-time-string "%Y-%m-%d-T%H-%M-%S"))))
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
                   (let ((ret (call-process "gcc" nil "*objdump-result*" "-v" "-c" "-march=native" "-O3" srcfile "-o" dstfile)))
                     (cond ((equal ret 0) (let* ((#1=#:v (get-buffer-create "*objdump-result*")))
                                            (with-current-buffer #1#
                                              (erase-buffer)
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


(global-unset-key "\C-c\C-o")
(global-set-key "\C-c\C-o" 'my-obj-dump)

(global-unset-key "\C-c\C-w")
(global-set-key "\C-c\C-w" 'whitespace-cleanup-region)


;         (let ((cmd (format "gcc -c %s -o /home/noah/tmp/%s-%s.o" buffer-file-name (file-name-base buffer-file-name)  (format-time-string "%Y-%m-%d-T%H-%M-%S"))))
 ;            (call-process cmd nil nil)))))
                 
;;(call-process )))
;;(format-time-string "%Y-%m-%d-T%H-%M-%S")

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
;  (whitespace-mode)
  (define-key isearch-mode-map "\C-j" 'isearch-repeat-backward)
)


(add-hook 'asm-mode-hook 'my-asm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(defun my-shell-hook ()
  (define-key shell-mode-map "\C-c\C-r" 'do-recompile)
  )
(add-hook 'shell-mode-hook 'my-shell-hook)



(defun my-c-mode-hook ()
  (define-key c-mode-map "\C-c\C-o" 'my-obj-dump)
  (define-key c-mode-map "\C-c\C-w" 'whitespace-cleanup-region)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)



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
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
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
