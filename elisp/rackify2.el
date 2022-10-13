;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This file is a near copy of yapfify.el [https://github.com/JorisE/yapfify]
;
;(require 'cl-lib)
;
;(defcustom rackify-executable "raco"
;  "Executable used to start rack."
;  :type 'string
;  :group 'rackify)
;
;(defun rackify-call-bin (input-buffer output-buffer)
;  "Call process rack on INPUT-BUFFER saving the output to OUTPUT-BUFFER.
;   Return the exit code."
;  (with-current-buffer input-buffer
;    (call-process rackify-executable nil output-buffer nil "fmt" (buffer-file-name))))
;                         
;
;;;;###autoload
;(defun rackify-region-kernel ()
;  "Try to rackify the current region.
;   If rack exits with an error, the output will be shown in a help-window."
;  (interactive "r")
;  (when (get-buffer "*rackify*")
;    (kill-buffer "*rackify*"))
;  (let* ((original-buffer (current-buffer))
;         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
;         (buffer-windows (get-buffer-window-list original-buffer nil t))
;         (original-window-pos (mapcar 'window-start buffer-windows))
;
;         (tmpbuf (get-buffer-create "*rackify*"))
;         (exit-code (rackify-call-bin original-buffer tmpbuf)))
;    (deactivate-mark)
;    ;; There are three exit-codes defined for RACK:
;    ;; 0: Exit with success (change or no change on rack >=0.11)
;    ;; 1: Exit with error
;    ;; 2: Exit with success and change (Backward compatibility)
;    ;; anything else would be very unexpected.
;    (cond ((eq exit-code 0)
;           (with-current-buffer tmpbuf
;             (copy-to-buffer original-buffer (point-min) (point-max))))
;          ((eq exit-code 1)
;           (error "Rack failed, see %s buffer for details" (buffer-name tmpbuf))))
;    ;; Clean up tmpbuf
;    ;;(kill-buffer tmpbuf)
;    ;; restore window to similar state
;    (goto-char original-point)
;    (cl-mapcar 'set-window-start buffer-windows original-window-pos))) 
;
;;;;###autoload
;(defun rackify-buffer ()
;  "Rackify whole buffer."
;  (interactive)
;  (rackify-region-kernel))
;
;
;;;;###autoload
;(define-minor-mode rack-mode
;  "Automatically run RACK before saving."
;  :lighter " RACK"
;  (if rack-mode
;      (add-hook 'before-save-hook 'rackify-buffer nil t)
;    (remove-hook 'before-save-hook 'rackify-buffer t)))
;
;;(provide 'rackify)
;
;;;; rackify.el ends here
;
