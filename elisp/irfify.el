;;; irfify.el --- (automatically) format python buffers using IRF.

;; Copyright (C) 2016 Joris Engbers

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Homepage: https://github.com/JorisE/irfify
;; Version: 0.0.9
;; Package-Version: 20200406.830
;; Package-Commit: 3df4e8ce65f55fd69479b3417525ce83a2b00b45
;; Package-Requirfes: ()

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Irfify uses irf to format a Python buffer. It can be called explicitly on a
;; certain buffer, but more conveniently, a minor-mode 'irf-mode' is provided
;; that turns on automatically running IRF on a buffer before saving.
;;
;; Installation:
;;
;; Add irfify.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the function
;; irf-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'irf-mode)
;;
;;; Code:

(require 'cl-lib)

(defcustom irfify-executable "fmt-irf"
  "Executable used to start irf."
  :type 'string
  :group 'irfify)

(defun irfify-call-bin (input-buffer output-buffer start-line end-line)
  "Call process irf on INPUT-BUFFER saving the output to OUTPUT-BUFFER.

Return the exit code.  START-LINE and END-LINE specify region to
format."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max)
                         irfify-executable nil output-buffer
                         nil "-l" "--lines" (concat (number-to-string start-line) ","
                                                    (number-to-string end-line)))))

(defun get-buffer-string (buffer)
  "Return the contents of BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun irfify-region-kernel (beginning end)
  "Try to irfify the current region.

If irf exits with an error, the output will be shown in a help-window."
  (interactive "r")
  (when (get-buffer "*irfify*")
    (kill-buffer "*irfify*"))
  (let* ((original-buffer (current-buffer))
         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
         (buffer-windows (get-buffer-window-list original-buffer nil t))
         (original-window-pos (mapcar 'window-start buffer-windows))
         (start-line (line-number-at-pos beginning))
         (end-line (line-number-at-pos (if (or (= (char-before end) 10)
                                               (= (char-before end) 13))
                                           (- end 1)
                                         end)))

         (tmpbuf (get-buffer-create "*irfify*"))
         (exit-code (irfify-call-bin original-buffer tmpbuf start-line end-line)))
    (deactivate-mark)
    ;; There are three exit-codes defined for IRF:
    ;; 0: Exit with success (change or no change on irf >=0.11)
    ;; 1: Exit with error
    ;; 2: Exit with success and change (Backward compatibility)
    ;; anything else would be very unexpected.
    (cond ((eq exit-code 0)
           (with-current-buffer tmpbuf
             (copy-to-buffer original-buffer (point-min) (point-max))))
          ((eq exit-code 1)
           (error "Irf failed, see %s buffer for details" (buffer-name tmpbuf))))
    ;; Clean up tmpbuf
    ;;(kill-buffer tmpbuf)
    ;; restore window to similar state
    (goto-char original-point)
    (cl-mapcar 'set-window-start buffer-windows original-window-pos)))

;;;###autoload
(defun irfify-buffer ()
  "Irfify whole buffer."
  (interactive)
  (irfify-region-kernel (point-min) (point-max)))

;;;###autoload
(defun irfify-region ()
  "Irfify whole buffer."
  (interactive)
  (irfify-region-kernel (region-beginning) (region-end)))

;;;###autoload
(defun irfify-region-or-buffer ()
  "Irfify the region if it is active. Otherwise, irfify the buffer"
  (interactive)
  (if (region-active-p)
      (irfify-region-kernel (region-beginning) (region-end) "--no-indent" "--no-skip")
    (irfify-buffer)))

;;;###autoload
(define-minor-mode irf-mode
  "Automatically run IRF before saving."
  :lighter " IRF"
  (if irf-mode
      (add-hook 'before-save-hook 'irfify-buffer nil t)
    (remove-hook 'before-save-hook 'irfify-buffer t)))

(provide 'irfify)

;;; irfify.el ends here
