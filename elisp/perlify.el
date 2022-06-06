;;; perlify.el --- (automatically)

;; Based heavily on yapfify
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
;; Perlify uses perlf to perl a Python buffer. It can be called explicitly on a
;; certain buffer, but more conveniently, a minor-mode 'perlf-mode' is provided
;; that turns on automatically running PERLF on a buffer before saving.
;;
;; Installation:
;;
;; Add perlify.el to your load-path.
;;
;; To automatically perl all Python buffers before saving, add the function
;; perlf-mode to python-mode-hook:
;;
;;
;;; Code:

(require 'cl-lib)

(defcustom perlify-executable "perltidy"
  "Executable used to start perlf."
  :type 'string
  :group 'perlify)


(defun perlify-call-bin (input-buffer output-buffer start-line end-line)
  "Call process perlf on INPUT-BUFFER saving the output to OUTPUT-BUFFER.

Return the exit code.  START-LINE and END-LINE specify region to
perl."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max)
                         perlify-executable nil output-buffer
                         nil)))

(defun get-buffer-string (buffer)
  "Return the contents of BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun perlify-region (beginning end)
  "Try to perlify the current region.

If perlf exits with an error, the output will be shown in a help-window."
  (interactive "r")
  (when (get-buffer "*perlify*")
    (kill-buffer "*perlify*"))
  (let* ((original-buffer (current-buffer))
         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
         (buffer-windows (get-buffer-window-list original-buffer nil t))
         (original-window-pos (mapcar 'window-start buffer-windows))
         (start-line (line-number-at-pos beginning))
         (end-line (line-number-at-pos (if (or (= (char-before end) 10)
                                               (= (char-before end) 13))
                                           (- end 1)
                                         end)))

         (tmpbuf (get-buffer-create "*perlify*"))
         (exit-code (perlify-call-bin original-buffer tmpbuf start-line end-line)))
    (deactivate-mark)
    ;; There are three exit-codes defined for PERLF:
    ;; 0: Exit with success (change or no change on perlf >=0.11)
    ;; 1: Exit with error
    ;; 2: Exit with success and change (Backward compatibility)
    ;; anything else would be very unexpected.
    (cond ((equal exit-code 0)
           (with-current-buffer tmpbuf
             (copy-to-buffer original-buffer (point-min) (point-max))))
          (t
           (error "Perlf failed, see %s buffer for details" (buffer-name tmpbuf))))
    ;; Clean up tmpbuf
    (kill-buffer tmpbuf)
    ;; restore window to similar state
    (goto-char original-point)
    (cl-mapcar 'set-window-start buffer-windows original-window-pos)))

;;;###autoload
(defun perlify-buffer ()
  "Perlify whole buffer."
  (interactive)
  (perlify-region (point-min) (point-max)))

;;;###autoload
(defun perlify-region-or-buffer ()
  "Perlify the region if it is active. Otherwise, perlify the buffer"
  (interactive)
  (if (region-active-p)
      (perlify-region (region-beginning) (region-end))
    (perlify-buffer)))



(provide 'perlify)

;;; perlify.el ends here
