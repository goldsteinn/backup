;;; rackify.el --- (automatically) format python buffers using RACKF.

;; Copyright (C) 2016 Joris Engbers

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Homepage: https://github.com/JorisE/rackify
;; Version: 0.0.9
;; Package-Version: 20200406.830
;; Package-Commit: 3df4e8ce65f55fd69479b3417525ce83a2b00b45
;; Package-Requires: ()

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
;; Rackify uses rackf to format a Python buffer. It can be called explicitly on a
;; certain buffer, but more conveniently, a minor-mode 'rackf-mode' is provided
;; that turns on automatically running RACKF on a buffer before saving.
;;
;; Installation:
;;
;; Add rackify.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the function
;; rackf-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'rackf-mode)
;;
;;; Code:

(require 'cl-lib)

(defcustom rackify-executable "raco-fmt.py"
  "Executable used to start rackf."
  :type 'string
  :group 'rackify)

(defun rackify-call-bin (input-buffer output-buffer start-line end-line)
  "Call process rackf on INPUT-BUFFER saving the output to OUTPUT-BUFFER.

Return the exit code.  START-LINE and END-LINE specify region to
format."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max)
                         rackify-executable nil output-buffer
                         nil (concat (number-to-string start-line) "-"
                                     (number-to-string end-line)))))

(defun get-buffer-string (buffer)
  "Return the contents of BUFFER."
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun rackify-region (beginning end)
  "Try to rackify the current region.

If rackf exits with an error, the output will be shown in a help-window."
  (interactive "r")
  (when (get-buffer "*rackify*")
    (kill-buffer "*rackify*"))
  (let* ((original-buffer (current-buffer))
         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
         (buffer-windows (get-buffer-window-list original-buffer nil t))
         (original-window-pos (mapcar 'window-start buffer-windows))
         (start-line (line-number-at-pos beginning))
         (end-line (line-number-at-pos (if (or (= (char-before end) 10)
                                               (= (char-before end) 13))
                                           (- end 1)
                                         end)))

         (tmpbuf (get-buffer-create "*rackify*"))
         (exit-code (rackify-call-bin original-buffer tmpbuf start-line end-line)))
    (deactivate-mark)
    ;; There are three exit-codes defined for RACKF:
    ;; 0: Exit with success (change or no change on rackf >=0.11)
    ;; 1: Exit with error
    ;; 2: Exit with success and change (Backward compatibility)
    ;; anything else would be very unexpected.
    (cond ((equal exit-code 0)
           (with-current-buffer tmpbuf
             (copy-to-buffer original-buffer (point-min) (point-max))))
          (t
           (error "Rackf failed, see %s buffer for details" (buffer-name tmpbuf))))
    ;; Clean up tmpbuf
    (kill-buffer tmpbuf)
    ;; restore window to similar state
    (goto-char original-point)
    (cl-mapcar 'set-window-start buffer-windows original-window-pos)))

;;;###autoload
(defun rackify-buffer ()
  "Rackify whole buffer."
  (interactive)
  (rackify-region (point-min) (point-max)))

;;;###autoload
(defun rackify-region-or-buffer ()
  "Rackify the region if it is active. Otherwise, rackify the buffer"
  (interactive)
  (if (region-active-p)
      (rackify-region (region-beginning) (region-end))
    (rackify-buffer)))


(provide 'rackify)

;;; rackify.el ends here
