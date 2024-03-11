;;; package --- tng.el
;;; Commentary:

;;; Code:
(defvar-local tng-overlays nil "Overlays used in this buffer.")
(defvar-local tng-available nil "Overlays available for reuse.")

(defgroup tng nil
  "Show line numbers in the right  margin."
  :group 'convenience)

(defface tng
  '((t :inherit (shadow default)))
  "Face for displaying line numbers in the display margin."
  :group 'tng)

(define-minor-mode tng-mode
  "Margin and lighter."
  :lighter " MM"
  (if tng-mode
      (progn
        (add-hook 'window-scroll-functions 'tng-after-scroll nil t)
        (add-hook 'change-major-mode-hook 'tng-delete-overlays nil t)
        (add-hook 'window-configuration-change-hook
                  'tng-update-current nil t)
        (add-hook 'post-command-hook 'tng-update-current nil t)
        (add-hook 'after-change-functions 'tng-after-change t)
        (tng-update-current))
    (remove-hook 'post-command-hook 'tng-update-current t)
    (remove-hook 'window-scroll-functions 'tng-after-scroll t)
    (remove-hook 'after-change-functions 'tng-after-change t)
    (remove-hook 'window-configuration-change-hook 'tng-update-current t)
    (remove-hook 'change-major-mode-hook 'tng-delete-overlays t)
    (tng-delete-overlays)))

(defun tng-after-change (beg end _len)
  "Update overlays on deletions, and after newlines are inserted BEG END _LEN."
  (when (or (= beg end)
            (= end (point-max))
            (string-search "\n" (buffer-substring-no-properties beg end)))
    (tng-update-current)))

(defun tng-after-scroll (win _start)
  "Tng after scroll WIN _START."
  (tng-update (window-buffer win)))


(defun tng-update (buffer)
  "Tng update BUFFER."
  (with-current-buffer buffer
    (when tng-mode
      (setq tng-available tng-overlays)
      (setq tng-overlays nil)
      (save-excursion
        (mapc #'tng-update-window
              (get-buffer-window-list buffer nil 'visible)))
      (mapc #'delete-overlay tng-available)
      (setq tng-available nil))))


(defun tng-delete-overlays ()
  "Delete tng overlays."
  (mapc #'delete-overlay tng-overlays)
  (setq tng-overlays nil)
  (dolist (w (get-buffer-window-list (current-buffer) nil t))
    (let ((set-margins (window-parameter w 'tng--set-margins))
          (current-margins (window-margins w)))
      (when (and set-margins
                 (equal set-margins current-margins))
        (set-window-margins w (car current-margins) 0)
        (set-window-parameter w 'tng--set-margins nil)))))


(defun tng-update-current ()
    "Tng update current."
  (tng-update (current-buffer)))

(declare-function font-info "font.c" (name &optional frame))

(defun tng--face-width (face)
  "Tng face width FACE."
  (let ((info (font-info (face-font face)))
        width)
    (setq width (aref info 11))
    (if (<= width 0)
        (setq width (aref info 10)))
    width))

(defun tng-get-status (filename line)
  "Get tng status for line.  FILENAME LINE."
  "[pew]"
  )


(defun tng-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let ((line (line-number-at-pos))
        (limit (window-end win t))
        (fmt "%s")
        (width 0))
    (while (and (not (eobp)) (< (point) limit))
      (let* ((str (tng-get-status "filename" line))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
                                 (overlay-get o 'tng-str) str)
                            (unless (memq o tng-overlays)
                              (push o tng-overlays))
                            (setq tng-available (delq o tng-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null tng-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop tng-available) (point) (point)))))
            (push ov tng-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin right-margin) ,str)))
            (overlay-put ov 'tng-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (when (display-graphic-p)
      (setq width (ceiling
                   (/ (* width 1.0 (tng--face-width 'tng))
                      (frame-char-width)))))
    (let ((existing-margins (window-margins win)))
      (when (> width (or (cdr existing-margins) 0))
        (set-window-margins win (car existing-margins) width)
        (set-window-parameter win 'tng--set-margins (window-margins win))))))


(provide 'tng)
;;; tng.el ends here
