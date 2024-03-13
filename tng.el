;;; package --- tng.el

;;; Commentary:
;;; based on linum.el

;;; TODO:
;;; 

;;; Code:

(defvar-local
    tng-overlays (make-hash-table) ; TODO: vector
  "Overlays used in this buffer.")

(defvar-local tng-db-filename "tango.sqlite3")

(defun tng-setup-meta ()
  "Setup the meta-info for each line."
  (let* ((tngpy-response
          (json-read-from-string
           (shell-command-to-string
            ".\\v\\Scripts\\python.exe tango.py file-status tng.el"))))
    (save-excursion
      (cl-loop
       for line-meta-info
       across tngpy-response
       do
       ;; create overlays, set margins. TODO: collect to vector `tng-overlays'
       (let-alist line-meta-info
         (goto-line .line)
         (let* ((ov (make-overlay (point) (point)))
                (str (tng-get-margin-str line-meta-info)))
           (puthash
            .line
            (gethash .line tng-overlays ov)
            tng-overlays)
           (overlay-put ov 'before-string
                        (propertize " " 'display
                                    `((margin right-margin) ,str)))))))))

(defun tng-get-margin-str (line-meta-alist)
  "Get tng margin str for line."
  (let-alist line-meta-alist
    (let* ((left-bracket (propertize "[" 'face 'bold))
           (marker (propertize (if (zerop .status) " " "*") 'face 'bold))
           (right-bracket (propertize "]" 'face 'bold))
           (face (if (or (> .line 100) (< .line 10)) 'bold 'shadow)))
      (format "%s%s%s]" left-bracket marker right-bracket))))


;;; tng major mode for displaying deps

;;; header-line-format

(defun tng-goto-next-dst ())
(defun tng-goto-next-src ())
(defun tng-show-pop-up ())
(defun tng-save-current-lines ())
(defun tng-add-src-to-res-under-point ())
(defun tng-add-dst-to-res-under-point ())

(defun tng-cc ()
  (interactive)
  (message "TNG C-C"))

(defun tng-send-region (arg begin end)
  "Add new resource from the region"
  (interactive "P\nr")
  (if (region-active-p)
      (let* ((begin-line (line-number-at-pos begin t))
             (end-line (line-number-at-pos end t))
             (sha1-hash (sha1 (buffer-substring-no-properties begin end)))
             (filename (buffer-file-name))
             (comment (if arg (read-from-minibuffer "Comment for this chunk: "))))
        (sqlite-execute
         (sqlite-open tng-db-filename)
         "INSERT \
INTO resource(filename,comment,start_line,end_line) \
VALUES (?,?,?,?)"
         (list filename comment begin-line end-line))
        (message "lines: %s %s %s [arg: %s]" begin-line end-line sha1-hash arg))
    (error "No region selected")))

(defvar tng-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c") #'tng-cc)
    (define-key map (kbd "v") #'tng-describe-line)
    (define-key map (kbd "i") #'tng-mode)
    (define-key map (kbd "a") #'tng-send-region)
    map))

(global-set-key (kbd "C-c t") tng-keymap)

(defun tng-vtable-buffer ()
  "Pop a BUF-NAME with a `vtable' of COLUMNS listing OBJECTS.
ACTIONS should be a plist of key and command. See the vtable
manual for details."
  (interactive)
  (let ((buf-name "*Tng list*")
        (columns '("ID" "Name" "N"))
        (objects '((1 "n1" 1) (2 "n2" 2))))
    (with-current-buffer (get-buffer-create buf-name)
      (require 'vtable)
      (setf buffer-read-only nil)
      (erase-buffer)
      (make-vtable
       :columns columns
       :objects objects
       ;; force fixed width face
       :face 'default
       ;; sort by display text (or, name)
       :sort-by '((0 . ascend))
       :keymap (define-keymap
                 "q" #'quit-window)
       ;; :actions actions
       )
      (local-set-key (kbd "q") #'quit-window)
      (setf buffer-read-only t))
    (pop-to-buffer buf-name)))

(defgroup tng nil
  "Show tng status in the right margin."
  :group 'convenience)

(defface tng-shadow
  '(
    (t :inherit (shadow default))
    )
  "Face for displaying tng status in the display margin."
  :group 'tng)

(defface tng-highlight
  '(
    (t :inherit (highlight default))
    )
  "Face for displaying tng status in the display margin."
  :group 'tng)

(define-minor-mode tng-mode
  "Tango mode."
  :lighter " T"
  (if tng-mode
      (progn
        (add-hook 'window-scroll-functions 'tng-after-scroll nil t)
        (add-hook 'change-major-mode-hook 'tng-delete-overlays nil t)
        (add-hook 'window-configuration-change-hook
                  'tng-update-current nil t)
        (add-hook 'post-command-hook 'tng-update-current nil t)
        (add-hook 'after-change-functions 'tng-after-change t)
        (tng-delete-overlays)
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
        (maphash (lambda (k v) (delete-overlay v)) tng-overlays)
        (clrhash tng-overlays)
        (mapc #'tng-update-window
              (get-buffer-window-list buffer nil 'visible)))))


(defun tng-delete-overlays ()
  "Delete tng overlays."
  (maphash (lambda (k v) (delete-overlay v)) tng-overlays)
  (clrhash tng-overlays)
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

(defun tng-update-window (win)
  "Set window margins in window WIN."
  (tng-setup-meta)
  (let ((width 3)
        (existing-margins (window-margins win)))
    (progn
      (when (display-graphic-p)
        (setq width (ceiling
                     (/ (* width 1.0 (tng--face-width 'default))
                        (frame-char-width)))))
      (when (> width (or (cdr existing-margins) 0))
        (set-window-margins win (car existing-margins) width)
        (set-window-parameter win 'tng--set-margins (window-margins win))))))

(provide 'tng)
;;; tng.el ends here
