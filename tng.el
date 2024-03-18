;;; package --- tng.el -*- lexical-binding:t; coding:utf-8 -*-

;;; Commentary:
;;; 
;;; "C-c t a" `#'tng-add-region'
;;; "C-c t i" `#'tng-mode'
;;; 
;;; tng major mode to view dependencies
;;; - list all resources for current file
;;; - list dependencies for each resource
;;; - delete resource
;;; - goto
;;;
;;; - 
;;; 

;;; TODO:
;;; 

;;; Code:

(require 'cl-lib)
(require 'magit-section)

(defvar-local
    tng-overlays (make-hash-table) ; TODO: vector
  "Overlays used in this buffer.")

(defvar-local tng-project-dir "c:/Users/power/dev/tng/")

(defvar-local tng-db-filename
    (file-name-concat tng-project-dir "tango.sqlite3"))

(defun tng-init-db (&optional dir)
  "Create tng DB in the project root dir."
  (progn
    (sqlite-execute
     (sqlite-open tng-db-filename)
     "CREATE TABLE \"chunk\" (\
	\"id\"	INTEGER,\
	\"filepath\"	TEXT NOT NULL,\
	\"sha1hash\"	TEXT NOT NULL,\
	\"start_line\"	INTEGER NOT NULL,\
	\"end_line\"	INTEGER,\
	\"comment\"	TEXT,
	PRIMARY KEY(\"id\" AUTOINCREMENT));")
    (sqlite-execute
     (sqlite-open tng-db-filename)
     "CREATE TABLE \"dep\" (
	\"id\"	INTEGER,
	\"src\"	INTEGER NOT NULL,
	\"dst\"	INTEGER NOT NULL,
	\"comment\"	TEXT,
	FOREIGN KEY(\"src\") REFERENCES \"chunk\"(\"id\"),
	PRIMARY KEY(\"id\" AUTOINCREMENT),
	FOREIGN KEY(\"dst\") REFERENCES \"chunk\"(\"id\"));")))


;;; MAGIT

(defclass tng-section (magit-section)
  ((id :initform nil)
   (start :initform nil)
   (end :initform nil)
   (comment :initform nil)
   (keymap :initform 'tng-info-map))
  "A `magit-section' used by `tng-mode'")

(define-derived-mode tng-info-mode magit-section-mode "Tng-info"
  "TNG Buf Major mode"
  :group 'tng)

(defun tng-message-yo ()
  (interactive)
  (message "=======YO=========="))

(defvar tng-info-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "C-m") 'tng-message-yo)
    ;; (define-key map [remap revert-buffer] 'org-roam-buffer-refresh)
    map)
  "Parent keymap")

(defun tng-display-sections ()
  ""
  (interactive)
  (with-current-buffer
      (get-buffer-create "*TNG*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (tng-info-mode)
      (magit-insert-section resources-section (tng-section "Pew pew" nil)
        (magit-insert-heading "Resources: ")
        (insert ?\n)))
  (display-buffer (get-buffer-create "*TNG*"))))

;;; --------------------------------------------------------------------


(defun tng-current-chunks ()
  "Return alist of all chunks in current file."
  (let* ((filepath
          (file-relative-name
           (buffer-file-name)
           (projectile-project-root)))
         (db (sqlite-open tng-db-filename))
         (records (sqlite-select
                  db
                  "select id,filepath,start_line,end_line,comment,sha1hash from chunk where filepath=?"
                  (list filepath)
                  'full))
         (header (car records))
         (chunks (cdr records)))
    (mapcar
     (lambda (chunk) (cl-pairlis header chunk))
     chunks)))


(defun tng-setup-meta ()
  "Setup the meta-info for each line of the current file."
  (let ((chunks (tng-current-chunks)))
    (save-excursion
      (progn
        (dolist ((chunk chunks))
         (let-alist chunk
           (goto-line .line)
           (let* ((ov (make-overlay (point) (point)))
                  (str (tng-get-margin-str chunk)))
             (puthash
              .line
              (gethash .line tng-overlays ov)
              tng-overlays)
             (overlay-put ov 'before-string
                          (propertize " " 'display
                                      `((margin right-margin) ,str))))))))))

(defun tng-get-margin-str (line-meta-alist)
  "Get tng margin str for line."
  (let-alist line-meta-alist
    (let* ((left-bracket (propertize "[" 'face 'bold))
           (marker (propertize (if (zerop .status) " " "*") 'face 'bold))
           (right-bracket (propertize "]" 'face 'bold))
           (face (if (or (> .line 100) (< .line 10)) 'bold 'shadow)))
      (format "%s%s%s]" left-bracket marker right-bracket))))

(defun tng-visible-lines ()
  "Return start and end")

(defun tng-goto-next-dst ())
(defun tng-goto-next-src ())
(defun tng-show-pop-up ())
(defun tng-save-current-lines ())
(defun tng-add-src-to-res-under-point ())
(defun tng-add-dst-to-res-under-point ())

(defun tng-add-region (arg begin end)
  "Add new resource from the region"
  (interactive "P\nr")
  (if (region-active-p)
      (let* ((begin-line (line-number-at-pos begin t))
             (end-line (line-number-at-pos end t))
             (sha1-hash (sha1 (buffer-substring-no-properties begin end)))
             (filepath (file-relative-name (buffer-file-name) (projectile-project-root)))
             (comment (if arg (read-from-minibuffer "Comment for this chunk: "))))
        (sqlite-execute
         (sqlite-open tng-db-filename)
         "INSERT \
INTO chunk(filepath,sha1hash,comment,start_line,end_line) \
VALUES (?,?,?,?,?)"
         (list filepath sha1-hash comment begin-line end-line))
        (deactivate-mark)
        (message "lines: %s %s %s [arg: %s]" begin-line end-line sha1-hash arg))
    (error "No region selected")))

(defvar tng-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'tng-display-sections)
    (define-key map (kbd "i") #'tng-mode)
    (define-key map (kbd "a") #'tng-add-region)
    map))

(global-set-key (kbd "M-t") tng-keymap)

(defgroup tng nil
  "Track deps."
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
