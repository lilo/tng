;;; package --- tng.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;; TODO:
;;; rename start/begin
;;; rename start_line, end_line
;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'projectile)

(defvar-local tng--overlays-hash-table (make-hash-table)
  "The hash-table.")

(defvar-local tng-project-dir (projectile-project-root)
  "Root of current project.")

(defvar-local tng-db-filename
    (file-name-concat tng-project-dir "tango.sqlite3")
  "Path to the tng database.")

(defgroup tng nil
  "Track deps."
  :group 'convenience)

(defcustom tng-auto-fix-moved-chunks t
  "Auto-fix and auto-update moved chunks."
  :group 'tng
  :type 'boolean)

(defvar-local tng--last-added-chunk-id nil
  "Last added chunk id.")

(defvar tng--global-last-added-chunk-id nil
  "Global last added chunk id.")

(defvar-local tng--effective-chunk-id nil
  "Current effective chunk id.")

(defun alist-strget (key alist)
  "Shortcut for `string-equal' `alist-get'.
Argument KEY the key.
Argument ALIST the alist."
  (alist-get key alist nil nil #'string-equal))

(defun tng--current-filepath ()
  "Return relative path for file in current buffer."
  (file-relative-name
           (buffer-file-name)
           (projectile-project-root)))

(defun tng-init-db (&optional dir)
  "Create tng DB in the project root DIR."
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

(define-derived-mode tng-info-mode magit-section-mode "Tng-info"
  "TNG Buf Major mode."
  :group 'tng)

(defvar tng--post-jump-region-functions nil
  "Function to call after jumping to chunk.
Takes START and END as arguments.")

(defun tng--line-rectangle (begin-line end-line)
  "Return bol of BEGIN-LINE and eol of END-LINE."
  (save-excursion
    (let* ((start
            (progn
              (goto-char (point-min))
              (beginning-of-line begin-line)
              (point)))
           (end
            (1+
             (progn
               (goto-char (point-min))
               (end-of-line end-line) (point)))))
      (cons start end))))

(defun tng-pulse-region (begin-line end-line)
  "Pulse the chunk.
Argument BEGIN-LINE from that line.
Argument END-LINE to that."
  (save-excursion
    (let* ((rectangle (tng--line-rectangle begin-line end-line))
           (start (car rectangle))
           (end (cdr rectangle)))
      (pulsar--pulse nil 'pulsar-face start end)))
  "Pulse region from BEGIN-LINE to END-LINE.")

(add-to-list 'tng--post-jump-region-functions #'tng-pulse-region)

(defun tng-jump-to-chunk ()
  "Jump to the chunk."
  (interactive)
  (let* ((current-section (magit-current-section))
         (filepath (oref current-section filepath))
         (start-line (oref current-section start-line))
         (end-line (oref current-section end-line))
         (buf (find-file-noselect filepath))
         ;; #'pop-to-buffer-same-window)))
         (display-buffer-fn #'switch-to-buffer-other-window))
    (funcall display-buffer-fn buf)
    (with-current-buffer buf
      (widen)
      (goto-line start-line)
      (dolist (fn tng--post-jump-region-functions)
        (funcall fn start-line end-line)))))

(defvar tng-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "C-m") #'tng-jump-to-chunk)
    map)
  "Parent keymap.")

(defclass tng-section (magit-section)
  ((id :initform nil)
   (start-line :initform nil)
   (end-line :initform nil)
   (filepath :initform nil)
   (comment :initform nil)
   (keymap :initform 'tng-section-map)))

(defun tng-display-sections ()
  "Display the list of chunks."
  (interactive)
  (let ((current-chunks (tng-current-chunks)))
    (with-current-buffer
        (get-buffer-create "*TNG*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (tng-info-mode)
        (magit-insert-section (tng-section)
          (magit-insert-heading "Chunks: ")
          (dolist (chunk current-chunks)
            (magit-insert-section section (tng-section)
              (let ((filepath
                     (alist-strget 'filepath chunk))
                    (start-line
                     (alist-strget 'start_line chunk))
                    (end-line
                     (alist-strget 'end_line chunk))
                    (comment (alist-strget 'comment chunk)))
                (insert
                 (format "%s[%d:%d] -- %s\n"
                         filepath start-line end-line comment))
                (oset section filepath filepath)
                (oset section start-line start-line)
                (oset section end-line end-line))))))))
    (display-buffer (get-buffer-create "*TNG*")))

;;; --------------------------------------------------------------------

(defun tng-current-chunks ()
  "Return alists of all chunks in current file."
  (let* ((filepath (tng--current-filepath))
         (db (sqlite-open tng-db-filename))
         (records (sqlite-select
                  db
                  "select id,filepath,start_line,end_line,comment,sha1hash from chunk where filepath=?"
                  (list filepath)
                  'full))
         (header (mapcar #'make-symbol (car records)))
         (chunks (cdr records)))
    (mapcar
     (lambda (chunk) (cl-pairlis header chunk))
     chunks)))

(defun tng--chunk-region (chunk)
  "Return begin pos and end pos of the CHUNK."
  (let* (begin end
         (start-line (alist-strget 'start_line chunk))
         (end-line (alist-strget 'end_line chunk)))
    (tng--line-rectangle start-line end-line)))

(defun tng-make-overlay (chunk)
  "Create/update overlay from CHUNK alist."
  (let* ((chunk-id (alist-strget 'id chunk))
         (begin-line (alist-strget 'start_line chunk))
         (end-line (alist-strget 'end_line chunk))
         (sha1hash (alist-strget 'sha1hash chunk))
         (reg (tng--chunk-region chunk))
         (begin (car reg))
         (end (cdr reg))
         (sha1hashfact
          (sha1 (buffer-substring-no-properties begin end)))
         (diff-flag (not (string-equal sha1hashfact sha1hash)))
         (ov-in-overlays-table
          (gethash
           chunk-id tng--overlays-hash-table))
         (ov
          (or
           ov-in-overlays-table
           (make-overlay begin end)))
         (ov-begin-marker
          (or
           (overlay-get ov 'tng-begin-marker)
           (prog1
               (setq marker (make-marker))
             (set-marker marker begin))))
         (ov-end-marker
          (or
           (overlay-get ov 'tng-end-marker)
           (prog1 (setq marker (make-marker)) (set-marker marker end))))
         (ov-markers-sha1
          (sha1
           (buffer-substring-no-properties
            ov-begin-marker ov-end-marker)))
         (moved-flag
          (and
           diff-flag
           (string-equal ov-markers-sha1 sha1hash)))
         (ov-face
          (cond (moved-flag 'diff-refine-changed)
                (diff-flag 'diff-refine-removed)
                (t 'highlight)))
         (left-fringe
          (cond (moved-flag '(left-fringe up-arrow shr-mark))
                (diff-flag '(left-fringe question-mark dired-flagged))
                (t '(left-fringe large-circle shadow)))))
    (prog1 ov
      (overlay-put ov 'tng-chunk-id chunk-id)
      (overlay-put ov 'tng-moved-flag moved-flag)
      (overlay-put ov 'tng-begin-line (line-number-at-pos ov-begin-marker))
      (overlay-put ov 'tng-end-line (1- (line-number-at-pos ov-end-marker))) ;; TODO: line num
      (overlay-put ov 'tng-begin-marker ov-begin-marker)
      (overlay-put ov 'tng-end-marker ov-end-marker)
      (overlay-put ov 'face ov-face)
      (overlay-put ov 'before-string
                   (propertize
                    " " 'display
                    left-fringe)))))

(defun tng-auto-fix-moved ()
  "Update moved chunks' start-line and end-line."
  (dolist (k (hash-table-keys tng--overlays-hash-table))
    (let ((ov (gethash k tng--overlays-hash-table)))
      (when (overlay-get ov 'tng-moved-flag)
        (let ((chunk-id (overlay-get ov 'tng-chunk-id))
              (begin-line (overlay-get ov 'tng-begin-line))
              (end-line (overlay-get ov 'tng-end-line)))              
          (tng--update-chunk-begin-end-lines chunk-id begin-line end-line)
          (overlay-put ov 'tng-moved-flag nil)
          (overlay-put ov 'tng-begin-marker nil)
          (overlay-put ov 'tng-end-marker nil))))))

(defun tng-create-overlays ()
  "Create overlays for chunks in current buffer."
  (interactive)
  (dolist (chunk (tng-current-chunks))
    (let* ((ov (tng-make-overlay chunk))
          (chunk-id (overlay-get ov 'tng-chunk-id)))
      (puthash chunk-id ov tng--overlays-hash-table))))

(defvar tng--post-add-region-functions nil
  "Function to call after new chunk added.
Takes START and END as arguments.")

(add-to-list 'tng--post-add-region-functions #'tng-pulse-region)


(defun tng-add-region (arg begin end)
  "Add new resource from the region.
If not a region, use current string.
Argument ARG arg.
Argument BEGIN from here.
Argument END to here."
  (interactive "P\nr")
  (let* ((region (region-active-p))
         (begin-line (line-number-at-pos (if region begin) t))
         (begin-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- begin-line))
                      (beginning-of-line)
                      (point)))
         (end-line (line-number-at-pos
                    (if region
                        (save-excursion
                          (progn
                            (goto-char end)
                            (when (bolp)
                              (forward-line -1))
                            (end-of-line)
                            (point)))) t))
         (end-pos (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- end-line))
                      (end-of-line)
                      (+ 1 (point))))
         (rectangle (tng--line-rectangle begin-line end-line))
         (rectangle-string (if region
              (buffer-substring-no-properties begin-pos end-pos)
            (thing-at-point 'line t)))
         (sha1-hash (sha1 rectangle-string))
         (filepath
          (file-relative-name
           (buffer-file-name)
           (projectile-project-root)))
         (comment
          (if (not arg)
              (read-from-minibuffer "Comment for this chunk: ")))
         (last-added-chunk
          (sqlite-select
           (sqlite-open tng-db-filename)
           "
INSERT INTO
 chunk(filepath,sha1hash,comment,start_line,end_line)
VALUES
 (?,?,?,?,?)
RETURNING
 id"
           (list filepath sha1-hash comment begin-line end-line)
           'full)))
    (setq-local tng--last-added-chunk-id (caadr last-added-chunk))
    (setq tng--global-last-added-chunk-id (caadr last-added-chunk))
    (dolist (fn tng--post-add-region-functions)
      (funcall fn begin-line end-line)))
  (deactivate-mark))

(defun tng--update-chunk-begin-end-lines (chunk-id begin-line end-line)
  "Update BEGIN-LINE and END-LINE for chunk where id = CHUNK-ID"
  (let ((ov (gethash chunk-id tng--overlays-hash-table)))
    (sqlite-select
     (sqlite-open tng-db-filename)
     "
UPDATE chunk
SET start_line = ?,
    end_line = ?
WHERE id = ?
RETURNING
 id"
     (list begin-line end-line chunk-id)
     nil)))

(defvar tng-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'tng-display-sections)
    (define-key map (kbd "t") #'tng-mode)
    (define-key map (kbd "a") #'tng-add-region)
    (define-key map (kbd "r") #'tng--read-chunks)
    map))

(global-set-key (kbd "M-t") tng-keymap)

(define-minor-mode tng-mode
  "Tango mode."
  :lighter " T"
  (if tng-mode
      (progn
        (add-hook 'change-major-mode-hook 'tng-delete-overlays nil t)
        (add-hook 'after-change-functions 'tng-after-change t t)
        (tng-create-overlays))
    (remove-hook 'after-change-functions 'tng-after-change t)
    (remove-hook 'change-major-mode-hook 'tng-delete-overlays t)
    (tng-delete-overlays)))

(defun tng-after-change (beg end _len)
  "Update overlays on deletions,
and after newlines are inserted BEG END _LEN."
  (tng-create-overlays)
  (when tng-auto-fix-moved-chunks
    (tng-auto-fix-moved))
    (tng-create-overlays))

(defun tng-delete-overlays ()
  "Delete tng overlays."
  (maphash
   (lambda (k v) (delete-overlay v))
   tng--overlays-hash-table)
  (clrhash tng--overlays-hash-table))

;;;;;;;;;;;;;

(defun tng-list-chunks-refresh ()
  "Refresh list of chunks."
  (let ((entry (list
                'chunk1
                [("1") ("tng.el") ("9:12") ("Requirements")])))
    (setq tabulated-list-entries (list entry))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(define-derived-mode tng-list-chunks-mode tabulated-list-mode "tng-list-chunk-mode"
  "Major mode for listing chunks"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("id" 4 t)
			       ("filename" 12 t)
                               ("pos" 8 t)
			       ("comment"  25 t)])
  (setq tabulated-list-sort-key (cons "filename" nil))
  (add-hook 'tabulated-list-revert-hook #'tng-list-chunks-refresh nil t))

(put 'bookmark-bmenu-mode 'mode-class 'special)

(defun tng-list-chunks ()
  "Display a list of existing chunks."
  (interactive)
  (let ((buf (get-buffer-create "* TNG chunks*")))
    (switch-to-buffer buf)
    (tng-list-chunks-mode)
    (tng-list-chunks-refresh)))

(provide 'tng)
;;; tng.el ends here
