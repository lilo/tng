;;; package --- tng.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;;
;;; TODO:
;;; don't include last line in the region if empty
;;; fix highlight after jump to chunk
;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'projectile)

(defvar-local
    tng-overlays (make-hash-table) ; TODO: vector
  "Overlays used in this buffer.")

(defvar-local tng-project-dir (projectile-project-root)
  "Root of current project.")

(defvar-local tng-db-filename
    (file-name-concat tng-project-dir "tango.sqlite3")
  "Path to the tng database.")

(defvar-local tng--last-added-chunk-id nil
  "Last added chunk id.")

(defvar-local tng--effective-chunk-id nil
  "Current effective chunk id.")

(defun tng--current-filepath ()
  "Return relative path for file in current buffer"
  (file-relative-name
           (buffer-file-name)
           (projectile-project-root)))

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


(define-derived-mode tng-info-mode magit-section-mode "Tng-info"
  "TNG Buf Major mode"
  :group 'tng)

(defvar tng--post-jump-region-functions nil
  "Function to call after jumping to chunk.
Takes START and END as arguments.")

(defun tng-highlight-region (begin-line end-line)
  (save-excursion
    (let* ((start (prog1 (point) (goto-line begin-line)))
           (end (prog1 (point) (goto-line end-line))))
      (pulsar--pulse :no-pulse 'pulsar-face start end)))
  "Pulse region from BEGIN-LINE to END-LINE.")

(add-to-list 'tng--post-jump-region-functions #'tng-highlight-region)

(defun tng-jump-to-chunk ()
  "Jump to the chunk"
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
    ;; (define-key map (kbd "C-m") #'pulsar-pulse-line)
    (define-key map (kbd "C-m") #'tng-jump-to-chunk)
    ;; (define-key map [remap revert-buffer] 'org-roam-buffer-refresh)
    map)
  "Parent keymap")

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
                     (alist-get 'filepath chunk nil nil #'string-equal))
                    (start-line
                     (alist-get 'start_line chunk nil nil #'string-equal))
                    (end-line
                     (alist-get 'end_line chunk nil nil #'string-equal))
                    (comment (alist-get 'comment chunk nil nil #'string-equal)))
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

(defun tng-lines-report (&optional start end)
  "Return list of meta-info alists for lines from START to END."
  (let* ((filepath (tng--current-filepath))
         (db (sqlite-open tng-db-filename))
         (records (sqlite-select
                   db
                   "
select
 c.id cid,
 count(sd.id) sc,
 count(dd.id) sd,
 c.filepath cf,
 c.start_line csl,
 c.end_line cel,
 c.sha1hash csha1,
 c.comment cc
from
 chunk c
 left join dep sd on (sd.src = cid)
 left join dep dd on (dd.dst = cid)
group by cid
order by cid
"
                   ;;(list start end filepath)
                   nil
                   'full))
         (header (mapcar #'make-symbol (car records)))
         (records (cdr records)))
    (mapcar
     (lambda (record) (cl-pairlis header record))
     records)))

(defun tng-setup-meta ()
  "Setup the meta-info for each line of the current file."
  (let* ((current-lines (tng--visible-lines))
         (line-start (car current-lines))
         (line-end (cdr current-lines))
         (current-metainfo (tng-lines-report)))
    (save-excursion
      (cl-loop
       for line from line-start to line-end
       for chunks = (seq-keep
                      (lambda (a)
                        (and
                         (> line (alist-get 'start_line a nil nil #'equal))
                         (> (alist-get 'end_line a nil nil #'equal) line)
                         a))
                      (tng-current-chunks))
       do
       (message "[%s]" chunks)))))

;; (let-alist chunk
;;            (goto-line .line)
;;            (let* ((ov (make-overlay (point) (point)))
;;                   (str (tng-get-margin-str chunk)))
;;              (puthash
;;               .line
;;               (gethash .line tng-overlays ov)
;;               tng-overlays)
;;              (overlay-put ov 'before-string
;;                           (propertize " " 'display
;;                                       `((margin right-margin) ,str)))))


(defun tng-get-margin-str (line-meta-alist)
  "Get tng margin str for line."
  (let-alist line-meta-alist
    (let* ((left-bracket (propertize "[" 'face 'bold))
           (marker (propertize (if (zerop .status) " " "*") 'face 'bold))
           (right-bracket (propertize "]" 'face 'bold))
           (face (if (or (> .line 100) (< .line 10)) 'bold 'shadow)))
      (format "%s%s%s]" left-bracket marker right-bracket))))

(defun tng--visible-lines ()
  "Return start and end lines of the current buffer's contents
in current visible window."
  (save-excursion
    (cons
     (line-number-at-pos (goto-char (window-start)))
     (line-number-at-pos (goto-char (1- (window-end)))))))

(defun tng-goto-next-dst ())
(defun tng-goto-next-src ())
(defun tng-show-pop-up ())
(defun tng-save-current-lines ())
(defun tng-add-src-to-res-under-point ())
(defun tng-add-dst-to-res-under-point ())

(defvar tng--post-add-region-functions nil
  "Function to call after new chunk added.
Takes START and END as arguments.")

(add-to-list 'tng--post-add-region-functions #'tng-highlight-region)

(defun tng-add-region (arg begin end)
  "Add new resource from the region.
If not a region, use current string."
  (interactive "P\nr")
  (let* ((region (region-active-p))
         (begin-line (line-number-at-pos (if region begin) t))
         (end-line (line-number-at-pos (if region end) t))
         (sha1-hash
          (if region
              (sha1 (buffer-substring-no-properties begin end))
            (sha1 (thing-at-point 'line t))))
         (filepath
          (file-relative-name
           (buffer-file-name)
           (projectile-project-root)))
         (comment (if arg (read-from-minibuffer "Comment for this chunk: ")))
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
    (dolist (fn tng--post-add-region-functions)
      (funcall fn begin-line end-line)))
  (deactivate-mark))

(defvar tng-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'tng-display-sections)
    (define-key map (kbd "t") #'tng-mode)
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
