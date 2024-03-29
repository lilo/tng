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

(defvar-local tng-overlays nil
  "Overlays used in this buffer.")

(defvar-local tng-project-dir (projectile-project-root)
  "Root of current project.")

(defvar-local tng-db-filename
    (file-name-concat tng-project-dir "tango.sqlite3")
  "Path to the tng database.")

(defvar-local tng--last-added-chunk-id nil
  "Last added chunk id.")

(defvar tng--global-last-added-chunk-id nil
  "Global last added chunk id.")

(defvar-local tng--effective-chunk-id nil
  "Current effective chunk id.")

(defun alist-strget (key alist)
  "Shortcut for `string-equal' alist-get."
  (alist-get key alist nil nil #'string-equal))

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
  "Pulse the chunk."
  (save-excursion
    (let* ((rectangle (tng--line-rectangle begin-line end-line))
           (start (car rectangle))
           (end (cdr rectangle)))
      (pulsar--pulse nil 'pulsar-face start end)))
  "Pulse region from BEGIN-LINE to END-LINE.")

(add-to-list 'tng--post-jump-region-functions #'tng-pulse-region)

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
  "Create overlay from CHUNK alist."
  (let* ((start-line (alist-strget 'start_line chunk))
         (end-line (alist-strget 'end_line chunk))
         (sha1hash (alist-strget 'sha1hash chunk))
         (reg (tng--chunk-region chunk))
         (begin (car reg))
         (end (cdr reg))
         (sha1hashfact
          (sha1 (buffer-substring-no-properties begin end)))
         (ov (make-overlay begin end))
         (ov-face
          (if
              (string-equal sha1hashfact sha1hash)
              'highlight
            'isearch-fail)))
    (overlay-put ov 'face ov-face)
    ;; (overlay-put ov 'before-string
    ;;              (propertize
    ;;               " " 'display
    ;;               `(left-fringe right-arrow warning)))
    ov))

(defun tng-create-overlays ()
  "Create overlays for chunks in current buffer."
  (interactive)
  (setq tng-overlays (mapcar #'tng-make-overlay (tng-current-chunks))))

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

(defun tng-link-set-dst ()
  ;; new-chunk-id = tng-add-region begin end
  ;; create dep (src: effective-id dst: new-chunk-id)
  ;; effective-id = new-chunk-id
  "Create dep. Set active chunk as dest.")

(defun tng-link-set-src ()
  "Create chunk. Set active chunk as source.")

(defun tng--read-chunks (ids)
  "Read chunks using completing-read."
  (interactive
   (list
    (alt-completing-read
     "Select chunk: "
     '(("Chunk1" . 1)
       ("Chunk2" . 2)
       ("Chunk3" . 3)))))
  (message "IDS: %s" ids))

(defvar tng--post-add-region-functions nil
  "Function to call after new chunk added.
Takes START and END as arguments.")

(add-to-list 'tng--post-add-region-functions #'tng-pulse-region)


(defun tng-add-region (arg begin end)
  "Add new resource from the region.
If not a region, use current string."
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

(defvar tng-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "v") #'tng-display-sections)
    (define-key map (kbd "t") #'tng-mode)
    (define-key map (kbd "a") #'tng-add-region)
    (define-key map (kbd "r") #'tng--read-chunks)
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
        (add-hook 'change-major-mode-hook 'tng-delete-overlays nil t)
        (add-hook 'after-change-functions 'tng-after-change t)
        (tng-update-current))
    (remove-hook 'after-change-functions 'tng-after-change t)
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
      (tng-delete-overlays)
      (tng-create-overlays)
      (mapc #'tng-update-window
            (get-buffer-window-list buffer nil 'visible)))))

(defun tng-delete-overlays ()
  "Delete tng overlays."
  (dolist (ov tng-overlays)
    (delete-overlay ov))
  (setq tng-overlays nil))

  ;; (dolist (w (get-buffer-window-list (current-buffer) nil t))
  ;;   (let ((set-margins (window-parameter w 'tng--set-margins))
  ;;         (current-margins (window-margins w)))
  ;;     (when (and set-margins
  ;;                (equal set-margins current-margins))
  ;;       (set-window-margins w (car current-margins) 0)
  ;;       (set-window-parameter w 'tng--set-margins nil)))))


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
  ;; (let ((width 3)
  ;;       (existing-margins (window-margins win)))
  ;;   (progn
  ;;     (when (display-graphic-p)
  ;;       (setq width (ceiling
  ;;                    (/ (* width 1.0 (tng--face-width 'default))
  ;;                       (frame-char-width)))))
  ;;     (when (> width (or (cdr existing-margins) 0))
  ;;       (set-window-margins win (car existing-margins) width)
  ;;       (set-window-parameter win 'tng--set-margins (window-margins win)))))
  )

(provide 'tng)
;;; tng.el ends here
