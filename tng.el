;;; package --- tng.el -*- lexical-binding:t; coding:utf-8 -*-
;;; Commentary:
;;; Package-Requires: ((dash "2.19.1"))
;;; TODO:
;;; rename start/begin
;;; rename start_line, end_line
;;; replace chunk (keeping deps)
;;; emacsql
;;; naming
;;; minor-mode-map-alist
;;; Code:


(require 'cl-lib)
(require 'dash)

(defvar-local tng--overlays-hash-table (make-hash-table)
  "The hash-table.")

(defvar-local tng-project-dir (project-root (project-current))
  "Root of current project.")

(defvar-local tng-db-filename
    (file-name-concat tng-project-dir "tango.sqlite3")
  "Path to the tng database.")

(defgroup tng nil
  "Track deps."
  :group 'tng)

(defcustom tng-auto-fix-moved-chunks t
  "Auto-fix and auto-update moved chunks."
  :group 'tng
  :type 'boolean)

(defcustom tng-use-fringes t
  "Use fringes indicators."
  :group 'tng
  :type 'boolean)

(defvar-local tng--last-added-chunk-id nil
  "Last added chunk id.")

(defvar tng--global-last-added-chunk-id nil
  "Global last added chunk id.")

(defvar-local tng--effective-chunk-id nil
  "Current effective chunk id.")

(defun tng--current-filepath ()
  "Return relative path for file in current buffer."
  (file-relative-name
           (buffer-file-name)
           tng-project-dir))

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


(defvar tng--post-jump-region-functions nil
  "Functions to call after jumping to chunk.
Take START and END as arguments.")

(defun tng--line-rectangle (begin-line end-line)
  "Return bol of BEGIN-LINE and eol of END-LINE for current buffer."
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


(defun tng-current-chunks ()
  "Return alists of all chunks in current file."
  (let* ((filepath (tng--current-filepath))
         (db (sqlite-open tng-db-filename))
         (records (sqlite-select
                  db
                  "select id,filepath,start_line,end_line,comment,sha1hash from chunk where filepath=?"
                  (list filepath)
                  'full))
         (header (mapcar #'intern (car records)))
         (chunks (cdr records)))
    (mapcar
     (lambda (chunk) (cl-pairlis header chunk))
     chunks)))

(defun tng-project-chunks ()
  "Return alists of all chunks (in current project)."
  (let* ((db (sqlite-open tng-db-filename))
         (records (sqlite-select
                  db
                  "select id,filepath,start_line,end_line,comment,sha1hash from chunk"
                  nil
                  'full))
         (header (mapcar #'intern (car records)))
         (chunks (cdr records)))
    (mapcar
     (lambda (chunk) (cl-pairlis header chunk))
     chunks)))

(defun tng-make-overlay (chunk)
  (let-alist chunk
    (let* ((reg (tng--line-rectangle .start_line .end_line))
           (begin (car reg))
           (end (cdr reg))
           (sha1hashfact
            (sha1 (buffer-substring-no-properties begin end)))
           (diff-flag (not (string-equal sha1hashfact .sha1hash)))
           (ov-in-overlays-table
            (gethash
             .id tng--overlays-hash-table))
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
             (string-equal ov-markers-sha1 .sha1hash)))
           (ov-face
            (cond (moved-flag 'diff-refine-changed)
                  (diff-flag 'diff-refine-removed)
                  (t 'highlight)))
           (left-fringe
            (cond (moved-flag '(left-fringe up-arrow shr-mark))
                  (diff-flag '(left-fringe question-mark dired-flagged))
                  (t '(left-fringe large-circle shadow)))))
      (prog1 ov
        (overlay-put ov 'tng-chunk-id .id)
        (overlay-put ov 'tng-chunk chunk)
        (overlay-put ov 'tng-moved-flag moved-flag)
        (overlay-put ov 'tng-begin-line (line-number-at-pos ov-begin-marker))
        (overlay-put ov 'tng-end-line (1- (line-number-at-pos ov-end-marker))) ;; TODO: line num
        (overlay-put ov 'tng-begin-marker ov-begin-marker)
        (overlay-put ov 'tng-end-marker ov-end-marker)
        (overlay-put ov 'face ov-face)
        (when tng-use-fringes
          (overlay-put ov 'before-string
                       (propertize
                        " " 'display
                        left-fringe)))))))


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

;; (add-to-list 'tng--post-add-region-functions #'tng-pulse-region)

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
           tng-project-dir))
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

(defun tng--update-chunk-hash (chunk-id sha1hash)
  "Update BEGIN-LINE and END-LINE for chunk where id = CHUNK-ID"
  (let ((ov (gethash chunk-id tng--overlays-hash-table)))
    (sqlite-select
     (sqlite-open tng-db-filename)
     "
UPDATE chunk
SET sha1hash = ?
WHERE id = ?
RETURNING
 id"
     (list sha1hash chunk-id)
     nil)))


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


(defun tng-list-chunks-refresh ()
  "Refresh list of chunks."
  (let* ((chunks (tng-project-chunks)))
    (cl-flet
        ((tabulate-chunk (chunk)
           (let-alist chunk
             (list
              chunk
              (vector
               (list
                (number-to-string .id)
                'face 'default
                'action (lambda (_button) (tng-list-chunks-jump)))
               (list .filepath) (list (format "%s:%s" .start_line .end_line)) (list .comment))))))
      (setq tabulated-list-entries (mapcar #'tabulate-chunk chunks)))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun tng-list-chunks-jump ()
  "Jump to the chunk."
  (interactive)
  (let-alist (tabulated-list-get-id)
    (let* ((buf (find-file-noselect .filepath))
           ;; #'pop-to-buffer-same-window)))
           (display-buffer-fn #'switch-to-buffer-other-window))
      (funcall display-buffer-fn buf)
      (with-current-buffer buf
        (widen)
        (goto-line .start_line)
        (dolist (fn tng--post-jump-region-functions)
          (funcall fn .start_line .end-line))))))

;; (defvar-keymap tng-list-chunks-mode-map
;;   :doc "Keymap for `tng-list-chunks-mode'."
;;   :parent tabulated-list-mode-map
;;   "C-m" #'tng-list-chunks-jump)

(defvar tng-list-chunks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tng-list-chunks-jump)
    (define-key map (kbd "C-m") #'tng-list-chunks-jump)
    map))

(define-derived-mode tng-list-chunks-mode tabulated-list-mode "tng-list-chunks-mode"
  "Major mode for listing chunks.
\\<tng-list-chunks-mode-map>
\\{tng-list-chunks-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("id" 4 t)
			       ("filename" 12 t)
                               ("pos" 8 t)
			       ("comment"  25 t)])
  (setq tabulated-list-sort-key (cons "filename" nil))
  (add-hook 'tabulated-list-revert-hook #'tng-list-chunks-refresh nil t))

(put 'tng-list-chunks-mode 'mode-class 'special)

(defun tng-list-chunks ()
  "Display a list of existing chunks."
  (interactive)
  (let ((buf (get-buffer-create "* TNG chunks*")))
    (switch-to-buffer buf)
    (tng-list-chunks-mode)
    (tng-list-chunks-refresh)))

;; list chunks: project, file
;; status: Src, Dst, Both
;; status: Modified, Unfixed, Fixed


(defun tng-link-chunks-list-refresh ()
  "Refresh list of chunks."
  (let* ((chunks (tng-project-chunks)))
    (cl-flet
        ((tabulate-chunk (chunk)
           (let-alist chunk
             (list
              chunk
              (vector
               (list
                (number-to-string .id)
                'face 'default
                'action (lambda (_button) (tng-list-chunks-jump)))
               (list .filepath) (list (format "%s:%s" .start_line .end_line)) (list .comment)))))))
    (setq tabulated-list-entries (mapcar #'tabulate-chunk chunks))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun tng-link-chunks-ret ()
  "Handle RET."
  (interactive)
  (let* ((chunk (tabulated-list-get-id))
         (filepath (alist-strget 'filepath chunk))
         (start-line (alist-strget 'start_line chunk))
         (end-line (alist-strget 'end_line chunk))
         (buf (find-file-noselect filepath))
         ;; #'pop-to-buffer-same-window)))
         (display-buffer-fn #'switch-to-buffer-other-window))
    (funcall display-buffer-fn buf)
    (with-current-buffer buf
      (widen)
      (goto-line start-line)
      (dolist (fn tng--post-jump-region-functions)
        (funcall fn start-line end-line)))))

;; (defvar-keymap tng-list-chunks-mode-map
;;   :doc "Keymap for `tng-list-chunks-mode'."
;;   :parent tabulated-list-mode-map
;;   "C-m" #'tng-list-chunks-jump)

(defvar tng-list-chunks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tng-list-chunks-jump)
    (define-key map (kbd "C-m") #'tng-list-chunks-jump)
    map))

(define-derived-mode tng-list-chunks-mode tabulated-list-mode "tng-list-chunks-mode"
  "Major mode for listing chunks.
\\<tng-list-chunks-mode-map>
\\{tng-list-chunks-mode-map}"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("id" 4 t)
			       ("filename" 12 t)
                               ("pos" 8 t)
			       ("comment"  25 t)])
  (setq tabulated-list-sort-key (cons "filename" nil))
  (add-hook 'tabulated-list-revert-hook #'tng-list-chunks-refresh nil t))

(put 'tng-list-chunks-mode 'mode-class 'special)

(defun tng-list-chunks ()
  "Display a list of existing chunks."
  (interactive)
  (let ((buf (get-buffer-create "* TNG chunks*")))
    (switch-to-buffer buf)
    (tng-list-chunks-mode)
    (tng-list-chunks-refresh)))

(defun tng-link-select-chunk (chunk)
  "Display a list of chunks for linking the CHUNK.
DIRECTION is 'src or 'dst."
  (interactive)
  (let* ((current-chunk ())
         (buf
          (get-buffer-create
           (format "*TNG connect %s*" (tng--current-filepath)))))
    (switch-to-buffer buf)
    (tng-link-)
    (tng-list-chunks-refresh)))


;; https://www.howardism.org/Technical/Emacs/alt-completing-read.html
(defun alt-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Calls `completing-read' but returns the value from COLLECTION.

Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice. For instance,
give a variable of choices like:

    (defvar favorite-hosts '((\"Glamdring\" . \"192.168.5.12\")
                             (\"Orcrist\"   . \"192.168.5.10\")
                             (\"Sting\"     . \"192.168.5.220\")
                             (\"Gungnir\"   . \"192.168.5.25\")))

We can use this function to `interactive' without needing to call
`alist-get' afterwards:

    (defun favorite-ssh (hostname)
      \"Start a SSH session to a given HOSTNAME.\"
      (interactive (list (alt-completing-read \"Host: \" favorite-hosts)))
      (message \"Rockin' and rollin' to %s\" hostname))"

  ;; Yes, Emacs really should have an `alistp' predicate to make this code more readable:
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))

    (let* ((choice
            (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t                         choice))))
      (if (listp results) (first results) results))))


(defun tng-chunks-at-point ()
  "Return list of tng chunks that include current point."
  (let* ((overlays (overlays-at (point))))
    (-filter
     (lambda (o) (plist-member (overlay-properties o) 'tng-chunk-id))
     overlays)))

(defun tng-chunk-shift-down ()
  "Shift current chunk downwards."
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines chunk-id (1+ begin-line) (1+ end-line))
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-chunk-shift-up ()
  "Shift current chunk upwnwards."
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines chunk-id (1- begin-line) (1- end-line))
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-chunk-expand-upper-up ()
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines chunk-id (1- begin-line) end-line)
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-chunk-expand-upper-down ()
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines chunk-id (1+ begin-line) end-line)
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-chunk-expand-down-up ()
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines chunk-id begin-line (1- end-line))
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-chunk-expand-down-down ()
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines chunk-id begin-line (1+ end-line))
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-get-completion-alist (tng-overlays)
  "Return alist '((file:beg:end:comment . chunk-id) ...)"
  (setq completions nil) ; TODO:
  (dolist (chunk-ov tng-overlays)
    (let*
        ((chunk-alist (plist-get (overlay-properties chunk-ov) 'tng-chunk))
         (filename (alist-get 'filepath chunk-alist))
         (comment (alist-get 'comment chunk-alist))
         (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
         (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line))
         (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
         (chunk-fmt
          (format "%s:%d:%d:%s" filename begin-line end-line comment)))
      (push `(,chunk-fmt . ,chunk-id) completions)))
  completions)

(defun tng-chunk-resize-down-down (chunk-id)
  (interactive
   (list
    (when-let* ((chunks (tng-chunks-at-point))
                (first-chunk (car chunks)))
      (if (length= chunks 1)
          (plist-get (overlay-properties first-chunk) 'tng-chunk-id)
        (alt-completing-read
         "Select: "
         (tng-get-completion-alist chunks))))))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin 0 :end +1)
    (tng-delete-overlays)
    (tng-create-overlays)))

(cl-defun tng-chunk-resize (chunk-id &key begin end)
  (when-let* ((chunk-ov (gethash chunk-id tng--overlays-hash-table))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    (tng--update-chunk-begin-end-lines
     chunk-id
     (+ begin-line begin) (+ end-line end))
    (tng-delete-overlays)
    (tng-create-overlays)))

(defun tng-chunk-rehash ()
  (interactive)
  (when-let* ((chunks (tng-chunks-at-point))
              (chunk-ov (when (= 1 (length chunks)) (car chunks)))
              (chunk-id (plist-get (overlay-properties chunk-ov) 'tng-chunk-id))
              (begin-line (plist-get (overlay-properties chunk-ov) 'tng-begin-line))
              (end-line (plist-get (overlay-properties chunk-ov) 'tng-end-line)))
    ;; TODO:
    (tng-delete-overlays)
    (tng-create-overlays)))


(defvar tng-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'tng-mode)
    (define-key map (kbd "a") #'tng-add-region)
    (define-key map (kbd "r") #'tng--read-chunks)
    (define-key map (kbd "l") #'tng-list-chunks)
    (define-key map (kbd "]") #'tng-chunk-shift-down)
    (define-key map (kbd "[") #'tng-chunk-shift-up)
    (define-key map (kbd "n") #'tng-chunk-expand-upper-down)
    (define-key map (kbd "p") #'tng-chunk-expand-upper-up)
    (define-key map (kbd "N") #'tng-chunk-expand-down-down)
    (define-key map (kbd "P") #'tng-chunk-expand-down-up)
    map))

(global-set-key (kbd "M-t") tng-keymap)

(provide 'tng)
;;; tng.el ends here
