;;; package --- tng.el Track dependencies across your code -*- lexical-binding:t; coding:utf-8 -*-
;; Copyright (C) 2024 Sergey Lilo
;; Author: Sergey Lilo <s3rg31110@gmail.com>
;; URL: https://github.com/lilo/tng.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:



(require 'cl-lib)
(require 'dash)

(defvar-local tng--status nil
  "Current buffer status as alist.")

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
  (let* ((filepath (tng--current-filepath)))
    (tng--file-chunks filepath)))

(defun tng--file-chunks (filepath)
  "Return alists of chunks in FILEPATH (relative to the project's root)."
  (let* ((db (sqlite-open tng-db-filename))
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
                  (not :values)
                  'full))
         (header (mapcar #'intern (car records)))
         (chunks (cdr records)))
    (mapcar
     (lambda (chunk) (cl-pairlis header chunk))
     chunks)))


(defvar tng--post-add-region-functions nil
  "Function to call after new chunk added.
Takes START and END as arguments.")

(defun tng--refresh-status-after-chunk-add (&rest _)
  "Refresh"
  (tng--refresh-current-buffer-status))

(add-to-list 'tng--post-add-region-functions #'tng-pulse-region)
(add-to-list 'tng--post-add-region-functions #'tng--refresh-status-after-chunk-add)

(defun tng-add-region (arg begin end)
  "Add new resource from the region.
If not a region, use current string.
Argument ARG arg.
Argument BEGIN from here.
Argument END to here."
  (interactive "P\nr")
  (let* ((region (region-active-p))
         (begin-line (line-number-at-pos (if region begin) :absolute))
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
                            (point)))) :absolute))
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
    (dolist (fn tng--post-add-region-functions)
      (funcall fn begin-line end-line)))
  (deactivate-mark))


(defun tng--update-chunk-begin-end-lines (chunk-id begin-line end-line)
  "Update BEGIN-LINE and END-LINE for chunk where id = CHUNK-ID"
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
   (not 'return-value)))

(defun tng--update-chunk-hash (chunk-id sha1hash)
  "Update SHA1HASH for chunk where id = CHUNK-ID"
  (sqlite-select
   (sqlite-open tng-db-filename)
   "
UPDATE chunk
SET sha1hash = ?
WHERE id = ?
RETURNING
 id"
   (list sha1hash chunk-id)
   nil))

(defun tng--update-chunk-comment (chunk-id comment)
  "Update COMMENT for chunk where id = CHUNK-ID"
  (sqlite-select
   (sqlite-open tng-db-filename)
   "
UPDATE chunk
SET comment = ?
WHERE id = ?
RETURNING
 id"
   (list comment chunk-id)
   nil))

(defun tng--delete-chunk (chunk-id)
  "Delete chunk where id = CHUNK-ID."
  (sqlite-select
   (sqlite-open tng-db-filename)
   "
DELETE FROM chunk
WHERE id = ?"
   (list chunk-id)
   nil))


(define-minor-mode tng-mode
  "Tango mode."
  :lighter (:eval (tng-minor-mode-lighter))
  (if tng-mode
      (progn
        (tng--refresh-current-buffer-status)
        (tng--refresh-indicators)
        (add-hook 'after-change-functions 'tng-after-change :depth :local))
    (remove-hook 'after-change-functions 'tng-after-change :local)
    (setq tng--status nil)
    (mapc #'delete-overlay ; TODO
          (-filter
           (lambda (o) (plist-member (overlay-properties o) 'tng-chunk))
           (car (overlay-lists))))))

(defun tng-after-change (beg end _len)
  "BEG END _LEN."
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))


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

(defun tng-list-chunks-delete ()
  "Delete chunk."
  (interactive)
  (let-alist (tabulated-list-get-id)
    (tng--delete-chunk .id))
  (tng-list-chunks-refresh))

(defvar tng-list-chunks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tng-list-chunks-jump)
    (define-key map (kbd "C-m") #'tng-list-chunks-jump)
    (define-key map (kbd "C-k") #'tng-list-chunks-delete)
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
  (setq tabulated-list-sort-key (cons "filename" (not :invert)))
  (add-hook 'tabulated-list-revert-hook #'tng-list-chunks-refresh (not :depth) :local))

(put 'tng-list-chunks-mode 'mode-class 'special)

(defun tng-list-chunks ()
  "Display a list of existing chunks."
  (interactive)
  (let ((buf (get-buffer-create "* TNG chunks*")))
    (switch-to-buffer buf)
    (tng-list-chunks-mode)
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
      (if (listp results) (car results) results))))

(defun tng-get-completion-chunk-alist (chunk-alists)
  "Return alist '((file:beg:end:comment . chunk-id) ...)"
  (mapcar
   (lambda (chunk)
     (let-alist chunk
       (let*
           ((chunk-fmt
             (format "%s:%d:%d:%s" .filepath .start_line .end_line .comment)))
         `(,chunk-fmt . ,.id))))
   chunk-alists))

(defun tng-link-chunks (src-chunk-id dst-chunk-id)
  "Link chunk at point with another chunk."
  (interactive
   (let* ((all-chunks (tng-project-chunks))
          (src-chunk (alt-completing-read "Select SRC: " (tng-get-completion-chunk-alist all-chunks)))
          (but-src-chunks
           (-remove
            (lambda (c) (eq (let-alist c .id) src-chunk))
            all-chunks))
          (dst-chunk (alt-completing-read "Select DST: " (tng-get-completion-chunk-alist but-src-chunks))))
     (list src-chunk dst-chunk)))
  (let* ((all-chunks (tng-project-chunks))
         (src-chunk (-find (lambda (c) (eq (let-alist c .id) src-chunk-id)) all-chunks))
         (dst-chunk (-find (lambda (c) (eq (let-alist c .id) dst-chunk-id)) all-chunks))
         (srcsha1 (let-alist src-chunk .sha1hash))
         (dstsha1 (let-alist dst-chunk .sha1hash))
         (directed 1)
         (flag 1)
         (comment (read-from-minibuffer "Comment for link: "))
         (last-added-link
          (sqlite-select
           (sqlite-open tng-db-filename)
           "
INSERT INTO
 dep(src,dst,comment,flag,srcsha1,dstsha1,directed)
VALUES
 (?,?,?,?,?,?,?)
RETURNING
 id"
           (list src-chunk-id dst-chunk-id comment flag srcsha1 dstsha1 directed)
           'full)))))

(defun tng-chunk-move-up (chunk-id)
  (interactive
   (list (tng-select-chunk)))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin -1 :end -1)
    (forward-line -1)))

(defun tng-chunk-move-down (chunk-id)
  (interactive
   (list (tng-select-chunk)))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin +1 :end +1)
    (forward-line +1)))

(defun tng-chunk-expand-up (chunk-id)
  (interactive
   (list (tng-select-chunk)))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin -1 :end 0)))

(defun tng-chunk-expand-down (chunk-id)
  (interactive
   (list (tng-select-chunk)))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin 0 :end +1)))

(defun tng-chunk-shrink-up (chunk-id)
  (interactive
   (list (tng-select-chunk)))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin +1 :end 0)))

(defun tng-chunk-shrink-down (chunk-id)
  (interactive
   (list (tng-select-chunk)))
  (when chunk-id
    (tng-chunk-resize chunk-id :begin 0 :end -1)))

(defun tng--current-buffer-status ()
  (let* ((chunks (tng--file-chunks (tng--current-filepath)))
         changed out good)
    (dolist (c chunks)
      (let-alist c
        (let* ((chunk-rectangle (tng--line-rectangle .start_line .end_line))
               (chunk-bol (car chunk-rectangle))
               (chunk-eol (cdr chunk-rectangle))
               (eob (point-max)))
          (if (> chunk-eol eob)
              (push c out)
            (let ((sha1 (sha1 (buffer-substring-no-properties chunk-bol chunk-eol))))
              (if (string-equal sha1 .sha1hash)
                  (push c good)
                  (push c changed)))))))
    `((chunks . ,chunks)
      (good . ,good)
      (changed . ,changed)
      (out . ,out))))

(defun tng-chunks-at-point ()
  "Return list of tng chunks that include current point."
  (let* ((overlays (overlays-at (point)))
         (tng-overlays
          (-filter
           (lambda (o) (plist-member (overlay-properties o) 'tng-chunk))
           overlays)))
    (mapcar (lambda (o) (overlay-get o 'tng-chunk)) tng-overlays)))

(defun tng-get-completion-chunk-alist (chunk-alists)
  "Return alist '((file:beg:end:comment . chunk-id) ...)"
  (mapcar
   (lambda (chunk)
     (let-alist chunk
       (let*
           ((chunk-fmt
             (format "%s:%d:%d:%s" .filepath .start_line .end_line .comment)))
         `(,chunk-fmt . ,chunk))))
   chunk-alists))

(defun tng-select-chunk ()
  (when-let* ((chunks (tng-chunks-at-point))
              (first-chunk (car chunks)))
    (if (length= chunks 1)
        first-chunk
      (alt-completing-read
       "Select: "
       (tng-get-completion-chunk-alist chunks)))))

(defun tng--create-overlay (chunk plist)
  (let-alist chunk
    (let* ((chunk-rectangle (tng--line-rectangle .start_line .end_line))
           (boc (car chunk-rectangle))
           (eoc (cdr chunk-rectangle))
           (ov (make-overlay boc eoc)))
      (prog1 ov
        (cl-loop for (k v) on plist by #'cddr
                 do (overlay-put ov k v))))))

(defun tng--refresh-indicators ()
  "Re-create overlays."
  (mapc #'delete-overlay ; TODO
          (-filter
           (lambda (o) (plist-member (overlay-properties o) 'tng-chunk))
           (car (overlay-lists))))
  (let-alist tng--status
    (dolist (cc .changed)
      (tng--create-overlay
       cc
       `(face custom-changed
         before-string ,(propertize " " 'display '(left-fringe question-mark shadow))
         tng-chunk-id ,(let-alist cc .id)
         tng-chunk ,cc)))
    (dolist (gc .good)
      (tng--create-overlay
       gc
       `(face highlight
         before-string ,(propertize " " 'display '(left-fringe large-circle shadow))
         tng-chunk-id ,(let-alist gc .id)
         tng-chunk ,gc)))))

(defun tng--refresh-current-buffer-status ()
  "Set local variable tng--status."
  (setq-local tng--status (tng--current-buffer-status)))

(cl-defun tng-chunk-resize (chunk &key begin end)
  "Update chunk, increasing or decreasing its boundaries."
  (let-alist chunk
    (tng--update-chunk-begin-end-lines
     .id
     (+ .start_line begin) (+ .end_line end)))
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))

(defun tng-chunk-comment (chunk)
  (interactive
   (list (tng-select-chunk)))
  (let-alist chunk
    (message "Current comment: %s" .comment)
    (sit-for 1.5) ; TODO:
    (tng--update-chunk-comment
     .id
     (read-from-minibuffer "New comment: ")))
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))

(defun tng-chunk-delete (chunk)
  (interactive
   (list (tng-select-chunk)))
  (let-alist chunk
    (tng--delete-chunk .id))
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))

(defun tng-minor-mode-lighter ()
  "Get lighter.
T[10]
T[*]
T[!]
T[.]
T[3/7]
T[=]"
  (let-alist tng--status
    (let* ((all (length .chunks))
           (good (length .good))
           (changed (length .changed))
           (out (length .out)))
      (propertize (format " T[%d/%d/%d/%d]" all good changed out) 'face 'compilation-error))))

(defvar-keymap tng-repeat-keymap
  :repeat t
  "]" #'tng-chunk-move-down
  "[" #'tng-chunk-move-up
  "P" #'tng-chunk-expand-up
  "N" #'tng-chunk-expand-down
  "n" #'tng-chunk-shrink-up
  "p" #'tng-chunk-shrink-down)

(defvar-keymap tng-keymap
  :parent tng-repeat-keymap
  "t" #'tng-mode
  "a" #'tng-add-region
  "l" #'tng-list-chunks
  "h" #'tng-chunk-rehash
  "c" #'tng-chunk-comment
  "C-k" #'tng-chunk-delete
  "C-c" #'tng-link-chunks)

(global-set-key (kbd "M-t") tng-keymap)

(provide 'tng)
;;; tng.el ends here
