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
(require 'org-ml)

(defvar-local tng--status nil
  "Current buffer status as alist.")

(defvar-local tng-project-dir (project-root (project-current))
  "Root of current project.")

(defvar-local tng-db-filename
    (file-name-concat tng-project-dir "tango.org")
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
  (let* ((all-chunks (tng-org-project-chunks)))
    (-filter
     (lambda (chunk) (let-alist chunk (string-equal .filepath filepath)))
     all-chunks)))

(defun tng--chunk-from-org-item-at-point ()
  (let ((pt (point)))
    (when-let
        ((id (org-entry-get pt "TNG_ID"))
         (filepath (org-entry-get pt "TNG_FILEPATH"))
         (start_line (string-to-number (org-entry-get pt "TNG_START_LINE")))
         (end_line (string-to-number (org-entry-get pt "TNG_END_LINE")))
         (comment (org-entry-get pt "TNG_COMMENT"))
         (sha1hash (org-entry-get pt "TNG_SHA1HASH"))
         (chunkfilepath (file-relative-name (buffer-file-name) tng-project-dir)))
      `((id . ,id)
        (filepath . ,filepath)
        (start_line . ,start_line)
        (end_line . ,end_line)
        (comment . ,comment)
        (sha1hash . ,sha1hash)
        (chunkfilepath . ,chunkfilepath)))))

(defun tng-org-project-chunks ()
  (org-ql-query
  :select #'tng--chunk-from-org-item-at-point
  :from (mapcar
         (lambda (file)
           (file-name-concat ".tng" file))
         (directory-files (file-name-concat tng-project-dir ".tng") (null :full) "org$"))
  :where '(property "tng_id")))


(defvar tng--post-add-region-functions nil
  "Function to call after new chunk added.
Takes START and END as arguments.")

(defun tng--refresh-status-after-chunk-add (&rest _)
  "Refresh"
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))

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
         (chunk-id (org-id-new)) ;; TODO
         (element (format "* %s\n:PROPERTIES:\n:tng_id: %s\n:tng_filepath: %s\n:tng_start_line: %s\n:tng_end_line: %s\n:tng_comment: %s\n:tng_sha1hash: %s\n:END:\n" comment chunk-id filepath begin-line end-line comment sha1-hash))) ;; TODO: slugify title
    (let ((temporary-file-directory
           (file-name-concat tng-project-dir ".tng")))
      (make-temp-file "chunk-" (null :dir-flag) ".org" element))
    (dolist (fn tng--post-add-region-functions)
      (funcall fn begin-line end-line)))
  (deactivate-mark))

(defun tng--reset-chunk-to-region (begin end)
  (interactive "r")
  (let* ((start-line (line-number-at-pos begin))
         (end-line (line-number-at-pos end))
         (chunk (alt-completing-read
                 "Select: "
                 (tng-get-completion-chunk-alist (let-alist tng--status .chunks)))))
    (let-alist chunk
      (tng--update-chunk-begin-end-lines .id start-line end-line)))
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))


(defun tng--update-chunk-begin-end-lines (chunk-id begin-line end-line)
  "Update BEGIN-LINE and END-LINE for chunk where id = CHUNK-ID"
  (progn
    (tng--update-chunk-property chunk-id "tng_start_line" (number-to-string begin-line))
    (tng--update-chunk-property chunk-id "tng_end_line" (number-to-string end-line))))


(defun tng--update-chunk-property (chunk-id property value)
  "Update PROPERTY for chunk where id = CHUNK-ID"
  (let* ((chunks (tng-current-chunks))
         (chunk (-first
                 (lambda (chunk)
                   (let-alist chunk
                     (string-equal .id chunk-id)))
                 chunks))
         (chunkfile (let-alist chunk .chunkfilepath)))
    (with-current-buffer (find-file-noselect chunkfile)
      (org-entry-put (point) property value)
      (save-buffer))))

(defun tng--update-chunk-hash (chunk-id sha1hash)
  "Update SHA1HASH for chunk where id = CHUNK-ID"
  (tng--update-chunk-property chunk-id "tng_sha1hash" sha1hash))

(defun tng--update-chunk-comment (chunk-id comment)
  "Update COMMENT for chunk where id = CHUNK-ID"
  (tng--update-chunk-property chunk-id "tng_comment" comment))

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
        (add-hook 'after-change-functions #'tng-after-change :depth :local))
    (remove-hook 'after-change-functions #'tng-after-change :local)
    (setq tng--status nil)
    (mapc #'delete-overlay ; TODO
          (-filter
           (lambda (o) (plist-member (overlay-properties o) 'tng-chunk))
           (car (overlay-lists))))))

(defun tng-after-change (beg end _len)
  "BEG END _LEN."
  (tng--auto-fix-moved-chunks)
  (tng--refresh-current-buffer-status)
  (tng--refresh-indicators))


(defun tng-list-chunks-refresh ()
  "Refresh list of chunks."
  (let* ((chunks (tng-org-project-chunks)))
    (cl-flet
        ((tabulate-chunk (chunk)
           (let-alist chunk
             (list
              chunk
              (vector
               (list
                .id
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

(defun tng-list-chunks-reset-lines ()
  "Reset chunk's lines. Set start_line=1 end_line=1"
  (interactive)
  (let-alist (tabulated-list-get-id)
    (tng--update-chunk-begin-end-lines .id 1 1))
  (tng-list-chunks-refresh))

(defun tng-list-chunks-reset-lines-read-lines (new-start new-end)
  "Reset chunk's lines. Set start_line=1 end_line=1"
  (interactive "nNew start line: \nnNew end line: ")
  (let-alist (tabulated-list-get-id)
    (tng--update-chunk-begin-end-lines .id new-start new-end))
  (tng-list-chunks-refresh))

(defvar tng-list-chunks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tng-list-chunks-jump)
    (define-key map (kbd "C-m") #'tng-list-chunks-jump)
    (define-key map (kbd "C-k") #'tng-list-chunks-delete)
    (define-key map (kbd "C-r") #'tng-list-chunks-reset-lines)
    (define-key map (kbd "r") #'tng-list-chunks-reset-lines-read-lines)
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
    (setq-local tng-project-dir tng-project-dir)
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
      results)))

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

(defun tng-link-chunks (src-chunk dst-chunk)
  "Link chunk at point with another chunk."
  (interactive
   (let* ((all-chunks (tng-org-project-chunks))
          (src (alt-completing-read "Select SRC: " (tng-get-completion-chunk-alist all-chunks)))
          (but-src-chunks
           (-remove
            (lambda (c) (eq (let-alist c .id) (let-alist src .id)))
            all-chunks))
          (dst (alt-completing-read "Select DST: " (tng-get-completion-chunk-alist but-src-chunks))))
     (list src dst)))
  (let* ((src-id (let-alist src-chunk .id))
         (dst-id (let-alist dst-chunk .id))
         (srcsha1 (let-alist src-chunk .sha1hash))
         (dstsha1 (let-alist dst-chunk .sha1hash))
         (src-comment (let-alist src-chunk .comment))
         (dst-comment (let-alist dst-chunk .comment))
         (directed 1)
         (flag 1)
         (comment (read-from-minibuffer "Comment for link: "))
         (element
          (format "* %s\n:PROPERTIES:\n:tng_link_src_id: %s\n:tng_link_dst_id: %s\n:tng_link_src_sha1: %s\n:tng_link_dst_sha1: %s\n:tng_link_directed: %s\n:tng_link_flag: %s\n:tng_link_comment: %s\n:tng_link_src_comment: %s\n:tng_link_dst_comment: %s\n:END:\n" comment src-id dst-id srcsha1 dstsha1 directed flag comment src-comment dst-comment)))
    (let ((temporary-file-directory
           (file-name-concat tng-project-dir ".tng")))
      (make-temp-file "link-" (null :dir-flag) ".org" element))))

(defun tng-chunk-move-up (chunk)
  (interactive
   (list (tng-select-chunk)))
  (when chunk
    (tng-chunk-resize chunk :begin -1 :end -1)
    (forward-line -1)))

(defun tng-chunk-move-down (chunk)
  (interactive
   (list (tng-select-chunk)))
  (when chunk
    (tng-chunk-resize chunk :begin +1 :end +1)
    (forward-line +1)))

(defun tng-chunk-expand-up (chunk)
  (interactive
   (list (tng-select-chunk)))
  (when chunk
    (tng-chunk-resize chunk :begin -1 :end 0)))

(defun tng-chunk-expand-down (chunk)
  (interactive
   (list (tng-select-chunk)))
  (when chunk
    (tng-chunk-resize chunk :begin 0 :end +1)))

(defun tng-chunk-shrink-up (chunk)
  (interactive
   (list (tng-select-chunk)))
  (when chunk
    (tng-chunk-resize chunk :begin +1 :end 0)))

(defun tng-chunk-shrink-down (chunk)
  (interactive
   (list (tng-select-chunk)))
  (when chunk
    (tng-chunk-resize chunk :begin 0 :end -1)))

(defun tng--current-buffer-status ()
  (let* ((chunks (tng--file-chunks (tng--current-filepath)))
         changed out good)
    (dolist (c chunks)
      (let-alist c
        (let* ((chunk-rectangle (tng--line-rectangle .start_line .end_line))
               (chunk-boc (car chunk-rectangle))
               (chunk-eoc (cdr chunk-rectangle))
               (eob (point-max)))
          (if (> chunk-eoc eob)
              (push c out)
            (let ((sha1be (sha1 (buffer-substring-no-properties chunk-boc chunk-eoc))))
              (if (string-equal sha1be .sha1hash)
                  (progn
                    (let* ((chunk-boc-marker (set-marker (make-marker) chunk-boc))
                           (chunk-eoc-marker (set-marker (make-marker) chunk-eoc)))
                      (set-marker-insertion-type chunk-boc-marker :advances)
                      (add-to-list 'c `(boc-marker . ,chunk-boc-marker) :append)
                      (add-to-list 'c `(eoc-marker . ,chunk-eoc-marker) :append))
                    (push c good))
                (push c changed)))))))
    `((chunks . ,chunks)
      (good . ,good)
      (changed . ,changed)
      (out . ,out))))

(defun tng--auto-fix-moved-chunks ()
  "Update start_line and end_line for shifted chunk.

A chunk is shifted if it's sha1hash remains unchanged but one of
the markers or both point to new lines."
  (let-alist tng--status
    (dolist (gc .good)
      (let-alist gc
        (let* ((chunk-rectangle (tng--line-rectangle .start_line .end_line))
               (chunk-boc (car chunk-rectangle))
               (chunk-eoc (cdr chunk-rectangle))
               (chunk-marker-start-line (line-number-at-pos .boc-marker))
               (chunk-marker-end-line (1- (line-number-at-pos .eoc-marker)))
               (sha1markers (sha1 (buffer-substring-no-properties .boc-marker .eoc-marker))))
          (when (and
                 (or (not (= chunk-marker-start-line .start_line))
                     (not (= chunk-marker-end-line .end_line)))
                 (string= .sha1hash sha1markers))
            (tng--update-chunk-begin-end-lines
             .id
             chunk-marker-start-line
             chunk-marker-end-line)))))))

(defun tng-chunks-at-point ()
  "Return list of tng chunks that include current point."
  (let* ((overlays (overlays-at (point)))
         (tng-overlays
          (-filter
           (lambda (o) (plist-member (overlay-properties o) 'tng-chunk))
           overlays)))
    (mapcar (lambda (o) (overlay-get o 'tng-chunk)) tng-overlays)))

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
       `(face isearch-fail
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

(defun tng--buffer-status (buffer)
  (let* ((chunks (tng--file-chunks (tng--current-filepath)))
         changed out good)
    (dolist (c chunks)
      (let-alist c
        (let* ((chunk-rectangle (tng--line-rectangle .start_line .end_line))
               (chunk-boc (car chunk-rectangle))
               (chunk-eoc (cdr chunk-rectangle))
               (eob (point-max)))
          (if (> chunk-eoc eob)
              (push c out)
            (let ((sha1be (sha1 (buffer-substring-no-properties chunk-boc chunk-eoc))))
              (if (string-equal sha1be .sha1hash)
                  (progn
                    (let* ((chunk-boc-marker (set-marker (make-marker) chunk-boc))
                           (chunk-eoc-marker (set-marker (make-marker) chunk-eoc)))
                      (set-marker-insertion-type chunk-boc-marker :advances)
                      (add-to-list 'c `(boc-marker . ,chunk-boc-marker) :append)
                      (add-to-list 'c `(eoc-marker . ,chunk-eoc-marker) :append))
                    (push c good))
                (push c changed)))))))
    `((chunks . ,chunks)
      (good . ,good)
      (changed . ,changed)
      (out . ,out))))

(defun tng--refresh-buffer-status (buffer)
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

(defun tng-chunk-rehash (chunk)
  (interactive
   (list (tng-select-chunk)))
  (let-alist chunk
    (let* ((rectangle (tng--line-rectangle .start_line .end_line))
           (start (car rectangle))
           (end (cdr rectangle))
           (new-sha1hash (sha1 (buffer-substring-no-properties start end))))
      (tng--update-chunk-hash .id new-sha1hash)
      (tng--refresh-current-buffer-status)
      (tng--refresh-indicators))))

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
  "R" #'tng--reset-chunk-to-region
  "C-k" #'tng-chunk-delete
  "C-c" #'tng-link-chunks)

(global-set-key (kbd "M-t") tng-keymap)

(provide 'tng)
;;; tng.el ends here
