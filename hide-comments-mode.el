;;; hide-comments-mode.el --- Smart comment character hiding -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, comments
;; URL: https://github.com/Laluxx/hide-comments-mode

;;; Commentary:

;; TODO advise `comment-dwim' to correctly uncomment
;; This package provides a minor mode to hide comment characters
;; while preserving the comment text and indentation.

;;; Code:

(defgroup hide-comments nil
  "Hide comment characters in various programming modes."
  :group 'convenience
  :prefix "hide-comments-")

(defvar-local hide-comments--overlays nil
  "List of overlays used to hide comment characters in the current buffer.")

(defun hide-comments--make-overlay (beg end)
  "Create an overlay to visually hide comment characters between BEG and END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'display "")
    (overlay-put overlay 'hide-comments t)
    (overlay-put overlay 'evaporate t)  ; Auto-remove if text is modified
    (overlay-put overlay 'modification-hooks '(hide-comments--overlay-modified))
    overlay))

(defun hide-comments--overlay-modified (overlay after _beg _end &optional _len)
  "Handle overlay modification.
OVERLAY is the affected overlay, AFTER indicates if this is after the modification."
  (when (and after (overlay-get overlay 'hide-comments))
    (delete-overlay overlay)))

(defun hide-comments--find-comment-region ()
  "Find comment region around point."
  (let* ((syntax-ppss (syntax-ppss))
         (in-comment (nth 4 syntax-ppss))
         (comment-start (nth 8 syntax-ppss)))
    (when in-comment
      (save-excursion
        (goto-char comment-start)
        (skip-chars-backward " \t")
        (let ((end (progn
                     (forward-line)
                     (point))))
          (cons comment-start end))))))

(defun hide-comments--handle-line ()
  "Handle comments in current line."
  (save-excursion
    (beginning-of-line)
    (let ((line-end (line-end-position)))
      (cond
       ((eq major-mode 'emacs-lisp-mode)
        (while (re-search-forward ";+[ \t]?" line-end t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (unless (nth 3 (syntax-ppss))  ; not in string
              (push (hide-comments--make-overlay start end)
                    hide-comments--overlays)))))
       ((eq major-mode 'c-mode)
        (while (re-search-forward "//[ \t]?" line-end t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (unless (nth 3 (syntax-ppss))
              (push (hide-comments--make-overlay start end)
                    hide-comments--overlays)))))))))

(defun hide-comments--handle-buffer ()
  "Process the entire buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (hide-comments--handle-line)
      (forward-line))))

(defun hide-comments--refresh ()
  "Refresh comment hiding in the buffer."
  (when hide-comments-mode
    (mapc #'delete-overlay hide-comments--overlays)
    (setq hide-comments--overlays nil)
    (hide-comments--handle-buffer)))

(defun hide-comments--after-change (&rest _)
  "Handle buffer changes."
  (when (timerp hide-comments--refresh-timer)
    (cancel-timer hide-comments--refresh-timer))
  (setq hide-comments--refresh-timer
        (run-with-idle-timer 0.1 nil #'hide-comments--refresh)))

(defun hide-comments--get-real-text (pos)
  "Get the actual text at POS, ignoring our overlays."
  (let ((overlays (overlays-at pos))
        (text (buffer-substring-no-properties pos (1+ pos))))
    (dolist (ov overlays)
      (when (overlay-get ov 'hide-comments)
        (setq text (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov)))))
    text))

(defun hide-comments--line-commented-p ()
  "Return t if current line starts with a comment."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let* ((pos (point))
           (real-text (hide-comments--get-real-text pos)))
      (or (and (eq major-mode 'emacs-lisp-mode)
               (string-prefix-p ";" real-text))
          (and (eq major-mode 'c-mode)
               (string-prefix-p "/" real-text))))))

(defun hide-comments-dwim (arg)
  "Smart comment/uncomment function for hide-comments-mode.
With prefix ARG, insert comment on empty line or kill comments."
  (interactive "*P")
  (let ((commented (hide-comments--line-commented-p)))
    (if (use-region-p)
        (if commented
            (uncomment-region (region-beginning) (region-end) arg)
          (comment-region (region-beginning) (region-end) arg))
      (if (save-excursion (beginning-of-line) (looking-at "\\s-*$"))
          ;; Empty line
          (if comment-insert-comment-function
              (funcall comment-insert-comment-function)
            (let ((add (comment-add arg)))
              (indent-according-to-mode)
              (insert (comment-padright comment-start add))
              (save-excursion
                (unless (string= "" comment-end)
                  (insert (comment-padleft comment-end add))))))
        ;; Non-empty line
        (if commented
            (uncomment-region (line-beginning-position)
                              (line-end-position))
          (comment-region (line-beginning-position)
                          (line-end-position)))))
    ;; Refresh comment hiding
    (hide-comments--handle-buffer)))

;;;###autoload
(define-minor-mode hide-comments-mode
  "Toggle hiding of comment characters in the current buffer."
  :lighter " HideCom"
  :group 'hide-comments
  (if hide-comments-mode
      (progn
        (setq hide-comments--overlays nil)
        (hide-comments--handle-buffer)
        (add-hook 'after-change-functions #'hide-comments--after-change nil t)
        ;; Override M-; with our version when mode is active
        (local-set-key (kbd "M-;") #'hide-comments-dwim))
    (progn
      (mapc #'delete-overlay hide-comments--overlays)
      (setq hide-comments--overlays nil)
      (remove-hook 'after-change-functions #'hide-comments--after-change t)
      ;; Restore original M-; binding
      (local-unset-key (kbd "M-;")))))


;;;###autoload
(define-globalized-minor-mode global-hide-comments-mode
  hide-comments-mode
  (lambda ()
    (when (memq major-mode '(c-mode emacs-lisp-mode))
      (hide-comments-mode 1))))

(provide 'hide-comments-mode)

;;; hide-comments-mode.el ends here
