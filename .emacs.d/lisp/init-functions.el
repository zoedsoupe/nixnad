;;; init-functions.el --- Sets up custom functions -*- lexical-binding: t -*-
;;; Commentary:
;;;  Custom functions
;;; Code:

(defun seek-backward-to-char (char)
  "Seek backwards to a character (CHAR)."
  (interactive "cSeek back to char: ")
  (while (not (= (char-after) char))
    (forward-char -1)))

(defun delete-between-pair (char)
  "Delete in between the given pair(CHAR)."
  (interactive "cDelete between char: ")
  (forward-char 1)
  (zap-to-char 1 char)
  (insert char)
  (forward-char -1))

(provide 'init-functions)
;;; init-functions ends here
