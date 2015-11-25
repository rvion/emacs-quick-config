(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)

;; (add-to-list 'load-path "<wherever>/ghc-mod/elisp") ; if you didn't use ELPA
;; (setq ghc-debug t) ; enable debug logging
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


(require 'undo-tree)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
;; (global-undo-tree-mode)

(defun end-of-line-and-indented-new-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun beginning-of-line-and-indented-new-line-before ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line))

(global-set-key (kbd "s-<return>") 'end-of-line-and-indented-new-line)

(global-set-key (kbd "s-S-<return>") 'beginning-of-line-and-indented-new-line-before)

(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))


(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
;; Bind some keys:	  ;

(global-set-key (kbd "s-P") 'smex)
;; (global-set-key (kbd "s-P") 'smex-major-mode-commands)
(global-set-key (kbd "s-S-p") 'smex)
;; (global-set-key (kbd "s-S-p") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".stack-work"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))


(global-set-key (kbd "s-p") 'fiplr-find-file)
(global-set-key (kbd "s-r") 'eval-buffer) 
(global-set-key (kbd "s-/") 'comment-dwim)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))


;; (global-set-key [M-S-up] 'move-text-up)
;; (global-set-key [M-S-down] 'move-text-down)
(global-set-key (kbd "M-S-<up>") 'move-text-up)
(global-set-key (kbd "M-S-<down>") 'move-text-down)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(delete-selection-mode 1)



;; You can get help for a command by typing

;; C-h f function-name
;; This will usually tell you if it has a standard key binding. Conversely, given a key sequence, you can type

;; C-h k key-sequence
;; To get the command that would run. For more help on getting help, you can type

;; C-h ?

(global-set-key (kbd "s-o") 'find-file)
