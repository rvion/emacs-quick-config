(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;; Ensure { and [ can be written with right alt / alt-shift keys
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Installed packages
;; async              20151123.256  installed             Asynchronous processing in Emacs
;; autopair           20140825.427  installed             Automagically pair braces and quotes like TextMate
;; company            20151103.230  installed             Modular text completion framework
;; company-ghc        20151122.2015 installed             company-mode ghc-mod backend
;; cycbuf             20131203.1237 installed             Cycle buffers, inspired by swbuff.el, swbuff-x.el, and bs.el
;; dash               20151021.113  installed             A modern list library for Emacs
;; fiplr              20140723.2345 installed             Fuzzy Search for Files in Projects
;; flymake-easy       20140818.55   installed             Helpers for easily building flymake checkers
;; flymake-hlint      20130309.145  installed             A flymake handler for haskell-mode files using hlint
;; fringe-helper      20140620.1409 installed             helper functions for fringe bitmaps
;; ghc                20151013.1219 installed             Sub mode for Haskell mode
;; git-commit         20151111.418  installed             Edit Git commit messages
;; git-gutter+        20150925.231  installed             Manage Git hunks straight from the buffer
;; git-gutter-fringe+ 20140729.403  installed             Fringe version of git-gutter+.el
;; grizzl             20150711.2230 installed             Fast fuzzy search index for Emacs.
;; haskell-mode       20151125.613  installed             A Haskell editing mode
;; hindent            20151113.24   installed             Indent haskell code using the "hindent" program
;; rainbow-delimiters 20150320.17   installed             Highlight brackets according to their depth
;; smex               20150822.1146 installed             M-x interface with Ido-style fuzzy matching.
;; solarized-theme    20151119.1459 installed             The Solarized color theme, ported to Emacs.
;; tabbar             20141109.143  installed             Display a tab bar in the header line
;; undo-tree          20140509.522  installed             Treat undo history as a tree
;; with-editor        20151111.418  installed             Use the Emacsclient as $EDITOR


;; Git gutter and line numbers
(global-linum-mode +1)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode +1)

;; (global-set-key (kbd "C-<tab>") 'cycbuf-switch-to-next-buffer)
;; (global-set-key (kbd "C-S-<tab>") 'cycbuf-switch-to-previous-buffer)
;; (setq buffer-flip-keys (kbd "C-<tab>"))	;

(setenv "PATH" (concat "/Users/rvion/.local/bin/:" (getenv "PATH")))

(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)

;; (add-to-list 'load-path "<wherever>/ghc-mod/elisp") ; if you didn't use ELPA
;; (setq ghc-debug t) ; enable debug logging
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Follow sylinks without warnings
(setq vc-follow-symlinks t)

(require 'undo-tree)
(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
;; (global-undo-tree-mode)

(defun conf()
  (interactive)
  (find-file user-init-file))


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
(global-set-key (kbd "C-s-<up>") 'move-text-up)
(global-set-key (kbd "C-s-<down>") 'move-text-down)
(global-set-key (kbd "s-w") 'kill-this-buffer)

(delete-selection-mode 1)



;; You can get help for a command by typing

;; C-h f function-name
;; This will usually tell you if it has a standard key binding. Conversely, given a key sequence, you can type

;; C-h k key-sequence
;; To get the command that would run. For more help on getting help, you can type

;; C-h ?

(global-set-key (kbd "s-o") 'find-file)

;; Backups ================
(setq backup-directory-alist
   `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))
;; This will all place all auto-saves and backups in the directory pointed to by temporary-file-directory (e.g., C:/Temp/ on Windows).

;; If you visit the backup directory from time to time to retrieve an old file version then it’s a good idea to prevent the directory from cluttering up with very old backup files. Put this into your .emacs to automatically purge backup files not accessed in a week:

;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

(global-set-key (kbd "<backtab>") 'indent-rigidly)


;; (defun kill-full-line (arg)
;;   "Kill while line even if not at the beginning"
;;   (interactive)
;;   (beginning-of-line)
;;   (kill-line))

;; (global-set-key (kbd "s-<backspace>") 'kill-full-line)

;; =====================================================================================
;; (global-set-key (kbd "s-O") ('fiplr-find-directory "test")) ;
(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))

(global-set-key (kbd "s-T") 'reopen-killed-file)

;; ====================================================================

;;  kills to the left
(global-set-key (kbd "s-<backspace>") '(lambda () (interactive) (kill-line 0)) )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; cmd-n : new file
(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
(global-set-key (kbd "s-n") 'xah-new-empty-buffer)

;; ============================================================
;; ```

(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)
; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
This function is a custom function for tabbar-mode's tabbar-buffer-groups.
This function group all buffers into 3 groups:
Those Dired, those user buffer, and those emacs buffer.
Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    (t
     "User Buffer"
     )
    )))

(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

(global-set-key (kbd "C-S-<tab>") 'tabbar-backward-tab)
(global-set-key (kbd "C-<tab>") 'tabbar-forward-tab)


;; ================haskell mode==============
(let ((my-cabal-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(haskell-tags-on-save t))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook #'hindent-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-hi2)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))


;; ;; ========================

;; ;; not working
;; (defun switch-to-minibuffer-window ()
;;   "switch to minibuffer window (if active)"
;;   (interactive)
;;   (when (active-minibuffer-window)
;;     (select-window (active-minibuffer-window))))
;; (global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)


(require 'rainbow-delimiters)
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)
(global-set-key (kbd "M-<tab>") 'company-complete)

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers


(require 'flymake-hlint)
(add-hook 'haskell-mode-hook 'flymake-hlint-load)
;; =============== =============== =============== =============== =============== =============== ===============
(message "Loaded. Hello Remi, nice to see you today :)")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
