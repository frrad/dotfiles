; Frederick Robinson
; 5 December 2013


;Load path to include subdirectories of .emacs.d
(let ((default-directory "~/.emacs.d/")) 
  (normal-top-level-add-subdirs-to-load-path))

;;LOAD OTHER FILES
;Customize generated settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;LaTeX / AUCTeX / TeX related settings
(load "~/.emacs.d/tex.el")
;C++ Mode modifications
(load "~/.emacs.d/cpp.el")

(global-set-key (kbd "M-B") 'recompile)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-save-file "~/.emacs.d/.smex-items") ; smex data inside .emacs.d

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(require 'yasnippet)
(yas-global-mode t)

(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'flyspell-prog-mode)

(add-hook 'rcirc-mode-hook (lambda () (set (make-local-variable 'scroll-conservatively) 8192)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(global-set-key (kbd "C-x O") (lambda ()  (interactive) (other-window -1)))


(setq snake-score-file "~/.emacs.d/snake-scores")
(setq tetris-score-file "~/.emacs.d/tetris-scores")

;;GENERAL CUSTOMIZATIONS
(setq inhibit-startup-screen t)    ; Skip emacs splash screen
(put 'upcase-region 'disabled nil) ; Turn on upcase-region
(put 'downcase-region 'disabled nil)
(ido-mode t)                       ; ido-mode!
(setq-default fill-column 100)     ; Default fill width 70 is too small
(setenv "PAGER" "/bin/cat")        ; so man works in terminal  
;Turn off menu-bar but only if in a terminal
(if (not window-system) (menu-bar-mode -1)) 

;;BACKUP FILE SETTINGS
(defvar --bu-directory (concat user-emacs-directory "saves"))
(if (not (file-exists-p --bu-directory))
        (make-directory --bu-directory t))
(setq
   make-backup-files t       ; backup of a file the first time it is saved
   backup-by-copying t       ; don't clobber symlinks
   backup-directory-alist
   `(("." . ,--bu-directory)); backfup save path
   delete-old-versions t     ; delete excess backup files silently
   kept-new-versions 6
   kept-old-versions 2
   version-control t         ; version numbers for backup files
)

;;FONT / WINDOW SIZE FOR WINDOWED SETUP
(when (window-system)
  (set-frame-size (selected-frame) 128 50)
  (set-face-attribute 'default nil :family "Inconsolata")
  (set-face-attribute 'default nil :height 120)
)

