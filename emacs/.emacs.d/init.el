; Frederick Robinson
; 5 December 20131

;;GENERAL CUSTOMIZATIONS
(setq inhibit-startup-screen t)    ;Skip emacs splash screen
(put 'upcase-region 'disabled nil) ;Turn on upcase-region
;Turn off menu-bar but only if in a terminal
(if (not (window-system))  (menu-bar-mode -1)) 


;Put `customize' generated settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;Default fill width 70 is too small
(setq-default fill-column 100)

;;Backup File settings
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

;;TEX / AUTEX RELATED SETTINGS

(setq
   TeX-parse-self t         ; Enable parse on load.
   TeX-auto-save t          ; Enable parse on save.
   TeX-PDF-mode t           ; output PDFs (not DVI)
)

;Set some shortcuts in math-mode
(setq LaTeX-math-list '(
    (?R "mathbb{R} " nil)
))

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)   ; enable LaTeX Math mode by default
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill) ; and auto-fill mode

