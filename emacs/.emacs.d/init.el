; Frederick Robinson
; 5 December 2013

;;LOAD OTHER SETTING FILES
;Customize generated settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;LaTeX / AUCTeX / TeX related settings
(load "~/.emacs.d/tex.el")

;;GENERAL CUSTOMIZATIONS
(setq inhibit-startup-screen t)    ; Skip emacs splash screen
(put 'upcase-region 'disabled nil) ; Turn on upcase-region
(ido-mode t)                       ; ido-mode!
(setq-default fill-column 100)     ; Default fill width 70 is too small
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
