; Frederick Robinson
; 5 December 20131

;Set default window size
(if (window-system) (set-frame-size (selected-frame) 128 50))

;Backup File settings
(setq
   make-backup-files t      ; backup of a file the first time it is saved
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))   ; backup save path
   delete-old-versions t    ; delete excess backup files silently
   kept-new-versions 6
   kept-old-versions 2
   version-control t        ; version numbers for backup files
)