; Frederick Robinson
; 5 December 2013


;Load path to include subdirectories of .emacs.d
(let ((default-directory "~/.emacs.d/")) 
  (normal-top-level-add-subdirs-to-load-path))
;also add subdirectories of site-lisp
(let ((default-directory "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)


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
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


;;CLANG
(load "clang-format")

(defun clang-before-save () 
  (interactive) 
  (when 
	  (eq major-mode 'c++-mode) (clang-format-buffer)))

(add-hook 'before-save-hook 'clang-before-save)




;;rcirc-mode
(add-hook 'rcirc-mode-hook (lambda () (set (make-local-variable 'scroll-conservatively) 8192)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(global-set-key (kbd "C-x O") (lambda ()  (interactive) (other-window -1)))


(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
   Ease of use features:
    - Move to start of next line.
    - Appends the copy on sequential calls.
    - Use newline as last char even on the last line of the buffer.
    - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
		(end (line-end-position arg)))
	(when mark-active
	  (if (> (point) (mark))
		  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
		(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
	(if (eq last-command 'copy-line)
		(kill-append (buffer-substring beg end) (< end beg))
	  (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "M-k") 'copy-line)


(setq snake-score-file "~/.emacs.d/snake-scores")
(setq tetris-score-file "~/.emacs.d/tetris-scores")


(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "_") 'my-dired-create-file)
     (defun my-dired-create-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))


;;GENERAL CUSTOMIZATIONS
(setq inhibit-startup-screen t)    ; Skip emacs splash screen
(put 'upcase-region 'disabled nil) ; Turn on upcase-region
(put 'downcase-region 'disabled nil)
(ido-mode t)                       ; ido-mode!
(setq-default fill-column 80)     ; Default fill width 70 is too small
(setenv "PAGER" "/bin/cat")        ; so man works in terminal  
;Turn off menu-bar but only if in a terminal
(if (not window-system) (menu-bar-mode -1)) 

(define-key help-map "a" 'apropos) ;slightly more results than default

;;BACKUP FILE SETTINGS
(defvar --bu-directory (concat user-emacs-directory "saves"))
(if (not (file-exists-p --bu-directory))
        (make-directory --bu-directory t))
(setq
   make-backup-files t       ; backup of a file the first time it is saved
   backup-by-copying t       ; don't clobber symlinks
   backup-directory-alist
   `(("." . ,--bu-directory)); backup save path
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

