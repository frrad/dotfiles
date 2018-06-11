;;; package --- Summary
;;; Commentary:

;;; Code:

;;  ____   _    ____ _  __    _    ____ _____ ____
;; |  _ \ / \  / ___| |/ /   / \  / ___| ____/ ___|
;; | |_) / _ \| |   | ' /   / _ \| |  _|  _| \___ \
;; |  __/ ___ \ |___| . \  / ___ \ |_| | |___ ___) |
;; |_| /_/   \_\____|_|\_\/_/   \_\____|_____|____/



;; Loads use-package which is then used to load and configure all other packages
;; https://github.com/CachesToCaches/getting_started_with_use_package/blob/master/init-use-package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Now all the rest of my packages
(use-package magit
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :commands company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package go-mode
  :ensure t
  :mode "\\.go$")

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :custom
  (smex-save-file "~/.emacs.d/.smex-items" "Put smex cache in ~/.emacs.d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(global-set-key (kbd "M-B") 'recompile)


(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'flyspell-prog-mode)
(add-hook 'go-mode-hook (lambda ()
						  (local-set-key (kbd "M-.") 'godef-jump)))

;;py-autopep8
;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;rcirc-mode
(add-hook 'rcirc-mode-hook (lambda () (set (make-local-variable 'scroll-conservatively) 8192)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(global-set-key (kbd "C-x O") (lambda ()  (interactive) (other-window -1)))

;;markdown-mode ($sudo apt-get install emacs-goodies-el)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)

(add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook
                          'check-parens
                          nil t))))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))



(setq snake-score-file "~/.emacs.d/snake-scores")
(setq tetris-score-file "~/.emacs.d/tetris-scores")




;;GENERAL CUSTOMIZATIONS
(setq inhibit-startup-screen t)    ; Skip emacs splash screen
(put 'upcase-region 'disabled nil) ; Turn on upcase-region
(put 'downcase-region 'disabled nil)
(require 'ido)
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

(put 'erase-buffer 'disabled nil)

(random t) ; seed random number
(defun insert-random-number (*n)
  "Insert *n random digits.
*n default to 5.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2016-01-12"
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *n) (abs *n) 5 ))
      (insert (elt -charset (random -baseCount))))))


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
(provide 'init)
;;; init.el ends here
