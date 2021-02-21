;;; package --- Summary
;;; Commentary:

;;; Code:

;;  ____   _    ____ _  __    _    ____ _____ ____
;; |  _ \ / \  / ___| |/ /   / \  / ___| ____/ ___|
;; | |_) / _ \| |   | ' /   / _ \| |  _|  _| \___ \
;; |  __/ ___ \ |___| . \  / ___ \ |_| | |___ ___) |
;; |_| /_/   \_\____|_|\_\/_/   \_\____|_____|____/

;; work around some problems with emacs 26.1
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
(require 'bind-key)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Now all the rest of my packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to bootstrap, set package-check-signature to nil
(use-package gnu-elpa-keyring-update)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package smart-compile
  :init
  (progn
    (setq smart-compile-alist
        '((go-mode          . "go test ./...")
          ("\\.py$"         . "python %f")
          ("\\.sh$"         . "./%f")
          (emacs-lisp-mode  . (emacs-lisp-byte-compile))
         ))
	)
  :custom
  (compilation-read-command nil "don't ask for alterations to compile command")
  :bind (("C-c C-c" . smart-compile)))

(use-package diminish)

(use-package ivy
  :diminish
  :config
  (setq
   ivy-count-format "(%d/%d) "
   ivy-use-virtual-buffers t)
  :hook ((after-init . ivy-mode)))

(use-package which-key
  :bind
  ("C-h b" . which-key-show-top-level)
  :init
  (which-key-mode t))

(use-package counsel
  :after ivy
  :bind (("C-x b" . counsel-switch-buffer)
		 ("M-y" . counsel-yank-pop)
		 ("M-s" . counsel-rg)
		 :map ivy-minibuffer-map
		 ("M-y" . ivy-next-line-and-call))
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
		 :map swiper-map
         ("C-r" . ivy-previous-line)))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package python-black
  :hook python-mode
  :config
  (python-black-on-save-mode))

(use-package magit)

(use-package flycheck
  :bind (("M-n" . flycheck-next-error)
		 ("M-p" . flycheck-previous-error))
  :hook ((after-init . global-flycheck-mode)))

(use-package company
  :hook ((go-mode python-mode) . company-mode)
  :custom
  (company-dabbrev-downcase nil "don't downcase everything")
  (company-idle-delay 0.05 "react faster")
  (company-minimum-prefix-length 2 "react sooner")
)

(use-package lsp-ui
  :commands lsp-ui-mode
  :defer t
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil))

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp
  :bind (("C-c r" . lsp-find-references))
  :config
  (setq lsp-before-save-edits t)
  (setq lsp-enable-snippet nil))

(use-package go-mode
  :mode "\\.go$"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :custom
  (gofmt-args (list "-s" "-r=(a) -> a") "simplify on gofmt"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Load path to include subdirectories of .emacs.d
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;also add subdirectories of site-lisp
(let ((default-directory "/usr/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;;LOAD OTHER FILES
;Customize generated settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;LaTeX / AUCTeX / TeX related settings
(load "~/.emacs.d/tex.el")


;;rcirc-mode
(add-hook 'rcirc-mode-hook (lambda () (set (make-local-variable 'scroll-conservatively) 8192)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(global-set-key (kbd "C-x O") (lambda ()  (interactive) (other-window -1)))

(setq snake-score-file "~/.emacs.d/snake-scores")
(setq tetris-score-file "~/.emacs.d/tetris-scores")

;;GENERAL CUSTOMIZATIONS
(setq inhibit-startup-screen t)      ; Skip emacs splash screen
(put 'upcase-region 'disabled nil)   ; Turn on upcase-region
(put 'downcase-region 'disabled nil)
(setq-default fill-column 80)        ; Default fill width 70 is too small
(setenv "PAGER" "/bin/cat")          ; so man works in terminal
;Turn off menu-bar but only if in a terminal
(if (not window-system) (menu-bar-mode -1))
(define-key help-map "a" 'apropos) ;slightly more results than default
(electric-pair-mode 1)


;;BACKUP FILE SETTINGS
(defvar --bu-directory (concat user-emacs-directory "saves"))
(if (not (file-exists-p --bu-directory))
        (make-directory --bu-directory t))
;(custom-set-variables
;  '(auto-save-file-name-transforms '((".*" (concat user-emacs-directory "saves/\\1") t))))
(setq
   make-backup-files t       ; backup of a file the first time it is saved
   backup-by-copying t       ; don't clobber symlinks
   backup-directory-alist    ; backup save path
  `(("." . ,(concat user-emacs-directory "backups")))
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
(defun insert-random-number (*N)
  "Insert *N random digits.
*n default to 5.
  Call `universal-argument' before for different count.
  URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'"
  (interactive "P")
  (let ((-charset "1234567890" )
        (-baseCount 10))
    (dotimes (-i (if (numberp *N) (abs *N) 5 ))
      (insert (elt -charset (random -baseCount))))))


(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring ARG.
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
