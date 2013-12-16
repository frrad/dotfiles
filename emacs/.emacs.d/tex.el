;;LaTeX / AUCTeX / TeX related settings

(setq
   TeX-parse-self t         ; Enable parse on load.
   TeX-auto-save t          ; Enable parse on save.
)

; output PDFs (not DVI) - in console this interferes with
; display via dvi2tty, so restric to window-system
(if window-system (setq TeX-PDF-mode t))

;Set some shortcuts in math-mode
(setq LaTeX-math-list '(
    (?R "mathbb{R} " nil)
    (?C "mathbb{C} " nil)
    (?Z "mathbb{Z} " nil)
))

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)   ; enable LaTeX Math mode by default
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill) ; and auto-fill mode

