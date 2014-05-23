(require 'compile)
(add-hook 'c++-mode-hook
           (lambda ()
			      (unless (file-exists-p "Makefile")
					       (set (make-local-variable 'compile-command)
								(concat "g++ -pedantic " buffer-file-name)))))
