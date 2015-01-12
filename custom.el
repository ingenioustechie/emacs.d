

(setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                              (cdr mode-line-buffer-identification)))))


;; Standard Jedi.el setting
(require 'epc)
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
(setq jedi:server-command '("python" "~/.emacs.d/elpa/jedi-20150109.2230/jediepcserver.py"))
(setq jedi:tooltip-method '(pos-tip))
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)


;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.
