
;; setting my theme.
(load-theme 'gruvbox-dark-soft t)

;; setting up spaceline with some options.
(require 'spaceline-config)
(spaceline-emacs-theme)

;; gui settings.
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(setq ring-bell-function 'ignore)

;; reload my init.el
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-x C-l") 'reload-init-file)

;; setting my font.
(set-face-attribute 'default nil :font "dina-8" )
(set-frame-font "dina-8" nil t)

;; Load Custom Themes from the ~/.emacs.d/themes/ dir
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

;;Edit Files as Root
(defun er-sudo-edit (&optional arg):
       (interactive "P")
       (if (or arg (not buffer-file-name)):
	 (find-file (concat "/sudo:root@localhost:"
			    (ido-read-file-name "Find file(as root): ")))
	 (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r") #'er-sudo-edit)

;; enabling melpa.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(highlight-indentation spaceline gruvbox-theme which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
