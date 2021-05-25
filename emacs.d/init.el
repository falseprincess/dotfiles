
;; setting my theme.
(load-theme 'nord t)

;; gui settings.
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(setq ring-bell-function 'ignore)

;; my global keys
(global-set-key (kbd "C-x C-r") 'sudo-edit)
(global-set-key (kbd "C-x C-l") 'reload-init-file)

;; reload my init.el
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

;; setting my font.
(set-face-attribute 'default nil :font "GohuFont:style=Regular:size=14" )
(set-frame-font "GohuFont:style=Regular:size=14" nil t)

;; Load Custom Themes from the ~/.emacs.d/themes/ dir
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

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
   '(nord-theme doom-modeline sudo-edit all-the-icons-dired all-the-icons highlight-indentation gruvbox-theme which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq doom-modeline-height 20)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;; editing files as root.
(defface find-file-root-header-face
  '((t (:foreground "#282828" :background "#fb4933")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING FILE AS ROOT!")
           (space (+ 6 (- (window-width) (length warning))))
           (bracket (make-string (/ space 2) ?-))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face 'find-file-root-header-face)))))

(add-hook 'find-file-hook 'find-file-root-header-warning)
(add-hook 'dired-mode-hook 'find-file-root-header-warning)
