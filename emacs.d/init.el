;; - - - - - - - - - - - - - - -
;;Falseprincess's Init.el Config
;;Visit The Emacs Wiki before making any modifications to this file!!!
;;https://www.emacswiki.org/emacs/SiteMap
;; - - - - - - - - - - - - - - -


;; Requiring Melpa at Startup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))


;; - - - - General Rules - - - -

;; Disable Welcome Screen
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Custom Startup Message
(setq initial-scratch-message "")

;; Disable/Enable The Menu-bar
;;(menu-bar-mode 1)
(menu-bar-mode -1)

;; Disable The Toolbar
(tool-bar-mode -1)

;; Setting the Default Font
(set-face-attribute 'default nil :font "Terminus-8")

;; Display Line Numbers
(when (version<= "26.0.50" emacs-version )
(global-display-line-numbers-mode))

;; Disable The Scrollbar
(scroll-bar-mode -1)

;; Global line Highlight
(global-hl-line-mode +1)

;; Delete Selection Mode
(delete-selection-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(lua-mode highlight-indent-guides haskell-mode doom-modeline all-the-icons which-key doom-themes use-package nord-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package all-the-icons
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))


;; - - - - Theming Rules - - - -

;; Load Custom Themes from the ~/.emacs.d/themes/ dir
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

;; Use `nord8` from Nord's "Frost" palette as background color.
(setq nord-region-highlight "frost")

;; Setting Nord as the default theme
(load-theme 'nord t)

;; Highlight parens
(show-paren-mode 1)

