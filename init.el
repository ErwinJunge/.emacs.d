;; /This/ file (~init.el~) that you are reading
;; should be in this folder
(add-to-list 'load-path "~/.emacs.d/")

;; Package Manager
;; See ~Cask~ file for its configuration
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Elpy
(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)

;; Git
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))

;; Theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)
(set-cursor-color "firebrick")

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Django templates
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
    '(("django"    . "\\.html\\'"))
)
(setq web-mode-markup-indent-offset 4)

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Highlight matching parens
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; Graphviz
(require 'graphviz-dot-mode)

;; Folding
(require 'fold-dwim)

;; Sass
(require 'sass-mode)
(defun sass-indent-p ()
  "Return t if the current line can have lines nested beneath it."
  (unless (or (looking-at "^.* :.*$")
              (looking-at "^.*: .*$"))
    (loop for opener in sass-non-block-openers
          if (looking-at opener) return nil
          finally return t)))

;; Scss
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Org mode
(setq org-fontify-emphasized-text nil)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-safe-themes (quote ("f715f948867d85fa384b6c20d793dfd126d71996afd62f9d003705c960929977" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#383838")
 '(graphviz-dot-auto-indent-on-semi nil)
 '(graphviz-dot-preview-extension "png")
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(safe-local-variable-values (quote ((project-venv-name . "openict-erp") (project-venv-name . "django-rest-framework-test") (project-venv-name . "django-rest-framework") (project-venv-name . "pony") (project-venv-name . "qollap") (project-venv-name . "bacchi") (project-venv-name . "beebox") (project-venv-name . "projects") (project-venv-name . "myproject-env"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
