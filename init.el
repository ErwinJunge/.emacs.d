;; /This/ file (~init.el~) that you are reading
;; should be in this folder
(add-to-list 'load-path "~/.emacs.d/.cask")

;; Package Manager
;; See ~Cask~ file for its configuration
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Quack (racket)
(require 'quack)

;; Delete selection mode
(delete-selection-mode 1)

;; Tramp
(require 'tramp)
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(defun sudo-edit-current-file ()
  (interactive)
  (let ((position (point)))
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
         (let ((vec (tramp-dissect-file-name (buffer-file-name))))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" (buffer-file-name))))
    (goto-char position)))

;; Turn off toolbar
(tool-bar-mode -1)

;; Elpy
(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc--timeout 10)

;; Git
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Theme
;; https://github.com/bbatsov/zenburn-emacs
;; (load-theme 'zenburn t)
;; (set-cursor-color "firebrick")
;; (set-face-attribute 'default nil :height 100)
;; https://github.com/bbatsov/solarized-emacs
(load-theme 'solarized-light t)

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Django templates
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
    '(("django"    . "\\.html\\'"))
)
(setq web-mode-markup-indent-offset 2)

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Highlight matching parens
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; Graphviz
;; (require 'graphviz-dot-mode)

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

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Don't create lockfiles
(setq create-lockfiles nil)

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Docker mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Clojure
(require 'clojure-mode)
(require 'inf-clojure)
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

;; Move text easily
(require 'move-text)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

;; Swiper/ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)

;; No bell
(setq ring-bell-function 'ignore)

;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
  ))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs/")
(defun set-project-venv ()
  (hack-local-variables)
  (when (boundp 'project-venv-name)
    (venv-workon project-venv-name)))
(add-hook 'python-mode-hook 'set-project-venv)
(add-hook 'buffer-list-update-hook 'set-project-venv)
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

;; Django
(require 'python-django)
(setq python-shell-prompt-detect-failure-warning nil)
(defun runserver ()
  (interactive)
  (let* ((project-dir (dir-locals-find-file "."))
         (project-dir (if (stringp project-dir)
                          project-dir
                        (car project-dir)))
         (project-dir (file-name-directory project-dir)))
    (if (get-process "Python") (delete-process "Python"))
    (run-python (concat project-dir "manage.py runserver"))))

;; Lua
(require 'lua-mode)

;; Rust
(require 'rust-mode)

;; icicles
(require 'icicles)
(icy-mode 1)

;; kivy
(require 'kivy-mode)

;; f5 to recompile (for testing in elpy)
(global-set-key [f5] (quote recompile))

;; Helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; Resizing windows
(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)

;; Slack integration
(defun slack ()
  (interactive)
  (erc-tls :server "stamkracht.irc.slack.com" :port 6667 :nick "erwin" :password nil))

(setq erc-ignore-list '("SK_Dashboard" "Beanstalk" "SLACK" "Nagios"))

;; Folding
(require 'origami)
(global-origami-mode 1)
(global-set-key (kbd "C-<tab>") 'origami-recursively-toggle-node)

;; Copy filename
(defun copy-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let* ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
        (filename (format "%s:%d" filename (count-lines 1 (point)))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; Rainbow parens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(case-fold-search nil)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c093ea152d7185cc78b61b05e52648c6d2fb0d8579c2119d775630fa459e0be" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default)))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-timeout 10)
 '(elpy-test-django-runner-command
   (quote
    ("python" "-Wi" "manage.py" "test" "--noinput" "--settings=settings.settings_test" "--failfast")))
 '(elpy-test-runner (quote elpy-test-django-runner))
 '(fci-rule-color "#383838")
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "migrations")))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(python-indent-guess-indent-offset t)
 '(python-indent-offset 4)
 '(safe-local-variable-values
   (quote
    ((project-venv-name . "django-rosetta")
     (project-venv-name . "sams")
     (project-venv-name . "django-websocket-redis")
     (project-venv-name . "sepapi")
     (project-venv-name . "sepa")
     (project-venv-name . "pony")
     (project-venv-name . "qollap-aac")
     (project-venv-name . "agriplace")
     (project-venv-name . "coach360")
     (project-venv-name . "qollap_chat_kivy")
     (project-venv-name . "kivy")
     (project-venv-name . "brandfighters")
     (project-venv-name . "teg")
     (project-venv-name . "ah_parser")
     (project-venv-name . "tutti-ricambi")
     (python-django-project-settings . "settings.erwin")
     (eval set
           (make-local-variable
            (quote python-django-project-root))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (project-venv-name . "heat")
     (project-venv-name . "scope-backend")
     (project-venv-name . "bacchi")
     (project-venv-name . "beebox")
     (project-venv-name . "qollap")
     (project-venv-name . "openict-erp")
     (project-venv-name . "dasboard-backend"))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(yas-also-auto-indent-first-line nil)
 '(yas-indent-line (quote auto)))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
