(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)
(set-language-environment "UTF-8")
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))

(defvar myPackages
  '(better-defaults
    elpy
    py-autopep8
    jedi
    jedi-direx
    material-theme
    tron-legacy-theme
    afternoon-theme
    exec-path-from-shell
    use-package
    python-black
    nav
    centaur-tabs
    web-mode
    flycheck-mypy
    )
)

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; Config tron-legacy-thema
(setq tron-legacy-theme-dark-fg-bright-comments t)
(setq tron-legacy-theme-vivid-cursor t)

;; Config thema
(setq inhibit-startup-message t)
(load-theme 'afternoon t)
(global-linum-mode t)              

;; Set up elpy
(exec-path-from-shell-initialize)
(setq elpy-rpc-virtualenv-path 'current)
(elpy-enable)


;; Set up Flymake
(add-hook 'after-init-hook 'flymake-start-on-save-buffer)
;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; Set up FlyCheck
(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)

;; Set up MyPy
(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-imports"
              "--python-version" "3.6"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-pylint 'python-mypy t)


;; Set up virtualenv paths
(setenv "WORKON_HOME" "~/.virtualenv")

;; Disable wierd indentation
(electric-indent-mode -1)


(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Set up visible tabs
(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-c <left>")  'centaur-tabs-backward)
(global-set-key (kbd "C-c <right>") 'centaur-tabs-forward)
;; Bar in the same color
(centaur-tabs-headline-match)
;; Bar style
(setq centaur-tabs-style "bar")
;; Enable icons
(setq centaur-tabs-set-icons t)
;; Enable active tab marker
(setq centaur-tabs-set-bar 'left)

;; Config side nav
(add-to-list 'load-path "/home/taba1uga/Desktop/")
(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key (kbd "<f8>") 'nav-toggle)
;;;; Lynx-like motion
(defun nav-mode-hl-hook ()
  (local-set-key (kbd "<right>") 'nav-open-file-under-cursor)
  (local-set-key (kbd "<left>")  'nav-go-up-one-dir))
(add-hook 'nav-mode-hook 'nav-mode-hl-hook)


;; Autoformatting for python
;;(require 'py-autopep8)
;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;(setq py-autopep8-options '("--max-line-length=60"))
(add-hook 'python-mode-hook 'python-black-on-save-mode)

;; Autocompletion for python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                     
(setq jedi:complete-on-dot t)
    
;; Open terminal by pressing F4 key
(global-set-key [f4] '(lambda () (interactive) (term (getenv "SHELL"))))

;; Extend default indent
;;(add-hook 'python-mode-hook '(lambda () 
;;(setq python-indent 4)))

;;Custom commenting
(defun toggle-comment ()
  "Toggle comments on the current line or highlighted region."
  (interactive)
  (if mark-active
      (let ((mark (mark))
            (point (point)))
        (if (> (mark) (point))
            (comment-or-uncomment-region
             point
             mark)
          (comment-or-uncomment-region
           mark
           point)))
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-end-position))))
(global-set-key (kbd "<f1>") 'toggle-comment)

;; Configure wb-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Autocompletion for html and css
;;(ac-config-default)
;;(setq ac-auto-start t)
;;(setq ac-delay 0.1)
;;(setq ac-auto-show-menu nil)
;;(setq ac-show-menu-immediately-on-auto-complete t)
;;(setq ac-trigger-key nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(custom-safe-themes
   '("cf9414f229f6df728eb2a5a9420d760673cca404fee9910551caf9c91cff3bfa" default))
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(exec-path-from-shell virtualenv pyenv-mode material-theme py-autopep8 jedi elpy better-defaults))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans" :foundry "PfEd" :slant normal :weight bold :height 158 :width normal)))))

