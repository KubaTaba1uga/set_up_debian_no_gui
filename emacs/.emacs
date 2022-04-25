;;Packages

;start package.el
(require 'package)

;add MELPA repository list
(add-to-list 'package-archives '("melpa"."https://melpa.org/packages/")t)
;initializae package.el
(package-initialize)

; dependencies to install
(defvar myPackages
  '(
    ; themas
    solarized-theme
    powerline
    flycheck-color-mode-line
    afternoon-theme

    ; global dependencies
    exec-path-from-shell
    use-package
    auto-complete
    yasnippet
    yasnippet-snippets
    flycheck
    flycheck-inline
    flycheck-pos-tip
    flycheck-popup-tip
    nav
    centaur-tabs
    company
    company-quickhelp
    company-box
    magit
    
    ; C dependencies
    auto-complete-c-headers
    google-c-style
    flycheck-clang-analyzer
    clang-format
    company-irony-c-headers
    company-irony
    irony
    irony-eldoc

    ; python dependencies
    jedi
    jedi-direx
    company-jedi
    python-black
    flycheck-mypy))


; install all dependencies
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

; start using environment variables
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;Thema

 
; set bottom line 
(require 'powerline)
(powerline-default-theme)

; enable line numeration
(global-linum-mode t)

; enable column numeration.
(column-number-mode t)

; enable full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; set up visible tabs
(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-c <left>")  'centaur-tabs-backward)
(global-set-key (kbd "C-c <right>") 'centaur-tabs-forward)
; bar in the same color
(centaur-tabs-headline-match)
; bar style
(setq centaur-tabs-style "bar")
; enable icons
(setq centaur-tabs-set-icons t)
; enable active tab marker
(setq centaur-tabs-set-bar 'left)

; config side nav
(add-to-list 'load-path "/home/taba1uga/Desktop/")
(require 'nav)
(nav-disable-overeager-window-splitting)
(global-set-key (kbd "<f8>") 'nav-toggle)
; lynx-like motion
(defun nav-mode-hl-hook ()
  (local-set-key (kbd "<right>") 'nav-open-file-under-cursor)
  (local-set-key (kbd "<left>")  'nav-go-up-one-dir))
(add-hook 'nav-mode-hook 'nav-mode-hl-hook)

; add powerline colors
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; Global config

; assign undo to ctrl + >
(global-set-key (kbd "C-z") 'undo)

; enable eldoc
(global-eldoc-mode 1)

; enable git
(use-package magit
:config
(global-set-key (kbd "C-c g") 'magit-status))
; usage:
    ;; `S` in magit-status stages all files
    ;; `c c` initiates a commit. Write the message and then press C-c C-c to actually create the commit.
    ;; `P u` pushes to the upstream branch. In the popup that appears after you have pressed P you can
    ;;		 see the upstream. If the upstream is not set yet, then you can still use P u. You'll be asked for a
    ;;		branch which is then configured as the upstream before pushing.




; enable auto-completion
;;(require 'auto-complete)
; do default config
;;(require 'auto-complete-config)
;;(ac-config-default)



; enable auto paranthesis completion
(electric-pair-mode 1)

; enable company auto-completion
(use-package company
	:ensure t
	:config
	(setq company-tooltip-limit 20)
	(setq company-show-numbers t)
	(setq company-idle-delay 0)
	(setq company-echo-delay 0)
	(setq company-minimum-prefix-length 2)
	(setq company-selection-wrap-around t)
        ; Use tab key to cycle through suggestions.
        ; ('tng' means 'tab and go')
	(company-tng-configure-default)
)
(add-hook 'after-init-hook 'global-company-mode)

; enable company code docs
;(company-quickhelp-mode t)
;(setq company-quickhelp-delay 1)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

; enable snippets completetion
(require 'yasnippet)
(yas-global-mode 1)

; enable code checking
(add-hook 'after-init-hook #'global-flycheck-mode)

; display errors in small pop-up window
(require 'pos-tip)
(eval-after-load 'flycheck
 (if (display-graphic-p)
     (flycheck-pos-tip-mode)
   (flycheck-popup-tip-mode)))

;; ; display error inline
;; (require 'flycheck-inline)
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

; turn Yes or No into y or n
(defalias 'yes-or-no-p 'y-or-n-p)
    
;; Open terminal by pressing F4 key
(global-set-key [f4] '(lambda () (interactive) (term (getenv "SHELL"))))

; custom commenting
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

; move file
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

; make small compilation window
  (defun my-compile ()
      "Run compile and resize the compile window"
      (interactive)
      (progn
        (call-interactively 'compile)
        (setq cur (selected-window))
        (setq w (get-buffer-window "*compilation*"))
        (select-window w)
        (setq h (window-height w))
        (shrink-window (- h 10))
        (select-window cur)
        )
    )
  (defun my-compilation-hook () 
    "Make sure that the compile window is splitting vertically"
    (progn
      (if (not (get-buffer-window "*compilation*"))
         (progn
	    (split-window-vertically)
	    )
	  )
      )
  )
(add-hook 'compilation-mode-hook 'my-compilation-hook)
  (global-set-key [f9] 'my-compile)

; navigate threw errors
  (defun my-next-error () 
    "Move point to next error and highlight it"
    (interactive)
    (progn
      (next-error)
      (end-of-line-nomark)
      (beginning-of-line-mark)
      )
  )
  
  (defun my-previous-error () 
    "Move point to previous error and highlight it"
    (interactive)
    (progn
      (previous-error)
      (end-of-line-nomark)
      (beginning-of-line-mark)
      )
  )
  (global-set-key (kbd "M-n") 'my-next-error)
  (global-set-key (kbd "M-p") 'my-previous-error)
  
 

;; C

; enable flycheck 
(add-hook 'c-mode-hook 'flycheck-mode)

; enable always code formatting
(add-hook 'c-mode-hook 'google-set-c-style)
; enable audo indent on new line
(add-hook 'c-mode-hook 'google-make-newline-indent)

; static code analysis
(with-eval-after-load 'flycheck
  (require 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup))

; compile C program
(defun compile-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (let ((file (file-name-nondirectory buffer-file-name)))
         (concat "gcc -std=c17 -o " 
             (concat (file-name-sans-extension file) ".out")
             " " file)))
  (shell-command foo))

; compilation shortcut
(add-hook 'c-mode-common-hook
          ;; rebind key
          (lambda () (global-set-key  (kbd "<f2>") 'compile-c-program))
          t)

; run C program
(defun run-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (let ((file (file-name-nondirectory buffer-file-name))) (concat "./" (concat (file-name-sans-extension file) ".out"))))
  (shell-command foo))

; run shortcut
(add-hook 'c-mode-common-hook
          ;; rebind key
          (lambda () (global-set-key  (kbd "<f3>") 'run-c-program))
          t)

; create formatting standard file
(defun create-clang-format ()
  (when (eq major-mode 'c-mode)
  (interactive)
  (shell-command "clang-format -style=llvm -dump-config > .clang-format"))
)
; enable auto creation
(add-hook 'before-save-hook #'create-clang-format)

; reformat buffer using .clang-format file 
(defun use-clang-format ()
  (when (eq major-mode 'c-mode)
    (clang-format-buffer))
)
; enable reformat
(add-hook 'after-save-hook #'use-clang-format)


; add company backend
(use-package company-irony
	:ensure t
	:config
	(require 'company)
	(add-to-list 'company-backends 'company-irony)
)

(require 'company)
(with-eval-after-load 'company
	(add-hook 'c-mode-hook 'company-mode)
	(add-hook 'c-mode-hook 'company-irony)
	(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
	(add-hook 'c-mode-hook 'irony-eldoc)
	;; (add-hook 'c-mode-hook 'irony-mode)
)

; company headers completion
(require 'company-irony-c-headers)
   ;; Load with `irony-mode` as a grouped backend
   (eval-after-load 'company
     '(add-to-list
       'company-backends 'company-irony-c-headers))




;; python

; disable auto-complete because of conflicts
(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))

; enable code checking
(require 'flycheck-mypy)
(add-hook 'python-mode-hook 'flycheck-mode)
; disable other checkers than mypy
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)

; add autocompletion
(add-to-list 'company-backends 'company-jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

; add aut-formating
(require 'python-black)
 (add-hook 'python-mode-hook 'python-black-on-save-mode)

; use ipython3
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")


; disable wierd indentation
(electric-indent-mode -1)

; set up virtualenv
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))
;; usage:
;;;;   in terminl `python -m venv env`
;;;;   in emacs `M-x pyvenv-activate RET dir_to_the_environment/env`'


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(irony-cmake-executable "/usr/bin/cmake")
 '(package-selected-packages
   '(irony yasnippet-snippets use-package solarized-theme python-black py-autopep8 nav jedi-direx google-c-style flycheck-pos-tip flycheck-popup-tip flycheck-mypy flycheck-clang-analyzer exec-path-from-shell elpy clang-format centaur-tabs auto-complete-c-headers)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Futura LT" :foundry "PfEd" :slant normal :weight bold :height 203 :width normal))))
 '(company-tooltip-selection ((t (:extend t :background "black" :foreground "dark red" :weight bold)))))
