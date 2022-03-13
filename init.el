(require 'package)
 
(add-to-list 'package-archives
'("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; spacemacs dark theme in default emacs
(load-theme 'spacemacs-dark t)

;; spacemacs mode line in default emacs
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)

;; have mode icons instead of names
;; (mode-icons-mode)

;; -------------------------------------------- org configuration

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-todo-keywords
 '((sequence "TODO" "IN-PROGRESS" "PROCESSING" "WAITING" "CANCELED" "DONE"))
 )

(setq org-todo-keyword-faces
 '(("IN-PROGRESS" . "orange") ("PROCESSING" . "orange") ("WAITING" . "orange") ("CANCELED" . "yellow") )
 )

(setq inhibit-startup-screen t)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-log-done 'time)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (spacemacs-theme helm-bibtex dumb-jump tree-mode tree-sitter vscode-dark-plus-theme code-cells cdlatex lean-mode yasnippet-classic-snippets yasnippet-snippets hlinum autothemer display-theme hydra magit eink-theme flycheck-pos-tip zenburn-theme use-package org-bullets python-cell pyenv-mode material-theme flycheck exec-path-from-shell elpy ein color-theme-sanityinc-tomorrow blacken better-defaults anaconda-mode)))
 '(safe-local-variable-values
   (quote
    ((eval when
	   (require
	    (quote rainbow-mode)
	    nil t)
	   (rainbow-mode 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; spell checking
(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US")
(use-package flyspell-correct-ivy
  :ensure t
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic)))

;; resolve Windmove conflicts (like org state looping)
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-<right>")  'org-shiftright)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "S-<right>")  'windmove-right)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-<left>")  'org-shiftleft)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "S-<left>")  'windmove-left)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-<up>")  'org-shiftup)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "S-<up>")  'windmove-up)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-<down>")  'org-shiftdown)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "S-<down>")  'windmove-down)))

;; -------------------------------------------- Latex configuration

(require 'tex-site)
;; flychecks needs ispell, aspell, hunspell
(add-hook 'LaTeX-mode-hook '(lambda ()
                              (reftex-mode)
                              (flyspell-mode)
                              ))
(setq reftex-plug-into-AUCTeX t)
;; (setq reftex-load-hook (quote (imenu-add-menubar-index)))
;; (setq reftex-mode-hook (quote (imenu-add-menubar-index)))
(setq TeX-view-program-list (quote ("Okular" ("output-pdf" "okular"))))


;; ;; cdlatex
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode

;; break line after 80 characters
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(setq-default fill-column 80)

(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

;;(setq TeX-source-correlate-mode t)
(setq TeX-view-program-list (quote (("okular" ("okular"
                                            (mode-io-correlate " -p %(outpage)")
                                            " %o"))
                                 )))
(setq TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "okular") (output-html "xdg-open"))))

(setq ring-bell-function 'ignore)

(add-hook 'flyspell-mode-hook #'flyspell-buffer)

;; -------------------------------------------- useful settings

;; hide menu-bar and tool-bar forever
(menu-bar-mode -1)
(tool-bar-mode -1)

;; show parentheses
(show-paren-mode 1)

;; always reload files if they changed on disk
(setq global-auto-revert-mode t)

;; make Pg-Up Pg-Down return to the same point
(setq scroll-preserve-screen-position t)

;; start emacs as server
(server-start)

;; simple window traveling
(windmove-default-keybindings)

(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)

;; Default mode for unknown files
(setq default-major-mode 'text-mode)          

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Jump multiple lines at once (efficient travelling)
(global-set-key (kbd "C-d") (lambda () (interactive) (previous-line 10)))
(global-set-key (kbd "C-f") (lambda () (interactive) (next-line 10)))

;; move to the middle of the current line
(defun my/move-to-middle ()
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (middle (/ (+ end begin) 2)))
    (goto-char middle)))
(global-set-key (kbd "M-s") 'my/move-to-middle)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; simple window switch
(windmove-default-keybindings)

;; auto bullet mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; enable tree-sitter
;; (global-tree-sitter-mode)

;; enable dumb jump (M-.)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; show current match and number of matches
(global-anzu-mode +1)
;; let spaceline take car of matches
(setq anzu-cons-mode-line-p nil)

;; -------------------------------------------- git push

;; easy git add commit and push
(defun push-all (comment)
  (interactive "Mmessage:")
  (save-some-buffers t) ; save all buffer
  (shell-command (format "git add -A; git commit -a -m \" %s\"; git push &"
			 comment)))

(global-set-key (kbd "C-c g") 'push-all)

;; -------------------------------------------- MOOSE

;; syntax highlighting for MOOSE input and test files
(add-to-list 'load-path "/home/adrien/.emacs.d/custom-modes/emacs-moose-mode")
(require 'moose-mode)

;; -------------------------------------------- modify buffers

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     ;; Disable ido auto merge since it too frequently jumps back to the original
     ;; file name if you pause while typing. Reenable with C-z C-z in the prompt.
     (let ((ido-auto-merge-work-directories-length -1))
       (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                       (buffer-file-name))))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; Only rename if the file was saved before. Update the
  ;; buffer name and visited file in all cases.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil)))

  (setq default-directory (file-name-directory new-name))

  (message "Renamed to %s." new-name))
(global-set-key (kbd "C-c r")  #'rename-file-and-buffer)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
(global-set-key (kbd "C-c d")  #'delete-file-and-buffer)

;; -------------------------------------------- ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List

(setq ibuffer-saved-filter-groups
      '(("default"
	 ("emacs-config" (or (filename . ".emacs.d")
			     (filename . ".emacs")
			     (filename . "emacs-config")))
	 ("org" (or (mode . org-mode)
		    (filename . "OrgMode")))
	 ("python-dev" 
                (or
                 (mode . python-mode)))
	 ("c-dev" 
                (or
                 (mode . c-mode)
                 (mode . c++-mode)))
	 ("folders"
                (mode . dired-mode))
	 ("LaTeX"
	        (mode . latex-mode))
	 ("MOOSE" (filename . ".i"))
	 )))
	
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "default")))

;; automatically refresh ibuffer
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; -------------------------------------------- python dev

;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    blacken                         ;; Black formatting on save
    magit                           ;; Git integration
    )
  )

(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally

;; Enable elpy
(elpy-enable)

;; Enable Black
(add-hook 'elpy-mode-hook #'blacken-mode)

;; Enable flycheck
(add-hook 'elpy-mode-hook #'flycheck-mode)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; activate EO-IO environment on start
(add-hook 'elpy-mode-hook '(lambda ()
			     (pyvenv-activate "~/mambaforge3/envs/EO-IO")))

;; run ipython instead of python with run-python
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")

;; display errors and warnings in pos-tip popups
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; make flycheck inline errors red
;; (set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style wave))

;; disable flycheck warnings and infos
(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-fringe-info nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-warning nil :underline nil)
(set-face-attribute 'flycheck-info nil :underline nil)

;; automatic python header
(auto-insert-mode 1)
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\py\\'" . "python-UZH-header")
     '(""
       "#!/usr/bin/env python3" \n
       "# -*- coding: utf-8 -*-" \n
       "\"\"\"" \n \n
       "@author: Adrien Wehrlé, University of Zurich, Switzerland" \n \n
       "\"\"\"" \n \n \n)))

;; disable indentation marks
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; prevent very long ipython buffers
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; set linum (line numbering) colors
(set-face-background 'linum "#222b35")
(set-face-foreground 'linum "#999999")

;; highlight current line
(require 'hlinum)
(hlinum-activate)
(set-face-foreground 'linum-highlight-face "#ffffff")
(set-face-background 'linum-highlight-face "#222b35")

;; customise fringe
(set-face-background 'fringe "#222b35")
(set-face-foreground 'flycheck-fringe-error "#FF0000")
(fringe-mode '(14 . 0))

(provide '.emacs)
;;;.emacs ends here
