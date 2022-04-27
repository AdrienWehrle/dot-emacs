(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

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

;; -------------------------------------------- org

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () 
			   (org-bullets-mode 1)))

;; change default org-level fontsizes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t 
		 (:inherit outline-1 
			   :height 1.5)))) 
 '(org-level-2 ((t 
		 (:inherit outline-2 
			   :height 1.0)))))

(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "PROCESSING" "WAITING" "CANCELED" "DONE")))

(setq org-todo-keyword-faces '(("IN-PROGRESS" . "orange") 
			       ("PROCESSING" . "orange") 
			       ("WAITING" . "orange") 
			       ("CANCELED" . "yellow") ))

(setq inhibit-startup-screen t)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-log-done 'time)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote ("," "	" ";"))) 
 '(custom-safe-themes (quote ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
			      default))) 
 '(package-selected-packages (quote (loccur org-cliplink eglot julia-repl julia-mode markdown-mode
					    vc-msg json-mode yaml-mode helm-ag org-ref-prettify
					    org-ref ibuffer-vc csv-mode lispy elisp-format
					    spacemacs-theme helm-bibtex dumb-jump tree-mode
					    tree-sitter vscode-dark-plus-theme code-cells cdlatex
					    lean-mode yasnippet-classic-snippets yasnippet-snippets
					    hlinum autothemer display-theme hydra magit eink-theme
					    flycheck-pos-tip zenburn-theme use-package org-bullets
					    python-cell pyenv-mode material-theme flycheck
					    exec-path-from-shell elpy ein
					    color-theme-sanityinc-tomorrow blacken better-defaults
					    anaconda-mode))) 
 '(safe-local-variable-values (quote ((eval when 
					    (require (quote rainbow-mode) nil t) 
					    (rainbow-mode 1))))))


;; spell checking
(setq ispell-program-name (executable-find "hunspell") ispell-dictionary "en_US")
(use-package 
  flyspell-correct-ivy 
  :ensure t 
  :demand t)

;; enable flyspell
(add-hook 'org-mode-hook #'turn-on-flyspell)

;; correct word
(eval-after-load 'org '(define-key org-mode-map (kbd "C-c c") #'flyspell-correct-word-before-point))

;; insert org-mode link with title of page found in URL
(eval-after-load 'org '(define-key org-mode-map (kbd "C-c C-i") 'org-cliplink))

;; resolve Windmove conflicts (like org state looping)
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "C-<right>")  'org-shiftright)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "S-<right>")  'windmove-right)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "C-<left>")  'org-shiftleft)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "S-<left>")  'windmove-left)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "C-<up>")  'org-shiftup)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "S-<up>")  'windmove-up)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "C-<down>")  'org-shiftdown)))
(add-hook 'org-mode-hook (lambda () 
			   (local-set-key (kbd "S-<down>")  'windmove-down)))

;; -------------------------------------------- yaml

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; -------------------------------------------- json

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
;; (add-hook 'json-mode-hook 'json-mode-beautify)

;; -------------------------------------------- tex

(require 'tex-site)
;; flychecks needs ispell, aspell, hunspell
(add-hook 'LaTeX-mode-hook '(lambda () 
			      (reftex-mode) 
			      (flyspell-mode)))
(setq reftex-plug-into-AUCTeX t)
;; (setq reftex-load-hook (quote (imenu-add-menubar-index)))
;; (setq reftex-mode-hook (quote (imenu-add-menubar-index)))
(setq TeX-view-program-list (quote ("Okular" ("output-pdf" "okular"))))


;; ;; cdlatex
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ; with AUCTeX LaTeX mode

;; break line after 80 characters
(add-hook 'LaTeX-mode-hook #'auto-fill-mode)
(setq-default fill-column 70)

(add-hook 'LaTeX-mode-hook #'turn-on-flyspell)

;;(setq TeX-source-correlate-mode t)
(setq TeX-view-program-list (quote (("okular" ("okular" (mode-io-correlate " -p %(outpage)") "
%o")))))
(setq TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") 
					 (output-dvi "xdvi") 
					 (output-pdf "okular") 
					 (output-html "xdg-open"))))

(setq ring-bell-function 'ignore)

(add-hook 'flyspell-mode-hook #'flyspell-buffer)

;; correct word
(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "C-c c")
			   #'flyspell-correct-word-before-point))

;; get synonyms from Thesaurus.com (get-synonyms)
(add-to-list 'load-path "~/.emacs.d/custom-modes/thesaurus.el")
(require 'thesaurus)
(eval-after-load 'latex '(define-key LaTeX-mode-map (kbd "C-c t")  #'get-synonyms))

;; -------------------------------------------- useful global settings

;; restore the last saved desktop on startup
(desktop-save-mode 1)

;; remember recent files
(recentf-mode 1)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 30)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Save minibuffer prompts (M-n and M-p to access them)
(setq history-length 50)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; hide menu-bar and tool-bar forever
(menu-bar-mode -1)
(tool-bar-mode -1)

;; show parentheses
(show-paren-mode 1)

;;;  format elisp code
;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;; 				  (add-hook 'after-save-hook 'elisp-format-buffer)))

;; always reload files if they changed on disk
(setq global-auto-revert-mode t)

;; make Pg-Up Pg-Down return to the same point
(setq scroll-preserve-screen-position t)

;; start emacs as server
(server-start)

;; simple window traveling
(windmove-default-keybindings)

;; Default mode for unknown files
(setq default-major-mode 'text-mode)

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Jump multiple lines at once (efficient travelling)
(global-set-key (kbd "C-d") 
		(lambda () 
		  (interactive) 
		  (previous-line 10)))
(global-set-key (kbd "C-f") 
		(lambda () 
		  (interactive) 
		  (next-line 10)))

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

;; revert but keep undo history
(defun revert-buffer-keep-undo 
    (&rest 
     -)
  "Revert buffer but keep undo history." 
  (interactive) 
  (let ((inhibit-read-only t)) 
    (erase-buffer) 
    (insert-file-contents (buffer-file-name)) 
    (set-visited-file-modtime (visited-file-modtime)) 
    (set-buffer-modified-p nil)))
(setq revert-buffer-function 'revert-buffer-keep-undo)

;; some upper limits on sizes
(setq max-lisp-eval-depth '40000)
(setq max-specpdl-size '100000)
(setq undo-limit 40000 undo-outer-limit 8000000 undo-strong-limit 100000)

;; simple window switch
(windmove-default-keybindings)

;; auto bullet mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () 
			   (org-bullets-mode 1)))

;; enable tree-sitter
;; (global-tree-sitter-mode)

;; enable dumb jump (M-.)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; show current match and number of matches
(global-anzu-mode +1)
;; let spaceline take car of matches
(setq anzu-cons-mode-line-p nil)

;; activate csv-mode for csv files
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;; facilite move to beginning/end of buffer
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)

;; give a DOI, get a bibtex entry (complementary to org-ref)
(defun get-bibtex-from-doi (doi) 
  "Get a BibTeX entry from the DOI" 
  (interactive "MDOI: ") 
  (let ((url-mime-accept-string "text/bibliography;style=bibtex")) 
    (with-current-buffer (url-retrieve-synchronously (format "http://dx.doi.org/%s"
							     (replace-regexp-in-string
							      "http://dx.doi.org/" "" doi))) 
      (switch-to-buffer (current-buffer)) 
      (goto-char (point-max)) 
      (setq bibtex-entry 
	    (buffer-substring 
	     (string-match "@" (buffer-string)) 
	     (point))) 
      (kill-buffer (current-buffer)))) 
  (insert (decode-coding-string bibtex-entry 'utf-8)) 
  (bibtex-fill-entry))
(global-set-key (kbd "C-c b") 'get-bibtex-from-doi)

(use-package 
  org-ref 
  :custom (reftex-default-bibliography '("~/org/references.bib")) 
  (org-ref-bibliography-notes "~/org/notes.org") 
  (org-ref-default-bibliography '("~/org/references.bib")) 
  (org-ref-pdf-directory "~/org/books") 
  )

;; write bibtex entry in bib from DOI
(global-set-key (kbd "C-c u") 'org-ref-url-html-to-bibtex)

;; run a shell command quickly
(global-set-key (kbd "C-c s") 'shell-command)

;; facilitate string replacement
(global-set-key (kbd "C-x r") 'replace-string)

;; git blame
(defun vc-msg-hook-setup (vcs-type commit-info)
  ;; copy commit id to clipboard
  (message (format "%s\n%s\n%s\n%s" (plist-get commit-info 
					       :id) 
		   (plist-get commit-info 
			      :author) 
		   (plist-get commit-info 
			      :author-time) 
		   (plist-get commit-info 
			      :author-summary))))
(add-hook 'vc-msg-hook 'vc-msg-hook-setup)

;; show file VC historic
(global-set-key (kbd "C-x c") 'magit-log-buffer-file)

;; list pattern occurences in current buffer and go
(use-package 
  loccur 
  :bind ((("C-q" .  loccur-current))))

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; use helm mainly for pattern search
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)

;; find file or buffer matching pattern and open
(setq helm-locate-command "mlocate %s -wAe --regex %s")
(setq helm-find-files-sort-directories t)
(setq helm-semantic-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(global-set-key (kbd "M-X")  'helm-for-files)

;; show kill ring history
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; use helm mainly for pattern search
(add-to-list 'load-path "~/.emacs.d/color-moccur.el")
(require 'color-moccur)

;; -------------------------------------------- git push

;; easy git add commit and push
(defun push-all (comment) 
  (interactive "Mmessage:") 
  (save-some-buffers t)			; save all buffer
  (shell-command (format "git add -A; git commit -a -m \" %s\"; git push &" comment)))

(global-set-key (kbd "C-c g") 'push-all)

;; -------------------------------------------- MOOSE

;; syntax highlighting for MOOSE input and test files
;; https://github.com/dylanjm/emacs-moose-mode
(add-to-list 'load-path "~/.emacs.d/custom-modes/emacs-moose-mode")
(require 'moose-mode)

;; -------------------------------------------- modify buffers

;; Originally from stevey, adapted to support moving to a new directory.
(defun rename-file-and-buffer (new-name) 
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive (progn (if (not (buffer-file-name)) 
			  (error 
			   "Buffer '%s' is not visiting a file!"
			   (buffer-name)))
		      ;; Disable ido auto merge since it too frequently jumps back to the original
		      ;; file name if you pause while typing. Reenable with C-z C-z in the prompt.
		      (let ((ido-auto-merge-work-directories-length -1)) 
			(list (read-file-name (format "Rename %s to: " (file-name-nondirectory
									(buffer-file-name)))))))) 
  (if (equal new-name "") 
      (error 
       "Aborted rename")) 
  (setq new-name (if (file-directory-p new-name) 
		     (expand-file-name (file-name-nondirectory (buffer-file-name)) new-name) 
		   (expand-file-name new-name)))
  ;; Only rename if the file was saved before. Update the
  ;; buffer name and visited file in all cases.
  (if (file-exists-p (buffer-file-name)) 
      (rename-file (buffer-file-name) new-name 1)) 
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name) 
    (if was-modified (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))) 
  (setq default-directory (file-name-directory new-name)) 
  (message "Renamed to %s." new-name))
(global-set-key (kbd "C-c r")  #'rename-file-and-buffer)

(defun delete-file-and-buffer () 
  "Kill the current buffer and deletes the file it is visiting." 
  (interactive) 
  (let ((filename (buffer-file-name))) 
    (when filename (if (vc-backend filename) 
		       (vc-delete-file filename) 
		     (progn (delete-file filename) 
			    (message "Deleted file %s" filename) 
			    (kill-buffer))))))
(global-set-key (kbd "C-c d")  #'delete-file-and-buffer)

;; -------------------------------------------- ibuffer

(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Use Ibuffer for Buffer List

(setq ibuffer-saved-filter-groups '(("default" ("elisp" (mode . emacs-lisp-mode)) 
				     ("org" (or (mode . org-mode) 
						(filename . "OrgMode"))) 
				     ("python" (or (mode . python-mode))) 
				     ("C++" (or (mode . c-mode) 
						(mode . c++-mode))) 
				     ("folders" (mode . dired-mode)) 
				     ("tex" (mode . latex-mode)) 
				     ("csv" (mode . csv-mode)) 
				     ("txt" (mode . text-mode)) 
				     ("bash" (mode . sh-mode)) 
				     ("yml" (mode . yaml-mode)) 
				     ("magit" (or (mode . magit-status-mode) 
						  (mode . magit-diff-mode) 
						  (mode . magit-revision-mode))) 
				     ("MOOSE" (mode . moose-mode)))))

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook '(lambda () 
				(ibuffer-switch-to-saved-filter-groups "default")))

;; automatically refresh ibuffer
(add-hook 'ibuffer-mode-hook (lambda () 
			       (ibuffer-auto-mode 1)))

;; human readable size column
(defun ajv/human-readable-file-sizes-to-bytes (string) 
  "Convert a human-readable file size into bytes." 
  (interactive) 
  (cond ((string-suffix-p "G" string t) 
	 (* 1000000000 (string-to-number (substring string 0 (- (length string) 1))))) 
	((string-suffix-p "M" string t) 
	 (* 1000000 (string-to-number (substring string 0 (- (length string) 1))))) 
	((string-suffix-p "K" string t) 
	 (* 1000 (string-to-number (substring string 0 (- (length string) 1))))) 
	(t (string-to-number (substring string 0 (- (length string) 1))))))

(defun ajv/bytes-to-human-readable-file-sizes (bytes) 
  "Convert number of bytes to human-readable file size." 
  (interactive) 
  (cond ((> bytes 1000000000) 
	 (format "%10.1fG" (/ bytes 1000000000.0))) 
	((> bytes 100000000) 
	 (format "%10.0fM" (/ bytes 1000000.0))) 
	((> bytes 1000000) 
	 (format "%10.1fM" (/ bytes 1000000.0))) 
	((> bytes 100000) 
	 (format "%10.0fk" (/ bytes 1000.0))) 
	((> bytes 1000) 
	 (format "%10.1fk" (/ bytes 1000.0))) 
	(t (format "%10d" bytes))))

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h 
  (:name "Size" 
	 :inline t 
	 :summarizer (lambda (column-strings) 
		       (let ((total 0)) 
			 (dolist (string column-strings) 
			   (setq total
				 ;; like, ewww ...
				 (+ (float (ajv/human-readable-file-sizes-to-bytes string)) total))) 
			 (ajv/bytes-to-human-readable-file-sizes total))) ;; :summarizer nil
	 ) 
  (ajv/bytes-to-human-readable-file-sizes (buffer-size)))

;; Modify the default ibuffer-formats
(setq ibuffer-formats '((mark modified read-only locked " " (name 20 20
								  :left 
								  :elide) " " (size-h 11 -1 
										      :right) " "
										      (mode 16 16
											    :left 
											    :elide)
										      " "
										      filename-and-process) 
			(mark " " (name 16 -1) " " filename)))

(defun ibuffer-advance-motion (direction) 
  (forward-line direction) 
  (beginning-of-line) 
  (if (not (get-text-property (point) 'ibuffer-filter-group-name)) t (ibuffer-skip-properties
								      '(ibuffer-filter-group-name)
								      direction) nil))

;; Improve line movement in ibuffer
(defun ibuffer-previous-line 
    (&optional 
     arg)
  "Move backwards ARG lines, wrapping around the list if necessary." 
  (interactive "P") 
  (or arg 
      (setq arg 1)) 
  (let (err1 err2) 
    (while (> arg 0) 
      (cl-decf arg) 
      (setq err1 (ibuffer-advance-motion -1) err2 (if (not (get-text-property (point)
									      'ibuffer-title)) t
						    (goto-char (point-max)) 
						    (beginning-of-line) 
						    (ibuffer-skip-properties '(ibuffer-summary
									       ibuffer-filter-group-name)
									     -1) nil))) 
    (and err1 
	 err2)))

(defun ibuffer-next-line 
    (&optional 
     arg)
  "Move forward ARG lines, wrapping around the list if necessary." 
  (interactive "P") 
  (or arg 
      (setq arg 1)) 
  (let (err1 err2) 
    (while (> arg 0) 
      (cl-decf arg) 
      (setq err1 (ibuffer-advance-motion 1) err2 (if (not (get-text-property (point)
									     'ibuffer-summary)) t
						   (goto-char (point-min)) 
						   (beginning-of-line) 
						   (ibuffer-skip-properties '(ibuffer-summary
									      ibuffer-filter-group-name
									      ibuffer-title) 1)
						   nil))) 
    (and err1 
	 err2)))

;; Improve header movement in ibuffer
(defun ibuffer-next-header () 
  (interactive) 
  (while (ibuffer-next-line)))

(defun ibuffer-previous-header () 
  (interactive) 
  (while (ibuffer-previous-line)))

(define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)
(define-key ibuffer-mode-map (kbd "<right>") 'ibuffer-next-header)
(define-key ibuffer-mode-map (kbd "<left>") 'ibuffer-previous-header)

;; -------------------------------------------- julia

(require 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(add-hook 'julia-mode-hook 'eglot-jl-init)
(add-hook 'julia-mode-hook 'eglot-ensure)

(add-hook 'julia-mode-hook '(lambda () 
			      (local-set-key (kbd "C-d") 'julia-repl-send-line) 
			      (local-set-key (kbd "C-c C-c") 'julia-repl-send-buffer)))

;; -------------------------------------------- python

;; myPackages contains a list of package names
(defvar myPackages 
  '(better-defaults ;; Set up some better Emacs defaults
    elpy	    ;; Emacs Lisp Python Environment
    flycheck	    ;; On the fly syntax checking
    blacken	    ;; Black formatting on save
    magit	    ;; Git integration
    ))

(setq inhibit-startup-message t) ;; Hide the startup message
(global-linum-mode t)		 ;; Enable line numbers globally

;; Enable elpy
(elpy-enable)

;; Enable Black
(add-hook 'elpy-mode-hook #'blacken-mode)

;; Enable flycheck
(add-hook 'elpy-mode-hook #'flycheck-mode)

;; Enable Flycheck
(when 
    (require 'flycheck nil t) 
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) 
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; activate EO-IO environment on start
(add-hook 'elpy-mode-hook '(lambda () 
			     (pyvenv-activate "~/mambaforge3/envs/EO-IO")))

;; run ipython instead of python with run-python
(setq python-shell-interpreter "ipython3" python-shell-interpreter-args "-i --simple-prompt")

;; display errors and warnings in pos-tip popups
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;; make flycheck inline errors red
;; (set-face-attribute 'flycheck-error nil :underline '(:color "red2" :style wave))

;; disable flycheck warnings and infos
(set-face-attribute 'flycheck-fringe-warning nil 
		    :foreground (face-attribute 'fringe 
						:background ))
(set-face-attribute 'flycheck-fringe-info nil 
		    :foreground (face-attribute 'fringe 
						:background ))
(set-face-attribute 'flycheck-warning nil 
		    :underline nil)
(set-face-attribute 'flycheck-info nil 
		    :underline nil)

;; automatic python header
(auto-insert-mode 1)
(eval-after-load 'autoinsert '(define-auto-insert '("\\.\\py\\'" . "python-UZH-header") 
				'("" "#!/usr/bin/env python3" \n "# -*- coding: utf-8 -*-" \n
				  "\"\"\"" \n \n
				  "@author: Adrien Wehrlé, University of Zurich, Switzerland" \n \n
				  "\"\"\"" \n \n \n)))

;; disable indentation marks
(add-hook 'elpy-mode-hook (lambda () 
			    (highlight-indentation-mode -1)))

;; prevent very long ipython buffers
(add-hook 'inferior-python-mode-hook #'my-inferior-python-mode-hook)

(defun my-inferior-python-mode-hook () 
  "Custom `inferior-python-mode' behaviours."
  (setq-local comint-buffer-maximum-size 2000) ;; maximum lines
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer nil 
	    :local))

;; handy keybinding for matching history
(add-hook 'inferior-python-mode-hook (lambda () 
				       (local-set-key (kbd "C-c <C-up>")
						      'comint-previous-matching-input-from-input)))
(add-hook 'inferior-python-mode-hook (lambda () 
				       (local-set-key (kbd "C-c <C-down>")
						      'comint-next-matching-input-from-input)))

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
