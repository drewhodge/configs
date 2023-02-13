; -------------------------------------------------------------------------- ;
; ~/.emacs.d/init.el                                                         ;
; -------------------------------------------------------------------------- ;
; History:                                                                   ;
; 06.Feb.2023    Initial setup.                                              ;
;                Updated org-mode to 9.6.1 (via M-x list-packages)           ;
; 07.Feb.2023    Installed vterm.                                            ;
; 09.Feb.2023    Updates based on daviwill.com configuration.                ;
; 10.Feb.2023    Tested and refined config.                                  ;
; 11.Feb.2023    Installed and setup Denote to nmanage bibtex files.         ;
; 12.Feb.2023    Installed pdf-tools.                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 00 Table of contents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (occur "^;; [0-9]+")                            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 10 Name, email, int file location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Drew Hodge"
      user-mail-address "drew@drewhodge.org")
                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up repos and package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
			                   ("org" . "https://orgmode.org/elpa/")
                         ("gnu/elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Uncomment this to get a reading on packages that get loaded at startup
;;(setq use-package-verbose t)

(require 'use-package)
; Ensures that each package that is configured using ~use-package~ is installed
(setq use-package-always-ensure t)

; Bootstrap straight.el
(defvar bootstrap-version)                        
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; Add custom emacs lisp libraries to load-path
(push "~/.dotfiles/.emacs.d/lisp" load-path)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 30 Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No startup screen
(setq inhibit-startup-message t)

;; Show line numbers
(global-display-line-numbers-mode)

;; Tab settings
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Emacs app
(when window-system
  (set-frame-size (selected-frame) 180 60)
  (set-cursor-color "orange")
  (set-scroll-bar-mode nil)
  (tool-bar-mode -1))

;; Typefaces
;; Set default typeface 
(set-face-attribute 'default nil
                    ;; :font "JetBrains Mono"
                    :font "Fira Code"
                    :weight 'light
                    :height 130)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    ;; :font "JetBrains Mono"
                    :font "Fira Code"
                    :weight 'light
                    :height 130)

;; Modify theme's link face.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(link ((t (:foreground "#d3869b" :underline t :weight normal)))))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-env-load-string "..."))

; Show column numbers
(setq column-number-mode t)

(use-package all-the-icons
  :ensure t)

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 40 Global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define prefixed key bindings.
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dh/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

;; Define key bindings for org-mode commands that must be available everywhere
;; in Emacs.
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Set fill column.
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

; Prefer UTF 8, but don't override current encoding if specified
(prefer-coding-system 'utf-8-unix)

; Ovwerite/replace the active region just by typing text
(delete-selection-mode 1)

; IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Show tool tips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

; Dictionary
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
   '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 85)))

; Get overview of keybindings available based on the prefix keys
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Workspaces
(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Clean up whitespace automatically.
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Highlight colour strings.
(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode))

;; Match parentheses
(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#dba11a")
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Stateful keymaps
(use-package hydra
  :defer 1)

;; Put backup files in ~/.trash.
(setq backup-directory-alist '((".*" . "~/.Trash")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 50 Navigation (Ivy, Counsel, Swiper)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators.
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info.
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                     ; Return file path relative to project root or `default-directory' if project is nil.
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers.
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^.

(use-package flx  ;; Improves sort for fuzzy-matched results.
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package wgrep)

(use-package ivy-posframe
  :disabled
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(dh/leader-key-def
  "r"   '(ivy-resume :which-key "ivy resume")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "C-f" 'counsel-find-file
  "fr"  '(counsel-recentf :which-key "recent files")
  "fR"  '(revert-buffer :which-key "revert file")
  "fj"  '(counsel-file-jump :which-key "jump to file"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 60  File management (dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons-dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (setq dired-listing-switches "-la"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t))


(use-package diredc
  :ensure t)

;; Identify file types by colour.
(use-package dired-rainbow
  :defer 2
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 70 Writing (org, org-ref, Markdown, Pandoc, bibtex, PDF, LaTeX,
;;             org-static-blog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note org-mode and auctex have beenm installed using 'list-packages'.

;; Set margins
(defun dh/org-mode-visual-fill ()
  (setq visual-fill-column-width 95
        visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dh/org-mode-visual-fill))

;; Turn on indentation and auto-fill mode for Org files
(defun dh/org-mode-setup ()
  (org-indent-mode -1)
  ; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . dh/org-mode-setup)
  :config
  (setq org-ellipsis "  ..."
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-eshell))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))

  ;; Easy edit of Org documents when org-hide-emphasis-markers is turned on.
  (use-package org-appear
    :hook (org-mode . org-appear-mode))

  ;; ;; Heading bullets
  ;; (use-package org-bullets
  ;;   :hook (org-mode . org-bullets-mode)
  ;;   :config
  ;;   (setq org-bullets-bullet-list '("■" "■" "■" "■" "■" "■")))

  ;; Heading colurs
  (set-face-attribute 'org-level-1 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-2 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-3 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-4 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-5 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-6 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-7 nil :foreground "#6495ed")
  (set-face-attribute 'org-level-8 nil :foreground "#6495ed")

  ;; ;; Increase the size of various headings
  ;; (set-face-attribute 'org-document-title nil :font "JetBrains Mono" :weight 'medium :height 1.2)
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.05)
  ;;                 (org-level-5 . 1.05)
  ;;                 (org-level-6 . 1.05)
  ;;                 (org-level-7 . 1.05)
  ;;                 (org-level-8 . 1.05)))
  ;; (set-face-attribute (car face) nil :font "JetBrains Mono" :weight 'medium :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)

  ;; Src-block template.
  ;; Example: type <el then press Tab to expand the template.
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  ;; Workflow states
  (setq org-todo-keywords
        (quote
         ((sequence "TODO(@/!)" "IN-PROGRESS(@/!)" "PENDING(P@/!)" "DONE(D@/!)"))))

   (setq org-todo-keyword-faces
        (quote
         (("TODO" . "#cc241d")
          ("IN-PROGRESS" . "#d65d0e")
          ("PENDING" . "#d79921")
          ("DONE" . "#00cd00"))))

  ;; Configure common tags
   (setq org-tag-alist
         '((:startgroup)
          ; Put mutually exclusive tags here
         (:endgroup)
         ("@home" . ?H)
         ("@work" . ?W)
         ("batch" . ?b)
         ("followup" . ?f)))

  ;; Org journal setup
  (setq org-agenda-files (quote ("/Users/drew/org/todo.org")))
  (setq org-journal-dir "/Users/drew/org/journal/")
  (setq org-journal-date-format "%A, %d %B %Y")

  ;; Org capture (Deprecated date/weektree capture templates changed to ‘file+olp+datetree’.)
  (setq org-default-notes-file (concat org-directory "/Users/drew/org/notes/notes.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "/Users/drew/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("J" "Journal(= (org)" entry (file+olp+datetree "/Users/drew/org/journal/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("w" "Link from browser" entry (file "/Users/drew/org//inbox/inbox.org")
           "* %? |- (%:description) :BOOKMARK:\n:PROPERTIES:\n:CREATED: %U\n:Source: %:link\n:END:\n%i\n")
          ("s" "Selection from browser" entry (file "/Users/drew/org//inbox/inbox.org")
           "* %? :BOOKMARK:\n%(replace-regexp-in-string \"\n.*\" \"\" \"%i\")\n:PROPERTIES:\n:CREATED: %U\n:Source: %:link\n:END:\n%i\n"))))

;; Citations
(use-package ivy-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography '("/Users/drew/biblio/core.lib.bib"
                                         "/users/drew/biblio/testing.bib")
        bibtex-completion-library-path '("/Users/drew/biblio/bibtex-pdfs/")
	      bibtex-completion-notes-path "/Users/drew/biblio/notes/"
      	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	      bibtex-completion-additional-search-fields '(keywords)
	      bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	        (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	        (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath))))

(use-package org-ref
  :ensure t
  :config
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
	      bibtex-autokey-name-year-separator "-"
	      bibtex-autokey-year-title-separator "-"
	      bibtex-autokey-titleword-separator "-"
	      bibtex-autokey-titlewords 2
	      bibtex-autokey-titlewords-stretch 1
	      bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)
  (require 'org-ref-ivy)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function
        (lambda (_) (org-ref-citation-hydra/body))))

; Pandoc and Pandoc export
(use-package pandoc
  :ensure t)
(use-package ox-pandoc
  :ensure t)

; Markdown
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'")

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

;; Auctex, TeX, and LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Set up for an  org-static-blog
(use-package org-static-blog
  :ensure t
  :config
  (setq org-static-blog-publish-title "My Static Org Blog")
  (setq org-static-blog-publish-url "https://staticblog.org/")
  (setq org-static-blog-publish-directory "~/projects/blog/")
  (setq org-static-blog-posts-directory "~/projects/blog/posts/")
  (setq org-static-blog-drafts-directory "~/projects/blog/drafts/")
  (setq org-static-blog-enable-tags t)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)

  ;; This header is inserted into the <head> section of every page:
  ;;   (you will need to create the style sheet at
  ;;    ~/projects/blog/static/style.css
  ;;    and the favicon at
  ;;    ~/projects/blog/static/favicon.ico)
  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"John Dow\">
         <meta name=\"referrer\" content=\"no-referrer\">
         <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
         <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
         <link rel=\"icon\" href=\"static/favicon.ico\">")

  ;; This preamble is inserted at the beginning of the <body> of every page:
  ;;   This particular HTML creates a <div> with a simple linked headline
  (setq org-static-blog-page-preamble
        "<div class=\"header\">
         <a href=\"https://staticblog.org\">My Static Org Blog</a>
         </div>")

  ;; This postamble is inserted at the end of the <body> of every page:
  ;;   This particular HTML creates a <div> with a link to the archive page
  ;;   and a licensing stub.
  (setq org-static-blog-page-postamble
         "<div id=\"archive\">
          <a href=\"https://staticblog.org/archive.html\">Other posts</a>
          </div>
          <center>
            <a rel=\"license\"
               href=\"https://creativecommons.org/licenses/by-sa/3.0/\">
              <img alt=\"Creative Commons License\"
                   style=\"border-width:0\"
                   src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" />
            </a>
            <br />
            <span xmlns:dct=\"https://purl.org/dc/terms/\"
                  href=\"https://purl.org/dc/dcmitype/Text\"
                  property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by
            <a xmlns:cc=\"https://creativecommons.org/ns#\"
               href=\"https://bastibe.de\"
               property=\"cc:attributionName\"
               rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a
            <a rel=\"license\"
               href=\"https://creativecommons.org/licenses/by-sa/3.0/\">
               Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.
          </center>")

  ;; This HTML code is inserted into the index page between the preamble and
  ;;   the blog posts
  (setq org-static-blog-index-front-matter
        "<h1> Welcome to my blog </h1>\n"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 80 Notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deft
(use-package deft
  :commands (deft)
  :config (setq deft-directory "/Users/drew/org/roam"
                deft-recursive t
                deft-extensions '("md" "org")))

;; Denote
; [[https://protesilaos.com/emacs/denote#h:a09b70a2-ae0b-4855-ac14-1dddfc8e3241][Denote]]
; Denote aims to be a simple-to-use, focused-in-scope, and effective
; note-taking tool for Emacs.

(use-package denote
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "/Users/drew/denote"))
  (setq denote-known-keywords '("emacs" "philosophy" "theology"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Read the manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)

  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; Specify an org-capture template for a standard Denote note.
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; Define interactive function to create a Denote journal. The key
  ;; binding is defined below.
  (defun dh-denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal")))

  ;; Define key bindings.
  (let ((map global-map))
    (define-key map (kbd "C-c n j") #'dh-denote-journal) ; our custom command
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n t") #'denote-template)
    ;; Bbind the link-related commands to the `global-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-link-add-links)
    (define-key map (kbd "C-c n b") #'denote-link-backlinks)
    (define-key map (kbd "C-c n f f") #'denote-link-find-file)
    (define-key map (kbd "C-c n f b") #'denote-link-find-backlink)
    ;; `denote-rename-file' can work from any context, so it is
    ;; bound to the `global-map'.
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
    (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

  ;; Define Denote silos.
  ;; Silos are identified as directory-local values of the user option
  ;; denote-directory. This means that all Denote commands read from
  ;; the local value if they are invoked from that context. For example,
  ;; if ~/Videos/recordings is a silo and ~/Documents/notes is the
  ;; default/global value of denote-director, all Denote commands will
  ;; read the video’s path when called from there (e.g. by using Emacs’
  ;; dired); any other context reads the global value.
  (defvar my-denote-silo-directories
    `("/home/prot/Videos/recordings"
      "/home/prot/Documents/books"
      ;; You don't actually need to include the `denote-directory' here
      ;; if you use the regular commands in their global context.  I am
      ;; including it for completeness.
      ,denote-directory)
    "List of file paths pointing to my Denote silos.
     This is a list of strings.")

  (defvar my-denote-commands-for-silos
    '(denote
      denote-date
      denote-subdirectory
      denote-template
      denote-type)
    "List of Denote commands to call after selecting a silo.
     This is a list of symbols that specify the note-creating
     interactive functions that Denote provides.")

  (defun my-denote-pick-silo-then-command (silo command)
    "Select SILO and run Denote COMMAND in it.
     SILO is a file path from `my-denote-silo-directories', while
     COMMAND is one among `my-denote-commands-for-silos'."
    (interactive
     (list (completing-read "Select a silo: " my-denote-silo-directories nil t)
           (intern (completing-read
                    "Run command in silo: "
                    my-denote-commands-for-silos nil t))))
    (let ((denote-directory silo))
      (call-interactively command)))

  ;; Denote menu
  ;;
  (add-to-list 'load-path "/Users/drew/gitrepos/denote-menu/")
  (require 'denote-menu)

  ;; Add a text to bibliography using an org-template:
  ;; 1. Find interesting paper online
  ;; 2. Copy its BiBTeX entry to clipboard.
  ;; 3. In emacs, open an org-capture template that requests the BibTeX,
  ;;    note title (defaults to the title of the entry), and  tags.
  ;; 4. In the capture window (Denote note with BiBTeX  org source block,
  ;;    write notes; then close the capture window (C-c C-c).
  ;; From https://www.scss.tcd.ie/~sulimanm/posts/denote-bibliography.html
  ;; written by Mohamed Suliman.
  (defun namilus-denote-org-capture-biblio ()
    "Asks for a bibtex entry, title, and keywords; then creates a Denote
    note template with the following content:

    1. Bibtex entry in an org bibtex source block.
    2. The keyword \"biblio\" and the bibtex entry's sanitised key as
       part of the Denote file's tags.

   If the bibtex entry is empty or doesn't match the regexp, adds only the
   \"biblio\" keyword and any other keywords added by the user."
    (let* ((bibtex (namilus-denote-bibtex-prompt))
           (title (denote-title-prompt (namilus-denote-bibtex-title bibtex)))
           (keywords (append (denote-keywords-prompt) (namilus-denote-biblio-keywords bibtex)))
           (front-matter (denote--format-front-matter
                         title (denote--date nil 'org) keywords
                         (format-time-string denote-id-format nil) 'org)))
      (setq denote-last-path
            (denote--path title keywords
                          (file-name-as-directory (denote-directory))
                          (format-time-string denote-id-format) 'org))
      (denote--keywords-add-to-history keywords)
      (concat front-matter (namilus-denote-bibtex-org-block bibtex))))

  ;; Functions that read input from the user, sanitise the BiBTeX, extract
  ;; the key, generate the file tags, and create the org source block.
  (defun namilus-denote-bibtex-prompt (&optional default-bibtex)
    "Ask the user for a bibtex entry. Returns the sanitised
     version. See `namilus-denote-sanitise-bibtex' for details."
    (let* ((def default-bibtex)
           (format (if (and def (not (string-empty-p def)))
                       (format "Bibtex [%s]: " def)
                     "Bibtex: "))
           (sanitised-bibtex (namilus-denote-sanitise-bibtex (read-string format nil nil def))))
      (if sanitised-bibtex
          sanitised-bibtex
        (error "Invalid BiBTeX"))))

  (defun namilus-denote-sanitise-bibtex (bibtex)
    "Returns a santised version of BIBTEX. Sanitisation entails remove
     all non alpha-numeric characters from the bibtex key, and
     returning this updated bibtex entry. If BIBTEX is not a valid
     bibtex entry, returns nil."
    (when (string-match "@.*{\\(.*\\)," bibtex)
      (let* ((key (match-string-no-properties 1 bibtex))
             (sanitised-key (replace-regexp-in-string "[^A-Za-z0-9]" "" key)))
        (replace-regexp-in-string key sanitised-key bibtex))))

  (defun namilus-denote-bibtex-key (bibtex)
    "Returns the bibtex key from BIBTEX."
    (when (string-match "@.*{\\(.*\\)," bibtex)
      (match-string-no-properties 1 bibtex)))

  (defun namilus-denote-bibtex-title (bibtex)
    "Returns the bibtex title from BIBTEX."
    (when (string-match "\\s *title\\s *=\\s *{\\(.*\\)}," bibtex)
      (match-string-no-properties 1 bibtex)))

  (defun namilus-denote-biblio-keywords (bibtex)
    "Returns a list of strings \"biblio\" and the key from the BIBTEX
     entry, otherwise, just returns a list consisting of the string
     \"biblio\"."
    (let ((bibtex-key (namilus-denote-bibtex-key bibtex)))
      (if bibtex-key
          `("biblio" ,bibtex-key)
        '("biblio"))))

  (defun namilus-denote-bibtex-org-block (bibtex)
    "Returns a string representing an org `bibtex' source block
     encompassing BIBTEX, a string of a bibtex entry."
    (concat "#+begin_src bibtex\n" bibtex "\n#+end_src"))

  ;; Org-capture template.
  (add-to-list 'org-capture-templates
               '("B" "Bibliography (with Denote) BibTeX" plain
                 (file denote-last-path)
                 #'namilus-denote-org-capture-biblio
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured nil))

  ;; Mark all ’biblio’ tagged entries in denote-directory in Dired and
  ;; extract their BiBTeX entries into a single .bib file.
  (defun namilus-denote-biblio-read-bibtex (file)
    "Reads the bibtex entry from a given Denote FILE. Does so by
     searching for a org bibtex source block and returns the contents
     therein."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((contents (buffer-string)))
        (when (string-match "#\\+begin_src.*bibtex\\(\\(.*\n\\)*\\)#\\+end_src" contents)
          (match-string-no-properties 1 contents)))))

  (defun namilus-denote-generate-bibliography (denote-biblio-files bibliography-file)
    "Writes the org bibtex source blocks located in each of
     DENOTE-BIBLIO-FILES to BIBLIOGRAPHY-FILE."
    (with-temp-file bibliography-file
      (dolist (file denote-biblio-files)
        (let ((bibtex (namilus-denote-biblio-read-bibtex file)))
          (if bibtex
              (insert bibtex))))))

  ;; Interactive function: takes the marked Dired files, asks for a .bib file
  ;; in which to save the BiBTeX entries. This method can also generate .bib
  ;; files for specific bibliography items, e.g those tagged with a specific keyword.
  (defun namilus-denote-bibliography-file-prompt (&optional default-bibliography-file)
    "Ask the user for a bibliography file."
    (let* ((def default-bibliography-file)
           (format (if (and def (not (string-empty-p def)))
                       (format "Bibliography file [%s]: " def)
                     "Bibliography file: ")))
      (expand-file-name (read-file-name format nil def))))

  (defun namilus-denote-dired-generate-bibliography-from-marked ()
    (interactive)
    (namilus-denote-generate-bibliography (dired-get-marked-files nil nil nil t)
                                          (namilus-denote-bibliography-file-prompt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 90 Calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

(setq calendar-intermonth-header
      (propertize "Wk"
                  'font-lock-face 'font-lock-keyword-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 100 Shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vterm
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 110 Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lisp
(use-package sly
  ; :disabled
  :mode "\\.lisp\\'")

(use-package slime
  ; :disabled
  :mode "\\.lisp\\'")

; Clojure
(use-package cider
  :mode "\\.clj[sc]?\\'")
  ; :config
  ;(evil-collection-cider-setup))

; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :defer t)

; HTML
(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

; Compilation
(use-package compile
  :straight nil
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
      (add-hook 'after-save-hook #'recompile nil t)))

;; Ediff -- keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 120 Version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
(use-package magit
  :bind ("C-M-;" . magit-status) ; Or :bind (("C-x g" . magit-status)))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(use-package git-gutter
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 130 Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Google queries -- M-x google
(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

;; Unfill paragraphs and regions
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; End init.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((TeX-master . main_document\.tex))))
