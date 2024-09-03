; -------------------------------------------------------------------------- ;
; ~/.emacs.d/init.el                                                         ;
; -------------------------------------------------------------------------- ;
; History:                                                                   ;
; 02.Sep.2024    Initial setup.                                              ;
; -------------------------------------------------------------------------- ;


;; Table of contents
(occur "^;;; ")

;; ------------------------------------------------------------------------- ;
;;; Name, email, int file location
;; ------------------------------------------------------------------------- ;
(setq user-full-name "Drew Hodge"
      user-mail-address "drew@drewhodge.org")

;; ------------------------------------------------------------------------- ;
;;; Package management
;; ------------------------------------------------------------------------- ;
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
; (setq use-package-verbose t)

(require 'use-package)

; Ensures that each package that is configured using ~use-package~ is installed
(setq use-package-always-ensure t)

;; Bootstrap straight.el
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
(push "~/.site-lisp/lisp" load-path)

;; ------------------------------------------------------------------------- ;
;;; Appearance
;; ------------------------------------------------------------------------- ;
;; No startup screen
(setq inhibit-startup-message t)

;; Show line numbers
(global-display-line-numbers-mode)

;; Tab settings
(setq-default tab-width 2)

;; Use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; Themes
;; Grubox theme
;; (use-package gruvbox-theme
;;   :config
;;   ;; Load the theme of choice:
;;   ;(load-theme 'gruvbox-dark-soft :no-confirm)
;;   (load-theme 'gruvbox-dark-medium :no-confirm)
;;   ;(load-theme 'gruvbox-dark-hard :no-confirm)
;; )

;; Ef-themes
;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-elea-dark ef-light))

  (setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 fixed-pitch regular 1.2)
        (1 fixed-pitch regular 1.2)
        (2 fixed-pitch regular 1.2)
        (3 fixed-pitch regular 1.2)
        (4 fixed-pitch regular 1.2)
        (5 fixed-pitch regular 1.2)
        (6 fixed-pitch regular 1.2)
        (7 fixed-pitch regular 1.2)
        (t fixed-pitch regular 1.2)))

  ;; They are nil by default...
  (setq ef-themes-mixed-fonts nil
        ef-themes-variable-pitch-ui nil)

  ;; Read the doc string or manual for this one.  The symbols can be
  ;; combined in any order.
  (setq ef-themes-region '(intense no-extend neutral))

  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)

  ;; Load the theme of choice:
  (load-theme 'ef-autumn :no-confirm)

  ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
  ;(ef-themes-select 'ef-autumn)

  ;; The themes we provide are recorded in the `ef-themes-dark-themes',
  ;; `ef-themes-light-themes'.

  ;; We also provide these commands, but do not assign them to any key:
  ;;
  ;; - `ef-themes-toggle'
  ;; - `ef-themes-select'
  ;; - `ef-themes-select-dark'
  ;; - `ef-themes-select-light'
  ;; - `ef-themes-load-random'
  ;; - `ef-themes-preview-colors'
  ;; - `ef-themes-preview-colors-current'
)

;; Modus themes
;; (use-package modus-themes
;;   :config
;;   ;; Add all your customizations prior to loading the themes
;;   ;(setq modus-themes-italic-constructs t
;;   ;      modus-themes-bold-constructs nil)

;;   ;; Maybe define some palette overrides, such as by using our presets
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-faint)
;;   (setq modus-themes-common-palette-overrides
;;         modus-themes-preset-overrides-warmer)

;;   (setq modus-themes-region '(bg-only))

;;   (setq modus-themes-completion 'minimal)
;;   ;(setq modus-themes-completion 'opinionated)


;;   ;; Faint yellow comments and a different shade of green for strings
;;   (setq modus-themes-common-palette-overrides
;;         '((comment green-faint)
;;           (string yellow-warmer)))

;;   ;; Disable all other themes to avoid awkward blending:
;;   (mapc #'disable-theme custom-enabled-themes)

;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-vivendi t)

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Treemacs icons
(use-package treemacs-all-the-icons
  ;:ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

;; Emacs app
(use-package emacs
  :config
  (set-frame-size (selected-frame) 180 60)
  (set-cursor-color "orange")
)

;; Typefaces
;; Set default typeface
(set-face-attribute 'default nil
                    :font "Iosevka Comfy"
                    ;:font "JetBrains Mono"
                    :weight 'light
                    :height 130)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Comfy"
                    ;:font "JetBrains Mono"
                    :weight 'light
                    :height 130)

;; Modeline
(use-package doom-modeline
  ;:ensure t
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

;; Spacious-padding: increase spacing/padding in windows and frames.
(use-package spacious-padding
  :config

;; These is the default value, but I keep it here for visiibility.
(setq spacious-padding-widths
      '( :internal-border-width 5
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        '(:mode-line-active error))

  (spacious-padding-mode 1)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode)
  )

;; ------------------------------------------------------------------------- ;
;;; Global settings
;; ------------------------------------------------------------------------- ;

;; Meow-edit (modal editing)
(use-package meow
  :config

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (meow-setup)
  (meow-global-mode 1)
  )

;; Sow empty lines in a buffer
(setq-default indicate-empty-lines t)

;; Toggle input method -- Greek character input and composition (postfix).
(setq default-input-method "greek-ibycus4")

;; Spelling
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_GB")
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)

;; Turn off sounds and enable 'visual bell'.
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Define prefixed key bindings.
(use-package general
  :config

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

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Set fill column.
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Prefer UTF 8, but don't override current encoding if specified
(prefer-coding-system 'utf-8-unix)

;; Ovwerite/replace the active region just by typing text
(delete-selection-mode 1)

;; Use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Show tool tips in echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Dictionary
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
   '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 85)))

;; Get overview of keybindings available based on the prefix keys
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

;; Remeber recent files
(recentf-mode 1)

;; Remember minibuffer history
(setq history-length 25)
(savehist-mode 1)

;; Change location of 'custom file'
;; Moves customiztion variable to a file other than init.el and loads the file.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Put backup files in ~/.trash.
(setq backup-directory-alist '((".*" . "~/.Trash")))

;; Enter debug mode when encountering an error.
(setq set-debug-on-error t)

;; Use Prot's Pulsar package (show and highlight current line)
;; Make available the elisp files in the following directory:.
(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ; OR use the local mode for select mode hooks

  (dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'pulsar-mode))

  ;; pulsar does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  ;;
  ;; Prot uses C-x l for `pulsar-pulse-line' and C-x L for
  ;; `pulsar-highlight-line'.
  ;;
  ;; You can replace `pulsar-highlight-line' with the command
  ;; `pulsar-highlight-dwim'.
  (let ((map global-map))
    (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
    (define-key map (kbd "C-c h h") #'pulsar-highlight-line))
)

;; Use Prot's Logos package (for easier paging)
(use-package logos
  :config
  ; To use outlines instead of page breaks (the ^L), set the following variable
  ; to 't'. To insert the '^L' page break delimiter, type 'C-q C-l'.
  (setq logos-outlines-are-pages nil)

  ; Default values for page breaks using outlines:
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . "^;;;+ ")
          (org-mode . "^\\*+ +")
          (markdown-mode . "^\\#+ +")))

  ; The following settings apply when `logos-focus-mode' is enabled.  Their
  ; values are buffer-local.
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti nil)

  ; Check the manual for `logos-focus-mode-hook'--it extends `logos-focus-mode'.
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode))

  ; Consider adding keys to `logos-focus-mode-map'.  They will take
  ; effect when `logos-focus-mode' is enabled.
)


