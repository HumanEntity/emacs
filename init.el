(defvar rune/use-ivy nil "Config option to use ivy (t) or vertico (nil)")
(defvar rune/use-company nil "Config option to use company (t) or corfu (nil)")

(setq package-archives
	'(("melpa" . "https://melpa.org/packages/")
	  ("elpa" . "https://elpa.gnu.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	  ("org" . "https://orgmode.org/elpa/")))

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

(setq straight-disable-native-compile nil)
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; (package-initialize)

;; (setq use-package-always-ensure t)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-defer t)
(setq package-native-compile t)
(setq comp-deferred-compilation nil)

;; Using garbage magic hack.
(use-package gcmh
  :config
  (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defun rune/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook 'rune/display-startup-time)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :defer nil)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq create-lockfiles nil)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-char-mode
                  circe-query-mode
                  term-mode
                  sauron-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil-collection
  :defer nil
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil
  :defer t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;;  :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Little code to get mode indication in evil

(evil-mode nil)
(evil-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :defer 0
  :config
  ;; (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" (if rune/use-ivy '(counsel-load-theme :which-key "choose theme") '(consult-theme :which-key "choose theme"))
	"tf" '(treemacs :which "toggle file explorer"))
  (when rune/use-ivy
      (rune/leader-keys
	"f" '(:ignore t :which-key "find")
	"ff" '(counsel-find-file :which-key "find file"))))
  ;; (rune/leader-keys
  ;;   "o"  '(:ignore t :which-key "org-mode")
  ;;   "oa" '(org-agenda :which-key "Org Agenda")
  ;;   "oc" '(org-capture :which-key "Org Capture"))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.05 )
  (which-key-mode))

(add-to-list 'custom-theme-load-path "~/.config/emacs.gnu/themes/")
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t))

(use-package catppuccin-theme)

(load-theme 'catppuccin t)

(defvar rune/default-font-size 120)
(defvar rune/default-variable-font-size 120)
(defvar rune/frame-transparency '(90 . 90))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq inhibit-startup-message t)
(setq use-dialog-box nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(auto-revert-mode 1)
(set-fringe-mode 10)
(xterm-mouse-mode 1)
(column-number-mode)
(global-auto-revert-mode 1)

(global-display-line-numbers-mode 1)

;; (dolist (mode '(prog-mode text-mode))
;;   (lambda () (display-line-numbers-mode 1)))

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                eww-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun rune/configure-font-faces ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha rune/frame-transparency)
  (add-to-list 'default-frame-alist `(alpha . ,rune/frame-transparency))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'light :height rune/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'light :height  rune/default-font-size)

  ;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.3)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light))


(if (daemonp)
    (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (setq doom-modeline-icon t)
		  (with-selected-frame frame
		    (rune/configure-font-faces))))
  (rune/configure-font-faces))

;; (use-package all-the-icons
;;   :defer nil
;;   :config
;;   (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;;   (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
;;   ;; (set-fontset-font t 'unicode "FontAwesome" nil 'prepend)
;;   (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)
;;   ;; (set-fontset-font t 'unicode (font-spec :family "JetBrainsMono Nerd Font") nil 'append)
;;   :if (display-graphic-p))

(use-package nerd-icons
  :defer 0
  :straight t
  ;; :straight (nerd-icons
  ;;            :type git
  ;;            :host github
  ;;            :repo "rainstormstudio/nerd-icons.el"
  ;;            :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-scale-factor 1.25)
  (nerd-fonts-icons-font-family "JetBrainsMono Nerd Font"))

  ;; (use-package nerd-fonts
  ;;   :straight (nerd-fonts :type git :host github :repo "mohkale/nerd-fonts.el")
  ;;   :after all-the-icons
  ;;   :demand t
  ;;   :config
  ;;   (require 'nerd-fonts-data))

  ;; (use-package all-the-icons-nerd-fonts
  ;;   :straight
  ;;   (all-the-icons-nerd-fonts :host github :repo "mohkale/all-the-icons-nerd-fonts")
  ;;   :after all-the-icons
  ;;   :demand t
  ;;   :config
  ;;   (all-the-icons-nerd-fonts-prefer))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :straight (doom-modeline
             :type git
             :host github
             :repo "seagle0128/doom-modeline")
  :demand t
  :init
  (require 'nerd-icons)
  (doom-modeline-mode 1)
  :custom ((doom-modeline-height 50)))

;; (use-package dashboard
;;   :straight (dashboard
;;              :type git
;;              :host github
;;              :repo "rainstormstudio/emacs-dashboard")
;;   :commands (dashboard dashboard-open)
;;   :init
;;   (progn
;;     (setq dashboard-items '((recents . 5)
;;                             (projects . 3)
;;                             (agenda . 4))
;;           dashboard-center-content t
;;           dashboard-set-file-icons nil
;;           dashboard-set-heading-icons nil
;;           dashboard-startup-banner "~/.config/emacs.gnu/images/logo.png"
;;           dashboard-set-navigator t
;;           dashboard-set-init-info t
;;           dashboard-set-footer nil
;;           dashboard-show-shortcuts nil
;;           dashboard-week-agenda t
;; 		  dashboard-icon-type 'nerd-icons
;; 		  dashboard-display-icons-p nil
;;           ))
;;   :config
;;   ;; (dashboard-modify-heading-icons '((recents . "nf-oct-)))
;;   (dashboard-setup-startup-hook))

;; ;; daemon config
;; ;; (if (daemonp) 
;; (setq initial-buffer-choice (lambda ()
;;                               (get-buffer-create "*dashboard*")
;;                               (dashboard-open)))

(use-package dashboard
  :defer 0
  ;; :disabled t
  :init
  ;; (setq initial-buffer-choice 'dashboard-open)
  ;; (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.config/emacs/images/logo.png"
        dashboard-set-navigator t
        dashboard-set-init-info t
        dashboard-set-footer nil
        dashboard-show-shortcuts nil
		dashboard-icon-type 'nerd-icons
		dashboard-display-icons-p t)
  ;; (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "/home/dt/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
						  (projects . 3)
                          (registers . 3)))
  ;; (dashboard-modify-heading-icons '((recents . "file-text")
  ;; 								(bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))



(defun open-config ()
  (interactive)
  (find-file "~/.config/emacs.gnu/Emacs.org"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))
  
(rune/leader-keys
  "h" '(:ignore t :which-key "hub")
  "hd" '(dashboard-open :which-key "dashboard")
  "hc" '(open-config :which-key "config")
  "hr" '(reload-init-file :which-key "hot reload"))

(use-package tree-sitter-langs
  :demand t)
(use-package tree-sitter
  :defer 0
  :requires (tree-sitter-langs)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode org-mode) '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://" "lambda"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(defun rune/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
        '(("lambda" . "λ"))))
   ;;        ("|>" . "▷")
   ;;        ("<|" . "◁")
   ;;        ("->>" . "↠")
   ;;        ("->" . "→")
   ;;        ("<-" . "←")
   ;;        ("=>" . "⇒")
   ;;        ("<=" . "≤")
   ;;        (">=" . "≥"))))

(defun rune/prettify-org-set ()
  (interactive)
  (rune/prettify-set)
  (setq prettify-symbols-alist '(("TODO" . "")
                                 ("WAIT" . "")        
                                 ("NOPE" . "")
                                 ("DONE" . "﫠")
                                 ("[#A]" . "")
                                 ("[#B]" . "")
                                 ("[#C]" . "")
                                 ("#+author:" . "")
                                 ("#+title:" . "﫳"))))
                         ;; ("[ ]" . "☐")
                         ;; ("[X]" . "☑")
                         ;; ("[-]" . "❍")
                         ;; ;; ("TODO" . "")
                         ;; ("TODO" . "")
                         ;; ("WAIT" . "󰏦")        
                         ;; ("NOPE" . "󰜺")
                         ;; ("DONE" . "")
                         ;; ;; ("[#A]" . "")
                         ;; ("[#A]" . "")
                         ;; ;; ("[#B]" . "")
                         ;; ("[#B]" . "󱐋")
                         ;; ;; ("[#C]" . "")
                         ;; ("[#C]" . "󰅶")
       ;; (add-to-list 'prettify-symbols-alist ligature)))

   ;; (add-hook 'prog-mode-hook 'rune/prettify-set)
;; (add-hook 'org-mode-hook 'rune/prettify-org-set)
;; (rune/prettify-set)
;; (global-prettify-symbols-mode 1)

(use-package writeroom-mode
  :defer 0)

(when rune/use-ivy

  (use-package ivy
    :diminish
    :bind (("C-s" . swiper)
  	   :map ivy-minibuffer-map
  	   ("TAB" . ivy-alt-done)
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
    :config
    (ivy-mode 1))
  
  ;; Counsel Configuration
  (use-package counsel
    :bind (("M-x" . counsel-M-x)
  	   ("C-x b" . counsel-switch-buffer)
  	   ("C-x C-f" . counsel-find-file)
  	   :map minibuffer-local-map
  	   ("C-r" . 'counsel-minibuffer-history))
    :config
    (counsel-mode 1))
  
  
  (use-package ivy-rich
    :init
    (setq ivy-prescient-retain-classic-highlighting t)
    (ivy-rich-mode 1))
  
  (use-package ivy-prescient
    :after counsel
    :config
    (prescient-persist-mode 1)
    (ivy-prescient-mode 1))

)

(unless rune/use-ivy
  (defun rune/minibuffer-backward-kill (arg)
    "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
    (interactive "p")
    (if minibuffer-completing-file-name
	;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
	(if (string-match-p "/." (minibuffer-contents))
            (zap-up-to-char (- arg) ?/)
          (delete-minibuffer-contents))
      (delete-backward-char arg)))
  
  (use-package vertico
    :bind (:map minibuffer-local-map
		("<backspace>" . rune/minibuffer-backward-kill))
    :init
    (vertico-mode 1))

  (use-package vertico-quick
    :straight nil
    :after vertico)

  (use-package consult
    :demand t
    :bind (
	   ("C-s" . consult-line)
	   ("C-x b" . consult-buffer)))
  
  ;; Enable rich annotations using the Marginalia package
  (use-package marginalia
    ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
    ;; available in the *Completions* buffer, add it to the
    ;; `completion-list-mode-map'.
    :bind (:map minibuffer-local-map
  		("M-A" . marginalia-cycle))
    
    ;; The :init section is always executed.
    :init
    
    ;; Marginalia must be activated in the :init section of use-package such that
    ;; the mode gets enabled right away. Note that this forces loading the
    ;; package.
    (marginalia-mode))

  (use-package orderless
    :custom
    (completion-styles '(orderless basic regexp prefixes))
    (completion-category-overrides '((file (styles basic partial-completion))))))

;; Helpful Configuration

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  ;; :custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package yasnippet
  :defer t
  :custom
  (yas-snippet-dirs '("~/.config/emacs.gnu/snippets/"))
  :config
  (yas-global-mode 1))

;; Automatically tangle our Emacs.org config file when we save it
(defun rune/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.config/emacs/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rune/org-babel-tangle-config)))

(use-package toc-org
  :straight t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(electric-indent-mode -1)

(defun rune/org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

  (set-face-attribute 'org-block nil    :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun rune/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (company-mode 0)
  ;; (writeroom-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :defer 0
  :commands (org-capture org-agenda)
  :hook (org-mode . rune/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        ;; '("~/dev/tasks/OrgFiles/Tasks.org"
        ;;   "~/dev/tasks/OrgFiles/Birthdays.org"
        ;;   "~/dev/tasks/OrgFiles/Journal.org"
          '("~/.agenda.org"))

  (setq recentf-exclude org-agenda-files)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)))

  (setq org-babel-python-command "python3")

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "python")))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (setq org-confirm-babel-evaluate nil)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(r)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1 )))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/dev/tasks/OrgFiles/Tasks.org" "Tasks")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/dev/tasks/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/dev/tasks/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/dev/tasks/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/dev/tasks/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
   (rune/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun rune/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook ((org-mode . rune/org-mode-visual-fill)
         (eww-mode . rune/org-mode-visual-fill)))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory "~/.RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-present
  :after org
  :bind (:map org-present-mode-keymap
	      ("<left>" . org-present-prev)
	      ("<right>" . org-present-next))
  :commands (org-present)
  :init
  (add-hook 'org-present-mode-hook 'rune/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'rune/org-present-quit)
  (add-hook 'org-present-after-navigate-functions 'rune/org-present-prepare-slide))

(defun rune/org-present-prepare-slide (buffer-name heading)
  (org-overview)

  (org-show-entry)

  (org-show-children))

(defun rune/org-present-start ()
  (evil-normal-state)
  (setq org-hide-emphasis-markers t)
  (setq header-line-format " ")
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block))))

(defun rune/org-present-quit ()
  (setq org-hide-emphasis-markers nil)
  (setq header-line-format nil)
  (setq-local face-remapping-alist '((default variable-pitch default))))

(rune/leader-keys
  "o"  '(:ignore t :which-key "org-mode")
  "oa" '(org-agenda :which-key "Org Agenda")
  "oc" '(org-capture :which-key "Org Capture")
  "oe" '(org-babel-execute-src-block :which-key "Execute Src Block"))

(use-package htmlize
  :defer 0
  :config
  (setq htmlize-output-type 'inline-css))

(use-package org-alert
  :straight t
  :custom (alert-default-style 'message)
  :config
  (setq org-alert-interval 300)
  (setq org-alert-notification-title "Org Reminder")
  (org-alert-enable))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev/*")
    (setq projectile-project-search-path '("~/dev/*")))
  (setq projectile-switch-project-action #'projectile-dired))

(when rune/use-ivy
  (use-package counsel-projectile
    :after projectile
    :config (counsel-projectile-mode)))

(unless rune/use-ivy
  (use-package consult-projectile
    :after consult))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(rune/leader-keys
  "g"  '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "git status"))

(use-package forge
  :after magit)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :requires git-gutter-fringe
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package autopair
  :init
  (setq autopair-autowrap t))
  ;; (autopair-global-mode 1))

(when rune/use-company
  (use-package company-mode
    :bind (:map company-active-map
		("<tab>" . company-complete-selection))
    (:map eglot-map
          ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.1)
    :init
    (global-company-mode 1))
  
  (use-package company-box
    :hook (company-mode . company-box-mode))

  (use-package company-prescient
    :after company
    :config
    (company-prescient-mode 1)))

(unless rune/use-company

  (use-package corfu
    ;; Optional customizations
    :custom
    (corfu-cycle t)                 ; Allows cycling through candidates
    (corfu-auto t)                  ; Enable auto completion
    (corfu-auto-prefix 1)
    (corfu-auto-delay 0.0)
    (corfu-popupinfo-delay '(0.5 . 0.2))
    (corfu-preview-current 'insert) ; Do not preview current candidate
    (corfu-preselect-first nil)
    (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
    (tab-always-indent 'complete)
    
    ;; Optionally use TAB for cycling, default is `corfu-complete'.
    :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-complete)
              ([tab]        . corfu-complete)
              ("<up>"       . corfu-previous)
              ([up]         . corfu-previous)
              ("<down>"     . corfu-next)
              ([down]       . corfu-next)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
    
    :init
    (global-corfu-mode)
    :config
    (add-hook 'eshell-mode-hook
              (lambda () (setq-local corfu-quit-at-boundary t
				     corfu-quit-no-match t
				     corfu-auto nil)
		(corfu-mode)))))

;; (corfu-mode 1)

(unless rune/use-company
  (use-package corfu-terminal
    :after corfu
    :load-path "lisp/corfu-terminal"
    :config
    (corfu-terminal-mode 1)))

(unless rune/use-company
  (use-package cape
    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    :config
    ;; Silence then pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    
    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

;; (use-package lsp-mode
;;   :disabled
;;   :demand nil
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-mode 1))

;; (use-package rustic
;;   :defer t
;;   :custom
;;   ;;(rustic-lsp-server . ('rust-analyzer))
;;   (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . (lambda () (eglot)))
  :custom
  ;; scratchpad for rust
  ;; (setq lsp-rust-clippy-preference "on")
  (use-package rust-playground
    :commands (rust-playground)))

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-shell-completion-native-enable nil)
  (python-mode)
  :hook (python-mode . (lambda ()
                         (eglot))))  ; or lsp-deferred

;; (use-package ccls
;;   :defer t)
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp-deferred))))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook ((haskell-mode . (lambda () (eglot)))
		 (haskell-mode . interactive-haskell-mode)
		 (haskell-mode . haskell-doc-mode))
		 ;; (haskell-mode . hindent-mode))
  :custom (haskell-stylish-on-save t)
  :bind ("C-c C-c" . haskell-compile))
  ;; :config
  ;; (require 'lsp-haskell))
;; (use-package lsp-haskell
;;   :ensure t
;;   :config
;;   (setq lsp-haskell-process-path-hie "ghcide")
;;   (setq lsp-haskell-process-args-hie '()))

(use-package go-mode
  :hook (go-mode . (lambda () (eglot)))
  :mode "\\.go\\'")

(use-package v-mode
  :straight (v-mode
             :type git
             :host github
             :repo "damon-kwok/v-mode"
             :files ("tokens" "v-mode.el"))
  :config
  :bind-keymap
  ("M-z" . v-menu)
  ("<f6>" . v-menu)
  ("C-c C-f" . v-format-buffer)
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))

(use-package gdscript-mode
  :mode "\\.gd\\'")

(use-package vterm
  :commands (vterm vterm-other-window))

(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on split-window-sensibly, but designed to prefer a horizontal split,
i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
           (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq split-height-threshold 4
      split-width-threshold 40
      split-window-preferred-function 'split-window-really-sensibly)
