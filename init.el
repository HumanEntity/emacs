(defun rune/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'rune/display-startup-time)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("org" . "https://orgmode.org/elpa/")))
        ;; ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")))
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; (package-initialize)

;; (setq use-package-always-ensure t)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))
;; ;; (setq use-package-verbose t)
;; (setq use-package-always-defer t)
;; (setq package-native-compile t)
;; (setq comp-deferred-compilation t)

; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defvar rune/default-font-size 180)
(defvar rune/default-variable-font-size 180)
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

(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package all-the-icons
  :defer nil
  :if (display-graphic-p))

(defun rune/configure-font-faces ()
  (set-frame-parameter (selected-frame) 'alpha rune/frame-transparency)
  (add-to-list 'default-frame-alist `(alpha . ,rune/frame-transparency))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'light :height rune/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :weight 'light :height  rune/default-font-size)

  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'light :height 1.3))


(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (rune/configure-font-faces))))
  (rune/configure-font-faces))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :defer nil
  :demand t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-palenight t))

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
  :demand
  :config
  ;; (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme"))
  (rune/leader-keys
    "o"  '(:ignore t :which-key "org-mode")
    "oa" '(org-agenda :which-key "Org Agenda")))

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

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

  ;; Counsel Configuration
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :init
  (counsel-mode t))

;; Helpful Configuration

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package yasnippet
  :defer 0
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;; Automatically tangle our Emacs.org config file when we save it
(defun rune/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rune/org-babel-tangle-config)))

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

  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
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
  (setq evil-auto-indent nil))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . rune/org-mode-setup)
  :config
  (message "Org Mod loaded")
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/dev/tasks/OrgFiles/Tasks.org"
          "~/dev/tasks/OrgFiles/Birthdays.org"))

  (org-babel-do-load-languages
   'org-babel-do-load-languages
   '((emacs-lisp . t)
     (python . t)))

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
      ("tt" "Task" entry (file+olp "~/dev/tasks/OrgFiles/Tasks.org" "Inbox")
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
  :hook (org-mode . rune/org-mode-visual-fill))

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

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1 ))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev/*")
    (setq projectile-project-search-path '("~/dev/*")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-diff-v1))

(use-package forge
  :after magit)

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
              (corfu-mode))))

(corfu-mode 1)

(use-package corfu-terminal
  :after corfu
  :load-path "lisp/corfu-terminal"
  :config
  (corfu-terminal-mode 1))

(use-package cape
  :defer 10
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  :config
  :init
  ;; scratchpad for rust
  (setq lsp-rust-clippy-preference "on")
  (use-package rust-playground
    :commands (rust-playground)))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

(use-package ccls
  :defer t
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp-deferred))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package dap-mode
  :commands dap-debug
  :config
  (dap-ui-mode)

  ;; (require 'dap-codelldb)
  ;; (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)

  ;; (dap-codelldb-setup)
  (dap-cpptools-setup)
  (dap-gdb-lldb-setup))
  ;;(require 'dap-codelldb)
  ;; installs .extension/vscode
  ;;(dap-codelldb-setup)
  ;;(dap-cpptools-setup))
  ;;:custom
  ;; (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  ;;(dap-lldb-debug-program `(,(expand-file-name "/opt/homebrew/opt/llvm/bin/lldb"))))

(with-eval-after-load 'dap-mode
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "Rust::Run"
         :MIMode "lldb"
         :miDebuggerPath "rust-lldb"
         :enviroment []
         :program "${workspaceFolder}/target/debug/${target}"
         :cwd "${workspaceFolder}"
         :console "external"
         :dap-compilation "cargo build"
         :dap-compilation-dir "${workspaceFolder}")))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args
  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun rune/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

  (use-package eshell-git-prompt
    :after eshell-)

  (use-package eshell
    :hook (eshell-first-time-mode . rune/configure-eshell)
    :config

    (with-eval-after-load 'esh-opt
      (setq eshell-destroy-buffer-when-process-dies t)
      (setq eshell-visual-commands '("htop" "zsh" "vim")))

    (eshell-git-prompt-use-theme 'multiline))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :after evil-collection
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
                              "H" 'dired-hide-dotfiles-mode))

(use-package dired
  :straight nil
  :defer 0
  :after evil-collection
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
                              "h" 'dired-single-up-directory
                              "l" 'dired-single-buffer))

(use-package perspective
  :demand t
  :custom
  (persp-initial-frame-name "Main")
  (persp-mode-prefix-key (kbd "C-x x"))
  :bind (("C-x k" . persp-kill-buffer*))
  :init
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package speed-type
  :defer 1)

;; (use-package erc
;;   :custom
;;   (erc-nick "HumanEntity"))

;;  (with-eval-after-load 'erc
;;    (load "~/.emacs.d/lisp/erc/erc-sasl.el")

;;    (require 'erc-sasl)
;;    (add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

;;    ;; Redefine/Override the erc-login() function from the erc package, so that
;;    ;; it now uses SASL
;;    (defun erc-login ()
;;      "Perform user authentication at the IRC server. (PATCHED)"
;;      (erc-log (format "login: nick: %s, user: %s %s %s :%s"
;;                       (erc-current-nick)
;;                       (user-login-name)
;;                       (or erc-system-name (system-name))
;;                       erc-session-server
;;                       erc-session-user-full-name))
;;      (if erc-session-password
;;          (erc-server-send (format "PASS %s" erc-session-password))
;;        (message "Logging in without password"))
;;      (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
;;        (erc-server-send "CAP REQ :sasl"))
;;      (erc-server-send (format "NICK %s" (erc-current-nick)))
;;      (erc-server-send
;;       (format "USER %s %s %s :%s"
;;               ;; hacked - S.B.
;;               (if erc-anonymous-login erc-email-userid (user-login-name))
;;               "0" "*"
;;               erc-session-user-full-name))
;;      (erc-update-mode-line)))

;;  (defun rune/erc-chat ()
;;    (erc-tls :server "irc.libera.chat" :port 6697 :nick "HumanEntity"
;;    :full-name "HumanEntity"
;;    :password "HuM@n@b1e"))

(setq-default indent-tabs-mode t)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-mode 1))

;; 
;;   (defun split-window-sensibly-prefer-horizontal (&optional window)
;;   "Based on split-window-sensibly, but designed to prefer a horizontal split,
;;   i.e. windows tiled side-by-side."
;;     (let ((window (or window (selected-window))))
;;       (or (and (window-splittable-p window t)
;;            ;; Split window horizontally
;;            (with-selected-window window
;;              (split-window-right)))
;;       (and (window-splittable-p window)
;;            ;; Split window vertically
;;            (with-selected-window window
;;              (split-window-below)))
;;       (and
;;            ;; If WINDOW is the only usable window on its frame (it is
;;            ;; the only one or, not being the only one, all the other
;;            ;; ones are dedicated) and is not the minibuffer window, try
;;            ;; to split it horizontally disregarding the value of
;;            ;; `split-height-threshold'.
;;            (let ((frame (window-frame window)))
;;              (or
;;               (eq window (frame-root-window frame))
;;               (catch 'done
;;                 (walk-window-tree (lambda (w)
;;                                     (unless (or (eq w window)
;;                                                 (window-dedicated-p w))
;;                                       (throw 'done nil)))
;;                                   frame)
;;                 t)))
;;        (not (window-minibuffer-p window))
;;        (let ((split-width-threshold 0))
;;          (when (window-splittable-p window t)
;;            (with-selected-window window
;;              (split-window-right))))))))
;; 
;;   (defun split-window-really-sensibly (&optional window)
;;     (let ((window (or window (selected-window))))
;;       (if (> (window-total-width window) (* 2 (window-total-height window)))
;;           (with-selected-window window (split-window-sensibly-prefer-horizontal window))
;;         (with-selected-window window (split-window-sensibly window)))))
;; 
;;   (setq split-height-threshold 4
;;         split-width-threshold 40
;;         split-window-preferred-function 'split-window-really-sensibly)
