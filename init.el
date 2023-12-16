;; Global config

(defvar rune/config-dir "~/.config/emacs/")

(defvar rune/use-ivy nil "Config option to use ivy (t) or vertico (nil)")
(defvar rune/use-company nil "Config option to use company (t) or corfu (nil)")

;; Load path

(add-to-list 'load-path (expand-file-name "lisp/modeline" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/corfu-terminal" user-emacs-directory))

(setq package-archives
	'(("melpa" . "https://melpa.org/packages/")
	  ("elpa" . "https://elpa.gnu.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	  ("org" . "https://orgmode.org/elpa/")))

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca setup (require 'setup))
(elpaca-process-queues)

(defun setup-wrap-to-install-package (body _name)
  "Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
  (if (assq 'elpaca setup-attributes)
      `(elpaca ,(cdr (assq 'elpaca setup-attributes)) ,@(macroexp-unprogn body))
    body))
;; Add the wrapper function
(add-to-list 'setup-modifier-list #'setup-wrap-to-install-package)
(setup-define :elpaca
	      (lambda (order &rest recipe)
		(push (cond
		       ((eq order t) `(elpaca . ,(setup-get 'feature)))
		       ((eq order nil) '(elpaca . nil))
		       (`(elpaca . (,order ,@recipe))))
		      setup-attributes)
		;; If the macro wouldn't return nil, it would try to insert the result of
		;; `push' which is the new value of the modified list. As this value usually
		;; cannot be evaluated, it is better to return nil which the byte compiler
		;; would optimize away anyway.
		nil)
	      :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
	      :shorthand #'cadr)

(setup-define :load-after
    (lambda (features &rest body)
      (let ((body `(progn
                     (require ',(setup-get 'feature))
                     ,@body)))
        (dolist (feature (if (listp features)
                             (nreverse features)
                           (list features)))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)

(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

;; (setq straight-disable-native-compile nil)
;; (straight-use-package 'use-package)

;; (setq straight-use-package-by-default t)

;; (package-initialize)

;; (setq use-package-always-ensure t)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(setq use-package-verbose t)
(setq use-package-always-defer t)
(setq package-native-compile t)
(setq comp-deferred-compilation nil)

(elpaca diminish (require 'diminish))
(elpaca-wait)

;; Using garbage magic hack.
;; (use-package gcmh
;;   :config
;;   (gcmh-mode 1))
;; ;; Setting garbage collection threshold
;; (setq gc-cons-threshold 402653184
;;       gc-cons-percentage 0.6)
(setup (:elpaca gcmh)
  (:option gc-cons-threshold 402653184)
  (:option gc-cons-percentage 0.6)
  (gcmh-mode 1))

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

;; (use-package no-littering
;;   :defer nil
;;   :config

;;   ;; no-littering doesn't set this by default so we must place
;;   ;; auto save files in the same path as it uses for sessions
;;   (setq auto-save-file-name-transforms
;; 	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setup (:elpaca no-littering)
  (:option auto-save-file-name-transforms
	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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

;; (use-package evil-collection
;;   :defer nil
;;   :after evil
;;   :config
;;   (evil-collection-init))
(setup (:elpaca evil-collection)
  (:load-after evil
	       (evil-collection-init)))

;; (use-package evil-nerd-commenter
;;   :after evil
;;   :bind ("M-/" . evilnc-comment-or-uncomment-lines))
(setup (:elpaca evil-nerd-commenter)
  (:global "M-/" evilnc-comment-or-uncomment-lines))

;; (use-package evil
;;   :defer t
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   ;;  :hook (evil-mode . rune/evil-hook)
;;   :config
;;   (evil-mode)
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;;   ;; Use visual line motions even outside of visual-line-mode buffers
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
;;   (evil-set-initial-state 'dashboard-mode 'normal))

;; Little code to get mode indication in evil

(setup (:elpaca evil)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;;  :hook (evil-mode . rune/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(elpaca-wait)
(evil-mode nil)
(evil-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (use-package general
;;   :demand t
;;   :config
;;   ;; (general-evil-setup t)
;;   (general-create-definer rune/leader-keys
;; 	:keymaps '(normal insert visual emacs)
;; 	:prefix "SPC"
;; 	:global-prefix "C-SPC")

;;   (rune/leader-keys
;; 	"t"  '(:ignore t :which-key "toggles")
;; 	"tt" (if rune/use-ivy '(counsel-load-theme :which-key "choose theme") '(consult-theme :which-key "choose theme"))
;; 	"tf" '(treemacs :which "toggle file explorer"))
;;   (rune/leader-keys
;; 	"f" '(:ignore t :which-key "find"))
;;   (when rune/use-ivy
;; 	  (rune/leader-keys
;; 	"ff" '(counsel-find-file :which-key "find file")))
;;   (unless rune/use-ivy
;; 	(rune/leader-keys
;; 	  "ff" '(find-file :which-key "find file"))))
  ;; (rune/leader-keys
  ;;   "o"  '(:ignore t :which-key "org-mode")
  ;;   "oa" '(org-agenda :which-key "Org Agenda")
  ;;   "oc" '(org-capture :which-key "Org Capture"))

(setup (:elpaca general)
  ;; (general-evil-setup t)
  (general-create-definer rune/leader-keys
	:keymaps '(normal insert visual emacs)
	:prefix "SPC"
	:global-prefix "C-SPC")

  (rune/leader-keys
	"t"  '(:ignore t :which-key "toggles")
	"tt" (if rune/use-ivy '(counsel-load-theme :which-key "choose theme") '(consult-theme :which-key "choose theme"))
	"tf" '(treemacs :which "toggle file explorer"))
  (rune/leader-keys
	"f" '(:ignore t :which-key "find"))
  (when rune/use-ivy
	  (rune/leader-keys
	"ff" '(counsel-find-file :which-key "find file")))
  (unless rune/use-ivy
	(rune/leader-keys
	  "ff" '(find-file :which-key "find file"))))

(elpaca-wait)

;; (use-package which-key
;;   :defer 0
;;   :diminish which-key-mode
;;   :config
;;   (setq which-key-idle-delay 0.05 )
;;   (which-key-mode))

(setup (:elpaca which-key)
  (diminish 'which-key-mode)
  (setq which-key-idle-delay 0.05)
  (which-key-mode))

;; (add-to-list 'custom-theme-load-path "~/.config/emacs.gnu/themes/")
;; (use-package doom-themes
;;   :defer t
;;   :config
;;   (setq doom-themes-enable-bold t
;; 		doom-themes-enable-italic t))

;; (use-package catppuccin-theme)

;; ;;(use-package spaceway-theme
;;  :elpaca nil
;;  :load-path "lisp/spaceway/"
;;  :init
;;  (load-file "~/.config/emacs/lisp/spaceway/spaceway-theme.el"))

;; (use-package modus-themes
;;   :demand t
;;   :init
;;   (load-theme 'modus-vivendi-tinted t))
(setup (:elpaca modus-themes)
  (load-theme 'modus-vivendi-tinted t))
(elpaca-wait)

(defvar rune/default-font-size 140)
(defvar rune/default-variable-font-size 140)
(defvar rune/frame-transparency '(100 . 100))

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

;; (use-package spacious-padding
;;   :custom
;;   (spacious-padding-widths 
;;    '( :internal-border-width 15
;;       :header-line-width 4
;;       :mode-line-width 6
;;       :tab-width 4
;;       :right-divider-width 30
;;       :scroll-bar-width 8))
;;   (spacious-padding-subtle-mode-line
;;    '(:mode-line-active default :mode-line-inactive vertical-border))
;;   :init
;;   (spacious-padding-mode 1))

(setup (:elpaca spacious-padding)
  (:option spacious-padding-widths 
	   '( :internal-border-width 15
	      :header-line-width 4
	      :mode-line-width 6
	      :tab-width 4
	      :right-divider-width 30
	      :scroll-bar-width 8)
	   spacious-padding-subtle-mode-line
	   '(:mode-line-active default :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

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

;; (use-package nerd-icons
;;   :defer 0
;;   ;; :straight (nerd-icons
;;   ;;            :type git
;;   ;;            :host github
;;   ;;            :repo "rainstormstudio/nerd-icons.el"
;;   ;;            :files (:defaults "data"))
;;   :custom
;;   ;; The Nerd Font you want to use in GUI
;;   ;; "Symbols Nerd Font Mono" is the default and is recommended
;;   ;; but you can use any other Nerd Font if you want
;;   (nerd-icons-scale-factor 1.25)
;;   (nerd-fonts-icons-font-family "JetBrainsMono Nerd Font"))

(setup (:elpaca nerd-icons)
  (:option nerd-icons-scale-factor 1.25
	   nerd-fonts-icons-font-family "JetBrainsMono Nerd Font"))

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

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))
(setup (:elpaca rainbow-delimiters)
  (:with-mode prog-mode
    (:hook rainbow-delimiters-mode)))

;; (use-package doom-modeline
;;   :disabled
;;   :straight (doom-modeline
;;              :type git
;;              :host github
;;              :repo "seagle0128/doom-modeline")
;;   :demand t
;;   :init
;;   (require 'nerd-icons)
;;   (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 50)))

(load-file (expand-file-name "lisp/modeline/modeline.el" rune/config-dir))

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

;; (use-package dashboard
;;   :defer 0
;;   ;; :disabled t
;;   :init
;;   ;; (setq initial-buffer-choice 'dashboard-open)
;;   ;; (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-startup-banner "~/.config/emacs/images/logo.png"
;;         dashboard-set-navigator t
;;         dashboard-set-init-info t
;;         dashboard-set-footer nil
;;         dashboard-show-shortcuts nil
;;   	      dashboard-icon-type 'nerd-icons
;;   	      dashboard-display-icons-p t)
;;   ;; (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
;;   ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;;   ;; (setq dashboard-startup-banner "/home/dt/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
;;   (setq dashboard-center-content t) ;; set to 't' for centered content
;;   (setq dashboard-items '((recents . 5)
;;                           (agenda . 5 )
;;   						(projects . 3)
;;                           (registers . 3)))
;;   ;; (dashboard-modify-heading-icons '((recents . "file-text")
;;   ;; 								(bookmarks . "book")))
;;   :config
;;   (dashboard-setup-startup-hook))

(setup (:elpaca dashboard)

  ;; Init

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

  ;; Config

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

;; (use-package tree-sitter-langs
;;   :demand t)
(setup (:elpaca tree-sitter-langs))
;; (use-package tree-sitter
;;   :demand t
;;   :requires (tree-sitter-langs)
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(setup (:elpaca tree-sitter)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package ligature
;;   :load-path "path-to-ligature-repo"
;;   :config
;;   ;; Enable the "www" ligature in every possible major mode
;;   (ligature-set-ligatures 't '("www"))
;;   ;; Enable traditional ligature support in eww-mode, if the
;;   ;; `variable-pitch' face supports it
;;   (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
;;   ;; Enable all Cascadia Code ligatures in programming modes
;;   (ligature-set-ligatures '(prog-mode org-mode) '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                                        ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                                        "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                                        "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                                        "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                                        "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                                        "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                                        "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                                        ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                                        "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                                        "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                                        "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                                        "\\\\" "://" "lambda"))
;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))

;; (defun rune/prettify-set ()
;;   (interactive)
;;   (setq prettify-symbols-alist
;;         '(("lambda" . "λ"))))
;;    ;;        ("|>" . "▷")
;;    ;;        ("<|" . "◁")
;;    ;;        ("->>" . "↠")
;;    ;;        ("->" . "→")
;;    ;;        ("<-" . "←")
;;    ;;        ("=>" . "⇒")
;;    ;;        ("<=" . "≤")
;;    ;;        (">=" . "≥"))))

;; (defun rune/prettify-org-set ()
;;   (interactive)
;;   (rune/prettify-set)
;;   (setq prettify-symbols-alist '(("TODO" . "")
;;                                  ("WAIT" . "")        
;;                                  ("NOPE" . "")
;;                                  ("DONE" . "﫠")
;;                                  ("[#A]" . "")
;;                                  ("[#B]" . "")
;;                                  ("[#C]" . "")
;;                                  ("#+author:" . "")
;;                                  ("#+title:" . "﫳"))))
;;                          ;; ("[ ]" . "☐")
;;                          ;; ("[X]" . "☑")
;;                          ;; ("[-]" . "❍")
;;                          ;; ;; ("TODO" . "")
;;                          ;; ("TODO" . "")
;;                          ;; ("WAIT" . "󰏦")        
;;                          ;; ("NOPE" . "󰜺")
;;                          ;; ("DONE" . "")
;;                          ;; ;; ("[#A]" . "")
;;                          ;; ("[#A]" . "")
;;                          ;; ;; ("[#B]" . "")
;;                          ;; ("[#B]" . "󱐋")
;;                          ;; ;; ("[#C]" . "")
;;                          ;; ("[#C]" . "󰅶")
;;        ;; (add-to-list 'prettify-symbols-alist ligature)))

;;    ;; (add-hook 'prog-mode-hook 'rune/prettify-set)
;; ;; (add-hook 'org-mode-hook 'rune/prettify-org-set)
;; ;; (rune/prettify-set)
;; ;; (global-prettify-symbols-mode 1)

(setup (:elpaca writeroom-mode))

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
  
  ;; (use-package vertico
  ;;   :bind (:map minibuffer-local-map
  ;; 		("<backspace>" . rune/minibuffer-backward-kill)
  ;; 		("C-j"         . vertico-next)
  ;; 		("C-k"         . vertico-previous))
  ;;   :custom
  ;;   (vertico-cycle t)
  ;;   :init
  ;;   (vertico-mode 1))

  (setup (:elpaca vertico)
    (vertico-mode)
    (:with-map vertico-map
      (:bind "C-j" vertico-next
	     "C-k" vertico-previous))
    (:with-map minibuffer-local-map
      (:bind "<backspace>" rune/minibuffer-backward-kill))
    (:option vertico-cycle t))

  ;; (use-package vertico-quick
  ;;   :elpaca nil
  ;;   :after vertico)

  (setup vertico-quick
    (:load-after vertico))

  ;; (use-package consult
  ;;   :demand t
  ;;   :bind (
  ;; 	   ("C-s" . consult-line)
  ;; 	   ("C-x b" . consult-buffer)))

  (setup (:elpaca consult)
    (require 'consult)
    (:global "C-s" consult-line
	     "C-x b" consult-buffer)
    (:with-map minibuffer-local-map
      (:bind "C-r" consult-history)))
  
  ;; Enable rich annotations using the Marginalia package
  ;; (use-package marginalia
  ;;   ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;;   ;; available in the *Completions* buffer, add it to the
  ;;   ;; `completion-list-mode-map'.
  ;;   :bind (:map minibuffer-local-map
  ;; 		("M-A" . marginalia-cycle))
    
  ;;   ;; The :init section is always executed.
  ;;   :init
    
  ;;   ;; Marginalia must be activated in the :init section of use-package such that
  ;;   ;; the mode gets enabled right away. Note that this forces loading the
  ;;   ;; package.
  ;;   (marginalia-mode))
  
  (setup (:elpaca marginalia)
    (:option marginalia-annotators '(marginalia-annotators-heavy
				     marginalia-annotators-light
				     nil))
    (marginalia-mode))

  ;; (use-package orderless
  ;;   :custom
  ;;   (completion-styles '(orderless basic))
  ;;   (completion-category-overrides '((file (styles basic partial-completion))))))

  (setup (:elpaca orderless)
    (require 'orderless)
    (setq completion-styles '(orderless)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion)))))))

;; Helpful Configuration

;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   ;; :custom
;;   ;; (counsel-describe-function-function #'helpful-callable)
;;   ;; (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . helpful-callable)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . helpful-variable)
;;   ([remap describe-key] . helpful-key))

(setup (:elpaca helpful)
  (:global
      [remap describe-function] helpful-callable
      [remap describe-command] helpful-command
      [remap describe-variable] helpful-variable
      [remap describe-key] helpful-key))

;; (use-package yasnippet
;;   :defer t
;;   :custom
;;   (yas-snippet-dirs '("~/.config/emacs.gnu/snippets/"))
;;   :config
;;   (yas-global-mode 1))

;; Automatically tangle our Emacs.org config file when we save it
(defun rune/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.config/emacs/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'rune/org-babel-tangle-config)))

;; (use-package toc-org
;;   :commands toc-org-enable
;;   :init (add-hook 'org-mode-hook 'toc-org-enable))
(setup (:elpaca toc-org)
  (:with-mode org-mode
    (:hook toc-org-enable)))

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
  ;; (when rune/use-company
  ;;     (company-mode 0))
  ;; (unless rune/use-company
  ;;   (corfu-mode 0))
  ;; (writeroom-mode 1)
  (setq evil-auto-indent nil))

(use-package org
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

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode)
;;   :custom
;;   (org-superstar-leading-bullet " ")
;;   (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))
;; (use-package org-modern
;;   :hook ((org-mode . org-modern-mode)
;; 	 (org-agenda-finalize-hook . org-modern-agenda))
;;   :init
;;   (global-org-modern-mode 1))
(setup (:elpaca org-modern)
  (:with-mode org-mode
    (:hook org-modern-mode))
  (add-hook 'org-agenda-finalize-hook 'org-modern-agenda))

(defun rune/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :defer t
;;   :hook ((org-mode . rune/org-mode-visual-fill)
;;          (eww-mode . rune/org-mode-visual-fill)))
(setup (:elpaca visual-fill-column)
  (:with-mode org-mode
    (:hook rune/org-mode-visual-fill))
  (:with-mode eww-mode
    (:hook rune/org-mode-visual-fill)))

;; (use-package org-roam
;;   :after org
;;   :custom
;;   (org-roam-directory "~/.RoamNotes")
;;   (org-roam-completion-everywhere t)
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert))
;;   :config
;;   (org-roam-setup))

(setup (:elpaca org-roam)
  (:load-after org
    (org-roam-db-autosync-enable))
  (:option org-roam-directory "~/.RoamNotes"
	   org-roam-completion-everywhere t))

;; (use-package org-present
;;   :after org
;;   :bind (:map org-present-mode-keymap
;; 	      ("<left>" . org-present-prev)
;; 	      ("<right>" . org-present-next))
;;   :commands (org-present)
;;   :init
;;   (add-hook 'org-present-mode-hook 'rune/org-present-start)
;;   (add-hook 'org-present-mode-quit-hook 'rune/org-present-quit)
;;   (add-hook 'org-present-after-navigate-functions 'rune/org-present-prepare-slide))

(setup (:elpaca org-present)
  (:with-map org-present-mode-keymap
    (:bind "<left>" org-present-prev
	   "<right>" org-present-next))
  (:hook rune/present-start)
  (:with-hook org-present-mode-quit-hook
    (:hook rune/org-present-quit))
  (:with-hook org-present-after-navigate-functions
    (:hook rune/org-present-prepare-slide)))

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

;; (use-package htmlize
;;   :defer 0
;;   :config
;;   (setq htmlize-output-type 'inline-css))
(setup (:elpaca htmlize)
  (setq htmlize-output-type 'inline-css))

;; (use-package org-timeline
;;   :commands org-agenda
;;   :init
;;   (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;; (use-package org-alert
;;   :custom (alert-default-style 'message)
;;   :config
;;   (setq org-alert-interval 300)
;;   (setq org-alert-notification-title "Org Reminder")
;;   (org-alert-enable))

;; (use-package projectile
;;   :defer t
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/dev/*")
;;     (setq projectile-project-search-path '("~/dev/*")))
;;   (setq projectile-switch-project-action #'projectile-dired))

(setup (:elpaca projectile)
  (:global "C-c p" projectile-command-map)
  (diminish projectile-mode)
  ;; Init
  (when (file-directory-p "~/dev/*")
    (setq projectile-project-search-path '("~/dev/*")))
  (setq projectile-switch-project-action #'projectile-dired)
  ;; Config
  (projectile-mode))

(when rune/use-ivy
  (use-package counsel-projectile
    :after projectile
    :config (counsel-projectile-mode)))

(unless rune/use-ivy
  (setup (:elpaca consult-projectile)
    (:load-after consult)))

;; (use-package magit
;;   :commands (magit-status magit-get-current-branch)
;;   :custom
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(setup (:elpaca magit)
  (:also-load magit-todos)
  (:global "C-M-;" magit-status)
  (:option magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(rune/leader-keys
  "g"  '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "git status"))

(setup (:elpaca forge)
  (:load-after magit))

;; (use-package forge
;;   :after magit)

;; (use-package git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :requires git-gutter-fringe
;;   :config
;;   (setq git-gutter:update-interval 0.02))

(setup (:elpaca git-gutter)
  (setup (:elpaca git-gutter-fringe)
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
  (:with-mode prog-mode
    (:hook git-gutter-mode))
  (setq git-gutter:update-interval 0.02))

;; (use-package git-gutter-fringe
;;   :after git-gutter
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; (use-package autopair
;;   :init
;;   (setq autopair-autowrap t))
;; (autopair-global-mode 1)

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

  ;; (use-package corfu
  ;;   ;; Optional customizations
  ;;   :custom
  ;;   (corfu-cycle t)                 ; Allows cycling through candidates
  ;;   (corfu-auto t)                  ; Enable auto completion
  ;;   (corfu-auto-prefix 2)
  ;;   (corfu-auto-delay 0.1)
  ;;   (corfu-popupinfo-delay '(0.5 . 0.2))
  ;;   (corfu-preview-current 'insert) ; Do not preview current candidate
  ;;   (corfu-preselect-first nil)
  ;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;;   (tab-always-indent 'complete)
    
  ;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;;   :bind (:map corfu-map
  ;;             ("M-SPC"      . corfu-insert-separator)
  ;;             ("TAB"        . corfu-complete)
  ;;             ([tab]        . corfu-complete)
  ;;             ("<up>"       . corfu-previous)
  ;;             ([up]         . corfu-previous)
  ;;             ("<down>"     . corfu-next)
  ;;             ([down]       . corfu-next)
  ;;             ("S-<return>" . corfu-insert)
  ;;             ("RET"        . nil))
    
  ;;   :init
  ;;   (use-package nerd-icons-corfu
  ;;     :after corfu
  ;;     :init
  ;;     (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  ;;   (global-corfu-mode 1)
  ;;   (corfu-popupinfo-mode 1)
  ;;   (corfu-echo-mode 1)
  ;;   :config
  ;;   (add-hook 'eshell-mode-hook
  ;;             (lambda () (setq-local corfu-quit-at-boundary t
  ;; 				     corfu-quit-no-match t
  ;; 				     corfu-auto nil)
  ;; 		(corfu-mode))))
  (setup (:elpaca corfu)
    (:option corfu-cycle t                 ; Allows cycling through candidates
	     corfu-auto t                  ; Enable auto completion
	     corfu-auto-prefix 2
	     corfu-auto-delay 0.1
	     corfu-popupinfo-delay '(0.5 . 0.2)
	     corfu-preview-current 'insert ; Do not preview current candidate
	     corfu-preselect-first nil
	     corfu-on-exact-match nil      ; Don't auto expand tempel snippets
	     tab-always-indent 'complete)
    (:bind "M-SPC" corfu-insert-separator
	     "TAB" corfu-complete
	     "<up>" corfu-previous
	     "<down>" corfu-next
	     "S-<return>" corfu-insert
	     "RET" nil)
    (setup (:elpaca nerd-icons-corfu)
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

    (global-corfu-mode)
    (corfu-popupinfo-mode 1)
    (corfu-echo-mode 1)))
;; (corfu-mode 1)

;; (unless rune/use-company
;;   ;; (use-package corfu-terminal
;;   ;;   :after corfu
;;   ;;   :load-path "lisp/corfu-terminal"
;;   ;;   :config
;;   ;;   (corfu-terminal-mode 1))
;;   (setup corfu-terminal
;;     (:load-after corfu
;;       (corfu-terminal-mode))))

(unless rune/use-company
  ;; (use-package cape
  ;;   :init
  ;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;   (add-to-list 'completion-at-point-functions #'cape-file)
  ;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;   :config
  ;;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;;   ;; Silence then pcomplete capf, no errors or messages!
  ;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    
  ;;   ;; Ensure that pcomplete does not write to the buffer
  ;;   ;; and behaves as a pure `completion-at-point-function'.
  ;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
  (setup (:elpaca cape)
    
    ;; Init
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)

    ;; Config
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    ;; Silence then pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    
    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)))

;; (use-package lsp-mode
;;   :disabled
;;   :demand nil
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-mode 1))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-l")  ;; Or 'C-l', 's-l'
;;   :config
;;   (lsp-enable-which-key-integration t))
;; (use-package eglot
;;   :commands (eglot eglot-ensure))
(setup (:elpaca eglot)
  (require 'eglot))
    ;; :hook (eglot-mode . (lambda () 
    ;; 	   (add-to-list 'completion-at-point-functions #'eglot-completion-at-point))))

;; (use-package rustic
;;   :mode "\\.rs\\'"
;;   :hook (rustic-mode . eglot-ensure)
;;   :defer t
;;   :custom
;;   ;; (rustic-lsp-client 'eglot)
;;   (rustic-lsp-server . ('rust-analyzer))
;;   (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :hook (rust-mode . eglot-ensure)
;;   :custom
;;   ;; scratchpad for rust
;;   ;; (setq lsp-rust-clippy-preference "on")
;;   :config
;;   (use-package rust-playground
;;     :commands (rust-playground)))

(setup (:elpaca rust-mode)
  (:file-match "\\.rs\\'")
  (:hook eglot-ensure))

;; (use-package python-mode
;;   :mode "\\.py\\'"
;;   :config
;;   (setq python-shell-completion-native-enable nil)
;;   (python-mode)
;;   :hook (python-mode . eglot-ensure))  ; or lsp-deferred 

(setup (:elpaca python-mode)
  (:file-match "\\.py\\'")
  (:hook eglot-ensure)
  (setq python-shell-completion-native-enable nil)
  (python-mode))

;; (use-package ccls
;;   :defer t)
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp-deferred))))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))

;; (use-package haskell-mode
;;   :mode "\\.hs\\'"
;;   :hook ((haskell-mode . eglot-ensure)
;; 		 (haskell-mode . interactive-haskell-mode)
;; 		 (haskell-mode . haskell-doc-mode))
;; 		 ;; (haskell-mode . hindent-mode)
;;   :custom (haskell-stylish-on-save t)
;;   :bind ("C-c C-c" . haskell-compile))
  ;; :config
  ;; (require 'lsp-haskell))
;; (use-package lsp-haskell
;;   :ensure t
;;   :config
;;   (setq lsp-haskell-process-path-hie "ghcide")
;;   (setq lsp-haskell-process-args-hie '()))

;; (use-package go-mode
;;   :hook (go-mode . eglot-ensure)
;;   :mode "\\.go\\'")
(setup (:elpaca go-mode)
  (:file-match "\\.go\\'")
  (:hook eglot-ensure))

;;(use-package v-mode
;;  :straight (v-mode
;;             :type git
;;             :host github
;;             :repo "damon-kwok/v-mode"
;;             :files ("tokens" "v-mode.el"))
;;  :config
;;  :bind-keymap
;;  ("M-z" . v-menu)
;;  ("<f6>" . v-menu)
;;  ("C-c C-f" . v-format-buffer)
;;  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))

;; (use-package gdscript-mode
;;   :mode "\\.gd\\'")

;; (use-package vterm
;;   :commands (vterm vterm-other-window))
(setup (:elpaca vterm))

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
