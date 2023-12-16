

;; Unoccupied
(defface rune-modeline-default-face
  '((t :background "#111f33" :foreground "white"))
  "Rune modeline face for empty space")

;; Buffer name

;; (defface rune-modeline-buffer-face
;;   '((t :background "#153566" :foreground "#bf962f" :inherit bold))
;;   "Rune modeline face for displaying buffers.")

(defun rune-modeline-buffer-face ()
  "Return appropriate face or face list for `prot-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun rune-modeline--buffer-name ()
  "Helper function for customizing buffer name look"
  (format " %s " (buffer-name)))

(defvar-local rune-modeline-buffer-name
    '(:eval
      (propertize (rune-modeline--buffer-name) 'face (rune-modeline-buffer-face)))
  "Rune modeline buffer name segment")

;; Major mode

(defun rune-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun rune-modeline-major-mode-help-echo ()
  (format "Major Mode:%s" (rune-modeline--major-mode)))

(defface rune-modeline-major-mode-face
  '((t :foreground "white" :inherit rune-modeline-default-face)) ;; 4a6082
  "Rune modeline face for displaying major mode")

(defun rune-modeline--major-mode ()
  "Helper function for customizing major mode look"
  (format " %s %s " (rune-modeline-major-mode-indicator) (capitalize (symbol-name major-mode))))

(defvar-local rune-modeline-major-mode
    '(:eval (propertize (rune-modeline--major-mode) 'mouse-face 'mode-line-highlight 'face 'rune-modeline-major-mode-face 'help-echo (rune-modeline-major-mode-help-echo)))
  "Rune modeline major mode segment")

;; Evil state

(defface rune-modeline-evil-face
  '((t :background "#3355bb" :foreground "white" :inherit rune-modeline-default-face))
  "Rune modeline face for displaying evil state.")

(defun rune-modeline--evil-state ()
  (capitalize (format " %s " (symbol-name evil-state))))

(defvar-local rune-modeline-evil-state
    '(:eval (when (mode-line-window-selected-p)
	      (propertize (rune-modeline--evil-state) 'face 'rune-modeline-evil-face))))

;; Git

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defface rune-modeline-git-branch-face
  '((t :foreground "white" :inherit rune-modeline-default-face))
  "Rune modeline face for displaying git branch")

(defun rune-modeline--git-branch ()
  "Helper function for getting git branch"
  (capitalize (format "  %s " (vc-git--symbolic-ref (buffer-file-name)))))

(defvar-local rune-modeline-git-branch '(:eval (when (mode-line-window-selected-p)
						  (propertize (rune-modeline--git-branch) 'face 'rune-modeline-git-branch-face ))))

;; Eglot

(defvar-local rune-modeline-eglot '(:eval (when (and (featurep eglot) (mode-line-window-selected-p))
					    '(eglot--managed-mode eglot--mode-line-format))))

;; Right align

(setq mode-line-right-align-edge '(+ right right-margin right-fringe))

;; Misc info

(defvar-local rune-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Miscalenous info")

;; Making vars available
(dolist (construct '(rune-modeline-major-mode
		     rune-modeline-buffer-name
		     rune-modeline-evil-state
		     rune-modeline-git-branch
		     rune-modeline-eglot
		     rune-modeline-misc-info))
  (put construct 'risky-local-variable t))

(defvar mode-line-right-align-edge 'right-edge)

(defvar-local rune-modeline-format
    '("%e"
      rune-modeline-evil-state
      rune-modeline-buffer-name
      rune-modeline-major-mode

      " "

      rune-modeline-eglot

      mode-line-format-right-align

      rune-modeline-git-branch

      rune-modeline-misc-info

      )
  "Full rune modeline")

(kill-local-variable 'mode-line-format)

(setq-default mode-line-format rune-modeline-format)

(setq mode-line-format rune-modeline-format)

(force-mode-line-update)
