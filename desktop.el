(defun rune/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
  	(apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun rune/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun rune/exwm-update-title ()
  (pcase exwm-class-name
  	("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun rune/configure-window-by-class ()
  (interactive)
  ;; (message "Window '%s' appeared!" exwm-class-name)
  (pcase exwm-class-name
	("firefox" (exwm-workspace-move-window 2))))

(defvar rune/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun rune/kill-panel ()
  (interactive)
  (when rune/polybar-process
    (ignore-errors
      (kill-process rune/polybar-process))
  (setq rune/polybar-process nil)))

(defun rune/start-panel ()
  (interactive)
  (rune/kill-panel)
  (setq rune/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun rune/exwm-init-hook ()
  (exwm-workspace-switch-create 1)
  (rune/start-panel))
 
(defvar rune/exwm-started t)

(use-package exwm
  :defer nil
  :demand t
  :config
  (setq exwm-workspace-number 5)
  
  ;; Update names of the applications
  (add-hook 'exwm-update-class-hook #'rune/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'rune/exwm-update-title)
  
  (add-hook 'exwm-manage-finish-hook #'rune/configure-window-by-class)
  
  ;; Start some applications on init
  (add-hook 'exwm-init-hook #'rune/exwm-init-hook)

  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 32)
  (exwm-systemtray-enable) 
  
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
  		'(?\C-x
  		  ?\C-u
  		  ?\C-h
  		  ?\M-x
  		  ?\M-`
  		  ?\M-&
  		  ?\M-:
  		  ?\C-\M-j ;; Buffer list
  		  ?\C-\ ))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map [?\C-=] 'exwm-workspace-switch)
  
  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
		`(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)
		  
          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
		  
          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
		  
          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
		  
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
   						(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
   					(number-sequence 0 9))))
  
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")    
  
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  (exwm-enable))
