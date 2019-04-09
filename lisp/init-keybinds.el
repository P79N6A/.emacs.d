;;;;快捷键
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))

(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
;;(global-set-key (kbd "M-o") 'helm-find-files)
;;(global-set-key (kbd "C-c") 'kill-ring-save)
;;(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-.") 'revert-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-S-f") 'indent-whole)
;;(global-set-key (kbd "M-x") 'helm-smex)
;;(global-set-key (kbd "C-S-r") 'helm-locate)
;;(global-set-key (kbd "C-o") 'helm-imenu-anywhere)
;;(global-set-key (kbd "M-f") 'helm-ag-project-root)
;;(global-set-key (kbd "C-R") 'find-file-in-project)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "RET") 'newline)
(global-set-key (kbd "C-z") 'undo)
;;buffer跳转
(global-set-key (kbd "M-<left>") 'back-button-global-backward)
(global-set-key (kbd "M-<right>") 'back-button-global-forward)
(global-set-key (kbd "M-h") 'hs-toggle-hiding)


;; (defhydra hydra-pinky (:color pink :hint nil :exit t)
;;   "
;; ^Move^             ^Unmark^           ^Actions^          ^Search
;; ^^^^^^^^-----------------------------------------------------------------
;; _n_: next-line
;; _p_: previous-line
;; _f_: forward-char
;; _b_: backward-char
;; _v_: scroll-up-command
;; _V_: scroll-down-command
;; "
;;   ("n" next-line)
;;   ("p" previous-line)
;;   ("f" forward-char)
;;   ("b" backward-char)
;;   ("a" beginning-of-line)
;;   ("e" move-end-of-line)
;;   ("v" scroll-up-command)
;;   ("V" scroll-down-command)
;;   ("g" keyboard-quit)
;;   ("j" git-gutter:next-hunk)
;;   ("k" git-gutter:previous-hunk)
;;   ("o" other-window-or-split)
;;   ("l" recenter-top-bottom)
;;   ("s" save-buffer)
;;   ("S" window-swap-states)
;;   ("q" kill-buffer)
;;   ("<" beginning-of-buffer)
;;   (">" end-of-buffer)
;;   ("SPC" set-mark-command)
;;   ("1" delete-other-windows)
;;   ("2" split-window-below)
;;   ("3" split-window-right)
;;   ("0" delete-window)
;;   ("x" delete-window))

;; ;; key-chord
;; (bind-key "C-'" 'hydra-pinky/body)
(provide 'init-keybinds)
