(require 'cc-mode)
(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (require 'use-package)))

(use-package projectile :ensure t)
(use-package treemacs :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp
  :after  company
  :ensure t
  :config
  (add-hook 'java-mode-hook (lambda () (push 'company-lsp company-backends)))
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))
;;(push 'java-mode company-global-modes))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point))

(setq lsp-java-server-install-dir "~/.emacs.d/javalibs/jdt")
(use-package lsp-java
  :ensure t
  :after lsp
  :requires (lsp-ui-flycheck lsp-ui-sideline)
  :init
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all t
        ;;lsp-java-vmargs '("-noverify" "-Xmx2G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-Dhttp.proxyHost=dev-proxy.oa.com" "-Dhttp.proxyPort=8080" "-Dhttps.proxyHost=dev-proxy.oa.com" "-Dhttps.proxyPort=8080")
        lsp-java-trace-server "messages"
        lsp-java-format-comments-enabled t
        lsp-java-format-enabled t
        lsp-java-auto-build nil
        indent-tabs-mode nil
        lsp-highlight-symbol-at-point t
        lsp-java-server-install-dir "~/.emacs.d/javalibs/jdt"
        )
  :config
  (add-hook 'java-mode-hook (lambda
                              (setq tab-width 2)
                              (hs-minor-mode t)
                              (yas-minor-mode t)
                              (lsp-ui-sideline-mode t)
                              (highlight-symbol-mode t)
                              (set-face-attribute 'region nil :background "#99CC00" :foreground "#ffffff")
                              (set-face-attribute 'highlight-symbol-face nil :background "#99CC00" :underline t)
                              (lsp-ui-flycheck-enable t)
                              ;;key binds
                              (local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
                              (local-set-key (kbd "C-t") 'lsp-ui-peek-find-implementation)
                              (local-set-key (kbd "M-g") 'lsp-ui-peek-find-references)
                              (local-set-key (kbd "C-c i") 'lsp-java-add-import)
                              ))
  )

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (require 'dap-java)
  (dap-ui-mode t))

(require 'lsp-java-treemacs)
;;(use-package dap-java :after (lsp-java))
;;(use-package lsp-java-treemacs :after (treemacs))

;; (defhydra hydra-meghanada (:hint nil :exit t)
;;   "
;; ^Edit^                           ^Tast or Task^
;; ^^^^^^-------------------------------------------------------
;; _f_: meghanada-compile-file      _m_: meghanada-restart
;; _c_: meghanada-compile-project   _t_: meghanada-run-task
;; _o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
;; _s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
;; _v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
;; _i_: meghanada-import-all        _r_: meghanada-reference
;; _g_: magit-status                _T_: meghanada-typeinfo
;; _l_: helm-ls-git-ls
;; _q_: exit
;; "
;;   ("f" meghanada-compile-file)
;;   ("m" meghanada-restart)

;;   ("c" meghanada-compile-project)
;;   ("o" meghanada-optimize-import)
;;   ("s" meghanada-switch-test-case)
;;   ("v" meghanada-local-variable)
;;   ("i" meghanada-import-all)

;;   ("g" magit-status)
;;   ("l" helm-ls-git-ls)

;;   ("t" meghanada-run-task)
;;   ("T" meghanada-typeinfo)
;;   ("j" meghanada-run-junit-test-case)
;;   ("J" meghanada-run-junit-class)
;;   ("R" meghanada-run-junit-recent)
;;   ("r" meghanada-reference)

;;   ("q" exit)
;;   ("z" nil "leave"))

(provide 'init-java)
