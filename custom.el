;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "liubangchen")           ; User full name
(setq centaur-mail-address "liubangchen@tencent.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
(setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china netease or tuna
(setq centaur-theme 'classic)                  ; Color theme: default, classic, doom, dark, light or daylight,monokai
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
(setq centaur-lsp 'lsp-mode)                         ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))
(setq user-full-name "liubangchen")
(setq user-mail-address "liubangchen@tencent.com")
(setq default-directory "~/")
(prefer-coding-system 'utf-8)

(unless (boundp 'confirm-kill-processes) ;; new on Emacs 26
  (advice-add 'save-buffers-kill-emacs :before
              (lambda (&rest _)
                (defun processes-with-query (process)
                  (and (memq (process-status process) '(run stop open listen))
                       (process-query-on-exit-flag process)))
                (let ((processes (seq-filter 'processes-with-query (process-list))))
                  (dolist (process processes)
                    (set-process-query-on-exit-flag process nil)))))
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))


;; paste and copy
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogramx-paste-function 'copy-from-osx)

;; 语法高亮
(global-font-lock-mode t)
;; 以 y/n代表 yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; 显示括号匹配
(scroll-bar-mode 0)
(global-hl-line-mode 1)
(tool-bar-mode 0)
(show-paren-mode t)
(setq show-paren-style 'parentheses)
;;;; 显示时间，格式如下
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq inhibit-startup-message t)
(setq display-time-day-and-date t)
(transient-mark-mode t)
;; 支持emacs和外部程序的粘贴
(setq x-select-enable-clipboard t)
;; 不产生备份文件
(setq make-backup-files nil)
;;不产生临时文件
(setq-default make-backup-files nil)

(setq-default cursor-type '(bar . 1))
;;设置光标形状
(setq default-cursor-type 'bar)
(set-cursor-color "#ffffff")                    ;;设置备份文件路径
(setq backup-directory-alist (quote (("." . "~/backups"))))

(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (setq coding-str
            (concat (substring coding-str 0 (match-beginning 0)) "-unix"))
      (message "CODING: %s" coding-str)
      (set-buffer-file-coding-system (intern coding-str)) )))
(add-hook 'find-file-hooks 'no-junk-please-were-unixish)

(define-key isearch-mode-map [escape] 'isearch-abort)   ;;;; isearch
(global-set-key [escape] 'keyboard-escape-quit)         ;;;; everywhere else
;;设置选中区域的颜色
(set-face-attribute 'region nil :background "#99CC00" :foreground "#ffffff")
(setq helm-locate-command "locate %s  -d /Users/chenlong/locate.database -l 100  %s")
(setq yas-snippet-dirs '("~/.emacs.d/snippets/emacs-snippets"))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;(toggle-frame-maximized)
;;(add-to-list 'load-path "/Users/chenlong/.emacs.d/meghanada/meghanada-emacs-1.0.7")
(global-eldoc-mode -1)
(eldoc-mode -1)

;; Fonts
(defvar emacs-font-size-pair '(15  . 13)
  "Default font size pair for (english . chinese)")
(defun font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."

  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

(when (display-graphic-p)
  ;; Set default fonts
  (cond
   ;;((member "Inconsolata" (font-family-list))
   ;;(set-face-attribute 'default nil :font "Inconsolata"))
   ;;(set-font "Inconsolata" "Inconsolata" emacs-font-size-pair))
   ((member "Source Code Pro" (font-family-list))
    (set-face-attribute 'default nil :font "Source Code Pro"))
   ((member "Menlo" (font-family-list))
    (set-face-attribute 'default nil :font "Menlo"))
   ((member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco"))
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
   ((member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas")))

  (cond
   (sys/mac-x-p
    (set-face-attribute 'default nil :height 130))
   (sys/win32p
    (set-face-attribute 'default nil :height 110)))

  ;; Specify fonts for all unicode characters
  (cond
   ((member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
   ((member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

  ;; Specify fonts for Chinese characters
  (cond
   ((member "WenQuanYi Micro Hei" (font-family-list))
    (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
   ((member "Microsoft Yahei" (font-family-list))
    (set-fontset-font t '(#x4e00 . #x9fff) "Microsoft Yahei")))
  )

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
