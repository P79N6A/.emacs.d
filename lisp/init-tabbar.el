(use-package tabbar :ensure t)
(require 'tabbar)
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (find (aref (buffer-name buffer) 0) " *"))
         (buffer-list))))
(setq tabbar-cycle-scope 'tabs)
(setq *tabbar-ignore-buffers* '("idle.org" ".bbdb" "diary" "TAGS*"))

(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(setq tabbar-use-images nil)
(setq tabbar-separator '(2.0))
(set-face-attribute
 'tabbar-default nil
 :background "brightblack"
 :foreground "white"
 )
(set-face-attribute
 'tabbar-selected nil
 :background "#ff5f00"
 :foreground "brightwhite"
 :box nil
 )
(set-face-attribute
 'tabbar-modified nil
 :background "brightred"
 :foreground "brightwhite"
 :box nil
 )

(defun liubang-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
     Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ;; ((member (buffer-name)
    ;;          '("*scratch*" "*Messages*" "*Help*"))
    ;;  "Common"
    ;;  )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Common"
     )
    ((member (buffer-name)
             '("xyz" "day" "m3" "abi" "for" "nws" "eng" "f_g" "tim" "tmp"))
     "Main"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Common"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

;; (when window-system                       ; GUI時
;;   ;; 外観変更
;;   (set-face-attribute
;;    'tabbar-default nil
;;    ;;:family "MeiryoKe_Gothic"
;;    :background "#34495E"
;;    :foreground "#EEEEEE"
;;    :height 1
;;    )
;;   (set-face-attribute
;;    'tabbar-unselected nil
;;    :background "#34495E"
;;    :foreground "#EEEEEE"
;;    :box nil
;;    )
;;   (set-face-attribute
;;    'tabbar-modified nil
;;    :background "#E67E22"
;;    :foreground "#EEEEEE"
;;    :box nil
;;    )
;;   (set-face-attribute
;;    'tabbar-selected nil
;;    :background "#E74C3C"
;;    :foreground "#EEEEEE"
;;    :box nil)
;;   (set-face-attribute
;;    'tabbar-button nil
;;    :box nil)
;;   (set-face-attribute
;;    'tabbar-separator nil
;;    :height 2.0)
;;   )

(tabbar-mwheel-mode nil)
(setq tabbar-buffer-groups-function 'liubang-tabbar-buffer-groups)

(tabbar-mode t)
(provide 'init-tabbar)
