;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package org
  :ensure nil
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :hook (org-indent-mode . (lambda()
                             (require 'ox-taskjuggler)
                             (set-face-attribute 'org-level-1 nil :height 1.2 :bold t)
                             (set-face-attribute 'org-level-2 nil :height 1.1 :bold t)
                             (set-face-attribute 'org-level-3 nil :height 1.0 :bold t)
                             (setq org-taskjuggler-default-reports '("include \"/Users/chenlong/.emacs.d/tpl/reports.tji\""))
                             ;;(set-face-attribute 'region nil :background "#99CC00" :foreground "#ffffff")
                             (diminish 'org-indent-mode)))
  :config
  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(T)" "DOING(I)" "HANGUP(H)" "|" "DONE(D)" "CANCEL(C)")
                            (sequence "⚑(t)" "🏴(i)" "❓(h)" "|" "✔(d)" "✘(c)"))
        org-todo-keyword-faces '(("HANGUP" . warning)
                                 ("❓" . warning))
        org-log-done 'time
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities t
        org-hide-emphasis-markers t)

  (add-to-list 'org-export-backends 'md)

  ;; More fancy UI
  (use-package org-bullets
    :if (char-displayable-p ?◉)
    :hook (org-mode . org-bullets-mode))

  (use-package org-fancy-priorities
    :diminish
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (unless (char-displayable-p ?❗)
      (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-list))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
                ("C-<f7>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  ;; Pomodoro
  (use-package org-pomodoro
    :after org-agenda
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))

  ;; Visually summarize progress
  (use-package org-dashboard)

  (eval-and-compile
    (defun hot-expand (str &optional mod)
      "Expand org template."
      (let (text)
        (when (region-active-p)
          (setq text (buffer-substring (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end)))
        (insert str)
        (org-try-structure-completion)
        (when mod (insert mod) (forward-line))
        (when text (insert text)))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
_c_enter  qu_o_te     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   p_y_thon        _i_ndex:
_a_scii   _v_erse     ip_Y_thon       _I_NCLUDE:
_s_rc     _g_o        _r_uby          _H_TML:
_h_tml    _S_HELL     _p_erl          _A_SCII:
^ ^       ^ ^         _P_erl tangled  plant_u_ml
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("o" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("y" (hot-expand "<s" "python :results output"))
    ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0"))
    ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)"))
    ("p" (hot-expand "<s" "perl"))
    ("r" (hot-expand "<s" "ruby"))
    ("S" (hot-expand "<s" "sh"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
    ("P" (progn
           (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
           (hot-expand "<s" "perl")))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("q" nil "quit"))

  (bind-key "<"
            (lambda () (interactive)
              (if (or (region-active-p) (looking-back "^\s*" 1))
                  (hydra-org-template/body)
                (self-insert-command 1)))
            org-mode-map))

;;latex
;; 使用xelatex一步生成PDF
;;window 1.安装MiKTeX 2.9
;;http://orgmode.org/worg/org-tutorials/org-latex-export.html
;;http://blog.qinjian.me/2013/02/20/chinese_latex_in_org_mode/
;;http://orgmode.org/manual/LaTeX-specific-attributes.html
;;https://github.com/huwenbiao/hwb-notes/blob/master/Org/emacs/org-mode.org
;;http://kuanyui.github.io/2014/05/10/emacs-org-mode-xelatex-output-chinese-pdf/
;; Include the latex-exporter
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-export-with-sub-superscripts nil)
(setq org-latex-minted-options
      '(("frame" "none") ("linenos=false") ("xleftmargin=1.5cm") ("bgcolor=lightgray") ("breaklines=true") ("breakanywhere=true")))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(setq org-image-actual-width nil)
;;(setq org-latex-classes nil)
(setq org-latex-pdf-process
      '("xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"))


(setq org-latex-with-hyperref t)
(setq org-src-fontify-natively 't)

(org-add-link-type
 "latex" nil
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:grey;\">%s</span>" desc))
    ((eq format 'latex)
     (format "\\%s{%s}" path desc)))))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[12pt, a4paper]{article}
\\usepackage{xeCJK}
\\usepackage{minted}
\\usepackage{fontspec}
\\setmainfont{微软雅黑}
\\setCJKmainfont{微软雅黑}
\\setCJKsansfont{微软雅黑}
\\setCJKmonofont{微软雅黑}
\\usepackage[dvipsnames]{xcolor}
\\usepackage[margin=1.6cm]{geometry}
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.36}
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("ctexart"
               "\\documentclass[12pt, a4paper]{ctexart}
\\usepackage{xeCJK}
\\setCJKmainfont{Microsoft YaHei}
\\setCJKsansfont{Microsoft YaHei}
\\usepackage[margin=1.6cm]{geometry}
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.36}
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(setq org-beamer-outline-frame-title "目录")
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[dvipdfmx,presentation]{beamer}
\\usepackage{xeCJK}
\\usepackage{fontspec}
\\setmainfont{Microsoft YaHei}
\\setCJKmainfont{Microsoft YaHei}
\\setCJKsansfont{Microsoft YaHei}
\\setCJKmonofont{Microsoft YaHei}
               [NO-DEFAULT-PACKAGES] [PACKAGES] [EXTRA]"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))


(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             )

(defcustom org-mactions-numbered-action-format "TODO Action #%d "
  "Default structure of the headling of a new action.
    %d will become the number of the action."
  :group 'org-edit-structure
  :type 'string)

(defcustom org-mactions-change-id-on-copy t
  "Non-nil means make new IDs in copied actions.
If an action copied with the command `org-mactions-collect-todos-in-subtree'
contains an ID, that ID will be replaced with a new one."
  :group 'org-edit-structure
  :type 'string)


(defun org-mactions-new-numbered-action (&optional inline)
  "Insert a new numbered action, using `org-mactions-numbered-action-format'.
    With prefix argument, insert an inline task."
  (interactive "P")
  (let* ((num (let ((re "\\`#\\([0-9]+\\)\\'"))
                (1+ (apply 'max 0
                           (mapcar
                            (lambda (e)
                              (if (string-match re (car e))
                                  (string-to-number (match-string 1 (car e)))
                                0))
                            (org-get-buffer-tags))))))
         (tag (concat "#" (number-to-string num))))
    (if inline
        (org-inlinetask-insert-task)
      (org-insert-heading 'force))
    (unless (eql (char-before) ?\ ) (insert " "))
    (insert (format org-mactions-numbered-action-format num))
    (org-toggle-tag tag 'on)
    (if (= (point-max) (point-at-bol))
        (save-excursion (goto-char (point-at-eol)) (insert "\n")))
    (unless (eql (char-before) ?\ ) (insert " "))))


(defun org-mactions-collect-todos-in-subtree ()
  "Collect all TODO items in the current subtree into a flat list."
  (interactive)
  (let ((buf (get-buffer-create "Org TODO Collect"))
        (cnt 0) beg end string s)
    (with-current-buffer buf (erase-buffer) (org-mode))
    (org-map-entries
     (lambda ()
       (setq beg (point) end (org-end-of-subtree t t) cnt (1+ cnt)
             string (buffer-substring beg end)
             s 0)
       (when org-mactions-change-id-on-copy
         (while (string-match "^\\([ \t]*:ID:\\)[ \t\n]+\\([^ \t\n]+\\)[ \t]*$"
                              string s)
           (setq s (match-end 1)
                 string (replace-match (concat "\\1 "
                                               (save-match-data (org-id-new)))
                                       t nil string))))
       (with-current-buffer buf (org-paste-subtree 1 string)
                            (goto-char (point-max))))
     (format "TODO={%s}" (regexp-opt org-not-done-keywords))
     'tree)
    (if (= cnt 0)
        (message "No TODO items in subtree")
      (message "%d TODO entries copied to kill ring" cnt)
      (prog1 (with-current-buffer buf
               (kill-new (buffer-string)))
        (kill-buffer buf)))))


(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;;GDT代办任务设置

(setq org-default-notes-file "~/GTD/inbox.org")
(setq org-directory "~/GTD/")
(setq org-capture-templates
      '(
        ("t" "待办任务" entry (file+olp "~/GTD/inbox.org" "EMR" "计划任务")
         "* TODO %?  \n " :empty-lines 1)
        ("p" "组件特性" entry (file+olp "~/GTD/inbox.org" "EMR" "组件特性")
         "* TODO %?  \n " :empty-lines 1)
        ("n" "笔记待办" entry (file+olp "~/GTD/inbox.org" "笔记")
         "*  %?  \n " :empty-lines 1)
        ))
;;显示他们的内容
(setq org-agenda-files
      (list "~/GTD/inbox.org"
            "~/GTD/started.org"
            "~/GTD/finished.org"))
;; 将项目转接在各文件之间，方便清理和回顾。
(custom-set-variables
 '(org-refile-targets
   (quote
    (("inbox.org" :level . 1)("started.org" :level . 1) ("finished.org":level . 1))
    )))

;; ! : 切换到该状态时会自动添加时间戳
;; @ : 切换到该状态时要求输入文字说明
;; 如果同时设定@和!,使用@/!
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "DONE(d@)" "CANCELED(c@/!)")))
(setq org-log-done 'note)
;;颜色设置
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("STARTED" . "yellow") ("DONE" . "#32cd32")
        ("CANCELED" . (:foreground "blue" :weight bold))))

;;日期设置
(setq system-time-locale "zh_CN")
(custom-set-variables
 '(org-display-custom-times t)
 '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d %A>" . "<%Y %m %d  %A [%H:%M]>"))))
(format-time-string "%Y-%m-%d %A")

;;日历面板设置
(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '(
          (:name "待办任务"  ; Optionally specify section name
                 :time-grid t  ; Items that appear on the time grid
                 :tag "TODO"  :order 100)  ; Items that have this TODO keyword
          (:name "重要任务"
                 ;; Single arguments given alone
                 :and (:todo "TODO" :priority "A")
                 :time-grid t
                 :order 99 )
          (:name "进行中" :todo "STARTED"
                 ;; Show this group at the end of the agenda (since it has the
                 ;; highest number). If you specified this group last, items
                 ;; with these todo keywords that e.g. have priority A would be
                 ;; displayed in that group instead, because items are grouped
                 ;; out in the order the groups are listed.
                 :time-grid t
                 :order 98)
          ;; Set order of multiple groups at once
          (:name "Hive" :tag "hive")
          (:name "Hbase" :tag "hbase")
          (:name "Hadoop" :tag "hadoop")
          (:name "Presto" :tag "presto")))
  (org-super-agenda-mode))

;;画图
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (ditaa . t)
   (dot . t)))

(setq org-ditaa-jar-path
      (expand-file-name "~/.emacs.d/javalibs/ditaa0_9.jar"))

(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/javalibs/plantuml.jar"))
(setq org-confirm-babel-evaluate nil)


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
