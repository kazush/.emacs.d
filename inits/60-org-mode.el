(use-package org
  :commands (org-agenda org-iswitchb org-capture)
  :bind (("C-c o a" . org-agenda)
         ("C-c o b" . org-iswitchb)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link)
         )
  :init
  (global-set-key "\C-coj" '(lambda () (interactive) (org-capture nil "j")))
  (global-set-key "\C-com" '(lambda () (interactive) (org-capture nil "m")))
  (global-set-key "\C-cot" '(lambda () (interactive) (org-capture nil "t")))
  :config
  (setq org-src-window-setup 'other-window)
  (setq org-agenda-window-setup 'other-window)
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-emphasis-markers t)
  (setq org-capture-templates
        '(("j" "Journal"
           entry (file+datetree "~/org/journal.org")
           "* %?\n\n  %i\n\n  From: %a")
          ("m" "Memo"
           entry (file "~/org/notes.org")
           "* %? %T\n\n  %i\n\n  From: %a")
          ("t" "Todo"
           entry (file "~/org/todo.org")
           "* TODO %?\n  %i\n  %a")
          ))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
  (setq org-log-done 'time)
  (setq org-agenda-files (list org-directory))
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  (setq org-tag-alist
        '(("PROJECT" . ?p) ("TECH" . ?t) ("MONEY" . ?m)))
  (setq org-agenda-custom-commands
        '(("x" "Unscheduled TODO" tags-todo "-SCHEDULED>=\"<now>\"" nil)))
  (setq org-stuck-projects
        '("+PROJECT/-DONE-SOMEDAY" ("TODO" "WAIT")))
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 ()
  ;;                                 (compose-region (match-beginning 1)
  ;;                                                 (match-end 1) "•"))))))
  ;; enable babel for languages
  (require 'ob-shell)
  (require 'ob-java)
  (require 'ob-python)
  (require 'ob-C)
  (require 'ob-emacs-lisp)
  (require 'ob-org)
  (require 'ob-awk)
  (require 'ob-sed)
  (require 'ob-js)
  (require 'ob-css)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  )

(use-package ob-ipython
  :disabled)
(use-package ob-go)

;; helm support
(use-package helm-org-rifle
  :if my/enable-helm)

;; ob-async
(use-package ob-async)

;; org-bullets
(use-package org-bullets
  :config
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "○" "►" "◇"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-cliplink
(use-package org-cliplink
  :bind (("C-c o L" . org-cliplink)))

(use-package ox-hugo
  :after ox
  :config
  (setq org-hugo-default-section-directory "posts"))
