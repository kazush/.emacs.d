(use-package org
  :ensure t
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
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-hide-leading-stars t)
  (setq org-startup-truncated nil)
  (setq org-startup-folded nil)
  (setq org-capture-templates
        '(("j" "Journal"
           entry (file+datetree (concat org-directory "/journal.org"))
           "* %?\n\n  %i\n\n  From: %a")
          ("m" "Memo"
           entry (file nil)
           "* %? %T\n\n  %i\n\n  From: %a")
          ("t" "Todo"
           entry (file (concat org-directory "/todo.org"))
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
  (setq org-src-fontify-natively t)
  ;; enable babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)))
  )

;; helm support
(use-package helm-org-rifle :ensure t)

;; ob-async
(use-package ob-async
  :ensure t
  :config
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block))
