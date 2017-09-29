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
  (global-set-key "\C-coj" '(lambda () (interactive) (org-capture nil "j")))
  (setq org-hide-leading-stars t)
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
  (setq org-src-fontify-natively t)
  (setq org-confirm-babel-evaluate nil)
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
)

;; helm support
(use-package helm-org-rifle :ensure t)

;; ob-async
(use-package ob-async
  :ensure t)

;; org-bullets
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
