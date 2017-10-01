;; shell (comint)
(setq explicit-bash-args '("--noediting" "-i" "-l"))
;; custom dir track list
(add-hook 'shell-mode-hook
          '(lambda ()
             (shell-dirtrack-mode 0)
             (dirtrack-mode 1)
             (setq dirtrack-list '("(..:..)\\((.+)\\)*\\([^\033()$]+\\)" 2))
             (company-mode 0))
          'APPEND)

;; custom password prompt regexp
(setq comint-password-prompt-regexp
   "\\(^ *\\|\\( *Password\\| *SSO\\| *IronKey\\| SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) *\\)\\(?:\\(?:adgangskode\\|contrase\\(?:\\(?:ny\\|ñ\\)a\\)\\|geslo\\|h\\(?:\\(?:asł\\|esl\\)o\\)\\|iphasiwedi\\|jelszó\\|l\\(?:ozinka\\|ösenord\\)\\|m\\(?:ot de passe\\|ật khẩu\\)\\|[Pp]a\\(?:rola\\|s\\(?:ahitza\\|s\\(?: phrase\\|code\\|ord\\|phrase\\|wor[dt]\\)\\|vorto\\)\\)\\|s\\(?:alasana\\|enha\\|laptažodis\\)\\|wachtwoord\\|лозинка\\|пароль\\|ססמה\\|كلمة السر\\|गुप्तशब्द\\|शब्दकूट\\|গুপ্তশব্দ\\|পাসওয়ার্ড\\|ਪਾਸਵਰਡ\\|પાસવર્ડ\\|ପ୍ରବେଶ ସଙ୍କେତ\\|கடவுச்சொல்\\|సంకేతపదము\\|ಗುಪ್ತಪದ\\|അടയാളവാക്ക്\\|රහස්පදය\\|ពាក្យសម្ងាត់\\|パスワード\\|密[码碼]\\|암호\\)\\|Response\\)\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: for .+\\)?[:：៖]\\s *\\'")

;; xterm-color
(use-package xterm-color
  :ensure t
  :config
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output
                comint-output-filter-functions))
  (setq compilation-environment '("TERM=xterm-256color"))
  (add-hook 'compilation-start-hook
          (lambda (proc)
            ;; We need to differentiate between compilation-mode buffers
            ;; and running as part of comint (which at this point we assume
            ;; has been configured separately for xterm-color)
            (when (eq (process-filter proc) 'compilation-filter)
              ;; This is a process associated with a compilation-mode buffer.
              ;; We may call `xterm-color-filter' before its own filter function.
              (set-process-filter
               proc
               (lambda (proc string)
                 (funcall 'compilation-filter proc
                          (xterm-color-filter string))))))))

;; shell-pop
;; (use-package shell-pop
;;   :disabled
;;   :ensure t
;;   :bind (("C-c e" . shell-pop))
;;   :config
;;   (setq shell-pop-universal-key "\C-ce")
;;   (setq shell-pop-window-size 40)
;;   (setq shell-pop-window-position "bottom")
;;   (setq shell-pop-full-span t))

;; To get the shell buffer in the same buffer.
;; (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;; Key bindings
(global-set-key "\C-cs" 'shell)
