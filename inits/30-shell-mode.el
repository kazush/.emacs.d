;; shell (comint)
;; (setq explicit-bash-args '("--noediting" "-i" "-l"))

;; dirtrack using procfs
(defun shell-procfs-dirtrack (str)
  (prog1 str
    (when (string-match comint-prompt-regexp str)
      (let ((directory (file-symlink-p
                        (format "/proc/%s/cwd"
                                (process-id
                                 (get-buffer-process
                                  (current-buffer)))))))
        (when (file-directory-p directory)
          (cd directory))))))

(define-minor-mode shell-procfs-dirtrack-mode
  "Track shell directory by inspecting procfs."
  nil nil nil
  (cond (shell-procfs-dirtrack-mode
         (when (bound-and-true-p shell-dirtrack-mode)
           (shell-dirtrack-mode 0))
         (when (bound-and-true-p dirtrack-mode)
           (dirtrack-mode 0))
         (add-hook 'comint-preoutput-filter-functions
                   'shell-procfs-dirtrack nil t))
        (t
         (remove-hook 'comint-preoutput-filter-functions
                      'shell-procfs-dirtrack t))))

(add-hook 'shell-mode-hook '(lambda () (shell-procfs-dirtrack-mode 1)))

;; custom dir track list
;; (add-hook 'shell-mode-hook
;;           '(lambda ()
;;              (shell-dirtrack-mode 0)
;;              (dirtrack-mode 1)
;;              (setq dirtrack-list '("(..:..)\\((.+)\\)*\\([^\033()$#]+\\)" 2))
;;              (company-mode 0))
;;           'APPEND)

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
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")
              (setq-local xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
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

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-dedicated-close-back-to-open-buffer-p nil)
  (setq multi-term-dedicated-select-after-open-p t)
  (setq multi-term-program "/bin/bash")
  (setq term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-u"))
  (setq term-bind-key-alist
        (append
         '(("C-c C-c" . term-send-raw)
           ("C-c C-x" . term-send-raw)
           ("C-c C-z" . term-send-raw)
           ("C-c C-h" . term-send-raw)
           ("C-c C-u" . term-send-raw)
           ("C-c C-k" . term-char-mode)
           ("C-c C-j" . term-line-mode))
         term-bind-key-alist))

  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-mode-map (kbd "C-a") 'term-bol)
              (define-key term-mode-map (kbd "C-c C-a") 'move-beginning-of-line)
              (setq-local term-prompt-regexp "^[^#$%>]*[#$%>] *")))

  ;; override multi-term to use display-buffer
  (defun multi-term ()
    "Create new term buffer.
Will prompt you shell name when you type `C-u' before this command."
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer (multi-term-get-buffer nil))
      (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (select-window (display-buffer term-buffer))))
  )

(defun my/shellish-buffer-p (buf)
  "Return if BUF is a shell-ish buffer."
  (let ((mode (with-current-buffer buf major-mode)))
    (and (string-match-p
          (rx bos "*" (or "term" "shell" "eshell") (* not-newline) "*" eos)
          (buffer-name buf))
         (seq-contains '(shell-mode eshell-mode term-mode) mode))))

(defvar my/helm-source-shellish-buffers-list
  (helm-make-source "Shell/Eshell/Term Buffers" 'helm-source-buffers
    :buffer-list
    (lambda ()
      (let ((buflist (mapcar
                      #'buffer-name
                      (cl-remove-if-not 'my/shellish-buffer-p (buffer-list)))))
        (message "cdr buflist:%s car buflist:%s" (cdr buflist) (car buflist))
        (append (cdr buflist) (list (car buflist)))))))

(defun my/last-shellish-buffer (buflist)
  "Return most recently used shell-ish buffer in BUFLIST."
  (when buflist
    (if (my/shellish-buffer-p (car buflist))
        (car buflist) (my/last-shellish-buffer (cdr buflist)))))

(defun my/chdir (dir)
  "Change directory to DIR."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc)))
    (goto-char pmark)
    (unless comint-process-echoes
      (insert (concat "cd " dir)) (insert "\n"))
    (sit-for 0)  ; force redisplay
    ;; (comint-send-string proc (concat "cd " dir "\n"))
    (comint-send-input)
    (set-marker pmark (point))))

(defun my/helm-shellish-buffers-list ()
  "Launch Helm buffers list with shell-ish buffers."
  (interactive)
  (my/helm-buffers-list my/helm-source-shellish-buffers-list))

(defvar my/shellish-last-buffer nil)
(defun my/get-shellish (arg shellfunc)
  "Switch to the shell-ish buffer last used or create new without prefix (ARG).
Close the window if the current buffer is already a shell-ish
buffer.  With prefix show Helm buffers list.  Create a new one if
double prefixes by calling SHELLFUNC."
  (interactive "p")
  (let ((cwd default-directory)
        (b (my/last-shellish-buffer (buffer-list (selected-frame)))))
    (cond ((or (not b) (= arg 64))
           (setq my/shellish-last-buffer (current-buffer))
           (funcall shellfunc))
          ((and (= arg 4) b)
           (select-window (display-buffer my/shellish-last-buffer)))
          ((= arg 16)
           (my/helm-shellish-buffers-list))
          ((my/shellish-buffer-p (current-buffer))
           (when my/shellish-last-buffer
             (delete-window (get-buffer-window b))
             (select-window (display-buffer my/shellish-last-buffer))))
          (b
           (setq my/shellish-last-buffer (current-buffer))
           (select-window (display-buffer b))))))

(defun my/newshell ()
  "Create a new shell with base directory name."
  (interactive)
  (if (my/last-shellish-buffer (buffer-list (selected-frame)))
      (shell (format "*shell<%s>*" (read-string "Shell buffer name: ")))
    (shell)))

(defun my/get-shell (arg)
  (interactive "p")
  (my/get-shellish arg 'my/newshell))

(defun my/get-term (arg)
  (interactive "p")
  (my/get-shellish arg 'multi-term))

;; Key bindings
(global-set-key (kbd "C-c s") 'my/get-shell)
(global-set-key (kbd "C-c t") 'my/get-term)
