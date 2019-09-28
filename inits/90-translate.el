(use-package xah-lookup
  :bind (("C-c L j" . my-xah-lookup-weblio)
         ("C-c L a" . my-xah-lookup-all-dictionaries)
         ("C-c L g" . my-xah-lookup-google)
         ("C-c L w" . my-xah-lookup-wikipedia))
  :commands (xah-lookup-word-on-internet
             xah-lookup-google
             xah-lookup-all-dictionaries
             xah-lookup-wikipedia)
  :config
  (put 'xah-lookup-google 'xah-lookup-browser-function 'browse-url)
  :init
  (setq xah-lookup-browser-function 'eww)
  (setq xah-lookup-dictionary-list
        [
         "http://www.google.com/search?q=define:+word02051"
         "http://ejje.weblio.jp/content/word02051"
         "http://www.thefreedictionary.com/word02051"
         "http://en.wiktionary.org/wiki/word02051"
         ])
  (defun xah-lookup-wrapper (name lookup-func arg)
    (let ((word (cond ((stringp arg) arg)
                      ((= arg 4)
                       (read-string (concat name " lookup: "))))))
      (funcall lookup-func word)))
  (defun my-xah-lookup-weblio (arg)
    (interactive "p")
    (xah-lookup-wrapper "Weblio" 'xah-lookup-weblio arg))
  (defun my-xah-lookup-google (arg)
    (interactive "p")
    (xah-lookup-wrapper "Google" 'xah-lookup-google arg))
  (defun my-xah-lookup-all-dictionaries (arg)
    (interactive "p")
    (xah-lookup-wrapper "All dict" 'xah-lookup-all-dictionaries arg))
  (defun my-xah-lookup-wikipedia (arg)
    (interactive "p")
    (xah-lookup-wrapper "Wikipedia" 'xah-lookup-wikipedia arg))
  (defun xah-lookup-weblio (&optional word)
    "Lookup definition of current word or text selection in URL `http://ejje.weblio.jp/'"
    (interactive)
    (xah-lookup-word-on-internet
     word
     "http://ejje.weblio.jp/content/word02051"))
  )

(defadvice eww-render (around eww-render-popwin activate)
  (save-window-excursion ad-do-it)
  (unless (get-buffer-window "*eww*")
    (pop-to-buffer "*eww*")))
(push "*eww*" popwin:special-display-config)

(use-package google-translate
  :ensure t
  :bind (("C-c L t" . google-translate-enja))
  :commands (google-translate-translate google-translate-get-string)
  :init
  ;; Borrowed from http://qiita.com/styzo/items/72197ad6717eb9266315
  (defun google-translate-get-string (arg)
    (or (cond ((stringp arg) arg)
              ((= arg 4)          ;C-u
               (thing-at-point 'sentence))
              ((= arg 16)         ;C-u C-u
               (thing-at-point 'paragraph))
              ((= arg 64)         ;C-u C-u C-u
               (read-string "Google Translate: "))
              ((use-region-p)         ;リージョン指定
               (buffer-substring (region-beginning) (region-end)))
              (t              ;デフォルト
               (thing-at-point 'word)))
        ""))
  (defun google-translate-enja (arg)
    "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
    (interactive "p")
    (let* ((string (google-translate-get-string arg)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate "en" "ja" string)))
  (defun google-translate-jaen (arg)
    "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
    (interactive "p")
    (let* ((string (google-translate-get-string arg)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate "ja" "en" string)))
  (push '("*Google Translate*" :height 0.5 :stick t) popwin:special-display-config))
