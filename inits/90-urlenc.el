;; urlencode
(defvar urlencode-default-coding-system 'utf-8)
(defvar urlencode-exceptional-chars "[a-zA-Z0-9]")

(defun urlencode-region (begin end &optional cdp)
  (interactive "r\nP")
  (let* ((coding-system (and cdp (read-coding-system "Coding-system: ")))
         (encoded (urlencode (buffer-substring begin end) coding-system)))
    (delete-region begin end)
    (insert encoded)))

(defun urldecode-region (begin end &optional cdp)
  (interactive "r\nP")
  (let* ((coding-system (and cdp (read-coding-system "Coding-system: ")))
         (decoded (urldecode (buffer-substring begin end) coding-system)))
    (delete-region begin end)
    (insert decoded)))

(defun urlencode-string (str &optional cdp)
  (interactive "sString: \nP")
  (let ((coding-system (and cdp (read-coding-system "Coding-system: "))))
    (insert (urlencode str coding-system))))

(defun urldecode-string (str &optional cdp)
  (interactive "sString: \nP")
  (let ((coding-system (and cdp (read-coding-system "Coding-system: "))))
    (insert (urldecode str coding-system))))

(defun urlencode (str &optional coding-system)
  (mapconcat
   (lambda (c)
     (format (if (string-match urlencode-exceptional-chars (char-to-string c))
                 "%c" "%%%02X") c))
   (encode-coding-string str
                         (or coding-system urlencode-default-coding-system))
   ""))

(defun urldecode (str &optional coding-system)
  (let (pos
        decoded)
    (while (setq pos (string-match "%.." str))
      (setq decoded
            (concat decoded (substring str 0 pos)
                    (format "%c"
                      (string-to-int (substring str (+ pos 1) (+ pos 3)) 16)))
            str (substring str (+ pos 3))))
    (decode-coding-string (concat decoded str)
                          (or coding-system urlencode-default-coding-system))))
