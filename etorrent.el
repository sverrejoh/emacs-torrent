;;; etorrent.el --- An Emacs Bittorrent Client
;;; Author: Sverre Johansen (sverre.johansen@gmail.com)

;; Usage

;; M-x etorrent RET

;; Development Settings
(setq debug-on-error t)

(make-local-variable 'after-save-hook)
(add-hook 'after-save-hook (lambda () (eval-buffer)))

;;; Code: 

(require 'sha1)
(require 'calc)
(require 'calc-ext)

;;;
;;; external Emacs Torrent functions
;;;

(defun etorrent-download-file (file)
  (interactive "fTorrent File: ")
  (let ((metainfo (etorrent-parse-torrent file)))
    (etorrent-download-tracker-response metainfo)))

;;;
;;; internal Emacs Torrent functions
;;;

(defun etorrent-download-tracker-response (metainfo)
  (let* ((info-hash (etorrent-escape-url (sha1
                                          (etorrent-value-to-bencode
                                           (gethash "info" metainfo))
                                          nil nil t)))
         (peer-id "-AZ2060-QWERTYUIOPAS")
         (port "6969")
         (uploaded "0")
         (downloaded "0")
         (left "0")
         (url (concat (gethash "announce" metainfo)
                      "?"
                      "info_hash=" info-hash "&"
                      "peer_id=" peer-id "&"
                      "port=" port "&"
                      "uploaded=" uploaded "&"
                      "downloaded=" downloaded "&"
                      "left=" left))
         (url-request-method "GET"))
    (save-excursion
      (set-buffer (url-retrieve-synchronously url))
      (progn (goto-char (point-min))
             (delete-region (point-min) (search-forward "\n\n"))
             (etorrent-bencode-to-value)))))

(defun etorrent-parse-torrent (file)
  "Parses a bittorrent file into a Lisp datastructure"
  (etorrent-bencode-to-hash file))

(defun etorrent-bencode-to-hash (file)
  "Parses the torrent file and returns a hashtable"
  (let ((metainfo (make-hash-table :test 'equal)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (etorrent-bencode-to-value)))))

(defun etorrent-bencode-to-value ()
  (cond
   ((equal (char-to-string (char-after)) "d")
    (etorrent-bencode-parse-dict))
   ((equal (char-to-string (char-after)) "i")
    (etorrent-bencode-parse-integer))
   ((equal (char-to-string (char-after)) "l")
    (etorrent-bencode-parse-list))
   ((string-match "[0-9]" (char-to-string (char-after)))
    (etorrent-bencode-parse-string))
   ((equal (char-to-string (char-after)) "e")
    'end)
   (t
    (message (concat "ERROR: " (char-to-string (following-char)))))))

(defun etorrent-bencode-parse-dict ()
  "dictionary encoded as d<contents>e"
  (forward-char)
  (let ((dict (make-hash-table :test 'equal))
        (key (etorrent-bencode-to-value))
        (value (etorrent-bencode-to-value)))
    (while (not (eq key 'end))
      (puthash key value dict)
      (setq key (etorrent-bencode-to-value))
      (setq value (etorrent-bencode-to-value)))
    (forward-char)
    dict))

(defun etorrent-bencode-parse-string ()
  "A byte string is encoded as <length>:<contents>"
  (let* ((length-string
          (buffer-substring-no-properties (point) (- (search-forward ":") 1)))
         (string-length (string-to-int length-string))
         (string-start (point)))
    (forward-char string-length)
    (buffer-substring-no-properties string-start (+ string-start string-length))))

(defun etorrent-bencode-parse-integer ()
  "integer encoded as i<number in base 10 notation>e"
  (forward-char)
  (math-read-number (buffer-substring-no-properties
                     (point) (- (search-forward "e") 1))))

(defun etorrent-bencode-parse-list ()
  "list of values is encoded as l<contents>e"
  (forward-char)
  (let ((content nil)
        (value (etorrent-bencode-to-value)))
    (while (not (eq value 'end))
      ; FIXME: Better way to add a value to a list???
      (setq content (append content (list value)))
      (setq value (etorrent-bencode-to-value)))
    (forward-char)
    content))

(defun etorrent-value-to-bencode (value)
  "Encodes a lisp value into bencode"
  (cond
   ((math-numberp value)
    (concat "i" (math-format-number value) "e"))
   ((stringp value)
    (concat (number-to-string (length value)) ":" value))
   ((listp value)
    (concat "l"
            (mapconcat 'etorrent-value-to-bencode value "")
            "e"))
   ((hash-table-p value)
    ;(etorrent-hash-to-sorted-list value))
    (concat "d"
            (mapconcat '(lambda (x)
                          (concat (etorrent-value-to-bencode (first x))
                                  (etorrent-value-to-bencode (second x))))
                       (etorrent-hash-to-sorted-list value)
                       "")
            "e"))
   (t
    (debug-print-to-buffer value))))
                         
(defun etorrent-hash-to-sorted-list (hashtable)
  "Return a list that represent the hashtable."
  (let (mylist)
    (maphash (lambda (kk vv)
               (setq mylist (cons (list kk vv) mylist)))
             hashtable)
    (sort mylist (lambda (a b) (string< (car a) (car b))))))

;; Copied from emacs-wiki, which copied it from w3m-url-encode-string (w3m.el)
(defun etorrent-escape-url (str)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string into a list of chars.
	  (append (encode-coding-string (or str "") 'raw-text)
		  nil))))

(defun etorrent-file-to-string (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents file)
    (goto-char (point-min))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun hash-dump-content (hash)
  "Dumps the content of a hash as a key/value string"
  (let ((content ""))
    (maphash '(lambda (key value)
                (setq content
                      (concat content "Key:" key "\n" "Value: "
                              (prin1-to-string value) "\n\n")))
             hash)
    content))

;; Debug

(defun debug-print-to-buffer (msg)
  (with-current-buffer (get-buffer-create "etorrent-debug")
    (insert (concat (prin1-to-string msg) "\n"))))

;(hash-dump-content
; (etorrent-parse-torrent "/home/sverrej/Desktop/monsen.torrent"))

(defun save-string-to-file (filename string)
  (with-temp-buffer
    (insert string)
    (write-file filename)))