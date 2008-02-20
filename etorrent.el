;; etorrent.el --- An Emacs Bittorrent Client
;; Author: Sverre Johansen (sverre.johansen@gmail.com)

;; Usage

;; M-x etorrent RET
(setq debug-on-error t)

;;; Code:
(defun etorrent ()
  "Bittorrent client for emacs"
  (interactive)
  (message "Welcome to Emact Bittorrent"))

(defun etorrent-parse-torrent (file)
  "Parses a bittorrent file into a Lisp datastructure"
  (interactive "fTorrent File: ")
  (etorrent-metainfo-to-hash file))

(defun etorrent-metainfo-to-hash (file)
  "Parses the torrent file and returns a hashtable"
  (let ((metainfo (make-hash-table :test 'equal)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
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
   ((equal (char-to-string (char-after)) "e")
    nil)
   ((string-match "[0-9]" (char-to-string (char-after)))
    (etorrent-bencode-parse-string))
   (t
    (message (concat "ERROR: " (char-to-string (char-after)))))))

(defun etorrent-bencode-parse-dict ()
  (message "Dict")
  (forward-char)
  (let ((dict (make-hash-table :test 'equal))
        (key (etorrent-bencode-to-value))
        (value (etorrent-bencode-to-value)))
    (while key
      (puthash key value dict)
      (setq key (etorrent-bencode-to-value))
      (setq value (etorrent-bencode-to-value)))
    dict))

(defun etorrent-bencode-parse-string ()
  "A byte string is encoded as <length>:<contents>"
  (let ((string-length (string-to-int
                         (buffer-substring (point)
                                           (- (search-forward ":") 1)))))
    (buffer-substring (point) (+ (point) string-length))))

(defun etorrent-bencode-parse-integer ()
  (message "Integer")
  nil)

(defun etorrent-bencode-parse-list ()
  (message "List")
  nil)

(defun etorrent-print-metainfo (metainfo)
  metainfo)