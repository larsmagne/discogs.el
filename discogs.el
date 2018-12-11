;;; discogs.el --- discogs interface
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; discogs.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'json)

(defvar discogs-consumer-key nil
  "Get the key at discogs.com.")

(defvar discogs-consumer-secret nil
  "Get the secret at discogs.com.")

(defun discogs-query (object identifier)
  (let ((url-request-extra-headers
	 '(("User-Agent" . "discogs.el/1.0 +http://lars.ingebrigtsen.no/"))))
    (with-current-buffer
	(url-retrieve-synchronously
	 (format "https://api.discogs.com/%s/%s&key=%s&secret=%s"
		 object identifier
		 discogs-consumer-key
		 discogs-consumer-secret))
      (goto-char (point-min))
      (prog1
	  (when (re-search-forward "\r?\n\r?\n" nil t)
	    (json-read))
	(kill-buffer (current-buffer))))))

(defun discogs-clean (string)
  (replace-regexp-in-string "[?&-.,]" " " string))

(defun discogs-search (artist title)
  (setq artist (discogs-clean artist)
	title (discogs-clean title))
  (let ((results
	 (discogs-query
	  "database" (format "search?type=master&artist=%s&release_title=%s"
			     artist title))))
    (if (plusp (length (cdr (assoc 'results results))))
	results
      ;; Rate-limit to the API limit.
      (sleep-for 1)
      (discogs-query
       "database" (format "search?type=release&q=%s %s"
			  artist title)))))

(defun discogs-find-year (artist title)
  (let ((data (discogs-search artist title)))
    (when (eq (caar data) 'results)
      (loop for release across (cdar data)
	    for year = (cdr (assq 'year release))
	    when year
	    minimize (string-to-number year)))))

(provide 'discogs)
