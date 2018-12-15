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

(defun discogs-query (object identifier &optional auth)
  (let ((url-request-extra-headers
	 '(("User-Agent" . "discogs.el/1.0 +http://lars.ingebrigtsen.no/"))))
    (with-current-buffer
	(url-retrieve-synchronously
	 (format "https://api.discogs.com/%s/%s%s"
		 object identifier
		 (if auth
		     (format "&key=%s&secret=%s"
			     discogs-consumer-key
			     discogs-consumer-secret)
		   "")))
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
	  "database"
	  (format "search?type=master&artist=%s&release_title=%s" artist title)
	  t)))
    (if (plusp (length (cdr (assoc 'results results))))
	results
      ;; Rate-limit to the API limit.
      (sleep-for 1)
      (discogs-query
       "database"
       (format "search?type=release&q=%s %s" artist title)
       t))))

(defun discogs-find-year (artist title)
  (loop for release across (cdr (assq 'results (discogs-search artist title)))
	for year = (cdr (assq 'year release))
	when year
	minimize (string-to-number year)))

(defun discogs-find-tracklist (artist title)
  (let* ((search (cdr (assq 'results (discogs-search artist title))))
	 (id
	  (loop for release across search
		when (equal (cdr (assq 'type release)) "master")
		return (cdr (assq 'id release))))
	 (data (and id (discogs-query "masters" id))))
    (unless data
      (setq id
	    (loop for release across search
		  when (equal (cdr (assq 'type release)) "release")
		  return (cdr (assq 'id release))))
      (setq data (discogs-query "releases" id)))
    (debug)
    (loop for track across (cdr (assq 'tracklist data))
	  collect (cdr (assq 'title track)))))

(defun discogs-release-data (id)
  (discogs-query "masters" id))

(provide 'discogs)
