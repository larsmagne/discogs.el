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

(defun discogs-query (object identifier)
  (let ((url-request-extra-headers
	 '(("User-Agent" . "discogs.el/1.0 +http://lars.ingebrigtsen.no/"))))
    (with-current-buffer
	(url-retrieve-synchronously
	 (format "http://api.discogs.com/%s/%s"
		 object identifier))
      (goto-char (point-min))
      (prog1
	  (when (re-search-forward "\r?\n\r?\n" nil t)
	    (json-read))
	(kill-buffer (current-buffer))))))

(defun discogs-search (artist title)
  (discogs-query
   "database" (format "search?type=master&artist=%s&release_title=%s"
		      artist title)))

(defun discogs-find-year (artist title)
  (let ((data (discogs-search artist title)))
    (when (eq (caar data) 'results)
      (loop for release across (cdar data)
	    for year = (cdr (assq 'year release))
	    when year
	    minimize (string-to-number year)))))

(provide 'discogs)
