;;; discogs.el --- discogs interface
;; Copyright (C) 2013 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; discogs.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; To use this, you need an account at discogs.com, and you then have
;; to register an application here:

;; https://www.discogs.com/settings/developers

;; You'll then get a consumer key and secret pair, and you set the
;; variables below to those two values.  You should then be able to
;; say

;; (discogs-find-tracklist "xiu xiu" "knife play")
;; => ("Don Diasco" "I Broke Up (SJ)" "Luber" "Hives Hives" "Dr. Troll" "Over Over" "Anne Dong" "Suha" "Poe Poe" "Homonculus" "Tonite And Today (What Chu' Talkin' About)")

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
    (discogs-collect-tracklist data)))

(defun discogs-collect-tracklist (data)
  (let ((tracks
	 (loop for track across (cdr (assq 'tracklist data))
	       collect (cons
			(mapconcat
			 'identity
			 (loop for artist across (cdr (assq 'artists track))
			       collect (discogs-clean-artist-name
					(cdr (assq 'name artist))))
			 ", ")
			(cdr (assq 'title track))))))
    (if (= (length (delete-duplicates
		    (mapcar #'car tracks)
		    :test #'equal))
	   1)
	;; If there's just one band, then return only track names.
	(mapcar #'cdr tracks)
      ;; If there's more, return Band / Track.
      (loop for elem in tracks
	    collect (format "%s / %s" (car elem) (cdr elem))))))

(defun discogs-clean-artist-name (artist)
  (replace-regexp-in-string " +([0-9]+)$" "" artist))

(defun discogs-release-data (id)
  (discogs-query "masters" id))

(defun discogs-find-release-tracklist (artist title)
  (setq artist (discogs-clean artist)
	title (discogs-clean title))
  (let ((releases
	 (loop for release across
	       (cdr (assq 'results
			  (discogs-query
			   "database"
			   (format "search?type=release&q=%s %s" artist title)
			   t)))
	       when (equal (cdr (assq 'type release)) "release")
	       collect release)))
    (if (= (length releases) 1)
	(discogs-collect-tracklist (car releases))
      (let* ((entries
	      (loop for release in releases
		    collect (cons
			     (format "%s %s %s (%s)"
				     (cdr (assq 'title release))
				     (cdr (assq 'format release))
				     (cdr (assq 'year release))
				     (cdr (assq 'id release)))
			     (cdr (assq 'id release)))))
	     (choice
	      (completing-read "Choose release: " (mapcar #'car entries))))
	(if (not choice)
	    nil
	  (discogs-collect-tracklist
	   (setq data (discogs-query "releases"
				     (cdr (assoc choice entries))))))))))

(provide 'discogs)
