;;; stardog-sparql.el --- Interactively run Stardog SPARQL queries.

;; Copyright (C) 2023       Gregory Grubbs

;; Author: Greg Grubbs <gregory.grubbs at gmail dot com>
;; Homepage: https://github.com/gregoryg/sparql-mode
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SPARQL mode. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions to sparql-mode for use with Stardog
;;

;;; Usage:

;; Add to your Emacs config:
;; TBD

;;; Code:

(require 'dash)

(defgroup  stardog-sparql nil
  "Stardog companion for sparql-mode and ob-sparql."
  :group 'languages)

(defcustom stardog-connections-alist
  nil
  "List of stardog connections "
  :type '(alist :key-type (symbol :tag "Connection name")
                :value-type (list (string :tag "Endpoint URL")
                                  (string :tag "Default Database name")))
  :group 'stardog-sparql
  :version "0.1")

;; TODO: Switch to use either 'local (or "localhost") OR a TRAMP string path (VALUE Menu option in Customize)
(defcustom stardog-commands-host "localhost"
  "The host where the Stardog binaries can be found. A TRAMP host URL could be something like /kube:stardog-stardog-0:"
  :type 'string
  :group 'stardog-sparql)

(defcustom stardog-binaries-dir "/opt/stardog/bin"
  "The directory on `stardog-commands-host' containing the stardog and stardog-admin commands."
  :type 'string
  :group 'stardog-sparql)



(defun org-babel-sparql--replace-prefix (query-result)
  ;; (message "%s" query-result)
  (let ((dbnamespace (cdr (assq :dbnamespace org-babel-default-header-args:sparql))))
    (-tree-map (lambda (s) (if (stringp s) (s-replace-all dbnamespace s) s)) query-result)))

(advice-add #'org-babel-execute:sparql :filter-return #'org-babel-sparql--replace-prefix )
;; (advice-remove  #'org-babel-execute:sparql #'org-babel-sparql--replace-prefix)

(defun select-stardog-database (&optional endpoint)
  "Select from a list of databases on current endpoint."
  (interactive)
  (let* ((endpoint (or endpoint (cdr (assoc :server org-babel-default-header-args:sparql))))
         (databases (get-databases-on-endpoint endpoint))
         (selected-db (when databases
                        (completing-read "Database: " databases)))
         (dbnamespace (get-namespace-for-db selected-db))
         )
    (if selected-db
        (set-sparql-header-args endpoint selected-db "query" dbnamespace)
      (message "Cannot get list of databases from %s - check endpoint and .sdpass" endpoint))))

(defun get-namespace-for-db (db)
  "Get list of RDF prefixes in the namespace of the Stardog db.
The return is a list of CONS cells with the URI as car and the prefix as the cdr.
This list of CONS cells can be used directly in a post-processing function to transform full IRIs from a query result."
  (let ((endpoint (cdr (assoc :server org-babel-default-header-args:sparql)) )
         (default-directory (if (file-remote-p stardog-commands-host)
                      (expand-file-name stardog-commands-host)
                    "/tmp")) ;; assume localhost for non-tramp
)
    (with-connection-local-variables
     (cons '("http://www.w3.org/1999/02/22-rdf-syntax-ns#type" . "a")
           (mapcar #'(lambda (pair)
                       (cons (cadr pair) (car pair)))
                   (seq-partition
                    (s-split "[ \n]"
                             (s-replace-all '(("PREFIX " . "" ) ("<" . "") (">" . ""))
                                            (shell-command-to-string (concat stardog-binaries-dir "/stardog namespace export --sparql " endpoint "/" db ))) t)
                    2))))))

(defun get-databases-on-endpoint (endpoint)
  "Given a stardog endpoint, return list of databases."
  ;; It would be nice to get a machine-readable list using REST API - but the CLI makes use of =.sdpass= which is what we rely on here
  (let ((default-directory   (if (file-remote-p stardog-commands-host)
                      (expand-file-name stardog-commands-host)
                    "/tmp")))
    (with-connection-local-variables
     (split-string (s-trim
                    (s-replace  "|" ""
                                (shell-command-to-string (concat stardog-binaries-dir "/stardog-admin --server " endpoint " db list | egrep '^\\| [^ ]'|cut -d' ' -f2"))))))))

(defun select-stardog-endpoint ()
  "Set Org Babel headers for SPARQL.   table named `svar` should have these columns: connection-name, base-url, database, api-type"
  (interactive)
  (let* ((marginalia-align-offset 80)
         (completion-extra-properties '(:annotation-function annotate-sparql-selection))
         (myconnection (assoc (intern (completing-read "SPARQL Server name: " stardog-connections-alist)) stardog-connections-alist))
         (connection-name (nth 0 myconnection))
         (url (nth 1 myconnection))
         (db (nth 2 myconnection))
         (api-type "query"))
    ;; (message (format "I will surely set sparql header args to %s %s %s %s" connection-name url db api-type))
    (set-sparql-header-args url db api-type)))

(defun set-sparql-header-args (url db &optional api-endpoint dbnamespace)
  "Set local buffer header args for SPARQL."
  ;; TODO: assq-delete only the one or 2 things I want to replace
  (let* ((api-endpoint (or api-endpoint "query"))
         (dbnamespace (or dbnamespace (cdr (assq :dbnamespace org-babel-default-header-args:sparql))))
         (url (replace-regexp-in-string "//+$" "" url))
         (fullurl (concat url "/" db "/" api-endpoint  )))
    ;; (message "My lovely url is %s\n" fullurl)
    (let ((myheader
           (add-to-list 'org-babel-default-header-args:sparql
                        `(:server . url )
                        ))))
    (setq-local org-babel-default-header-args:sparql
                (->> org-babel-default-header-args:sparql
                     (assq-delete-all :server)
                     (assq-delete-all :url)
                     (assq-delete-all :dbnamespace)
                     (cons `(:dbnamespace . ,dbnamespace))
                     (cons `(:server . ,url))
                     (cons `(:url . ,fullurl))))))

(defun annotate-sparql-selection (s)
  "Provide annotations for completing-read using the data in a SPARQL Stardog server table"
  ;; (message (format "DEBUG: stardog list |%s|, table |%s}" s minibuffer-completion-table))
  (let* ((item (assoc (intern s) minibuffer-completion-table))
         (connection-name (symbol-name (nth 0 item)))
         (displayurl (s-truncate 50 (nth 1 item)))
         (db (nth 2 item))
         )
    (when item (concat
                (string-pad "" (- 20 (string-width connection-name)))
                displayurl
                (string-pad "" (- 55 (string-width displayurl)))
                db))))
(provide 'stardog-sparql)
