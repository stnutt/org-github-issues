;;; org-github-issues.el --- Manage GitHub issues in Org-mode  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017  Stephen Nutt
;;
;; Author: Stephen Nutt <stnutt@gmail.com>
;; URL: https://github.com/stnutt/org-github-issues
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: outlines
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package provides the command org-github-issues-push for
;; creating or updating a GitHub issue from a heading in an Org-mode
;; buffer.  A GitHub personal access token with public_repo access is
;; required and be should be configured as org-github-issues-token
;; (visit the GitHub account settings page to create one).
;;
;; An example issue in Org-mode:
;;
;; * org-github-issues
;;   :PROPERTIES:
;;   :CATEGORY: org-github-issues
;;   :END:
;; ** issues [0/1]
;; *** TODO Omit state notes from body :enhancement:
;;     If notes or timestamps were added for todo state changes, these
;;     will be included in the GitHub issue body.  Omit these from body
;;     before pushing the issue to GitHub.  This may involve some smart
;;     parsing of the heading and exporting the subsequent string using
;;     =org-export-string-as=.
;;
;; When org-github-issues-push is called with the point on the leaf
;; heading, it will create a new issue in the authenticated user's
;; org-github-issues repository with a title, body, and enhancement
;; label.  The repository is determined from the CATEGORY property
;; which is inherited from the root heading.  The repository owner is
;; assumed to be the authenticated user, but can be explicitly
;; specified by setting the CATEGORY to <owner>/<repo>.  The overall
;; structure of the Org-mode buffer is irrelevant, as long as the
;; heading at the point represents a single issue with no sub-headings
;; and has either an inherited or explicit CATEGORY property
;; indicating the repository.  The issue will be unassigned unless
;; org-github-issues-assign-states is t or includes the current todo
;; state of heading (if it has one), in which case it will be assigned
;; to the authenticated user.  After the issue is created on GitHub,
;; the heading will become a link to the issue on GitHub, and the
;; CUSTOM_ID property will be set to identify the issue so subsequent
;; calls to org-github-issues-push will update the issue.  You can
;; close the issue by setting the todo state to a done state and
;; running org-github-issues-push.  The body is exported to Markdown
;; or GitHub-flavored markdown if the ox-gfm package is installed.
;;
;; Roadmap
;; -------
;; * Omit state notes
;; * Unit tests
;; * Set milestone
;; * Customizable owner/repo property
;; * Customizable id property
;; * Priority -> label
;; * Assign to other users
;; * Support mentions and references
;; * Pull issue updates and new issues from GitHub
;; * Read and add comments?
;; * Handle attachments?
;; * Integrate with GitHub Projects API?
;; * Support other bug trackers?
;;
;;; Code:

(require 'json)
(require 'seq)
(require 'org)
(unless (require 'ox-gfm nil t)
  (require 'ox-md))
(require 'url)

(defvar org-github-issues--user nil
  "The authenticated GitHub user.")

(defgroup org-github-issues nil
  "Manage GitHub issues in Org-mode"
  :tag "Org GitHub Issues"
  :group 'outlines)

(defcustom org-github-issues-token nil
  "GitHub API authentication token."
  :group 'org-github-issues
  :type '(choice (string :tag "Literal token")
                 (function :tag "Function that returns token")))

(defcustom org-github-issues-linkify-headings t
  "Convert the heading to a link after pushing an issue."
  :group 'org-github-issues
  :type 'boolean)

(defcustom org-github-issues-assign-states nil
  "List of todo states for which to perform issue assignment.
When nil, never assign.  When t, always assign.  The issue is
assigned to the authenticated user."
  :group 'org-github-issues
  :type '(choice
          (const :tag "Never assign" nil)
          (const :tag "Always assign" t)
          (repeat :tag "Todo states" (string :tag "State"))))

(defun org-github-issues--send-request (method path &optional params)
  "Send a HTTP request to the GitHub API.

METHOD is the HTTP request method to use.  PATH is the API path
following the domain name.  PARAMS is an optional alist
representing JSON to include with the request."
  (cl-letf (((symbol-function 'message) #'ignore)
            (url-request-extra-headers `(("User-Agent" . "stnutt/org-github-issues")
                                         ("Accept" . "application/vnd.github.v3+json")
                                         ("Authorization" . ,(concat "token " org-github-issues-token))))
            (url-request-method method)
            (url (concat "https://api.github.com/" path))
            (json-object-type 'alist)
            (json-array-type 'vector)
            (url-request-data (when params (json-encode params))))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (let ((status (progn
                      (goto-char (point-min))
                      (string-to-number (cadr (split-string (buffer-substring (point) (point-at-eol)))))))
            (response (progn
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (forward-line)
                        (json-read-from-string (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8)))))
        (if (>= status 400)
            (error "GitHub API responded with %s: %s" status (assoc-default 'message response))
          response)))))

(defun org-github-issues--get-user ()
  "Get the authenticated user from GitHub."
  (or org-github-issues--user
      (setq org-github-issues--user (assoc-default 'login (org-github-issues--send-request "GET" "user")))))

(defun org-github-issues--create-issue (issue)
  "Create an issue on GitHub with data from ISSUE."
  (org-github-issues--send-request
   "POST"
   (format "repos/%s/%s/issues" (assoc-default 'owner issue) (assoc-default 'repo issue))
   (seq-filter (lambda (elt) (member (car elt) '(title body milestone labels assignees))) issue)))

(defun org-github-issues--update-issue (issue)
  "Update an issue on GitHub with data from ISSUE."
  (org-github-issues--send-request
   "PATCH"
   (format "repos/%s/%s/issues/%s" (assoc-default 'owner issue) (assoc-default 'repo issue) (assoc-default 'number issue))
   (seq-filter (lambda (elt) (member (car elt) '(title body state milestone labels assignees))) issue)))

(defun org-github-issues--parse-heading ()
  "Parse the current heading to derive GitHub issue data."
  (unless (org-at-heading-p)
    (user-error "Point is not on an Org-mode heading"))
  (let* ((category (split-string (org-get-category) "/"))
         (owner (when (> (length category) 1) (car category)))
         (repo (if owner (cadr category) (car category)))
         (id (org-entry-get (point) "CUSTOM_ID"))
         (number (when id (cadr (split-string id "#"))))
         (title (org-link-display-format (org-no-properties (org-get-heading t t))))
         (state (if (org-entry-is-done-p) "closed" "open"))
         (assign (or (eq org-github-issues-assign-states t) (member (org-get-todo-state) org-github-issues-assign-states)))
         (body (org-export-as (if (featurep 'ox-gfm) 'gfm 'md) t t t))
         (labels (vconcat (org-get-tags))))
    `((owner . ,owner)
      (repo . ,repo)
      (number . ,number)
      (title . ,title)
      (state . ,state)
      (body . ,body)
      (labels . ,labels)
      (assign . ,assign))))

(defun org-github-issues--update-heading (issue)
  "Modify the current heading to relate it to the GitHub issue ISSUE."
  (let* ((owner (assoc-default 'owner issue))
         (repo (assoc-default 'repo issue))
         (number (assoc-default 'number issue))
         (id (format "%s/%s#%s" owner repo number))
         (heading (org-no-properties (org-get-heading t t)))
         (link (when org-github-issues-linkify-headings
                 (org-make-link-string (format "https://github.com/%s/%s/issues/%s" owner repo number) (assoc-default 'title issue)))))
    (unless (equal (org-entry-get (point) "CUSTOM_ID") id)
      (org-set-property "CUSTOM_ID" id))
    (unless (or (not link) (eq heading link))
      (save-excursion
        (org-back-to-heading)
        (search-forward heading)
        (replace-match link t t)))))

;;;###autoload
(defun org-github-issues-push ()
  "Create or update a GitHub issue based on the current heading."
  (interactive)
  (let ((issue (org-github-issues--parse-heading)))
    (unless (assoc-default 'owner issue)
      (setcdr (assoc 'owner issue) (org-github-issues--get-user)))
    (when (assoc-default 'assign issue)
      (push `(assignees . (,(org-github-issues--get-user))) issue))
    (if (assoc-default 'number issue)
        (org-github-issues--update-issue issue)
      (setcdr (assoc 'number issue) (assoc-default 'number (org-github-issues--create-issue issue)))
      (org-github-issues--update-heading issue))))

(provide 'org-github-issues)
;;; org-github-issues.el ends here
