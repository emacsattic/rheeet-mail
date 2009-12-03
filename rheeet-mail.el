;;; rheeet-mail.el --- rheeeeeeeeet!

;; Author: Noah Friedman <friedman@netscape.com>
;; Maintainer: friedman@splode.com
;; Created: 1999-01-06
;; Public domain

;; $Id: rheeet-mail.el,v 1.3 1999/02/24 23:15:06 friedman Exp $

;;; Commentary:

;; To use with VM: autoload this package, then do
;;   (add-hook 'vm-reply-hook 'rheeet-maybe-frob-subject-line)

;;; Code:

(require 'order-head)
(require 'fmailutils)

(defvar rheeet-addr-match-list
  '(("^president@whitehouse.gov$" . never)
    ".*")
  "Addresses in To: and Cc: headers to scan for rheeet subversion.
Entries should be of the form:

          \"regexp\"
          (\"regexp\" . always)
          (\"regexp\" . never)
          (\"regexp\" . #'(lambda (addr address-list) ...))

That is, either a simple regular expression or a cons.

If any recipient address in a reply buffer fails to match at least one of
the simple regular expressions, do not modify the subject line.

However, if any one of the \"always\" regular expressions matches, then
modify the subject line anyway regardless of other recipients.

However, if any one of the \"never\" regular expressions matches, then do
not modify the subject line even if an \"always\" regexp also matched.
That is, matching \"never\" regexps have highest precedence.

A pair which has as its cdr a lambda expression or symbol other than
`always' or `never' is called as a function with two arguments: the
address which the regexp matched, and the complete address list for the
message.  The result of this function may be t or nil or another pair with
`always' or `never' in the cdr, but it may not be another lambda
expression.

These regular expressions are always case-insensitive, regardless of the
current value of case-fold-search.")

(defvar rheeet-query-p t)

(defvar rheeet-prefix "Rheeet!  ")

;; This may be case-sensitive, depending on default case-fold-search.
(defvar rheeet-re-match-list
  '("^[Re][Ee]:[ \t]+"
    "[Rr][Hh]?[Ee]+[Tt]![ \t]+"))

(defvar rheeet-address-headers
  '("To" "Cc"))


;;;###autoload
(defun rheeet-maybe-frob-subject-line ()
  (interactive)
  (and (rheeet-match-p
        (mail-reorder-headers-rfc822-addresses
         (apply 'append (mapcar 'fmailutils-get-header-contents
                                rheeet-address-headers)))
        rheeet-addr-match-list)
       (or (not rheeet-query-p)
           (y-or-n-p "Rheeet? "))
       (rheeet-frob-subject-line)))

(defun rheeet-match-p (addresses match-list)
  (let ((case-fold-search t)
        (addrs addresses)
        (match-p t)
        this-match)
    (while addrs
      (setq this-match
            (rheeet-any-string-match match-list (car addrs) nil match-p))

      (and (consp this-match)
           (consp (cdr this-match))
           (setq this-match
                 (funcall (cdr this-match)
                          (car addrs) addresses)))

      (cond ((consp this-match)
             (cond ((eq (cdr this-match) 'never)
                    (setq match-p nil)
                    (setq addrs nil))
                   ((eq (cdr this-match) 'always)
                    (setq match-p this-match))))
            ((consp match-p))
            ((not this-match)
             (setq match-p nil)))

      (setq addrs (cdr addrs)))
    ;; normalize to boolean
    (not (not match-p))))

(defun rheeet-any-string-match (regexps string &optional pos skip-simple)
  (let ((result nil))
    (while regexps
      (cond ((and (consp skip-simple)
                  (stringp (car regexps))))
            ((string-match (if (consp (car regexps))
                               (car (car regexps))
                             (car regexps))
                           string pos)
             (setq result (car regexps))
             (setq regexps nil)))
      (setq regexps (cdr regexps)))
    result))

(defun rheeet-frob-subject-line ()
  (let ((subj (car (fmailutils-get-header-contents "Subject")))
        (pos 0))
    (cond
     (subj
      (while (rheeet-any-string-match rheeet-re-match-list subj pos)
        (setq pos (match-end 0)))
      (setq subj (concat rheeet-prefix
                         (if (zerop pos)
                             subj
                           (substring subj pos))))
      (fmailutils-put-unique-header "Subject" subj t)))))

(provide 'rheeet-mail.el)

;; rheeet-mail.el ends here
