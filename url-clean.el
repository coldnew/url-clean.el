;;; url-clean.el --- Clean tracking parameters from URLs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yen-Chin, Lee <coldnew.tw@gmail.com>

;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; Keywords: url, tracking, clean, privacy
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; url-clean provides utilities to remove tracking parameters from URLs.
;;
;; Supported tracking parameters:
;; - Google Analytics: utm_source, utm_medium, utm_campaign, utm_term, utm_content
;; - Facebook: fbclid
;; - Google Ads: gclid, gclsrc, dclid, msclkid
;; - Mailchimp: mc_eid, mc_cid
;; - HubSpot: _hsenc, _hsmi, hsCtaTracking
;; - Yandex: yclid
;; - Others: ref, ref_, ref_src, ref_url, _openstat, wickedid, ttclid, irclickid
;;
;; Usage:
;;   (require 'url-clean)
;;   (url-clean "https://example.com?utm_source=telegram") ; => "https://example.com"

;;; Code:

(require 'url-parse)
(require 'url-util)

(defvar url-clean-parameters
  '("utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content"
    "fbclid" "gclid" "gclsrc" "dclid" "msclkid"
    "ref" "ref_" "ref_src" "ref_url"
    "mc_eid" "mc_cid"
    "_hsenc" "_hsmi" "hsCtaTracking"
    "yclid" "_openstat"
    "wickedid" "ttclid" "irclickid")
  "URL parameters to remove for tracking cleanup.")

(defun url-clean (url)
  "Remove tracking parameters from URL.
Returns cleaned URL string, or original URL string if URL is invalid."
  (let ((parsed (url-generic-parse-url url)))
    (if (and parsed (aref parsed 1) ; scheme (index 1)
             (not (string-empty-p url)))
        (let* ((full-filename (aref parsed 6))
               (parts (if (string-match "\\?" full-filename)
                          (split-string full-filename "\\?")
                        (list full-filename)))
               (filename (car parts))
               (query-string (cadr parts))
               (query-alist (when query-string
                              (url-parse-query-string query-string)))
               (filtered-query (delq nil
                                     (mapcar (lambda (pair)
                                               (let ((param (car pair)))
                                                 (unless (or (member param url-clean-parameters)
                                                             (cl-some (lambda (p)
                                                                        (string-prefix-p p param))
                                                                      url-clean-parameters))
                                                   pair)))
                                             query-alist)))
               (new-query-string (when filtered-query
                                   (url-build-query-string filtered-query))))
          (setf (aref parsed 6) (if new-query-string
                                    (concat filename "?" new-query-string)
                                  filename))
          (url-recreate-url parsed))
      url)))

(provide 'url-clean)
;;; url-clean.el ends here
