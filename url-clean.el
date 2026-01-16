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
Returns cleaned URL string, or nil if URL is invalid."
  (let ((parsed (url-generic-parse-url url)))
    (when parsed
      (let* ((query (url-query parsed))
             (new-query (delq nil
                              (mapcar (lambda (pair)
                                        (unless (member (car pair) url-clean-parameters)
                                          pair))
                                      query))))
        (setf (url-query parsed) new-query)
        (url-recreate-url parsed)))))

(provide 'url-clean)
;;; url-clean.el ends here
