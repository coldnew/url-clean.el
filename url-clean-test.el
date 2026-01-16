;;; url-clean-test.el --- Tests for url-clean  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yen-Chin, Lee <coldnew.tw@gmail.com>

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

;; Unit tests for url-clean.el using ERT.

;;; Code:

(require 'url-clean)
(require 'ert)

;;; Google Analytics parameters

(ert-deftest url-clean-test/utm-source ()
  "Test removal of utm_source parameter."
  (should (string= (url-clean "https://example.com?utm_source=google")
                   "https://example.com")))

(ert-deftest url-clean-test/utm-medium ()
  "Test removal of utm_medium parameter."
  (should (string= (url-clean "https://example.com?utm_medium=cpc")
                   "https://example.com")))

(ert-deftest url-clean-test/utm-campaign ()
  "Test removal of utm_campaign parameter."
  (should (string= (url-clean "https://example.com?utm_campaign=summer_sale")
                   "https://example.com")))

(ert-deftest url-clean-test/utm-term ()
  "Test removal of utm_term parameter."
  (should (string= (url-clean "https://example.com?utm_term=keyword")
                   "https://example.com")))

(ert-deftest url-clean-test/utm-content ()
  "Test removal of utm_content parameter."
  (should (string= (url-clean "https://example.com?utm_content=banner")
                   "https://example.com")))

(ert-deftest url-clean-test/all-utm-params ()
  "Test removal of all utm parameters at once."
  (should (string= (url-clean "https://example.com?utm_source=google&utm_medium=cpc&utm_campaign=test&utm_term=term&utm_content=content")
                   "https://example.com")))

;;; Facebook parameter

(ert-deftest url-clean-test/fbclid ()
  "Test removal of fbclid parameter."
  (should (string= (url-clean "https://example.com?fbclid=IwAR0test")
                   "https://example.com")))

;;; Google Ads parameters

(ert-deftest url-clean-test/gclid ()
  "Test removal of gclid parameter."
  (should (string= (url-clean "https://example.com?gclid=TEST123")
                   "https://example.com")))

(ert-deftest url-clean-test/gclsrc ()
  "Test removal of gclsrc parameter."
  (should (string= (url-clean "https://example.com?gclsrc=dsadsad")
                   "https://example.com")))

(ert-deftest url-clean-test/dclid ()
  "Test removal of dclid parameter."
  (should (string= (url-clean "https://example.com?dclid=TEST456")
                   "https://example.com")))

(ert-deftest url-clean-test/msclkid ()
  "Test removal of msclkid parameter."
  (should (string= (url-clean "https://example.com?msclkid=TEST789")
                   "https://example.com")))

;;; Mailchimp parameters

(ert-deftest url-clean-test/mc-eid ()
  "Test removal of mc_eid parameter."
  (should (string= (url-clean "https://example.com?mc_eid=abc123")
                   "https://example.com")))

(ert-deftest url-clean-test/mc-cid ()
  "Test removal of mc_cid parameter."
  (should (string= (url-clean "https://example.com?mc_cid=campaign123")
                   "https://example.com")))

;;; HubSpot parameters

(ert-deftest url-clean-test/_hsenc ()
  "Test removal of _hsenc parameter."
  (should (string= (url-clean "https://example.com?_hsenc=enc123")
                   "https://example.com")))

(ert-deftest url-clean-test/_hsmi ()
  "Test removal of _hsmi parameter."
  (should (string= (url-clean "https://example.com?_hsmi=msg123")
                   "https://example.com")))

(ert-deftest url-clean-test/hsCtaTracking ()
  "Test removal of hsCtaTracking parameter."
  (should (string= (url-clean "https://example.com?hsCtaTracking=cta123")
                   "https://example.com")))

;;; Yandex parameter

(ert-deftest url-clean-test/yclid ()
  "Test removal of yclid parameter."
  (should (string= (url-clean "https://example.com?yclid=yclid123")
                   "https://example.com")))

;;; Other tracking parameters

(ert-deftest url-clean-test/_openstat ()
  "Test removal of _openstat parameter."
  (should (string= (url-clean "https://example.com?_openstat=stat123")
                   "https://example.com")))

(ert-deftest url-clean-test/wickedid ()
  "Test removal of wickedid parameter."
  (should (string= (url-clean "https://example.com?wickedid=wicked123")
                   "https://example.com")))

(ert-deftest url-clean-test/ttclid ()
  "Test removal of ttclid parameter."
  (should (string= (url-clean "https://example.com?ttclid=ttclid123")
                   "https://example.com")))

(ert-deftest url-clean-test/irclickid ()
  "Test removal of irclickid parameter."
  (should (string= (url-clean "https://example.com?irclickid=click123")
                   "https://example.com")))

;;; Ref parameters

(ert-deftest url-clean-test/ref ()
  "Test removal of ref parameter."
  (should (string= (url-clean "https://example.com?ref=source")
                   "https://example.com")))

(ert-deftest url-clean-test/ref_ ()
  "Test removal of ref_ parameter (matches ref_source, etc.)."
  (should (string= (url-clean "https://example.com?ref_source=twitter")
                   "https://example.com")))

(ert-deftest url-clean-test/ref-src ()
  "Test removal of ref_src parameter."
  (should (string= (url-clean "https://example.com?ref_src=newsletter")
                   "https://example.com")))

(ert-deftest url-clean-test/ref-url ()
  "Test removal of ref_url parameter."
  (should (string= (url-clean "https://example.com?ref_url=https://google.com")
                   "https://example.com")))

;;; Mixed parameters

(ert-deftest url-clean-test/mixed-tracking-params ()
  "Test removal of multiple tracking parameters."
  (should (string= (url-clean "https://example.com?utm_source=google&fbclid=fb123&gclid=gclid123&id=keep")
                   "https://example.com?id=keep")))

(ert-deftest url-clean-test/multiple-keep-params ()
  "Test keeping non-tracking parameters while removing tracking ones."
  (let* ((result (url-clean "https://example.com?utm_source=test&page=5&sort=date&limit=10"))
         (params (when result
                   (url-parse-query-string
                    (cadr (split-string result "\\?"))))))
    (should (and result
                 (assoc "page" params)
                 (assoc "sort" params)
                 (assoc "limit" params)
                 (= (length params) 3)))))

;;; Edge cases

(ert-deftest url-clean-test/no-tracking-params ()
  "Test URL with no tracking parameters is unchanged."
  (should (string= (url-clean "https://example.com?page=1")
                   "https://example.com?page=1")))

(ert-deftest url-clean-test/no-query-string ()
  "Test URL with no query string is unchanged."
  (should (string= (url-clean "https://example.com")
                   "https://example.com")))

(ert-deftest url-clean-test/url-with-fragment ()
  "Test URL with fragment is handled correctly."
  (should (string= (url-clean "https://example.com?utm_source=test#section")
                   "https://example.com#section")))

(ert-deftest url-clean-test/invalid-url ()
  "Test invalid URL returns nil."
  (should (null (url-clean "not-a-valid-url"))))

(ert-deftest url-clean-test/empty-string ()
  "Test empty string returns nil."
  (should (null (url-clean ""))))

(ert-deftest url-clean-test/preserves-protocol ()
  "Test that the protocol is preserved."
  (should (string= (url-clean "http://example.com?utm_source=test")
                   "http://example.com")))

(ert-deftest url-clean-test/https-url ()
  "Test HTTPS URLs are handled correctly."
  (should (string= (url-clean "https://example.com/page?utm_source=test")
                   "https://example.com/page")))

(ert-deftest url-clean-test/subdomain ()
  "Test URLs with subdomains are handled correctly."
  (should (string= (url-clean "https://sub.example.com/page?utm_source=test")
                   "https://sub.example.com/page")))

(ert-deftest url-clean-test/path-with-special-chars ()
  "Test URLs with special characters in path."
  (should (string= (url-clean "https://example.com/path/to/page?utm_source=test")
                   "https://example.com/path/to/page")))

(ert-deftest url-clean-test/case-sensitive-param-names ()
  "Test that parameter matching is case-sensitive."
  (should (string= (url-clean "https://example.com?UTM_SOURCE=test")
                   "https://example.com?UTM_SOURCE=test")))

;;; url-clean-test.el ends here
