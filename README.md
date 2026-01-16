<a href="http://github.com/coldnew/url-clean.el"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
# url-clean

An emacs-lisp library that provide function to removes annoying tracking parameters from URLs.

[![CI](https://github.com/coldnew/url-clean.el/actions/workflows/main.yml/badge.svg)](https://github.com/coldnew/url-clean.el/actions/workflows/main.yml)
[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

## Installation

1. Download the latest `url-clean.el` file from the repository
2. Place it in your Emacs load path (e.g., `~/.emacs.d/user-lisp/`)
3. Add the following to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/user-lisp/")
(require 'url-clean)
```

## Usage

The main function `url-clean` takes a URL string and returns a cleaned URL with tracking parameters removed:

```elisp
(require 'url-clean)

;; Remove Google Analytics tracking
(url-clean "https://example.com?utm_source=google&utm_medium=cpc")
;; => "https://example.com"

```

The function returns `nil` for invalid URLs:

```elisp
(url-clean "not-a-valid-url")
;; => nil

(url-clean "")
;; => nil
```

## Customization

You can customize the list of tracking parameters to remove by modifying `url-clean-parameters`:

```elisp
;; Add custom tracking parameters
(add-to-list 'url-clean-parameters "custom_tracker")

;; Remove a parameter from the list
(setq url-clean-parameters (delete "fbclid" url-clean-parameters))
```
