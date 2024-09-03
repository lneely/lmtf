# LispyMcTestFace (lmtf)

LispyMcTestFace (lmtf) is a lightweight, multipurpose testing
framework for Emacs Lisp projects. It provides a simple yet powerful
way to discover, organize, and run tests in your Elisp codebase.

## Motivation

lmtf was born from a dissatisfaction with the state of testing for
elisp. There seemed to be no solution that gave me the ability to run
both simple unit tests and complex integration tests quickly and
effortlessly. This made testing elisp code a cumbersome, manual
process. Adding to this frustration, I had to completely restart emacs
if I renamed or deleted a test with `ert`.

`lmtf` seeks to solve these problems through:

- Automatic, project-aware test discovery which does not require
  restarting emacs as the tests change. 
- Support for both `ert` unit tests and custom tests
- Ability to run all tests or a single test using an interactive user
  interface
- Simple and informative logging
- Ability to exclude more complex or less frequently needed tests from
  "run all tests" for better performance. These tests can still be
  invoked manually by running a single test.

## Installation

### Using use-package with straight.el

Add the following to your Emacs configuration:

```elisp
(use-package lmtf
  :straight (lmtf :type git :host github :repo "lneely/lmtf")
     :bind (:map emacs-lisp-mode-map
            ("C-c t !" . lmtf-run-all-tests)
            ("C-c t r" . lmtf-show-results))
```

### Manual Installation

Replace `$EMACSDIR` with the location of your emacs configuration,
e.g., `~/.config/emacs` or `~/.emacs.d`. 

1. Clone the lmtf repository:
   ```
   git clone https://github.com/lneely/lmtf.git $EMACSDIR/lmtf
   ```

2. Add the following to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "~/$EMACSDIR/lmtf")
   (require 'lmtf)
   
   ;; recommended key bindings
   (define-key emacs-lisp-mode-map (kbd "C-c t !") #'lmtf-run-all-tests)
   (define-key emacs-lisp-mode-map (kbd "C-c t r") #'lmtf-show-results)
   ```

### Key Bindings

`lmtf` does not provide any default key bindings. Here are some
recommendations for a vanilla emacs configuration (i.e., no doom or
spacemacs):

```elisp
(define-key emacs-lisp-mode-map (kbd "C-c t !") #'lmtf-run-all-tests)
(define-key emacs-lisp-mode-map (kbd "C-c t r") #'lmtf-show-results)
```

Note that you can use the universal prefix `C-u` on `C-c t !` to run a
single test instead of all tests.

## Usage

### Writing and Organizing Tests

lmtf discovers tests in the directory specified by
`lmtf-test-directory`, relative to the project root. This is "tests"
by default, and may be customized. For example, if you want your tests
to reside in `src/tests`, simply use:

```elisp
(setq lmtf-test-directory "src/tests")
```

Test files can be named whatever you want. All test functions must
follow the convention "test-" (e.g., `test-feature-a`) to be
discovered by lmtf.

### Custom Test Functions

`lmtf` supports custom test functions, and custom functions may use
`ert` assertions or an error condition to indicate pass/fail. A custom
test with `ert` assertions looks exactly like an `ert` test, but uses 
`defun` instead of the `ert-deftest` macro.

```elisp
(defun test-addition ()
  (should (= (+ 2 2) 4))
  (should-not (= (+ 2 2) 5)))
```

A custom test without `ert` assertions simply uses an error condition
to fail. If the error state is not entered, then the test passes.

```elisp
(defun test-string-length ()
  (let ((result (length "hello")))
    (if (= result 5)
        (message "Test passed: string length is correct")
      (error "Test failed: expected length 5, got %s" result))))
```

### ERT Tests

`lmtf` supports running `ert` tests. A benefit of this feature is that
existing `ert` tests can be run by `lmtf` without modification, so
long as they are named according to the convention and they reside in
the `lmtf-test-directory`.

```elisp
(require 'ert)

(ert-deftest test-multiplication ()
  (should (= (* 3 3) 9))
  (should-error (/ 1 0)))
```

### Running Tests

- To run all tests: `M-x lmtf-run-all-tests`
- To run a single test: `M-x lmtf-run-single`

### Excluding Tests

Sometimes it is desirable to exclude certain tests from the automated test 
run. To do this:

```elisp
(lmtf-exclude 'test-to-exclude-1
              'test-to-exclude-2) ;; and so on...
```

## License

LispyMcTestFace (lmtf) - A lightweight, multipurpose testing framework
for Emacs Lisp Copyright (C) 2024 Levi Neely

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see https://www.gnu.org/licenses/.

