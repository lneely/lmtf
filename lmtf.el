(require 'ert)

(defvar lmtf-test-directory "tests"
  "Directory containing tests, relative to the project root.")

(defvar lmtf-test-file-map (make-hash-table :test 'equal)
  "Hash table to store the mapping between test names and their source files.")

(defvar lmtf-test-project-indicators
  '(".projectile" ".git" ".svn" ".hg" ".dir-locals.el")
  "List of files or directories that indicate the root of an Elisp project.")

(defun lmtf-find-project-root ()
  "Find the root directory of the current Elisp project."
  (let ((root (seq-some (lambda (indicator)
                          (locate-dominating-file default-directory indicator))
                        lmtf-test-project-indicators)))
    (if root
        (expand-file-name root)
      (error "Unable to find project root"))))

(defun lmtf-show-results ()
  "Show the test results in a pop-up buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Project Test Results*")))
    (with-current-buffer buffer
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") 'quit-window)
        (use-local-map map)))
    (pop-to-buffer buffer '((display-buffer-in-side-window)
                            (side . bottom)
                            (window-height . 0.3)))))

(defun lmtf-run-single ()
  "Run a single test selected from the list of discovered tests."
  (interactive)
  (lmtf-setup)
  (let* ((all-tests (lmtf-discover-all-tests))
         (test-list (lmtf-flatten-test-list all-tests))
         (selected-test (completing-read "Select test to run: " test-list nil t)))
    (if selected-test
        (lmtf-execute-tests (list selected-test))
      (message "No test selected."))))

(defun lmtf-run-multiple ()
  "Run multiple tests selected from the list of discovered tests."
  (interactive)
  (lmtf-setup)
  (let* ((all-tests (lmtf-discover-all-tests))
         (test-list (lmtf-flatten-test-list all-tests))
         (selected-tests (completing-read-multiple "Select tests to run: " test-list nil t)))
    (if selected-tests
        (lmtf-execute-tests selected-tests)
      (message "No tests selected."))))

(defun lmtf-discover-tests (file)
  "Discover all test functions and ERT tests defined in FILE, ignoring commented lines."
  (let ((test-functions '())
        (temp-buffer (generate-new-buffer " *temp*")))
    (unwind-protect
        (progn
          (with-current-buffer temp-buffer
            (insert-file-contents file nil nil nil t)
            (emacs-lisp-mode)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (when (and (not (string-match-p "^\\s-*;" line-content))
                           (string-match-p "(defun\\|ert-deftest" line-content))
                  (cond
                   ((string-match "(defun\\s-+\\(test-\\S-+\\)" line-content)
                    (let ((func-name (match-string 1 line-content)))
                      (push (cons func-name nil) test-functions)
                      (puthash func-name (file-name-nondirectory file) lmtf-test-file-map)))
                   ((string-match "(ert-deftest\\s-+\\(\\S-+\\)" line-content)
                    (let ((func-name (match-string 1 line-content)))
                      (push (cons func-name nil) test-functions)
                      (puthash func-name (file-name-nondirectory file) lmtf-test-file-map))))))
              (forward-line 1))))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))
    (nreverse test-functions)))

(defun lmtf-run-tests (file)
  "Run all tests in FILE, ignoring commented tests."
  (let ((tests (lmtf-discover-tests file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (test tests)
        (let ((test-name (car test)))
          (goto-char (point-min))
          (when (re-search-forward (format "^\\([^;]\\|\\\";\\)*\\((defun\\|ert-deftest)\\s-+%s\\>" test-name) nil t)
            (let ((start (match-beginning 0)))
              (goto-char start)
              (forward-sexp)
              (eval-region start (point))))
          (lmtf-log (format "Running test: %s" test-name))
          (lmtf-execute-single-test test-name))))))

(defun lmtf-run-all-tests (&optional arg)
  "Run all discovered tests. With prefix ARG, run a single test."
  (interactive "P")
  (lmtf-setup)
  (if arg
      (call-interactively 'lmtf-run-single)
    (let* ((all-tests (lmtf-discover-all-tests))
           (test-names (mapcar #'car (apply #'append (mapcar #'cdr all-tests)))))
      (lmtf-execute-tests test-names))))

(defun lmtf-find-test-files ()
  "Find all Emacs Lisp test files in the test directory."
  (let* ((test-dir (lmtf-get-test-directory))
         (files (directory-files-recursively test-dir "\\.el$")))
    files))

(defun lmtf-discover-all-tests ()
  "Discover all tests in the project."
  (let* ((files (lmtf-find-test-files))
         (all-tests (mapcar (lambda (file)
                              (cons file (lmtf-discover-tests file)))
                            files)))
    all-tests))

(defun lmtf-log (message &optional test-name)
  "Log MESSAGE to the project test results buffer.
If TEST-NAME is provided, include the associated filename in the log."
  (let ((buffer (get-buffer-create "*Project Test Results*"))
        (filename (when test-name (gethash test-name lmtf-test-file-map))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "%s%s\n"
                      (if filename (format "[%s] " filename) "")
                      message))
      (goto-char (point-max)))))

(defun lmtf-get-test-directory ()
  "Get the absolute path of the test directory."
  (expand-file-name lmtf-test-directory (lmtf-find-project-root)))

(defun lmtf-setup ()
  "Set up the environment for running tests."
  (let ((test-dir (lmtf-get-test-directory)))
    (add-to-list 'load-path test-dir)))

(defun lmtf-flatten-test-list (all-tests)
  "Flatten the nested list of ALL-TESTS into a single list of test names."
  (cl-loop for file-tests in all-tests
           append (cl-loop for test in (cdr file-tests)
                           collect (car test))))

(defun lmtf-ert-test-p (test-name)
  "Check if TEST-NAME refers to an ERT test."
  (ert-test-boundp (intern-soft test-name)))

(defun lmtf-execute-single-test (test-name)
  "Execute a single test function with TEST-NAME. Return 'pass if the test passed, 'fail if it failed."
  (let ((test-symbol (intern-soft test-name)))
    (cond
     ((ert-test-boundp test-symbol)
      (condition-case err
          (let* ((test (ert-get-test test-symbol))
                 (result (ert-run-test test)))
            (if (ert-test-passed-p result)
                'pass
              (lmtf-log (format "Error in test %s: %s" test-name (ert-test-result-expected-value result)) test-name)
              'fail))
        (error
         (lmtf-log (format "Error in test %s: %s" test-name err) test-name)
         'fail)))
     
     ((fboundp test-symbol)
      (condition-case err
          (progn
            (funcall test-symbol)
            'pass)
        (error
         (lmtf-log (format "Error in test %s: %s" test-name err) test-name)
         'fail)))
     
     (t
      (lmtf-log (format "Test %s not found" test-name) test-name)
      'fail))))

(defun lmtf-execute-tests (test-names)
  "Execute tests with TEST-NAMES (a list of test name strings)."
  (if test-names
      (let ((all-passed t))
        (dolist (file (lmtf-find-test-files))
          (load-file file))
        
        (catch 'test-failed
          (dolist (test-name test-names)
            (let ((result (lmtf-execute-single-test test-name)))
              (cond
               ((eq result 'pass)
                (lmtf-log (format "%s: PASS" test-name) test-name))
               ((eq result 'fail)
                (lmtf-log (format "%s: FAIL" test-name) test-name)
                (setq all-passed nil)
                (throw 'test-failed nil))))))
        
        (if all-passed
            (lmtf-log "All tests passed.")
          (lmtf-log "Tests failed. Stopping execution.")))
    (lmtf-log "No tests found to run.")))

(defun lmtf-exclude (&rest test-functions)
  "Exclude one or more test functions from running.
TEST-FUNCTIONS should be a list of function names (symbols or strings)."
  (dolist (func test-functions)
    (let ((func-symbol (if (symbolp func) func (intern func))))
      (put func-symbol 'excluded t)
      (message "Excluded test: %s" func-symbol))))

(provide 'lmtf)
