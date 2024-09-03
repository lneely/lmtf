;; sample.el provides a small number of simple test examples that
;; demonstrate lmtf functionality. 


;; discover and run ert test
(ert-deftest test-addition ()
  "Test simple addition."
  (should (= (+ 2 3) 5))
  (should (= (+ -1 1) 0))
  (should (= (+ 100 200) 300)))

;; discover and run custom test
(defun test-string-length ()
  "Test string length function."
  (unless (= (length "hello") 5)
    (error "Expected length of 'hello' to be 5"))
  (unless (= (length "") 0)
    (error "Expected length of empty string to be 0"))
  (unless (= (length "a b c") 5)
    (error "Expected length of 'a b c' to be 5")))

;; discover and run custom test with ert assertions
(defun test-string-length-ert ()
  "Test string length function with ert assertions."
  (should (= (length "hello") 5))
  (should (= (length "") 0))
  (should (= (length "a b c") 5)))


;; discover and run a failing test
(ert-deftest test-failing-comparison ()
  "This test is designed to fail."
  (should (string= "apple" "orange"))
  (should (> 5 10)))

;; exclude a test from auto run.
(lmtf-exclude 'test-failing-comparison)
