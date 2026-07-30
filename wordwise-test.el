;;; wordwise-test.el --- Tests for wordwise  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Palak Mathur

;;; Commentary:

;; Run with: make test
;; (emacs --batch -Q -L . -l wordwise-test.el -f ert-run-tests-batch-and-exit)

;;; Code:

(require 'ert)
(require 'wordwise)

(defun wordwise-test--hint-lines ()
  "Return the overlays carrying a phantom hint line in the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'before-string))
              (overlays-in (point-min) (point-max))))

(ert-deftest wordwise-merges-hints-on-one-line ()
  "Two annotated words on one line share a single phantom hint line."
  (with-temp-buffer
    (insert "alpha bravo\n")
    (wordwise--make-overlay 1 6 "first hint")
    (wordwise--make-overlay 7 12 "second hint")
    (let ((lines (wordwise-test--hint-lines)))
      (should (= (length lines) 1))
      (let ((s (overlay-get (car lines) 'before-string)))
        (should (string-match-p "first hint" s))
        (should (string-match-p "second hint" s))
        (should (= 1 (seq-count (lambda (c) (eq c ?\n)) s)))))))

(ert-deftest wordwise-hints-ordered-by-word-position ()
  "Hints appear left-to-right in buffer order, however they were added."
  (with-temp-buffer
    (insert "alpha bravo\n")
    (wordwise--make-overlay 7 12 "later-word")
    (wordwise--make-overlay 1 6 "earlier-word")
    (let ((s (overlay-get (car (wordwise-test--hint-lines)) 'before-string)))
      (should (< (string-match "earlier-word" s)
                 (string-match "later-word" s))))))

(ert-deftest wordwise-annotates-word-at-line-start ()
  "A hint elsewhere on the line must not mark the line-start word annotated."
  (with-temp-buffer
    (insert "alpha bravo\n")
    (wordwise--make-overlay 7 12 "second hint")
    (should-not (wordwise--already-annotated-p 1 6))
    (wordwise--make-overlay 1 6 "first hint")
    (should (wordwise--already-annotated-p 1 6))))

(ert-deftest wordwise-separate-lines-stay-separate ()
  "Words on different lines keep their own hint lines."
  (with-temp-buffer
    (insert "alpha\nbravo\n")
    (wordwise--make-overlay 1 6 "first hint")
    (wordwise--make-overlay 7 12 "second hint")
    (should (= (length (wordwise-test--hint-lines)) 2))))

(provide 'wordwise-test)
;;; wordwise-test.el ends here
