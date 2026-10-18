;;; wordwise-test.el --- Tests for wordwise  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Palak Mathur

;;; Commentary:

;; Run with: make test
;; (emacs --batch -Q -L . -l wordwise-test.el -f ert-run-tests-batch-and-exit)

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'savehist)
(require 'wordwise)

(defmacro wordwise-test--with-state (&rest body)
  "Run BODY with isolated Wordwise cache and request state."
  (declare (indent 0) (debug t))
  `(let ((wordwise-cache (make-hash-table :test #'equal))
         (wordwise--process nil)
         (wordwise--connection-ready nil)
         (wordwise--wire-data "")
         (wordwise--queue nil)
         (wordwise--pending (make-hash-table :test #'equal))
         (wordwise--current-word nil)
         (wordwise--response "")
         (wordwise--request-timer nil)
         (wordwise--retry-timer nil)
         (wordwise--retry-at nil)
         (wordwise--failure-cache (make-hash-table :test #'equal))
         (wordwise--failure-retry-timer nil)
         (wordwise--last-network-error nil))
     (unwind-protect
         (progn ,@body)
       (dolist (timer (list wordwise--request-timer
                            wordwise--retry-timer
                            wordwise--failure-retry-timer))
         (when (timerp timer)
           (cancel-timer timer)))
       (when (processp wordwise--process)
         (set-process-sentinel wordwise--process #'ignore)
         (when (process-live-p wordwise--process)
           (delete-process wordwise--process))))))

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

(ert-deftest wordwise-response-status-waits-for-terminal-line ()
  "Only a terminal 250/5xx line completes a DICT response."
  (should-not
   (wordwise--response-status
    "150 1 definitions retrieved\r\n151 word db description\r\n"))
  (should
   (eq 'complete
       (wordwise--response-status
        "150 1 definitions retrieved\r\n.\r\n250 ok\r\n")))
  (should (eq 'missing (wordwise--response-status "552 no match\r\n")))
  (should (eq 'error (wordwise--response-status "550 invalid database\r\n"))))

(ert-deftest wordwise-definition-from-response-extracts-first-sense ()
  "The first numbered sense of the first 151 body becomes the hint."
  (should
   (equal
    "Having keen insight."
    (wordwise--definition-from-response
     (concat "150 1 definitions retrieved\r\n"
             "151 perspicacious db description\r\n"
             "  1. Having keen insight.\r\n"
             ".\r\n250 ok\r\n")))))

(ert-deftest wordwise-queue-deduplicates-words-and-buffers ()
  "A word is queued once; extra requests only add demanding buffers."
  (wordwise-test--with-state
    (let ((first (generate-new-buffer " *wordwise-first*"))
          (second (generate-new-buffer " *wordwise-second*")))
      (unwind-protect
          (cl-letf (((symbol-function 'wordwise--pump) #'ignore))
            (wordwise--queue-word "perspicacious" first)
            (wordwise--queue-word "perspicacious" first)
            (wordwise--queue-word "perspicacious" second)
            (should (equal wordwise--queue '("perspicacious")))
            (should (= 2 (length (gethash "perspicacious"
                                          wordwise--pending))))
            (puthash "cached" "already known" wordwise-cache)
            (wordwise--queue-word "cached" first)
            (should (equal wordwise--queue '("perspicacious"))))
        (kill-buffer first)
        (kill-buffer second)))))

(ert-deftest wordwise-filter-handles-chunked-success ()
  "A DICT response split across filter chunks still parses and caches."
  (wordwise-test--with-state
    (let ((proc (make-pipe-process :name "wordwise-test" :noquery t)))
      (unwind-protect
          (progn
            (setq wordwise--process proc
                  wordwise--connection-ready t
                  wordwise--current-word "perspicacious")
            (puthash "perspicacious" (list (current-buffer))
                     wordwise--pending)
            (cl-letf (((symbol-function 'wordwise--pump) #'ignore)
                      ((symbol-function 'wordwise--schedule-scan) #'ignore))
              (wordwise--process-filter
               proc
               (concat "150 1 definitions retrieved\r\n"
                       "151 perspicacious db description\r\n"
                       "  1. Having keen insight.\r\n"))
              (should (equal wordwise--current-word "perspicacious"))
              (wordwise--process-filter proc ".\r\n250 ok\r\n")
              (should-not wordwise--current-word)
              (should (equal (gethash "perspicacious" wordwise-cache)
                             "Having keen insight."))))
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest wordwise-completion-persists-missing-separately ()
  "A DICT 552 caches `missing' and clears the pending demand."
  (wordwise-test--with-state
    (setq wordwise--current-word "neologism")
    (puthash "neologism" (list (current-buffer)) wordwise--pending)
    (cl-letf (((symbol-function 'wordwise--pump) #'ignore)
              ((symbol-function 'wordwise--schedule-scan) #'ignore))
      (wordwise--complete-current 'missing))
    (should (eq 'missing (gethash "neologism" wordwise-cache)))
    (should (eq 'miss (gethash "neologism" wordwise--pending 'miss)))))

(ert-deftest wordwise-in-prose-p-follows-face-inheritance ()
  "Faces that inherit from a skip face are not prose."
  (with-temp-buffer
    (insert "word")
    (dolist (case '((nil . t)
                    (org-quote . t)
                    (org-block . nil)
                    ((org-block) . nil)
                    ((:inherit (org-block)) . nil)
                    ((:inherit org-block :extend t) . nil)
                    ((bold (:inherit org-code)) . nil)))
      (put-text-property (point-min) (point-max) 'face (car case))
      (should (eq (cdr case)
                  (and (wordwise--in-prose-p (point-min)) t))))))

(ert-deftest wordwise-scan-queues-cold-and-renders-warm-words ()
  "Scanning queues uncached words and renders cached ones immediately."
  (wordwise-test--with-state
    (with-temp-buffer
      (fundamental-mode)
      (insert "perspicacious")
      (let ((wordwise-allowed-modes '(fundamental-mode))
            (wordwise-common-words (make-hash-table :test #'equal))
            (font-lock-defaults nil)
            queued
            rendered)
        (cl-letf (((symbol-function 'wordwise--in-prose-p)
                   (lambda (_pos) t))
                  ((symbol-function 'wordwise--queue-word)
                   (lambda (word _buffer) (push word queued)))
                  ((symbol-function 'wordwise--make-overlay)
                   (lambda (_beg _end hint &optional _window)
                     (push hint rendered))))
          (wordwise--scan-region (point-min) (point-max))
          (should (equal queued '("perspicacious")))
          (should-not rendered)
          (setq queued nil)
          (puthash "perspicacious" "Having keen insight." wordwise-cache)
          (wordwise--scan-region (point-min) (point-max))
          (should-not queued)
          (should (equal rendered '("Having keen insight."))))))))

(ert-deftest wordwise-scroll-hook-only-schedules-work ()
  "The scroll hook schedules a scan without measuring windows in redisplay."
  (let ((buf (generate-new-buffer " *wordwise-scroll*"))
        scheduled)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buf)
          (setq-local wordwise-mode t)
          (cl-letf (((symbol-function 'wordwise--schedule-scan)
                     (lambda (buffer &optional _delay)
                       (setq scheduled buffer)))
                    ((symbol-function 'window-end)
                     (lambda (&rest _args)
                       (ert-fail "scroll hook read window-end during redisplay"))))
            (wordwise--on-scroll (selected-window) nil)
            (should (eq scheduled buf))))
      (kill-buffer buf))))

(defvar wordwise-test--savehist-cache nil)

(ert-deftest wordwise-cache-round-trips-through-savehist ()
  "The definition cache survives a savehist save/load cycle intact."
  (let ((test-file (make-temp-file "wordwise-savehist-test-"))
        (wordwise-test--savehist-cache
         (let ((table (make-hash-table :test #'equal)))
           (puthash "perspicacious" "Having keen insight." table)
           (puthash "unlisted" 'missing table)
           table)))
    (unwind-protect
        (let ((savehist-file test-file)
              (savehist-additional-variables
               '(wordwise-test--savehist-cache)))
          (savehist-save)
          (setq wordwise-test--savehist-cache nil)
          (load test-file nil t t)
          (should (hash-table-p wordwise-test--savehist-cache))
          (should (equal "Having keen insight."
                         (gethash "perspicacious"
                                  wordwise-test--savehist-cache)))
          (should (eq 'missing
                      (gethash "unlisted" wordwise-test--savehist-cache))))
      (delete-file test-file))))

(ert-deftest wordwise-completion-advances-the-queue ()
  "Finishing one word sends the DICT request for the next queued word."
  (wordwise-test--with-state
    (let ((proc (make-pipe-process :name "wordwise-pump-test" :noquery t))
          sent)
      (unwind-protect
          (progn
            (setq wordwise--process proc
                  wordwise--connection-ready t
                  wordwise--current-word "first"
                  wordwise--queue '("second"))
            (puthash "first" (list (current-buffer)) wordwise--pending)
            (puthash "second" (list (current-buffer)) wordwise--pending)
            (cl-letf (((symbol-function 'wordwise--schedule-scan) #'ignore)
                      ((symbol-function 'process-send-string)
                       (lambda (_process string) (setq sent string)))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _args) nil)))
              (wordwise--complete-current "first definition")
              (should (equal wordwise--current-word "second"))
              (should (equal sent "DEFINE * second\r\n"))
              (should (equal (gethash "first" wordwise-cache)
                             "first definition"))))
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest wordwise-timeout-requeues-and-backs-off ()
  "A request timeout requeues the word and starts the network backoff."
  (wordwise-test--with-state
    (let (retry-scheduled)
      (setq wordwise--current-word "perspicacious")
      (puthash "perspicacious" (list (current-buffer)) wordwise--pending)
      (cl-letf (((symbol-function 'wordwise--close-process) #'ignore)
                ((symbol-function 'wordwise--schedule-network-retry)
                 (lambda () (setq retry-scheduled t)))
                ((symbol-function 'message) #'ignore))
        (wordwise--request-timed-out "perspicacious"))
      (should-not wordwise--current-word)
      (should (equal wordwise--queue '("perspicacious")))
      (should (> wordwise--retry-at (float-time)))
      (should retry-scheduled))))

(provide 'wordwise-test)
;;; wordwise-test.el ends here
