;;; wordwise.el --- Kindle-style inline vocabulary hints -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Palak Mathur

;; Author: Palak Mathur <palakmathur@gmail.com>
;; URL: https://github.com/systemhalted/wordwise.el
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, wp, hypermedia

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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

;; A reading aid inspired by (and an homage to) Amazon Kindle's Word
;; Wise feature.  When `wordwise-mode' is active, uncommon English words
;; in the buffer get a short dictionary definition displayed on a
;; phantom line above the word.
;;
;; A bundled frequency list (~9 500 common English words in five tiers)
;; decides which words are common enough to skip; the aggressiveness is
;; controlled by `wordwise-difficulty' (1-5, default 3).  Definitions
;; come from dict.org over one shared asynchronous DICT connection
;; (note: annotated words are sent to dict.org in plain text) and are
;; cached in `wordwise-cache', which persists across restarts when
;; savehist is enabled.  Annotation is restricted to prose regions of
;; reading-oriented major modes (`wordwise-allowed-modes').
;;
;; Usage: M-x wordwise-mode in an Org, Markdown, nov or eww buffer, and
;; bind it to a convenient key.  `wordwise-set-difficulty' changes the
;; annotation density; `wordwise-ignore-word' silences the word at
;; point; `wordwise-clear-cache' flushes cached definitions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'wordwise-common-words)
(defgroup wordwise nil
  "Kindle-style inline vocabulary hints."
  :group 'convenience
  :prefix "wordwise-")

(defcustom wordwise-difficulty 3
  "How aggressively to annotate words (1-5).
Level 1: only words absent from the entire frequency list.
Level 2: words outside the top ~10 000.
Level 3: words outside the top ~5 000.
Level 4: words outside the top ~3 000.
Level 5: words outside the top ~1 000 (most annotations)."
  :type '(integer :tag "Difficulty" :match
                  (lambda (_w value) (and (integerp value) (<= 1 value 5))))
  :group 'wordwise)

(defcustom wordwise-allowed-modes
  '(org-mode nov-mode markdown-mode gfm-mode eww-mode)
  "Major modes in which Wordwise annotations are permitted."
  :type '(repeat symbol)
  :group 'wordwise)

(defcustom wordwise-max-hint-length 50
  "Maximum character length of a hint string above a word."
  :type 'integer
  :group 'wordwise)

(defcustom wordwise-request-timeout 5
  "Seconds to wait for one asynchronous DICT response."
  :type 'number
  :group 'wordwise)

(defcustom wordwise-ignored-words nil
  "Words that Wordwise should never annotate.
Use `wordwise-ignore-word' to add the word at point
interactively, or customize this list directly."
  :type '(repeat string)
  :group 'wordwise)

(defface wordwise-hint-face
  '((t :inherit shadow :height 0.7))
  "Face for the Wordwise hint text displayed above annotated words."
  :group 'wordwise)

(defvar wordwise-cache (make-hash-table :test #'equal :size 512)
  "Persistent cache mapping a word to its definition, or `missing'.
Held in the `defvar' so re-loading the library keeps it — every entry
costs a network round-trip to dict.org, and flushing on reload would
cold-start annotation.  Flush explicitly with `wordwise-clear-cache'.")

;; Persist fetched definitions across Emacs restarts through savehist —
;; the same mechanism that already keeps kill-ring and corfu-history.
;; Savehist restores the table before this library loads, so the defvar
;; above leaves the restored value in place.  Old `none' entries combined
;; "not found" with transport errors, so discard them during migration;
;; new `missing' entries represent a confirmed DICT 552 response.
(defvar savehist-additional-variables)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'wordwise-cache))
(maphash (lambda (word def)
           (unless (or (stringp def) (eq def 'missing))
             (remhash word wordwise-cache)))
         wordwise-cache)

(when (fboundp 'wordwise--reset-requests)
  ;; Re-loading the library keeps the expensive definition cache but must
  ;; not leave timers or process filters from the old implementation alive.
  (wordwise--reset-requests))

(defconst wordwise--retry-delay 60
  "Seconds to back off after a DICT connection failure.")

(defvar wordwise--process nil
  "Shared asynchronous DICT network process.")

(defvar wordwise--connection-ready nil
  "Non-nil after the DICT server greeting has been received.")

(defvar wordwise--wire-data ""
  "Unprocessed data received from the DICT server.")

(defvar wordwise--queue nil
  "FIFO list of words waiting for a DICT request.")

(defvar wordwise--pending (make-hash-table :test #'equal)
  "Words queued or in flight, mapped to interested buffers.")

(defvar wordwise--current-word nil
  "Word whose DICT response is currently in flight.")

(defvar wordwise--response ""
  "Accumulated DICT response for `wordwise--current-word'.")

(defvar wordwise--request-timer nil
  "Timeout timer for the current DICT request.")

(defvar wordwise--retry-timer nil
  "Timer used to retry the DICT connection after backoff.")

(defvar wordwise--retry-at nil
  "Absolute time after which a failed DICT connection may be retried.")

(defvar wordwise--failure-cache
  (make-hash-table :test #'equal)
  "Session-local word failures mapped to their retry time.")

(defvar wordwise--failure-retry-timer nil
  "Timer that re-scans visible buffers after word-level failures expire.")

(defvar wordwise--last-network-error nil
  "Last reported DICT connection error, used to suppress message spam.")

(defvar wordwise--skip-faces
  '(org-block org-block-begin-line org-block-end-line
    org-meta-line org-drawer org-property-value
    org-special-keyword org-document-info-keyword
    org-link org-tag org-code org-verbatim
    markdown-pre-face markdown-inline-code-face
    markdown-language-keyword-face markdown-code-face
    markdown-url-face
    shr-link shr-code)
  "Faces marking non-prose regions that Wordwise skips.")

(defvar-local wordwise--scan-timer nil
  "Idle timer for re-scanning visible windows after scrolling.")

;; --- Tier / difficulty logic ---

(defun wordwise--tier-threshold ()
  "Return the maximum tier that should be SKIPPED at the current difficulty."
  (- 6 wordwise-difficulty))

(defun wordwise--word-needs-annotation-p (word)
  "Return non-nil if WORD should be annotated at the current difficulty."
  (when (and (boundp 'wordwise-common-words)
             wordwise-common-words
             (not (member word wordwise-ignored-words)))
    (let ((tier (gethash word wordwise-common-words)))
      (if tier
          (> tier (wordwise--tier-threshold))
        t))))

;; --- Prose detection and code-like word filter ---

(defun wordwise--in-prose-p (pos)
  "Return non-nil if POS is in a prose region suitable for annotation.
Checks the face text property at POS; non-prose faces (source blocks,
inline code, drawers, links, etc.) are listed in `--skip-faces'."
  (let* ((face (get-text-property pos 'face))
         (faces (cond ((null face) nil)
                      ((listp face) face)
                      (t (list face)))))
    (not (seq-some (lambda (f)
                     (memq f wordwise--skip-faces))
                   faces))))

(defun wordwise--code-like-p (word)
  "Return non-nil if WORD looks like a code identifier rather than prose."
  (let ((case-fold-search nil))
    (or (string-match-p "[_/]" word)
        (string-match-p "[a-z][A-Z]" word)
        (string-match-p "\\`[A-Z]+\\'" word)
        (string-match-p "\\`[A-Z][a-z]+[A-Z]" word))))

;; --- DICT response parsing ---

(defun wordwise--extract-first-sense (body)
  "Extract the first sense from a DICT protocol definition BODY."
  (when (and body (stringp body) (> (length body) 0))
    (let* ((lines (split-string body "\n" t "[ \t]+"))
           (def-line
            (or (seq-find (lambda (l) (string-match-p "\\`[0-9]+\\." l)) lines)
                (seq-find (lambda (l)
                            (and (> (length l) 10)
                                 (not (string-match-p "\\\\[A-Z]" l))
                                 (not (string-match-p "\\`From " l))
                                 (not (string-match-p "\\`\\[" l))))
                          lines)))
           (trimmed (when def-line
                      (replace-regexp-in-string
                       "\\`[0-9]+\\.\\s-*" ""
                       (replace-regexp-in-string "\\\\[^\\\\]*\\\\" "" def-line)))))
      (when (and trimmed (> (length (string-trim trimmed)) 0))
        (let ((clean (string-trim trimmed)))
          (if (> (length clean) wordwise-max-hint-length)
              (concat (substring clean 0 (- wordwise-max-hint-length 3)) "...")
            clean))))))

;; --- Asynchronous DICT client ---

(defun wordwise--response-status (response)
  "Return the terminal DICT status represented by RESPONSE.
The result is `complete', `missing', `error', or nil while more data is needed."
  (when (string-match
         "\\(?:\\`\\|\n\\)\\(250\\|5[0-9][0-9]\\) [^\r\n]*\\(?:\r?\n\\|\\'\\)"
         response)
    (pcase (match-string 1 response)
      ("250" 'complete)
      ("552" 'missing)
      (_ 'error))))

(defun wordwise--definition-from-response (response)
  "Extract the first definition body from a complete DICT RESPONSE."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (when (re-search-forward "^151 " nil t)
      (forward-line 1)
      (let ((body-start (point)))
        (when (re-search-forward "^\\.\r?$" nil t)
          (wordwise--extract-first-sense
           (buffer-substring-no-properties
            body-start (line-beginning-position))))))))

(defun wordwise--cancel-request-timer ()
  "Cancel the current DICT request timeout."
  (when (timerp wordwise--request-timer)
    (cancel-timer wordwise--request-timer))
  (setq wordwise--request-timer nil))

(defun wordwise--close-process ()
  "Close and forget the shared DICT process without running its sentinel."
  (let ((proc wordwise--process))
    (setq wordwise--process nil
          wordwise--connection-ready nil
          wordwise--wire-data "")
    (when (processp proc)
      (let ((buf (process-buffer proc)))
        (set-process-sentinel proc #'ignore)
        (when (process-live-p proc)
          (delete-process proc))
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(defun wordwise--cancel-scan-timer ()
  "Cancel the current buffer's deferred Wordwise scan."
  (when (timerp wordwise--scan-timer)
    (cancel-timer wordwise--scan-timer))
  (setq wordwise--scan-timer nil))

(defun wordwise--reset-requests ()
  "Cancel transient Wordwise network and scan state, preserving definitions."
  (wordwise--cancel-request-timer)
  (dolist (timer (list wordwise--retry-timer
                       wordwise--failure-retry-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq wordwise--retry-timer nil
        wordwise--failure-retry-timer nil)
  (wordwise--close-process)
  (setq wordwise--queue nil
        wordwise--current-word nil
        wordwise--response ""
        wordwise--retry-at nil
        wordwise--last-network-error nil)
  (clrhash wordwise--pending)
  (clrhash wordwise--failure-cache)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (local-variable-p 'wordwise--scan-timer buf)
        (wordwise--cancel-scan-timer)))))

(defun wordwise--active-buffers ()
  "Return live buffers where Wordwise mode is enabled."
  (seq-filter
   (lambda (buf)
     (with-current-buffer buf
       (bound-and-true-p wordwise-mode)))
   (buffer-list)))

(defun wordwise--schedule-scan (buffer &optional delay)
  "Schedule a Wordwise scan of BUFFER after idle DELAY seconds."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (bound-and-true-p wordwise-mode)
        (wordwise--cancel-scan-timer)
        (setq wordwise--scan-timer
              (run-with-idle-timer
               (or delay 0.1) nil
               (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq wordwise--scan-timer nil)
                     (wordwise--scan-visible))))
               buffer))))))

(defun wordwise--schedule-active-scans (&optional delay)
  "Schedule scans in all active Wordwise buffers after DELAY seconds."
  (dolist (buf (wordwise--active-buffers))
    (wordwise--schedule-scan buf delay)))

(defun wordwise--schedule-failure-retry ()
  "Arrange one future scan after session-local word failures expire."
  (unless (timerp wordwise--failure-retry-timer)
    (setq wordwise--failure-retry-timer
          (run-at-time
           wordwise--retry-delay nil
           (lambda ()
             (setq wordwise--failure-retry-timer nil)
             (wordwise--schedule-active-scans))))))

(defun wordwise--schedule-network-retry ()
  "Schedule the request pump when the current connection backoff expires."
  (when (timerp wordwise--retry-timer)
    (cancel-timer wordwise--retry-timer))
  (let ((delay (max 0.1 (- (or wordwise--retry-at 0)
                           (float-time)))))
    (setq wordwise--retry-timer
          (run-at-time
           delay nil
           (lambda ()
             (setq wordwise--retry-timer nil
                   wordwise--retry-at nil)
             (wordwise--pump))))))

(defun wordwise--network-failure (reason)
  "Handle a DICT connection failure described by REASON without blocking."
  (wordwise--cancel-request-timer)
  (when wordwise--current-word
    (let ((word wordwise--current-word))
      (if (gethash word wordwise--pending)
          (push word wordwise--queue)
        (remhash word wordwise--pending))))
  (setq wordwise--current-word nil
        wordwise--response ""
        wordwise--retry-at
        (+ (float-time) wordwise--retry-delay))
  (wordwise--close-process)
  (unless (equal reason wordwise--last-network-error)
    (setq wordwise--last-network-error reason)
    (message "Wordwise DICT unavailable; retrying in %ds (%s)"
             wordwise--retry-delay reason))
  (when wordwise--queue
    (wordwise--schedule-network-retry)))

(defun wordwise--complete-current (result)
  "Finish the current request with RESULT and advance the queue.
RESULT is a definition string, `missing', or `error'."
  (let* ((word wordwise--current-word)
         (buffers (and word
                       (copy-sequence
                        (gethash word wordwise--pending)))))
    (wordwise--cancel-request-timer)
    (when word
      (pcase result
        ((pred stringp)
         (puthash word result wordwise-cache)
         (setq wordwise--last-network-error nil))
        ('missing
         (puthash word 'missing wordwise-cache)
         (setq wordwise--last-network-error nil))
        (_
         (puthash word
                  (+ (float-time) wordwise--retry-delay)
                  wordwise--failure-cache)
         (wordwise--schedule-failure-retry)))
      (remhash word wordwise--pending))
    (setq wordwise--current-word nil
          wordwise--response "")
    (dolist (buf buffers)
      (wordwise--schedule-scan buf))
    (wordwise--pump)))

(defun wordwise--request-timed-out (word)
  "Fail the active request when WORD exceeds its timeout."
  (when (equal word wordwise--current-word)
    (wordwise--network-failure
     (format "timeout looking up %s" word))))

(defun wordwise--process-filter (proc chunk)
  "Consume a CHUNK of DICT protocol data from PROC."
  (when (eq proc wordwise--process)
    (setq wordwise--wire-data
          (concat wordwise--wire-data chunk))
    (unless wordwise--connection-ready
      (when (string-match "\\`\\([^\n]*\\)\r?\n" wordwise--wire-data)
        (let ((greeting (match-string 1 wordwise--wire-data)))
          (setq wordwise--wire-data
                (substring wordwise--wire-data (match-end 0)))
          (if (string-prefix-p "220 " greeting)
              (progn
                (setq wordwise--connection-ready t)
                (wordwise--pump))
            (wordwise--network-failure greeting)))))
    (when (and (eq proc wordwise--process)
               wordwise--connection-ready
               wordwise--current-word
               (> (length wordwise--wire-data) 0))
      (setq wordwise--response
            (concat wordwise--response
                    wordwise--wire-data)
            wordwise--wire-data "")
      (pcase (wordwise--response-status
              wordwise--response)
        ('complete
         (wordwise--complete-current
          (or (wordwise--definition-from-response
               wordwise--response)
              'error)))
        ('missing
         (wordwise--complete-current 'missing))
        ('error
         (wordwise--complete-current 'error))))))

(defun wordwise--process-sentinel (proc event)
  "Handle a connection state EVENT for the shared DICT PROC."
  (when (and (eq proc wordwise--process)
             (memq (process-status proc) '(closed failed exit signal)))
    (if (or wordwise--current-word
            wordwise--queue)
        (wordwise--network-failure (string-trim event))
      (wordwise--close-process))))

(defun wordwise--ensure-process ()
  "Start the asynchronous shared DICT connection when necessary."
  (unless (process-live-p wordwise--process)
    (wordwise--close-process)
    (let ((buf (generate-new-buffer " *wordwise-dict*")))
      (condition-case err
          (setq wordwise--process
                (make-network-process
                 :name "wordwise-dict"
                 :buffer buf
                 :host "dict.org"
                 :service 2628
                 :nowait t
                 :noquery t
                 :coding 'utf-8-unix
                 :filter #'wordwise--process-filter
                 :sentinel #'wordwise--process-sentinel))
        (error
         (when (buffer-live-p buf)
           (kill-buffer buf))
         (wordwise--network-failure
          (error-message-string err)))))))

(defun wordwise--pump ()
  "Send the next queued word when the shared DICT connection is ready."
  (cond
   ((and wordwise--retry-at
         (> wordwise--retry-at (float-time)))
    (when wordwise--queue
      (wordwise--schedule-network-retry)))
   ((and (null wordwise--current-word)
         wordwise--queue)
    (wordwise--ensure-process)
    (when (and (process-live-p wordwise--process)
               wordwise--connection-ready)
      (while (and wordwise--queue
                  (null wordwise--current-word))
        (let ((word (pop wordwise--queue)))
          (if (eq (gethash word wordwise--pending 'miss) 'miss)
              nil
            (setq wordwise--current-word word
                  wordwise--response "")
            (condition-case err
                (progn
                  (process-send-string
                   wordwise--process
                   (format "DEFINE * %s\r\n" word))
                  (setq wordwise--request-timer
                        (run-at-time
                         wordwise-request-timeout nil
                         #'wordwise--request-timed-out word)))
              (error
               (wordwise--network-failure
                (error-message-string err)))))))))))

(defun wordwise--queue-word (word buffer)
  "Queue WORD for BUFFER, deduplicating pending requests."
  (let ((failed-until (gethash word wordwise--failure-cache)))
    (when (and failed-until (<= failed-until (float-time)))
      (remhash word wordwise--failure-cache)
      (setq failed-until nil))
    (when (and (eq (gethash word wordwise-cache 'miss) 'miss)
               (not failed-until))
      (let ((waiting (gethash word wordwise--pending 'miss)))
        (if (eq waiting 'miss)
            (progn
              (puthash word (list buffer) wordwise--pending)
              (setq wordwise--queue
                    (nconc wordwise--queue (list word))))
          (cl-pushnew buffer waiting)
          (puthash word waiting wordwise--pending)))
      (wordwise--pump))))

(defun wordwise--remove-buffer-demand (buffer)
  "Remove BUFFER from queued Wordwise requests that it no longer needs."
  (maphash
   (lambda (word buffers)
     (let ((remaining (delq buffer buffers)))
       (cond
        ((eq word wordwise--current-word)
         (puthash word remaining wordwise--pending))
        (remaining
         (puthash word remaining wordwise--pending))
        (t
         (remhash word wordwise--pending)
         (setq wordwise--queue
               (delete word wordwise--queue))))))
   wordwise--pending))

;;;###autoload
(defun wordwise-clear-cache ()
  "Empty Wordwise definitions, cancel requests, and re-annotate active buffers."
  (interactive)
  (wordwise--reset-requests)
  (clrhash wordwise-cache)
  (dolist (buf (wordwise--active-buffers))
    (with-current-buffer buf
      (wordwise--remove-overlays)
      (wordwise--scan-visible)))
  (message "Wordwise cache cleared"))

;; --- Overlay management ---
;;
;; One display overlay per visual line plus one marker overlay per word:
;; 1. The display overlay sits at the visual-line beginning; its
;;    before-string is the phantom hint line.  All annotated words on
;;    that visual line share it — each contributes an (X UNIT HINT)
;;    entry to its `wordwise-hints' property and the line is re-rendered,
;;    so hints sit side by side instead of stacking one line each.
;;    Hints are pixel-aligned to their word via (space :align-to) so
;;    alignment survives proportional fonts and wrapped lines.
;; 2. The marker overlay spans the word — for duplicate detection.
;;
;; Both evaporate: nov (on chapter change) and eww (on history
;; navigation) erase the buffer without removing overlays, and an empty
;; display overlay would otherwise keep drawing a stale hint line at the
;; top of the new page.

(defun wordwise--display-overlay-at (pos)
  "Return the Wordwise hint-line overlay starting at POS, if any."
  (seq-find (lambda (ov)
              (and (overlay-get ov 'wordwise-hints)
                   (= (overlay-start ov) pos)))
            (overlays-in pos (min (1+ pos) (point-max)))))

(defun wordwise--hint-line (hints)
  "Render HINTS, a list of (X UNIT HINT) entries, as one phantom line.
X is a pixel offset when UNIT is `px' (from `posn-x-y'), or a character
column when UNIT is `col' (the no-window fallback).  Hints are laid out
left to right in word order; each is pushed to its word's position with
\(space :align-to), which collapses to nothing when the previous hint
already reached past that point, leaving a plain two-space gap."
  (let ((sorted (sort (copy-sequence hints)
                      (lambda (a b) (< (car a) (car b)))))
        (parts nil))
    (dolist (entry sorted)
      (pcase-let ((`(,x ,unit ,hint) entry))
        (when parts (push "  " parts))
        (push (propertize " " 'display
                          (if (eq unit 'px)
                              `(space :align-to (,x))
                            `(space :align-to ,x)))
              parts)
        (push (propertize hint 'face 'wordwise-hint-face) parts)))
    (apply #'concat (nreverse (cons "\n" parts)))))

(defun wordwise--make-overlay (beg end hint &optional window)
  "Add HINT for the word BEG..END to its line's phantom hint line.
WINDOW supplies display measurements for pixel alignment; without a
window position the hint falls back to character-column alignment."
  (save-excursion
    (goto-char beg)
    (let* ((visual-bol (progn (vertical-motion 0 window) (point)))
           (posn (posn-at-point beg window))
           (x-pixel (when posn (car (posn-x-y posn))))
           (entry (if x-pixel
                      (list x-pixel 'px hint)
                    (list (- beg visual-bol) 'col hint)))
           (display-ov (or (wordwise--display-overlay-at visual-bol)
                           (make-overlay visual-bol
                                         (min (1+ visual-bol) (point-max)))))
           (marker-ov (make-overlay beg end nil t nil))
           (hints (cons entry (overlay-get display-ov 'wordwise-hints))))
      (overlay-put display-ov 'wordwise t)
      (overlay-put display-ov 'evaporate t)
      (overlay-put display-ov 'wordwise-hints hints)
      (overlay-put display-ov 'before-string (wordwise--hint-line hints))
      (overlay-put marker-ov 'wordwise t)
      (overlay-put marker-ov 'wordwise-marker t)
      (overlay-put marker-ov 'evaporate t))))

(defun wordwise--already-annotated-p (beg end)
  "Return non-nil if a Wordwise marker overlay spans BEG..END.
Only marker overlays count: the shared display overlay at the visual
line's beginning must not make the line's first word look annotated."
  (seq-find (lambda (ov)
              (and (overlay-get ov 'wordwise-marker)
                   (> (- (overlay-end ov) (overlay-start ov)) 0)))
            (overlays-in beg end)))

(defun wordwise--remove-overlays ()
  "Remove all Wordwise overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'wordwise t))

;; --- Scan visible region (replaces jit-lock) ---

(defun wordwise--scan-visible ()
  "Scan every live window showing the current Wordwise buffer."
  (when (bound-and-true-p wordwise-mode)
    (let ((buf (current-buffer)))
      ;; Reconcile demand on each complete visible scan so rapidly scrolling
      ;; through a book cannot leave an ever-growing queue of stale words.
      (wordwise--remove-buffer-demand buf)
      (dolist (win (get-buffer-window-list buf nil t))
        (when (and (window-live-p win)
                   (eq (window-buffer win) buf))
          ;; `window-end' is valid here because scroll hooks only schedule
          ;; this scan; they never inspect it during redisplay.
          (let ((end (window-end win t)))
            (when end
              (wordwise--scan-region
               (window-start win) end win))))))))

(defun wordwise--scan-region (beg end &optional window)
  "Scan BEG..END for uncommon prose words.
Cached definitions get overlays immediately; misses are queued without
waiting for network output.  WINDOW supplies display measurements."
  (when (apply #'derived-mode-p wordwise-allowed-modes)
    ;; Prose detection reads `face' text properties, which exist only after
    ;; font-lock has fontified the text — and the scroll hook fires before
    ;; jit-lock reaches newly exposed regions.  Fontify first, or source
    ;; blocks scan as prose.  Gated on `font-lock-defaults': shr-rendered
    ;; modes (nov, eww) don't use font-lock, their faces are already in
    ;; place, and `font-lock-ensure' would strip them.
    (when font-lock-defaults
      (font-lock-ensure beg end))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\b\\([a-zA-Z]\\{4,\\}\\)\\b" end t)
        (let* ((word-beg (match-beginning 1))
               (word-end (match-end 1))
               (raw-word (match-string-no-properties 1))
               (word (downcase raw-word)))
          (unless (or (not (wordwise--in-prose-p word-beg))
                      (wordwise--already-annotated-p
                       word-beg word-end)
                      (wordwise--code-like-p raw-word)
                      (not (wordwise--word-needs-annotation-p word)))
            (let ((cached (gethash word wordwise-cache 'miss)))
              (cond
               ((eq cached 'miss)
                (wordwise--queue-word word (current-buffer)))
               ((eq cached 'missing) nil)
               ((stringp cached)
                (wordwise--make-overlay
                 word-beg word-end cached window))))))))))

;; --- Scroll hook ---

(defun wordwise--on-scroll (win _start)
  "Schedule a scan of WIN's buffer after scrolling and redisplay settle."
  (when (window-live-p win)
    (let ((buf (window-buffer win)))
      (when (buffer-live-p buf)
        (wordwise--schedule-scan buf)))))

;; --- Re-render hooks ---

(defconst wordwise--render-hooks
  '(eww-after-render-hook nov-post-html-render-hook)
  "Hooks run after a reading mode replaces the buffer with a new page.
A fresh render does not reliably trigger `window-scroll-functions',
so without these the new page stays unannotated until the next scroll.")

(defun wordwise--on-render ()
  "Drop hints from the previous page and schedule a scan of the new one."
  (wordwise--remove-overlays)
  (wordwise--schedule-scan (current-buffer)))

(defun wordwise--add-hooks ()
  "Install Wordwise's buffer-local scroll and render hooks."
  (add-hook 'window-scroll-functions #'wordwise--on-scroll nil t)
  (dolist (hook wordwise--render-hooks)
    (add-hook hook #'wordwise--on-render nil t)))

(defun wordwise--remove-hooks ()
  "Remove Wordwise's buffer-local scroll and render hooks."
  (remove-hook 'window-scroll-functions #'wordwise--on-scroll t)
  (dolist (hook wordwise--render-hooks)
    (remove-hook hook #'wordwise--on-render t)))

;; --- Minor mode ---

;;;###autoload
(define-minor-mode wordwise-mode
  "Kindle-style inline vocabulary hints.
When enabled, uncommon English words in prose regions are annotated
with a short dictionary definition on a phantom line above the word.
Definitions are fetched over one asynchronous dict.org connection, one
word at a time, without waiting in redisplay.  Difficulty is controlled by
`wordwise-difficulty' (1-5)."
  :lighter " WordWise"
  :group 'wordwise
  (if wordwise-mode
      (if (apply #'derived-mode-p wordwise-allowed-modes)
          (progn
            (wordwise--add-hooks)
            (wordwise--scan-visible)
            (message "Wordwise enabled (difficulty %d)"
                     wordwise-difficulty))
        (setq wordwise-mode nil)
        (message "Wordwise not available in %s" major-mode))
    (wordwise--remove-hooks)
    (wordwise--cancel-scan-timer)
    (wordwise--remove-buffer-demand (current-buffer))
    (wordwise--remove-overlays)
    (unless (wordwise--active-buffers)
      (wordwise--reset-requests))
    (message "Wordwise disabled")))

;;;###autoload
(defun wordwise-set-difficulty (level)
  "Set Wordwise difficulty to LEVEL (1-5) and re-annotate."
  (interactive
   (list (read-number "Wordwise difficulty (1-5): "
                      wordwise-difficulty)))
  (unless (<= 1 level 5)
    (user-error "Difficulty must be between 1 and 5"))
  (setq wordwise-difficulty level)
  (when (bound-and-true-p wordwise-mode)
    (wordwise--cancel-scan-timer)
    (wordwise--remove-overlays)
    (wordwise--scan-visible))
  (message "Wordwise difficulty set to %d" level))

;;;###autoload
(defun wordwise-ignore-word ()
  "Add the word at point to the ignore list and remove its annotation.
The word is saved to `wordwise-ignored-words' via
Customize so it persists across sessions."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (raw (when bounds (buffer-substring-no-properties
                            (car bounds) (cdr bounds))))
         (word (when raw (downcase raw))))
    (unless word (user-error "No word at point"))
    (if (member word wordwise-ignored-words)
        (message "Wordwise: '%s' is already ignored" word)
      (customize-save-variable
       'wordwise-ignored-words
       (cons word wordwise-ignored-words))
      ;; Clear all overlays and re-scan so the ignored word disappears
      (when (bound-and-true-p wordwise-mode)
        (wordwise--remove-overlays)
        (wordwise--scan-visible))
      (message "Wordwise: '%s' will no longer be annotated" word))))

;; `define-minor-mode' keeps the mode variable when the library is
;; re-loaded.  Resume scans in buffers where Wordwise was already active
;; after the transient state reset above.
(dolist (buf (wordwise--active-buffers))
  (with-current-buffer buf
    (wordwise--add-hooks)
    (wordwise--schedule-scan buf)))

(provide 'wordwise)
;;; wordwise.el ends here
