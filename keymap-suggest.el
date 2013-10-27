
(defvar keymap-suggest:interval 1
  "Suggest you a key if emacs is idle this number of seconds.

   Keymap Suggest only displays suggestions if emacs is currently
   waiting for you to complete a key sequence and emacs has been
   idle for this number (or fraction) of seconds waiting for you to
   type something.")

(defvar keymap-suggest:timer nil)

(defun keymap-suggest:keymap-bindings (keymap)
  (and (keymapp keymap)
       (cdr (if (symbolp keymap) (symbol-function keymap) keymap))))

(defun keymap-suggest:format-binding (binding)
  (cond ((keymapp binding)
         (keymap-suggest:format-keymap binding 'keymap-suggest:format-entry))
        ((keymap-suggest:format-entry binding))))

(defun keymap-suggest:format-key (key)
  (key-description (list (car key))))

(defun keymap-suggest:format-entry (entry)
  (cond ((char-table-p entry)
         (mapconcat 'keymap-suggest:format-key
          (let (m)
            (map-char-table (lambda (a b) (add-to-list 'm (cons a b))) entry)
            m) " "))
        ((consp entry) (keymap-suggest:format-key entry))))

(defun keymap-suggest:format-keymap (keymap &optional format-function)
  (mapconcat (or format-function 'keymap-suggest:format-binding)
             (keymap-suggest:keymap-bindings keymap)
             " "))

(defun keymap-suggest:message (prefix keymap)
  (message (concat
             (key-description prefix nil) "- "
             "possible completions:\n\n"
             (keymap-suggest:format-keymap keymap))))

(defun keymap-suggest:suggest (prefix keymap)
  (message (format "%s" keymap))
  (let (message-log-max)
    (message (keymap-suggest:message prefix keymap))))


(defun keymap-suggest:interval nil
  "Echo a key suggestion if emacs is waiting for a keymap sequence."
  (unless (or cursor-in-echo-area (minibufferp)
              (= 0 (length (this-command-keys-vector)))
              (= 13 (aref  (this-command-keys-vector) 0)))
    (let ((keymap-suggest:keymap (key-binding (this-command-keys-vector))))
      (and (keymapp keymap-suggest:keymap)
           (keymap-suggest:suggest (this-command-keys-vector)
                                   keymap-suggest:keymap)))))

(defun keymap-suggest:enable nil
  (setq keymap-suggest:timer
      (run-with-idle-timer keymap-suggest:interval t
                           'keymap-suggest:interval)))

(defun keymap-suggest:disable nil
  (cancel-timer keymap-suggest:timer))


(provide 'keymap-suggest)
