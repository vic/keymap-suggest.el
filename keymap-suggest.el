;;; keymap-suggest.el --- Let emacs suggest you which keys to press

;; Copyright (C) 2013 Victor Borja

;; Author: Victor Borja <vic.borja@gmail.com>
;; Version: 0.1
;; URL: https://github.com/vic/keymap-suggest.el
;; Keywords: convenience keymap bindings

;; Keymap Suggest is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Control mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Keymap Suggest.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Let emacs suggest you which keys you should press
;; dependening on your current command input.
;; No need to memorize emacs keymaps anymore.
;; For example if you just type C-c emacs will suggest which keys
;; you can use in that keymap.

;;; Code:

(defvar keymap-suggest:interval 1
  "Suggest you a key if emacs is idle this number of seconds.

   Keymap Suggest only displays suggestions if emacs is currently
   waiting for you to complete a key sequence and emacs has been
   idle for this number (or fraction) of seconds waiting for you to
   type something.

   Note that if you change this value, you need to disable and
   re-enable keymap-suggest.")

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

;;;###autoload
(define-minor-mode keymap-suggest-mode
  "Let Emacs suggest you which keys to press.

Just type an incomplete command like `C-c` or `C-x` or any other
incomplete key binding and let emacs give you hints on what to press
next."
  nil nil nil
  (if keymap-suggest-mode (keymap-suggest:enable) (keymap-suggest:disable)))

(provide 'keymap-suggest)
