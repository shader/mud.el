;;; mud.el --- The Emacs MUD Client

;; Copyright (C) 2014 Samuel Christie
;; Based on mudel.el (C) 2004, 2005  Jorgen Schaefer
;; Some pieces borrwed from mu.el, by Alex Schroeder <alex@gnu.org>
;; and Aidan Gauland <aidalgol@amuri.net>

;; Author: Samuel Christie, Jorgen Schaefer
;; Keywords: application, games

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'comint)
(require 'json)
(require 'dash-functional)
(require 's)

(setq lexical-binding t)

(defgroup mud nil
  "The Mud Client"
  :prefix "mud-"
  :group 'games)

(defcustom mud-mode-hook nil
  "Hook being run after `mud-mode' has completely set up the buffer."
  :type 'hook
  :options '(mud-add-scroll-to-bottom)
  :group 'mud)

(defvar mud-input-filter-functions
  '(mud-process-aliases)
  "Functions run before sending input, intended for handling aliases and triggers, among other things.")

(defvar mud-preoutput-filter-functions
  '(mud-process-telnet)
  "Functions run before line and block filtering, intended for removing protocol information from the stream.")

(defcustom mud-output-line-filters
  '(mud-handle-reflexes)
  "Functions being run for each complete line of output from the
server. Each function is passed the line from the server as an
argument, and point is also in the displayed line from the server.

You probably often will want to set this buffer-local from
`mud-mode-hook', and only if you're in the right `mud-world'."
  :type 'hook
  :group 'mud)

(defcustom mud-output-block-filters
  '(mud-truncate-buffer mud-handle-echo)
  "Functions being run on the entire block of input received from the server, with the block of text as the only argument."
  :type 'hook
  :group 'mud)

(defvar-local mud-world nil
  "The name of the current world.")

(defvar-local mud-buffer nil
  "The buffer for the current mud session.")

(defvar-local mud-process nil
  "The process for the current mud connection.")

(defcustom mud-world-list
  '(("Aardwolf-Test" "aardmud.net" 6555))
  "List of worlds for easy connection.
  ((Name Host Port User Password)...)"
  :type 'list
  :group 'mud)

(defun mud-world-name (world)
  "The name of WORLD, which should be a string"
  (nth 0 world))

(defun mud-world-host (world)
  "The host url of WORLD"
  (nth 1 world))

(defun mud-world-port (world)
  "The host port of WORLD, if provided. Otherwise 23"
  (or (nth 2 world) 23))

(defun mud-world-server (world)
  "The server as a cons of host and port, as used by comint"
  (cons (mud-world-host world)
        (mud-world-port world)))

(defun mud-world-user (world)
  "The user name for WORLD, if provided"
  (nth 3 world))

(defun mud-world-password (world)
  "The password to use for WORLD, if provided"
  (nth 4 world))

(defun mud-action (action &rest args)
  "If action is a string, send it to the mud. Otherwise presume it is a function and send the results of calling it."
  (if (stringp action)
      (mud-send action)
    (let ((result (apply action args)))
      (if result (mud-send result)))))

(defvar mud-action-bindings
  `((,(kbd "<kp-1>") . "sw")
    (,(kbd "<kp-2>") . "s")
    (,(kbd "<kp-3>") . "se")
    (,(kbd "<kp-4>") . "w")
    (,(kbd "<kp-5>") . "look")
    (,(kbd "<kp-6>") . "e")
    (,(kbd "<kp-7>") . "nw")
    (,(kbd "<kp-8>") . "n")
    (,(kbd "<kp-9>") . "ne")
    (,(kbd "<kp-divide>") . "in")
    (,(kbd "<kp-multiply>") . "out")
    (,(kbd "<kp-subtract>") . "up")
    (,(kbd "<kp-add>") . "down")
    ))

(defun mud-key-action nil
  "This function is used for binding keys to mud commands"
  (interactive)
  (let* ((k (this-command-keys))
         (action (cdr (assoc k mud-action-bindings))))
    (mud-action action)))

(defun mud-bind-actions (map)
  "Create bindings for all pairs in `mud-action-bindings'"
  (mapc (lambda (key)
          (define-key map (car key) 'mud-key-action))
        mud-action-bindings))

(defvar mud-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
        (set-keymap-parent map comint-mode-map)        ; Emacs
      (set-keymap-parents map (list comint-mode-map))) ; XEmacs
    (when (functionp 'set-keymap-name)
      (set-keymap-name map 'mud-mode-map))    ; XEmacs
    (define-key map (kbd "TAB") 'hippie-expand)
    (define-key map (kbd "RET") 'mud-send-input)
    (mud-bind-actions map)
    map)
  "The keymap for the MUD client.")

(defvar-local mud-world-history nil
  "History for `mud-get-world'.")

(defun mud-get-world nil
  "Prompt the user for the world to connect to, offering the world list as completion options."
  (let ((world-completions
         (mapcar (lambda (w) (cons (mud-world-name w) w))
                 mud-world-list)))
    (if world-completions
        (cdr (assoc (completing-read "World: "
                                      world-completions
                                      nil t nil
                                      mud-world-history)
                    world-completions))
      (list (read-string "World name: ")
            (read-string "Host: ")
            (string-to-int (read-string "Port: " "23"))))))

(defvar-local mud-send-password-flag t
  "Whether to send the password automatically on a non-echo prompt. Defaults to true, but then is turned off after sending once.")

(defun mud-login (world)
  (let ((user (mud-world-user world)))
    (if user
        (run-at-time 0.1 nil (lambda nil (message "user: %s" user) (mud-send user))))))

(defun mud (world)
  "Open a MUD connection to WORLD on HOST, port SERVICE.

To see how to add commands, see `mud-command-sender'."
  (interactive (list (mud-get-world)))
  (let ((buf (make-comint (mud-world-name world)
                          (mud-world-server world))))
    (with-current-buffer buf
      (setq mud-buffer buf
            mud-process (get-buffer-process buf)
            mud-process-mark (process-mark mud-process)
            mud-input-mark (copy-marker mud-process-mark)
            mud-prompt-mark (copy-marker mud-input-mark)
            message-truncate-lines t ;don't resize minibuffer for large log messages
            )

      (set-buffer-multibyte nil) ;necessary to prevent conversion for high-byte characters

      (add-hook 'pre-command-hook 'mud-move-to-prompt nil t)
      (mud-mode (mud-world-name world))
      (switch-to-buffer (current-buffer))
      (mud-login world))
    buf))

(defvar mud-telnet-codes
  '((IAC . 255)
    (SE . 240)
    (NOP . 241)
    (DM . 242)
    (BRK . 243)
    (IP . 244)
    (AO . 245)
    (AYT . 246)
    (EC . 247)
    (EL . 248)
    (GA . 249)
    (SB . 250)
    (WILL . 251)
    (WONT . 252)
    (DO . 253)
    (DONT . 254)
    )
  "List of telnet command codes.")

(defvar mud-known-options
  '((ATCP . 200)
    (GMCP . 201)
    (MCCP1 . 85)
    (MCCP2 . 86)
    (EOR . 25)
    (TTYPE . 24)
    (ECHO . 1)
    (NAWS . 31)
    (AARD . 102)
    )
  "List of options known to mud.el")

(defun mud-all-codes nil
  "Return a list of all code values"
  (append (mapcar #'cdr mud-telnet-codes)
          (mapcar #'cdr mud-known-options)))

(defvar mud-supported-options
  '(GMCP ECHO)
  "List of options automatically enabled during connection, as requested by the server.")

(defun mud-code (sym)
  "Find the numeric telnet code given by SYM"
  (if (integerp sym)
      sym
    (cdr (or (assoc sym mud-telnet-codes)
             (assoc sym mud-known-options)))))

(defun mud-codes (&rest syms)
  "Returns a sequence of telnet codes as a unibyte-string."
  (apply #'unibyte-string (mapcar #'mud-code syms)))

(defun mud-lookup-code (num)
  "Find the symbol describing numeric code NUM"
  (or (car (rassoc num mud-telnet-codes))
      (car (rassoc num mud-known-options))))

(defun mud-send-code (&rest codes)
  "Send a sequence of telnet codes to the process.

CODES should be a sequence of symbols defined in mud-telnet-codes or mud-supported-options"
  (let ((str (apply #'mud-codes codes)))
    (mud-send-raw str)))

(defun mud-send-gmcp (key value)
  "Send KEY and VALUE as a GMCP message to the server"
  (mud-send-raw (concat (mud-codes 'IAC 'SB 'GMCP)
                        key " " (json-encode value)
                        (mud-codes 'IAC 'SE))))

(defun mud-next-code nil
  "Seek forward to the next telnet code sequence"
  (search-forward (mud-codes 'IAC) nil t))

(defun mud-backward-code nil
  "Seek back to the beginning of the current telnet code sequence"
  (search-backward (mud-codes 'IAC) nil t))

(defun mud-code-start-position nil
  (save-excursion
    (mud-backward-code)
    (point)))

(defun mud-code-end nil
  "Go to the first character after a code sequence"
  (skip-chars-forward (apply #'unibyte-string (delete 255 (mud-all-codes)))
                      (+ (point) 2))) ;somewhat hackish, need a better way to prevent two code segments from being merged

(defun mud-code-end-position nil
  "The position of the end of the current telnet code sequence."
  (save-excursion
    (mud-code-end)
    (point)))

(defun mud-code-block-end-position nil
  "Returns the position after the SE of a telnet code block."
  (save-excursion
    (search-forward (mud-codes 'SE))
    (point)))

(defun mud-delete-code nil
  "Delete a single telnet code sequence."
  (delete-region (mud-code-start-position) (mud-code-end-position)))

(defun mud-code-block-contents nil
  "Get the contents between telnet SB and SE as a string"
  (buffer-substring-no-properties
   (+ (mud-code-start-position) 3)
   (- (mud-code-block-end-position) 2)))

(defun mud-delete-code-block nil
  "Delete an entire telnet SB SE block"
  (delete-region (mud-code-start-position)
                 (mud-code-block-end-position)))

(defvar-local mud-option-status
  (make-hash-table))

(defun mud-handle-negotiation (code option)
  (cond ((member option mud-supported-options)
         (case code
           ('DO (if (not (gethash option mud-option-status))
                    (progn (mud-send-code 'IAC 'WILL option)
                           (puthash option t mud-option-status))))
           ('DONT (mud-send-code 'IAC 'WONT option)
                (remhash option mud-option-status))
           ('WILL (if (not (gethash option mud-option-status))
                      (progn (mud-send-code 'IAC 'DO option)
                             (puthash option t mud-option-status))))
           ('WONT (mud-send-code 'IAC 'DONT option)
                  (remhash option mud-option-status)))))
  (mud-delete-code))

(defun mud-handle-echo (string)
  (if (gethash 'ECHO mud-option-status)
      (let ((pass (mud-world-password (assoc mud-world mud-world-list))))
        (if (and mud-send-password-flag pass)
            (progn (mud-send pass) (set (make-local-variable 'mud-send-password) nil))
          (send-invisible "Password: ")))))

(defun mud-handle-code-block (code option)
  "Handle a block of data between SB and SE markers. The code sequence is IAC SB <option>.
If there is a handler defined for the option, run it on the contents between the option code and the next IAC. Otherwise, delete the entire block, including the codes."
  (let* ((handler (cdr (assoc option mud-code-block-handlers)))
         (block (mud-code-block-contents)))
    (if handler (funcall handler block))
    (mud-delete-code-block)))

(defvar mud-code-handlers
  '((DO . mud-handle-negotiation)
    (WILL . mud-handle-negotiation)
    (DONT . mud-handle-negotiation)
    (WONT . mud-handle-negotiation)
    (SB . mud-handle-code-block)))

(defvar mud-code-block-handlers
  '((GMCP . mud-handle-gmcp)))

(defun mud-handle-gmcp (block)
  "Handle a block of GMCP data"
  ;(message "gmcp block: %s" block)
  )

(defun mud-process-telnet (string)
  "Process any telnet codes in STRING, and return the string without any telnet codes and related data."
  (let ((proc mud-process)
        (status mud-option-status))
    (with-temp-buffer
      (let ((mud-process proc)
            (mud-option-status status)) ;mucky double-let for buffer-local variable preservation
        (set-buffer-multibyte nil)
        (insert string)
        (goto-char (point-min))
        (while (mud-next-code)
          (let* ((code (mud-lookup-code (char-after)))
                 (option (mud-lookup-code (char-after (+ (point) 1))))
                 (handler (cdr (assoc code mud-code-handlers))))
            (message "recieved %s %s" code option)
            (if handler
                (funcall handler code option)
              (mud-delete-code)))))
      (buffer-string))))

(defun mud-mode (world)
  "A mode for your MUD experience.

\\{mud-mode-map}"
  (unless (and (eq major-mode 'comint-mode)
               (comint-check-proc (current-buffer)))
    (error "This buffer is not a comint buffer!"))
  (setq major-mode 'mud-mode
        mode-name (format "Mud/%s" world)
        mud-world world)
  (use-local-map mud-mode-map)
  (face-remap-set-base 'comint-highlight-prompt nil)
  (set (make-local-variable 'comint-input-sender)
       'mud-input-sender)
  (let ((coding (coding-system-from-name 'no-conversion)))
    (set-process-coding-system mud-process coding coding))
  (add-hook 'comint-preoutput-filter-functions
            'mud-preoutput-filter
            t t)
  (add-hook 'comint-output-filter-functions
            'mud-output-filter
            t t)
  ;; User stuff.
  (run-hooks 'mud-mode-hook))
(put 'mu-connection-mode 'mode-class 'special)

(defun mud-send (str)
  "Send STR to the current MUD server."
  (mud-input-sender mud-process str))

(defun mud-send-raw (&rest strings)
  "Send STRINGS to the current MUD server without sending a newline"
  (process-send-string mud-process (apply #'concat strings)))

(defun mud-insert (str)
  "Insert STR as if it were output in the mud buffer."
  (save-excursion
    (goto-char mud-process-mark)
    (insert str)
    (set-marker mud-process-mark (point))
    (set-marker mud-prompt-mark (point))
    (set-marker mud-input-mark (+ mud-input-mark (length str)))))

(defun mud-move-to-prompt ()
  "Move point to the prompt when typing."
  (when (and (< (point) mud-input-mark)
             (eq 'self-insert-command this-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun mud-input-sender (proc str)
  "This is the function used as `comint-input-sender'.

It applies each function in mud-input-filter-functions to the input in turn, returning the final result to be sent to the mud."
  (let ((input (funcall (apply #'-compose mud-input-filter-functions) str))
        (comint-input-sender-no-newline nil))
    (comint-simple-send proc input)
    (save-excursion
      (let ((inhibit-read-only t))
        (forward-line 0)
        (put-text-property (point) mud-input-mark 'read-only nil)
        (delete-region (line-beginning-position) (point-max))))))

(defun mud-send-input nil
  "This is the function for binding to RET to call `comint-send-input'"
  (interactive)
  (buffer-disable-undo)
  (set-marker mud-process-mark mud-input-mark)
  (comint-send-input t)
  (buffer-enable-undo))

(defun mud-preoutput-filter (string)
  "Filter STRING before it gets added to the current buffer. Used for removing control characters and data from the visible output. This calls each function in `mud-preoutput-filter-functions' sequentially, using the final return value as the output."
  (when (> (length string) 0)
    (buffer-disable-undo)
    (set-marker mud-process-mark mud-prompt-mark)
    (if mud-preoutput-filter-functions
        (funcall (apply #'-compose mud-preoutput-filter-functions) string)
      string)))

(defun mud-output-filter (string)
  "Filter STRING that was inserted into the current buffer. This runs
`mud-output-filter-functions', and should be in
`comint-output-filter-functions'."
  (when (string-match "\n" string) ;required because comint somtimes calls with no output
    (save-excursion (run-hook-with-args 'mud-output-block-filters string))
    (save-excursion
      (message "start: %s, input: %s" comint-last-output-start mud-input-mark)
      (put-text-property comint-last-output-start (point-max) 'read-only t)
      (goto-char comint-last-output-start)
      (beginning-of-line)
      (while (< (point-at-eol)
                (process-mark (get-buffer-process (current-buffer))))
        (run-hook-with-args 'mud-output-line-filters
                            (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (forward-line 1))
      (goto-char (point-max))
      (set-marker mud-input-mark (point))
      (forward-line 0)
      (set-marker mud-prompt-mark (point)))
    (goto-char mud-input-mark)
    (buffer-enable-undo)))

(defun mud-output-fill (string)
  "Fill the region between `comint-last-output-start' and the
process-mark.

Don't forget to set `fill-column' when you use this."
  (fill-region (point-at-bol) (point-at-eol) nil t))

(defcustom mud-truncate-buffer-size 100000
  "The maximum size of the buffer. If it ever exceeds that,
`mud-truncate-buffer' will truncate old data."
  :type 'integer
  :group 'mud)

(defun mud-truncate-buffer (string)
  "Truncate the current buffer if it's size exceeds
`mud-truncate-buffer-size' bytes.

This should be added to `mud-output-block-filter-functions'."
  (when (> (buffer-size) mud-truncate-buffer-size)
    (buffer-disable-undo)
    (delete-region (point-min)
                   (- (buffer-size) (/ mud-truncate-buffer-size 2)))
    (buffer-enable-undo)))

(defvar mud-aliases nil
  "An alist of aliases. Each key should be the word to match, and each value should be the substitution")

(defun mud-process-aliases (input)
  "This function searches for each alias in the input, and applies the substitution."
  (let ((input 
         (mapcar (lambda (word)
                   (let ((match (assoc word mud-aliases)))
                     (if match
                         (cdr match)
                       word)))
                 (split-string input))))
    (mapconcat 'identity input " ")))

(defvar mud-reflexes nil
  "An alist of reflexes. The key should be the regexp to match against a line of output, and the value should be an action to perform in response. If the action is a function, it must accept the matching line as an argument. Any matched groups should be available through match-string etc.")

(defun mud-reflex (&rest reflexes)
  "Helper function for adding reflexes. Adds any number of reflexes, by taking alternating pairs of regexp and action. E.g. (mud-reflex rx action rx action...)"
  (mapc (lambda (reflex) (add-to-list 'mud-reflexes (apply 'cons reflex)))
        (-partition 2 reflexes)))

(defun mud-handle-reflexes (line)
  "This function tests each line of output against all user-defined reflexes, running the actions of any that matched with LINE as their only argument."
  (mapc (lambda (reflex)
          (if (string-match (car reflex) line)
              (mud-action (cdr reflex) line)))
        mud-reflexes))

(provide 'mud)
;;; mud.el ends here
