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

(defgroup mud nil
  "The Mud Client"
  :prefix "mud-"
  :group 'games)

(defcustom mud-mode-hook nil
  "Hook being run after `mud-mode' has completely set up the buffer."
  :type 'hook
  :options '(mud-add-scroll-to-bottom)
  :group 'mud)

(defvar mud-preoutput-filter-functions
  '(mud-process-telnet)
  "Functions run before line and block filtering, intended for removing protocol information from the stream.")

(defcustom mud-output-line-filters nil
  "Functions being run for each complete line of output from the
server. Each function is passed the line from the server as an
argument, and point is also in the displayed line from the server.

You probably often will want to set this buffer-local from
`mud-mode-hook', and only if you're in the right `mud-world'."
  :type 'hook
  :group 'mud)

(defun mud-test-block-filter (string)
  "A filter for printing the block out to the Messages buffer, so you know what is included."
  (message (concat "***" string "***"))
  string)

(defcustom mud-output-block-filters
  '(mud-truncate-buffer mud-handle-echo)
  "Functions being run on the entire block of input received from the server, with the block of text as the only argument."
  :type 'hook
  :group 'mud)

(defvar mud-world nil
  "The name of the current world.")

(defvar mud-buffer nil
  "The buffer for the current mud session.")

(defvar mud-process nil
  "The process for the current mud connection.")

(defcustom mud-world-list
  '(("Achaea" "achaea.com" 23)
    ("Aardwolf" "aardmud.org" 23)
    ("Aardwolf-Test" "aardmud.net" 6555))
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

(defvar mud-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-parent)
        (set-keymap-parent map comint-mode-map)        ; Emacs
      (set-keymap-parents map (list comint-mode-map))) ; XEmacs
    (when (functionp 'set-keymap-name)
      (set-keymap-name map 'mud-mode-map))    ; XEmacs
    (define-key map (kbd "TAB") 'dabbrev-expand)
    map)
  "The map for the Mud MUD client.")

(defvar mud-world-history nil
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

(defun mud (world)
  "Open a MUD connection to WORLD on HOST, port SERVICE.

To see how to add commands, see `mud-command-sender'."
  (interactive (list (mud-get-world)))
  (let ((buf (make-comint (mud-world-name world)
                          (mud-world-server world))))
    (with-current-buffer buf
      (setq mud-buffer buf)
      (setq mud-process (get-buffer-process buf))
      (mud-enable-options)
      (set-buffer-multibyte nil) ;necessary to prevent conversion for high-byte characters
      (switch-to-buffer (current-buffer))
      (add-hook 'pre-command-hook 'mud-move-to-prompt nil t)
      (mud-mode (mud-world-name world)))
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

(defvar mud-enabled-options nil
  "List of options currently enabled during connection")

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

(defun mud-enable (option)
  "Enable a single telnet option. OPTION should be a symbol from mud-known-options."
  (mud-send-code 'IAC 'DO option)
  (add-to-list 'mud-enabled-options option))

(defun mud-send-gmcp (key value)
  "Send KEY and VALUE as a GMCP message to the server"
  (mud-send-raw (concat (mud-codes 'IAC 'SB 'GMCP)
                        key " " (json-encode value)
                        (mud-codes 'IAC 'SE))))

(defun mud-enable-options nil
  "Enable desired options."
  (mapcar #'mud-enable mud-desired-options))

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
  (skip-chars-forward (apply #'unibyte-string (mud-all-codes))
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

(defvar mud-option-status
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
      (send-invisible "Password: ")))

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
  (message "gmcp block: %s" block))

(defun mud-process-telnet (string)
  "Process any telnet codes in STRING, and return the string without any telnet codes and related data."
  (with-temp-buffer
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
          (mud-delete-code))))
    (buffer-string)))

;[160/160hp 150/150mn 500/500mv 0qt 1000tnl] > 
;1540h, 1633m, 6600e, 7065w ex-

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
  (set (make-local-variable 'comint-input-sender)
       'mud-command-sender)
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
  (funcall comint-input-sender
           (get-buffer-process (current-buffer))
           str))

(defun mud-send-raw (&rest strings)
  "Send STRINGS to the current MUD server without sending a newline"
  (process-send-string mud-process (apply #'concat strings)))

(defun mud-send-input (str)
  "Send STR as input to the comint process."
  (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
         (old-input (buffer-substring pmark (point-max)))
         (idx (- (point) pmark)))
    (delete-region pmark (point-max))
    (goto-char pmark)
    (insert str)
    (comint-send-input)
    (goto-char pmark)
    (insert old-input)
    (goto-char (+ pmark idx))))

(defun mud-insert (str)
  "Insert STR as if it where output in the mud buffer."
  (save-excursion
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (goto-char pmark)
      (forward-line 0)
      (insert str)
      (if comint-last-prompt-overlay
          (move-overlay comint-last-prompt-overlay
                        (point)
                        (overlay-end comint-last-prompt-overlay))
        (set-marker pmark (point))))))

(defun mud-move-to-prompt ()
  "Move point to the prompt when typing. Copied from ERC"
  (when (and (< (point) (process-mark mud-process))
             (eq 'self-insert-command this-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun mud-command-sender (proc str)
  "This is the function used as `comint-input-sender'. It extracts
commands and aliases from the string, and handles them off to the
appropriate function.

Let FOO be the first word in STR. If mud-cmd-FOO exists, call it
with PROC and all words in STR as the arguments. If mud-cmd-FOO has
the property 'do-not-parse-args set, pass the arguments (including any
leading space) verbatim as a single argument. If the symbol is not
bound to a function, send STR unmodified to the server."
  (comint-simple-send proc str))

(defun mud-preoutput-filter (string)
  "Filter STRING before it gets added to the current buffer. Used for removing control characters and data from the visible output. This calls each function in `mud-preoutput-filter-functions' sequentially, using the final return value as the output."
  (if mud-preoutput-filter-functions
      (funcall (apply #'-compose mud-preoutput-filter-functions) string)
    string))

(defun mud-output-filter (string)
  "Filter STRING that was inserted into the current buffer. This runs
`mud-output-filter-functions', and should be in
`comint-output-filter-functions'."
  (when (string-match "\n" string)
    (save-excursion
      (goto-char comint-last-output-start)
      (forward-line 0)
      (run-hook-with-args 'mud-output-block-filters string)
      (while (< (point-at-eol)
                (process-mark (get-buffer-process (current-buffer))))
        (run-hook-with-args 'mud-output-line-filters
                            (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (forward-line 1)))))

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

This should be added to `comint-output-filter-functions'."
  (when (> (buffer-size) mud-truncate-buffer-size)
    (buffer-disable-undo)
    (delete-region (point-min)
                   (- (buffer-size) mud-truncate-buffer-size))
    (buffer-enable-undo)))

(defun mud-add-scroll-to-bottom ()
  "Add this to `mud-mode-hook' to recenter output at the bottom of
the window.

This works whenever scrolling happens, so it's added to
`window-scroll-functions'."
  (add-hook 'window-scroll-functions 'mud-scroll-to-bottom nil t))

(defun mud-scroll-to-bottom (window display-start)
  "Recenter WINDOW so that point is on the last line.

This is added to `window-scroll-functions' by
`mud-add-scroll-to-bottom'.

The code is shamelessly taken (but adapted) from ERC."
  (when (and window
             (window-live-p window)
             (comint-check-proc (current-buffer))
             (>= (point)
                 (process-mark (get-buffer-process (current-buffer)))))
    (let ((resize-mini-windows nil))
      (save-selected-window
        (select-window window)
        (save-restriction
          (widen)
          (when (>= (point)
                    (process-mark (get-buffer-process (current-buffer))))
            (save-excursion
              (recenter -1)
              (sit-for 0))))))))

(provide 'mud)
;;; mud.el ends here
