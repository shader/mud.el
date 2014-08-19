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

(defgroup mud nil
  "The Mud Client"
  :prefix "mud-"
  :group 'games)

(defcustom mud-mode-hook nil
  "Hook being run after `mud-mode' has completely set up the buffer."
  :type 'hook
  :options '(mud-add-scroll-to-bottom)
  :group 'mud)

(defcustom mud-output-filter-functions nil
  "Functions being run for each complete line of output from the
server. Each function is passed the line from the server as an
argument, and point is also in the displayed line from the server.

You probably often will want to set this buffer-local from
`mud-mode-hook', and only if you're in the right `mud-world'."
  :type 'hook
  :group 'mud)

(defvar mud-world nil
  "The name of the current world.")

(defvar mud-buffer nil
  "The buffer for the current mud session.")

(defvar mud-process nil
  "The process for the current mud connection.")

(defvar mud-world-list nil
  "List of worlds for easy connection.
  ((Name Host Port User Password)...)"
  )

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
      (switch-to-buffer (current-buffer))
      (mud-mode (mud-world-name world)))
    buf))

(defvar mud-telnet-codes
  '((IAC . 255)
    (SE . 240)
    (NOP . 241)
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
    (MCCP . 86)
    (EOR . 25)
    (TTYPE . 24)
    )
  "List of options known to mud.el")

(defvar mud-desired-options
  '(GMCP)
  "List of options automatically enabled during connection, if supported by the server")

(defvar mud-enabled-options nil
  "List of options currently enabled during connection")

(defun mud-code (sym)
  "Find the numeric telnet code given by SYM"
  (cdr (or (assoc sym mud-telnet-codes)
           (assoc sym mud-known-options))))

(defun mud-codes (&rest syms)
  "Returns a sequence of telnet codes as a unibyte-string."
  (apply #'unibyte-string (mapcar #'mud-code syms)))

(defun mud-lookup-code (num)
  "Find the symbol describing numeric code NUM"
  (car (rassoc num mud-telnet-codes)))

(defun mud-send-code (&rest codes)
  "Send a sequence of telnet codes to the process.

CODES should be a sequence of symbols defined in mud-telnet-codes or mud-supported-options"
  (let ((str (apply #'mud-codes codes)))
    (mud-send-raw str)))

(defun mud-enable (option)
  "Enable a single telnet option. OPTION should be a symbol from mud-known-options."
  (mud-send-code 'IAC 'DO option))

(defun mud-send-gmcp (key value)
  "Send KEY and VALUE as a GMCP message to the server"
  (mud-send-raw (concat (mud-codes 'IAC 'SB 'GMCP)
                        key " " (json-encode value)
                        (mud-codes 'IAC 'SE))))

(defun mud-enable-options nil
  "Enable desired options."
  (mapcar #'mud-enable mud-desired-options))

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
  (add-hook 'comint-output-filter-functions
            'mud-output-filter
            nil t)
  ;; User stuff.
  (run-hooks 'mud-mode-hook))
(put 'mu-connection-mode 'mode-class 'special)

(defun mud-send (str)
  "Send STR to the current MUD server."
  (funcall comint-input-sender
           (get-buffer-process (current-buffer))
           str))

(defun mud-send-raw (str)
  "Send STR to the current MUD server without sending a newline"
  (process-send-string mud-process str))

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

(defun mud-command-sender (proc str)
  "This is the function used as `comint-input-sender'. It extracts
commands and aliases from the string, and handles them off to the
appropriate function.

Let FOO be the first word in STR. If mud-cmd-FOO exists, call it
with PROC and all words in STR as the arguments. If mud-cmd-FOO has
the property 'do-not-parse-args set, pass the arguments (including any
leading space) verbatim as a single argument. If the symbol is not
bound to a function, send STR unmodified to the server."
  (if (zerop (length str))
      (comint-simple-send proc str)
    (mapc (lambda (line)
            (if (and mud-interpret-commands
                     (string-match "^ *\\(\\w+\\)" line))
                (let* ((name (match-string 1 line))
                       (args (substring line (match-end 0)))
                       (cmd (intern (format "mud-cmd-%s" (upcase name)))))
                  (if (fboundp cmd)
                      (if (get cmd 'do-not-parse-args)
                          (funcall cmd 
                                   (replace-regexp-in-string
                                    "^ *" ""
                                    args))
                        (apply cmd (split-string args))))))
            (comint-simple-send proc line))
          (split-string str "\n"))))

(defun mud-output-filter (string)
  "Filter STRING that was inserted into the current buffer. This runs
`mud-output-filter-functions', and should be in
`comint-output-filter-functions'."
  (when (string-match "\n" string)
    (save-excursion
      (goto-char comint-last-output-start)
      (forward-line 0)
      (while (< (point-at-eol)
                (process-mark (get-buffer-process (current-buffer))))
        (run-hook-with-args 'mud-output-filter-functions
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
