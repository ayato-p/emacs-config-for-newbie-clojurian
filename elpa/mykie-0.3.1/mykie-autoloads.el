;;; mykie-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "helm-mykie-keywords" "helm-mykie-keywords.el"
;;;;;;  (22870 1844 595532 730000))
;;; Generated autoloads from helm-mykie-keywords.el

(autoload 'helm-show-mykie-keywords "helm-mykie-keywords" "\
Show mykie.el keywords.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mykie" "mykie.el" (22870 1844 595532 730000))
;;; Generated autoloads from mykie.el

(autoload 'mykie:loop "mykie" "\


\(fn &rest ARGS)" nil t)

(autoload 'mykie:do-while "mykie" "\
Firstly do 1th function of ARGS and then do `mykie:loop' with ARGS.

\(fn &rest ARGS)" nil t)

(autoload 'mykie:attach-mykie-func-to "mykie" "\
Attach mykie's functions to the MODE's same key function without :default.
Use the MODE's function as :default function.
If you didn't specify the MODE, then use current major-mode by default.
The MODE is mode name's symbol such as 'emacs-lisp-mode.

\(fn &optional MODE-SYMBOL)" t nil)

(autoload 'mykie:define-key "mykie" "\
In KEYMAP, define key sequence KEY as `mykie' command with ARGS.
In other words, `mykie' + `define-key'.

Example:
 (mykie:define-key global-map \"y\"
   :default self-insert-command
   :region (message \"%s\" mykie:region-str)
   :C-u (message \"C-u y\"))

\(fn KEYMAP KEY &rest ARGS)" nil t)

(autoload 'mykie:global-set-key "mykie" "\
Give KEY a global binding as `mykie' command.
In other words, `mykie' + `global-set-key'.

Example:
 (mykie:global-set-key \"z\"
   :default self-insert-command
   :region (message \"%s\" mykie:region-str)
   :C-u (message \"C-u z\"))

\(fn KEY &rest ARGS)" nil t)

(autoload 'mykie:define-key-with-self-key "mykie" "\
Set self-insert-key(KEY) with `mykie' command.
This function register :default `self-insert-command' automatically to ARGS.
Example:
  (mykie:define-key-with-self-key
      \"a\" :C-u (message \"I am C-u\"))

\(fn KEY &rest ARGS)" nil t)

(autoload 'mykie:set-keys "mykie" "\
Set keybinds as `mykie' command.
Examples:
  Set keybinds to global-map:
  (mykie:set-keys nil ; You can set 'global or global-map instead of nil too.
    \"C-a\"
    :default     (beginning-of-line)
    :C-u         mark-whole-buffer
    \"C-e\"
    :default     (end-of-line)
    :C-u         (message \"Hello\"))

  Set keybinds to specific keymap:
  (mykie:set-keys emacs-lisp-mode-map
    \"C-1\"
    :default (message \"C-1\")
    :C-u     (message \"C-1+C-u\")
    \"C-2\"
    :default (message \"C-2\")
    :C-u     (message \"C-2+C-u\"))

  Set keybinds for self-insert-key
  You don't need to specify :default state, it's specified to
  'self-insert-command automatically to it.
  (mykie:set-keys 'with-self-key
   \"a\"
   :C-u (message \"called a\")
   :region query-replace-regexp
   \"b\"
   :C-u (message \"called b\"))

\(fn KEYMAP-OR-ORDER &rest ARGS)" nil t)

(autoload 'mykie:parse-parenthesized-syntax "mykie" "\


\(fn ARGS)" nil nil)

(autoload 'mykie:define-prefix-key "mykie" "\
Make prefix key with MYKIE-KEYS(mykielized keybindings).

This function takes PARENT-MAP as parent of PREFIX-KEY.

The PARAMS can take several parameters of anonymous function:
  :keep   -- keep prefix keymap as long as this function return non-nil
  :exit   -- exit prefix keymap when the function return non-nil
  :before -- call this function before entering prefix keymap
  :after  -- call this function after exited prefix keymap

Note that those keyword PARAMS are optional; which means if you set
nil to the PARAMS, created function will be exited after you type any
created key.

\(fn PARENT-MAP PREFIX-KEY PARAMS &rest MYKIE-KEYS)" nil t)

(defadvice mykie (around mykie:parse-parenthesized-syntax activate) "\
Parse args to convert parenthesized-syntax if it was needed." (ad-set-args 0 (mykie:parse-parenthesized-syntax (ad-get-args 0))) ad-do-it)

;;;***

;;;### (autoloads nil nil ("mykie-pkg.el") (22870 1844 595532 730000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mykie-autoloads.el ends here
