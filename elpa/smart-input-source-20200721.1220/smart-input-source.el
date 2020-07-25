;;; smart-input-source.el --- Switch OS native input source smartly -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-smart-input-source
;; Package-Version: 20200721.1220
;; Package-Commit: 6b84e3299caa0fde198e7e99513fdec8e060c551
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((names "0.5") (emacs "25") (terminal-focus-reporting "0.0"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provide modes to switch OS native input source smartly.
;; For more information see the README in the GitHub repo.

;;; Code:

;; `define-namespace' is autoloaded, so there's no need to require `names'.
;; However, requiring it here means it will also work for people who don't
;; install through package.el.
(eval-when-compile (require 'names))
(require 'subr-x)

;;;###autoload
(define-namespace smart-input-source-


(defvar external-ism "macism"
  "Path of external ism.")

(defvar do-get nil
  "Function to get the current input source.

Should return a string which is the id of the input source.")

(defvar do-set nil
  "Function to set the input source.

Should accept a string which is the id of the input source.")

(defvar english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")

(defvar english "com.apple.keylayout.US"
  "Input source for english.")

(defvar other-pattern "\\cc"
  "Pattern to identify a character as other lang.")

(defvar other "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other lang.")

(defvar blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")

(defvar auto-refresh-seconds 0.2
  "Idle timer interval to auto refresh input source status from OS.

Emacs-nativ input method don't need it. nil to disable the timer.")

(defvar change-hook nil
  "Hook to run when input source changes.")

(defvar default-cursor-color nil
  "Default cursor color, used for English.

nil means obtained from the envrionment.")

(defvar other-cursor-color "green"
  "Cursor color for other language.")

(defvar respect-start 'english
  "Switch to specific input source when `global-respect-mode' enabled.")

(defvar respect-evil-normal-escape t
  "<escape> to english in normal state when `global-respect-mode' enabled.")

(defvar respect-prefix-and-buffer t
  "Preserve buffer input source when `global-respect-mode' enabled.")

(defvar preserve-go-english-triggers nil
  "Triggers to save input source to buffer and then go to english.")

(defvar preserve-restore-triggers nil
  "Triggers to restore the input source from buffer.")

(defvar prefix-override-keys
  '("C-c" "C-x" "C-h")
  "Prefix keys to be overrided.")

(defvar prefix-override-recap-triggers
  '(evil-local-mode yas-minor-mode eaf-mode)
  "Commands trigger the recap of the prefix override.

Some functions take precedence of the override, need to recap after.")

(defvar follow-context-fixed nil
  "Context is fixed to a specific language in `follow-context-mode'.

Possible values:
nil: dynamic context
'english: English context
'other: other language context.")

(defvar follow-context-aggressive-line t
  "Aggressively detect context across blank lines.")

(defvar follow-context-hooks
  '(evil-insert-state-entry-hook)
  "Hooks trigger the set of input source following context.")

(defvar inline-english-activated-hook nil
  "Hook to run after inline english region activated.")

(defvar inline-english-deactivated-hook nil
  "Hook to run after inline english region deactivated.")

(defvar inline-other-activated-hook nil
  "Hook to run after inline other language region activated.")

(defvar inline-other-deactivated-hook nil
  "Hook to run after inline other language region deactivated.")

(defface inline-face
  '()
  "Face of the inline region overlay."
  :group 'smart-input-source)

(set-face-attribute
 'smart-input-source-inline-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video t)

(defvar inline-not-max-point t
  "Make sure there are other characters after inline region.

Insert new line when the whole buffer ends with the region, to avoid
autocomplete rendering a large area with the region background.")

(defvar inline-tighten-head-rule 1
  "Rule to delete head spaces.

Possible values:
1: delete 1 space if exists
0: don't delete space
'all: delete all space.")

(defvar inline-tighten-tail-rule 1
  "Rule to delete tail spaces.

Possible values:
1: delete 1 space if exists
0: don't delete space
'all: delete all space.")

(defvar inline-single-space-close nil
  "Single space closes the inline region.")

(defvar inline-with-english t
  "With the inline region.")

(defvar inline-with-other nil
  "With the inline other lang region.")

:autoload
(define-minor-mode log-mode
  "Log the execution of this package."
  :global t
  :init-value nil)

;;
;; Following symbols are not supposed to be used directly by end user.
;;

(declare-function evil-normal-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-visual-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-motion-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-operator-state-p
                  "ext:evil-states.el" (&optional state) t)
(declare-function company--active-p "ext:company.el" () t)
(declare-function mac-input-source "ext:macfns.c" (&optional SOURCE FORMAT) t)
(declare-function mac-select-input-source "ext:macfns.c"
                  (SOURCE &optional SET-KEYBOARD-LAYOUT-OVERRIDE-P) t)

;;
;; Following codes are mainly about input source manager
;;

(defvar -ism nil "The input source manager.")
(defvar -ism-inited nil "Input source manager initialized.")

(defvar -current nil
  "Current input source.")

(defvar -previous nil
  "Previous input source.")

(defvar -for-buffer nil
  "Saved buffer input source.")
(make-variable-buffer-local 'smart-input-source--for-buffer)

(defvar -for-buffer-locked nil
  "Buffer input source is locked.")
(make-variable-buffer-local 'smart-input-source--for-buffer-locked)

(defun -init-ism ()
  "Init input source manager."
  ;; `do-get'and `do-set' takes the first precedence.
  (unless (and (functionp do-get)
               (functionp do-set))
    ;; EMP
    (when (and (string= (window-system) "mac")
               (fboundp 'mac-input-source))
      ;; EMP
      (setq -ism 'emp))

    ;; external ism
    (when (and (not -ism) (stringp external-ism))
      (let ((ism-path (executable-find external-ism)))
        (when ism-path (setq -ism ism-path))))

    ;; make `do-set' and `do-get'
    (when -ism
      ;; avoid override user customized do-get
      (unless (functionp do-get)
        (setq do-get (-mk-get-fn)))
      ;; avoid override user customized do-set
      (unless (functionp do-set)
        (setq do-set (-mk-set-fn)))))

  ;; successfully inited
  (when (and (functionp do-get)
             (functionp do-set))
    ;; a t `-ism' means customized by `do-get' and `do-set'
    (unless -ism (setq -ism t)))

  ;; just inited, successfully or not
  (setq -ism-inited t))

(defmacro -ensure-ism (&rest body)
  "Only run BODY with valid ism."
  `(progn
     (unless smart-input-source--ism-inited
       (smart-input-source--init-ism))
     (when smart-input-source--ism
       ,@body)))

(defsubst -normalize-to-lang (lang)
  "Normalize LANG in the form of source id or lang to lang."
  (cond
   (; english
    (member lang (list 'english english))
    'english)
   (; other
    (member lang (list 'other other))
    'other)))

(defsubst -normalize-to-source (source)
  "Normalize SOURCE in the form of source id or lang to source."
  (cond
   (; english
    (member source (list 'english english))
    english)
   (; other
    (member source (list 'other other))
    other)))

(defun -mk-get-fn-cmd (cmd)
  "Make a function to be bound to `do-get' from CMD."
  (lambda ()
    (condition-case err
        (string-trim (shell-command-to-string cmd))
      ((file-missing file-error)
       (when (equal (car (cdr err))
                    "Setting current directory")
         (message
          "Default directory for buffer <%s> is missing, now set to '~'"
          (current-buffer))
         (setq default-directory "~"))))))

(defun -mk-get-fn ()
  "Make a function to be bound to `do-get'."
  (cond
   (; EMP
    (equal -ism 'emp)
    #'mac-input-source)
   (; external ism
    (-mk-get-fn-cmd -ism))))

(defun -mk-set-fn ()
  "Make a function to be bound to `do-set'."
  (cond
   (; EMP
    (equal -ism 'emp)
    (lambda (source) (mac-select-input-source source)))
   (; external ism
    (lambda (source)
      (start-process "set-input-source" nil -ism source)))))

(defun -update-state (source)
  "Update input source state.

SOURCE should be 'english or 'other."

  (setq -previous -current)
  (setq -current source)
  (unless -for-buffer-locked
    (setq -for-buffer source))
  (when (not (eq -previous -current))
    (run-hooks 'smart-input-source-change-hook)))

(defsubst -get ()
  "Get the input source id."
  (-ensure-ism
   (-update-state (-normalize-to-lang (funcall do-get)))))

(defsubst -set (source)
  "Set the input source according to source SOURCE."
  (-ensure-ism
   (-update-state (-normalize-to-lang source))
   (funcall do-set (-normalize-to-source source))
   (when log-mode (message "Do set input source: [%s]" source))))

:autoload
(defun get ()
  "Get input source."
  (interactive)
  (-get)
  -current)

:autoload
(defun set-english ()
  "Set input source to `english'."
  (interactive)
  (-set 'english))

:autoload
(defun set-other ()
  "Set input source to `other'."
  (interactive)
  (-set 'other))

:autoload
(defun switch ()
  "Switch input source between english and other."
  (interactive)
  (-ensure-ism
   (cond
    (; current is english
     (eq -current 'english)
     (-set 'other))
    (; current is other
     (eq -current 'other)
     (-set 'english)))))

:autoload
(defun ism-lazyman-config (english-source other-source &optional ism-type)
  "Config ism for lazy man.

english-source: ENGLISH input source, nil means default,
                ignored by ISM-TYPE of 'fcitx, 'fcitx5, 'emacs. 
other-source: OTHER language input source, nil means default,
              ignored by ISM-TYPE of 'fcitx, 'fcitx5.
type: TYPE can be 'emacs, 'emp, 'macism, 'im-select, 'fcitx, 'fcitx5, 'ibus.
      nil TYPE fits both 'emp and 'macism."
  (interactive)
  (when english-source
    (setq english english-source))
  (when other-source
    (setq other other-source))
  (when ism-type
    (setq external-ism (pcase ism-type
                         ('emacs nil)
                         ('emp nil)
                         ('macism "macism")
                         ('im-select "im-select.exe")
                         ('fcitx "fcitx-remote")
                         ('fcitx5 "fcitx5-remote")
                         ('ibus "ibus"))))

  (cond
   (; emacs builtin input method, set do-get and do-set
    (eq ism-type 'emacs)
    (setq default-input-method other-source)
    (setq english nil)
    (setq do-get (lambda() current-input-method))
    (setq do-set (lambda(source)
                   (unless (equal source current-input-method)
                     (toggle-input-method)))))
   (; for builtin supoort, use the default do-get and do-set
    (memq ism-type (list nil 'emp 'macism 'im-select))
    t)
   (; fcitx and fcitx5, use the default do-get, set do-set
    (memq ism-type (list 'fcitx 'fcitx5))
    (setq english "1")
    (setq other "2")
    (setq do-set (lambda(source)
                   (pcase source
                     ("1" (start-process "set-input-source" nil -ism "-c"))
                     ("2" (start-process "set-input-source" nil -ism "-o"))))))
   (; ibus, set do-get and do-set
    (eq ism-type 'ibus)
    (setq do-get (-mk-get-fn-cmd (format "%s engine" -ism)))
    (setq do-set (lambda(source)
                   (start-process
                    "set-input-source" nil -ism "engine" source))))))

;;
;; Following codes are mainly about auto update mode
;;

(defvar -auto-refresh-timer nil
  "Timer for `-auto-refresh-timer-function'.")

(defvar -auto-refresh-manager-timer nil
  "Timer to manage `-auto-refresh-timer'.")

(defun -auto-refresh-timer-function ()
  "Auto refresh input source on idle timer."
  (when -auto-refresh-timer
    (cancel-timer -auto-refresh-timer))

  (-save-to-buffer)

  (when (and auto-refresh-seconds -auto-refresh-mode)
    (setq -auto-refresh-timer
          (run-with-idle-timer
           ;; every time the wait period increases by auto-refresh-seconds
           (time-add (current-idle-time)
                     (* auto-refresh-seconds -auto-refresh-timer-scale))
           nil
           #'-auto-refresh-timer-function))
    (setq -auto-refresh-timer-scale (* 1.05 -auto-refresh-timer-scale))))

(defvar -auto-refresh-timer-scale 1
  "Interval scale during this idle period.")

(defun -auto-refresh-timer-restart ()
  "Restart `-auto-refresh-timer'."
  (when (and auto-refresh-seconds -auto-refresh-mode)
    (setq -auto-refresh-timer-scale 1)
    (-auto-refresh-timer-function)))

(define-minor-mode -auto-refresh-mode
  "Automaticly refresh input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    -auto-refresh-mode
    (when auto-refresh-seconds
      (when -auto-refresh-manager-timer
        (cancel-timer -auto-refresh-manager-timer))
      (setq -auto-refresh-manager-timer
            (run-with-idle-timer auto-refresh-seconds t
                                 #'-auto-refresh-timer-restart))))
   (; turn off the mode
    (and (not -auto-refresh-mode)
         ;; /cusor color mode/ depends on this mode
         (not global-cursor-color-mode)
         ;; /respect mode/ depends on this mode
         (not global-respect-mode))
    (when -auto-refresh-manager-timer
      (cancel-timer -auto-refresh-manager-timer))
    (when -auto-refresh-timer (cancel-timer -auto-refresh-timer)))))

;;
;; Following codes are mainly about cursor color mode
;;

(defun -set-cursor-color-advice (fn color)
  "Advice for FN of `set-cursor-color' with COLOR.

The advice is needed, because other packages may set cursor color in their only
way."
  (pcase -current
    ('english
     (funcall fn default-cursor-color))
    ('other
     (funcall fn other-cursor-color))
    (unknown
     (funcall fn color))))

(defun -update-cursor-color()
  "Update cursor color according to input source."
  ;; for GUI
  (when (display-graphic-p)
    ;; actually which color passed to the function does not matter,
    ;; the advice will take care of it.
    (set-cursor-color default-cursor-color))

  ;; for TUI
  (unless (display-graphic-p)
    (pcase -current
      ('english
       (send-string-to-terminal (format "\e]12;%s\a" default-cursor-color)))
      ('other
       (send-string-to-terminal (format "\e]12;%s\a" other-cursor-color))))))

:autoload
(define-minor-mode global-cursor-color-mode
  "Automaticly change cursor color according to input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    global-cursor-color-mode
    ;; save original cursor color
    (unless default-cursor-color
      (setq default-cursor-color
            (or (when (display-graphic-p)
                  (or (cdr (assq 'cursor-color default-frame-alist))
                      (face-background 'cursor)))
                "white")))
    (advice-add 'set-cursor-color :around #'-set-cursor-color-advice)
    (add-hook 'smart-input-source-change-hook #'-update-cursor-color))
   (; turn off the mode
    (and (not global-cursor-color-mode)
         ;; /respect mode/ depends on /cursor color mode/
         (not global-respect-mode))
    (advice-remove 'set-cursor-color #'-set-cursor-color-advice)
    (remove-hook 'smart-input-source-change-hook #'-update-cursor-color))))

;;
;; Following codes are mainly about respect mode
;;

(defsubst -save-to-buffer (&optional lock-after)
  "Save buffer input source, optional LOCK-AFTER save."
  (-get)
  (when lock-after
    (setq -for-buffer-locked t)))

(defsubst -restore-from-buffer ()
  "Restore buffer input source."
  (setq -for-buffer-locked nil)
  (-set (or -for-buffer 'english)))

(defvar -prefix-override-map-enable nil
  "Enabe the override keymap.")

:autoload
(defun prefix-override-buffer-disable ()
  "Disable prefix override in current buffer."
  (interactive)
  (make-local-variable
   'smart-input-source--prefix-override-map-enable)
  (setq -prefix-override-map-enable nil))

:autoload
(defun prefix-override-buffer-enable ()
  "Disable prefix override in current buffer."
  (interactive)
  (when (local-variable-p 'smart-input-source--prefix-override-map-enable)
    (kill-local-variable 'smart-input-source--prefix-override-map-enable)))

(defvar -prefix-override-map-alist nil
  "Map alist for override.")

(defvar -prefix-handle-stage 'normal
  "Processing state of the prefix key.

Possible values: 'normal, 'prefix, 'sequence.")

(defvar -buffer-before-prefix nil
  "Current buffer before prefix.")

(defvar -buffer-before-minibuffer nil
  "Current buffer before enter minibuffer.")

(defvar -buffer-before-command nil
  "Current buffer before prefix.")

(defvar -real-this-command nil
  "Real this command. Some commands overwrite it.")

(defvar -prefix-override-order -1000
  "Order of the prefix override in `emulation-mode-map-alists'.")

(defun -prefix-override-recap-advice (&rest res)
  "Advice for `prefix-override-recap-triggers' with RES."
  (add-to-ordered-list
   'emulation-mode-map-alists
   'smart-input-source--prefix-override-map-alist
   -prefix-override-order)
  res)

(defun -prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  ;; Restore the prefix arg
  (setq prefix-arg arg)
  (prefix-command-preserve-state)
  ;; Push the key back on the event queue
  (setq unread-command-events
        (append (mapcar (lambda (e) `(t . ,e))
                        (listify-key-sequence (this-command-keys)))
                unread-command-events)))

(defun -preserve-focus-out-handler ()
  "Handler for `focus-out-hook'."

  ;; `mouse-drag-region' causes lots of noise.
  (unless (eq this-command 'mouse-drag-region)
    ;; can't use `-save-to-buffer' directly
    ;; because OS may has already changed input source
    ;; when other windows get focus.
    ;; so, don't get the current OS input source
    (setq -for-buffer-locked t)
    (set-english))

  (when log-mode
    (message "Handle save hook, save [%s] to [%s]."
             -for-buffer (current-buffer))))

(defun -preserve-focus-in-handler ()
  "Handler for `focus-in-hook'."
  (when log-mode
    (message "Handle restore hook, restore [%s] from [%s] ."
             -for-buffer (current-buffer)))
  (-restore-from-buffer))

(defun -preserve-pre-command-handler ()
  "Handler for `pre-command-hook' to preserve input source."
  (setq -buffer-before-command (current-buffer))
  (setq -real-this-command this-command)

  (when log-mode
    (message "pre@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             -prefix-handle-stage
             (this-command-keys)
             -real-this-command
             (current-buffer)
             -prefix-override-map-enable))

  (pcase -prefix-handle-stage
    (; current is normal stage
     'normal
     (cond
      (; enter english era
       (memq -real-this-command preserve-go-english-triggers)
       (-save-to-buffer t)
       (set-english))
      (; not prefix key
       (not (eq -real-this-command #'-prefix-override-handler))
       t)

      (; for prefix key
       (eq -real-this-command #'-prefix-override-handler)

       ;; go to pre@[prefix] directly
       (when log-mode
         (message
          "[%s] is a prefix key, short circuit to prefix phase."
          (this-command-keys)))
       (setq -prefix-handle-stage 'prefix)
       (-preserve-pre-command-handler))))
    (; current is prefix stage
     'prefix
     (setq -prefix-override-map-enable nil)
     (setq -buffer-before-prefix (current-buffer))
     (-save-to-buffer t)
     (set-english)
     (when log-mode
       (message "Input source: [%s] (saved) => [%s]." -for-buffer english)))
    (; current is sequence stage
     'sequence t)))

(defvar prefix-override-buffer-disable-predicates
  (list 'minibufferp
        (;; magit revision
         lambda (buffer)
         (-string-match-p "^magit-revision:" (buffer-name buffer)))
        (;; special buffer
         lambda (buffer)
         (and (-string-match-p "^\*" (buffer-name buffer))
              (not (-string-match-p "^\*New" (buffer-name buffer)))
              (not (-string-match-p "^\*Scratch" (buffer-name buffer)))))
        )
  "Predicates on buffers to disable prefix overriding.")

(defsubst -prefix-override-buffer-disable-p (buffer)
  "Final predicate on disabling prefix override in BUFFER."
  (let ((value nil))
    (dolist (p prefix-override-buffer-disable-predicates)
      (setq value (or value (funcall p buffer))))
    value))

(defsubst -to-normal-stage (restore)
  "Transite to normal stage and restore input source if RESTORE is t."
  (when restore 
    ;; minibuffer are handled separately.
    ;; some functions like `exit-minibuffer' won't trigger post-command-hook
    (unless (or (minibufferp)
                (minibufferp -buffer-before-command))
      (when log-mode
        (message "restore: [%s]@[%s]." -for-buffer (current-buffer)))
      (-restore-from-buffer))

    (when (and (not (local-variable-p
                     'smart-input-source--prefix-override-map-enable))
               (-prefix-override-buffer-disable-p (current-buffer)))
      (prefix-override-buffer-disable))

    (unless (local-variable-p 'smart-input-source--prefix-override-map-enable)
      (setq -prefix-override-map-enable t)))

  (setq -prefix-handle-stage 'normal))

(defun -preserve-post-command-handler ()
  "Handler for `post-command-hook' to preserve input source."
  ;; (setq this-command -real-this-command)
  (when log-mode
    (message "post@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             -prefix-handle-stage
             (this-command-keys)
             -real-this-command
             (current-buffer)
             -prefix-override-map-enable))

  (pcase -prefix-handle-stage
    (; current is prefix stage
     'prefix
     (setq -prefix-handle-stage 'sequence))
    (; current is sequence stage
     'sequence
     (cond
      (; still in progress
       (minibufferp)
       (setq -prefix-handle-stage 'sequence))
      (; key sequence is canceled
       (not -real-this-command)
       (when log-mode (message "Key sequence canceled."))
       (-to-normal-stage t))

      (; end key sequence
       t
       (when log-mode (message "Key sequence ended."))
       (-to-normal-stage t))))
    (; current is normal stage
     'normal
     (let ((restore (or (not (eq -buffer-before-command (current-buffer)))
                        (memq -real-this-command preserve-restore-triggers))))
       (-to-normal-stage restore)))))

(defun -minibuffer-setup-handler ()
  "Handler for `minibuffer-setup-hook'."
  (when log-mode 
    (message "enter minibuffer: [%s]@current [%s]@last [%s]@command"
             (current-buffer)
             -buffer-before-command
             this-command))
  (setq -buffer-before-minibuffer -buffer-before-command)
  (set-english))

(defun -minibuffer-exit-handler ()
  "Handler for `minibuffer-exit-hook'."
  (when log-mode 
    (message "exit minibuffer: [%s]@before [%s]@command"
             -buffer-before-minibuffer
             this-command))
  (with-current-buffer -buffer-before-minibuffer
    (unless (minibufferp)
      (-restore-from-buffer))))

:autoload
(define-minor-mode global-respect-mode
  "Respect buffer/mode by proper input source.

- Respect start: start this mode with specific input source.
- Respect ~evil~: switch to English when leaving ~evil~ ~insert~ mode.
- Respect prefix key: switch to English for ~C-c~/ ~C-x~/ ~C-h~.
- Respect buffer: recover buffer input source when it regain focus."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    global-respect-mode
    (-ensure-ism
     ;; /respect mode/ depends on /auto refresh mode/
     (-auto-refresh-mode t)
     ;; set english when mode enabled
     (when respect-start (-set respect-start))

     (when respect-prefix-and-buffer
       ;; set english when exit evil insert state
       (when (featurep 'evil)
         (add-hook 'evil-insert-state-exit-hook #'set-english)
         (when respect-evil-normal-escape
           (define-key evil-normal-state-map (kbd "<escape>") #'set-english)))

       ;; preserve buffer input source
       (add-hook 'pre-command-hook #'-preserve-pre-command-handler)
       (add-hook 'post-command-hook #'-preserve-post-command-handler)
       (add-hook 'minibuffer-setup-hook #'-minibuffer-setup-handler)
       (add-hook 'minibuffer-exit-hook #'-minibuffer-exit-handler)

       ;; enable terminal focus event
       (unless (display-graphic-p)
         (require 'terminal-focus-reporting)
         (terminal-focus-reporting-mode t))

       (add-hook 'focus-out-hook #'-preserve-focus-out-handler)
       (add-hook 'focus-in-hook #'-preserve-focus-in-handler)

       ;; set english when prefix key pressed
       (setq -prefix-override-map-alist
             `((smart-input-source--prefix-override-map-enable
                .
                ,(let ((keymap (make-sparse-keymap)))
                   (dolist (prefix prefix-override-keys)
                     (define-key keymap
                       (kbd prefix) #'-prefix-override-handler))
                   keymap))))

       (setq -prefix-override-map-enable t)
       (-prefix-override-recap-advice)
       (dolist (trigger prefix-override-recap-triggers)
         (advice-add trigger :after #'-prefix-override-recap-advice)))))
   (; turn off the mode
    (not global-respect-mode)
    ;; for evil
    (when (featurep 'evil)
      (remove-hook 'evil-insert-state-exit-hook #'set-english)
      (when respect-evil-normal-escape
        (define-key evil-normal-state-map (kbd "<escape>") nil)))

    (when respect-prefix-and-buffer
      ;; for preserving buffer input source
      (remove-hook 'focus-out-hook #'-preserve-focus-out-handler)
      (remove-hook 'focus-in-hook #'-preserve-focus-in-handler)

      ;; for prefix key
      (setq emulation-mode-map-alists
            (delq 'smart-input-source--prefix-override-map-alist
                  emulation-mode-map-alists))
      (setq -prefix-override-map-enable nil)))))

;;
;; Following codes are mainly about follow-context-mode
;;

(defsubst -string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

Works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p'."
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))

(defsubst -english-p (str)
  "Predicate on STR is English."
  (-string-match-p english-pattern str))

(defsubst -not-english-p (str)
  "Predicate on STR is not English."
  (not (-string-match-p english-pattern str)))

(defsubst -other-p (str)
  "Predicate on STR is other language."
  (-string-match-p other-pattern str))

(defsubst -not-other-p (str)
  "Predicate on STR is not other language."
  (not (-string-match-p other-pattern str)))

(cl-defstruct back-detect ; result of backward detect
  to ; point after first non-blank char in the same line
  char ; first non-blank char at the same line (just before position `to')
  cross-line-to ; point after first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines before the current position
  )

(defun -back-detect-chars ()
  "Detect char backward by two steps.

  First backward skip blank in the current line,
  then backward skip blank across lines."
  (save-excursion
    (skip-chars-backward blank-pattern)
    (let ((to (point))
          (char (char-before (point))))
      (skip-chars-backward (concat blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-before (point))))
        (make-back-detect :to to
                          :char (when char (string char))
                          :cross-line-to (point)
                          :cross-line-char (when cross-line-char
                                             (string cross-line-char)))))))

(cl-defstruct fore-detect ; result of forward detect
  to ; point before first non-blank char in the same line
  char ; first non-blank char at the same line (just after position `to')
  cross-line-to ; point before first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines after the current position
  )

(defun -fore-detect-chars ()
  "Detect char forward.

  Forward skip blank in the current line."
  (save-excursion
    (skip-chars-forward blank-pattern)
    (let ((to (point))
          (char (char-after (point))))
      (skip-chars-forward (concat blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-after (point))))
        (make-fore-detect :to to
                          :char (when char (string char))
                          :cross-line-to (point)
                          :cross-line-char (when cross-line-char
                                             (string cross-line-char)))))))

(defun -context-other-p (back-detect fore-detect &optional position)
  "Predicate for context of other language.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (back-detect-to back-detect))
         (back-char (back-detect-char back-detect))
         (cross-line-back-to (back-detect-cross-line-to back-detect))
         (cross-line-back-char (back-detect-cross-line-char back-detect))

         (fore-to (fore-detect-to fore-detect))
         (fore-char (fore-detect-char fore-detect))
         (cross-line-fore-to (fore-detect-cross-line-to fore-detect))
         (cross-line-fore-char (fore-detect-cross-line-char fore-detect)))
    (cond
     (; [other]^
      (and (= back-to (or position (point))) (-other-p back-char))
      t)
     (; ^[other]
      (and (= fore-to (or position (point))) (-other-p fore-char))
      t)
     (; [other lang][blank or not][^][blank or not][not english] 
      (and (-other-p back-char) (-not-english-p fore-char))
      t)
     (; [not english][blank or not][^][blank or not][other lang] 
      (and (-not-english-p back-char) (-other-p fore-char))
      t)
     (; [other lang: to the previous line][blank][^]
      (and (or follow-context-aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-other-p cross-line-back-char))
      t))))

(defun -context-english-p (back-detect fore-detect &optional position)
  "Predicate for context of English.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (back-detect-to back-detect))
         (back-char (back-detect-char back-detect))
         (cross-line-back-to (back-detect-cross-line-to back-detect))
         (cross-line-back-char (back-detect-cross-line-char back-detect))

         (fore-to (fore-detect-to fore-detect))
         (fore-char (fore-detect-char fore-detect)))
    (cond
     (; [english]^
      (and (= back-to (or position (point))) (-english-p back-char))
      t)
     (; ^[english]
      (and (= fore-to (or position (point))) (-english-p fore-char))
      t)
     (; [english][blank or not][^][blank or not][not other] 
      (and (-english-p back-char) (-not-other-p fore-char))
      t)
     (; [not other][blank or not][^][blank or not][english] 
      (and (-not-other-p back-char) (-english-p fore-char))
      t)
     (; [english: to the previous line][blank][^]
      (and (or follow-context-aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (-english-p cross-line-back-char))
      t))))

(defun -context-guess ()
  "Guest the lang context for the current point."
  (let* ((back-detect (-back-detect-chars))
         (fore-detect (-fore-detect-chars)))
    (cond
     (; context is fixed.
      follow-context-fixed
      follow-context-fixed)
     (; english context
      (-context-english-p back-detect fore-detect)
      'english)
     (; other lang context
      (-context-other-p back-detect fore-detect)
      'other))))

:autoload
(define-minor-mode follow-context-mode
  "Switch input source smartly according to context."
  :init-value nil
  (cond
   (; turn of the mode
    follow-context-mode
    (-ensure-ism
     (dolist (hook follow-context-hooks)
       (add-hook hook #'follow-context nil t))))
   (; turn off the mode
    (not follow-context-mode)
    (dolist (hook follow-context-hooks)
      (remove-hook hook #'follow-context nil)))))

:autoload
(define-globalized-minor-mode
  smart-input-source-global-follow-context-mode
  follow-context-mode
  follow-context-mode)

:autoload
(defun follow-context ()
  "Follow the context to switch input source."
  (let ((context (-context-guess)))
    (when context
      (-set context))))

;;
;; Following codes are mainly about the inline region overlay
;;

(defvar -inline-overlay nil
  "The active inline overlay.")
(make-variable-buffer-local 'smart-input-source--inline-overlay)

(defvar -inline-lang nil
  "Language of the active inline overlay.")
(make-variable-buffer-local 'smart-input-source--inline-lang)

(defvar -inline-first-space-point nil
  "First effective space point to check overlay activation.")
(make-variable-buffer-local 'smart-input-source--inline-first-space-point)

(defsubst -inline-overlay-start ()
  "Start position of the inline overlay."
  (when -inline-overlay
    (overlay-start -inline-overlay)))

(defsubst -inline-overlay-end ()
  "End position of the inline overlay."
  (when -inline-overlay
    (overlay-end -inline-overlay)))

:autoload
(define-minor-mode inline-mode
  "English overlay mode for mixed language editing."
  :init-value nil
  (cond
   (; turn on the mode
    inline-mode
    (-ensure-ism
     (add-hook 'post-self-insert-hook #'-inline-check-to-activate nil t)))
   (; turn off the mode
    (not inline-mode)
    (remove-hook 'post-self-insert-hook #'-inline-check-to-activate t))))

:autoload
(define-globalized-minor-mode
  smart-input-source-global-inline-mode
  inline-mode
  inline-mode)

(defsubst -inline-effect-space-inserted-p ()
  "A effective space is inserted"
  (and inline-mode
       (not (overlayp -inline-overlay))
       (not (button-at (point)))
       (not (and (featurep 'evil)
                 (or (evil-normal-state-p)
                     (evil-visual-state-p)
                     (evil-motion-state-p)
                     (evil-operator-state-p))))
       ;; around char is <spc> <DBC spc>
       (memq (preceding-char) (list ?\s 12288))))

(defun -inline-check-to-activate()
  "Check whether to activate the inline region overlay.

Check the context to determine whether the overlay should be activated or not,
if the answer is yes, then activate the /inline region/, set the
input source to English."
  (let ((effective-space (-inline-effect-space-inserted-p)))
    (cond
     (;if not effective space inserted, reset times to 0
      (not effective-space)
      (setq -inline-first-space-point nil))
     (;if effective space inserted
      effective-space
      (let* ((back-detect (-back-detect-chars))
             (fore-detect (-fore-detect-chars)))

        (unless -inline-first-space-point
          (setq -inline-first-space-point (point)))

        (cond
         (;inline english region
          (and inline-with-english
               (-context-other-p back-detect fore-detect (1- (point)))
               (equal -for-buffer 'other))
          (-inline-activate 'english (1- (point))))

         (;inline other lang region
          (and inline-with-other
               (= (1+ -inline-first-space-point) (point))
               (-context-english-p back-detect fore-detect (- (point) 2))
               (equal -for-buffer 'english))
          (-inline-activate 'other (- (point) 2)))))))))

(defun -inline-activate (lang start)
  "Activate the inline region overlay from START."
  (interactive)
  (-ensure-ism
   (setq -inline-lang lang)
   (when (overlayp -inline-overlay)
     (delete-overlay -inline-overlay))

   (setq -inline-overlay (make-overlay start (point) nil t t ))
   (overlay-put -inline-overlay 'face 'smart-input-source-inline-face)
   (overlay-put -inline-overlay 'keymap
                (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "RET")
                    #'-inline-ret-check-to-deactivate)
                  (define-key keymap (kbd "<return>")
                    #'-inline-ret-check-to-deactivate)
                  keymap))
   (add-hook 'post-command-hook #'-inline-fly-check-deactivate nil t)
   (-set -inline-lang))

  (pcase -inline-lang
    ('other (run-hooks 'smart-input-source-inline-other-activated-hook))
    ('english (run-hooks 'smart-input-source-inline-english-activated-hook))))

(defun -inline-fly-check-deactivate ()
  "Check whether to deactivate the inline region overlay."
  (interactive)
  (when (and inline-mode
             (overlayp -inline-overlay))

    (when inline-not-max-point
      ;; When cursor is at point-max,
      ;; autocomplete may display with a huge inline overlay background.
      (when (= (point) (point-max))
        (save-excursion (insert-char ?\n))))

    ;; In case some package automatically insert \n before EOF,
    ;; then kick \n out of the the overlay
    (when (and (= (char-before (-inline-overlay-end))
                  ?\n)
               (< (-inline-overlay-start)
                  (-inline-overlay-end)))
      (move-overlay -inline-overlay
                    (-inline-overlay-start)
                    (1- (-inline-overlay-end))))

    ;; select input source
    (let* ((back-detect (-back-detect-chars))
           (back-to (back-detect-to back-detect)))
      (when (or
             ;; zero length overlay
             (= (-inline-overlay-start)
                (-inline-overlay-end))
             ;; out of range
             (or(< (point) (-inline-overlay-start))
                (> (point) (-inline-overlay-end)))
             ;; " inline  ^"
             ;; but not "           ^"
             (and (= (point) (-inline-overlay-end))
                  (> back-to (-inline-overlay-start))
                  (= (+ (if inline-single-space-close 1 2)
                        back-to) (point))))
        (-inline-deactivate)))))

(defun -inline-ret-check-to-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  (when (and inline-mode (overlayp -inline-overlay))
    ;; company
    (if (and (featurep 'company)
             (company--active-p))
        (company-complete-selection)
      (-inline-deactivate))))

(defun -inline-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  ;; clean up
  (remove-hook 'post-command-hook #'-inline-fly-check-deactivate t)

  ;; select input source
  (let* ((back-detect (-back-detect-chars))
         (back-to (back-detect-to back-detect))
         (back-char (back-detect-char back-detect)))


    (cond
     (; inline english region
      (eq -inline-lang 'english)
      ;; [other lang][blank inline overlay]^
      ;; [overlay with trailing blank]^
      (when (or (and (= back-to (-inline-overlay-start))
                     (-other-p back-char))
                (and (> back-to (-inline-overlay-start))
                     (< back-to (-inline-overlay-end))
                     (< back-to (point))))
        (set-other)))

     (; inline english region
      (eq -inline-lang 'other)
      ;; [not-other][blank inline overlay]^
      ;; [overlay with trailing blank]^
      (when (or (and (= back-to (-inline-overlay-start))
                     (-not-other-p back-char))
                (and (> back-to (-inline-overlay-start))
                     (< back-to (-inline-overlay-end))
                     (< back-to (point))))
        (set-english))))

    ;; only tighten for none-blank inline region
    (when (and (<= (point) (-inline-overlay-end))
               (> back-to (-inline-overlay-start)))

      (save-excursion
        (goto-char (-inline-overlay-end))
        (let* ((tighten-back-detect (-back-detect-chars))
               (tighten-back-to (back-detect-to tighten-back-detect)))
          (when (and (< tighten-back-to (-inline-overlay-end))
                     (> tighten-back-to (-inline-overlay-start)))
            (cond
             (; just delete one space
              (eq inline-tighten-tail-rule 1)
              (delete-char -1))
             (; don't delete space
              (eq inline-tighten-tail-rule 0)
              t)
             (; don't delete space
              (eq inline-tighten-tail-rule 'all)
              (delete-region (point) tighten-back-to))))))

      (save-excursion
        (goto-char (-inline-overlay-start))
        (let* ((tighten-fore-detect (-fore-detect-chars))
               (tighten-fore-to (fore-detect-to tighten-fore-detect)))
          (when (> tighten-fore-to (-inline-overlay-start))
            (cond
             (; just delete one space
              (eq inline-tighten-head-rule 1)
              (delete-char 1))
             (; don't delete space
              (eq inline-tighten-head-rule 0)
              t)
             (; don't delete space
              (eq inline-tighten-head-rule 'all)
              (delete-region (point) tighten-fore-to))))))))
  (delete-overlay -inline-overlay)
  (setq -inline-overlay nil)
  (pcase -inline-lang
    ('other (run-hooks 'smart-input-source-inline-other-deactivated-hook))
    ('english (run-hooks 'smart-input-source-inline-english-deactivated-hook))))


;; end of namespace
)

(provide 'smart-input-source)
;;; smart-input-source.el ends here
