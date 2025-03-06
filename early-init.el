;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      native-compile-prune-cache t
      native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors 'silent
      ;; In noninteractive sessions, prioritize .el file. It saves IO time
      load-prefer-newer noninteractive
      ;; Increase how much is read from processes in a single chunk (default is 4kb).
      ;; This is further increased elsewhere, where needed (like our LSP module).
      read-process-output-max (* 64 1024) ; 64kb
      ;; Font compacting can be terribly expensive
      inhibit-compacting-font-caches t
      ;; Case-insensitive pass over `auto-mode-alist' is time wasted.
      auto-mode-case-fold nil
      ;; Disabling BPA makes redisplay faster
      bidi-inhibit-bpa t
      ;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
      ;; in non-focused windows.
      highlight-nonselected-windows nil
      ;; Inhibits fontification while
      ;; receiving input, which should help a little with scrolling performance.
      redisplay-skip-fontification-on-input t
      jit-lock-stealth-time 16
      jit-lock-contextually t
      jit-lock-stealth-load 20
      jit-lock-stealth-nice 0.5
      jit-lock-chunk-size 4096
      ;; More performant rapid scrolling over unfontified regions.
      fast-but-imprecise-scrolling t
      ;; Update UI slowly
      idle-update-delay 1.0
      ;; smaller threshold to improve long line performance
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-default-init t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      server-client-instructions nil
      menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tab-bar-mode nil
      tooltip-mode nil
      blink-cursor-mode nil
      window-divider-mode nil
      ring-bell-function 'ignore
      use-dialog-box nil
      use-file-dialog nil  
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '((:eval (or buffer-file-truename "%b")) " • Emacs")
      x-underline-at-descent-line t
      default-input-method nil
      system-time-locale "C"
      use-package-always-demand (daemonp)
      use-package-always-defer (not (daemonp))
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(setq-default ;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
 ;; in non-focused windows.
 cursor-in-non-selected-windows nil
 ;; Disable bidirectional text scanning for a modest performance boost.
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right
 inhibit-redisplay t
 inhibit-message t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil)

(defun reset-gc-options ()
  (setq gc-cons-threshold 16777216 ;; 16MB
        gc-cons-percentage 0.2))
(add-hook 'after-init-hook #'reset-gc-options)
;; `file-name-handler-alist' is consulted on each call to `require', `load', or various file/io functions
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-value)))))))

;; Site files will use `load-file', which emit messages and triggers redisplay
;; Make it silent and undo advice later
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))
(define-advice startup--load-user-init-file (:after (&rest _) undo-silence)
  (advice-remove #'load-file #'load-file@silence))
;; there's a timeout that adds latency to frame operations,
;; like `make-frame-invisible', which Emacs frequently calls without a guard
;; because it's inexpensive in non-PGTK builds.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)  
(push '(tab-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
;; (push '(undecorated-round . t) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(advice-add #'tool-bar-setup :override #'ignore)
(put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
(setq-default mode-line-format nil)
(dolist (buf (buffer-list))
  (with-current-buffer buf (setq mode-line-format nil)))
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (unless (default-toplevel-value 'mode-line-format)
              (setq-default mode-line-format (get 'mode-line-format 'initial-value)))
            (redraw-frame)))
;; Display startup time
(defun dohna-display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'dohna-display-startup-time)

(set-language-environment "UTF-8")
(unless (memq system-type '(ms-dos windows-nt cygwin))
  (setq selection-coding-system 'utf-8))

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?│))
(define-fringe-bitmap 'right-curly-arrow
  [#b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000])
(define-fringe-bitmap 'right-arrow
  [#b00000000
   #b00000000
   #b00001110
   #b00001110
   #b00001110
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b01110000
   #b01110000
   #b01110000
   #b00000000
   #b00000000])

(defun +gen-envvars-file ()
  (interactive)
  (with-temp-buffer
    (cons (or (apply #'call-process (getenv "SHELL") nil t nil
                     (remq nil '("-lic" "emacs -q --no-site-file --batch --load $HOME/opt/emacs-scripts/gen-env.el")))
              -1)
          (string-trim (buffer-string)))))

(defun +load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No envvar file exists" file)))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let (env (read (current-buffer)))
        (let ((tz (getenv-internal "TZ")))
          (setq-default
           process-environment
           (append env (default-value 'process-environment))
           exec-path
           (append (split-string (getenv "PATH") path-separator t)
                   (list exec-directory))
           shell-file-name
           (or (getenv "SHELL")
               (default-value 'shell-file-name)))
          (when-let (newtz (getenv-internal "TZ"))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

(+load-envvars-file (expand-file-name "env" user-emacs-directory))

(defconst dohna-font-pixelsize 16)
(defconst dohna-fonts '((mono . "Donut")
                        (sans . "Donut")
                        (serif . "Donut")
                        (cjk . "Noto Sans CJK SC")
                        (emoji . "twemoji")
                        (symbol . "Symbola")))

(defun dohna--get-font-family (key)
  (let ((font (alist-get key dohna-fonts)))
    (if (string-empty-p font)
        (alist-get 'mono dohna-fonts)
      font)))

(defun dohna--format-font-name (family)
  (format "%s:pixelsize=%s:antialias=none" family dohna-font-pixelsize))

(defun dohna-load-default-font ()
  "Load default font configuration."
  (let ((default-font (dohna--format-font-name (dohna--get-font-family 'mono))))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun dohna-load-face-font ()
  "Load face font configuration."
  (let ((sans (dohna--get-font-family 'sans))
        (mono (dohna--get-font-family 'mono))
        (serif (dohna--get-font-family 'serif)))
    (set-face-attribute 'variable-pitch nil :family sans :height 130)
    (set-face-attribute 'variable-pitch-text nil :family serif :height 120)
    (set-face-attribute 'fixed-pitch nil :family mono :height 120)
    (set-face-attribute 'fixed-pitch-serif nil :family mono :height 120)
    (set-face-attribute 'font-lock-string-face nil :family serif)
    (set-face-attribute 'font-lock-doc-face nil :family serif :slant 'normal)
    (set-face-attribute 'font-lock-comment-face nil :family sans :slant 'normal)
    (set-face-attribute 'font-lock-comment-delimiter-face nil :family sans :slant 'normal)))

(defun dohna-load-charset-font (&optional font)
  "Load charset font configuration."
  (when window-system
    (let ((default-font (or font (dohna--format-font-name (dohna--get-font-family 'mono))))
          (cjk-font (dohna--get-font-family 'cjk))
          (emoji-font (dohna--get-font-family 'emoji))
          (symbol-font (dohna--get-font-family 'symbol)))
      (set-frame-font default-font)
      (dolist (charset '(kana han hangul cjk-misc bopomofo))
        (set-fontset-font t charset cjk-font))
      (set-fontset-font t 'emoji emoji-font)
      (set-fontset-font t 'symbol symbol-font))))

(setq use-default-font-for-symbols nil)
(dohna-load-default-font)
(dohna-load-face-font)
(add-hook 'after-init-hook #'dohna-load-charset-font)
(require-theme 'modus-themes)

(setq
 modus-themes-italic-constructs nil
 modus-themes-bold-constructs nil
 modus-themes-mixed-fonts nil
 modus-themes-completions '((matches . (regular))
                            (selection . (regular)))
 modus-themes-common-palette-overrides `((fg-region unspecified)
                                         (fg-completion-match-0 magenta-warmer)
                                         (fg-completion-match-1 cyan)
                                         (fg-completion-match-2 red)
                                         (fg-completion-match-3 blue)
                                         (bg-completion-match-0 bg-magenta-nuanced)
                                         (bg-completion-match-1 bg-cyan-nuanced)
                                         (bg-completion-match-2 bg-red-nuanced)
                                         (bg-completion-match-3 bg-blue-nuanced)
                                         ,@modus-themes-preset-overrides-faint)
 modus-operandi-tinted-palette-overrides '((bg-region bg-ochre)
                                           (cursor red-faint))
 modus-vivendi-tinted-palette-overrides '((keyword magenta-warmer)))


(defun dohna-tweak-theme-faces (_)
  (custom-set-faces
   '(region ((t :extend nil)))
   '(mode-line ((t :box nil)))
   '(mode-line-inactive ((t :box nil)))
   '(completions-annotations ((t :inherit variable-pitch)))
   '(marginalia-documentation ((t :inherit variable-pitch)))
   '(corfu-popupinfo ((t :inherit variable-pitch)))
   '(org-table ((t :inherit fixed-pitch-serif)))
   '(org-code ((t :inherit fixed-pitch-serif)))
   '(org-block ((t :inherit fixed-pitch-serif)))
   '(org-checkbox ((t :inherit fixed-pitch :background unspecified :box nil)))
   '(org-latex-and-related ((t :inherit fixed-pitch-serif)))
   '(vertico-current ((t :inherit nil :extend nil :underline (:position 1))))))

(add-hook 'enable-theme-functions #'dohna-tweak-theme-faces)
(load-theme 'modus-vivendi)

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

      This macro accepts, in order:

        1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
           unquoted list of modes, a quoted hook variable or a quoted list of hook
           variables.
        2. Optional properties :local, :append, and/or :depth [N], which will make the
           hook buffer-local or append to the list of hooks (respectively),
        3. The function(s) to be added: this can be a quoted function, a quoted list
           thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
           implicitly be wrapped in a lambda).

      \(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (if (listp hooks) hooks (list hooks)))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

      Takes the same arguments as `add-hook!'.

      If N and M = 1, there's no benefit to using this macro over `remove-hook'.

      \(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

      ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
      PLACE is the function to which to add the advice, like in `advice-add'.
      DOCSTRING and BODY are as in `defun'.

      \(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

      This has the same signature as `defadvice!' an exists as an easy undefiner when
      testing advice (when combined with `rotate-text').

      \(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(defmacro +advice-pp-to-prin1! (&rest body)
  "Define an advice called SYMBOL that map `pp' to `prin1' when called.
    PLACE is the function to which to add the advice, like in `advice-add'.

    \(fn SYMBOL &rest [PLACES...]\)"
  `(progn
     (dolist (target (list ,@body))
       (advice-add target :around #'+call-fn-with-pp-to-prin1))))

(defmacro defun-call! (symbol args &rest body)
  "Define a function and optionally apply it with specified arguments.

    \(fn SYMBOL ARGS &optional [DOCSTRING] &optional [:call-with APPLY_ARGS] BODY\)"
  (declare (indent defun))
  (let* ((docstring (if (stringp (car body)) (pop body)))
         (apply-body (if (eq :call-with (car body))
                         (progn
                           (cl-assert (eq (pop body) :call-with))
                           (pop body))
                       nil)))
    `(progn
       (defun ,symbol ,args
         ,@(if docstring
               (cons docstring body)
             body))
       (apply ',symbol
              ,(if (listp apply-body)
                   `(list ,@apply-body)
                 `(list ,apply-body))))))


(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro setq! (&rest settings)
  "A more sensible `setopt' for setting customizable variables.

  This can be used as a drop-in replacement for `setq' and *should* be used
  instead of `setopt'. Unlike `setq', this triggers custom setters on variables.
  Unlike `setopt', this won't needlessly pull in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                              ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

  If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
  This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defun +call-fn-with-pp-to-prin1 (fn &rest args)
  "Call FN with ARGS, map `pp' to `prin1' when called."
  (cl-letf (((symbol-function #'pp) #'prin1)
            ((symbol-function #'pp-to-string) #'prin1-to-string))
    (apply fn args)))

(defun +unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
    This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun +temp-buffer-p (buffer)
  "Return t if BUFFER is temporary."
  (string-match-p "^ " (buffer-name buffer)))

(defun +num-to-sup-string (num)
  "Convert NUM to a small string.

    Example: 12 -> \"¹²\""
  (let ((str (number-to-string num))
        (superscripts "⁰¹²³⁴⁵⁶⁷⁸⁹"))
    (mapconcat (lambda (c) (char-to-string (elt superscripts (- c ?0)))) str)))
