;;; -*- lexical-binding: t -*-
(require 'package)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))
(setq package-vc-register-as-project nil
      package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

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

(setq use-short-answers t
      y-or-n-p-use-read-key t
      read-char-choice-use-read-key t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t
      make-backup-files nil
      create-lockfiles nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      scroll-step 0
      scroll-margin 3
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-conservatively 101
      auto-window-vscroll nil
      auto-hscroll-mode 'current-line
      hscroll-step 0
      hscroll-margin 2
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t
      truncate-string-ellipsis "…"
      what-cursor-show-names t
      uniquify-buffer-name-style 'forward
      confirm-nonexistent-file-or-buffer nil
      ffap-machine-p-known 'reject
      shell-command-prompt-show-cwd t
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t
      vc-follow-symlinks t
      echo-keystrokes 0
      echo-keystrokes-help nil
      comment-empty-lines t
      help-window-select t
      find-library-include-other-files nil
      blink-matching-paren nil
      epa-keys-select-method 'minibuffer
      read-extended-command-predicate #'command-completion-default-include-p
      minibuffer-default-prompt-format " [%s]"
      minibuffer-follows-selected-frame nil
      enable-recursive-minibuffers t
      resize-mini-windows t
      minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)
      completion-show-help nil
      completions-header-format nil
      completions-detailed t
      completion-show-inline-help nil
      completions-max-height 6
      minibuffer-completion-auto-choose t
      minibuffer-visible-completions t
      backward-delete-char-untabify-method 'hungry
      require-final-newline t
      copy-region-blink-delay 0
      delete-pair-blink-delay 0
      duplicate-line-final-position -1
      duplicate-region-final-position -1
      auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
      auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                                 (concat auto-save-list-file-prefix "tramp-\\2") t)
                                           (list ".*" auto-save-list-file-prefix t))
      epg-pinentry-mode 'loopback)

(setq-default
 tabify-regexp "^\t* [ \t]+"
 tab-always-indent t
 indent-tabs-mode nil
 tab-width 2
 word-wrap t
 truncate-lines t
 truncate-partial-width-windows nil
 word-wrap-by-category t
 fill-column 80
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil)

(use-package completion-preview
  :diminish completion-preview-mode
  :hook ((prog-mode conf-mode text-mode) . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate)
              ("<control-bracketleft>" . +escape)
              ("<escape>" . +escape)))

(use-package diminish :ensure t)

(use-package subword
  :diminish subword-mode
  :hook ((prog-mode minibuffer-setup) . subword-mode))

(use-package minibuffer
  :hook (minibuffer-setup . cursor-intangible-mode)
  :bind (:map minibuffer-local-map
              ("C-n" . minibuffer-next-completion)
              ("C-p" . minibuffer-previous-completion)
              ("<control-bracketleft>" . +escape))
  :config
  (setq minibuffer-depth-indicate-mode t
        minibuffer-electric-default-mode t))

(use-package isearch
  :diminish isearch-mode
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (setq
   isearch-resume-in-command-history t
   isearch-lax-whitespace t
   isearch-repeat-on-direction-change t
   isearch-allow-motion t
   isearch-motion-changes-direction t
   isearch-lazy-count t
   lazy-highlight-cleanup nil
   lazy-highlight-buffer t
   lazy-count-prefix-format nil
   lazy-count-suffix-format " [%s/%s]"
   search-ring-max 200
   regexp-search-ring-max 200))

(defadvice! +disable-autosave-notification-a (fn &rest args)
	:around #'after-find-file
	(cl-letf (((symbol-function #'sit-for) #'ignore))
	  (apply fn args)))

(use-package autorevert
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq revert-without-query '(".")))

(use-package goto-addr
  :hook
  (text-mode . goto-address-mode)
  (prog-mode . goto-address-prog-mode))

(use-package comint
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048
        comint-pager "cat"
        comint-history-isearch 'dwim))

(use-package so-long
  :diminish so-long-mode
  :hook (after-init . global-so-long-mode)
  :config
  (if (fboundp 'buffer-line-statistics)
      (unless (featurep 'native-compile)
 	      (setq so-long-threshold 5000))
    (setq so-long-threshold 400)
    (defun +buffer-has-long-lines-p ()
      (unless (bound-and-true-p visual-line-mode)
 	      (let ((so-long-skip-leading-comments
               (bound-and-true-p comment-use-syntax)))
          (so-long-detected-long-line-p))))
    (setq so-long-predicate #'+buffer-has-long-lines-p))
  (delq! 'font-lock-mode so-long-minor-modes)
  (delq! 'display-line-numbers-mode so-long-minor-modes)
  (delq! 'buffer-read-only so-long-variable-overrides 'assq)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  (appendq! so-long-minor-modes
            '(spell-fu-mode
              eldoc-mode
              highlight-numbers-mode
              better-jumper-local-mode
              ws-butler-mode
              auto-composition-mode
              undo-tree-mode
              highlight-indent-guides-mode
              hl-fill-column-mode
              flycheck-mode
              smartparens-mode
              smartparens-strict-mode)))
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (+advice-pp-to-prin1! 'save-place-alist-to-file))
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 200
        recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
		                          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
		                          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
		                          (lambda (file) (file-in-directory-p file package-user-dir))
		                          (expand-file-name recentf-save-file))
        recentf-keep nil)

  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (add-hook! dired-mode-hook
    (defun +dired--add-to-recentf-h ()
      (recentf-add-file default-directory))))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring
				                                mark-ring
				                                global-mark-ring
				                                search-ring
				                                regexp-search-ring))

  (with-eval-after-load 'vertico
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  (add-hook! savehist-save-hook
    (defun +savehist--remove-string-properties-h ()
      (setq kill-ring (mapcar #'substring-no-properties
			                        (cl-remove-if-not #'stringp kill-ring))
	          search-ring (mapcar #'substring-no-properties search-ring)
	          regexp-search-ring (mapcar #'substring-no-properties regexp-search-ring)
	          register-alist (cl-loop for (reg . item) in register-alist
				                            if (stringp item)
				                            collect (cons reg (substring-no-properties item))
				                            else collect (cons reg item))))
    (defun +savehist--remove-unprintable-registers-h ()
      (setq-local register-alist
		              (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package ediff
  :hook ((ediff-before-setup . +ediff-save-window-config)
         ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
  :config
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))

  (defvar +ediff-saved-window-config nil)
  (defun +ediff-save-window-config ()
    (setq +ediff-saved-window-config (current-window-configuration)))
  (defun +ediff-restore-window-config ()
    (when (window-configuration-p +ediff-saved-window-config)
      (set-window-configuration +ediff-saved-window-config)))

  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally
        ediff-highlight-all-diffs t
        ediff-diff-options "-w"))

(use-package elec-pair
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode minibuffer-mode) . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit))

(use-package winner
  :commands (winner-undo winner-redo)
  :init
  (setq winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers
        '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
          "*Apropos*" "*Help*" "*helpful*" "*cvs*" "*Buffer List*" "*Ibuffer*"
          "*esh command on file*")))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ("C-c C-d" . helpful-at-point)
  :config
  (setq helpful-max-buffers 1))

(use-package popper
  :ensure t
  :bind (:map popper-mode-map
              ("C-M-<tab>"   . popper-cycle)
              ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq +popper-reference-buffers-select
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          ;; "\\*Completions\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          ibuffer-mode
          help-mode
          helpful-mode
          tabulated-list-mode
          Buffer-menu-mode
          flymake-diagnostics-buffer-mode

          grep-mode occur-mode rg-mode
          osx-dictionary-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*quickrun\\*$"
          "\\*vc-.*\\*$"
          "^\\*macro expansion\\**"
          reb-mode

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Graphviz Preview: .*\\*"

          "^GPTel-popup: .*"

          (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'compilation-mode)))))
  (setq +popper-reference-buffer-no-select
        '("\\*Warnings\\*"))
  (setq popper-reference-buffers (append +popper-reference-buffers-select
                                         +popper-reference-buffer-no-select))
  :config
  (popper-echo-mode 1)

  (defadvice! +popper-close-window-hack (&rest _)
    :before #'keyboard-quit
    (when (and (not (region-active-p))
               (not (meow-insert-mode-p))
               (not (meow-beacon-mode-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))

  (defvar +popper-unpacked-vars '(popper--reference-names
                                  popper--reference-modes
                                  popper--reference-predicates
                                  popper--suppressed-names
                                  popper--suppressed-modes
                                  popper--suppressed-predicates))
  (defvar +popper-unpacked-vars-no-select '())

  (dolist (var +popper-unpacked-vars)
    (let ((var-name (intern (concat "+" (symbol-name var) "-no-select"))))
      (eval
       `(progn
          (defvar ,var-name nil)
          (push ',var-name +popper-unpacked-vars-no-select)))))
  (setq +popper-unpacked-vars-no-select (reverse +popper-unpacked-vars-no-select))

  (cl-progv `(popper-reference-buffers ,@+popper-unpacked-vars)
      (list +popper-reference-buffer-no-select)
    (popper--set-reference-vars)
    (cl-loop for var in +popper-unpacked-vars
             for var-no-select in +popper-unpacked-vars-no-select
             do (eval `(setq ,var-no-select ',(symbol-value var)))))

  (defun +popper-smart-popup-at-bottom (buffer &optional alist)
    (let ((window (popper-display-popup-at-bottom buffer alist)))
      (unless (cl-progv +popper-unpacked-vars
                  (mapcar #'symbol-value +popper-unpacked-vars-no-select)
                (popper-popup-p buffer))
        (select-window window))))
  (setq popper-display-function #'+popper-smart-popup-at-bottom))

(use-package ligature
  :ensure t
  :hook ((prog-mode markdown-mode) . ligature-mode)
  :config
  (ligature-set-ligatures '(prog-mode markdown-mode org-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://")))

(add-hook 'prog-mode-hook 'prettify-symbols-mode)

(setq-default header-line-format nil)

(use-package rg
  :ensure t)
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package dired
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq
   dired-recursive-deletes 'top
   dired-recursive-copies 'always
   dired-dwim-target t
   dired-create-destination-dirs 'ask
   dired-auto-revert-buffer #'dired-buffer-stale-p
   dired-hide-details-hide-symlink-targets nil)

  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (setq insert-directory-program "gls")
      (setq dired-use-ls-dired nil)))

  (when (or (not (eq system-type 'darwin)) (executable-find "gls"))
    (setq ls-lisp-use-insert-directory-program t
          dired-listing-switches "-alh --group-directories-first")))

(use-package dired-git-info
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("g" . dired-git-info-mode)
              ("r" . revert-buffer))
  :config
  (setq dgi-commit-message-format "%h %cs %s"
        dgi-auto-hide-details-p nil))

(use-package dired-aux
  :after dired
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))


(use-package dired-x
  :after dired
  :bind (:map dired-mode-map
              ("." . dired-omit-mode))
  :config
  (let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
                   ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
                   ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))

  (setq dired-omit-verbose nil
        ;; hide dot files
        dired-omit-files "^\\..*\\'")

  (setq dired-clean-confirm-killing-deleted-buffers nil))


(use-package fd-dired
  :ensure t
  :bind ([remap find-dired] . fd-dired))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package avy
  :ensure t
  :bind (("C-, ." . avy-goto-char)
         ("C-, ," . avy-goto-char-2)
         ("C-, l" . avy-goto-line))
  :hook (after-init . avy-setup-default)
  :config
  (setq avy-styles-alist '((avy-isearch . pre))))

(use-package ace-window
  :ensure t
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  :hook ((window-configuration-change . aw-update)) ;; For modeline
  :config
  (setq aw-scope 'global
        aw-background nil
        aw-ignore-current t)

  (defsubst +mode-line-window-name ()
    (char-to-string
     (+  (if (mode-line-window-selected-p)
             #x2775
           #x245f)
         (string-to-number
          (window-parameter (selected-window) 'ace-window-path)))))

  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  (:eval (+mode-line-window-name)) " "
                  (:propertize
                   ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                    mode-line-window-dedicated)
                   display (min-width (6.0)))
                  mode-line-frame-identification mode-line-buffer-identification "   "
                  mode-line-position (project-mode-line project-mode-line-format)
                  (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

  ;; Select widnow via `M-1'...`M-9'
  (defun +aw--select-window (number)
    "Select the specified window."
    (let* ((window-list (aw-window-list))
           (target-window nil))
      (cl-loop for win in window-list
               when (and (window-live-p win)
                         (eq number
                             (string-to-number
                              (window-parameter win 'ace-window-path))))
               do (setq target-window win)
               finally return target-window)

      ;; Select the target window if found
      (if target-window
          (aw-switch-to-window target-window)
        (message "No specified window: %d" number))))

  (defmacro +define-avy-switch-to-window (i)
    (let ((fn-name (intern (concat "+avy-switch-to-window-" (number-to-string i)))))
      `(progn
         (defun ,fn-name ()
           (interactive)
           (+aw--select-window ,i))
         (bind-key (concat "M-" (number-to-string ,i))
                   #',fn-name))))

  ;; Select window via `M-1'...`M-9'
  (cl-loop for i from 1 to 9
           do (eval `(+define-avy-switch-to-window ,i))))


(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package beginend
  :ensure t
  :hook (after-init . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
          (diminish mode)))

(use-package treesit
  :when (treesit-available-p)
  :init
  (setq major-mode-remap-alist
        '(;;(c-mode . c-ts-mode)
          ;;(c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode)
          (html-mode . html-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . javascript-ts-mode)
          (js2-mode . javascript-ts-mode)
          (javascript-mode . javascript-ts-mode)
          (tsx-mode . tsx-ts-mode)
          (typescript-tsx-mode . tsx-ts-mode)
          (typescript-mode . typescript-ts-mode)
          ;; (rust-mode . rust-ts-mode)
          ))

  (setq treesit-language-source-alist
        '(;; (c "https://github.com/tree-sitter/tree-sitter-c")
          ;; (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          ;; (rust "https://github.com/tree-sitter/tree-sitter-rust")
          ;; (typst "https://github.com/uben0/tree-sitter-typst")
          ))

  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))

  (setq treesit-font-lock-level 2))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook (after-init . ws-butler-global-mode)
  :config
  (pushnew! ws-butler-global-exempt-modes
	          'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode))

(use-package sudo-edit
  :ensure t
  :config
  (sudo-edit-indicator-mode t))

(use-package org
  :hook (org-mode . variable-pitch-mode)
  :config
  (setq org-directory (expand-file-name "~/Documents/org")
        org-special-ctrl-a/e nil
        org-special-ctrl-k nil
        org-M-RET-may-split-line '((default . nil))
        org-hide-emphasis-markers nil
        org-hide-macro-markers nil
        org-hide-leading-stars nil
        org-cycle-separator-lines 0
        org-fold-catch-invisible-edits 'show
        org-return-follows-link nil
        org-loop-over-headlines-in-active-region 'start-level
        org-use-sub-superscripts '{}
        org-insert-heading-respect-content t
        org-read-date-prefer-future 'time
        org-track-ordered-property-with-tag t
        org-priority-faces nil
        org-adapt-indentation nil
        org-indent-mode-turns-on-hiding-stars nil
        org-reverse-note-order nil
        org-use-fast-todo-selection 'expert
        org-fontify-done-headline nil
        org-fontify-todo-headline nil
        org-fontify-whole-heading-line nil
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-tag-alist nil
        org-auto-align-tags nil
        org-tags-column 0
        org-log-done 'time
        org-log-into-drawer t
        org-log-note-clock-out nil
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-todo-keywords '((sequence "TODO(t)" "|" "CANCEL(c@)" "DONE(d!)"))
        org-agenda-files (list org-directory)
        org-agenda-span 'week
        org-agenda-start-on-weekday 1
        org-agenda-confirm-kill t
        org-agenda-show-all-dates t
        org-agenda-show-outline-path nil
        org-agenda-window-setup 'current-window
        org-agenda-skip-comment-trees t
        org-agenda-menu-show-matcher t
        org-agenda-menu-two-columns nil
        org-agenda-sticky nil
        org-agenda-custom-commands-contexts nil
        org-agenda-max-entries nil
        org-agenda-max-todos nil
        org-agenda-max-tags nil
        org-agenda-max-effort nil
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c"))
        org-agenda-sorting-strategy '(((agenda habit-down time-up priority-down category-keep)
                                       (todo priority-down category-keep)
                                       (tags priority-down category-keep)
                                       (search category-keep)))
        org-agenda-breadcrumbs-separator "->"
        org-agenda-todo-keyword-format "%-1s"
        org-agenda-fontify-priorities 'cookies
        org-agenda-category-icon-alist nil
        org-agenda-remove-times-when-in-prefix nil
        org-agenda-remove-timeranges-from-blocks nil
        org-agenda-compact-blocks nil
        org-agenda-block-separator ?—
        org-agenda-bulk-mark-char "#"
        org-agenda-persistent-marks nil
        org-agenda-start-with-follow-mode nil
        org-agenda-follow-indirect t
        org-agenda-dim-blocked-tasks t
        org-agenda-todo-list-sublevels t
        org-agenda-persistent-filter nil
        org-agenda-restriction-lock-highlight-subtree t
        org-agenda-include-deadlines t
        org-deadline-warning-days 0
        org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-timestamp-if-deadline-is-shown t
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-deadline-prewarning-if-scheduled 1
        org-agenda-skip-scheduled-delay-if-deadline nil
        org-agenda-skip-additional-timestamps-same-entry nil
        org-agenda-skip-timestamp-if-done nil
        org-agenda-search-headline-for-time nil
        org-scheduled-past-days 365
        org-deadline-past-days 365
        org-agenda-move-date-from-past-immediately-to-today t
        org-agenda-show-future-repeats t
        org-agenda-prefer-last-repeat nil
        org-agenda-timerange-leaders '("" "(%d/%d): ")
        org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")
        org-agenda-inactive-leader "["
        org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: ")
        org-agenda-time-leading-zero t
        org-agenda-timegrid-use-ampm nil
        org-agenda-use-time-grid t
        org-agenda-show-current-time-in-grid t
        org-agenda-current-time-string (concat "Now " (make-string 70 ?.))
        org-agenda-time-grid '((daily today require-timed)
                               ( 0500 0600 0700 0800 0900 1000
                                 1100 1200 1300 1400 1500 1600
                                 1700 1800 1900 2000 2100 2200)
                               "" "")
        org-agenda-default-appointment-duration nil
        org-agenda-todo-ignore-with-date t
        org-agenda-todo-ignore-timestamp t
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-todo-ignore-time-comparison-use-seconds t
        org-agenda-tags-todo-honor-ignore-options nil
        org-agenda-show-inherited-tags t
        org-agenda-use-tag-inheritance '(todo search agenda)
        org-agenda-hide-tags-regexp nil
        org-agenda-remove-tags nil
        org-agenda-tags-column -100))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook
  ((jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode) . hs-minor-mode)
  ((jtsx-jsx-mode jtsx-tsx-mode jtsx-typescript-mode) . eglot-ensure)
  :config
  (setq js-indent-level 2))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package compile
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package xref
  :config
  (setq
   xref-search-program 'ripgrep
   xref-history-storage 'xref-window-local-history)

  ;; (defadvice! +xref--push-marker-stack-a (&rest rest)
  ;;   :before '(find-function consult-imenu consult-ripgrep citre-jump)
  ;;   (xref-push-marker-stack (point-marker)))
  )

(use-package eglot
  :hook ((c-mode c++-mode rust-mode python-mode java-mode c-ts-mode c++-ts-mode rust-ts-mode python-ts-mode) . eglot-ensure)
  :custom-face (eglot-highlight-symbol-face ((t (:underline t))))
  :bind (:map eglot-mode-map
              ("M-<return>" . eglot-code-actions)
              ("M-/" . eglot-find-typeDefinition)
              ("M-?" . xref-find-references))
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-report-progress 'messages)

  ;; eglot has it's own strategy by default
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq-default eglot-workspace-configuration
                '((:pyls . (:plugins (:jedi_completion (:fuzzy t))))
                  (:rust-analyzer . ( :cargo (:allFeatures t :allTargets t :features "full")
                                      :checkOnSave :json-false
                                      :completion ( :termSearch (:enable t)
                                                    :fullFunctionSignatures (:enable t))
                                      :hover ( :memoryLayout (:size "both")
                                               :show (:traitAssocItems 5)
                                               :documentation (:keywords (:enable :json-false)))
                                      :inlayHints (;:bindingModeHints (:enable t)
                                                   :lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
                                                   :closureReturnTypeHints (:enable "always")
                                                   :discriminantHints (:enable t)
                                                   :genericParameterHints (:lifetime (:enable t)))
                                      :semanticHighlighting ( :operator (:specialization (:enable t))
                                                              :punctuation (:enable t :specialization (:enable t)))
                                      :workspace (:symbol (:search ( :kind "all_symbols"
                                                                     :scope "workspace_and_dependencies")))
                                      :lru (:capacity 1024)))
                  (:typescript . (:preferences (:importModuleSpecifierPreference "non-relative")))
                  (:gopls . ((staticcheck . t)
                             (matcher . "CaseSensitive")))))

  (defsubst find-value-and-succ (value lst)
    (while (and lst (not (eq (car lst) value)))
      (setq lst (cdr lst)))
    (if lst
        (car (cdr lst))
      nil))

  (defsubst set-value-and-succ (key value lst)
    (let ((key-pos (member key lst)))
      (if key-pos
          (if (cdr key-pos)
              (setcar (cdr key-pos) value)
            (error "Key found but no value (no succ element) to update"))
        (setf lst (append lst (list key value)))))
    lst)

  (defsubst toggle-boolean-json (v)
    (if (eq v :json-false)
        t
      :json-false))

  (defun +eglot-toggle-exclude-imports-for-rust-analyzer ()
    (interactive)
    (let* ((current-config (alist-get :rust-analyzer eglot-workspace-configuration))
           (references (find-value-and-succ :references current-config))
           (val (find-value-and-succ :excludeImports references)))
      (if references
          (setf references (set-value-and-succ :excludeImports (toggle-boolean-json (or val :json-false)) references))
        (setq references (list :excludeImports t)))
      (setq current-config (set-value-and-succ :references references current-config))
      (setf (alist-get :rust-analyzer eglot-workspace-configuration) current-config)
      (if (eq val :json-false)
          (message "Exclude imports")
        (message "Include imports"))))

  (defun +eglot-toggle-exclude-tests-for-rust-analyzer ()
    (interactive)
    (let* ((current-config (alist-get :rust-analyzer eglot-workspace-configuration))
           (references (find-value-and-succ :references current-config))
           (val (find-value-and-succ :excludeTests references)))
      (if references
          (setf references (set-value-and-succ :excludeTests (toggle-boolean-json (or val :json-false)) references))
        (setq references (list :excludeTests t)))
      (setq current-config (set-value-and-succ :references references current-config))
      (setf (alist-get :rust-analyzer eglot-workspace-configuration) current-config)))

  ;; we call eldoc manually by C-h .
  (add-hook! eglot-managed-mode-hook
    (defun +eglot-disable-eldoc-mode ()
      (when (eglot-managed-p)
        (eldoc-mode -1)))))


(use-package eglot-booster
  :ensure nil
  :load-path (lambda () (locate-user-emacs-file "site-lisp/eglot-booster"))
  :after eglot
  :init (eglot-booster-mode))

(use-package eglot-x
  :ensure nil
  :load-path (lambda () (locate-user-emacs-file "site-lisp/eglot-x"))
  :hook (eglot-managed-mode . eglot-x-setup))


(use-package eldoc
  :diminish eldoc-mode
  :bind (("C-h h" . eldoc))
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t))

;; [consult-eglot] Eglot support for consult
;; (use-package consult-eglot
;;   :after consult eglot
;;   :ensure t
;;   :bind (:map eglot-mode-map
;;               ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        dumb-jump-default-project user-emacs-directory))

(use-package vc
  :config
  (setq vc-allow-async-revert t))

;; (use-package git-link
;;   :ensure t
;;   :bind (("C-, g l" . git-link)
;;          ("C-, g c" . git-link-commit)
;;          ("C-, g h" . git-link-homepage)))


(use-package diff-hl
  :ensure t
  :defines desktop-minor-mode-table
  :hook ((find-file    . diff-hl-mode)
         (vc-dir-mode  . diff-hl-dir-mode)
         (dired-mode   . diff-hl-dired-mode)
         ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) . +diff-hl--fallback-margin)
         ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) . diff-hl-show-hunk-mouse-mode))
  :config
  (setq
   diff-hl-disable-on-remote t
   vc-git-diff-switches '("--histogram"))

  (defun +diff-hl--fallback-margin ()
    (if (display-graphic-p)
        (diff-hl-margin-local-mode -1)
      (diff-hl-margin-local-mode))
    (diff-hl-update-once))

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (with-eval-after-load 'flymake
    (setq flymake-fringe-indicator-position 'right-fringe))
  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once)
  (advice-add #'vc-refresh-state :after #'diff-hl-update)
  (add-function :after after-focus-change-function #'diff-hl-update-once))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit))
  :hook ((magit-process-mode . goto-address-mode))
  :config
  (setq
   magit-diff-refine-hunk t
   magit-diff-paint-whitespace nil
   magit-save-repository-buffers nil
   magit-revision-insert-related-refs nil)

  (defun +magit-kill-buffers (&rest _)
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (let ((buffers (magit-mode-get-buffers)))
      (when (eq major-mode 'magit-status-mode)
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (if (and magit-this-process
                           (eq (process-status magit-this-process) 'run))
                      (bury-buffer buf)
                    (kill-buffer buf))))
              buffers))))
  (setq magit-bury-buffer-function #'+magit-kill-buffers)

  (defun +magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))

      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))

      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       :noquery t
       :sentinel
       (lambda (proc event)
         (when (eq (process-status proc) 'exit)
           (with-current-buffer (process-buffer proc)
             (goto-char (point-min))
             (ansi-color-apply-on-region (point-min) (point-max))
             (setq buffer-read-only t)
             (view-mode)
             (end-of-line)
             (let ((width (current-column)))
               (while (zerop (forward-line 1))
                 (end-of-line)
                 (setq width (max (current-column) width)))
               ;; Add column size of fringes
               (setq width (+ width
                              (fringe-columns 'left)
                              (fringe-columns 'right)))
               (goto-char (point-min))
               (pop-to-buffer
                (current-buffer)
                `(,(when (> 80 (- (frame-width) width))
                     #'display-buffer-at-bottom)
                  (window-width
                   . ,(min width (frame-width))))))))))))

  (defun +magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            (when (boundp 'rev) rev)
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (+magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))

  (defun +magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            (when (boundp 'range) range)
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (+magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic Diff (dwim)" +magit-diff-with-difftastic)
     ("S" "Difftastic Show" +magit-show-with-difftastic)]))


(use-package forge
  :ensure t
  :after magit
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline unspecified))))
  :config
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil))))


(use-package magit-todos
  :ensure t
  :after magit
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos))))


(use-package smerge-mode
  :hook ((find-file . +smerge-try-smerge))
  :config
  (defun +smerge-try-smerge ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (require 'smerge-mode)
          (smerge-mode 1))))))


(use-package browse-at-remote
  :ensure t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

(use-package git-modes
  :ensure t)


(use-package abridge-diff
  :ensure t
  :after magit ;; optional, if you'd like to use with magit
  :init (abridge-diff-mode 1))


(use-package magit-delta
  :ensure t
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-delta-args '("--max-line-distance" "0.6"
                                 "--24-bit-color" "always"
                                 "--color-only"
                                 "--features" "magit-delta"))

  (defun +magit-delta-toggle ()
    (interactive)
    (magit-delta-mode (if magit-delta-mode -1 1))
    (magit-refresh))
  (transient-append-suffix 'magit-diff '(-1 -1 -1)
    '("l" "Toggle magit-delta" +magit-delta-toggle)))

(use-package project
  :bind (:map project-prefix-map
              ("m" . magit-status))
  :config
  (setq project-switch-commands '((project-find-file "File")
                                  (project-find-regexp "Regexp")
                                  (project-switch-to-buffer "Buffer")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-search "Search")
                                  (magit-status "Magit")))

  (defun +project-previous-buffer (arg)
    "Toggle to the previous buffer that belongs to current project."
    (interactive "P")
    (unless arg
      (if-let ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))))

  ;; Use [fd] to find file in project
  (defun +search-project-files-with-fd (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'+search-project-files-with-fd
            (or dirs (list (project-root project))))))

(cl-loop for prefix in '("C-" "M-" "s-" "H-")
         do
         (cl-loop for cpunc in '("，" "。" "？" "！" "；" "：" "、" "（" "）" "【" "】" "《" "》" "—")
                  for epunc in '("," "." "?" "!" ";" ":" "\\" "(" ")" "[" "]" "<" ">" "_")
                  do (define-key key-translation-map (kbd (concat prefix cpunc)) (kbd (concat prefix epunc)))))

(defun +escape (&optional interactive)
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond
     ;; hide completion-preview
     ((and (boundp 'completion-preview-active-mode)
           completion-preview-active-mode)
      (completion-preview-hide))
     ;; hide *Completions* window
     ((window-live-p (get-buffer-window "*Completions*" 0))
      (minibuffer-hide-completions))
     ;; quit the minibuffer if open.
     ((minibuffer-window-active-p (minibuffer-window))
      (when interactive
        (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))
     ;; don't abort macros
     (executing-kbd-macro nil)
     ;; Back to the default
     ((unwind-protect
          (progn
            (meow-insert-exit)
            (keyboard-quit))
        (when interactive
          (setq this-command 'keyboard-quit)))))))

(defun +kill-word (arg)
  (interactive "p")
  (kill-region
   (point)
   (let* ((forward (> arg 0))
          (peek-ch (if forward (char-after) (char-before)))
          (move-ptr-fn (if forward 're-search-forward
                         're-search-backward))
          (move-back-fn (if forward 'left-char 'right-char)))
     (condition-case nil
         (progn
           (pcase (char-to-string peek-ch)
             ("\n" (funcall move-ptr-fn "[^\n]"))
             ((rx space) (funcall move-ptr-fn "[^[:space:]]\\|[\n]"))
             ((rx (any "$" "_" alnum)) (funcall move-ptr-fn "[^$_[:alnum:]]"))
             ((rx punct) (funcall move-ptr-fn "[$_[:alnum:][:space:]]")))
           (funcall move-back-fn))
       (search-failed
        (goto-char
         (if forward (point-max) (point-min)))))
     (point))))

(defun +backward-kill-word (arg)
  (interactive "p")
  (+kill-word (- arg)))

(define-key global-map (kbd "C--") #'text-scale-decrease)
(define-key global-map (kbd "C-=") #'text-scale-increase)
(define-key global-map (kbd "C-<backspace>") #'+backward-kill-word)
(define-key global-map (kbd "C-<delete>") #'+kill-word)
(define-key input-decode-map (kbd "C-[") [control-bracketleft])

;; [meow] Modal editing
(use-package meow
  :ensure t
  :hook (emacs-startup . meow-global-mode)
  :demand t
  :diminish
  meow-normal-mode
  meow-motion-mode
  meow-insert-mode
  meow-keypad-mode
  meow-beacon-mode
  :config
  (setq meow-expand-exclude-mode-list nil
        meow-expand-hint-remove-delay 4.0)
  (setq-default meow-replace-state-name-list '((normal . "N")
                                               (motion . "M")
                                               (keypad . "K")
                                               (insert . "I")
                                               (beacon . "B")))
  (define-key meow-insert-state-keymap [control-bracketleft] #'+escape)
  (define-key meow-normal-state-keymap [control-bracketleft] #'+escape)
  (define-key meow-motion-state-keymap [control-bracketleft] #'+escape)
  (define-key meow-keypad-state-keymap [control-bracketleft] #'+escape)
  (define-key meow-beacon-state-keymap [control-bracketleft] #'+escape)
  
  ;; [motion]
  (meow-motion-overwrite-define-key
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("<escape>" . +escape))

  ;; [leader]
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; [normal]
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . +escape))

  (dolist
      (state
       '((telega-root-mode . motion)
         (telega-chat-mode . normal)
         (view-mode . normal)
         ;; (compilation-mode . normal)
         (blink-search-mode . insert)
         (rcirc-mode . normal)
         (comint-mode . normal)
         (fundamental-mode . normal)
         (message-mode . normal)
         (emacs-lisp-mode . normal)
         (eshell-mode . insert)
         (shell-mode . insert)
         (term-mode . insert)
         (vterm-mode . insert)
         (help-mode . normal)))
    (add-to-list 'meow-mode-state-list state)))

(use-package eshell
  :config
  (setq eshell-banner-message ""))

(use-package eat
  :ensure t
  :diminish eat-shell-mode
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)
  :config
  (setq eat-kill-buffer-on-exit t))
