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
(put 'quit 'error-message "")

(mapc
 (lambda (lst)
   (set lst (cons '(width . (text-pixels . 1200)) (symbol-value lst)))
   (set lst (cons '(height . (text-pixels . 1200)) (symbol-value lst))))
 '(default-frame-alist initial-frame-alist))

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
  [#b00000000
   #b00000110
   #b00001100
   #b00011000
   #b00110000
   #b00011000
   #b00001100
   #b00000110])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000])

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

(defconst dohna-font-pixelsize 20)
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
