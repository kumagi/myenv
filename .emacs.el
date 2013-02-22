(add-to-list 'load-path "~/.emacs.d/")

;; default-settings
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(iswitchb-mode 1)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)
(setq inhibit-startup-screen 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(setq-default show-trailing-whitespace t)
(line-number-mode 1)
(column-number-mode 1)
(transient-mark-mode 1)
(blink-cursor-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(when (or window-system (eq emacs-major-version '21))
  (setq font-lock-support-mode
        (if (fboundp 'jit-lock-mode) 'jit-lock-mode 'lazy-lock-mode))
  (global-font-lock-mode 1))
(global-font-lock-mode t)

(setq auto-save-list-file-prefix (expand-file-name "~/.emacs.d/.autosave"))


(global-hl-line-mode t)
(set-face-background 'hl-line "gray12")
;;; Disable C-z
(define-key global-map (kbd "C-z") nil)
(global-set-key (kbd "C-z C-r") 'revert-buffer)
(global-set-key (kbd "M-g") 'goto-line)
;(setq grep-command "find -name \"*.[ch]\" -print | xargs grep -n ")
(setq grep-command '("grep -nH -r -e  . 2> /dev/null" . 16))
(define-key global-map (kbd "C-M-g") 'grep)

;;2tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-stop-list
  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))

;; kill
(setq kill-whole-line t)

;;; my func
(defun line-to-top-of-window () (interactive) (recenter 0))
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)(split-window-horizontally))
  (other-window 1))
;;;
;;; my keybind or shortcuts
;;;
;;; jump window with C-,
(global-set-key (kbd "C-,") 'other-window-or-split)
(global-set-key (kbd "C-M-.") 'next-buffer)
(global-set-key (kbd "C-M-,") 'previous-buffer)
;;; C-h set backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;; built-in packages
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; except *~*
(setq uniquify-ignore-buffers-re "*[^*]+*")
(require 'recentf)
(setq recentf-max-saved-items 10000)
(setq recentf-excluede '("/TAGS$"))
(recentf-mode)
;; C-c C-f to open file
(global-set-key (kbd "\C-z\C-f") 'recentf-open-files)
(server-start)

;; alpha
(modify-all-frames-parameters
 (list (cons 'alpha  '(100 90 40 30))))

;;; save history when emacs killed
(savehist-mode 1)
;;; save cursor position
(require 'saveplace)
(setq-default save-place t)

(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'gtags)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-." 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\M-*" 'gtags-pop-stack)))

(require 'mic-paren)
(setq paren-match-face 'bold
      paren-sexp-mode t)

;; modelist

(setq auto-mode-alist
      (append '(
    ("\\.cc$"      . c++-mode)
    ("\\.c$"       . c-mode)
    ("\\.cpp$"     . c++-mode)
    ("\\.lc$"      . c++-mode)
    ("\\.h$"       . c++-mode)
    ("\\.hpp$"     . c++-mode)
    ("\\Makefile$" . makefile-mode)
    ("\\makefile$" . makefile-mode)
    ("\\.make$"    . makefile-mode)
    ("\\.pl$"      . perl-mode)
    ("\\.cgi$"      . perl-mode)
    ("wscript"      . python-mode)
    ("\\.haml$"      . haml-mode)
    ("\\.sass$"      . sass-mode)
    ("\\.perl$"    . perl-mode)
    ("\\.yaml$"     . yaml-mode)
    ("\\.yml$"     . yaml-mode)
    ("Cakefile$"     . coffee-mode)
    ("\\.coffee$"     . coffee-mode)
    ("\\.rb$"     . ruby-mode)
    ("Gemfile"     . ruby-mode)
    ("Rakefile"     . ruby-mode)
    ) auto-mode-alist))
(require 'coffee-mode)
;; c

(require 'flymake)
;(require 'google-c-style)

;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'c-mode-common-hook
    '(lambda ()
       (setq c-basic-offset 2)
       (setq c++-basic-offset 2)
       (setq compile-command "./waf --check")
       (setq compilation-window-height 10)
       ;; include
       ;(require 'vc-hooks)
       (require 'hideshow)
       ;(flymake-mode t)
       (which-func-mode 1)
       (setq which-func-mode t)
       (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
       (setq-default header-line-format '(which-func-mode ("" which-func-format)))
       (setq completion-mode t)
       (gtags-mode 1)
       (setq gtags-path-style 'relative)
       ;; do not ask for make's option.
       (setq compilation-read-command nil)
       ;; auto-saving when you make
       (setq compilation-ask-about-save nil)
       (setq c-auto-newline nil)
       (define-key c-mode-base-map "\C-cx" 'next-error)
       (define-key c-mode-base-map "\C-cc" 'compile)))
;; c++
(add-hook 'c++-mode-hook
    '(lambda ()
       ;;indent
       (setq c-basic-offset 2)
       ;(c-set-style "stroustrup")
       (c-set-offset 'arglist-cont 0)
       (c-set-offset 'access-label '1)
       (c-set-offset 'arglist-close 0)
       (c-set-offset 'arglist-intro '+)
       (c-set-offset 'inline-open 0)
       (c-set-offset 'stream-op '+)
       (gtags-mode 1)
       (gtags-path-style)
       (setq c-basic-offset 2)
       (setq tab-width 2)
       (c-set-offset 'innamespace 0)
       (c-set-offset 'arglist-close 0)
       ))
(add-hook 'c++-mode-hook
  (function (lambda ()
              (define-key c++-mode-map "\C-c\C-p" 'ff-find-other-file)
              (define-key c++-mode-map "\C-c\C-n" 'ff-find-other-file)
)))

;; OCaml


;; ocaml
(require 'tuareg)
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

; flymake
(defun flymake-ocaml-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-ocaml-cmdline))
(defun flymake-get-ocaml-cmdline (source base-dir)
  (list "ocaml_flycheck.pl"
	(list source base-dir)))

(push '(".+\\.ml[yilp]?$" flymake-ocaml-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push
 '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	 (count               (length line-err-info-list))
	 )
    (while (> count 0)
      (when line-err-info-list
	(let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
	       (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
	       (text (flymake-ler-text (nth (1- count) line-err-info-list)))
	       (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
	  (message "[%s] %s" line text)
	  )
	)
      (setq count (1- count)))))

(add-hook
 'tuareg-mode-hook
 '(lambda ()
    (if (not (null buffer-file-name)) (flymake-mode))
    (define-key tuareg-mode-map "\C-cc" 'credmp/flymake-display-err-minibuf)
    (define-key tuareg-mode-map "\C-cm" `flymake-goto-next-error)))

;; twitter
(require 'twittering-mode)

;; color
(add-to-list 'custom-theme-load-path "~/.emacs.d/")
(deftheme pastels-on-dark
   "kumagi custom")
(custom-theme-set-faces
 'pastels-on-dark
 '(cursor ((t (:background "#FFFF40"))))
 '(escape-glyph ((t (:foreground "#47B8D6"))))
 '(minibuffer-prompt ((t (:foreground "#47B8D6"))))
 '(highlight ((t (:background "#225555"))))
 '(hl-line ((t (:inherit highlight :background "gray12" :weight bold))))
 '(region ((t (:background "#121AA3"))))
 '(shadow ((t (:foreground "#5555AA"))))
 '(secondary-selection ((t (:background "#4638aa"))))
 '(trailing-whitespace ((t (:background "#FFD0D0"))))
 '(font-lock-builtin-face ((t (:foreground "#3191A3" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#2dAbAb" :inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#2CA"))))
 '(font-lock-constant-face ((t (:foreground "#DF7921" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#6BF" :wight bold))))
; '(font-lock-function-name-face ((t (:foreground "#FA0" :wight bold))))
 '(font-lock-keyword-face ((t (:foreground "#4856F7" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#48FF37"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "#666666"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "#666666"))))
 '(font-lock-string-face ((t (:foreground "#DDE05E"))))
 '(font-lock-type-face ((t (:foreground "#80DA80"))))
 '(font-lock-variable-name-face ((t (:foreground "#C1C144")))) ; variable name
 '(font-lock-warning-face ((t (:foreground "#FA2E00"))))
 '(link ((t (:foreground "#0066FF" :underline t))))
 '(link-visited ((t (:inherit link :foreground "#FF0066"))))
 '(fringe ((t nil)))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(isearch ((t (:background "#26A320" :foreground "#ffffff" :weight bold))))
 '(compilation-error ((t (:inherit error))))
 '(error ((t (:foreground "#B20006" :weight bold))))
 '(warning ((t (:foreground "#CCCC00" :weight bold))))
 '(success ((t (:foreground "#00FF00" :weight bold))))
 '(compilation-line-number ((t (:foreground "#EC9EBA"))))
 '(glyphless-char ((t (:background "#4F4D4D"))))
 '(lazy-highlight ((t (:background "#505740"))))
 '(default ((t (:background "#000210" :foreground "#DDD"))))
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((((background dark)) (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((((background dark)) (:foreground "cyan"))))
 '(rainbow-delimiters-depth-6-face ((((background dark)) (:foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((((background dark)) (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((((background dark)) (:foreground "brown"))))
 '(rainbow-delimiters-depth-9-face ((((background dark)) (:foreground "gray")))))
(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'bibtex-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
;; delete encoded-kbd-mode
(let ((elem (assq 'encoded-kbd-mode minor-mode-alist)))
  (when elem
    (setcar (cdr elem) "")))

;; modeline
;(require 'sml-modeline)
;(sml-modeline-mode 1)
;(set-face-background 'sml-modeline-end-face "#009EFF")
;(set-face-background 'sml-modeline-vis-face "LightYellow4")

;; git
(require 'git-commit-mode)
(require 'gitignore-mode)

;; google
;(require 'google-translate)
;(setq google-translate-default-source-language "en")
;(setq google-translate-default-target-language "ja")
;(global-set-key (kbd "\C-ct") 'google-translate-at-point)
;(global-set-key (kbd "\C-cT") 'google-translate-at-point-reverse)


;; hungry delete
(setq backward-delete-char-untabify-method 'hungry)
(defun ys:trim-whitespaces ()
  "Trim excess whitespaces."
  (interactive "*")
  (let ((key (read-char "Convert spaces?: (t)abify (u)ntabify (n)o"))
        (reg (and transient-mark-mode mark-active)))
    (save-excursion
      (save-restriction
        (if reg
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (replace-match "" nil nil))
        (if reg nil
          (goto-char (point-max))
          (delete-blank-lines))
        (cond
         ((= key ?t)
          (tabify (point-min) (point-max)))
         ((= key ?u)
          (untabify (point-min) (point-max)))))))
  (deactivate-mark))
;; yank highlight
(when (or window-system (eq emacs-major-version '21))
  (defadvice yank (after ys:highlight-string activate)
    (let ((ol (make-overlay (mark t) (point))))
      (overlay-put ol 'face 'highlight)
      (sit-for 0.5)
      (delete-overlay ol)))
  (defadvice yank-pop (after ys:highlight-string activate)
    (when (eq last-command 'yank)
      (let ((ol (make-overlay (mark t) (point))))
        (overlay-put ol 'face 'highlight)
        (sit-for 0.5)
        (delete-overlay ol)))))

;; undo+redo
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-?") 'redo)

;; dabbrev-highlight
(require 'dabbrev-highlight)
(when (or window-system (eq emacs-major-version '21))
  (require 'dabbrev-highlight nil t))

;; dont repeat killring
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; title
(setq frame-title-format "%b (%f)")
(setq icon-title-format "%b")

;; haml
(require 'haml-mode)
(require 'sass-mode)
;; slim
(require 'slim-mode)


;; untabify
(global-set-key (kbd "M-T")
                '(lambda()
                   (interactive)
                   (untabify 0 (point-max))
                   (message "untabify done.")))
(global-set-key (kbd "C-T")
                '(lambda()
                   (interactive)
                   (tabify 0 (point-max))
                   (message "tabify done.")))


;; japanese
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(require 'powerline)
(powerline-default-center)
(setq powerline-arrow-shape 'arrow14)

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
                    :foreground "#fff"
                    :background "#043")
(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
                    :foreground "#fff"
                    :background "#032")
(make-face 'mode-line-color-3)
(set-face-attribute 'mode-line-color-3 nil
                    :foreground "#000"
                    :background "#fff")
(set-face-attribute 'mode-line nil
                    :foreground "#fee"
                    :background "#359"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fff"
                    :background "#274")

(require 'tramp)
(setq tramp-default-method "ssh")

;; clojure
(require 'nrepl)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(clojure-mode
                      nrepl
                      nrepl-ritz))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
(defun my-nrepl-mode-setup ()
  (require 'nrepl-ritz))

;; cython
(require 'cython-mode)


;; R
(require 'ess)
(setq auto-mode-alist
     (cons (cons "\\.r$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(require 'ess-R-object-popup)
(require 'ess-smart-underscore)
(require 'r-autoyas)


;; evernote

(require 'evernote-mode)
(setq evernote-username "kumagi") ; optional: you can use this username as default.
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)
