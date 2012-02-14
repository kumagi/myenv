;; kumazaki custom

(setq completions-format "vertical")
(setq tab-always-indent t)
(setq yank-pop-change-selection t)
(setq kill-do-not-save-duplicates t)
;;; coloring current line
(global-hl-line-mode t)
(set-face-background 'hl-line "gray10")
;;(set-face-background 'hl-line "white")

(add-to-list 'load-path "/home/kumagi/.emacs.d/")
(require 'color-theme)
(color-theme-initialize)
;;(setq color-theme-is-global t)
;;(color-theme-greiner)
(color-theme-dark-laptop)
;;(color-theme-dark-font-lock)
;;(color-theme-goldenrod)

;;; half transparent emacs frame
;;(active not-active moving resizing)
(modify-all-frames-parameters
 (list (cons 'alpha  '(100 90 40 30))))


;;; save history when emacs killed
(savehist-mode 1)
;;; save cursor position
(require 'saveplace)
(setq-default save-place t)

;;; view paren pair
(show-paren-mode 1)
(setq show-paren-style 'mixed)

(setq-default show-trailing-whitespace t)

;;; C-h set backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; display time and row-number and column number and region on mode-line
(display-time)
(line-number-mode 1)
(column-number-mode 1)
(transient-mark-mode 1)
(blink-cursor-mode 0)

;;; reduce GC frequency
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; save more logs
(setq message-log-max 100000)
;;; recursive minibuffer
;(setq enable-recursive-minibuffers t)

;;; disable dialogboc
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; save more history
(setq historyh-length 10000)
(setq history-length 10000)
;;; fast apply to echo area
(setq echo-keystrokes 0.1)

;;; warning on opening huge file (25MB
(setq large-file-warning-threshold (* 25 1024 1024))

;;; yes or no -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;;; conceal something
(tool-bar-mode -1)
;(menu-bar-mode 1)
(scroll-bar-mode -1)

;;;
;;; emacs lisp auto installer
;;;
;; ~/.emacs.d/auto-install/auto-install.el required
(setq load-path (cons "~/.emacs.d/auto-install" load-path))
(require 'auto-install)
;; add-load path for el installed by auto-install
(add-to-list 'load-path auto-install-directory)
;; add abbrev candidate EmacsWiki's page name
(auto-install-update-emacswiki-package-name t)
;; compatible install-elisp.el
(auto-install-compatibility-setup)
;; ediff file
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(require 'key-chord)
(setq key-chord-two-keys-delay 0.08)
(key-chord-mode 1)

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
;;; desable iconify
(define-key global-map (kbd "C-z") nil)
;;; jump begining of window
(key-chord-define-global "ui" 'line-to-top-of-window)
;;; indent region
(define-key global-map (kbd "s-i") 'indent-region)
;;; eval buffer
(define-key global-map (kbd "s-d") 'eval-buffer)
;;; grep
;(setq grep-command "find -name \"*.[ch]\" -print | xargs grep -n ")
(setq grep-command '("grep -nH -r -e  . 2> /dev/null" . 16))
(define-key global-map (kbd "C-M-g") 'grep)
;;; goto line
(global-set-key (kbd "M-g") 'goto-line)
;;; backups
(setq backup-inhibited t)
(setq delete-auto-save-files t)

;;(global-set-key (kbd "C-i") 'dabbrev-expand)

;; C-a C-a -> begin of buffer
;; C-e C-e -> end of buffer
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; find file at point (C-x C-f extention
(ffap-bindings)

;;; same filename indentify
(require 'uniquify)
;; filename<dir>
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; except *~*
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; buffer switching partitional match
(iswitchb-mode t)
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)


;;; find recentry fily
(setq recentf-max-saved-items 10000)
(setq recentf-excluede '("/TAGS$"))
(require 'recentf-ext)
(recentf-mode)

;; C-c C-f to open file
(global-set-key (kbd "\C-z\C-f") 'recentf-open-files)
;;(recentf-open-files)

;;; bookmark enable
(setq bookmark-save-flag 1)
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (const latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))
(global-set-key (kbd "\C-z\C-s") 'bookmark-set)
(global-set-key (kbd "C-.") 'bookmark-bmenu-list)


;;; emacsclient to connect
(server-start)
;(defun iconify-emacs-when-server-is-done ()
;  (unless server-clients (iconify-frame)))
;(add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
;; C-x C-c to close file
(global-set-key (kbd "C-x C-c") 'server-edit)
(defalias 'exit 'save-buffers-kill-emacs)

;;; auto save
(require 'auto-save-buffers)
(run-with-idle-timer 2 t 'auto-save-buffers)


;;; kill line
(setq kill-whole-line t)
;;; add final line
(setq require-final-newline t)
(setq next-line-add-newlines t)

;;; automatic closing buffer
;(require 'tempbuf)
;(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;;; logical line movement
(require 'screen-lines)
(add-hook 'text-mode-hook 'turn-on-screen-lines-mode)

;;; cursor movement undo and redo
(require 'point-undo)
(define-key global-map (kbd "<f7>") 'point-undo)
(define-key global-map (kbd "S-<f7>") 'point-redo)


;;; goto last modified place
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;;; yaml
(require 'yaml-mode)

;; image
(auto-image-file-mode t)

;;; auto insertion
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/insert/")
(define-auto-insert "\\.sh" "bash-template.sh")
          ; more insertion snipets come here...

(require 'snippet)
;; snippet.el で、addrev に定型文を追加する
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq-default abbrev-mode t) ;; abbrev-mode をon
             (snippet-with-abbrev-table 'local-abbrev-table
                                        ("pu" . "puts")
                                        )
             ))
;;; enable redo
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)
(setq undo-no-redo t)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;;; yasnippet
(setq load-path (cons "/home/kumagi/.emacs.d/plugins/yasnippet" load-path))
(require 'yasnippet-config)
(define-key yas/minor-mode-map (kbd "<C-henkan>") 'yas/expand)
(define-key yas/minor-mode-map (kbd "C-c s v") 'yas/visit-snippet-file)
(define-key yas/minor-mode-map (kbd "C-c s n") 'yas/new-snippet)
(define-key yas/minor-mode-map (kbd "C-c s i") 'yas/insert-snippet)
(define-key yas/minor-mode-map (kbd "C-c s r") 'yas/reload-all)
(define-key yas/minor-mode-map (kbd "C-M-j") 'yas/insert-snippet)
(setq yas/next-field-key "TAB")
(setq yas/prev-field-key "<S-tab>")
(setq yas/buffer-local-condition
      '(or (not (or (string= "font-lock-comment-face"
                             (get-char-property (point) 'face))
                    (string= "font-lock-string-face"
                             (get-char-property (point) 'face))))
           '(require-snippet-condition . force-in-comment)))
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets/")
(yas/load-directory "~/.emacs.d/rails/yasnippets-rails/rails-snippets/")
(yas/setup "~/.emacs.d/plugins/yasnippet")


;;; expand
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially
;;  try-complete-file-name
;;  try-expand-all-abbrevs
;;  try-expand-dabbrev
;;  try-expand-dabbrev-all-buffers
;;  try-expand-dabbrev-from-kill))

;;; auto complete
(require 'auto-complete-config)
(global-auto-complete-mode 1)
(add-to-list 'ac-dictionary-directories "/home/kumagi/.emacs.d/ac-dict")
(ac-config-default)
(add-hook 'auto-complete-mode-hook
          (lambda ()
            (define-key ac-completing-map (kbd "C-n") 'ac-next)
            (define-key ac-completing-map (kbd "C-p") 'ac-previous)))
(icomplete-mode 1)
(setq read-file-name-completion-ignore-case t)

;;; isearch occur
;;; create list where matched
(require 'occur-schroeder)
(define-key isearch-mode-map (kbd "M-s o") 'isearch-occur)

;;; multibuffer occur
(require 'color-moccur)
(setq moccur-split-word t)

;;; moccur edit mode
(require 'moccur-edit)
(setq moccur-split-word t)

;;; igrep
(require 'igrep)
(igrep-define Igrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
(igrep-find-define Igrep (igrep-use-zgrep nil)(igrep-regex-option "-n -Ou8"))
(global-set-key (kbd "C-z C-s") 'igrep)
(require 'grep-edit)

(global-set-key (kbd "C-z C-r") 'revert-buffer)
(define-key global-map (kbd "<f5>") 'revert-buffer)

;;; use gist
(require 'gist)
(global-set-key (kbd "C-z C-g") 'gist-buffer)

;;; tramp
(if (locate-library "tramp")
    (require 'tramp))
;;; ipa
;(if (locate-library "ipa")
;    (require 'ipa))

;;;;
;;; programming support
;;;;

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
    ("\\.perl$"    . perl-mode)
    ("\\.yaml"     .yaml-mode)
    ("\\.rb"     .ruby-mode)
    ) auto-mode-alist))



;;; flymake setting
(require 'flymake)
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-mmmx" "-msse" "-msse2" "-Wextra" "-fsyntax-only" "-std=gnu++0x" "-I." "-I/usr/include/qt4/QtCore" "-I/usr/include/qt4/QtGui" "-I/usr/include/qt4" "-I/usr/share/qt4/mkspecs/linux-g++" "-I/usr/include/wireshark" "-I/usr/include/wireshark/epan" "-I/usr/include/wireshark/wiretap" "-I/usr/include/wireshark/wsutil" "-I/usr/include/python2.6" "-I/usr/include/glib-2.0" local-file))))

;; ruby
(setq load-path (cons "~/.emacs.d/ruby/" load-path))

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;(set-face-background 'flymake-errline "pink")
;(set-face-background 'flymake-warnline "dark slate blue")
;(set-face-background 'flymake-warnline "gray")

(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
          (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
 
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode t))
             ))

(require 'ansi-color)


(add-hook 'ruby-mode-hook
    '(lambda ()
       (setq compile-command "rspec spec")
       (setq compilation-window-height 10)
       (define-key ruby-mode-map "\C-cc" 'compile)
       (setq compilation-read-command nil)
       (setq compilation-ask-about-save nil)
       ))
; cucumber
(add-to-list 'load-path "~/.emacs.d/cucumber")
(autoload 'feature-mode "feature-mode" "Mode for editing cucumber files" t)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
(setq feature-default-i18n-file "/path/to/gherkin/gem/i18n.yml")
(setq feature-default-language "ja")
(require 'feature-mode)


;; junk file manager
(require 'open-junk-file)
(setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S.")
;; summary view
(require 'summarye)
;; snapshot branch
(require 'multiverse)
(setq load-path (cons "/usr/share/emacs/site-lisp/" load-path))
(require 'gtags)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq default-process-coding-system '(utf-8 . utf-8))


;; specify that flymake use ant instead of make
;(setcdr (assoc "\\.java\\'" flymake-allowed-file-name-masks)
;        '(flymake-simple-ant-java-init flymake-simple-java-cleanup))

;; redefine to remove "check-syntax" target
;(defun flymake-get-ant-cmdline (source base-dir)
;  (list "ant"
;        (list "-buildfile"
;              (concat base-dir "/" "build.xml"))))

(add-hook 'java-mode-hook
          '(lambda ()
             (flymake-mode)))

(push '("\\.cc$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.hpp$" flymake-cc-init) flymake-allowed-file-name-masks)
(push '("\\.h$" flymake-cc-init) flymake-allowed-file-name-masks)

;;2tab
(setq-default tab-width 2)
(setq tab-width 2)
(setq-default tab-stop-list
  '(0 1 2 3 4 5 6 7 8 9 10 11 12 13))
(setq indent-tabs-mode nil)
(setq-default tab-width 2 indent-tabs-mode nil)


(add-hook 'c-mode-common-hook
    '(lambda ()
       (setq compile-command "make -k -j")
       (setq compilation-window-height 10)
       ;; include
       (require 'vc-hooks)
       (require 'hideshow)
       (require 'fold-dwim)
       (flymake-mode t)
       (hs-minor-mode t)
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
       (define-key c-mode-base-map "\C-z\C-u" 'imenu)
       (define-key c-mode-base-map "\C-t" 'fold-dwim-toggle)
       (define-key c-mode-base-map "\C-z\C-j" 'se/make-summary-buffer)
       (define-key c-mode-base-map "\C-cx" 'next-error)
       (define-key c-mode-base-map "\C-cc" 'compile)
       (define-key c-mode-base-map "\C-z\C-x" 'next-error)
       (define-key c-mode-base-map "\C-z\C-c" 'compile)))

(add-hook 'c++-mode-hook
    '(lambda ()
       ;;indent
       (c-set-style "stroustrup")
       (c-set-offset 'access-label '-)
       (c-set-offset 'arglist-close 0)
       (c-set-offset 'arglist-cont 0)
       (c-set-offset 'arglist-intro '+)
       (c-set-offset 'inline-open 0)
       (setq c-basic-offset 2)
       (setq tab-width 2)
       (c-set-offset 'innamespace 0)
       (c-set-offset 'arglist-close 0)))

;;; gdb settings
;;; buffers
(setq gdb-many-windows t)
;;; onmouse: print value
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
;;; io buffer
(setq gdb-use-separate-io-buffer t)
;;; show mini buffer in gud
(setq gud-tooltip-echo-area nil)

(setq diff-switches "-u")

;;; viewer mode
;; disable exit from view mode when write protected
(require 'viewer)
(viewer-stay-in-setup)
(setq view-mode-by-default-regexp "\\.log$")

;;; lisp paren edit
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;; edit server
(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))


;;; translator
(require 'text-translator)
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)



;;;
;;; Language
;;;
(set-language-environment "Japanese")
;;; coding system
;(set-default-coding-systems  'iso-2022-7bit)
;(set-buffer-file-coding-system 'iso-2022-7bit)
(set-default-coding-systems 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)

(set-terminal-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)

;;;
;;; Anthy
;;;
(require 'mozc)
(setq default-input-method "japanese-mozc")

;(push "/usr/share/emacs/site-lisp/anthy/" load-path)
(load-library "leim-list")
;(load-library "anthy")
;(if (>= emacs-major-version 23)
;(setq anthy-accept-timeout 1))
;(setq default-input-method "japanese-anthy")
(global-set-key (kbd "S-SPC") 'mozc-mode)

(setq its-enable-fullwidth-alphabet nil)
(setq anthy-wide-space " ")
(setq its-hira-period "．")
(setq its-hira-comma "，")
;;;
;;; latex
;;;

(add-hook 'LaTeX-mode-hook
    '(lambda ()
       (setq completion-mode t)
       (setq compile-command "omake")
       ;; do not ask for make's option.
       (setq compilation-read-command nil)
       ;; auto-saving when you make
       (setq compilation-ask-about-save nil)
       (global-set-key (kbd "C-c c") 'compile)
       (global-set-key (kbd "C-c x") 'next-error)))

(push "~/.emacs.d/auctex" load-path)
(if (locate-library "auctex")
    (load "auctex.el" nil t t))
(setq TeX-japanese-process-input-coding-system  'japanese-iso-8bit
      TeX-japanese-process-output-coding-system 'iso-2022-jp
      LaTeX-version     "2e"
      japanese-LaTeX-default-style  "jarticle"
      TeX-default-mode      'japanese-latex-mode
      TeX-force-default-mode    t
      LaTeX-top-caption-list    '("table" "tabular")
      TeX-command-default   "pLaTeX"
      TeX-parse-self      t
      japanese-LaTeX-command-default  "pLaTeX"
      TeX-output-view-style '(("^dvi$" "." "pxdvi %d"))
      LaTeX-float     "tn"
      LaTeX-figure-label    "fig:"
      LaTeX-table-label     "tab:"
      LaTeX-section-label   "sec:")
(eval-after-load "auctex"
  '(when window-system
     (require 'font-latex)))
;;; bibtex
(setq bib-bibtex-env-variable   "TEXMFHOME")
(autoload 'turn-on-bib-cite "bib-cite")
(add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
;;; reftex
(setq reftex-texpath-environment-variables  '("TEXMFHOME")
      reftex-bibpath-environment-variables  '("~/texmf//")
      reftex-plug-into-AUCTeX     t
      reftex-label-alist
      '(("figure"       ?F "fig:" "\\figref{%s}" caption nil)
  ("figure*"      ?F nil nil caption)
  ("table"        ?T "tab:" "\\tabref{%s}" caption nil)
  ("table*"       ?T nil nil caption))
      )
(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'ispell-minor-mode t)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)


(setq inhibit-startup-screen -1)

(require 'org-html5presentation)

;; lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'turn-on-font-lock)

;; font
;;(list-faces-display)
;;(create-fontset
;; "-unknown-Takaoゴシック-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1"
;; nil "takao")
;;(cond (window-system
;;       (set-default-font "-unknown-Takaoゴシック-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1")))
;;(setcdr (assoc 'font default-frame-alist) "-unknown-Takaoゴシック-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1")

;; twittering mode
(load "~/.emacs.d/twittering-mode.el")
(require 'twittering-mode)
(setq twittering-username "kumagi_bot")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 100 :width normal :foundry "unknown" :family "Takaoゴシック"))))
 '(cursor ((t (:background "saddle brown" :foreground "white"))))
 '(flymake-errline ((t (:background "red4" :underline "red"))))
 '(flymake-warnline ((t (:background "saddlebrown"))))
 '(hi-blue ((t (:background "blue" :foreground "white"))))
 '(hi-pink ((t (:background "pink" :foreground "white"))))
 '(hi-yellow ((t (:background "OrangeRed4" :foreground "white"))))
 '(highline-face ((t (:inherit nil :background "paleturquoise" :weight bold))))
 '(hl-line ((t (:inherit highlight :background "gray10" :weight bold))))
 '(primary-selection ((t (:background "dark green"))))
 '(region ((t (:background "green4")))))

;; gauche
(require 'cmuscheme)
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq scheme-program-name "gosh -i")

;; haml
(add-to-list 'load-path "/home/kumagi/.emacs.d/haml")
(require 'haml-mode)
(require 'sass-mode)

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

;; sml-mode-line
(require 'sml-modeline)
(sml-modeline-mode)

;; python
(setq load-path (cons "~/.emacs.d/python-mode.el-6.0.2" load-path))
(setq load-path (cons "/usr/share/emacs/site-lisp/pymacs/" load-path))
(require 'python)
(setq python-indent 2)

(add-hook 'python-mode-hook
          '(lambda ()
             (setq compile-command "nosetests -w tests -v")
             (setq compilation-window-height 10)
             (define-key python-mode-map "\C-cc" 'compile)
             (setq compilation-read-command nil)
             (setq compilation-ask-about-save nil)
             (setq python-indent 2)
             ))

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; cython
(setq load-path (cons "~/.emacs.d/cython/" load-path))
(require 'cython-mode)
(setq auto-mode-alist (cons '("\\.pyx$" . cython-mode) auto-mode-alist))

;; todo google-c-style


;; Ruby on Rails
;;; ido-mode
(require 'ido)
(ido-mode t)

;;; rinari
(add-to-list 'load-path "~/.emacs.d/rails/rinari")
(require 'rinari)

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rails/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))

;;; mumamo
;(require 'mumamo-fun)
;(setq mumamo-chunk-coloring 'submode-colored)
;(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))
;(add-hook 'eruby-mumamo-mode-hook
;          (lambda ()
;            (mumamo-no-chunk-coloring t)))
;; rinari-extend-by-emacs-rails.el

;; coffee
(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))


(add-hook 'haml-mode-hook
    '(lambda ()
       (setq compile-command "make")
       (setq compilation-window-height 10)
       (define-key ruby-mode-map "\C-cc" 'compile)
       (setq compilation-read-command nil)
       (setq compilation-ask-about-save nil)
       ))

;; ocaml
(add-to-list 'load-path "~/.emacs.d/ocaml/ocaml-mode")
(require 'caml)
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(global-set-key "\C-q" 'caml-types-show-type)

; tuareg-mode
(add-to-list 'load-path "~/.emacs.d/ocaml/tuareg-mode")
(require 'tuareg)
(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
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
;; caml-types
(add-to-list 'load-path "~/.emacs.d/ocaml")
(require 'caml-types)


;; highlighting
(global-set-key "\C-ch" 'highlight-regexp)
(global-set-key "\C-cu" 'unhighlight-regexp)
(put 'set-goal-column 'disabled nil)

;; battery display
(display-battery-mode t)

;; clojure mode
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)
;;(setq clojure-src-root (expand-file-name "/usr/bin/clojure"))
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
