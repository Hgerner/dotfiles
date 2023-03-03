;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!


;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")



(setq
 doom-font (font-spec :family "Ubuntu Mono" :size 20)
 doom-variable-pitch-font (font-spec :family "Ubuntu" :size 20))

(after! doom-themes--colors
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq
 projectile-git-command "git ls-files -zco"
 projectile-indexing-method 'alien
 projectile-enable-caching t
 projectile-project-search-path '("~/github/yocto/poky", "~/.config/")
 )

(set-face-attribute 'lazy-highlight nil :foreground "black" :background "green")

;;(require 'yasnippet)
;;(yas-global-mode 1)
(blink-cursor-mode 1)
(beacon-mode 1)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)))

(defconst my-c-style
    '((c-recognize-knr-p          . nil)
      (c-tab-always-indent        . t)
      (c-basic-offset             . 4)
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist     . ((block-close . c-snug-do-while)
                                     (brace-entry-open)
                                     (brace-list-close)
                                     (brace-list-open after)
                                     (brace-list-intro)
                                     (class-open after)
                                     (class-close before)
                                     (extern-lang-open after)
                                     (inexpr-class-open after)
                                     (inexpr-class-close before)
                                     (inline-open after)
                                     (inline-close before)
                                     (statement-cont)
                                     (substatement-open after)))
      (c-hanging-colons-alist     . ((member-init-intro before)
                                     (inher-intro)
                                     (case-label after)
                                     (label after)
                                     (access-label after)))
      (c-cleanup-list             . (scope-operator
                                     empty-defun-braces
                                     defun-close-semi))
      (c-offsets-alist            . ((access-label . -)
                                     (brace-list-close . 0)
                                     (brace-list-entry . 0)
                                     (brace-list-intro . +)
                                     (class-close . 0)
                                     (class-open . 0)
                                     (defun-block-intro . +)
                                     (defun-close . 0)
                                     (defun-open . 0)
                                     (inclass . +)
                                     (label . 0)
                                     (statement . 0)
                                     (statement-cont . *)
                                     (topmost-intro-cont . 0)
                                     (arglist-close . c-lineup-arglist)
                                     (block-open . 0)
                                     (case-label . +)
                                     (func-decl-cont . c-lineup-java-throws)
                                     (inexpr-class . 0)
                                     (inher-cont . c-lineup-java-inher)
                                     (inline-open . 0)
                                     (substatement-open . 0)
                                     (innamespace . 0)
                                     ))
      (c-echo-syntactic-information-p . t))
    "My C Programming Style")
;; Offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))
;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; other customizations
  (setq tab-width 4)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map "\M-a" 'backward-sexp)
  (define-key c-mode-base-map "\M-e" 'forward-sexp)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(setq c++-mode-hook
      '(lambda ()
         (font-lock-mode 1)
         ))
(setq java-mode-hook
      '(lambda ()
         (font-lock-mode 1)
         ))



(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq doom-theme 'doom-moonlight)

;;;; This is needed as of Org 9.2
;;(setup org-tempo
;;  (:when-loaded
;;    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
;;    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;    (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
;;    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
;;    (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
;;    (add-to-list 'org-structure-template-alist '("py" . "src python"))
;;    (add-to-list 'org-structure-template-alist '("go" . "src go"))
;;    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
;;    (add-to-list 'org-structure-template-alist '("json" . "src json"))))

;; (setq org-confirm-babel-evaluate nil)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
