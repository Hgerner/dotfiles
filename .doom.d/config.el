;;; Pre-requisites
;;; apt install pandoc ripgrep


;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setenv "PATH" (concat ".:/home/hakan/.npm-global/bin" (getenv "PATH")))
(setq exec-path (append exec-path '( "/home/hakan/.npm-global/bin")))

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Håkan Gerner"
      user-mail-address "hakan.gerner@ctek.com")

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

;; (defvar cm-ac-enable t
;;   "enable cm auto complete on TAB")
;; 
;; (defvar cm-ac-menu-lines 20
;;   "enable cm auto complete on TAB")
;; 
;; (set-variable 'load-path (append load-path (list nil (substitute-in-file-name "$CM_UNIX_HOME/emacs"))))
;; (load-library "cm")
;; (load-library "cm-hide")
;; 
;; (setq cm-ac-menu-lines 10)

;; (require 'citre)
;; (require 'citre-config)

;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")



(setq
 doom-font (font-spec :family "Ubuntu Mono" :size 18)
 doom-variable-pitch-font (font-spec :family "Ubuntu" :size 18))

(after! doom-themes--colors
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(add-to-list 'auto-mode-alist '("\\.bb\\'" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . bitbake-mode))

(add-hook 'after-save-hook 'magit-after-save-refresh-status t)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq
 projectile-git-command "git ls-files -zco"
 projectile-indexing-method 'alien
 projectile-enable-caching t
 projectile-project-search-path '("~/github/yocto/poky", "~/.config/")
 )

(defun my/prelude-copy-filepath-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename))
    (message filename)))

(set-face-attribute 'lazy-highlight nil :foreground "black" :background "green")

;;(require 'yasnippet)
;;(yas-global-mode 1)
(blink-cursor-mode 1)
(beacon-mode 1)

(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

(setq max-specpdl-size 13000)


;; (setq indent-line-function 'insert-tab)

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (shell . t)))

(global-set-key (kbd "C-'") 'goto-last-change)
(global-set-key (kbd "C-S-2") 'goto-last-change)

;; Set custom indentation (e.g., 4 spaces)
(defun my-custom-c-style ()
  (setq c-basic-offset 4))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'my-custom-c-style)

;; Remove conflicting c-basic-offset value set by Google C Style
(eval-after-load 'google-c-style
  '(setq-default c-basic-offset 4))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)


(use-package company
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package! org-pandoc-import :after org)

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
