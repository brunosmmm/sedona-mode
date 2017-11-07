;;; stmode.el ---  (Name here) Streaming Architecture DSL mode
;; Author: Bruno Morais <brunosmmm@gmail.com>

;;; Code:
(defvar wpdl-mode-hook nil)

(defvar st-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; strings
    (modify-syntax-entry ?\" "\"" table)
    ;; comment stuff
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for st-mode")

(setq st-mode-keywords '("module" "include" "import" "define"))
(setq st-mode-builtins '("in" "out" "inout" "buffer" "parent" "self"))
(setq st-mode-operators '("&"))

(setq st-mode-keywords-regexp (regexp-opt st-mode-keywords 'words))
(setq st-mode-builtins-regexp (regexp-opt st-mode-builtins 'words))
(setq st-mode-operators-regexp (regexp-opt st-mode-operators 'words))

(setq st-mode-font-lock-keywords
      `(
        (,st-mode-keywords-regexp . font-lock-keyword-face)
        (,st-mode-builtins-regexp . font-lock-builtin-face)
        (,st-mode-operators-regexp . font-lock-negation-char-face)
        ;; module declaration
        ("\\s-*module\\s-*\\(\\sw+\\)\\s-*(.*)" 1 font-lock-function-name-face)
        ;; module instantiation
        ("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*(.*)" 1 font-lock-function-name-face)
        ;; port type
        ("\\(in\\|out\\|inout\\)\\(\\s-+buffer\\)?\\s-+\\(\\sw+\\)\\s-+\\(\\sw+\\),?" 3 font-lock-type-face)
        ;; attribute assigment
        ("\\(\\sw+\\)\\s-*=\\s-*\\(\\sw+\\)" 1 font-lock-variable-name-face)
        ;; named scope
        ("\\(\\sw+\\)\\s-*{.*" 1 font-lock-variable-name-face)
        ;; define
        ("define\\s-+\\(\\sw+\\)\\s-+\\(\\sw+\\)" 1 font-lock-constant-face)
        ))

(font-lock-add-keywords
 'st-mode-font-lock-keywords
 '("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*(.*)" 1 'font-lock-function-name-face)
 )

(defun st-mode ()
  "Streaming Architecture DSL mode"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table st-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(st-mode-font-lock-keywords))
  (setq major-mode 'st-mode)
  (setq mode-name "ST")
  (run-hooks 'st-mode-hook))

(provide 'st-mode)
;;; stmode.el ends here
