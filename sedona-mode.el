;;; sedona-mode.el ---  StrEaming DOmaiN Architecture DSL mode
;; Author: Bruno Morais <brunosmmm@gmail.com>

;;; Code:
(defvar sedona-mode-hook nil)

(defvar sedona-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; strings
    (modify-syntax-entry ?\" "\"" table)
    ;; comment stuff
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for sedona-mode")

(setq sedona-mode-keywords '("hierarchy" "import" "generate" "type" "genport" "enum" "module" "note"))
(setq sedona-mode-builtins '("in" "out" "inout" "include" "define" "const"))
;;(setq sedona-mode-operators '("&"))

(setq sedona-mode-keywords-regexp (regexp-opt sedona-mode-keywords 'words))
(setq sedona-mode-builtins-regexp (regexp-opt sedona-mode-builtins 'words))
;;(setq sedona-mode-operators-regexp (regexp-opt sedona-mode-operators 'words))

(setq sedona-mode-font-lock-keywords
      `(
        (,sedona-mode-keywords-regexp . font-lock-keyword-face)
        (,sedona-mode-builtins-regexp . font-lock-builtin-face)
        ;;(,sedona-mode-operators-regexp . font-lock-negation-char-face)
        ;; module declaration
        ("\\s-*\\(hierarchy\\|module\\)\\s-*\\(\\sw+\\)\\s-*\\(#(.*)\\)?(.*)" 2 font-lock-function-name-face)
        ;; module instantiation
        ("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*\\(#(.*)\\)?\\s-*(.*)" 1 font-lock-function-name-face)
        ("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*\\(#(.*)\\)?\\s-*(.*)" 2 font-lock-variable-name-face)
        ;; port type
        ("\\(in\\|out\\|inout\\)\\s-+\\(genport(.+)\\s-+\\)?\\(\\sw+\\)\\s-+\\(\\sw+\\),?" 3 font-lock-type-face)
        ;; parameter type?
        ;; variable / channel declaration
        ("^\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\s-*,?\\s-*\\)+;" 1 font-lock-type-face)
        ("^\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\s-*,?\\s-*\\)+;" 2 font-lock-variable-name-face)
        ;; type declaration
        ("type\\s-+\\(\\sw+\\)\\s-*;" 1 font-lock-type-face)
        ;; define
        ("#define\\s-+\\(\\sw+\\)\\s-+\\(\\sw+\\)" 1 font-lock-constant-face)
        ;; templates
        ("\\(\\[\\[\\)" 1 font-lock-warning-face)
        ("\\(\\]\\]\\)" 1 font-lock-warning-face)
        ;; enum
        ("enum\\s-+\\(\\sw+\\)\\s-*{.+}\\s-*;" 1 font-lock-type-face)
        ;; enum access
        ("\\(\\sw+\\)::\\(\\sw+\\)" 1 font-lock-type-face)
        ("\\(\\sw+\\)::\\(\\sw+\\)" 2 font-lock-constant-face)
        ;; notes
        ("note\\s-+\\(\\sw+\\(\\.\\)\\)?\\(\\sw+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*;" 1 font-lock-type-face)
        ("note\\s-+\\(\\sw+\\(\\.\\)\\)?\\(\\sw+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*;" 3 font-lock-variable-name-face)
        ("note\\s-+\\(\\sw+\\(\\.\\)\\)?\\(\\sw+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*;" 4 font-lock-constant-face)
        ))

(font-lock-add-keywords
 'sedona-mode-font-lock-keywords
 '("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*(.*)" 1 'font-lock-function-name-face)
 )

;; indentation
(defun sedona-mode-indent-line ()
  "Indent lines"
  (interactive)
  (beginning-of-line)
  (if (bobp)b
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^.*}")
          (progn
            (save-excursion
              (if (looking-at "^.*{}")
                  (progn
                    (forward-line -1)
                    (setq cur-indent (current-indentation)))
                (forward-line -1)
                (setq cur-indent (- (current-indentation) default-tab-width))
                (if (< cur-indent 0)
                    (setq cur-indent 0)))))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^.*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^.*{")
                  (progn
                    (setq cur-indent (+ (current-indentation) default-tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0)))))

(defun sedona-mode ()
  "Streaming Architecture DSL mode"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table sedona-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(sedona-mode-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'sedona-mode-indent-line)
  (setq major-mode 'sedona-mode)
  (setq mode-name "SpecC-H")
  (run-hooks 'sedona-mode-hook))

(provide 'sedona-mode)
;;; sedona-mode.el ends here
