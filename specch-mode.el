;;; specch-mode.el ---  (Name here) Streaming Architecture DSL mode
;; Author: Bruno Morais <brunosmmm@gmail.com>

;;; Code:
(defvar specch-mode-hook nil)

(defvar specch-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; strings
    (modify-syntax-entry ?\" "\"" table)
    ;; comment stuff
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table for specch-mode")

(setq specch-mode-keywords '("hierarchy" "import" "generate" "type" "genport"))
(setq specch-mode-builtins '("in" "out" "inout" "include" "define"))
;;(setq specch-mode-operators '("&"))

(setq specch-mode-keywords-regexp (regexp-opt specch-mode-keywords 'words))
(setq specch-mode-builtins-regexp (regexp-opt specch-mode-builtins 'words))
;;(setq specch-mode-operators-regexp (regexp-opt specch-mode-operators 'words))

(setq specch-mode-font-lock-keywords
      `(
        (,specch-mode-keywords-regexp . font-lock-keyword-face)
        (,specch-mode-builtins-regexp . font-lock-builtin-face)
        ;;(,specch-mode-operators-regexp . font-lock-negation-char-face)
        ;; module declaration
        ("\\s-*hierarchy\\s-*\\(\\sw+\\)\\s-*\\(#(.*)\\)?(.*)" 1 font-lock-function-name-face)
        ;; module instantiation
        ("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*\\(#(.*)\\)?\\s-*(.*)" 1 font-lock-function-name-face)
        ;; port type
        ("\\(in\\|out\\|inout\\)\\s-+\\(\\sw+\\)\\s-+\\(\\sw+\\),?" 2 font-lock-type-face)
        ;; variable / channel declaration
        ("^\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\s-*,?\\s-*\\)+;" 1 font-lock-type-face)
        ;; type declaration
        ("type\\s-+\\(\\sw+\\)\\s-*;" 1 font-lock-type-face)
        ;; define
        ("#define\\s-+\\(\\sw+\\)\\s-+\\(\\sw+\\)" 1 font-lock-constant-face)
        ))

(font-lock-add-keywords
 'specch-mode-font-lock-keywords
 '("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*(.*)" 1 'font-lock-function-name-face)
 )

;; indentation
(defun specch-mode-indent-line ()
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

(defun specch-mode ()
  "Streaming Architecture DSL mode"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table specch-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(specch-mode-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'specch-mode-indent-line)
  (setq major-mode 'specch-mode)
  (setq mode-name "SpecC-H")
  (run-hooks 'specch-mode-hook))

(provide 'specch-mode)
;;; specch-mode.el ends here
