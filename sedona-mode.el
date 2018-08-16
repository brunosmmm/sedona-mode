;;; sedona-mode.el ---  StrEaming DOmaiN Architecture DSL mode
;; Author: Bruno Morais <brunosmmm@gmail.com>

;;; Code:

(require 'custom)
(require 'font-lock)
(require 'regexp-opt)

(defgroup sedona nil
  "Major mode for Sedona DSL."
  :prefix "sedona-"
  :group 'languages)

(defconst sedona/default-tab-width 2)

(defconst sedona/keywords-regexp
  (regexp-opt
   (list "hierarchy"
         "module"
         "import"
         "generate"
         "genport"
         "enum"
         "note"
         "factory"
         "when"
         "otherwise"
         "map"
         "fail"
         "broadcast"
         "slicer"
         "splicer"
         "slice"
         "splice"
         "join"
         "behavior"
         "pipe"
         "par"
         "seq"
         "lambda"
         "auto"
         "band"
         "field"
         "using")
   'words)
  "Regular expression for sedona keywords.")

(defconst sedona/builtin-regexp
  (regexp-opt
   (list "in"
         "out"
         "inout"
         "include"
         "define"
         "const"
         "type"
         "isequal"
         "range"
         "mkvar"
         "gettype"
         "override"
         "null")
   'words)
  "Regular expressions for sedona builtins.")

(defconst sedona/mapping-operator-regexp
  (regexp-opt
   (list "->")
   'words)
  "Mapping operators")

(defconst sedona/builtin-type-regexp
  (regexp-opt
   (list "int"
         "bool")
   'words)
  "Sedona builtin types")

(defconst sedona/builtin-const-regexp
  (regexp-opt
   (list "true"
         "false")
   'words)
  "Sedona builtin constants")

(defconst sedona/module-inst-regex
  "\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*\\(#(.*)\\)?\\s-*(.*)"
  )

(defconst sedona/variable-declaration-regex
  "\\(\\sw+\\)\\s-+[a-zA-Z_0-9, ]+;?"
  )

(defconst sedona/type-declaration-regex
  "\\(type\\)\\s-+\\(\\sw+\\)\\s-*;?"
  )

(defconst sedona/module-declaration-regex
  "\\s-*\\(hierarchy\\|module\\)\\s-*\\(\\sw+\\)\\s-*\\(#(.*)\\)?(.*)"
  )

(defconst sedona/channel-typification-regex
  "\\(type\\)\\s-+\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)\\((.*)\\)?\\s-*;?")

(defconst sedona/port-types-regex
  "\\(in\\|out\\|inout|const\\)\\s-+\\(genport(.+)\\s-+\\)?\\(\\sw+\\)\\s-+\\(\\sw+\\),?"
  )

(defconst sedona/note-regex
  "note\\s-+\\(\\sw+\\(\\.\\)\\)?\\(\\sw+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*;?")

(defconst sedona/mapping-regex
  "\\([0-9a-zA-Z_]+\\)\\s-+\\(->\\)\\s-+\\([0-9a-zA-Z_()]+\\)\\s-*,"
  )

(defconst sedona/when-regex
  "when\\s-+\\([\\s-\\sw<>,]+\\)\\s-*\\(->\\)\\s-*\\([\\s-\\sw()]+\\)\\s-*,"
  )

(defconst sedona/otherwise-regex
  "otherwise\\s-*\\(->\\)\\s-*.*")

(defconst sedona/enum-decl-regex
  "enum\\s-+\\(\\sw+\\)\\s-*{.+}\\s-*;?")

(defconst sedona/enum-access-regex
  "\\(\\sw+\\)::\\(\\sw+\\)")

(defconst sedona/define-regex
  "#define\\s-+\\(\\sw+\\)\\s-+\\(\\sw+\\)")

(defconst sedona/factory-definition-regex
  "factory\\s-+\\(\\sw+\\)\\s-*<[^>]+>\\s-*\\(=>\\)\\s-*\\(\\sw+\\)\\s-*:")

(defconst sedona/factory-call-regex
  "\\(\\sw+\\)<\\(.*\\)>")

(defconst sedona/custom-delimiter-regex
  "\\(<\\).*(\\(>\\))")

(defconst sedona/behavior-regex
  "behavior\\s-+\\(\\sw+\\)")

(defface sedona-functional-operator
  '((t :weight bold))
  "Face for funtional operators"
  )

(defconst sedona/font-lock-definitions
  `(
    ;; keywords, builtins, etc
    (,sedona/keywords-regexp
     (0 font-lock-keyword-face))
    (,sedona/builtin-regexp
     (0 font-lock-builtin-face))
    (,sedona/builtin-type-regexp
     (0 font-lock-type-face))
    (,sedona/builtin-const-regexp
     (0 font-lock-constant-face))
    (,sedona/mapping-operator-regexp
     (0 (get 'sedona-functional-operator 'face-defface-spec)))
    ;; module / hierarchy declaration
    (,sedona/module-declaration-regex
     (2 font-lock-function-name-face))
    ;; module / hierarchy instantiation
    (,sedona/module-inst-regex
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face))
    ;; behavior
    (,sedona/behavior-regex
     (1 font-lock-function-name-face))
    ;; variable and channel declarations
    (,sedona/variable-declaration-regex
     (1 font-lock-variable-name-face))
    ;; type declarations
    (,sedona/type-declaration-regex
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face))
    ;; channel typification
    (,sedona/channel-typification-regex
     (2 font-lock-type-face)
     (3 font-lock-type-face))
    ;; hierarchy ports
    (,sedona/port-types-regex
     (3 font-lock-type-face))
    ;; note statements
    (,sedona/note-regex
     (1 font-lock-type-face)
     (3 font-lock-variable-face)
     (4 font-lock-constant-face))
    ;; map statements
    (,sedona/mapping-regex
     (1 font-lock-variable-name-face t)
     (2 (get 'sedona-functional-operator 'face-defface-spec)))
    (,sedona/when-regex
     (2 (get 'sedona-functional-operator 'face-defface-spec)))
    (,sedona/otherwise-regex
     (1 (get 'sedona-functional-operator 'face-defface-spec)))
    ;; enums
    (,sedona/enum-decl-regex
     (1 font-lock-type-face))
    (,sedona/enum-access-regex
     (1 font-lock-type-face)
     (2 font-lock-constant-face))
    ;; define macros
    (,sedona/define-regex
     (1 font-lock-constant-face))
    ;; templates
    ("\\(\\[\\[\\)" 1 font-lock-warning-face)
    ("\\(\\]\\]\\)" 1 font-lock-warning-face)
    (,sedona/factory-definition-regex
     (1 font-lock-function-name-face)
     (2 (get 'sedona-functional-operator 'face-defface-spec)))
    (,sedona/factory-call-regex
     (1 font-lock-function-name-face))
    )
  "Sedona font-lock stuff.")

;; (font-lock-add-keywords
;;  'sedona-mode-font-lock-keywords
;;  '("\\s-*\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*(.*)" 1 'font-lock-function-name-face)
;;  )

;; indentation
(defun sedona-mode-indent-line ()
  "Indent lines"
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) extra-decl-indent decl-closing cur-indent)
      (if (looking-at "^.*}")
          (progn
            (save-excursion
              (if (looking-at "^.*{}")
                  (progn
                    (forward-line -1)
                    (setq cur-indent (current-indentation)))
                (forward-line -1)
                (setq cur-indent (- (current-indentation) sedona/default-tab-width))
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
                    (setq cur-indent (+ (current-indentation) sedona/default-tab-width))
                    (setq not-indented nil))
                (if (looking-at "^.*:$")
                    (progn
                      (setq cur-indent (+ (current-indentation) sedona/default-tab-width))
                      (setq not-indented nil))
                  (if (looking-at "^\\s-+otherwise\\s-*->.*;")
                      (progn
                        (setq cur-indent 0)
                        (setq not-indented nil))
                    (if (looking-at ".+#(.*[,)]$")
                        (progn
                          (setq extra-decl-indent (+ (string-match-p "#(.*[,)]$" (thing-at-point 'line t)) 1))
                          (if decl-closing
                              (setq cur-indent (- (current-indentation) extra-decl-indent))
                            (setq cur-indent (+ (current-indentation) extra-decl-indent)))
                          (setq not-indented nil))
                      (if (looking-at "^.+);$")
                          (progn
                            (setq decl-closing t))
                        (if (bobp)
                            (setq not-indented nil)))))))))))
      (if cur-indent
          (progn
            (if (< cur-indent 0)
                (setq cur-indent 0))
            (indent-line-to cur-indent))
        (indent-line-to 0)))))

(defvar sedona-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Sedona mode.")

;;;###autoload
(define-derived-mode sedona-mode prog-mode "Sedona"
  "Major mode for the Sedona DSL

\\{sedona-mode-map}"
  (use-local-map sedona-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(sedona/font-lock-definitions nil t))
  (set (make-local-variable 'indent-line-function) 'sedona-mode-indent-line)
  (font-lock-mode 1)
  ;; strings
  (modify-syntax-entry ?\" "\"" sedona-mode-syntax-table)
  ;; comment stuff
  (modify-syntax-entry ?/ ". 124b" sedona-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" sedona-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" sedona-mode-syntax-table)
  (modify-syntax-entry ?_ "w" sedona-mode-syntax-table)
  (modify-syntax-entry ?< "." sedona-mode-syntax-table)
  (modify-syntax-entry ?> "." sedona-mode-syntax-table)
  ;; do smartparens stuff
  (sp-local-pair 'sedona-custom-pairs "<" ">"
                 :skip-match (lambda (ms mb me)
                               (if mb
                                   nil
                                 t)))
  (sp-update-local-pairs 'sedona-custom-pairs))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.h\\(i|r\\)$" . sedona-mode)))

(provide 'sedona-mode)

;;; sedona-mode.el ends here
