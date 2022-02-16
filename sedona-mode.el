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
         "include"
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
         "stream"
         "attr"
         "endpoint"
         "splice"
         "join"
         "lambda"
         "auto"
         "band"
         "field"
         "using"
         "with"
         "partition"
         "interface"
         "constraint"
         "template"
         "localpartition"
         "implements")
   'words)
  "Regular expression for sedona keywords.")

(defconst sedona/builtin-regexp
  (regexp-opt
   (list "in"
         "out"
         "inout"
         "define"
         "const"
         "type"
         "isequal"
         "range"
         "mkstream"
         "gettype"
         "override"
         "read"
         "write"
         "application"
         "platform"
         "decomposable"
         "tile"
         "monolithic"
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
         "bool"
         "float"
         "str"
         "list"
         )
   'words)
  "Sedona builtin types")

(defconst sedona/builtin-const-regexp
  (regexp-opt
   (list "true"
         "false")
   'words)
  "Sedona builtin constants")

(defconst sedona/module-inst-regex
  "\\(\\sw+\\)\\s-+\\(\\sw+\\)\\s-*\\(#(.*)\\)?\\s-*(.*)"
  )

(defconst sedona/variable-declaration-regex
  "\\(\\sw+\\)\\s-+[a-zA-Z_0-9, ]+"
  )

(defconst sedona/type-declaration-regex
  "\\(type\\)\\s-+\\(\\sw+\\)\\s-*"
  )

(defconst sedona/module-declaration-regex
  "\\(\\(application\\s-+\\|platform\\s-+\\|tile\\s-+\\)\\|\\(monolithic\\s-+\\(decomposable\\s-+\\)?\\)\\)?\\(hierarchy\\|module\\)\\s-+\\(\\sw+\\)\\s-*\\(#(.*)\\)?\\s-*(.*)\\(\\s-*:\\s-*\\(\\sw+\\)\\)?"
  )

(defconst sedona/channel-typification-regex
  "\\(type\\)\\s-+\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)\\((.*)\\)?\\s-*")

(defconst sedona/port-types-regex
  "\\(in\\|out\\|inout\\|const\\)\\s-+\\(\\sw+\\)\\s-*\\(<[^>]+>\\)?\\s-*:\\s-*\\(\\sw+\\),?"
  )

(defconst sedona/note-regex
  "note\\s-+\\(\\sw+\\(\\.\\)\\)?\\(\\sw+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*")

(defconst sedona/mapping-regex
  "\\([0-9a-zA-Z_]+\\)\\s-+\\(->\\)\\s-+\\([0-9a-zA-Z_()]+\\)\\s-*,"
  )

(defconst sedona/when-regex
  "when\\s-+\\([\\s-\\sw<>,]+\\)\\s-*\\(->\\)\\s-*\\([\\s-\\sw()]+\\)\\s-*,"
  )

(defconst sedona/otherwise-regex
  "otherwise\\s-*\\(->\\)\\s-*.*")

(defconst sedona/enum-decl-regex
  "enum\\s-+\\(\\sw+\\)\\s-*{.+}\\s-*")

(defconst sedona/enum-access-regex
  "\\(\\sw+\\)::\\(\\sw+\\)")


(defconst sedona/factory-definition-regex
  "factory\\s-+\\(\\sw+\\)\\s-*<[^>]+>\\s-*\\(=>\\)\\s-*\\(\\sw+\\)\\s-*:")

(defconst sedona/factory-call-regex
  "\\(\\sw+\\)<\\(.*\\)>")

(defconst sedona/custom-delimiter-regex
  "\\(<\\).*(\\(>\\))")

(defconst sedona/simple-interface-decl-regex
  "\\(interface\\)\\s-+\\(\\sw+\\)"
  )

(defconst sedona/derived-interface-decl-regex
  "\\(interface\\)\\s-+\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)"
  )

(defconst sedona/partition-declaration-regex
  "^\\s-*\\(partition\\)\\s-+\\(\\sw+\\)"
  )

(defconst sedona/partition-implements-regex
  "\\(implements\\)\\s-+\\(\\(\\sw+\\),?\\)+"
  )

(defconst sedona/attr-regex
  "\\(attr\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*")

(defconst sedona/attr-member-regex
  "\\(attr\\)\\s-+\\(\\sw+\\).\\(\\sw+\\)\\s-*=\\s-*")

(defconst sedona/attr-decl-regex
  "\\(const\\s-+\\)?\\(attr\\)\\s-+\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)\\(\\s-*=\\s-*\\)?")

(defconst sedona/attr-decl-member-regex
  "\\(const\\s-+\\)?\\(attr\\)\\s-+\\(\\sw+\\).\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)\\(\\s-*=\\s-*\\)?")

(defconst sedona/endpoint-regex
  "\\(read\\|write\\|read\\s-+write\\|write\\s-+read\\)?\\s-+\\(endpoint\\)\\s-+\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)?\\s-+\\(\\sw+\\)")

(defconst sedona/name-list-regex
  "\\(|\\)\\s-*\\(\\sw+,?\\s-*\\)+\\s-*\\(|\\)")

(defconst sedona/slicer-regex
  "\\(slicer\\)\\s-+\\(\\sw+\\)\\(<[^>]+>\\)?\\s-*:\\s-*\\(\\sw+\\)\\s-*\\(=>\\)")

(defconst sedona/slicer-map-regex
  "\\(map\\)\\s-+\\(\\sw+\\).\\(\\sw+\\)\\(\\[[^]]+\\]\\)?\\s-*\\(->\\)")

(defconst sedona/stream-declaration-regex
  "\\(stream\\)\\s-+\\(\\sw+\\s-*,?\\s-*\\)+\\s-*:\\s-*\\(\\sw+\\)")

(defconst sedona/constraint-declaration-regex
  "constraint\\s-+\\(\\sw+\\)\\s-*:\\s-*")

(defconst sedona/template-declaration-regex
  "template\\s-+\\(\\sw+\\)")

(defface sedona-functional-operator
  '((t :weight bold))
  "Face for funtional operators"
  )

(defface sedona-pipeline-operator
  '((t :weight bold))
  "Face for pipeline operators"
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
    ;; template
    (,sedona/template-declaration-regex
     (1 font-lock-function-name-face))
    ;; stream
    (,sedona/stream-declaration-regex
     (3 font-lock-type-face))
    ;; slicer
    (,sedona/slicer-regex
     (2 font-lock-function-name-face)
     (4 font-lock-type-face)
     (5 (get 'sedona-functional-operator 'face-defface-spec))
     )
    (,sedona/slicer-map-regex
     (2 font-lock-type-face)
     (5 (get 'sedona-functional-operator 'face-defface-spec)))
    ;; attr
    (,sedona/attr-regex
     (2 font-lock-variable-name-face))
    (,sedona/attr-member-regex
     (2 font-lock-variable-name-face)
     (3 font-lock-function-name-face))
    (,sedona/attr-decl-regex
     (4 font-lock-type-face))
    (,sedona/attr-decl-member-regex
     (3 font-lock-variable-name-face)
     (4 font-lock-function-name-face)
     (5 font-lock-type-face))
    ;; name list
    (,sedona/name-list-regex
     (1 font-lock-builtin-face)
     (3 font-lock-builtin-face))
    ;;endpoint
    (,sedona/endpoint-regex
     (3 font-lock-variable-name-face)
     (4 font-lock-builtin-face nil t)
     (5 font-lock-type-face))
    ;; simple interface declaration
    (,sedona/simple-interface-decl-regex
     (2 font-lock-type-face))
    (,sedona/derived-interface-decl-regex
     (2 font-lock-type-face)
     (3 font-lock-type-face))
    ;; partition
    (,sedona/partition-declaration-regex
     (2 font-lock-type-face))
    ;; constraint
    (,sedona/constraint-declaration-regex
     (1 font-lock-function-name-face))
    ;; module / hierarchy declaration
    (,sedona/module-declaration-regex
     (6 font-lock-function-name-face)
     (9 font-lock-function-name-face nil t))
    ;; module / hierarchy instantiation
    (,sedona/module-inst-regex
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face))
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
     (2 font-lock-variable-name-face)
     (3 font-lock-constant-face nil t)
     (4 font-lock-type-face))
    ;; note statements
    (,sedona/note-regex
     (1 font-lock-type-face)
     (3 font-lock-variable-name-face)
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

(defvar sedona-imenu-generic-expression
  '(("Const" "const\\s-+\\(\\sw+\\)\\s-*:\\s-*\\(\\sw+\\)\\s-*=\\s-*\\(.+\\)$" 1)
    ("Module" "module\\s-+\\(\\sw+\\).*$" 1)
    ("Hierarchy" "hierarchy\\s-+\\(\\sw+\\).*$" 1)
    ("Interface" "interface\\s-+\\(\\sw+\\).*$" 1)
    ("Slicer" "slicer\\s-+\\(\\sw+\\).*$" 1)
    ("Template" "template\\s-+\\(\\sw+\\).*$" 1)
    )
  )

(defconst sedona-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "$" table)
    table)
  )

;;;###autoload
(define-derived-mode sedona-mode prog-mode "Sedona"
  "Major mode for the Sedona DSL."
  :syntax-table sedona-mode-syntax-table
  (setq font-lock-defaults '(sedona/font-lock-definitions))
  (setq-local indent-line-function 'sedona-mode-indent-line)
  (setq-local comment-start "// ")
  ;; do smartparens stuff
  (sp-local-pair 'sedona-custom-pairs "<" ">"
                 :skip-match (lambda (ms mb me)
                               (if mb
                                   nil
                                 t)))
  (sp-update-local-pairs 'sedona-custom-pairs)
  (setq-local imenu-generic-expression sedona-imenu-generic-expression)
  )


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.h\\(i|r\\)\\'" . sedona-mode))

(provide 'sedona-mode)

;;; sedona-mode.el ends here
