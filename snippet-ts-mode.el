;;; snippet-ts-mode.el --- Tree-sitter support for yasnippet snippets -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/snippet-ts-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (yasnippet "0.14"))
;; Created: 22 April 2024
;; Keywords: tree-sitter, convenience, yasnippet

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Tree-sitter major mode for Yasnippet snippets.
;; Compatible with grammar from https://github.com/nverno/tree-sitter-yasnippet.
;;
;;; Code:

(require 'treesit)
(require 'yasnippet)                    ; `snippet-mode-map'
(require 'elisp-mode)                   ; xref, completion

;; To add mode to `magic-fallback-mode-alist'
;; (autoload 'yas-snippet-mode-buffer-p "yasnippet")

(defcustom snippet-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in elisp code in `snippet-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'yasnippet)

(defface snippet-ts-header-key-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for header keys."
  :group 'yasnippet)

(defface snippet-ts-code-face
  '((t (:background "#181418")))
  "Face to highlight elisp code in snippets."
  :group 'yasnippet)

(defface snippet-ts-field-bracket-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for field expansion brackets."
  :group 'yasnippet)

(defface snippet-ts-field-value-face
  '((t (:inherit font-lock-property-use-face)))
  "Face field expansions."
  :group 'yasnippet)


;;; Indentation

(defvar snippet-ts-mode--indent-rules
  `((yasnippet
     ((parent-is "source_file") column-0 0)
     ((match ")" "elisp_code") parent-bol 0)
     ((node-is ")") parent 1)
     ((parent-is "elisp_code") parent-bol snippet-ts-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent snippet-ts-mode-indent-offset)
     ((parent-is "string") no-indent)
     (no-node no-indent)))
  "Tree-sitter indentation rules for `snippet-ts-mode'.")


;;; Font-locking

(defvar snippet-ts-mode--header-keys
  '("binding" "condition" "contributor" "expand-env" "group" "key" "name" "type"
    "uuid")
  "Snippet header keywords for tree-sitter font-locking.")

(defvar snippet-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'yasnippet
   :feature 'comment
   '((local_variables
      ["#" "-*-"] @font-lock-comment-delimiter-face

      (local_definition
       name: (variable) @font-lock-variable-name-face
       value: (value) @font-lock-doc-face)
      :*)

     (comment) @font-lock-comment-face

     (directive
      "#" @font-lock-comment-delimiter-face
      _ @font-lock-comment-face :*)

     (header_end) @font-lock-comment-delimiter-face)

   :language 'yasnippet
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'yasnippet
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'yasnippet
   :feature 'keyword
   :override t
   `([,@snippet-ts-mode--header-keys] @snippet-ts-header-key-face

     ;; (directive
     ;;  value: (value) @font-lock-doc-face)

     (mirror
      "${" @snippet-ts-field-bracket-face
      index: (number) @font-lock-variable-name-face
      "}" @snippet-ts-field-bracket-face)

     (elisp_code
      ["$"] @font-lock-preprocessor-face)

     (backquote_expression
      ["`"] @font-lock-keyword-face)

     (field "$" @font-lock-misc-punctuation-face
            index: (_) @font-lock-variable-name-face)

     (field "${" @snippet-ts-field-bracket-face
            index: (_) @font-lock-variable-name-face
            "}" @snippet-ts-field-bracket-face))

   :language 'yasnippet
   :feature 'variable
   :override t
   '((field (text) @snippet-ts-field-value-face))

   ;; :language 'yasnippet
   ;; :feature 'function
   ;; '((parenthesized_expression
   ;;    :anchor
   ;;    (text) @font-lock-function-call-face))

   :language 'yasnippet
   :feature 'number
   '([(number)] @font-lock-number-face)

   :language 'yasnippet
   :feature 'delimiter
   :override t
   '([":"] @font-lock-delimiter-face)

   :language 'yasnippet
   :feature 'code
   :override 'append
   '((elisp_code) @snippet-ts-code-face
     (backquote_expression
      code: _ @snippet-ts-code-face))

   :language 'yasnippet
   :feature 'bracket
   :override 'prepend
   '((parenthesized_expression ["(" ")"] @font-lock-bracket-face)))
  "Tree-sitter font-lock settings for Yasnippets.")

(defvar snippet-ts-mode-feature-list
  '(( comment)
    ( keyword string)
    ( escape-sequence number code)
    ( bracket delimiter variable))
  "Snippet keywords for tree-sitter font-locking.")


;;; Syntax

(defvar snippet-ts-mode-syntax-table
  (let ((table (copy-syntax-table snippet-mode-syntax-table)))
    (modify-syntax-entry ?$ "'" table)
    table)
  "Syntax table in `snippet-ts-mode'.")

(defvar snippet-ts-mode--xref-syntax-table
  (let ((table (copy-syntax-table snippet-ts-mode-syntax-table)))
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?\? "_" table)
    (modify-syntax-entry ?~ "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?= "_" table)
    table)
  "Syntax table during xref in `snippet-ts-mode'.")


;;; Elisp parser

(defvar snippet-ts-mode--range-rules
  (when (treesit-available-p)
    (treesit-range-rules
     :host 'yasnippet
     :embed 'elisp
     '((elisp_code) @elisp))))

(defun snippet-ts-mode--language-at-point (point)
  "Return the tree-sitter language at POINT."
  (let ((node (treesit-node-at point 'yasnippet)))
    (if (treesit-parent-until
         node (rx (or "elisp_code" "backquote_expression")) t)
        'elisp
      'yasnippet)))


;;; Commands

(defvar snippet-ts-mode--field-query
  (when (treesit-available-p)
    (treesit-query-compile
     'yasnippet '((field index: (_) @field)
                  (mirror index: (_) @field))))
  "Query to find snippet fields.")

(defsubst snippet-ts-mode--in-code-p (&optional point)
  "Return non-nil when POINT is in elisp code."
  (eq 'elisp (snippet-ts-mode--language-at-point (or point (point)))))

(defsubst snippet-ts-mode--in-header-p (&optional point)
  "Return non-nil when POINT is in snippet header."
  (when-let* ((node (treesit-node-at (or point (point)) 'yasnippet)))
    (treesit-parent-until node "header" t)))

(defun snippet-ts-mode-next-field (&optional previous)
  "Move to the next snippet field.
When PREVIOUS is non-nil, move to previous field."
  (interactive)
  (when-let* ((cap (treesit-query-capture
                    'yasnippet snippet-ts-mode--field-query
                    (if previous (point-min) (point))
                    (if previous (point) (point-max))
                    t)))
    (and previous (setq cap (nreverse cap)))
    (when (treesit-node-enclosed-p (treesit-node-at (point)) (car cap))
      (pop cap))
    (and cap (goto-char (treesit-node-start (car cap))))))

(defun snippet-ts-mode-previous-field ()
  "Move to the previous snippet field."
  (interactive)
  (snippet-ts-mode-next-field -1))

(defun snippet-ts-mode-increment-fields (beg end &optional decrement)
  "Increment snippet field indices between BEG and END.
Use whole buffer unless region is active when called interactively.
With prefix, DECREMENT them instead."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (point-min) (point-max) current-prefix-arg)))
  (combine-change-calls beg end
    (let ((inc (if decrement -1 1))
          (regions
           (mapcar (lambda (c) (cons (treesit-node-start c) (treesit-node-end c)))
                   (treesit-query-capture
                    'yasnippet snippet-ts-mode--field-query beg end t))))
      (pcase-dolist (`(,s . ,e) regions)
        (let ((num (number-to-string
                    (+ inc (string-to-number
                            (buffer-substring-no-properties s e))))))
          (goto-char s)
          (delete-region (point) e)
          (insert num))))))

(defun snippet-ts-mode-decrement-fields (beg end)
  "Decrement snippet fields between BEG and END.
BEG and END default to entire buffer unless region is active."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (funcall #'snippet-ts-mode-increment-fields beg end -1))

(defun snippet-ts-mode-xref-find-definitions ()
  "Find definition at point using xref."
  (interactive)
  (when (snippet-ts-mode--in-code-p)
    (with-syntax-table snippet-ts-mode--xref-syntax-table
      (let* ((xref-backend-functions '(elisp--xref-backend t))
             (thing (xref-backend-identifier-at-point 'elisp)))
        (xref-find-definitions thing)))))


(defvar-keymap snippet-ts-mode-map
  :doc "Keymap in `snippet-ts-mode'."
  :parent snippet-mode-map
  "M-."     #'snippet-ts-mode-xref-find-definitions
  "C-c C-n" #'snippet-ts-mode-next-field
  "C-c C-p" #'snippet-ts-mode-previous-field
  "C-c +"   #'snippet-ts-mode-increment-fields
  "C-c -"   #'snippet-ts-mode-decrement-fields)

(defvar-keymap snippet-ts-mode-repeat-increment-map
  :repeat t
  "=" #'snippet-ts-mode-increment-fields
  "+" #'snippet-ts-mode-increment-fields
  "-" #'snippet-ts-mode-decrement-fields)
(put 'snippet-ts-mode-increment-fields 'repeat-check-key 'no)

(defvar-keymap snippet-ts-mode-repeat-move-map
  :repeat t
  "n" #'snippet-ts-mode-next-field
  "p" #'snippet-ts-mode-previous-field)


;;; Completion

(defun snippet-ts-mode-completion-at-point ()
  "Completion at point function for `snippet-ts-mode'."
  (cond ((snippet-ts-mode--in-header-p)
         (let* ((end (point))
                (beg (save-excursion
                       (skip-syntax-backward "w_")
                       (point)))
                (node (treesit-node-at (point))))
           (pcase (treesit-node-type node)
             ("value"
              (let ((field (string-trim
                            (treesit-node-text
                             (treesit-node-child-by-field-name
                              (treesit-node-parent node) "name")))))
                (pcase field
                  ((or "expand-env" "condition")
                   (elisp-completion-at-point))
                  ("type"
                   (list beg end '("snippet" "command"))))))
             ("directive" nil)
             ("comment"
              (save-excursion
                (goto-char beg)
                (and (looking-back "#\\s-*" (line-beginning-position))
                     (list beg end snippet-ts-mode--header-keys)))))))
        ((snippet-ts-mode--in-code-p)
         (elisp-completion-at-point))))


;;;###autoload
(define-derived-mode snippet-ts-mode prog-mode "YaSnippet"
  "Major mode for editing yasnippet snippets, powered by tree-sitter."
  :group 'yasnippet
  :syntax-table snippet-ts-mode-syntax-table

  (when (treesit-ready-p 'yasnippet)
    (treesit-parser-create 'yasnippet)

    (when (treesit-ready-p 'elisp t)
      (treesit-parser-create 'elisp)
      (setq-local treesit-language-at-point-function
                  #'snippet-ts-mode--language-at-point)
      (setq-local treesit-range-settings snippet-ts-mode--range-rules))

    ;; Comments
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "#" (* (syntax whitespace))))
    (setq-local require-final-newline nil)

    ;; Font-Locking
    (setq-local treesit-font-lock-settings snippet-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list snippet-ts-mode-feature-list)

    ;; Indentation
    (setq-local treesit-simple-indent-rules snippet-ts-mode--indent-rules
                indent-tabs-mode nil)

    (treesit-major-mode-setup)

    (setq-local require-final-newline nil)
    (add-hook 'xref-backend-functions 'elisp--xref-backend nil t)
    (add-hook 'completion-at-point-functions #'snippet-ts-mode-completion-at-point nil t)
    (add-hook 'eldoc-documentation-functions #'elisp-eldoc-funcall nil t)
    (add-hook 'eldoc-documentation-functions #'elisp-eldoc-var-docstring nil t)
    (add-hook 'after-save-hook #'yas-maybe-load-snippet-buffer nil t)))


(derived-mode-add-parents 'snippet-ts-mode '(snippet-mode))

(if (treesit-ready-p 'yasnippet)
    (add-to-list 'magic-fallback-mode-alist
                 `(yas-snippet-mode-buffer-p . snippet-ts-mode)))


(provide 'snippet-ts-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; snippet-ts-mode.el ends here
