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

;; To add mode to `magic-fallback-mode-alist'
;; (autoload 'yas-snippet-mode-buffer-p "yasnippet")

(defcustom snippet-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `snippet-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'yasnippet)

(defface snippet-ts-code-face
  '((t (:background "#181418")))
  "Face for elisp code in snippets."
  :group 'yasnippet)

(defface snippet-ts-field-face
  '((t (:inherit font-lock-warning-face)))
  "Face for fields with default values."
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
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for `snippet-ts-mode'.")

;;; Font-locking

(defvar snippet-ts-mode--keywords
  '("binding" "condition" "contributor" "expand-env" "group" "key" "name" "type"
    "uuid")
  "Snippet keywords for tree-sitter font-locking.")

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
      _ @font-lock-comment-face :*
      ;; name: (key) @font-lock-keyword-face
      ;; ":" @font-lock-delimiter-face
      ;; value: (value) @font-lock-comment-face
      )

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
   `([,@snippet-ts-mode--keywords] @font-lock-keyword-face

     (directive
      value: (value) @font-lock-constant-face)

     (mirror
      "${" @snippet-ts-field-face
      index: (number) @snippet-ts-field-face
      "}" @snippet-ts-field-face)

     (elisp_code
      ["$(" ")"] @font-lock-preprocessor-face)

     (field "$" @font-lock-misc-punctuation-face
            index: (_) @font-lock-constant-face)

     (field "${" @snippet-ts-field-face
            index: (_) @snippet-ts-field-face
            "}" @snippet-ts-field-face))

   :language 'yasnippet
   :feature 'code
   :override 'append
   '((elisp_code) @snippet-ts-code-face)

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
   :feature 'bracket
   :override 'prepend
   '((parenthesized_expression ["(" ")"] @font-lock-bracket-face)))
  "Tree-sitter font-lock settings for Yasnippets.")

(defvar snippet-ts-mode-feature-list
  '(( comment)
    ( keyword string)
    ( escape-sequence number code)
    ( bracket delimiter))
  "Snippet keywords for tree-sitter font-locking.")

(defvar snippet-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?$ "'" table)
    table)
  "Syntax table in `snippet-ts-mode'.")

(defvar-keymap snippet-ts-mode-map
  :doc "Keymap in `snippet-ts-mode'."
  :parent snippet-mode-map)

;;;###autoload
(define-derived-mode snippet-ts-mode prog-mode "YaSnippet"
  "Major mode for editing yasnippet snippets, powered by tree-sitter."
  :group 'yasnippet
  :syntax-table snippet-ts-mode-syntax-table

  (when (treesit-ready-p 'yasnippet)
    (treesit-parser-create 'yasnippet)

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
