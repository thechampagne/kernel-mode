;;; kernel-mode.el --- Linux kernel helpers mode

;; Package: kernel-mode
;; Version: 0.2
;; Author: Jake Grossman <jakergrossman@gmail.com>
;; Keywords: Linux, C

;; This file is distributed under the terms of The Unlicense:
;;
;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org/>

(require 'cl-lib)
(require 'kernel-module)
(require 'kernel-modinfo)

(defgroup kernel nil
  "Support for Linux kernel development."
  :group 'languages)

(defcustom kernel-check-on-save nil
  "Recheck file for Linux kernel headers after saving a buffer."
  :type '(boolean)
  :group 'kernel)

(defcustom kernel-header-check-pattern-list '("\\(linux\\|asm\\)/[a-zA-Z0-9_]+\\.h")
  "List of single-line patterns to match the first `kernel-check-line-limit'
buffer lines to indicate that this is a C file using the Linux kernel.

Note that this is just the pattern for the inside of an #INCLUDE directive
(i.e., the filename). For example, the default pattern results in the following
effective pattern:

\"#include [<\\\"]\\\\(linux\\\\|asm\\\\)/[a-zA-Z_/]+\\\\.h[>\\\"]\""
  :type '(list string)
  :group 'kernel)

(defcustom kernel-check-line-limit 100
  "Number of lines to check at the start of the file using
`kernel-check-pattern-list'."
  :type '(integer)
  :group 'kernel)

(defvar kernel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k i") 'kernel-insmod)
    (define-key map (kbd "C-c C-k m") 'kernel-loaded-modinfo)
    (define-key map (kbd "C-c C-k r") 'kernel-rmmod)
    (define-key map (kbd "C-c C-k l") 'kernel-lsmod)
    (define-key map (kbd "C-c C-k t") 'kernel-taint-reason)
    map)
  "Keymap for `kernel-mode'")

(defun kernel--check-file-hook ()
  ;; check only when in c-mode, kernel-mode is not already active,
  ;; and kernel-mode is configured to check after save.
  (when (and (eq major-mode 'c-mode)
             (or (not kernel-mode)
                 kernel-check-on-save))
    (save-excursion
      (if (cl-loop initially (goto-char (point-min))
                   ;; read until end of buffer, or
                   ;; the configured amount of lines
                   while (not (eobp))

                   ;; `nil' and `0' mean "no limit"
                   until (and kernel-check-line-limit ; `nil'
                              (/= 0 kernel-check-line-limit)
                              (<= (line-number-at-pos) kernel-check-line-limit))

                   ;; check against each pattern in `kernel-header-check-pattern-list'
                   thereis (cl-loop for file-pat in kernel-header-check-pattern-list
                                    for pat = (concat "#include [<\"]" file-pat "[>\"]")
                                    thereis (string-match pat line)))
          (kernel-mode)))))

;;;###autoload
(add-hook 'c-mode-hook #'kernel--check-file-hook)

;;;###autoload
(add-hook 'c-mode-hook (lambda () (add-hook 'after-save-hook #'kernel--check-file-hook)))

;;;###autoload
(define-minor-mode kernel-mode
  "Toggle kernel minor mode for working with the linux kernel.

When kernel mode is enabled, commands for compiling, loading,
and interacting with kernel sources and modules are enabled

\\{kernel-mode-map}"
  :group 'languages
  :init-value nil
  :lighter " Kernel")

(cl-defmacro kernel--with-sudo (&body body)
  "Execute BODY in a sudo context with TRAMP."
  (declare (indent defun))
  `(with-temp-buffer
    (cd "/sudo::/")
    ,@body))

(cl-defmacro completing-read-module (&optional (prompt "Module: "))
  "Like `completing-read', but always present the output of `kernel-lsmod'
as choices."
  `(completing-read ,prompt (mapcar #'symbol-name (kernel-lsmod))))

(provide 'kernel-mode)
