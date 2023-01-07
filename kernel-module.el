;;; kernel-module.el --- Interact with Linux kernel modules

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
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org/>

(defconst kernel-taint-string-alist
  '((P . "Proprietary module has been loaded")
    (F . "Module has been forcibly loaded")
    (S . "SMP with CPUs not designed for SMP")
    (R . "User forced a module unload")
    (M . "System experienced a machine check exception")
    (B . "System has hit bad_page")
    (U . "Userspace-defined naughtiness")
    (D . "Kernel has oopsed before")
    (A . "ACPI table overridden")
    (W . "Taint on warning")
    (C . "modules from drivers/staging are loaded")
    (I . "Working around severe firmware bug")
    (O . "Out-of-tree module has been loaded"))
  "Association list between taint flags and human-readable reasons.")

(defun kernel-taint-reason (module-name &optional interactivep)
  "Return a list of reasons why MODULE-NAME taints the kernel."
  (interactive (list (completing-read-module "Module Taint Info: ") t))
  (kernel-with-modinfo (mod (intern module-name))
    (when (not mod)
      (error "%s is not a loaded module!" module-name))

    (let ((reasons (mapcar (lambda (tag) (or (assoc tag kernel-taint-string-alist)
                                             (tag ."Unknown taint reason")))
                           (kernel-modinfo-taints mod))))
      (when (and interactivep reasons)
        ;; print it all nice-like for the user
        (message (string-join
                  (cons (format "%s is tainting kernel:" module-name)
                        (mapcar (lambda (r) (concat (symbol-name (car r))
                                                    " - "
                                                    (cdr r)))
                                reasons))
                  "\n")))
  reasons)))

(defun kernel-lsmod (&optional print-info)
  "Return a list of loaded Linux kernel modules.

When run interactively or PRINT-INFO is non-nil, print a nice result"
  (interactive "p")
  (let ((modules (mapcar #'kernel-modinfo-name (kernel-loaded-modinfo))))
    (when print-info
      (princ modules))
    modules))

(defun kernel-insmod (module-path)
  "Insert a module into the Linux kernel."
  (interactive "fModule File: ")
  (kernel--with-sudo
    (compile (concat "sudo insmod " (shell-quote-argument module-path)))))

(defun kernel-rmmod (module)
  "Remove a module from the Linux kernel."
  (interactive (list (completing-read-module "Remove Module: ")))

  (when (and module (not (string-empty-p module)))
    (kernel--with-sudo
      (compile (concat "sudo rmmod " (shell-quote-argument module))))))

(provide 'kernel-module)
