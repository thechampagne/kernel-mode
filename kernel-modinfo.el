;;; kernel-modinfo.el --- Loaded kernel module information

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

(cl-defstruct kernel-modinfo
  "Module info for a loaded Linux kernel module"
  (name nil
        :read-only t
        :type '(string)
        :documentation "Unique name of a loaded Linux module")

  (mem-size 0
            :read-only t
            :type '(integer)
            :documentation "Memory size of the module, in bytes")

  (ref-count 0
             :read-only t
             :type '(integer)
             :documentation "Reference count of the module")

  (depends nil
           :read-only t
           :type '(list symbol)
           :documentation "List of symbols for other modules that depend on this modules")

  (load-state nil
              :read-only t
              :type '(symbol)
              :documentation "Load state of the module; one of LIVE, LOADING, or UNLOADING")

  (mem-offset nil
              :read-only t
              :type '(integer)
              :documentation "Kernel memory offset for module (if available)")

  (taints nil
        :read-only t
        :type '(list symbol)
        :documentation "List of tags indicating why the kernel thinks this module taints the image"))

(defun kernel--parse-taints (str)
  (let ((re "\\([A-Z]+\\)"))
    (when (string-match re str)
      (cl-loop for c across (substring str (match-beginning 0) (match-end 0))
               collect (intern (string c))))))

(defun kernel--modinfo-from-string (str)
  "Parse a line of module information (as presented from /proc/modules)"
  (let ((parts (split-string str)))
    (make-kernel-modinfo
     :name (intern (nth 0 parts))
     :mem-size (string-to-number (nth 1 parts))
     :ref-count (string-to-number (nth 2 parts))
     :depends (if (string-equal (nth 3 parts) "-")
                  nil
                (mapcar #'intern (delete "" (split-string (nth 3 parts) ","))))
     :load-state (intern (upcase (nth 4 parts)))
     :mem-offset (string-to-number (string-remove-prefix "0x" (nth 5 parts)))
     :taints (kernel--parse-taints (nth 6 parts)))))

(defun kernel--pretty-print-modinfo (modinfo)
  (message (if (not modinfo)
               "No loaded kernel modules..."
             (string-join
              (cons (format "%-20s %-13s %-12s %-12s %-15s %-10s %-16s"
                            "Name" "Memory Size" "References" "Load State"
                            "Kernel Offset" "Taints" "Dependents")
                    (cl-loop for mod in modinfo
                             collect (format "%-20s %-13d %-12d %-12s %#-15x %-10s %-16s"
                                             (kernel-modinfo-name mod)
                                             (kernel-modinfo-mem-size mod)
                                             (kernel-modinfo-ref-count mod)
                                             (kernel-modinfo-load-state mod)
                                             (kernel-modinfo-mem-offset mod)
                                             (kernel-modinfo-taints mod)
                                             (kernel-modinfo-depends mod))))
              "\n"))))

(defun kernel-loaded-modinfo (&optional print)
  "Return a list of module info for loaded Linux kernel modules."
  (interactive "p")
  (let* ((modinfo-raw
          (with-temp-buffer
            (insert-file-contents "/proc/modules")
            (buffer-string)))
         (modinfo-lines (delete "" (split-string modinfo-raw "\r?\n")))
         (modinfo (mapcar #'kernel--modinfo-from-string modinfo-lines)))
    (when print
      (kernel--pretty-print-modinfo modinfo))
    modinfo))

(cl-defmacro kernel-with-modinfo ((var module-name) &body body)
  "Evaluate an Emacs lisp block with the module information
for MODULE-NAME (if any) bound to VAR."
  (declare (indent defun))
  (let ((modinfo-sym (gensym))
        (mod-sym (gensym)))
    `(when (or t (symbolp ,module-name))
       (let* ((,modinfo-sym (kernel-loaded-modinfo))
              (,var (cl-loop for ,mod-sym in ,modinfo-sym
                             if (equal (kernel-modinfo-name ,mod-sym) ,module-name)
                             return ,mod-sym)))
       ,@body))))

(provide 'kernel-modinfo)
