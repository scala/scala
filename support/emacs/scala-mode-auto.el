;;; scala-mode-auto.el - Provides autoload definitions for scala-mode.
;;; $Id$

(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))


;;;### (autoloads (scala-mode) "scala-mode" "scala-mode.el" (15703
;;;;;;  49825))
;;; Generated autoloads from scala-mode.el

(autoload (quote scala-mode) "scala-mode" "\
Major mode for editing Scala code.

When started, run `scala-mode-hook'.

\\{scala-mode-map}" t nil)

;;;***

;;;### (autoloads (scala-quit-interpreter scala-load-file scala-eval-buffer
;;;;;;  scala-eval-region scala-switch-to-interpreter run-scala scala-interpreter-running-p-1)
;;;;;;  "inferior-scala-mode" "inferior-scala-mode.el" (15703 53506))
;;; Generated autoloads from inferior-scala-mode.el

(autoload (quote scala-interpreter-running-p-1) "inferior-scala-mode" nil nil nil)

(autoload (quote run-scala) "inferior-scala-mode" "\
Run a Scala interpreter in an Emacs buffer" t nil)

(autoload (quote scala-switch-to-interpreter) "inferior-scala-mode" "\
Switch to buffer containing the interpreter" t nil)

(autoload (quote scala-eval-region) "inferior-scala-mode" "\
Send current region to Scala interpreter." t nil)

(autoload (quote scala-eval-buffer) "inferior-scala-mode" "\
Send whole buffer to Scala interpreter." t nil)

(autoload (quote scala-load-file) "inferior-scala-mode" "\
Load a file in the Scala interpreter." t nil)

(autoload (quote scala-quit-interpreter) "inferior-scala-mode" "\
Quit Scala interpreter." t nil)

;;;***

(provide 'scala-mode-auto)
