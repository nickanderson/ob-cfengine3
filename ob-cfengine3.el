;;; ob-cfengine3.el --- Org Babel functions for CFEngine 3

;; Copyright (C) 2017  Nick Anderson

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; Author: Nick Anderson <nick@cmdln.org>
;; Keywords: tools, convenience
;; URL: https://github.com/nickanderson/ob-cfengine3
;; Version: 0.0.4

;;; Commentary:
;; Execute CFEngine 3 policy inside org-mode src blocks.

;;; Code:

(defvar ob-cfengine3-command "cf-agent"
  "Name of command to use for executing cfengine policy.")

(defvar ob-cfengine3-command-options nil
  "Option string that should be passed to the agent.
Note that --file will be appended to the options.")

(defvar ob-cfengine3-file-control-stdlib "body file control{ inputs => { '$(sys.libdir)/stdlib.cf' };}\n"
  "File control body to include the standard libriary from $(sys.libdir).
It is useful to inject into an example source block before execution.")

(defconst ob-cfengine3-header-args-cfengine3
  '(
    (debug . :any)
    (info . :any)
    (verbose . :any)
    (use-locks . :any)
    (include-stdlib . :any)
    (define . :any)
    (bundlesequence . :any)
    (command . :any)
    (command-in-result . :any)
    (command-in-result-command . :any)
    (command-in-result-prompt . :any)
    (command-in-result-filename . :any))
  "CFEngine specific header arguments.")

(defun org-babel-execute:cfengine3 (body params)
  "Actuate a block of CFEngine 3 policy.
This function is called by `org-babel-execute-src-block'.

  A temporary file is constructed containing
  `ob-cfengine3-file-control-stdlib and the BODY of the src
  block. `ob-cfengine3-command' is used to execute the
  temporary file."

  (let* ((temporary-file-directory ".")
         (debug                      (cdr (assoc :debug params)))
         (info                       (cdr (assoc :info params)))
         (verbose                    (cdr (assoc :verbose params)))
         (use-locks                  (cdr (assoc :use-locks params)))
         (include-stdlib             (not (string= "no" (cdr (assoc :include-stdlib params)))))
         (define                     (cdr (assoc :define params)))
         (bundlesequence             (cdr (assoc :bundlesequence params)))
         (command                    (or (cdr (assoc :command params)) ob-cfengine3-command))
         (command-in-result          (cdr (assoc :command-in-result params)))
         (command-in-result-command  (or (cdr (assoc :command-in-result-command params)) command))
         (command-in-result-prompt   (or (cdr (assoc :command-in-result-prompt params)) "# "))
         (tempfile-dir               (or (cdr (assoc :tmpdir params)) "."))
         (tempfile                   (make-temp-file (concat tempfile-dir "/cfengine3-")))
         (command-in-result-filename (or (cdr (assoc :command-in-result-filename params)) tempfile)))
    (with-temp-file tempfile
      (when include-stdlib (insert ob-cfengine3-file-control-stdlib))
      (insert body))
    (unwind-protect
        (let ((command-args
               (concat
                (when bundlesequence (concat "--bundlesequence "  bundlesequence " "))
                (when define (concat "--define "  define " "))
                (unless use-locks "--no-lock ")
                (when info "--inform ")
                (when verbose "--verbose ")
                ;; When debug header arg is given, add --debug with
                ;; all log modules enabled to the command string and
                ;; throw away the args
                (when debug (concat "--debug --log-modules=all "))
                (when ob-cfengine3-command-options (concat ob-cfengine3-command-options " ")))))
          (concat
           ;; When the :command-in-result header arg is specified,
           ;; include the command line in the output. The prompt,
           ;; command and filename to use (instead of the real ones)
           ;; can be specified with the :command-in-result-prompt,
           ;; :command-in-result-command and
           ;; :command-in-result-filename args.
           (when command-in-result
             (concat command-in-result-prompt
                     command-in-result-command " "
                     command-args
                     (format "--file %s" (shell-quote-argument command-in-result-filename))
                     "\n"))
           ;; Execute command and return output
           (shell-command-to-string
            (concat command " "
                    command-args
                    (format "--file %s" (shell-quote-argument tempfile))))))
      (delete-file tempfile))))

(add-to-list 'org-src-lang-modes '("cfengine3" . cfengine3))
(add-to-list 'org-babel-tangle-lang-exts '("cfengine3" . "cf"))

(setq org-babel-default-header-args:cfengine3
      '((:exports  . "both")
        ;; I don't know how to make it use the identity format directly
        ;;(:tangle-mode  . 384) ;; (identity #o600) The standard default
        (:tangle-mode  . 448)   ;; (identity #o700) Since we write the shebang, lets see how useful execution bit is
        (:shebang . "#!/var/cfengine/bin/cf-agent -f-")
        ;; By default, includ the standard library for exported files. To disable this set the :prologue header arg
        (:prologue . "body file control\n{\n      inputs => { '$(sys.libdir)/stdlib.cf' };\n}\n")
        (:results  . "output")))

(provide 'ob-cfengine3)
;;; ob-cfengine3.el ends here
