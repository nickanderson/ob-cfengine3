;;; ob-cfengine3.el --- Org Babel functions for CFEngine 3

;;; Commentary:
;; Execute CFEngine 3 policy inside org-mode src blocks.

;; Author: Nick Anderson <nick@cmdln.org>
;; URL: https://github.com/nickanderson/ob-cfengine3
;; Version: 0.0.1
;;
;;; Code:

(defvar ob-cfengine3-command "/var/cfengine/bin/cf-agent"
  "Name of command to use for executing cfengine policy.")

(defvar ob-cfengine3-command-options ""
  "Option string that should be passed to the agent.
Note that --file will be appended to the options.")

(defvar ob-cfengine3-file-control-stdlib "body file control{ inputs => { '$(sys.libdir)/stdlib.cf' };}\n"
  "File control body to include the standard libriary from $(sys.libdir).
It is usefult to inject into an example source block before execution.")

;;;###autoload
(defconst ob-cfengine3-header-args-cfengine3
  '(
    (no-lock . :any)
    (include-stdlib . :any)
    (define . :any)
    (bundlesequence . :any))
  "CFEngine specific header arguments.")

;;;###autoload
(defun org-babel-execute:cfengine3 (body params)
  "Actuate a block of CFEngine 3 policy.
This function is called by `org-babel-execute-src-block'.

  A temporary file is constructed containing
  `ob-cfengine3-file-control-stdlib and the BODY of the src
  block. `ob-cfengine3-command' is used to execute the
  temporary file."

    (let* ((temporary-file-directory ".")
           (use-locks (cdr (assoc :use-locks params)))
           (include-stdlib (not (string= "no" (cdr (assoc :include-stdlib params)))))
           (define (cdr (assoc :define params)))
           (bundlesequence (cdr (assoc :bundlesequence params)))
    (tempfile (make-temp-file "cfengine3-")))
      (with-temp-file tempfile
        (when include-stdlib (insert ob-cfengine3-file-control-stdlib))
        (insert body))
      (unwind-protect
      (shell-command-to-string
      (concat
        ob-cfengine3-command
        " "
        (when bundlesequence (concat "--bundlesequence "  bundlesequence ))
        " "
        (when define (concat "--define "  define ))
        " "
        (unless use-locks "--no-lock")
        ob-cfengine3-command-options
        " "
        (format " --file %s" tempfile)))
    (delete-file tempfile))))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("cfengine3" . cf3))
  '(add-to-list 'org-babel-tangle-lang-exts '("cfengine3" . "cf")))

(provide 'ob-cfengine3)

;;; ob-cfengine3.el ends here
