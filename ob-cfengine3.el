;;(defvar org-babel-tangle-lang-exts)
;;(add-to-list 'org-babel-tangle-lang-exts '("cfengine3" . "cf"))

(defconst org-babel-header-args:cfengine3
  '(
    (no-lock . :any)
    (include-stdlib . :any)
    (define . :any)
    (bundlesequence . :any))
  "CFEngine specific header arguments.")

(defvar org-babel-cfengine3-command "/var/cfengine/bin/cf-agent"
  "Name of command to use for executing cfengine policy.")

(defvar org-babel-cfengine3-command-options ""
  "Option string that should be passed to the agent. Note that
  --file will be appended to the options.")

(defvar org-babel-cfengine3-file-control-stdlib "body file control{ inputs => { '$(sys.libdir)/stdlib.cf' };}\n"
  "File control body to include the standard libriary from
  $(sys.libdir). It is usefult to inject into an example source
  block before execution.")

(defun org-babel-execute:cfengine3 (body params)
  "Actuate a block of CFEngine 3 policy.
  This function is called by `org-babel-execute-src-block'.

  A temporary file is constructed containing
  `org-babel-cfengine3-file-control-stdlib and the body of the src
  block. `org-babel-cfengine3-command' is used to execute the
  temporary file."

    (let* ((temporary-file-directory ".")
           (use-locks (cdr (assoc :use-locks params)))
           (include-stdlib (not (string= "no" (cdr (assoc :include-stdlib params)))))
           (define (cdr (assoc :define params)))
           (bundlesequence (cdr (assoc :bundlesequence params)))
    (tempfile (make-temp-file "cfengine3-")))
      (with-temp-file tempfile
        ;; Include the standard library automatically
        ;; TODO Make this conditional

        (when include-stdlib (insert org-babel-cfengine3-file-control-stdlib))
        (insert body))
      (unwind-protect
      (shell-command-to-string
      ;;(format "cf-agent -Kf %s" tempfile))
      (concat
        org-babel-cfengine3-command
        " "
        (when bundlesequence (concat "--bundlesequence "  bundlesequence ))
        " "
        (when define (concat "--define "  define ))
        " "
        (unless use-locks "--no-lock")
        org-babel-cfengine3-command-options
        " "
        (format " --file %s" tempfile)))
    (delete-file tempfile))))
(provide ob-cfengine3)


