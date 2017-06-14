(defun git-status (&rest args)
  (with-output-to-string (out)
    (run-program "/usr/bin/git"
                 (concatenate 'list '("status") args)
                 :output out)))

(defmacro stylize (color symbol string)
  "Wrapper around format to provide colors with shell escape sequences."
  (if (and symbol string)
      `(format nil "%{~C[~am%}~a~a%{~C[0m%}"  #\Esc ,color ,symbol ,string #\Esc)
      `(format nil "%{~C[~am%}~a%{~C[0m%}" #\Esc ,color ,(or symbol string) #\Esc)))

(defun get-branch (git-status)
  (cond ((search "no branch" git-status)
         (let ((commit (with-output-to-string (out)
                         (run-program "/usr/bin/git"
                                      '("rev-parse" "--short" "HEAD")
                                      :output out))))
           (stylize 92 nil (concatenate 'string
                                      ":"
                                      (subseq commit 0 (1- (length commit)))))))
        (t (cl-ppcre:register-groups-bind (branch)
               ("## (?:Initial commit on )?(.*)(?:\\.\\.\\.)" git-status)
             (stylize 92 nil branch)))))

(defun get-ahead-behind (git-status)
  (cl-ppcre:register-groups-bind (ahead behind)
      ("\\[(?:ahead (\\d))?(?:, )?(?:behind (\\d))?\\]" git-status)
    (concatenate 'string
                 (when ahead (stylize 95 "↑" ahead))
                 (when behind (stylize 95 "↓" behind)))))

(defmacro get-smc (git-status color symbol regex)
  "Get [S]taged, [M]odified, or [C]onflicts"
  `(let ((num (+ (length (cl-ppcre:all-matches-as-strings
                          ,regex
                          ,git-status)))))
     (when (/= num 0)
       (stylize ,color ,symbol num))))

(defun get-dirty (git-status)
  (when (cl-ppcre:scan "\\n\\?\\? " git-status) "…"))

(defun main (argv)
  (declare (ignore argv))
  "Produce a summary of current repo status."
  (let ((status (git-status "--porcelain" "-b")))
    (when (not (search "fatal" status))
      (let ((branch (get-branch status))
            (ahead-behind (get-ahead-behind status))
            (staged (get-smc status 94 "●" "\\n(A|M)"))
            (conflicts (get-smc status 91 "✖" "\\n(DD |AU |UD |UA |DU |AA |UU )"))
            (modified (get-smc status 91 "✚" "\\n( M | D |AM | T )"))
            (dirty (get-dirty status)))
        (format t
                "~a~%"
                (concatenate 'string
                             "("
                             branch
                             ahead-behind
                             "|"
                             (if (or staged conflicts modified dirty)
                                 (concatenate 'string
                                              staged
                                              conflicts
                                              modified
                                              dirty)
                                 (stylize 92 "✓" nil))
                             ")"))))))
