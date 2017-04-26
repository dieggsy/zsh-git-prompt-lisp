(defun git-status (&rest args)
  (with-output-to-string (out)
    (run-program "/usr/bin/git"
                 (concatenate 'list '("status") args)
                 :output out)))

(defun search-all (substring string)
  "Find all occurences of SUBSTRING in STRING"
  (loop
    :for i := 0 then (1+ j)
    :for counter := 0 then (1+ counter)
    :as j := (search substring string :start2 i)
    :when (not j)
      :return counter))

(defmacro stylize (color symbol string)
  "Wrapper around format to provide colors with shell escape sequences."
  (if (and symbol string)
      `(format nil "%{~C[~am%}~a~a%{~C[0m%}"  #\Esc ,color ,symbol ,string #\Esc)
      `(format nil "%{~C[~am%}~a%{~C[0m%}" #\Esc ,color (or ,symbol ,string) #\Esc)))

(defun get-ahead-behind (git-status)
  (let* ((first-line (subseq git-status 0 (position #\newline git-status)))
         (ahead (search "ahead" first-line))
         (behind (search "behind" first-line)))
    (cond ((and ahead behind)
           (concatenate 'string
                        (stylize 95 "↑" (parse-integer
                                         (subseq first-line
                                                 (+ 6 ahead)
                                                 (search "," first-line))))
                        (stylize 95 "↓" (parse-integer
                                         (subseq first-line
                                                 (+ 7 behind)
                                                 (search "]" first-line))))))
          (ahead
           (stylize 95 "↑" (parse-integer
                            (subseq first-line
                                    (+ 6 ahead)
                                    (search "]" first-line)))))
          (behind
           (stylize 95 "↓" (parse-integer
                            (subseq first-line
                                    (+ 7 behind)
                                    (search "]" first-line))))))))

(defun get-branch (git-status)
  (if (not (search "no branch" git-status))
      (stylize 92 nil (subseq git-status
                              (+ (search "## " git-status) 3)
                              (search "..." git-status)))

      (let ((commit (with-output-to-string (out)
                      (run-program "/usr/bin/git"
                                   '("rev-parse" "--short" "HEAD")
                                   :output out))))
        (stylize 92 nil (subseq commit 0 (1- (length commit)))))))

(defun get-staged (git-status)
  (let ((num (+ (search-all "A  " git-status)
                (search-all "M  " git-status))))
    (when (/= num 0)
      (stylize 94 "●" num))))

(defun get-modified (git-status)
  (let ((num (+ (search-all " M " git-status)
                (search-all "AM " git-status)
                (search-all " T " git-status))))
    (when (/= num 0)
      (stylize 91 "✚" num))))

(defun get-dirty (git-status)
  (when (search "?? " git-status)
    "…"))

(defun main (argv)
  "Produce a summary of current repo status."
  (let ((status (git-status "--porcelain" "-b")))
    (when (not (search "fatal" status))
      (let ((branch (get-branch status))
            (ahead-behind (get-ahead-behind status))
            (staged (get-staged status))
            (modified (get-modified status))
            (dirty (get-dirty status)))
        (format t
                "~a~%"
                (concatenate 'string
                             "("
                             branch
                             ahead-behind
                             "|"
                             (if (or staged modified dirty)
                                 (concatenate 'string
                                              staged
                                              modified
                                              dirty)
                                 (stylize 92 "✓" nil))
                             ")"))))))
