(defun search-all (substring string)
  "Count all occurences of SUBSTRING in STRING."
  (let ((counter 0)
        (position 0)
        (len (length substring)))
    (loop
       (let ((search (search substring string :start2 position)))
         (when (not search)
           (return (if (> counter 0) counter nil)))
         (setq position (+ len search))
         (setq counter (+ 1 counter))))))

(defun split-by-char (char string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defmacro stylize (color symbol string)
  "Wrapper around format to provide colors with shell escape sequences."
  (if (and symbol string)
      `(format nil "%{~C[~am%}~a~a%{~C[0m%}"  #\Esc ,color ,symbol ,string #\Esc)
      `(format nil "%{~C[~am%}~a%{~C[0m%}" #\Esc ,color (or ,symbol ,string) #\Esc)))

(defun get-ahead-behind (git-status)
  "Count pulls ahead or behind from git status STRING in current BRANCH."
  (when (not (search "up-to-date" git-status))
    (let ((info-line
           (split-by-char #\Space
                          (second (split-by-char #\Newline git-status)))))
      (let ((nums (loop
                     :for num :in info-line
                     :when (parse-integer num :junk-allowed t)
                     :collect (parse-integer num))))
        (cond ((search "have diverged" git-status)
               (concatenate 'string
                            (stylize 95 "↑" (first nums))
                            (stylize 95 "↓" (second nums))))
              ((search "is ahead" git-status)
               (stylize 95 "↑" (first nums)))
              ((search "is behind" git-status)
               (stylize 95 "↓" (first nums))))))))

(defun get-staged (git-status)
  (let ((start (search "Changes to be commited:" git-status)))
    (when start
      (let* ((end (or (search "Changes not staged for commit:" git-status)
                      (search "Untracked files:" git-status)))
             (substring (subseq git-status start end))
             (count (+ (or (search-all "modified:" substring) 0)
                       (or (search-all "new file:") 0))))
        (stylize 94 "●" count)))))

(defun get-unstaged (git-status)
  (let ((start (search "Changes not staged for commit" git-status)))
    (when start
      (let* ((end (or (search "Untracked files:" git-status)
                      (length git-status)))
             (substring (subseq git-status start end))
             (count (search-all "modified:" substring)))
        (stylize 91 "✚" count)))))

(defun main (argv)
  "Produce a summary of current repo status."
  (let* ((git-status (with-output-to-string (out)
                       (run-program "/usr/bin/git"
                                    '("status" "--long")
                                    :output out)))
         (branch (when (search "On branch" git-status)
                   (subseq git-status 10 (position #\linefeed git-status))))
         (untracked (when (search "Untracked files:" git-status)
                      "…"))
         (staged (get-staged git-status))
         (unstaged (get-unstaged git-status))
         (ahead-behind (get-ahead-behind git-status))
         (output ""))
    (when branch
      (setf output
            (concatenate 'string
                         "("
                         (stylize 92 nil branch)
                         (get-ahead-behind git-status)
                         "|"
                         (if (or staged unstaged untracked)
                             (concatenate 'string staged unstaged untracked)
                             (stylize 92 "✓" nil))
                         ")"))
      (format t "~a~%" output))))
