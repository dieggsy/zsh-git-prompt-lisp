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

(defun nums-in-string (string)
  "Parse all numbers from STRING.

Kinda hackish - no commas/periods allowed."
  (with-input-from-string (s string)
    (loop
       :for num := (read s nil nil)
       :while num
       :when (integerp num)
       :collect num)))

(defmacro stylize (color symbol string)
  "Wrapper around format to provide colors with shell escape sequences."
  (if (and symbol string)
      `(format nil "%{~C[~am%}~a~a%{~C[0m%}"  #\Esc ,color ,symbol ,string #\Esc)
      `(format nil "%{~C[~am%}~a%{~C[0m%}" #\Esc ,color (or ,symbol ,string) #\Esc)))

(defun get-ahead-behind (string branch)
  "Count pulls ahead or behind from git status STRING in current BRANCH."
  (let* ((branch-first (search branch string))
         (branch-second (search branch string :from-end t))
         (string (subseq string
                         (+ branch-first (length branch))
                         (- branch-second (length branch))))
         (start (position-if #'digit-char-p string))
         (end (position-if #'digit-char-p string :from-end t))
         (ahead-behind (when (or start end)
                         (nums-in-string
                          (subseq string (1- start) (1+ end))))))
    (when ahead-behind
      (cond ((search "have diverged" string)
             (format nil "%{~C[95m%}↑~d↓~d%{~C[0m%}"
                     #\Esc (first ahead-behind) (second ahead-behind) #\Esc))
            ((search "is behind" string)
             (stylize 95 "↓" (first ahead-behind)))
            ((search "is ahead" string)
             (stylize 95 "↑" (first ahead-behind)))
            (t "")))))

(defun main (argv)
  "Produce a summary of current repo status."
  (let* ((status-info (with-output-to-string (out)
                        (run-program "/usr/bin/git"
                                     '("status" "--long")
                                     :output out)))
         (branch (when (search "On branch" status-info)
                   (subseq status-info 10
                           (search (format nil "~C" #\linefeed) status-info))))
         (len (length status-info))
         (untracked (search "Untracked files:" status-info))
         (m-ind (search "Changes not staged for commit:" status-info))
         (s-ind (search "Changes to be committed:" status-info))
         (m-str (when m-ind (subseq status-info m-ind len)))
         (modified (when m-str (search-all "modified:" m-str)))
         (s-str (when s-ind (subseq status-info 0 (or m-ind len))))
         (staged (when s-str
                   (or (search-all "modified:" s-str)
                       (search-all "new file:" s-str))))
         (output ""))
    (when branch
      (setq output
            (concatenate 'string
                         "("
                         (stylize 92 nil branch)
                         (get-ahead-behind status-info branch)
                         "|"
                         output))
      (if (not (or staged modified untracked))
          (setq output (concatenate 'string output (stylize 92 "✓" nil)))
          (progn
            (when staged
              (setq output
                    (concatenate 'string output (stylize 94 "●" staged))))
            (when modified
              (setq output
                    (concatenate 'string output (stylize 91 "✚" modified))))
            (when untracked
              (setq output (concatenate 'string output "…")))))
      (format t (concatenate 'string output ")~%") #\Esc))))
