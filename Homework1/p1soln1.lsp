(defun p1soln ()
  (let (dat_file puzzle)
 	(setf dat_file   (open "p1soln.dat" :direction :input))   ; open file for input & save file ptr

	(loop 
	    (setf puzzle (read dat_file nil nil))    ; read next s-exp from file
	    (if (null puzzle)	   (return))         ; return breaks out of loop
	    (process puzzle)                        ; invoke function
	    )

	(close dat_file)                             ; close file
	)                                            ; end of LET scope

    '***All_Done***
    )

(defun process (puzzle)
  (let (feat_names feat_vals)
    (format t "~%Processing: ~A~%" puzzle)
    (dolist (item (car (cdr puzzle)))
      (setf feat_names (cons (car item) feat_names))
      (setf feat_vals  (cons (car (cdr item)) feat_vals))
      )
    (setf feat_names (reverse feat_names))
    (setf feat_vals  (reverse feat_vals))
    (solve feat_names feat_vals)
    )
  )

(defun solve (feature_names feature_values)
  (let ( (feature_count (length feature_names))
         (object_count (length (car feature_values)))
         (f1_values (car feature_values)) 
         (unused_symbols (cdr feature_values))
         )

    (setf puzzle_array (make-array  (list object_count feature_count)))

    (format t "~%Solving a ~D object ~D feature problem:" object_count feature_count )
    (format t "~%  Feature names: ~A" feature_names)
    (format t "~%  Feature values: ~A" feature_values)
    (format t "~%  Solutions:")

    (dotimes (x object_count)      ; bind first feature values as part of the initial state
      (setf (aref puzzle_array x 0) (nth x f1_values)) )

    (setf *soln_count* 0
          *node_count* 0
          *print_max* 20
          )

    (gen_states puzzle_array (car unused_symbols) (cdr unused_symbols) 0 1)          ;generate the state space
    (format t "~%  Solution count: ~D, Node count: ~D~%" *soln_count* *node_count*) 
    )
  )


(defun gen_states (puzzle_array col_symbols unused_symbols feat_row feat_col)
    (let ()
      (cond  ( (null col_symbols) 
                    (cond  ( (null unused_symbols)   ; at a leaf node of the tree
                                   (setq *soln_count* (1+ *soln_count*)) 
                                   (print_solution puzzle_array)
                                   )
                           ( t     (gen_states puzzle_array (car unused_symbols) (cdr unused_symbols) 0 (1+ feat_col)))
                           )
                    )
             ( t    (dolist (fval col_symbols)       ; fill in a slot & recurse
                      (setf (aref puzzle_array feat_row feat_col) fval
                            *node_count* (1+ *node_count*)
                            )
                      (gen_states puzzle_array (remove fval col_symbols) unused_symbols (1+ feat_row) feat_col))
                    )
             )
      )
    )
 

(defun print_solution (puzzle_array)
    (cond (  (< *soln_count* *print_max*)  (format t "~%     ~A" puzzle_array) )
          (  (= *soln_count* *print_max*)  (format t "~%       ***printing suppressed***") )
          )
    )


