(defun p2soln ()
  (let (dat_file puzzle)
        (setf dat_file   (open "p3test.dat" :direction :input))   ; open file for input & save file ptr
        (loop
            (setf puzzle (read dat_file nil nil))    ; read next s-exp from file
            (if (null puzzle)	   (return))         ; return breaks out of loop
            (process puzzle)                        ; invoke function
            )
        (close dat_file)                             ; close file
 
       '***All_Done***
       )                                            ; end of LET scope
    )



(defun process (puzzle)
  (let ( (feature_list (second puzzle))
         (constraint_list (third puzzle))
         feat_names
         feat_vals
         puzzle_error
         (feat_col 0)
         )

    (format t "~%Puzzle name: ~A" (first puzzle))

    (cond ( (eq (first feature_list) 'features)
            (setf *feature_list* (cdr feature_list))
            (dolist (item *feature_list*)     ; parse feature names and values
              (setf feat_names (cons (first item) feat_names))
              ;(set (first feat_names) feat_col)
              (set (first feat_names) (list feat_col (second item)))
              (setf feat_col (1+ feat_col))
              (setf feat_vals  (cons (second item) feat_vals))
              )
            (setf feat_names (reverse feat_names)
                  feat_vals  (reverse feat_vals)
                  )

            (cond ( (eq (first constraint_list) 'constraints)
                    (process_constraints (cdr constraint_list))
                    (solve feat_names feat_vals)
                    )
                  ( t   (format t "~%  *** Missing constraint list ***") )
                  )
            )
          ( t   (format t "~%  *** Missing feature list ***") )
          )
    )
  )


(defun process_constraints (constraint_list)
  (setf *constraints* nil)
  (format t "~% Constraints:")
  (dolist (constraint constraint_list)
    (cond ( (eq (first constraint) 'iffn)
            (setf *constraints* (cons constraint *constraints*))
            (format t "~%   ~A" constraint)
            )
          ( t   (format t "~%   *** Illegal constraint ignored: ~A" constraint))
          )
    )
  )


(defun solve (feature_names feature_values)
  (let* ( (feature_count (length feature_names))
         (object_count (length (car feature_values)))
         (f1_values (car feature_values)) 
         (unused_symbols (cdr feature_values))
         (puzzle_array (make-array  (list object_count feature_count)))
         )

    (format t "~% Solving a ~D object ~D feature problem:" object_count feature_count )
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
                      ( t
                        (gen_states puzzle_array (car unused_symbols) (cdr unused_symbols) 0 (1+ feat_col))
                        )
                      )
                )
             ( t
               (dolist (fval col_symbols)       ; fill in a slot, check constraints & recurse
                 (setf (aref puzzle_array feat_row feat_col) fval)
                 (cond ( (legal puzzle_array feat_row feat_col)
                         (setf *node_count* (1+ *node_count*))
                         (gen_states puzzle_array (remove fval col_symbols) unused_symbols (1+ feat_row) feat_col)
                         )
                       )
                 )
               (setf (aref puzzle_array feat_row feat_col) nil)   ; clean up
               )
             )
      )
    )


(defun legal (puzzle_array feat_row feat_col)
    (dolist (item *constraints* t)
      (if (check_iffn_match puzzle_array (rest item) feat_row feat_col)  (return nil))
      )
    )


(defun check_iffn_match (puzzle_array iffn_stuff feat_row feat_col)
  (let* ( (clause1 (first iffn_stuff))
          (clause2 (second iffn_stuff))
          (clause1_feat_col (first (eval (first clause1))))
          (clause2_feat_col (first (eval (first clause2))))
          (match nil)
          )

    (cond ( (eq clause1_feat_col feat_col)
            (cond ( (member (aref puzzle_array feat_row feat_col) (rest clause1))
                    (setf match (member (aref puzzle_array feat_row clause2_feat_col) (rest clause2)))
                    )
                  )
            )
          ( (eq clause2_feat_col feat_col)
            (cond ( (member (aref puzzle_array feat_row feat_col) (rest clause2))
                    (setf match (member (aref puzzle_array feat_row clause1_feat_col) (rest clause1)))
                    )
                  )
            )
         )
    match
    )
  )


(defun print_solution (puzzle_array)
    (cond (  (< *soln_count* *print_max*)  (format t "~%     ~A" puzzle_array) )
          (  (= *soln_count* *print_max*)  (format t "~%       ***printing suppressed***") )
          )
    )


