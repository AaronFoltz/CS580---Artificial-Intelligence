(defun p1 ()
  (let (feature_names feature_values)
    
    (setf dat_file   (open "p1.dat" :direction :input)) ; Input file
    (setf *print_max* 20)
    
    (loop
        (setq problem_list (read dat_file nil nil))
        (if (null problem_list) (return))
        
        (format t "~%Processing: ~A" problem_list)

        (setf feature_list (cadr problem_list)) ; Get List of feature names/values
                
        ; Reset the feature_name and feature_value list
        (setf feature_names '() feature_values '())
        
        ; For each feature in the list
        (dolist (val feature_list)
        
            ; Add the next feature_name to the list.  In this case, the feature_name is the 
            ; car of the current list (val)
            (setf feature_names (cons (car val) feature_names)) 
            
            ; Add the next feature_values to the list.  In this case, the feature_values are
            ; the cadr of the current list (val)
            (setf feature_values (cons (cadr val) feature_values))
            )
            
            ; Reverse the lists.  Cons will put the lists together backwards
            (solve (reverse feature_names) (reverse feature_values))
        )
    (close dat_file)
    '***All_Done***
    )
  )

(defun solve (feature_names feature_values)
  (let ( (feature_count (length feature_names))
         (object_count (length (car feature_values)))
         (f1_values (car feature_values)) 
         (unused_symbols (cdr feature_values))
         puzzle_array
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
          )

    (gen_states puzzle_array (car unused_symbols) (cdr unused_symbols) 0 1)          ;generate the state space
    (format t "~%  Solution count: ~D, Node count: ~D~%" *soln_count* *node_count*) 
    )
  )
  ()

(defun gen_states (puzzle_array col_symbols unused_symbols feat_row feat_col)
    (let ()
      (cond  ( (null col_symbols) 
                    (cond  ( (null unused_symbols)   ; at a leaf node of the tree
                                ; Condition to see if we should continue printing solutions
                                (cond 
                                    ( (< *soln_count* *print_max*) 
                                        (print_solution puzzle_array))
                                    ( (= *soln_count* *print_max*)  
                                        (format t "~%       Suppressing printing ..."))
                                )
                                (setq *soln_count* (1+ *soln_count*))      
                           )
                           ( t     (gen_states puzzle_array 
                                    (car unused_symbols) (cdr unused_symbols) 0 (1+ feat_col)))
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
    (format t "~%     ~A" puzzle_array)
  )


