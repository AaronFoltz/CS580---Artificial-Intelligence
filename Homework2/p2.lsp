

(defun p2 ()
  (let (dat_file puzzle)
 	(setf dat_file   (open "p2soln.dat" :direction :input))   ; open file for input & save file ptr

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
  (let (feat_names feat_vals constraints (feat_count 0))
    (format t "~%Processing: ~A~%" puzzle)
    
    ; Get the feature list, with cdr = "FEATURES"
    (setf feat_list (cdadr puzzle))
    ; Parse out the different features
    (dolist (feat feat_list)
        
        ; Give the feature_name its column
        (set (car feat) feat_count)
        ;increment feat_count to another column
        (incf feat_count)
        
        (setf feat_names (cons (car feat) feat_names))
        (setf feat_vals  (cons (car (cdr feat)) feat_vals))
      )
      
    (setf feat_names (reverse feat_names))
    (setf feat_vals  (reverse feat_vals))
    
    #|
        TODO Think about how to represent the constraints better (use an iffn function?)
    |#
    ; Just bring in the constraint list as a list of constraints
    (setf constraint_list (cdaddr puzzle))
    
    (solve feat_names feat_vals)
    )
  )

(defun solve (feature_names feature_values)
  (let ( (feature_count (length feature_names))
         (object_count (length (car feature_values)))
         (f1_values (car feature_values)) 
         (unused_symbols (cdr feature_values))
         )
    
    ; Create the two dimensional array objectsxfeatures
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
    (let (valid)
      (cond  ( (null col_symbols) 
                    (cond  ( (null unused_symbols)   ; at a leaf node of the tree
                                   (setq *soln_count* (1+ *soln_count*)) 
                                   (print_solution puzzle_array)
                                   )
                           ( t     (gen_states puzzle_array (car unused_symbols) (cdr unused_symbols) 0 (1+ feat_col)))
                           )
                    )
             ( t    (dolist (fval col_symbols)       ; fill in a slot & recurse
             
                      ; Fill in the element if it does not violate any constraints
                      (setf valid (check_constraints puzzle_array feat_row feat_col fval))
                      
                      ; If the filled in element violated a constraint, we will just return
                      ; If it did not violate a constraint, keep recursing
                      (cond
                          ; Invalid node
                          ((= valid 0)
                            ;(print "Invalid Node")
                          )
                          ; Valid Node
                          ((= valid 1)  
                              ; We have at least visited the node if we have made it here.
                              (setf *node_count* (1+ *node_count*))
                              (gen_states puzzle_array (remove fval col_symbols) unused_symbols (1+ feat_row) feat_col))
                      )
                    )                      
             )
      )
    )
)

(defun check_constraints (puzzle_array feat_row feat_col val)    
    
    
    ; Iterate over the constraint list for this puzzle
    (dolist (constraint constraint_list)
        
        (cond 
            ; If the current column symbol is in the "IF" part of the constraint
            ((member val (rest (second constraint)))
            
                ; If the row we are currently on violates the constraint
                (if (member (aref puzzle_array feat_row (eval (first (third constraint)))) (rest (third constraint)))
                    (progn
                        ;; Set the solution to be invalid
                        ;(print "Invalid")
                        ;(print puzzle_array)
                        (return-from check_constraints 0)
                        )
                )
            )
            ; ; If the current column symbol is in the "THEN" part of the constraint
            ((member val (rest (third constraint)))
                ; If the row we are currently on violates the constraint
                (if (member (aref puzzle_array feat_row (eval (first (second constraint)))) (rest (second constraint)))
                    (progn
                        ;; Set the solution to be invalid
                        ;(print "Invalid")
                        ;(print puzzle_array)
                        (return-from check_constraints 0)
                        )
                )
            
            )
            
        )
    )
    
    ; Add the element into the array if we didn't violate any constraints
    (setf (aref puzzle_array feat_row feat_col) val)
    (return-from check_constraints 1)    
) 

(defun print_solution (puzzle_array)
    (cond (  (< *soln_count* *print_max*)  (format t "~%     ~A" puzzle_array) )
          (  (= *soln_count* *print_max*)  (format t "~%       ***printing suppressed***") )
          )
    )


