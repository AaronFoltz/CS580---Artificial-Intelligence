(defmacro head (x)
  (list 'car x)
  )

(defmacro tail (x)
  (list 'cdr x)
  )

(defmacro iffn (x y)
  (list 'check_iffn `(quote ,x) `(quote ,y) 'puzzle_array 'row 'col)
  )

(defun p3soln ()
  (let (dat_file_name dat_file puzzle)

	(loop 
            (format t "Enter puzzle file name: ")
            (setf dat_file_name  (read-line))
            (if (string-equal dat_file_name "") (return))

            (setf dat_file   (open (concatenate 'string "~/kdj/CS580/HW3/" dat_file_name)))   ; open file for input & save file ptr
            (loop
	        (setf puzzle (read dat_file nil nil))    ; read next s-exp from file
	        (if (null puzzle)	   (return))         ; return breaks out of loop
                (process puzzle)                        ; invoke function
                )
            (close dat_file)                             ; close file
            )
 
       '***All_Done***
       )                                            ; end of LET scope
    )


(defun process (puzzle)
  (let ( (feature_list (second puzzle))
         (constraint_list (third puzzle))
         feat_names
         feat_vals
         (feat_col 0)
         )

    (format t "~%Puzzle: ~A" (first puzzle))

    (cond ( (eq (first feature_list) 'features)
            (setf *feature_list* (tail feature_list))
            (dolist (item *feature_list*)     ; parse feature names and values
              (setf feat_names (cons (first item) feat_names))
              (set (first feat_names) feat_col)
              (setf feat_col (1+ feat_col))
              (setf feat_vals  (cons (second item) feat_vals))
              )
            (setf feat_names (reverse feat_names)
                  feat_vals  (reverse feat_vals)
                  )

            (cond ( (eq (first constraint_list) 'constraints)
                    (process-constraints (tail constraint_list))
                    (solve feat_names feat_vals)
                    )
                ( t   (format t "~%  *** Missing constraint list ***") )
                )
            )
          ( t   (format t "~%  *** Missing feature list ***") )
          )
    )
  )


(defun process-constraints (constraint_list)
  (let ( (constraints nil)
         (legal_defun nil)
         )

;    (format t "~%  Constraints:")

    (dolist (constraint constraint_list)
;      (format t "~%    ~A" constraint)
      (setf constraint_type (first constraint))
      (cond ( (eq constraint_type 'iffn)
	       (setf constraints (cons constraint constraints))
	       )

            ( (eq constraint_type 'iff)
              (setf constraints (cons (convert_iff constraint) constraints))
              )

            ( t   (format t "~% *** Illegal constraint ignored: ~A" constraint))
            )
      )
    
    (dolist (constraint constraints)
      (if (eq (first constraint) 'iffn)
          (setf legal_defun (cons (macroexpand-1 constraint) legal_defun))
          )
      )

    (setf legal_defun (list 'defun 'legal '(puzzle_array row col) (cons 'and legal_defun)))
    (format t "~%Legal_defun: ~A" legal_defun)
    (eval legal_defun)
    )
  )


(defun convert_iff (constraint)
  (let ( (lhs (second constraint))
	 (rhs (third constraint))
         )
               
    (cond ( (= (length lhs) 2)     ;convert to IFFN via rhs complement
            (list 'iffn 
                  lhs 
                  (cons (first rhs)
                        (set-difference (second (assoc (first rhs) *feature_list*)) (tail rhs))
                        )
                  )
            )

          ( (= (length rhs) 2)     ;convert to IFFN via lhs complement
            (list 'iffn 
                  (cons (first lhs)
                        (set-difference (second (assoc (first lhs) *feature_list*)) (tail lhs))
                        )
                  rhs
                  )
            )
                       
          ( t   (format t "~% *** Illegal IFF constraint ignored: ~A" constraint))
          )
    )
  )


(defun solve (feature_names feature_values)
  (let* ( (feature_count (length feature_names))
          (f1_values (first feature_values)) 
          (unused_symbols (tail feature_values))
          (object_count (length f1_values))
          (puzzle_array (make-array  (list object_count feature_count)))
          )

    (format t "~%  Puzzle size: ~D objects and ~D features" object_count feature_count )
    (format t "~%  Feature names: ~A" feature_names)
    (format t "~%  Feature values: ~A" feature_values)
    (format t "~%  Solutions:")

    (dotimes (x object_count)      ; bind first feature values as part of the initial state
      (setf (aref puzzle_array x 0) (nth x f1_values)) )

    (setf *soln_count* 0
          *node_count* 0
          *print_max* 10
          )

    (gen_states puzzle_array (head unused_symbols) (tail unused_symbols) 0 1)          ;generate the state space
    (format t "~%  Solution count: ~D, Node count: ~D~%" *soln_count* *node_count*) 
    )
  )


(defun gen_states (puzzle_array col_symbols unused_symbols row col)
    (let ()
      (cond  ( (null col_symbols) 
               (cond  ( (null unused_symbols)   ; at a leaf node of the tree
                        (setf *soln_count* (1+ *soln_count*)) 
                        (print_solution puzzle_array)
                        )
                      ( t
                        (gen_states puzzle_array (head unused_symbols) (tail unused_symbols) 0 (1+ col))
                        )
                      )
                )
             ( t
               (dolist (fval col_symbols)       ; fill in a slot, check constraints & recurse
                 (setf (aref puzzle_array row col) fval)
                 (cond ( (legal puzzle_array row col)
                         (setf *node_count* (1+ *node_count*))
                         (gen_states puzzle_array (remove fval col_symbols) unused_symbols (1+ row) col)
                         )
                       )
                 )
               (setf (aref puzzle_array row col) nil)   ; clean up
               )
             )
      )
    )


(defun check_iffn (clause1 clause2 puzzle_array feat_row feat_col)
  (let (  (clause1_feat_col (eval (first clause1)))
          (clause2_feat_col (eval (first clause2)))
          (legal t)
          )

    (cond ( (eq clause1_feat_col feat_col)
            (cond ( (member (aref puzzle_array feat_row feat_col) (rest clause1))
                    (setf legal (not  (member (aref puzzle_array feat_row clause2_feat_col) (rest clause2))))
                    )
                  )
            )
          ( (eq clause2_feat_col feat_col)
            (cond ( (member (aref puzzle_array feat_row feat_col) (rest clause2))
                    (setf legal (not (member (aref puzzle_array feat_row clause1_feat_col) (rest clause1))))
                    )
                  )
            )
         )
    legal
    )
  )


(defun print_solution (puzzle_array)
    (cond (  (< *soln_count* *print_max*)  (format t "~%     ~A" puzzle_array) )
          (  (= *soln_count* *print_max*)  (format t "~%       ***printing suppressed***") )
          )
    )


