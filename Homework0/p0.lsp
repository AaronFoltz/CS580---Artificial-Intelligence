;
; p0.lsp:    A Simple Permutation Generator
;
;      - reads lists of symbols from p0.dat
;      - produces all permutations of each symbol set
;      - invoked via:  (p0)
;


;
; Top level control loop:
;
(defun p0 ()          
    (let (dat_file sym_list)
	(setf dat_file   (open "p0.dat" :direction :input))
	(setf *print_max* 30)     ; not declared in a LET => global variable

	(loop 
	    (setq sym_list (read dat_file nil nil))
	    (if (null sym_list)	   (return))
	    (gen_perm sym_list)
	    )

	(close dat_file)
	'***ALL_DONE***
    )
)

(defun gen_perm (sym_list)
    (format t "~%Generating permutations of: ~A~%" sym_list)
    (setf *perm_cnt* 0)        ; another global variable
    (permute sym_list '())
    (format t "~%  Permutation count: ~D~%~%" *perm_cnt*)
    )

;
; permute is invoked with two args
;        (unused symbols) (partial permutation)
;
; If the unused symbol list is null, the recursion has bottomed out, and
; the partial permutation is a complete one (a leaf node) ready to be
; counted and printed.
;
; If there are unused symbols, each in turn added to the front of the partial
; permutation, creating a child node, and permute is recursively invoked
; to generate the grandchildren.

(defun permute (unused_syms partial_perm)
   (cond
        ( (null unused_syms)               ; bottomed out at a leaf node
	     (cond  
	          ( (< *perm_cnt* *print_max*)   (print partial_perm) )
		  ( (= *perm_cnt* *print_max*)   
			  (format t "~%       suppressing printing ...") )
		  )
	     (setf *perm_cnt* (+ *perm_cnt* 1))
	     )

	( t                           ; generate children
	     (dolist (item unused_syms)
		 (permute  (remove item unused_syms) (cons item partial_perm))
		 )
	     )
	)
   )

;
; execute it?
;

;(p0)




