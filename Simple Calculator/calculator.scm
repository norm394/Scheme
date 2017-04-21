;---------------------------------------------------------------
; Function: add
;
; Returns the sum of the two parameters.
;---------------------------------------------------------------
(define (add val1 val2)
	
	(+ val1 val2)

)
;---------------------------------------------------------------
; Function: sub
;
; Returns the value of the first parameter subtracted by the 
; second parameter.
;---------------------------------------------------------------
(define (sub val1 val2)
	
	(- val1 val2)

)
;---------------------------------------------------------------
; Function: div
;
; Returns the value of the first parameter divided by the 
; second parameter. Returns false if a parameter is zero.
;---------------------------------------------------------------
(define (div val1 val2)

	(cond

		( (= val1 0)  #f            )
		( (= val2 0)  #f            )
		( else        (/ val1 val2) )

	)

)
;---------------------------------------------------------------
; Function: mul
;
; Returns the value of the first parameter multiplied by the 
; second parameter.
;---------------------------------------------------------------
(define (mul val1 val2)
	
	(* val1 val2)

)
;---------------------------------------------------------------
; Function: mod
;
; Returns the value of the first parameter modulo the 
; second parameter. Returns false if a parameter is zero.
;---------------------------------------------------------------
(define (mod val1 val2)

	(cond

		( (= val1 0)  #f                 )
		( (= val2 0)  #f                 )
		( else        (modulo val1 val2) )

	)

)
;---------------------------------------------------------------
; Function: askForOp
;
; Displys a prompt, returns user input.
;---------------------------------------------------------------
(define (askForOp)
		
	(display "Enter an operator: ")
	(read)
	
)
;---------------------------------------------------------------
; Function: askForVal
;
; Displys a prompt, returns user input.
;---------------------------------------------------------------
(define (askForVal)
		
	(display "Enter a numeric value: ")
	(read)
	
)
;---------------------------------------------------------------
; Function: getOp
;
; Continuously calls getOp function until the user enters a
; valid operator.
;---------------------------------------------------------------
(define (getOp)
	
	(let (
			( op (askForOp) )
		 )

		(cond

			( (EQV? op '+)  op )
			( (EQV? op '-)  op )
			( (EQV? op '/)  op )
			( (EQV? op '*)  op )
			( (EQV? op '%)  op )
			( 
				(display "Try again...\n")          
			  	(getOp) 
			)
		
		)

	)

)
;---------------------------------------------------------------
;Function: getVal
;
; Continuously calls getVal function until the user enters a
; valid numeric value.
;---------------------------------------------------------------
(define (getVal)
		
	(let (
			( val (askForVal) ) 
		 ) 

		(cond

			( (NUMBER? val)  val )
			( 
				(display "Try again...\n")          
			  	(getVal) 
			)
		
		)

	)
	
)
;---------------------------------------------------------------
;Function: excOp
;
; Middle man function. Calls and returns the value from the 
; function that matches the passed in operator.
;---------------------------------------------------------------
(define (excOp op val1 val2)

	(cond

		( (EQV? op '+)  (add val1 val2) )
		( (EQV? op '-)  (sub val1 val2) )
		( (EQV? op '/)  ( if 
							(not(false? (div val1 val2)) )
							
							(div val1 val2) 

							(begin
								(display "\nCannot divide by zero\n")
								(display "Restoring result to first value...\n\n")
								val1
							)  
							
						)
		)

		( (EQV? op '*)  (mul val1 val2) )
		( (EQV? op '%)  ( if 
							(not(false? (mod val1 val2)) )
							
							(div val1 val2) 

							(begin
								(display "\nCannot modulo by zero\n")
								(display "Restoring result to first value...\n\n")
								val1
							)  
							
						)
		)
	
	)
	
)
;---------------------------------------------------------------
; Function: handleResults
;
; Displays the results to the user, prompts the user for input:
;
;	yes  -  Do another calculation using the result as a value.
;	no   -  Exit the calculator.
;	new  -  Start a new calculation from scratch.
;---------------------------------------------------------------
(define (handleResults total)

	(display "Result: ")
	(display total)
	(newline)

	(display "Continue? (yes, no, new)")
	(newline)
	(display ">>: ")

	(let (
			( input (read) )
		 )


		(cond

			( (EQV? input 'yes)  (calc total) )
			( (EQV? input 'no)   "FINISHED"   )
			( (EQV? input 'new)  (calculator) )
			( (handleResults total)           )
			
		)

	)
	
)
;---------------------------------------------------------------
; Function: calculator
;
; Main function. Calls various helper functions to gether values
; and calculate the result. Passes result into the handleResult
; function.
;---------------------------------------------------------------
(define (calculator)

	(let (
			( val1  (getVal) ) 
			( op    (getOp)  )  
			( val2  (getVal) ) 
		 )

		(let ( 
				( total (excOp op val2 val1) )  
			 )

			(handleResults total)

		)

	)
	
)
;---------------------------------------------------------------
; Function: calc
;
; Similar to the calculator function, this function behaves
; exactly the same except the first value is passed in. 
;---------------------------------------------------------------
(define (calc val1)

	(let (
			
			( val2  (getVal) )
			( op    (getOp)  )   
		 )

		(let ( 
				( total (excOp op val1 val2) )  
			 )

			(handleResults total)

		)

	)
	
)

