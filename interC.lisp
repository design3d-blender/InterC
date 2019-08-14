;;choclo de parametros para que no moleste al compilar todo el archivo
(defparameter *variables* (list 'int))
(defparameter *stackvar* nil)
(defparameter *results* nil)
(defparameter *scan_value* nil)
(defparameter *constants* (list 'def))
(defparameter *funciones* nil)
(defvar *condicionales* (list '== '< '> '<= '>=))
(defvar *separators* (list '+ '- '* '/))
(defvar strcond)
 
(defun remove-brackets (lst)
  "Reduses lists with just one item to the item itself"
  (do ((result lst (car result)))
      ((or (not (consp result))
	   (not (null (cdr result)))) result)))

(defun separate-list (lst separator test)
  "Returns list of sub-sequences defined by separator"
  (if (not (consp lst))
      lst
      (let ((result (cons separator nil)) (end 0) (sub)
	    (lst (if (funcall test (car lst) separator)
		     (cdr lst)
		     lst)))
	(do () ((null lst) result)
	  (setf end 
		(position separator lst :test test))
	  (setf sub
		(cons (subseq lst 0 end) nil))
	  (setf result 
		(append result sub))
	  (setf lst 
		(if end 
		    (nthcdr (+ 1 end) lst)
		    nil)))
	(setf (cdr result) (mapcar #'remove-brackets (cdr result)))
	result)))

(defun separate-tree (lst separator test)
  "Apply separate-list on all sublists"
  (if (or (not (consp lst)) (eql (first lst) 'quote))
      lst
      (progn
	(setf lst (mapcar #'(lambda (x) 
			      (if (not (consp x))
				  x
				  (separate-tree x separator test)))
			  lst))
	(if (not (find separator (rest lst)))
	    lst
	    (separate-list lst separator test)))))

(defun infix->prefix (infix-expr separators &key (test #'eql))
  "Converts an infix expression to prefix"
  (let ((result infix-expr))
    (dolist (sep separators)
      (setf result (separate-tree result sep test)))
    (remove-brackets result)))

(defun insert-between (lst sep)
  (if (or (not (consp lst))
	  (not (rest lst)))
      lst
    (cons (first lst) (mapcan #'(lambda (x) (list sep x)) (rest lst)))))

(defun prefix->infix (prefix-expr separators &key (test #'eql))
  "Converts a prefix expression to infix"
  (let ((in-expr (mapcar #'(lambda (x)
			     (remove-brackets (if (listp x)
					      (prefix->infix x separators)
					    x)))
			 prefix-expr)))
    (if (or (not (listp in-expr))
	    (not (member (first in-expr) separators :test test)))
	in-expr
      (insert-between (rest in-expr) (first in-expr)))))


;;;; End of infix prefix conversion

;;Inicia interprete


(defun pertVar(atom listom) ;;Funcion para buscar si la variable fue asignada y devuelvo el valor 
  (if (null listom)
      nil
      (if (eq atom (car listom))
	  (if (eq (cadr listom) '=)
	      (caddr listom)
		'NO_VALUE)
	  (pertVar atom (cdr listom)))
  )
  )

(defun pertfun(atom listom) ;;Verificar si la funcion existe
  (if (null listom)
      nil
      (if (eq atom (car listom))
	  (list (car listom) (cadr listom) (caddr listom))
	  (pertVar atom (cdddr listom)))
  )
  )

(defun pert(atom listom) ;;Pert comun y corriente
  (if (null listom)
      nil
      (if (eq atom (car listom))
	  t
	  (pert atom (cdr listom)))
  )
  )


(defun pert_sec(list secuencia) ;;Pert, pero busca si en la lista hay algun elemento de la secuencia
  (if (null list)
      nil
      (if (pert (car list) secuencia)
	  t
	  (pert_sec (cdr list) secuencia))
  )
  )


;;Funcion para resolver operaciones

(defun buildOperation(operation variables signos) ;; reemplaza las variables por su valor.
  (cond  ((null operation) nil)
	 ((listp (car operation)) (cons (buildoperation (car operation) variables signos) (buildoperation (cdr operation) variables signos)))
	 ((numberp (car operation)) (cons (car operation) (buildOperation (cdr operation) variables signos)))
	 ((pert (car operation) signos) (cons (car operation) (buildOperation (cdr operation) variables signos)))
	 (t (cond  ((and (null (pertVar (car operation) *constants*)) (null (pertVar (car operation) variables))) (error 'VARIABLE_NO_DEFINIDA))
		   ((eq (pertVar (car operation) variables) 'NO_VALUE) (error 'VARIABLE_SIN_VALOR))
		   ((pert (car operation) *constants*) (cons (pertvar (car operation) *constants*)(buildoperation (cdr operation) variables signos)))
		   (t (cons (pertVar (car operation) variables) (buildOperation (cdr operation) variables signos))))
	 )))

;;(buildoperation '(10 * c + 3) *variables* *separators*)

(defun solveOperation(operation);; Pasa la expresion a notacion prefija y la evalua.
  (eval (infix->prefix (buildoperation operation *variables* *separators*) *separators*))
  )

;;(solveoperation '((10 + z) * (12 - c)))
;;(buildoperation '((10 + c) * (2 - 5) - 1) *variables* *separators*)


(defun asignar_valor(name value variables);;Modifica los valores de las variables almacenadas
  (cond ((null variables) nil)
	((null (pert name variables)) (append variables (list name '= value)))
	((eq (car variables) name) (if (eq (cadr variables) '=)
				       (append (cons (car variables) (list '= value)) (cdddr variables))
				       (append (cons (car variables) (list '= value)) (cdr variables))))
	(t (cons (car variables) (asignar_valor name value (cdr variables))))))

;;(asignar_valor 'b '3 *variables*)

(defun is_value(intline) ;;Verifica si luego de la igualdad hay un valor
  (and (null (cdr intline)) (numberp (car intline))))

(defun is_variable(intline) ;;Same pero variable
  (and (null (cdr intline)) (symbolp (car intline))))

(defun is_operation(intline) ;;Also same pero operacion
  (not (null (cdr intline))))

(defun process_intline(intline) ;;Recibe una linea tipo (a = (algo)) y la procesa
  (cond ((is_value (cddr intline)) (defparameter *variables* (asignar_valor (car intline) (caddr intline) *variables*)))
	((is_variable (cddr intline)) (defparameter *variables* (asignar_valor (car intline) (pertvar (caddr intline) *variables*) *variables*)))
	((is_operation (cddr intline)) (defparameter *variables* (asignar_valor (car intline) (solveoperation (cddr intline)) *variables*)))
	(t (error 'ERROR_DE_SINTAXIS))))

(defun send_intline(asignacion) ;;Verifica sintaxis y manda la linea de asignacion tipo (a = (algo))
  (cond ((and (null (cadr asignacion)) (symbolp (car asignacion))) (defparameter *variables* (append *variables* asignacion)))
	((not (symbolp (car asignacion))) (error 'ERROR_DE_SINTAXIS))
	((not (eq (cadr asignacion) '=)) (error 'ERROR_DE_SINTAXIS))
	(t (process_intline asignacion))))

(defun process_int(intcode) ;;Procesa multiples asignaciones despues de un INT
  (dolist (intline intcode 'SUCCESS) (send_intline intline)))

(defun add_result(result) ;;Agrega outputs del programa
  (defparameter *results* (append *results* (list result)))
  )

(defun print_result(results) ;;Imprimila salida del programa
  (list results)
  )

(defun add_funcion(name variables code) ;;Agrega la funcion
  (defparameter *funciones* (append *funciones* (list name variables code)))
  )

(defun scan(name) ;;scanf que asigna y lee la entrada del teclado
  (defparameter *scan_value* (read))
  (process_intline (list name '= *scan_value*))
  )

(defun split_cond (x) ;;Separa la primer operacion y el condicional del resto ((operacion) < (resto de cosas))
  (cond ((null x) x)
        ((pert (car x) *condicionales*) (list nil (if (eq (car x) '==)
						  'eq
						  (car x)) (cdr x)))
        (t (let ((l (split_cond (cdr x))))
             (push (car x) (car l))
             l))))

(defun test_cond_sintax(condicion) ;;Se fija que lo que resta de tambien sea o no una condicion
  (if (pert_sec condicion *condicionales*)
      (split_cond condicion)
      condicion)
  )

(defun build_cond_sintax(condicion) ;;Arma recursivamente todas las condiciones
  (if (not (pert_sec condicion *condicionales*))
      (list condicion)  
      (append (subseq (split_cond condicion) 0 2) (build_cond_sintax (caddr (test_cond_sintax condicion))))))

(defun build_cond(condicion) ;;Pasa de formato C a formato lisp las condiciones
  (defvar strcond)
  (setq strcond (build_cond_sintax condicion))
  (cond ((not (null (nthcdr 5 strcond))) (error 'ERROR_DE_SINTAXIS))
	((null (nthcdr 3 strcond)) (list 'and (list (cadr strcond) (solveoperation (car strcond)) (solveoperation (caddr strcond)))))
	((not (pert condicion '==)) (list 'and (list (cadr strcond) (solveoperation (car strcond)) (solveoperation (caddr strcond))) (list (cadddr strcond) (solveoperation (caddr strcond)) (solveoperation (car (nthcdr 4 strcond))))))
	(t (error 'ERROR_DE_SINTAXIS))
  ))

(defun solve_cond(condicion) ;;Resuelve la condicion ya en formato lisp
  (eval (build_cond condicion)))

(defun runfun(funcion varvalues)
  (add_stackvar)
  (defparameter *variables* (list 'int))
  (if (null varvalues)
      nil
      (send_all_variables (name_variables (cadr funcion)) (value_variables varvalues)))
  (runc (caddr funcion))
  (restore_var)
  )

(defun add_stackvar() ;;Agrega las variables actuales a un "stack" de variales
  (if (null *stackvar*)
      (defparameter *stackvar* (list *variables*))
      (defparameter *stackvar* (append  *stackvar* (list *variables*)))
  ))

(defun restore_var() ;;Restaura las ultimas variables usadas antes de entrar a una funcion
  (defparameter *variables* (car (last *stackvar*))))

(defun name_variables(varnames) ;;Arma la lista de nombres de las variables
  (if (null (car varnames))
      nil
      (cons (cadar varnames) (name_variables (cdr varnames))))
  )

(defun value_variables(varvalues);;Arma la lista de los valores de las variables
  (if (null (car varvalues))
      nil
      (cons (if (atom (car varvalues))
		(solveoperation (list (car varvalues)))
		(solveoperation (car varvalues)))
	    (value_variables (cdr varvalues))))
  )

(defun new_variables (name value) ;;Arma una linea tipo INT para procesar
  (send_intline (list  name '= value))
  )

(defun send_all_variables(names values);;Asigna todas las variables d ela funcion
  (if (eq (length names) (length values))
      (mapcar #'new_variables names values)
      (error 'CANTIDAD_DE_VARIABLES_INCORRECTA)))

(defun run(code) ;;Defino los parametros que voy a usar y corro el programa
  (defparameter *variables* (list 'int))
  (defparameter *stackvar* nil)
  (defparameter *results* nil)
  (defparameter *scan_value* nil)
  (defparameter *constants* (list 'def))
  (defparameter *funciones* nil)
  (defvar *condicionales* (list '== '< '> '<= '>=))
  (defvar *separators* (list '+ '- '* '/))
  (runc code)
  *results*)

(defun runc(code) ;;"main" del interprete
  (dolist (linecode code 'COMPILATION_SUCCESFUL)
    (cond ((eq (car linecode) 'int) (process_int (cdr linecode)))
	  ((pert (car linecode) *variables*) (cond ((eq (cadr linecode) '=) (process_intline linecode))
						   ((eq (cadr linecode) '+=) (process_intline (append (append (list (car linecode) '=) (list (car linecode) '+)) (list (cddr linecode)))))
						   ((eq (cadr linecode) '-=) (process_intline (append (append (list (car linecode) '=) (list (car linecode) '-)) (list (cddr linecode)))))
						   ((eq (cadr linecode) '*=) (process_intline (append (append (list (car linecode) '=) (list (car linecode) '*)) (list (cddr linecode)))))
						   ((eq (cadr linecode) '/=) (process_intline (append (append (list (car linecode) '=) (list (car linecode) '/)) (list (cddr linecode)))))
						   ((eq (cadr linecode) '++) (process_intline (append (list (car linecode) '=) (list (car linecode) '+ '1))))
						   ((eq (cadr linecode) '--) (process_intline (append (list (car linecode) '=) (list (car linecode) '- '1))))
						   (t (error 'ERROR_DE_SINTAXIS))))

	  ((eq (car linecode) '++) (process_intline (append (list (cadr linecode) '=) (list (cadr linecode) '+ '1))))
	  ((eq (car linecode) '--) (process_intline (append (list (cadr linecode) '=) (list (cadr linecode) '- '1))))
	  ((eq (car linecode) 'printf) (add_result (solveoperation (cond ((atom (cadr linecode)) (cdr linecode))
									 ((pert '= (cadr linecode)) (progn (send_intline (cadr linecode))(list (caadr linecode))))
									 (t (cdr linecode))))))

	  ((eq (car linecode) 'scanf) (scan (cadr linecode)))
	  ((eq (car linecode) 'main) (runc (cadr linecode)))
	  ((eq (car linecode) 'if) (if (solve_cond (cadr linecode))
				       (runc (caddr linecode))
				       (if (eq (cadddr linecode) 'else)
					   (runc (car (cddddr linecode)))
					   nil)))
	  ((eq (car linecode) 'while) (if (solve_cond (cadr linecode))
					  (progn (runc (caddr linecode)) (runc (list linecode)))
					  nil))
	  ((eq (car linecode) 'define) (cond ((not (symbolp (cadr linecode))) (error 'NOMBRE_INVALIDO))
					    ((not (numberp (caddr linecode))) (error 'VALOR_INVALIDO))
					    (t (defparameter *constants* (asignar_valor (cadr linecode) (caddr linecode) *constants*)))))
  
	  ((eq (car linecode) 'void) (cond ((not (null (pertfun (cadr linecode) *funciones*))) (error 'VARIABLE_YA_DEFINIDA))
					   (t (add_funcion (cadr linecode) (caddr linecode) (cadddr linecode)))))
	  ((pert (car linecode) *funciones*) (runfun (pertfun (car linecode) *funciones*) (list (solveoperation (cadr linecode)))))
	  )))

(run '((int (a = 0)(b = 3)(c = 5))
       (define cte 7)
       (void print6 ((int b)) (
			(printf b)
			(printf cte)
			(scanf a)
			       (printf a)
			       (printf (c = a + cte))
			)
	)
       (main (
	      (print6 ((b + 3)))
	      (printf (b + c + 1))
	      (printf cte))))
     
     )






