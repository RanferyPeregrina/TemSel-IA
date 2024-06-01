(defun Hacer-Planeta (Nombre Tipo Distancia Fecha_Descubrimiento Masa Temperatura)
  (list :Nombre Nombre :Tipo Tipo :Distancia Distancia :Fecha_Descubrimiento Fecha_Descubrimiento :Masa Masa :Temperatura Temperatura ))

(Hacer-Planeta "alf Cen B b" "Terrestre" 4.24 2012 1.14 794.75 )

(defvar *BaseDeDatos* nil)

(defun add-record (Planeta) (push Planeta *BaseDeDatos*))

(add-record (Hacer-Planeta "PSR B1957+20 b" "Joviano" 4.99 1988 6994.68 881.55))
(add-record (Hacer-Planeta "eps Eridani b" "Joviano" 10.44 2000 492.81 -161.35))
(add-record (Hacer-Planeta "tau Cet b" "Terrestre" 11.9 2012 2 373.35))
(add-record (Hacer-Planeta "tau Cet c" "Terrestre" 11.9 2012 3.12 201.95))

(defun Desglosar-BaseDeDatos ()
  (dolist (Planeta *BaseDeDatos*)
    (format t "~{~a:~10t~a~%~}~%" Planeta)))

(defun Leer-Preguntas (Pregunta_Recibida)
  (format *query-io* "~a: " Pregunta_Recibida)
  (force-output *query-io*)
  (read-line *query-io*))

 (defun Pedir_Datos_Planeta ()
 (Hacer-Planeta
 (Leer-Preguntas "Nombre")
 (Leer-Preguntas "Tipo")
 (or (parse-float (Leer-Preguntas "Distancia")) 0)
 (or (parse-integer (Leer-Preguntas "Fecha_Descubrimiento") :junk-allowed t) 0)
 (or (parse-float (Leer-Preguntas "Masa")) 0)
 (or (parse-float (Leer-Preguntas "Temperatura")) 0)))

(defun AgregarDatos ()
(loop (add-record (Pedir_Datos_Planeta))
    (if (not (y-or-n-p "¿Agregar otra?[y/n]:  ")) (return))))
;Agregamos hasta Gliese 680 b y son 19 registros

(add-record (Hacer-Planeta "HD 20794 b" "Terrestre" 19.77 2011 2.7 378.84))
(add-record (Hacer-Planeta "HD 20794 C" "Terrestre" 19.77 2011 2.42 234.75))
(add-record (Hacer-Planeta "HD 20794 d" "Terrestre" 19.77 20114 4.77 14.05))
(add-record (Hacer-Planeta "Gliese 581 b" "Neptuniano " 20.25 2005 15.9 14.05))
(add-record (Hacer-Planeta "Gliese 581 c" "Supertierra" 20.25 2007 5.4 39.45))
(add-record (Hacer-Planeta "Gliese 581 e" "Terrestre" 20.25 2009 1.94 228.35))
(add-record (Hacer-Planeta "HD 219134 b" "Terrestre" 21.36 2015 3.82 658.55))
(add-record (Hacer-Planeta "HD 219134 c" "Terrestre" 21.36 2015 3.5 444.61))
(add-record (Hacer-Planeta "HD 219134 d" "Neptunuano" 21.36 2015 21.3 103.75))
(add-record (Hacer-Planeta "HD 219134 e" "Terrestre" 21.36 2015 3.72 -159.75))
(add-record (Hacer-Planeta "HD 219134 f" "Supertierra" 21.36 2015 8.9 205.55))
(add-record (Hacer-Planeta "HD 219134 g" "Neptuniano" 21.36 2015 10.81 25.15))
(add-record (Hacer-Planeta "HD 219134 h" "Joviano" 21.36 2015 108.1 -169.55))
(add-record (Hacer-Planeta "WISE J0720-0846 b" "Joviano" 22.18 2014 19712.28 -238.45))
(add-record (Hacer-Planeta "Gliese 667 Cd" "Supertierra" 23.58 2009 5.6 114.35))
(add-record (Hacer-Planeta "Gliese 667 Cc" "Terrestre" 23.58 2011 3.8 -26.65))
(add-record (Hacer-Planeta "Gliese 667 Cd" "Supertierra" 23.58 2013 5.1 -107.25))
(add-record (Hacer-Planeta "Gliese 667 Ce" "Terrestre" 23.58 2013 2.7 -84.25))
(add-record (Hacer-Planeta "Gliese 667 Cf" "Terrestre" 23.58 2013 2.7 -52.45))
(add-record (Hacer-Planeta "Gliese 667 Cg" "Terrestre" 23.58 2013 4.6 -155.55))
(add-record (Hacer-Planeta "HD 95782 b" "Joviano" 24.66 2015 1462.52 -241.75))
(add-record (Hacer-Planeta "Fomalhaut b" "Joviano" 25.11 2008 953.82 -225.25))
(add-record (Hacer-Planeta "61 Vir b" "Supertierra" 27.79 2009 5.09 781.25))
(add-record (Hacer-Planeta "61 Vir c" "Neptuniano" 27.79 2009 18.22 233.25))
(add-record (Hacer-Planeta "61 Vir d" "Neptuniano" 27.79 2009 22.89 66.85))
(add-record (Hacer-Planeta "HD 192310 c" "Neptuniano" 28.77 2011 24.16 -101.35))
(add-record (Hacer-Planeta "Gliese 433b" "Supertierra" 29.96 2009 5.3 132.65))
(add-record (Hacer-Planeta "Gliese 433 c" "Neptuniano" 28.96 2012 44.51 -220.85))
(add-record (Hacer-Planeta "HD 192310 b" "Neptuniano" 29.06 2012 16.91 57.15))
(add-record (Hacer-Planeta "Gliese 849 b" "Joviano" 29.68 2006 286.15 -215.75))
(add-record (Hacer-Planeta "Gñoese 849 c" "Joviano" 29.68 2013 244.71 -236.25))

(defun Seleccionar-PorNombre (Nombre)
 (remove-if-not
 #'(lambda (Planeta) (equal (getf Planeta :Nombre) Nombre))
 *BaseDeDatos*))

 (defun Seleccionar-PorTipo (Tipo)
 (remove-if-not
 #'(lambda (Planeta) (equal (getf Planeta :Tipo) Tipo))
 *BaseDeDatos*))

 (defun Seleccionar-PorDistancia (Distancia)
 (remove-if-not
 #'(lambda (Planeta) (equal (getf Planeta :Distancia) Distancia))
 *BaseDeDatos*))

(defun Seleccionar-PorFecha (Fecha_Descubrimiento)
 (remove-if-not
 #'(lambda (Planeta) (equal (getf Planeta :Fecha_Descubrimiento) Fecha_Descubrimiento))
 *BaseDeDatos*))
 
 (defun Seleccionar-PorMasa (Masa)
 (remove-if-not
 #'(lambda (Planeta) (equal (getf Planeta :Masa) Masa))
 *BaseDeDatos*))

 (defun Seleccionar-PorTemperatura (Temperatura)
 (remove-if-not
 #'(lambda (Planeta) (equal (getf Planeta :Temperatura) Temperatura))
 *BaseDeDatos*))

(Seleccionar-PorFecha 2009)
(Seleccionar-PorNombre "61 Vir c")
(Seleccionar-PorTipo "Neptuniano")

(defun Seleccionar (selector-fn)
 (remove-if-not selector-fn *BaseDeDatos*))

 (Seleccionar #'(lambda (Planeta) (equal (getf Planeta :Tipo) "Supertierra")))

(defun Selector-Nombre (Nombre)
 #'(lambda (Planeta) (equal (getf Planeta :Nombre) Nombre)))
 (defun Selector-Tipo (Tipo)
 #'(lambda (Planeta) (equal (getf Planeta :Tipo) Tipo)))
 (defun Selector-Distancia (Distancia)
 #'(lambda (Planeta) (equal (getf Planeta :Distancia) Distancia)))
 (defun Selector-Fecha (Fecha_Descubrimiento)
 #'(lambda (Planeta) (equal (getf Planeta :Fecha_Descubrimiento) Fecha_Descubrimiento)))
  (defun Selector-Masa (Masa)
 #'(lambda (Planeta) (equal (getf Planeta :Masa) Masa)))
 (defun Selector-Temperatura (Temperatura)
 #'(lambda (Planeta) (equal (getf Planeta :Temperatura) Temperatura)))

 ;Nombre Tipo Distancia Fecha_Descubrimiento Masa Temperatura

 (remove-if-not (Selector-Tipo "Joviano") *BaseDeDatos*)


(Seleccionar (Selector-Nombre "HD 219134 g"))
(Seleccionar (Selector-Tipo "Terrestre"))
(Seleccionar (Selector-Distancia 21.36))
(Seleccionar (Selector-Fecha 2012))
(Seleccionar (Selector-Temperatura 205.55))

 (defun where (&key Nombre Tipo Distancia Fecha_Descubrimiento Masa Temperatura)
 #'(lambda (Planeta)
 (and
 (if Nombre (equal (getf Planeta :Nombre) Nombre) t)
 (if Tipo (equal (getf Planeta :Tipo) Tipo) t)
 (if Distancia (equal (getf Planeta :Distancia) Distancia) t)
 (if Fecha_Descubrimiento (equal (getf Planeta :Fecha_Descubrimiento) Fecha_Descubrimiento) t)
 (if Masa(equal (getf Planeta :Masa) Masa) t)
 (if Temperatura (equal (getf Planeta :Temperatura) Temperatura) t))))

 (defun Actualizar (selector-fn &key Nombre Tipo Distancia Fecha_Descubrimiento Masa Temperatura)
 (setf *BaseDeDatos*
 (mapcar
 #'(lambda (Registro)
 (when (funcall selector-fn Registro)
 (if Nombre (setf (getf Registro :Nombre) Nombre))
 (if Tipo (setf (getf Registro :Tipo) Tipo))
 (if Distancia (setf (getf Registro :Distancia) Distancia))
 (if Fecha_Descubrimiento (setf (getf Registro :Fecha_Descubrimiento) Fecha_Descubrimiento))
 (if Masa (setf (getf Registro :Masa) Masa))
 (if Temperatura (setf (getf Registro :Temperatura) Temperatura))
 Registro) *BaseDeDatos*))))

 (Seleccionar (where :Tipo "Supertierra"))
 (Seleccionar (where :Fecha_Descubrimiento 2012))

 (defun Borrar-Registro (selector-fn)
 (setf *BaseDeDatos* (remove-if selector-fn *BaseDeDatos*)))

;Borramos un registro porque tenemos 61 en lugar de 60, en este caso el último
;(Borrar-Registro ((Seleccionar (where :Nombre "Gñoese 849 c")))) Pero no funcionó...

(defvar selector (where :Nombre "Gñoese 849 c"))
(defvar registros-a-borrar (Seleccionar selector))
(Borrar-Registro selector)

(Seleccionar
 #'(lambda (Planeta)
 (and (equal (getf Planeta :Nombre) "61 Vir b")
      (equal (getf Planeta :Distancia) 27.79))))

(defmacro Reversa (expr) (reverse expr))