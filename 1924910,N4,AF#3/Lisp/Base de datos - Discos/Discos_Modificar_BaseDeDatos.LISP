
CL-USER 1 > (defun Hacer-CD (Titulo Artista Rating Reproducido)
  (list :Titulo Titulo :Artista Artista :Rating Rating :Reproducido Reproducido))
HACER-CD

CL-USER 2 > 

(Hacer-CD "Roses" "Kathy Mattea" 7 t)
(:TITULO "Roses" :ARTISTA "Kathy Mattea" :RATING 7 :REPRODUCIDO T)

CL-USER 3 > 
(defvar *BaseDeDatos* nil)
*BASEDEDATOS*

CL-USER 4 > 
(defun add-record (cd) (push cd *BaseDeDatos*))
ADD-RECORD

CL-USER 5 > 
 (add-record (Hacer-CD "Roses" "Kathy Mattea" 7 t))
((:TITULO "Roses" :ARTISTA "Kathy Mattea" :RATING 7 :REPRODUCIDO T))

CL-USER 6 >  (add-record (Hacer-CD "Fly" "Dixie Chicks" 8 t))
((:TITULO "Fly" :ARTISTA "Dixie Chicks" :RATING 8 :REPRODUCIDO T) (:TITULO "Roses" :ARTISTA "Kathy Mattea" :RATING 7 :REPRODUCIDO T))

CL-USER 7 >  (add-record (Hacer-CD "Home" "Dixie Chicks" 9 t))
((:TITULO "Home" :ARTISTA "Dixie Chicks" :RATING 9 :REPRODUCIDO T) (:TITULO "Fly" :ARTISTA "Dixie Chicks" :RATING 8 :REPRODUCIDO T) (:TITULO "Roses" :ARTISTA "Kathy Mattea" :RATING 7 :REPRODUCIDO T))

CL-USER 8 >  (add-record (Hacer-CD "Fly" "Dixie Chicks" 8 t))
((:TITULO "Fly" :ARTISTA "Dixie Chicks" :RATING 8 :REPRODUCIDO T) (:TITULO "Home" :ARTISTA "Dixie Chicks" :RATING 9 :REPRODUCIDO T) (:TITULO "Fly" :ARTISTA "Dixie Chicks" :RATING 8 :REPRODUCIDO T) (:TITULO "Roses" :ARTISTA "Kathy Mattea" :RATING 7 :REPRODUCIDO T))

CL-USER 9 > (defun Desglosar-BaseDeDatos ()
  (dolist (cd *BaseDeDatos*)
    (format t "~{~a:~10t~a~%~}~%" cd)))
DESGLOSAR-BASEDEDATOS

CL-USER 10 > 
(format t "~a:~10t~a" :artist "Dixie Chicks")
ARTIST:   Dixie Chicks
NIL

CL-USER 11 > 
(defun Leer-Preguntas (Pregunta_Recibida)
  (format *query-io* "~a: " Pregunta_Recibida)
  (force-output *query-io*)
  (read-line *query-io*))
LEER-PREGUNTAS

CL-USER 12 > 
 (defun Pedir_Datos_CD ()
 (Hacer-CD
 (Leer-Preguntas "Titulo")
 (Leer-Preguntas "Artista")
 (or (parse-integer (Leer-Preguntas "Rating") :junk-allowed t) 0)
 (y-or-n-p "Reproducido [y/n]: ")))
PEDIR_DATOS_CD

CL-USER 13 > 
(defun AgregarDatos ()
(loop (add-record (Pedir_Datos_CD))
    (if (not (y-or-n-p "¿Agregar otra?[y/n]:  ")) (return))))
AGREGARDATOS

CL-USER 14 > (AgregarDatos)
Titulo: Last Train at O'Clock
Artista: Lamp
Rating: 8
Titulo: Flurecent Adolecent
Artista: Arctic Monkeys
Rating: 8
NIL

CL-USER 15 > (Desglosar-BaseDeDatos)
TITULO:   Flurecent Adolecent
ARTISTA:  Arctic Monkeys
RATING:   8
REPRODUCIDO: NIL

TITULO:   Last Train at O'Clock
ARTISTA:  Lamp
RATING:   8
REPRODUCIDO: NIL

TITULO:   Fly
ARTISTA:  Dixie Chicks
RATING:   8
REPRODUCIDO: T

TITULO:   Home
ARTISTA:  Dixie Chicks
RATING:   9
REPRODUCIDO: T

TITULO:   Fly
ARTISTA:  Dixie Chicks
RATING:   8
REPRODUCIDO: T

TITULO:   Roses
ARTISTA:  Kathy Mattea
RATING:   7
REPRODUCIDO: T

NIL

CL-USER 16 > 
(defun Guardar-BaseDeDatos (nombre-archivo)
  (with-open-file (out nombre-archivo
                      :direction :output
                      :if-exists :supersede)
    (with-standard-io-syntax
      (print *BaseDeDatos* out))))
GUARDAR-BASEDEDATOS

CL-USER 17 > (