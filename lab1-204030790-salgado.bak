#lang racket
#|TDA Zonas|#
;El TDA de zonas estará dado por una lista de cuatro elementos, cada elemento representará una de las zonas básicas de Git.
;=> '(Workspace Index LocalRepository RemoteRepository)
;Cada uno de los elementos de "zonas" consistirá en  lista que albergará las distintas versiones del código, que para efectos de este laboratorio son strings.




;CONSTRUCTOR

;Dom: Las cuatro zonas básicas de Git(listas de Strings)
;Rec: El TDA zonas (listas de listas de Strings)
;Esta función crea una lista con las cuatro entradas que representan las zonas básicas

(define (zonas Workspace Index LocalRepository RemoteRepository)
  (list Workspace Index LocalRepository RemoteRepository)
  )

;PERTENENCIA
;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Booleano
;Esta función verifica que la entrada sea un TDA de zonas, retornado #t de ser así, si no, #f.
(define (zonas? zonasTDA)
  (if (and (list? zonasTDA) (= (length zonasTDA) 4)
           (list? (car zonasTDA)) (list? (cadr zonasTDA)) (list? (caddr zonasTDA)) (list? (cadddr zonasTDA))
         )
      #t
      #f
    )
  )

