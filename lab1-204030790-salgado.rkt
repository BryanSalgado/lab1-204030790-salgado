#lang racket
#|TDA Zonas|#
;El TDA de zonas estará dado por una lista de cuatro elementos, cada elemento representará una de las zonas básicas de Git.
;=> '(Workspace Index LocalRepository RemoteRepository)
;Cada uno de los elementos de "zonas" consistirá en  lista que albergará las distintas versiones del código, que para efectos de este laboratorio son strings.




;CONSTRUCTOR

;Dom: Las cuatro zonas básicas de Git(listas de Strings)
;Rec: El TDA zonas (listas de listas de Strings)
;Esta función crea una lista con las cuatro entradas que representan las zonas básicas

(define (TDAzonas Workspace Index LocalRepository RemoteRepository)
  (list Workspace Index LocalRepository RemoteRepository)
  )

;PERTENENCIA
;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Booleano
;Esta función verifica que la entrada sea un TDA de zonas, retornado #t de ser así, si no, #f.
(define (zonas? zonas)
  (if (and (list? zonas) (= (length zonas) 4)
           (list? (car zonas)) (list? (cadr zonas)) (list? (caddr zonas)) (list? (cadddr zonas))
         )
      #t
      #f
    )
  )

;SELECTORES

;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Zona (lista de Strings)
;Esta función obtiene la zona Workspace de un TDA zonas

(define (getWorkspace zonas)
  (if (zonas? zonas)
      (car zonas)
      null
  )
 )

;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Zona (lista de Strings)
;Esta función obtiene la zona Workspace de un TDA zonas

(define (getIndex zonas)
  (if (zonas? zonas)
      (cadr zonas)
      null
  )
 )

;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Zona (lista de Strings)
;Esta función obtiene la zona Workspace de un TDA zonas

(define (getLocalR zonas)
  (if (zonas? zonas)
      (caddr zonas)
      null
  )
 )

;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Zona (lista de Strings)
;Esta función obtiene la zona Workspace de un TDA zonas

(define (getRemoteR zonas)
  (if (zonas? zonas)
      (cadddr zonas)
      null
  )
 )

(define (setWorkspace newWorkspace zonas)
  (if (zonas? zonas)
      (TDAzonas newWorkspace
                (getIndex zonas)
                (getLocalR zonas)
                (getRemoteR zonas)
              )
      null
     )
 )

(define (setIndex newIndex zonas)
  (if (zonas? zonas)
      (TDAzonas (getWorkspace zonas)
                newIndex
                (getLocalR zonas)
                (getRemoteR zonas)
              )
      null
     )
 )

(define (setLocalR newLocalR zonas)
  (if (zonas? zonas)
      (TDAzonas (getWorkspace zonas)
                (getIndex zonas)
                newLocalR
                (getRemoteR zonas)
              )
      null
     )
 )

(define (setRemoteR newRemoteR zonas)
  (if (zonas? zonas)
      (TDAzonas (getWorkspace zonas)
                (getIndex zonas)
                (getLocalR zonas)
                newRemoteR
              )
      null
     )
 )


;Dom: Comando (funcion)
;Rec: Funcion git del comando (funcion)
;Esta función entrega la función que pase por comando
(define (git comando)
  comando
 )


(define (member? elemento lista)
 (if (null? lista)
     #f
     (if (equal? elemento lista)
         #t
         (member? elemento (cdr lista)) 
       )
    )
 )

(define (setCambios listaA listaB)
  (if (null? listaA)
      listaB
      (if (member? (car listaA) listaB)
          (setCambios (cdr listaA) listaB)
          (setCambios (cdr listaA) (append listaB (list (car listaA))))
         )
     )
 )

(define (pull zonas)
  (cons (setLocalR  (setCambios (getRemoteR zonas) (getLocalR zonas))
                    zonas
              )
        "pull"
       )
  )


(define (elemAsociado elemento lista)
  (if (equal? (car lista) elemento)
      (car lista)
      (elemAsociado elemento (cdr lista))
     )
 )



(define (setCambiosElecc listaA listaElem listaB)
  (if (null? listaElem)
      listaB
      (setCambiosElecc listaA (cdr listaElem)
                       (append listaB (list (elemAsociado (car listaElem) listaA)))
                   )
    )
  )


(define (add listaArchivos zonas)
  (if (null? listaArchivos)
      (add-all zonas)
      (setIndex (setCambios (getWorkspace zonas) listaArchivos (getIndex zonas))
                zonas
            )
     )
 )
      