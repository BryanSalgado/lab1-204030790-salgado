#lang racket
#|TDA Zonas|#
;El TDA de zonas estará dado por una lista de cuatro elementos, que representarán una de las zonas básicas de Git, y el historial de modificaciones.
;=> '(Workspace Index LocalRepository RemoteRepository Historial)
;Cada uno de los elementos de "zonas" consistirá en  lista que albergará las distintas versiones del código, que para efectos de este laboratorio son strings.




;CONSTRUCTOR

;Dom: Las cinco zonas básicas de Git(listas de Strings)
;Rec: El TDA zonas (listas de listas de Strings)
;Esta función crea una lista con las cuatro entradas que representan las zonas básicas y genera un historial vacío

(define (TDAzonas Workspace Index LocalRepository RemoteRepository)
  (list Workspace Index LocalRepository RemoteRepository '())
  )

;PERTENENCIA
;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Booleano
;Esta función verifica que la entrada sea un TDA de zonas, retornado #t de ser así, si no, #f.
(define (zonas? zonas)
  (if (and (list? zonas) (= (length zonas) 4)
           (list? (car zonas)) (list? (cadr zonas)) (list? (caddr zonas)) (list? (cadddr zonas)) (list? (cadddr (cdr zonas)))
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


;Dom: El TDA zonas (lista de cuatro listas)
;Rec: Historial (lista de Strings)
;Esta función obtiene el historial de un TDA zonas

(define (getHistorial zonas)
  (if (zonas? zonas)
      (cadddr (cdr zonas))
      null
  )
 )

;Dom: Una nueva zona (lista de strings) y un TDA zonas (lista de cuatro listas)
;Rec: El TDA zonas (lista de cuatro listas)
;Esta función retorna un TDA zonas con el Workspace modificado

(define (setWorkspace newWorkspace zonas)
  (if (zonas? zonas)
      (list newWorkspace
            (getIndex zonas)
            (getLocalR zonas)
            (getRemoteR zonas)
            (getHistorial zonas)
         )
      null
     )
 )

;Dom: Una nueva zona (lista de strings) y un TDA zonas (lista de cuatro listas)
;Rec: El TDA zonas (lista de cuatro listas)
;Esta función retorna un TDA zonas con el Index modificado
(define (setIndex newIndex zonas)
  (if (zonas? zonas)
      (list (getWorkspace zonas)
            newIndex
            (getLocalR zonas)
            (getRemoteR zonas)
            (getHistorial zonas)
         )
      null
     )
 )

;Dom: Una nueva zona (lista de strings) y un TDA zonas (lista de cuatro listas)
;Rec: El TDA zonas (lista de cuatro listas)
;Esta función retorna un TDA zonas con el Local Repository modificado
(define (setLocalR newLocalR zonas)
  (if (zonas? zonas)
      (list (getWorkspace zonas)
            (getIndex zonas)
            newLocalR
            (getRemoteR zonas)
            (getHistorial zonas)
         )
      null
     )
 )

;Dom: Una nueva zona (lista de strings) y un TDA zonas (lista de cuatro listas)
;Rec: El TDA zonas (lista de cuatro listas)
;Esta función retorna un TDA zonas con el Remote Repository modificado
(define (setRemoteR newRemoteR zonas)
  (if (zonas? zonas)
      (list (getWorkspace zonas)
            (getIndex zonas)
            (getLocalR zonas)
            newRemoteR
            (getHistorial zonas)
         )
      null
     )
 )

;Dom: Una nueva zona (lista de strings) y un TDA zonas (lista de cuatro listas)
;Rec: El TDA zonas (lista de cuatro listas)
;Esta función retorna un TDA zonas con el Historial modificado
(define (setHistorial newHistorial zonas)
  (if (zonas? zonas)
      (list (getWorkspace zonas)
            (getIndex zonas)
            (getLocalR zonas)
            (getRemoteR zonas)
            newHistorial
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

;Dom: Un elemento de una lista y una lista
;Rec: Booleano
;Esta función revisa si el elemento de entrada pertenece a la lista de entrada y retorna #t o #f de estar o no, respectivamente.
(define (member? elemento lista)
 (if (null? lista)
     #f
     (if (equal? elemento lista)
         #t
         (member? elemento (cdr lista)) 
       )
    )
 )
;Dom: Dos listas
;Rec: Una lista
;Esta función entrega una lista que contiene los elementos de la segunda lista de entrada, mas todos los que contiene la primera pero no la segunda.
(define (setCambios listaA listaB)
  (if (null? listaA)
      listaB
      (if (member? (car listaA) listaB)
          (setCambios (cdr listaA) listaB)
          (setCambios (cdr listaA) (append listaB (list (car listaA))))
         )
     )
 )

;Dom: TDA zonas
;Rec: TDA zonas
;Esta función entrega el TDA de entrada donde se le ha modificado el Local Repository para contener todos los cambios del Remote Repository
(define (pull zonas)
  (cons (setLocalR  (setCambios (getRemoteR zonas) (getLocalR zonas))
                    zonas
              )
        "pull"
       )
  )

;Dom: Un string y una lista de string
;Rec: Un string de la lista
;Esta función entrega un elemento de la lista llamdo igual que el elemento entregado
(define (elemAsociado elemento lista)
  (if (equal? (car lista) elemento)
      (car lista)
      (elemAsociado elemento (cdr lista))
     )
 )


;Dom: Tres lista de string
;Rec: Una lista de string
;Una lista que contenga los elementos de "listaB" mas todos los elementos que se requieran por medio de la "listaElem" de "listaA"
(define (setCambiosElecc listaA listaElem listaB)
  (if (null? listaElem)
      listaB
      (setCambiosElecc listaA (cdr listaElem)
                       (append listaB (list (elemAsociado (car listaElem) listaA)))
                   )
    )
  )

;Sin definir aún
(define (add-all zonas) zonas);Sin definir aún

;Dom: lista de string y un TDA zonas
;Rec: Un TDA zonas
; Esta funcion entrega el TDA con su Index modificado, para que este se le agreguen todos los elementos indicados por "listaArchivos" desde el Workspace
(define (add listaArchivos zonas)
  (if (null? listaArchivos)
      (add-all zonas)
      (setIndex (setCambiosElecc (getWorkspace zonas) listaArchivos (getIndex zonas))
                zonas
            )
     )
 )
      