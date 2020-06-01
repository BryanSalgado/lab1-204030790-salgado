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
  (if (and (list? zonas) (= (length zonas) 5)
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
(define (git comando zonas)
  (setHistorial (add (getHistorial zonas) (list (cdr (comando zonas)))) (car (comando zonas)))
 )

;Dom: Un elemento de una lista y una lista
;Rec: Booleano
;Esta función revisa si el elemento de entrada pertenece a la lista de entrada y retorna #t o #f de estar o no, respectivamente.
(define (member? elemento lista)
 (if (null? lista)
     #f
     (if (equal? elemento (car lista))
         #t
         (member? elemento (cdr lista)) 
       )
    )
 )

;Dom: Dos listas
;Rec: Una lista
;Esta función entrega una lista que contiene los elementos de la segunda lista de entrada, mas todos los que contiene la primera pero no la segunda.
(define (setCambiosRec listaA listaB)
  (if (null? listaA)
      '()
      (if (member? (car listaA) listaB)
          (append (setCambiosRec (cdr listaA) listaB))
          (append (list (car listaA)) (setCambiosRec (cdr listaA) listaB))
         )
     )
 )


(define (setCambios listaA listaB)
  (append listaB (setCambiosRec listaA listaB))
  )


(define (getLast lista)
  (if (null? (cdr lista))
      (car lista)
      (getLast (cdr lista))
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
  (if (equal? (car (car lista)) elemento)
      (car lista)
      (elemAsociado elemento (cdr lista))
     )
 )




;Dom: Tres lista de string
;Rec: Una lista de string
;Una lista que contenga los elementos de "listaB" mas todos los elementos que se requieran por medio de la "listaElem" de "listaA"
(define (setCambiosElecc listaA listaElem listaCambios)
  (if (null? listaElem)
      listaCambios
      (setCambiosElecc listaA (cdr listaElem)
                       (append listaCambios (list (elemAsociado (car listaElem) listaA)))
                   )
    )
  )

;Dom: TDA zonas
;Rec: TDA zonas
;Esta función retorna el TDA zonas en la que a su Index se le agrgan todos los posibles cambios desde su Workspace
(define (add-all zonas)
  (setIndex (setCambios (getWorkspace zonas) (getIndex zonas))
            zonas
        )
 )

;Dom: lista de string y un TDA zonas
;Rec: Un TDA zonas
; Esta funcion entrega el TDA con su Index modificado, para que este se le agreguen todos los elementos indicados por "listaArchivos" desde el Workspace
(define (add listaArchivos zonas)
  (cons (if (null? listaArchivos)
            (add-all zonas)
            (setIndex (setCambiosElecc (getWorkspace zonas) listaArchivos '())
                      zonas
                   )
          )
        "add"
     )
 )


;Dom: String y TDA zonas
;Rec: TDA zonas
;Esta función entrega un TDA zonas a la que su zona Local Repository se le agrega su Index de albergar cambios, sino, retorna la zona original.
(define (commit mensaje zonas)
  (if (equal? (cdr (getLast commit)) (getIndex zonas))
      zonas
      (cons (setLocalR (append (getLocalR zonas) (list (cons mensaje (getIndex zonas)))))
            "commit"
          )
    )
)


;Dom: TDA zonas
;Rec: TDA zonas
;Esta función entrega el TDA de entrada donde se le ha modificado el Remote Repository para contener todos los cambios del Local Repository
(define (push zonas)
  (cons (setRemoteR  (setCambios (getLocalR zonas) (getRemoteR zonas))
                    zonas
              )
        "push"
       )
  )

;'((("lab" . "T I C")) (("lab" . "T I C")) (("c1" ("lab" . "T")) ("c2" ("lab" . "T I C"))) (("c1" ("lab" . "T"))) ("Historial"))
