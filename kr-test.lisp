;;; -*- Mode: LISP; package: KR; Editor: T -*-


(in-package "KR")

#|
(load "/usr/dzg/dante/kr/kr.fasl")
|#

(create-schema :top)
(create-schema :one
  (:is-a :top))
(create-schema :two
  (:is-a :one))


(set-value :one :increment #'(lambda (schema value)
			       (incf (get-value schema :slot) value)))
(set-value :one :slot 0)


(get-value :one :is-a)
;; (GET-VALUE ONE IS-A)   : 0.02568412 msec.

(macroexpand '(get-value :one :is-a))
(OR (CAR (GET ONE IS-A))
    (CAR (GET-VALUES-INTERNAL ONE IS-A)))

(is-a-p :one :top)
;; (IS-A-P ONE TOP)   : 0.05537075 msec.

(set-value :one :slot 4)
;; (SET-VALUE ONE SLOT 4)   : 0.0696293 msec.
;; (SET-VALUE ONE SLOT 4)   : 0.04824233 msec.  without RELATION-P

(set-values :one :slot '(4 3))
;; (SET-VALUES ONE SLOT '(4 3))   : 0.0749024 msec.

(get-values :one :slot)
;; (GET-VALUE ONE SLOT)   : 0.0633794 msec.   ; as a function
;; (GET-VALUE ONE SLOT)   : 0.03974575 msec.
(get-value :two :slot)
;; (GET-VALUE TWO SLOT)   : 0.153418 msec.    ; as a function
;; (GET-VALUE TWO SLOT)   : 0.1194342 msec.
;; (GET-VALUE TWO SLOT)   : 0.110352 msec.

;; (GET-VALUE TWO SLOT)   : 0.121191 msec.

;; (GET-VALUE TWO SLOT)   : 0.1277347 msec.



(relation-p :slot)
;; (RELATION-P SLOT)   : 0.01052724 msec.
;; (RELATION-P SLOT)   : 0.01259762 msec.
;; (RELATION-P SLOT)   : 0.01001953 msec.
;; (RELATION-P SLOT)   : 0.00996096 msec.
;; (RELATION-P SLOT)   : 0.01025395 msec.


(schema-call :one :increment 2)
;; (SCHEMA-CALL ONE INCREMENT 2)   : 0.124805 msec.


(get-slots :one)
;; (GET-SLOTS ONE)   : 0.2187502 msec.
;;     32 bytes, List.


(defun buf (a b) (declare (ignore b)) a)
(do-slots :one #'buf)
;; (DO-SLOTS ONE #'BUF)   : 0.1627598 msec.
