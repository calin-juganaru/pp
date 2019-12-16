#lang racket

(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)

;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:

(define empty-stack null)

(define (make-stack) empty-stack)

(define (push element stack)
  (append (list element) stack))

(define (top stack)
  (if (empty? stack)
      empty-stack
      (car stack)))

(define (pop stack)
  (if (empty? stack)
      empty-stack
      (if (number? stack)
          stack
          (cdr stack))))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.

(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames

(define (get-varnames stack-machine) (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts

(define (get-consts stack-machine) (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names

(define (get-names stack-machine) (cadddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code

(define (get-code stack-machine) (car (cddddr stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack

(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0

(define (get-IC stack-machine) (cadr (cddddr stack-machine)))

(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.

(define (get-symbol-index symbol)
  (let find-symbol ((L symbols) (index 0))
    (if (equal? symbol (car L))
        index
        (find-symbol (cdr L) (add1 index)))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"

(define (update-stack-machine item symbol stack-machine)
  (let update-stack-machine-aux ((index (get-symbol-index symbol)) (new-stack-machine null) (old-stack-machine stack-machine))
    (if (empty? old-stack-machine)
        new-stack-machine
        (if (= 0 index)
            (update-stack-machine-aux (sub1 index) (append new-stack-machine (list item)) (cdr old-stack-machine))
            (update-stack-machine-aux (sub1 index) (append new-stack-machine (list (car old-stack-machine)))
                                      (cdr old-stack-machine))))))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție

(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.

(define (pop-exec-stack stack-machine)
   (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;---------------------------------------------------------------------------------------------------

(define not-empty? (compose1 not empty?))

(define (inc-IC stack-machine)
  (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER stack-machine))

(define (get-TOS stack-machine)
  (top (get-stack stack-machine)))

(define (current-op stack-machine)
  (list-ref (get-code stack-machine) (get-IC stack-machine)))

(define (get-operand stack-machine)
  (cdr (current-op stack-machine)))

(define (get-cmp-op stack-machine)
  (convert-cmp-op (cdr (current-op stack-machine))))

(define (my_write arg)
  (begin (println arg) arg))

(define functions (hash
  "print" my_write
  "range" range
  "sqrt"  sqrt
  "prod"  *
))

(define get-function ((curry hash-ref) functions))

;---------------------------------------------------------------------------------------------------

(define (RETURN_VALUE stack-machine)
  stack-machine)

(define (POP_TOP stack-machine)
  (pop-exec-stack (inc-IC stack-machine)))

(define (BINARY_MODULO stack-machine)
  (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
    (push-exec-stack (modulo TOS1 TOS) (pop-exec-stack (pop-exec-stack (inc-IC stack-machine))))))

(define (BINARY_ADD stack-machine)
  (let ((TOS (get-TOS stack-machine)) (TOS1 (top (pop (get-stack stack-machine)))))
    (push-exec-stack (+ TOS1 TOS) (pop-exec-stack (pop-exec-stack (inc-IC stack-machine))))))

(define (BINARY_SUBTRACT stack-machine)
  (let ((TOS (top (get-stack stack-machine))) (TOS1 (top (pop (get-stack stack-machine)))))
    (push-exec-stack (- TOS1 TOS) (pop-exec-stack (pop-exec-stack (inc-IC stack-machine))))))

(define INPLACE_ADD BINARY_ADD)
(define INPLACE_SUBTRACT BINARY_SUBTRACT)
(define INPLACE_MODULO BINARY_MODULO)

(define (LOAD_CONST stack-machine)
  (push-exec-stack (hash-ref (get-consts stack-machine) (get-operand stack-machine)) (inc-IC stack-machine)))

(define (LOAD_GLOBAL stack-machine)
  (push-exec-stack (hash-ref (get-names stack-machine) (get-operand stack-machine)) (inc-IC stack-machine)))

(define (LOAD_FAST stack-machine)
  (push-exec-stack (hash-ref (get-varnames stack-machine) (get-operand stack-machine)) (inc-IC stack-machine)))

(define (STORE_FAST stack-machine)
  (pop-exec-stack (inc-IC (update-stack-machine (hash-set (get-varnames stack-machine)
                                                          (get-operand stack-machine)
                                                          (get-TOS stack-machine))
                                                'CO-VARNAMES stack-machine))))

(define (POP_BLOCK stack-machine)
  (RETURN_VALUE (inc-IC stack-machine)))

(define (COMPARE_OP stack-machine)
  (let ((TOS (get-TOS stack-machine)) (TOS1 (top (pop (get-stack stack-machine)))))
    (push-exec-stack ((get-cmp-op stack-machine) TOS1 TOS) (pop-exec-stack (pop-exec-stack (inc-IC stack-machine))))))

(define (JUMP_ABSOLUTE stack-machine)
  (let* ((index (/ (get-operand stack-machine) 2))
         (new-stack-machine (update-stack-machine index 'INSTRUCTION-COUNTER stack-machine)))
    new-stack-machine))

(define (POP_JUMP_IF_FALSE stack-machine)
  (if (get-TOS stack-machine)
      (inc-IC (pop-exec-stack stack-machine))
      (let* ((index (/ (get-operand stack-machine) 2))
            (new-stack-machine (pop-exec-stack (update-stack-machine index 'INSTRUCTION-COUNTER stack-machine))))
            new-stack-machine)))

(define (top_of_the_top L)
  (if (number? L)
      L
      (if (empty? L)
          '()
          (top_of_the_top (car L)))))

(define (GET_ITER stack-machine)
  (RETURN_VALUE (inc-IC stack-machine)))

(define (SET_ITER stack-machine)
  (let* ((stack (get-stack stack-machine))
        (TOS (top_of_the_top stack))
        (TOS1 (pop (top stack)))
        (TOS2 (pop stack))
        (new-stack (append (cons TOS1 TOS2) null)))
    (inc-IC (update-stack-machine (push TOS new-stack) 'STACK stack-machine))))

(define (FOR_ITER stack-machine)

  (if (and (list? (top (get-stack stack-machine))) (not-empty? (top (get-stack stack-machine))))
      (let ((aux (SET_ITER stack-machine)))
                 aux)
  (let* ((delta (+ (/ (get-operand stack-machine) 2) 1)))
        (if (empty? (top (get-stack stack-machine)))
            (let*  ((index (+ (get-IC stack-machine) delta)))
              (update-stack-machine index 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine)))

            (let* ((topless-stack (pop (get-stack stack-machine)))
                   (next-iter (top_of_the_top topless-stack))
                   (new-stack (push next-iter (append (cons (pop (top topless-stack))
                                                            (pop topless-stack)) empty-stack))))
              (inc-IC (update-stack-machine new-stack 'STACK stack-machine)))))))

(define (SETUP_LOOP stack-machine)
  (RETURN_VALUE (inc-IC stack-machine)))

(define (CALL_FUNCTION stack-machine)
  (let call_func_aux ((argv empty-stack) (argc (get-operand stack-machine)) (new-stack-machine stack-machine))
    (if (= 0 argc)
        (push-exec-stack (apply (get-function (top (get-stack new-stack-machine))) argv)
                         (inc-IC (pop-exec-stack new-stack-machine)))
        (call_func_aux (push (top (get-stack new-stack-machine)) argv) (sub1 argc)
                          (pop-exec-stack new-stack-machine)))))

;---------------------------------------------------------------------------------------------------

(define operations (hash
  'POP_TOP            POP_TOP
  'BINARY_MODULO      BINARY_MODULO
  'BINARY_ADD         BINARY_ADD
  'BINARY_SUBTRACT    BINARY_SUBTRACT
  'INPLACE_ADD        INPLACE_ADD
  'INPLACE_SUBTRACT   INPLACE_SUBTRACT
  'INPLACE_MODULO     INPLACE_MODULO
  'GET_ITER           GET_ITER
  'RETURN_VALUE       RETURN_VALUE
  'POP_BLOCK          POP_BLOCK
  'FOR_ITER           FOR_ITER
  'LOAD_CONST         LOAD_CONST
  'COMPARE_OP         COMPARE_OP
  'JUMP_ABSOLUTE      JUMP_ABSOLUTE
  'POP_JUMP_IF_FALSE  POP_JUMP_IF_FALSE
  'LOAD_GLOBAL        LOAD_GLOBAL
  'SETUP_LOOP         SETUP_LOOP
  'LOAD_FAST          LOAD_FAST
  'STORE_FAST         STORE_FAST
  'CALL_FUNCTION      CALL_FUNCTION
))

(define convert-op ((curry hash-ref) operations))

(define (get-operation stack-machine)
  (convert-op (car (current-op stack-machine))))

;---------------------------------------------------------------------------------------------------

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define (run-stack-machine stack-machine)
  (if (equal? (get-operation stack-machine) RETURN_VALUE) stack-machine
      (run-stack-machine ((get-operation stack-machine) stack-machine))))