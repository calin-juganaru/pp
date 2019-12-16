#lang racket

(provide get-opname)
(provide convert-cmp-op)

(define opcodes (hash
  1   'POP_TOP
  22  'BINARY_MODULO
  23  'BINARY_ADD
  24  'BINARY_SUBTRACT
  55  'INPLACE_ADD
  56  'INPLACE_SUBTRACT
  59  'INPLACE_MODULO
  68  'GET_ITER
  83  'RETURN_VALUE
  87  'POP_BLOCK
  93  'FOR_ITER
  100 'LOAD_CONST
  107 'COMPARE_OP
  113 'JUMP_ABSOLUTE
  114 'POP_JUMP_IF_FALSE
  116 'LOAD_GLOBAL
  120 'SETUP_LOOP
  124 'LOAD_FAST
  125 'STORE_FAST
  131 'CALL_FUNCTION
  ))

(define != (compose1 not =))

(define cmp_ops (hash
  0 <
  1 <=
  2 =
  3 !=
  4 >
  5 >=
  6 member
  7 (compose not member)
))

(define get-opname ((curry hash-ref) opcodes))
(define convert-cmp-op ((curry hash-ref) cmp_ops))