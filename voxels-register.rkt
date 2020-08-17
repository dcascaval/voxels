#lang rosette

;;;  (require rosette/solver/smt/z3)
;;;  (require (only-in racket hash))

;;; (current-solver (z3
;;;     #:path "C:/Users/Dan/z3.exe" ; sub out your own path
;;;     #:logic 'QF_BV
;;;     #:options (hash
;;;     ;;; ':parallel.enable 'true
;;;     ;;; ':parallel.threads.max 2
;;;     )
;;;   )
;;; )

(define (u2 a b) (bvor a b))
;;; (define (u3 a b c d) (bvor a (bvor b c)))
;;; (define (u4 a b c d) (bvor (bvor a b) (bvor c d)))
(define (diff a b) (bvand a (bvnot b)))
(define (tX+  a b) (bvshl  a (bv 1 64)))
(define (tX+2 a b) (bvshl  a (bv 2 64)))
(define (tX-  a b) (bvlshr a (bv 1 64)))
(define (tX-2 a b) (bvlshr a (bv 2 64)))
(define (tY+  a b) (bvshl  a (bv 4 64)))
(define (tY+2 a b) (bvshl  a (bv 8 64)))
(define (tY-  a b) (bvlshr a (bv 4 64)))
(define (tY-2 a b) (bvlshr a (bv 8 64)))
(define (tZ+  a b) (bvshl  a (bv 16 64)))
(define (tZ+2 a b) (bvshl  a (bv 32 64)))
(define (tZ-  a b) (bvlshr a (bv 16 64)))
(define (tZ-2 a b) (bvlshr a (bv 32 64)))
(define (mirrorX a b) (bvor (bvor (bvand a (bv #x3333333333333333 64)) (bvshl (bvand a (bv #x2222222222222222 64)) (bv 1 64))) (bvshl (bvand a (bv #x1111111111111111 64)) (bv 3 64)) ))
(define (mirrorY a b) (define b1 (bvshl (bvand a (bv #x00FF00FF00FF00FF 64)) (bv 8 64))) (define b2   (bvor (bvshl (bvand b1 (bv #x0F0F0F0F0F0F0F0F 64)) (bv 4 64)) (bvlshr (bvand b1 (bv #xF0F0F0F0F0F0F0F0 64)) (bv 4 64)))) (define b3   (bvor (bvshl (bvand b2 (bv #x3333333333333333 64)) (bv 2 64)) (bvlshr (bvand b2 (bv #xCCCCCCCCCCCCCCCC 64)) (bv 2 64)))) (define b4   (bvor (bvshl (bvand b3 (bv #x5555555555555555 64)) (bv 1 64)) (bvlshr (bvand b3 (bv #xAAAAAAAAAAAAAAAA 64)) (bv 1 64)))) (bvor (bvand b4 (bv #xFF00FF00FF00FF00 64)) (bvand a (bv #x00FF00FF00FF00FF 64))))
(define (mirrorZ a b) (define b1 (bvshl (bvand a (bv #x00000000FFFFFFFF 64)) (bv 32 64))) (define b2 (bvor (bvshl (bvand b1 (bv #x0000FFFF00000000 64)) (bv 16 64)) (bvlshr (bvand b1 (bv #xFFFF000000000000 64)) (bv 16 64)))) (define b3 (bvor (bvshl (bvand b2 (bv #x00FF00FF00000000 64)) (bv 8 64)) (bvlshr (bvand b2 (bv #xFF00FF0000000000 64)) (bv 8 64)))) (define b4 (bvor (bvshl (bvand b3 (bv #x0F0F0F0F00000000 64)) (bv 4 64)) (bvlshr (bvand b3 (bv #xF0F0F0F000000000 64)) (bv 4 64)))) (define b5 (bvor (bvshl (bvand b4 (bv #x3333333300000000 64)) (bv 2 64)) (bvlshr (bvand b4 (bv #xCCCCCCCC00000000 64)) (bv 2 64)))) (define b6 (bvor (bvshl (bvand b5 (bv #x5555555500000000 64)) (bv 1 64)) (bvlshr (bvand b5 (bv #xAAAAAAAA00000000 64)) (bv 1 64)))) (bvor b6 (bvand a (bv #x00000000FFFFFFFF 64))))

(struct operator (function arity name) #:transparent)

(define union-op (operator u2 2 "union"))
;;; (define u3-op (operator u3 3 "union"))
;;; (define u4-op (operator u4 4 "union"))
(define diff-op  (operator diff  2 "diff"))
(define tX+-op (operator tX+ 1 "tX+"))
(define tX--op (operator tX- 1 "tX-"))
(define tY+-op (operator tY+ 1 "tY+"))
(define tY--op (operator tY- 1 "tY-"))
(define tZ+-op (operator tZ+ 1 "tZ+"))
(define tZ--op (operator tZ- 1 "tZ-"))
(define mirrorX-op (operator mirrorX 1 "mirrorX"))
(define mirrorY-op (operator mirrorY 1 "mirrorY"))
(define mirrorZ-op (operator mirrorZ 1 "mirrorZ"))
(define operator-list (list
    union-op
    ; higher union arity
    ;;; u3-op
    ;;; u4-op

    ; diff-op
    tX+-op
    ; tX--op
    tY+-op
    ; tY--op
    tZ+-op
    ; tZ--op

    ;;; tX2+-op
    ;;; tX2--op
    ;;; tY2+-op
    ;;; tY2--op
    ;;; tZ2+-op
    ;;; tZ2--op

    mirrorX-op
    mirrorY-op
    mirrorZ-op
))

(struct instruction (op-idx arg1-idx arg2-idx) #:transparent)

;; A symbolic instruction is an instruction with symbolic indexes (integers)
(define (get-sym-instruction)
  (define-symbolic* opidx integer?)
  (define-symbolic* argidx1 integer?)
  (define-symbolic* argidx2 integer?)
  (instruction opidx argidx1 argidx2))

;; When an instruction is evaluated, we use the operator index to look up the function
;; and the argument indexes to look up the values in the list of registers,
;; then call that function on those arguments
(define (eval-instruction insn registers)
  ((operator-function (list-ref operator-list (instruction-op-idx insn)))
   (list-ref registers (instruction-arg1-idx insn))
   (list-ref registers (instruction-arg2-idx insn))))

;; A program in this form is basically a list of registers. The initial registers
;; hold the arguments passed to the function, and each instruction is another register
;; that holds its value after evaluation. Each instruction can refer to any register
;; that was evaluated before it

;; We also indicate which register holds the return value of the function.
;; (We could alternatively state that the last register will always the return value.
;; That would require a program have *exactly* some number of instructions,
;; rather than up to some number, but it gives the solver one less decision to make
;; so can speed up synthesis.)

;; A program sketch thus holds a list of instructions, the number of arguments to be passed
;; to the program, and the index of the register that holds the return value
(struct sketch (instruction-list input-count retval-idx) #:transparent)

;; A symbolic program holds a list of symbolic instructions and a symbolic return value index
(define (get-symbolic-sketch insn-count input-count)
  (define-symbolic retvalidx integer?)
  (sketch (for/list ([i (range insn-count)]) (get-sym-instruction))
          input-count
          retvalidx))

;; This function acts as an interpreter: we take a sketch and return a function that will evaluate the
;; program it encodes, on either concrete or symbolic inputs
(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-registers i)
                ;; when we have evaluated all of the instructions, we are done
                (cond [(equal? (length (sketch-instruction-list sk)) i) calculated-registers]
                      ;; otherwise, evaluate the next instruction, letting it index into
                      ;; the list of registers (inputs + all previously evaluated instructions)
                      [else (let ([next-reg (eval-instruction (list-ref (sketch-instruction-list sk) i)
                                                              calculated-registers)])
                              (f (append calculated-registers (list next-reg)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))

;; To make debugging easier, we define a pretty-printer for register programs
;; (note some facts about our language are hardcoded in these functions!)
(define (instruction->string insn register-num)
  (let ([op (list-ref operator-list (instruction-op-idx insn))])
        (if (equal? (operator-arity op) 2)
            (format "  (define r~a (~a r~a r~a))" register-num (operator-name op) (instruction-arg1-idx insn) (instruction-arg2-idx insn))
            (format "  (define r~a (~a r~a))" register-num (operator-name op) (instruction-arg1-idx insn)))))

(define (sketch->string sk funcname)
  (let* ([input-count (sketch-input-count sk)]
         [arg-list (for/list ([i (range input-count)])
                     (format "input~a" i))]
         [input-stmt-list (for/list ([i (range input-count)])
                            (format "  (define r~a input~a)" i i))]
         [stmt-list (for/list ([i (range (length (sketch-instruction-list sk)))])
                          (instruction->string (list-ref (sketch-instruction-list sk) i) (+ i input-count)))]
         [return-stmt (format "  r~a))" (sketch-retval-idx sk))])
    (string-append (format "(define (~a ~a)\n" funcname (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join stmt-list "\n")
                   "\n"
                   return-stmt)))



(define (mirror-box) (bv #x9009064002609009 64))
(define (true-mirror-box) (bv #x9009066006609009 64))
(define (tetrominoes) (bv #x0990600240060990 64))
(define (tetrominoes-2) (bv #x0990681241860990 64))
(define (full-box) (bv #xFFFFFFFFFFFFFFFF 64))
(define (planes-3) (bv #x9009000000009009 64))
(define (planes-3-chunk) (bv #x9009000000009000 64))


(define sym-sk (get-symbolic-sketch 9 1))
(error-print-width 100000)
(define-symbolic* input1 integer?)
(define (reference-impl) (bv #xFFFFFFFFFFFFFFFF 64))


;; Finally, we synthesize a register program version of the reference function
(let ([sketch-program (get-sketch-function sym-sk)])
  (begin (define binding (time (synthesize #:forall (list input1)
                                           #:guarantee (assert (equal? (reference-impl)
                                                                       (sketch-program (bv 1 64)))))))
         (if (unsat? binding)
             (displayln "Cannot synthesize program that matches reference implementation")
             (displayln (sketch->string (evaluate sym-sk binding) "synthed-func")))))
